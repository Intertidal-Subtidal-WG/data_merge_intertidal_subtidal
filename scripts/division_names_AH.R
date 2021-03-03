### ASSIGNING COMMON DIVISION NAMES TO INTERTIDAL SPECIES

# load in all the necessary packages
pkgs <- c("tidyverse", "janitor", "stringdist")

lapply(pkgs, library, character.only = TRUE)

# read in all the files as objects
file.names <- list.files(path = "./raw_data/keen/", pattern = "*.csv", full.names = TRUE)

list2env(lapply(setNames(file.names, make.names(gsub(".*//", "", tools::file_path_sans_ext(file.names)))), 
                read_csv), envir = .GlobalEnv)

file.names <- list.files(path = "./raw_data/intertidal/", pattern = "*.csv", full.names = TRUE)

list2env(lapply(setNames(file.names, make.names(gsub(".*//", "", tools::file_path_sans_ext(file.names)))), 
                read_csv), envir = .GlobalEnv)

# get species names from KEEN files

# first from the cover data
division_names_cover <- keen_cover %>% 
  select(SP_CODE, GROUP:SPECIES) %>% 
  unique()

# next from the quadrat data
division_names_quads <- keen_quads %>% 
  select(SP_CODE, GROUP:SPECIES) %>% 
  unique()

# join these two name lists together
all_division_names <- division_names_cover %>% 
  full_join(division_names_quads) %>% 
  filter(GROUP != "Substrate") %>% 
  # change case of column names to lowercase
  clean_names(case = "snake")

# making this the master list of common division names
keen_names <- levels(as.factor(all_division_names$common_division_name))

### now read in the intertidal species list
itz_names <- levels(as.factor(species_list_aggregate$subtype))

# Looking at these lists of names, only small-ish adjustments need to be made here

# fuzzy match the intertidal name list with the KEEN names
matches_in_keen <- amatch(itz_names, keen_names, maxDist = 1)

keen_name <- c()

for (i in 1:length(matches_in_keen)){
  if (is.na(matches_in_keen[i]) == FALSE){
    keen_name[i] <- keen_names[matches_in_keen[i]]
  }
  else {
    keen_name[i] <- NA
  }
}

matches <- as.data.frame(cbind(itz_names, keen_name)) %>% 
  mutate(keen_name = if_else(str_detect(itz_names, pattern = "Brown Al"), "Brown Algae", keen_name)) %>% 
  mutate(keen_name = if_else(is.na(keen_name), itz_names, keen_name)) %>% 
  mutate(keen_name = str_replace_all(keen_name, c("Worm" = "Worms", "Polyplacophora" = "Polyplacophorans",
                                                  "Hydrozoan" = "Hydrozoans"))) %>% 
  rename(common_division_name = keen_name,
         subtype = itz_names)

write_csv(matches, "./subtype_to_divisionname.csv")
