### ASSIGNING COMMON DIVISION NAMES TO INTERTIDAL SPECIES

# load in all the necessary packages
pkgs <- c("tidyverse", "janitor", "stringdist")

lapply(pkgs, library, character.only = TRUE)

# read in all the files as objects
file.names <- list.files(path = "./SEED_data/subtidal_KEEN_data/", pattern = "*.csv", full.names = TRUE)

list2env(lapply(setNames(file.names, make.names(gsub(".*//", "", tools::file_path_sans_ext(file.names)))), 
                read_csv), envir = .GlobalEnv)

# the next lines of code keep causing fatal errors with the new folder structure. not sure why.
#file.names <- list.files(path = "./SEED_data/intertidal_data/", pattern = "*.csv", full.names = TRUE)

#list2env(lapply(setNames(file.names, make.names(gsub(".*//", "", tools::file_path_sans_ext(file.names)))), read_csv), envir = .GlobalEnv)

species_list_aggregate <- read_csv("./SEED_data/intertidal_data/species_list_aggregate.csv")

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

## read in categories from KEEN surveys
keen_names <- levels(as.factor(all_division_names$common_division_name))

### now read in the intertidal species list
itz_names <- levels(as.factor(species_list_aggregate$subtype)) %>% str_replace_all(itz_names, c("Alag" = "Alga"))

# fuzzy match the intertidal name list with the KEEN names
matches_in_keen <- amatch(itz_names, keen_names, maxDist = 1)

# now create the final shared column of common division name keys
common_division_name <- c()

# if there is a match between intertidal and subtidal, then use the KEEN name
for (i in 1:length(matches_in_keen)){
  if (is.na(matches_in_keen[i]) == FALSE){
    common_division_name[i] <- keen_names[matches_in_keen[i]]
  }
  # but if there is no match, leave it empty
  else {
    common_division_name[i] <- NA
  }
}

# Need to classify brown algae as erect in the initial KEEN data and preserve unique subtidal taxa
keen_original <- as.data.frame(cbind(keen_names, keen_names)) %>% 
  # we will eventually join with the other intertidal relabeled set by common_division_name
  # COMMON.DIVISION.NAME is the KEEN column heading, so preserve this scheme for converting to 
  # the new division naming system later
  rename(common_division_name = 1, COMMON.DIVISION.NAME = 2) %>% 
  mutate(common_division_name = str_replace_all(common_division_name, c("Brown Algae" = "Erect Brown Algae")))

# now join everything together to have three columns - one for the intertidal original names (subtype),
# one for the KEEN original names (COMMON.DIVISION.NAME), and one for the shared key between the two.
division_name_keys <- as.data.frame(cbind(itz_names, common_division_name)) %>% 
  mutate(common_division_name = if_else(is.na(common_division_name), itz_names, common_division_name)) %>% 
  # convert the intertidal names that are singular to plural
  mutate(common_division_name = str_replace_all(common_division_name, c("Worm" = "Worms",
                                                  "Hydrozoan" = "Hydrozoa"))) %>% 
  rename(subtype = itz_names) %>% 
  full_join(keen_original)

# write the final data file
# write_csv(division_name_keys, "./division_name_keys.csv")
