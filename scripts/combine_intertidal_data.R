#'----------------------------------------------------------
#' @description Read in and combine the SML intertidal datasets
#' @author Jarrett Byrnes
#'----------------------------------------------------------

library(readr)
library(dplyr)
setwd(here::here())

#load the data from the SEED repo
cover <- read_csv("https://github.com/brianscheng/SEED/blob/main/data/intertidal/pc_clean.csv?raw=true") %>%
  rename(Intertidal_Transect = Transect) %>%
  select(-starts_with("X")) %>%
  mutate(Measure = "Percent_Cover", Protocol = "Intertidal_Cover") %>%
  rename(Value = Percent_cover)

count <- read_csv("https://github.com/brianscheng/SEED/blob/main/data/intertidal/ct_clean.csv?raw=true") %>%
  rename(Intertidal_Transect = Transect) %>%
  select(-starts_with("X")) %>%
  mutate(Measure = "Count", Protocol = "Intertidal_Count") %>%
  rename(Value = Count)

# 
# barnacle_mussel <- read_csv("https://github.com/brianscheng/SEED/raw/main/data/intertidal/sz_clean.csv") %>%
#   rename(Intertidal_Transect = Transect) %>%
#   select(-starts_with("X")) %>%
#   mutate(Measure = "Count") %>%
#   rename(Value = Count)
# 
# fucus_asco <- read_csv("https://github.com/brianscheng/SEED/raw/main/data/intertidal/sw_sz_clean.csv") %>%
#   rename(Intertidal_Transect = Transect) %>%
#   select(-starts_with("X")) %>%
#  # mutate(Measure = "Count") %>%
#   rename(Value = Count)

#add sizes once cleaned

combined_int <- bind_rows(cover, count) %>%
  mutate(Organism_Type = stringr::str_extract(Organism, " \\(.*$"),
         Organism_Type = stringr::str_replace_all(Organism_Type, " \\((.*)\\)$", "\\1"),
         Organism = stringr::str_replace_all(Organism, " \\((.*)\\)$", ""),
  )


#read in the species info and merge with the data
sp_info <- read_csv("https://github.com/brianscheng/SEED/raw/main/data/intertidal/species_list_aggregate.csv") %>%
  rename(Organism = name)

combined_int <- left_join(combined_int, sp_info)

#check for bad merge
# combined_int[which(is.na(combined_int$valid_name)),] %>% 
#   pull(Organism) %>% unique() %>% sort()


#add the transect translation names
transect_translate <- read_csv("tidy_data/transect_translate.csv") %>%
  rename(Transect = Subtidal_Transect_Name)


combined_int <- left_join(combined_int, transect_translate)


# reorder, capitalize, filter data not takenand good to go!
combined_int <- combined_int %>%
  rename_all(toupper) %>%
  select(SITE, TRANSECT, INTERTIDAL_TRANSECT, YEAR, everything()) %>%
  dplyr::filter(DATA_TAKEN != "no") %>%
  mutate(AREA = 0.04, TIDE_HEIGHT_REL_MLLW = (13-LEVEL)*0.348)


#write out
readr::write_csv(combined_int, "tidy_data/combined_intertidal_abundance.csv")
saveRDS(combined_int, "tidy_data/combined_intertidal_abundance.RDS")

