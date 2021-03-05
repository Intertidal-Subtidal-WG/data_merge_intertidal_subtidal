#'-----------------------------------------------------------------------
#' @description Combine Intertidal and Subtidal Datasets of Abundances
#' @author Jarrett Byrnes
#'-----------------------------------------------------------------------
library(readr)
library(dplyr)

setwd(here::here())

subtidal <- readRDS("tidy_data/combined_subtidal_abundance.RDS")
intertidal <- readRDS("tidy_data/combined_intertidal_abundance.RDS")

#aggregate for combination

#subtidal to transect level for each protocol
subtidal <- subtidal %>%
  group_by(YEAR, SITE, TRANSECT, INTERTIDAL_TRANSECT, SP_CODE, AREA,
           across(GROUP:PROTOCOL)) %>%
  summarize(VALUE = mean(VALUE)) %>%
  ungroup() %>%
  select(YEAR, SITE, TRANSECT, INTERTIDAL_TRANSECT, 
         PROTOCOL, SPECIES, SIZE, MEASURE, VALUE, AREA, everything()) %>%
  rename(ORGANISM = SPECIES)


#intertidal to tide height:transect level for each protocol
intertidal <- intertidal %>%
  group_by(YEAR, SITE, TRANSECT, INTERTIDAL_TRANSECT, LEVEL, 
           TIDE_HEIGHT_REL_MLLW, ORGANISM, ORGANISM_TYPE, AREA,
           across(MEASURE:GENUS)) %>%
  summarize(VALUE = mean(VALUE)) %>%
  ungroup() %>%
  select(YEAR, SITE, TRANSECT, INTERTIDAL_TRANSECT, LEVEL,
         PROTOCOL, ORGANISM, ORGANISM_TYPE, MEASURE, VALUE, AREA, everything())


#join the datasets
combined_data <- bind_rows(subtidal, intertidal) %>%
  filter(YEAR>2013) %>% #so it's constrained to years that both were conducted
  filter(!is.na(TRANSECT)) %>%
  select(YEAR:INTERTIDAL_TRANSECT, LEVEL, TIDE_HEIGHT_REL_MLLW, PROTOCOL:SIZE, ORGANISM_TYPE, everything()) #some reordering for readability


#load in common division names
divisions <- read_csv("tidy_data/division_name_keys.csv")


#Get site averages for the datasets
combined_data_site <- combined_data %>%
  group_by(YEAR, SITE, across(LEVEL:MEASURE), across(AREA:VALID_AUTHORITY)) %>%
  summarise(MEAN_VALUE = mean(VALUE, na.rm=T),
            SD_VALUE = sd(VALUE, na.rm=T),
  ) %>%
  ungroup()

#filter to where both have data

combined_data_transect <- combined_data %>%
  group_by(TRANSECT) %>%
  mutate(has_intertidal = sum(stringr::str_detect(PROTOCOL, "Intertidal"))>0) %>%
  ungroup() %>%
  filter(has_intertidal) %>%
  select(-has_intertidal)


write_csv(combined_data, "tidy_data/combined_all_abundance_data.csv")
saveRDS(combined_data, "tidy_data/combined_all_abundance_data.RDS")


write_csv(combined_data_site, "tidy_data/combined_all_abundance_data_site.csv")
saveRDS(combined_data_site, "tidy_data/combined_all_abundance_data_site.RDS")


write_csv(combined_data_transect, "tidy_data/combined_all_abundance_data_transect_match.csv")
saveRDS(combined_data_transect, "tidy_data/combined_all_abundance_data_transect_match.RDS")
