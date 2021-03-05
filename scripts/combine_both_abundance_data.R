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
           ORGANISM, ORGANISM_TYPE, AREA,
           across(MEASURE:GENUS)) %>%
  summarize(VALUE = mean(VALUE)) %>%
  ungroup() %>%
  select(YEAR, SITE, TRANSECT, INTERTIDAL_TRANSECT, LEVEL,
         PROTOCOL, ORGANISM, ORGANISM_TYPE, MEASURE, VALUE, AREA, everything())


#join the datasets
combined_data <- bind_rows(subtidal, intertidal) %>%
  filter(YEAR>2013) %>% #so it's constrained to years that both were conducted
  filter(!is.na(TRANSECT)) %>%
  select(YEAR:INTERTIDAL_TRANSECT, LEVEL, PROTOCOL:SIZE, ORGANISM_TYPE, everything()) #some reordering for readability

write_csv(combined_data, "tidy_data/combined_all_abundance_data.csv")
saveRDS(combined_data, "tidy_data/combined_all_abundance_data.RDS")
