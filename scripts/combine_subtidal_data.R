#'----------------------------------------------------------
#' Read in and combine the SML subtidal datasets
#'----------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)

setwd(here::here())

# 1. Read the  Data----------------
keen_cover <- read_csv ("https://github.com/brianscheng/SEED/raw/main/data/keen/keen_cover.csv") %>%
  select(-NETWORK, -PI) %>%
  mutate(MEASURE = "PERCENT_COVER", PROTOCOL = "PERCENT_COVER") %>%
  rename(VALUE = PERCENT_COVER)

keen_fish <- read_csv("https://github.com/brianscheng/SEED/raw/main/data/keen/keen_fish.csv") %>%
  select(-NETWORK, -PI) %>%
  mutate(SIZE = FISH.SIZE, MEASURE = "COUNT", GROUP = "FISH", PROTOCOL = "FISH") %>%
  rename(VALUE = COUNT)

keen_quads <- read_csv ("https://github.com/brianscheng/SEED/raw/main/data/keen/keen_quads.csv")%>%
  select(-NETWORK, -PI) %>%
  mutate(MEASURE = "COUNT", PROTOCOL = "QUAD", QUAD = as.character(QUAD)) %>%
  rename(VALUE = COUNT)
  
keen_swath <- read_csv ("https://github.com/brianscheng/SEED/raw/main/data/keen/keen_swath.csv") %>%
  select(-NETWORK, -PI) %>%
  mutate(MEASURE = "COUNT", PROTOCOL = "SWATH") %>%
  rename(VALUE = COUNT)



#size - make long
keen_kelp <- read_csv ("https://github.com/brianscheng/SEED/raw/main/data/keen/keen_kelp.csv") %>%
  select(-NETWORK, -PI) %>%
  group_by(YEAR, SITE, TRANSECT, SP_CODE) %>%
  mutate(INDIVIDUAL = 1:n()) %>%
  ungroup() %>%
  pivot_longer(cols = BLADE_LENGTH_CM:WIDTH_CM,
               names_to = "MEASURE",
               values_to = "VALUE")


#combine abundance
abund_combine <- bind_rows(keen_cover,
                           keen_fish,
                           keen_quads,
                           keen_swath) %>%
  mutate(TRANSECT = stringr::str_replace_all(TRANSECT, "^8 Ball", "Magic 8 Ball"))

#add the transect translation names
transect_translate <- read_csv("tidy_data/transect_translate.csv") %>%
  rename(Transect = Subtidal_Transect_Name) %>%
  rename_all(toupper)

abund_combine <- left_join(abund_combine, transect_translate)

#write out
readr::write_csv(abund_combine, "tidy_data/combined_subtidal_abundance.csv")
saveRDS(abund_combine, "tidy_data/combined_subtidal_abundance.RDS")
