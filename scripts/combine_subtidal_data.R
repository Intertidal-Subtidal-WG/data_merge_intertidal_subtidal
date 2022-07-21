#'----------------------------------------------------------
#' @description Read in and combine the SML subtidal datasets
#' @author Jarrett Byrnes
#'----------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)

setwd(here::here())

# 1. Read the  Data----------------
keen_cover <- read_csv ("https://github.com/kelpecosystems/observational_data/raw/master/cleaned_data/keen_cover.csv") %>%
  select(-NETWORK, -PI) %>%
  mutate(MEASURE = "PERCENT_COVER", PROTOCOL = "PERCENT_COVER") %>%
  rename(VALUE = PERCENT_COVER)

keen_fish <- read_csv("https://github.com/kelpecosystems/observational_data/raw/master/cleaned_data/keen_fish.csv") %>%
  select(-NETWORK, -PI) %>%
  mutate(SIZE = FISH.SIZE, MEASURE = "COUNT", GROUP = "FISH", PROTOCOL = "FISH") %>%
  rename(VALUE = COUNT) %>%
  mutate(SPECIES = ifelse(SP_CODE=="NO_FISH", "No Fish", SPECIES))

keen_quads <- read_csv ("https://github.com/kelpecosystems/observational_data/raw/master/cleaned_data/keen_quads.csv")%>%
  select(-NETWORK, -PI) %>%
  mutate(MEASURE = "COUNT", PROTOCOL = "QUAD", QUAD = as.character(QUAD)) %>%
  rename(VALUE = COUNT)
  
keen_swath <- read_csv ("https://github.com/kelpecosystems/observational_data/raw/master/cleaned_data/keen_swath.csv") %>%
  select(-NETWORK, -PI) %>%
  mutate(MEASURE = "COUNT", PROTOCOL = "SWATH") %>%
  rename(VALUE = COUNT)

#ADD SUBSTRATE SPECIES NAME
substrate <- read_csv("tidy_data/substrate_translation.csv")
keen_cover <- left_join(keen_cover, substrate) %>%
  mutate(SPECIES = ifelse(GROUP == "Substrate", SUBSTRATE, SPECIES)) %>%
  select(-SUBSTRATE)

# #size - make long
# keen_kelp <- read_csv ("https://github.com/kelpecosystems/observational_data/raw/master/cleaned_data/keen_kelp.csv") %>%
#   select(-NETWORK, -PI) %>%
#   group_by(YEAR, SITE, TRANSECT, SP_CODE) %>%
#   mutate(INDIVIDUAL = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(cols = BLADE_LENGTH_CM:WIDTH_CM,
#                names_to = "MEASURE",
#                values_to = "VALUE")


#combine abundance
abund_combine <- bind_rows(keen_cover,
                           keen_fish,
                           keen_quads,
                           keen_swath) %>%
  mutate(TRANSECT = stringr::str_replace_all(TRANSECT, "^8 Ball", "Magic 8 Ball"))

readr::write_csv(abund_combine, "tidy_data/allKEEN_combined_subtidal_abundance.csv")
saveRDS(abund_combine, "tidy_data/allKEEN_combined_subtidal_abundance.RDS")


abund_combine_wide <- abund_combine %>%
  select(-c(SP_CODE, GROUP:GENUS)) %>%
  group_by(YEAR, SITE, TRANSECT, SPECIES, PROTOCOL) %>%
  summarize(VALUE = mean(VALUE, na.rm=TRUE)) %>%
  tidyr::pivot_wider(names_from = c("SPECIES", "PROTOCOL"),
                     values_from = "VALUE", 
                     values_fill = 0)

readr::write_csv(abund_combine_wide, "tidy_data/allKEEN_combined_subtidal_abundance_wide.csv")

abund_combine_appledore <- abund_combine %>%
  filter(SITE %in% c(
    "NE Appledore",
    "NW Appledore",
    "SW Appledore"
  ))

abund_combine_appledore_wide <- abund_combine_wide %>%
  filter(SITE %in% c(
    "NE Appledore",
    "NW Appledore",
    "SW Appledore"
  ))

#add the transect translation names
transect_translate <- read_csv("tidy_data/transect_translate.csv") %>%
  rename(Transect = Subtidal_Transect_Name) %>%
  rename_all(toupper)

abund_combine_appledore <- left_join(abund_combine_appledore, transect_translate)
abund_combine_appledore_wide <- left_join(abund_combine_appledore_wide, transect_translate)

#write out
readr::write_csv(abund_combine_appledore, "tidy_data/combined_subtidal_abundance.csv")
readr::write_csv(abund_combine_appledore_wide, "tidy_data/combined_subtidal_abundance_wide.csv")
saveRDS(abund_combine_appledore, "tidy_data/combined_subtidal_abundance.RDS")
