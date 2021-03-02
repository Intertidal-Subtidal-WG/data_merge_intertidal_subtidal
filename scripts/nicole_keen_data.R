#Purpose of this file is to merge the datasets created by the Kelp Ecosystems Ecology Network (KEEN) and the Shoals Marine Laboratory Intertidal Monitoring Program

library(tidyverse)

#Download KEEN data

keen_cover <- read_csv("data/keen/keen_cover.csv")   #percent cover data for algae, inverts and substrate
head(keen_cover)

keen_fish <- read_csv("data/keen/keen_fish.csv")   #fish count/size data
head(keen_fish)

keen_kelp <- read_csv("data/keen/keen_kelp.csv")   #kelp morphology data (10 of each species/transect I think)
head(keen_kelp)

keen_quads <- read_csv("data/keen/keen_quads.csv")   #counts and sizes of solitary large algae, invertebrates, and small cryptic fish
head(keen_quads)

keen_sites <- read_csv("data/keen/keen_sites.csv")   #site environmental data
head(keen_sites)

keen_swath <- read_csv("data/keen/keen_swath.csv")   #for large rare or clumped solitary species (there is a specific list of species included here)
head(keen_swath)

kelp_quads_biomass <- read_csv("data/keen/kelp_quads_biomass.csv") #average wet weight * abundance for each species
head(kelp_quads_biomass)




#Create dataframe with species lists for each .csv to identify species that were counted using multiple methods

#Create species lists for all data frames then combine to get rows

keen_cover_sp <- levels(as.factor(keen_cover$SPECIES))
keen_fish_sp <- levels(as.factor(keen_fish$SPECIES))
keen_quads_sp <- levels(as.factor(keen_quads$SPECIES))
keen_kelp_sp <- levels(as.factor(keen_kelp$SPECIES))
keen_swath_sp <- levels(as.factor(keen_swath$SPECIES))
kelp_quads_biomass_sp <- levels(as.factor(kelp_quads_biomass$SPECIES))

keen_all_sp <- c(keen_cover_sp, keen_fish_sp, keen_quads_sp, keen_kelp_sp, keen_swath_sp, kelp_quads_biomass_sp)

keen_all_sp <- levels(as.factor(keen_all_sp))

species_sampling_df <- data.frame(keen_all_sp)

colnames(species_sampling_df) <- "SPECIES"

keen_cover_col <- rep(0, nrow(species_sampling_df))
species_sampling_df <- cbind(species_sampling_df, keen_cover_col)
species_sampling_df[which(species_sampling_df$SPECIES %in% keen_cover_sp), 2] <- 1 


keen_fish_col <- rep(0, nrow(species_sampling_df))
species_sampling_df <- cbind(species_sampling_df, keen_fish_col)
species_sampling_df[which(species_sampling_df$SPECIES %in% keen_fish_sp), 3] <- 1 

keen_quads_col <- rep(0, nrow(species_sampling_df))
species_sampling_df <- cbind(species_sampling_df, keen_quads_col)
species_sampling_df[which(species_sampling_df$SPECIES %in% keen_quads_sp), 4] <- 1 

keen_kelp_col <- rep(0, nrow(species_sampling_df))
species_sampling_df <- cbind(species_sampling_df, keen_kelp_col)
species_sampling_df[which(species_sampling_df$SPECIES %in% keen_kelp_sp), 5] <- 1 

keen_swath_col <- rep(0, nrow(species_sampling_df))
species_sampling_df <- cbind(species_sampling_df, keen_swath_col)
species_sampling_df[which(species_sampling_df$SPECIES %in% keen_swath_sp), 6] <- 1 

kelp_quads_biomass_col <- rep(0, nrow(species_sampling_df))
species_sampling_df <- cbind(species_sampling_df, kelp_quads_biomass_col)
species_sampling_df[which(species_sampling_df$SPECIES %in% kelp_quads_biomass_sp), 7] <- 1 

species_sampling_df$method_count <- rowSums(species_sampling_df[2:6]) #identify species that have been quantified multiple ways

multi_method_sp <- subset(species_sampling_df, method_count > 1) #separate out species that have been quantified multiple ways
groups <- c("kelp", "kelp", "kelp", "seastar", "seastar", "crab", "crab", "crab", "greenalgae", "brownalgae", "brownalgae", "brownalgae", #up to desmarestia viridis
                           "brownalgae", "seastar", "lobster", "kelp", "anemone", "sculpin", "anemone", "gunnel", "kelp", "brownalgae", "urchin",
                           "cunner", "greenalgae", "juvenilekelp") #assign species to groups
multi_method_sp <- cbind(groups, multi_method_sp)
multi_method_sp <- multi_method_sp[order(multi_method_sp$groups),]


#Create merged KEEN dataframe





















#Download IMP data

imp_categories_data <- read_csv("data/intertidal/categories_data.csv", col_types = cols(Replicate = col_character(), Data_taken = col_character()))
imp_categories_data   #supposed to be number of squares out of 16 but that doesn't appear to be how data were entered- there are different units and abbreviations

imp_counts_data <- read_csv("data/intertidal/counts_data.csv", col_types = cols(Replicate = col_character(), Data_taken = col_character()))
imp_counts_data   #counts of mobile and sessile animals that have distinct individuals

imp_fucus_asco_max_size <- read_csv("data/intertidal/fucus_asco_max_size.csv", col_types = cols(Replicate = col_character(), Data_taken = col_character()))
imp_fucus_asco_max_size   #size of longest branch on longest plant in each quadrat

imp_percent_cover_data <- read_csv("data/intertidal/percent_cover_data.csv", col_types = cols(Replicate = col_character(), Data_taken = col_character()))
imp_percent_cover_data #percent cover for algal species, sessile organisms and bare rock.  Some issues with species that were sometimes recorded separately in
                       #canopy vs. primary, details in Nguyen 2018 for resolution

imp_sizes_data <- read_csv("data/intertidal/sizes_data.csv", col_types = cols(Replicate = col_character(), Data_taken.x = col_character()))
imp_sizes_data   #sizes of individual animals

imp_transect_info <- read_csv("data/intertidal/transect_info.csv")
imp_transect_info   #for each transect, lat/long/slope/fetch/position


#QUESTIONS FOR 02/03/2021
#Explain kelp_quads_biomass (JB explained average wet weight * abundance)
#When merging datasets we will want to keep in mind a) which predictor/response variables we are trying to match up, b) how to deal with within-site variation
#(I am assuming we will want to match KEEN/IMP sites but not necessarily quadrats)- keeping measures of variation probably important, not just mean
#Can build exploratory graphs and datasets meant to test specific predictions
