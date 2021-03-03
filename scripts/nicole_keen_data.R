#Purpose of this file is to merge the datasets created by the Kelp Ecosystems Ecology Network (KEEN) and the Shoals Marine Laboratory Intertidal Monitoring Program

library(tidyverse)

#Download KEEN data

keen_cover <- read_csv("SEED_data/subtidal_KEEN_data/keen_cover.csv")   #percent cover data for algae, inverts and substrate
head(keen_cover)

keen_fish <- read_csv("SEED_data/subtidal_KEEN_data/keen_fish.csv")   #fish count/size data
head(keen_fish)

keen_kelp <- read_csv("SEED_data/subtidal_KEEN_data/keen_kelp.csv")   #kelp morphology data (10 of each species/transect I think)
head(keen_kelp)

keen_quads <- read_csv("SEED_data/subtidal_KEEN_data/keen_quads.csv")   #counts and sizes of solitary large algae, invertebrates, and small cryptic fish
head(keen_quads)

keen_sites <- read_csv("SEED_data/subtidal_KEEN_data/keen_sites.csv")   #site environmental data
head(keen_sites)

keen_swath <- read_csv("SEED_data/subtidal_KEEN_data/keen_swath.csv")   #for large rare or clumped solitary species (there is a specific list of species included here)
head(keen_swath)

keen_kelp_quads_biomass <- read_csv("SEED_data/subtidal_KEEN_data/kelp_quads_biomass.csv") #average wet weight * abundance for each species
head(keen_kelp_quads_biomass)


#Create dataframe with species lists for each .csv to identify species that were counted using multiple methods

#Create species lists for all data frames then combine to get rows

keen_cover_sp <- levels(as.factor(keen_cover$SPECIES))
keen_fish_sp <- levels(as.factor(keen_fish$SPECIES))
keen_quads_sp <- levels(as.factor(keen_quads$SPECIES))
keen_kelp_sp <- levels(as.factor(keen_kelp$SPECIES))
keen_swath_sp <- levels(as.factor(keen_swath$SPECIES))
keen_kelp_quads_biomass_sp <- levels(as.factor(keen_kelp_quads_biomass$SPECIES))

keen_all_sp <- c(keen_cover_sp, keen_fish_sp, keen_quads_sp, keen_kelp_sp, keen_swath_sp, keen_kelp_quads_biomass_sp)

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

keen_kelp_quads_biomass_col <- rep(0, nrow(species_sampling_df))
species_sampling_df <- cbind(species_sampling_df, keen_kelp_quads_biomass_col)
species_sampling_df[which(species_sampling_df$SPECIES %in% keen_kelp_quads_biomass_sp), 7] <- 1 

species_sampling_df$method_count <- rowSums(species_sampling_df[2:6]) #identify species that have been quantified multiple ways

multi_method_sp <- subset(species_sampling_df, method_count > 1) #separate out species that have been quantified multiple ways
groups <- c("kelp", "kelp", "kelp", "seastar", "seastar", "crab", "crab", "crab", "greenalgae", "brownalgae", "brownalgae", "brownalgae", #up to desmarestia viridis
                           "brownalgae", "seastar", "lobster", "kelp", "anemone", "sculpin", "anemone", "gunnel", "kelp", "brownalgae", "urchin",
                           "cunner", "greenalgae", "juvenilekelp") #assign species to groups
multi_method_sp <- cbind(groups, multi_method_sp)
multi_method_sp <- multi_method_sp[order(multi_method_sp$groups),]


#Create merged KEEN dataframe
#Will summarize data at the transect level I think for now
#For each CSV file, I want to keep the major identifiers (NETWORK, PI, YEAR, SITE, TRANSECT) plus an identifier (species, type of substrate) and some measure of 
#abundance (percent cover, count etc.)

#-PERCENT COVER DATA--------------------------------------------------------------------#



#First, working with keen_cover (percent cover file).  Already estimated at the transect levels so no agggregating required but do need to do some cleanup

keen_cover_sub <- keen_cover

#Assign substrates to SPECIES column (column 21)
substrate_class <- read_csv("scripts/substrate_classifications.csv")
keen_cover_sub[which(keen_cover_sub$SP_CODE == "B"), 21] <- "bedrock"
keen_cover_sub[which(keen_cover_sub$SP_CODE == "P"), 21] <- "pebbles"
keen_cover_sub[which(keen_cover_sub$SP_CODE == "C"), 21] <- "cobble"
keen_cover_sub[which(keen_cover_sub$SP_CODE == "BS"), 21] <- "boulder_small"
keen_cover_sub[which(keen_cover_sub$SP_CODE == "BM"), 21] <- "boulder_medium"
keen_cover_sub[which(keen_cover_sub$SP_CODE == "BL"), 21] <- "boulder_large"
keen_cover_sub[which(keen_cover_sub$SP_CODE == "SS"), 21] <- "sand_shallow"
keen_cover_sub[which(keen_cover_sub$SP_CODE == "SH"), 21] <- "shell_debris"
keen_cover_sub[which(keen_cover_sub$SP_CODE == "S"), 21] <- "sand"

#There are about 10 rows in the keen_cover dataframe that have undefined species codes (checked 
#"KEEN ONE New England Species List v1.3.4.xlsx" but no dice.  none of them contribute much to percent cover so will remove for now

keen_cover_sub<-keen_cover_sub[!is.na(keen_cover_sub$SPECIES),]

keen_cover_sub <- keen_cover[,c(1:3, 6:7, 21, 9)]    #percent cover appears to have been estimated at the transect level in this file
keen_cover_sub

#remove species that have <50% cover total in dataset for now (just so final dataset isn't incredibly unmanageable)

cover_rare_sp <- aggregate(keen_cover_sub$PERCENT_COVER, by = list(keen_cover_sub$SPECIES), sum)
cover_rare_sp <- subset(cover_rare_sp,x < 50)

keen_cover_sub <- keen_cover_sub[-which(keen_cover_sub$SPECIES %in% cover_rare_sp$Group.1),]

#Going to modify species names to include a percent cover identifier to distinguish from species sampled using other methods

keen_cover_sub$SPECIES <- sub(" ", "_", keen_cover_sub$SPECIES) #these will eventually be column names so want to replace spaces with _


keen_cover_sub$PC <- "_PERCOV"
keen_cover_sub$SPECIES <- with(keen_cover_sub, paste0(SPECIES,PC))
keen_cover_sub <- keen_cover_sub[,-(8)]

colnames(keen_cover_sub) <- c("NETWORK", "PI", "YEAR", "SITE", "TRANSECT", "ID", "QUANTITY" ) #aiming to standardize column names across all dataframes


#-FISH COUNTS------------------------------------------------#

#OK, moving on to fish.  Realizing now that I've been listening to the same Feist song on repeat for 20 minutes

keen_fish_sub <- keen_fish

#Looking at SPECIES column.  Species code TAON is Tautoga onitis, also two entries that read TOAN.  I think this is a typo and will fix accordingly

keen_fish_sub[which(keen_fish_sub$SP_CODE == "TOAN"), 25] <- "Tautoga onitis"
keen_fish_sub[which(keen_fish_sub$SP_CODE == "NO_FISH"), 25] <- "NO_FISH"

#For now I am going to sum size classes not including YOY

keen_fish_sub <- subset(keen_fish_sub, FISH.SIZE != "YOY")

keen_fish_sub <- aggregate(keen_fish_sub$COUNT, by = list(keen_fish_sub$NETWORK, keen_fish_sub$PI, keen_fish_sub$YEAR, keen_fish_sub$SITE, 
                                                                   keen_fish_sub$TRANSECT,  keen_fish_sub$SPECIES), sum)

colnames(keen_fish_sub) <- c("NETWORK", "PI", "YEAR", "SITE", "TRANSECT", "ID", "QUANTITY")

keen_fish_sub$ID<- sub(" ", "_", keen_fish_sub$ID) #these will eventually be column names so want to replace spaces with _


#Eventually when I merge these files 0s will be assigned to transect/year combinations without an entry.  So for now I will assign all rows with NO_FISH 
#SP_CODE to have a count of 1 so they are identifiable (they currently have 0s)

keen_fish_sub[which(keen_fish_sub$SPECIES =="NO_FISH"), 7] <- 1

#Add .csv identifier
keen_fish_sub$FISH <- "_FC"
keen_fish_sub$SPECIES <- with(keen_fish_sub, paste0(SPECIES, FISH))
keen_fish_sub <- keen_fish_sub[,-(8)]

keen_fish_sub <- as_tibble(keen_fish_sub)



#-KELP BIOMASS QUADRATS----------------------------------------------------------#

#to do to keen_kelp_quad_sbiomass_sub: aggregate data at transect level

kelp_biomass_sub<- aggregate(keen_kelp_quads_biomass$transect_mean_wet_weight, 
                             by = list(keen_kelp_quads_biomass$NETWORK, keen_kelp_quads_biomass$PI, keen_kelp_quads_biomass$YEAR, keen_kelp_quads_biomass$SITE, 
                                       keen_kelp_quads_biomass$TRANSECT,  keen_kelp_quads_biomass$SPECIES), sum)


colnames(kelp_biomass_sub) <- c("NETWORK", "PI", "YEAR", "SITE", "TRANSECT", "ID", "QUANTITY")

kelp_biomass_sub <- as_tibble(kelp_biomass_sub)


kelp_biomass_sub$ID<- sub(" ", "_", kelp_biomass_sub$ID) #these will eventually be column names so want to replace spaces with _
kelp_biomass_sub$KB <- "_KB"

kelp_biomass_sub$ID <- with(kelp_biomass_sub, paste0(ID, KB))
kelp_biomass_sub <- kelp_biomass_sub[,-8]

#-QUADRAT COUNT DATA------------------------------------------------------------#

keen_quads_sub <- keen_quads

#Algae are already represented in percent cover and kelp biomass data so in the interest of simplicity will remove for now

keen_quads_sub <- subset(keen_quads_sub, GROUP != "Algae")

#Will also remove rare species (fewer than 10 individuals in entire dataset)
###NOTE the phylogenetic classifications are messed up for ulvaria, I think someone mistook it for green algae

rare_sp_quads <- aggregate(keen_quads_sub$COUNT, by = list(keen_quads_sub$SPECIES), sum)
rare_sp_quads <- subset(rare_sp_quads, x < 10)

keen_quads_sub <- keen_quads_sub[-(which(keen_quads_sub$SPECIES %in% rare_sp_quads$Group.1)), ]

keen_quads_sub <- aggregate(keen_quads_sub$COUNT, 
                            by = list(keen_quads_sub$NETWORK, keen_quads_sub$PI, keen_quads_sub$YEAR, keen_quads_sub$SITE, 
                                      keen_quads_sub$TRANSECT,  keen_quads_sub$SPECIES), sum)

colnames(keen_quads_sub) <- c("NETWORK", "PI", "YEAR", "SITE", "TRANSECT", "ID", "QUANTITY")

keen_quads_sub <- as_tibble(keen_quads_sub)


keen_quads_sub$ID <- sub(" ", "_", keen_quads_sub$ID) #these will eventually be column names so want to replace spaces with _

keen_quads_sub$QD <- "_QD"

keen_quads_sub$ID <- with(keen_quads_sub, paste0(ID, QD))
keen_quads_sub <- keen_quads_sub[,-8]

keen_quads_sub


#-SWATH DATA-------------------------------------------------------------------#

keen_swath_sub <- keen_swath

#Removing algae since its already represented in percent cover and kelp biomass data

keen_swath_sub <- subset(keen_swath_sub, GROUP != "Algae")

#Will also remove rare species (fewer than 10 individuals in entire dataset)

rare_sp_swath <- aggregate(keen_swath_sub$COUNT, by = list(keen_swath_sub$SPECIES), sum)
rare_sp_swath <- subset(rare_sp_swath, x < 10)

keen_swath_sub <- keen_swath_sub[-(which(keen_swath_sub$SPECIES %in% rare_sp_swath$Group.1)), ]

keen_swath_sub <- aggregate(keen_swath_sub$COUNT, 
                            by = list(keen_swath_sub$NETWORK, keen_swath_sub$PI, keen_swath_sub$YEAR, keen_swath_sub$SITE, 
                                      keen_swath_sub$TRANSECT,  keen_swath_sub$SPECIES), sum)

keen_swath_sub <- as_tibble(keen_swath_sub)
colnames(keen_swath_sub) <- c("NETWORK", "PI", "YEAR", "SITE", "TRANSECT", "ID", "QUANTITY")


keen_swath_sub$ID <- sub(" ", "_", keen_swath_sub$ID) #these will eventually be column names so want to replace spaces with _

keen_swath_sub$SW <- "_SW"

keen_swath_sub$ID <- with(keen_swath_sub, paste0(ID, SW))
keen_swath_sub <- keen_swath_sub[,-8]


#Check all new dataframes
keen_cover_sub
keen_fish_sub
kelp_biomass_sub
keen_quads_sub
keen_swath_sub


#Combine all new dataframes and convert to wide format

keen_data_sub <- bind_rows(kelp_biomass_sub, keen_fish_sub, keen_quads_sub, keen_cover_sub, keen_swath_sub)

library(reshape2)

keen_data_wide <- dcast(keen_data_sub, NETWORK + PI + YEAR + SITE + TRANSECT ~ ID, value.var = "QUANTITY", fun.aggregate = sum)

keen_data_wide <- as_tibble(keen_data_wide)

#Gonna drop network and PI since always the same

keen_data_wide <- keen_data_wide[,-(1:2)]

#Can now add in site data 

keen_sites$TRANSECTID <- paste(keen_sites$YEAR, keen_sites$SITE, keen_sites$TRANSECT, sep="_")
keen_data_wide$TRANSECTID <- paste(keen_data_wide$YEAR, keen_data_wide$SITE, keen_data_wide$TRANSECT, sep="_")


keen_data_wide <- keen_data_wide[order(keen_data_wide$TRANSECTID),]
keen_sites <- keen_sites[order(keen_sites$TRANSECTID),]

#keen_sites is missing a transect

shared_sites <- which(keen_sites$TRANSECTID %in% keen_data_wide$TRANSECTID) #subset to transects with data in sites and wide_data, will fix later
shared_sites

keen_data_wide <- keen_data_wide[which(keen_data_wide$TRANSECTID %in% keen_sites$TRANSECTID),]
keen_sites <- keen_sites[which(keen_sites$TRANSECTID %in% keen_data_wide$TRANSECTID),]

keen_sites_add <- keen_sites[,c(4,5,8:16)]

keen_data_wide <- bind_cols(keen_sites_add, keen_data_wide)

#QUESTIONS FOR 02/03/2021
#Explain keen_kelp_quads_biomass (JB explained average wet weight * abundance)
#When merging datasets we will want to keep in mind a) which predictor/response variables we are trying to match up, b) how to deal with within-site variation
#(I am assuming we will want to match KEEN/IMP sites but not necessarily quadrats)- keeping measures of variation probably important, not just mean
#Can build exploratory graphs and datasets meant to test specific predictions
