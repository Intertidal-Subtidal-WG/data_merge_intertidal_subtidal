### Checking taxonomy in datasheets is up to date

library(taxize)
library(tidyverse)

worms_check <- worms_downstream(species_list_aggregate$AphiaID, downto = "species")

# read in all the files as objects
file.names <- list.files(path = "./raw_data/keen/", pattern = "*.csv", full.names = TRUE)

list2env(lapply(setNames(file.names, make.names(gsub(".*//", "", tools::file_path_sans_ext(file.names)))), 
                read_csv), envir = .GlobalEnv)

file.names <- list.files(path = "./raw_data/intertidal/", pattern = "*.csv", full.names = TRUE)

list2env(lapply(setNames(file.names, make.names(gsub(".*//", "", tools::file_path_sans_ext(file.names)))), 
                read_csv), envir = .GlobalEnv)
