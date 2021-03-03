###############################################################################
###############################################################################
## Proyect Working group CIEE
## R-code for Merging data of intertidal and subtidal 
## Jenny Munoz
#### last update: March 2 2021
################################################################################
################################################################################
# Loading packages --------------------------------------------------------
# libraries for easier manipulation of data
install.packages("tidyr") 
install.packages("tidyverse") 
install.packages("dplyr")
install.packages ("data.table")
install.packages ("extrafont")
installed.packages("lubridate")  #for dates
install.packages("car")
#Other libraries for data analyses
install.packages("vegan")
install.packages("ggplot2")
install.packages("devtools")
install.packages("lme4")
install.packages("knitr")
install.packages("ts")
# To create presnece absence matrix
install.packages("letsR")



library(tidyverse)
library(tidyr)
library(plyr)
library(ggplot2)
library(dplyr)
library(data.table)
library(extrafont)
library(visreg)
library(lubridate)
library(letsR)
#library(ts)


loadfonts()


# Dataset subtidal_KEEN --------------------------------------------------
# 1. Read the  Data----------------
keen_cover<-read.csv ("keen_cover.csv", header=TRUE, strip.white=TRUE)
keen_fish <-read.csv("keen_fish.csv", header=TRUE, strip.white=TRUE)
keen_kelp <-read.csv ("keen_kelp.csv", header=TRUE, strip.white=TRUE)
keen_quads<-read.csv ("keen_quads.csv", header=TRUE, strip.white=TRUE)
keen_sites<-read.csv ("keen_sites.csv", header=TRUE, strip.white=TRUE)
keen_swath<-read.csv ("keen_swath.csv", header=TRUE, strip.white=TRUE)
kelp_quads_biomass<-read.csv ("kelp_quads_biomass.csv", header=TRUE, strip.white=TRUE)

#2.check the structure and variable types.
#Make sure that teh variables are being read  consistently in teh different files 

str(keen_cover)
str(keen_fish)
str(keen_kelp)
str(keen_quads)
str(keen_sites)
str(keen_swath)
str(keen_quads)

is.integer(keen_quads$QUAD)
keen_quads$QUAD<-as.character(keen_quads$QUAD)



# 3. check for the unique values for each data set

unique(keen_cover$YEAR)
unique(keen_cover$MONTH) 
unique(keen_cover$COMMON.DIVISION.NAME)
unique(keen_cover$SPECIES)
unique(keen_cover$SP_CODE) 

#4. check for the unique values for each data set, particularly for variables that are shared for example transect
#In this case transect is consistent in all files except for the keen_sites, which have 4 more transects than the rest ()
unique(keen_cover$TRANSECT)
unique(keen_fish$TRANSECT)
unique(keen_kelp$TRANSECT)
unique(keen_quads$TRANSECT)
unique(keen_swath$TRANSECT)
unique(keen_sites$TRANSECT)

#5. Add a column that specify the method used to collect the data
#keen_cover<-mutate(keen_cover$SAMPLING_METHOD) Error 

#6. Merge data  sets  from all methods in one file (There is a new colunm which identify each file by sampling method)

keen_all_methods_merged<-bind_rows(keen_fish,keen_quads)
keen_all_methods_merged<-bind_rows(keen_all_methods_merged,keen_swath)
keen_all_methods_merged<-bind_rows(keen_all_methods_merged,keen_kelp)
keen_all_methods_merged<-bind_rows(keen_all_methods_merged,keen_cover)

#OUTPUT FILE
write.csv(keen_all_methods_merged, "keen_all_methods_merged.csv")

#6.a.Merge with sitedescription file if needed

#First create a colum that is common betwen the two data sets

keen_sites<-unite_(keen_sites, "YEAR_MONTH_DAY_TRANSECT", c("YEAR","MONTH","DAY","TRANSECT"),remove=FALSE )
keen_all_methods_merged<-unite (keen_all_methods_merged, "YEAR_MONTH_DAY_TRANSECT", c("YEAR","MONTH","DAY","TRANSECT"),remove=FALSE)

#select (keen_sites) to avoid duplicate variables when merging

keen_sites<-select(keen_sites, -NETWORK,-PI,-YEAR,-MONTH,-DAY, -SITE,-TRANSECT)
keen_all_methods_site_merged<-left_join(keen_all_methods_merged, keen_sites, by = "YEAR_MONTH_DAY_TRANSECT")

#OUTPUT FILE
write.csv(keen_all_methods_site_merged, "keen_all_methods_site_merged.csv")

#7# Explore the dataset

unique(keen_all_methods_site_merged$SP_CODE)

#8#Merge datasets for counts only ( keen_fish.	keen_quads,	keen_swath,)
keen_all_counts_merged<-bind_rows(keen_fish,keen_quads)
keen_all_counts_merged<-bind_rows(keen_all_counts_merged,keen_swath)

#8.1. Merge with keen.sites for information about sites

keen_sites<-unite_(keen_sites, "YEAR_MONTH_DAY_TRANSECT", c("YEAR","MONTH","DAY","TRANSECT"),remove=FALSE )
keen_all_counts_merged<-unite (keen_all_counts_merged, "YEAR_MONTH_DAY_TRANSECT", c("YEAR","MONTH","DAY","TRANSECT"),remove=FALSE)

#select (keen_sites) to avoid duplicate variables when merging

keen_sites<-select(keen_sites, -NETWORK,-PI,-YEAR,-MONTH,-DAY,-SITE,-TRANSECT)
keen_all_counts_site_merged<-left_join(keen_all_counts_merged, keen_sites, by = "YEAR_MONTH_DAY_TRANSECT")


#Create a presence absence matrix _Site_year per species_code; only for groups that have a count data set ()
#For the presence absence we can use the data sets keen_fish.	keen_quads.	keen_swath, and keen_cover (but need to transform the species column into presneces), in theory we could also use keen kelp data and percentage cover if we summarise the number of individuals detected 
#For the abundance matrix we can use use only data sets with counts (keen_fish.	keen_quads.	keen_swath)

#The Keen_kelp data does not need to be use for the counts (PRESENCE/ABSENCE), because kelp was counted in the 

#Presence/Anbsence matrix
create.matrix()




###### Exercise filter species of interest

AGCL<-filter(keen_all_methods_site_merged, SP_CODE=="AGCL" )

TAAD<-filter(keen_all_methods_merged, SP_CODE=="TAAD" )


plot(COUNT~YEAR, data=AGCL)

ggplot(AGCL, aes(y=COUNT, x=YEAR, colour=factor(SAMPLING_METHOD)))+
  geom_point (aes(y=COUNT, x=YEAR))

ggplot(TAAD, aes(y=COUNT, x=YEAR, colour=factor(SAMPLING_METHOD)))+
  geom_point (aes(y=COUNT, x=YEAR))









#6create a presence absence data matrix per transect


#4. Transpose from wide to long format each files 

#a<-pivot_longer(keen_kelp,cols="SP_CODE", values_to="count")





#Things to do 
# In the cover data set Create an age column to separate column size with include both size and age (By Jarret recommendation is important to keep it together)
# In the fish data, I am not sure how are variables SIZE and SIZE.FISH different
#create a file of species code and species names maybe in the readme as a table, (Amelia is working on this)

#Unique columns per file 
unique(keen_cover) PERCENT_COVER
unique(keen_fish) QUAD, SIDE, FISH.SIZE,COUNT, AREA
keen_quads QUAD,SIDE, COUNT, AREA
keen_swath QUAD, SIDE, COUNT, AREA
keen_kelp  BLADE_LENGTH_CM, WIDTH_CM, STIPE_LENGTH_CM, WET_WEIGHT, DRY_WEIGHT

View (keen_cover)
#Make sure that they are consistent with the metadata






#Step 4#Combine  the dat of the new season with the master database ##
