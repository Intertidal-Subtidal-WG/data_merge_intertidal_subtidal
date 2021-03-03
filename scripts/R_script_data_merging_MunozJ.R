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


library(tidyverse)
library(tidyr)
library(plyr)
library(ggplot2)
library(dplyr)
library(data.table)
library(extrafont)
library(visreg)
library(lubridate)
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

#2.check the structure and variable type.
#Make sure that teh variables are being read  consistently in teh different files 

str(keen_cover)
str(keen_fish)
str(keen_kelp)
str(keen_quads)
str(keen_sites)
str(keen_swath)
str(keen_quads)



#Step 3. check for the unique values for each variable
#Make sure that they are consistent with the metadata

unique(data1995_2019$date_yymmdd)
unique(data1995_2019$year)                         
unique(data1995_2019$month)                        
unique(data1995_2019$day)                          
unique(data1995_2019$ julian_day )                  
unique(data1995_2019$ survey_start_time)           
unique(data1995_2019$ survey_end_time)              
unique(data1995_2019$ zone_start_time)              
unique(data1995_2019$ zone_end_time)                
unique(data1995_2019$ effort_per_zone_min)          
unique(data1995_2019$ zone_BIEAP)                  
unique(data1995_2019$ zone_BIEAP_description)      
unique(data1995_2019$ latitude)                  
unique(data1995_2019$ longitud)   
unique(data1995_2019$ species_code)  
unique(data1995_2019$ species_common_name)         
unique(data1995_2019$ species_scientific_name)     
unique(data1995_2019$ order)                        
unique(data1995_2019$ family)                      
unique(data1995_2019$ number_individuals)          
unique(data1995_2019$ max_detection_distance_m)     
unique(data1995_2019$ males)                      
unique(data1995_2019$ females)                      
unique(data1995_2019$ juveniles)                   
unique(data1995_2019$ unknown_sex)                  
unique(data1995_2019$ pairs)                        
unique(data1995_2019$ observer_1)                   
unique(data1995_2019$ observer_2)                   
unique(data1995_2019$ observer_3)                   
unique(data1995_2019$ cloud_cover)                 
unique(data1995_2019$ precipitation_mm) 
unique(data1995_2019$ visibility_m)                 
unique(data1995_2019$ wind_beaufort_description)    
unique(data1995_2019$ seastate_beaufort_description)
unique(data1995_2019$ tide_m)                       
unique(data1995_2019$ temperature_c)              
unique(data1995_2019$ disturbance)                 
unique(data1995_2019$ nasal_disk )                  
unique(data1995_2019$ comments) 

#Step 4#Combine  the dat of the new season with the master database ##
