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