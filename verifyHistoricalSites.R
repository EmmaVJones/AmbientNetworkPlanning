## This script double checks the HUC identification of all stations (less Cit Mon and stations
##  without StationID's) from Mike's ID305B.mbd MASTER station table. 

# Load Libraries
library(tidyverse)
library(rgdal)
library(readxl)

# Load Station Table 
#  To avoid connecting to .mbd from R (meaning using 32bit R) I first exported MONITOR table
#  to excel, sorted in excel by Citizen monitoring data level, removed all citizen monitoring 
#  stations, and then also deleted all rows that did not have a StationID or had numeric ones
#  (e.g. something that I couldn't connect to a site in CEDS)
stations <- read_excel('data/MONITOR10132017.xlsx',sheet='lessCitizenSites&unnamedsites')
