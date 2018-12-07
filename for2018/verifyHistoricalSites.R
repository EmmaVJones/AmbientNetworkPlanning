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
stations <- read_excel('data/MONITOR10132017.xlsx',sheet='lessCitizenSites&unnamedsites')%>%
  filter(MDECLONG<0)
stations_shp <- stations
coordinates(stations_shp) <- ~MDECLONG+MDEC_LAT
proj4string(stations_shp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")                                 

# Load new assessment regions
hucs <- readOGR('C:/GIS/EmmaGIS/WGS84projectionsforLeaflet','AssessmentRegions_VA84')
plot(hucs);plot(stations_shp,add=T)

# Intersect two and add column to stations_shp@data with HUC6 information
huc6poly <- data.frame(STATION=NA,huc6=NA)

for(i in 1:length(stations_shp)){
  print(i)
  STATION <- stations_shp[i,]@data$STATION
  poly <- hucs[stations_shp[i,],]
  if(nrow(poly@data)==0){
    huc6 <- "lat/long doesn't intersect Assessment Layer"
    }else{huc6 <- as.character(poly@data$VAHU6)}
  huc6poly[i,] <- cbind(STATION,huc6)
}

# Now join stations to huc6poly 
together <- left_join(stations,huc6poly,by='STATION')%>%
  mutate(sameAnswer=ifelse(VAHU6==huc6,TRUE,FALSE))%>%
  filter(sameAnswer==FALSE)
