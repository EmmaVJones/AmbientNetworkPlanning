## built in R 3.6.1 'Action of the Toes'

# Libraries
library(tidyverse)
library(readxl)
library(mapview)
library(sf)

# Statewide Assessment Layer
#state <- st_read('G:/evjones/GIS/EmmaGIS/WGS84projectionsforLeaflet/AssessmentRegions_VA84.shp')
#BRRO <- subset(state, ASSESS_REG=="BRRO") 
#writeOGR(BRRO, dsn="data", layer="BRRO", driver="ESRI Shapefile")

BRRO <- st_read('data/BRRO.shp')

# Bring in 2020 IR data pull (2013-2018 data), will still need to bring in 2017&2018 sites
# Goal: filter out all BRRO sites to get a list of which sites were sampled each year
#  and frequency if possible
conventionals <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/R&S_app_v4/data/final2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190305.csv') 
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

summary(conventionals$FDT_DATE_TIME2)

# Now add recent data (2018-Nov 2019- the day Roger made the data pull)
# already limited to BRRO (SCRO and WCRO)
conventionals2 <- read_excel('data/CEDSWQM_CONVENTIONALS_2018+.xlsx')
conventionals2$FDT_DATE_TIME2 <- as.POSIXct(conventionals2$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
conventionals2$FDT_DATE_TIME <- as.character(conventionals2$FDT_DATE_TIME) # for smashing with original conventionals
summary(conventionals2$FDT_DATE_TIME2)

# filter data to just 2019 to not duplicate data from 2018
conventionals2019 <- filter(conventionals2, FDT_DATE_TIME2 > '2018-12-31 23:59:00')
summary(conventionals2019$FDT_DATE_TIME2)
# cool.
glimpse(conventionals2019)

# what is in conventionals that isn't in conventionals2019??
names(conventionals)[!names(conventionals) %in% names(conventionals2019)]




conventionalsAll <- bind_rows(conventionals,conventionals2019) %>%
  # get groundwater sites out of here
  filter(FDT_SPG_CODE != 'GW')




# Just get BRRO data, previously this was done by the Deq_Region == 'Blue Ridge' column, but that is
# known to contain errors, so doing it spatially this time
conventionalsAll_sf <- conventionalsAll %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>% # can't make sf object with missing lat long, if missing from CEDS then not included
  distinct(FDT_STA_ID, .keep_all = T) %>% # only care about unique stations
  st_as_sf(coords = c("Longitude", "Latitude"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add projection, needs to be geographic for now bc entering lat/lng

BRROconventionals <- st_intersection(conventionalsAll_sf, BRRO)

#mapview(BRRO) +
#  mapview(BRROconventionals)


# Roger did not include basin or subbasin information in 2020 IR conventionals pull, which is important ot the app interface,
# so I need to spatially join that information to each site

basins <- st_read('F:/evjones/GIS/EmmaGIS/WGS84projectionsforLeaflet/VAbasins.shp')


BRROconventionals <- st_join(BRROconventionals,select(basins, BASIN)) %>%
  mutate(Subbasin = Huc6_Huc_8_Name) %>%
  rename('Basin' = 'BASIN')



## Now get just BRRO data from conventionals
## some NA's so make sure figure out which ones are in BRRO before discarding data
#unique(conventionalsAll$Deq_Region) 
naRegion <- filter(conventionalsAll,is.na(Deq_Region)) %>%
  filter(STA_REC_CODE %in% c("SCRO","WCRO"))



BRROdata <- filter(conventionalsAll,FDT_STA_ID %in% BRROconventionals$FDT_STA_ID) %>%
  # get basin information
  left_join(select(BRROconventionals, FDT_STA_ID, Basin, Subbasin) , by = 'FDT_STA_ID') %>%
  select (-geometry)


BRROdata <- BRROdata %>%
  fill(Deq_Region) %>% # fill in BRRO for all NA's
  # match previous conventionals data format
  select(FDT_STA_ID:FDT_COMMENT,Latitude:Subbasin)%>%# Drop each site data bc don't care about that for this exercise
  mutate(FDT_DATE = as.POSIXct(as.character(FDT_DATE_TIME2),format="%Y-%m-%d")) # strip off HMS information
BRROdata$FDT_YEAR <- lubridate::year(BRROdata$FDT_DATE) # can't do in mutate statement for some reason



write.csv(BRROdata, 'data/conventionalsBRRO2013_Nov2019.csv',row.names = F)




# split and run metrics to get unique stations and sampling stats associated
BRROdatalist <- split(BRROdata,f=BRROdata$FDT_STA_ID) # Split before FDT_STA_ID becomes factor

#BRROdata_sites <- data.frame(FDT_STA_ID=NA,STA_LV3_CODE=NA,STA_LV1_CODE=NA,STA_REC_CODE=NA,Deq_Region=NA,STA_DESC=NA,FDT_SSC_CODE=NA,FDT_SPG_CODE=NA,    
#                             STA_LV2_CODE=NA,Latitude=NA,Longitude=NA,Majorbasincode=NA,Majorbasinname=NA,Basin=NA,Subbasin=NA,Huc6_Huc_8=NA,
#                             Huc6_Huc_8_Name=NA,Huc6_Name=NA,Huc6_Huc_12=NA,Huc6_Huc_12_Name=NA,Huc6_Vahu5=NA,Huc6_Vahu6=NA,
#                             STA_CBP_NAME=NA,nObservations=NA,yearsSampled=NA,nYearsSampled=NA)

BRROdata_sites <- BRROdata[1,] %>%
  select(-c(FDT_DATE_TIME,FDT_DATE,FDT_YEAR))%>% #remove redundant date/time info
  mutate(nObservations=NA,yearsSampled=NA,nYearsSampled=NA) 
BRROdata_sites[1,] <- NA

for(i in 1:length(BRROdatalist)){
  dat <- BRROdatalist[i][[1]]
  nObservations <- nrow(dat)
  yearsSampled <- unique(dat$FDT_YEAR)
  nYearsSampled <- length(yearsSampled)
  
  dat <- mutate(dat,nObservations=nObservations,
                yearsSampled=toString(yearsSampled),
                nYearsSampled=nYearsSampled)%>%
    select(-c(FDT_DATE_TIME,FDT_DATE,FDT_YEAR))%>% #remove redundant date/time info
    filter(row_number()==1)
  
  BRROdata_sites[i,] <- dat
}
# REorder columns to make it easier to read
BRROdata_sites <- select(BRROdata_sites,FDT_STA_ID,nObservations,yearsSampled,nYearsSampled,STA_DESC,FDT_SPG_CODE,everything())
# Change certain variables to factor
BRROdata_sites$FDT_SPG_CODE <- as.factor(BRROdata_sites$FDT_SPG_CODE)

saveRDS(BRROdata_sites,'data/BRROdata2013toNov2019_sites.RDS')





# Change certain variables to factor
BRROdata$STA_LV3_CODE <- as.factor(BRROdata$STA_LV3_CODE)
BRROdata$STA_LV1_CODE <- as.factor(BRROdata$STA_LV1_CODE)
BRROdata$STA_REC_CODE <- as.factor(BRROdata$STA_REC_CODE)
BRROdata$FDT_SPG_CODE <- as.factor(BRROdata$FDT_SPG_CODE)
BRROdata$Basin <- as.factor(BRROdata$Basin)
BRROdata$Subbasin <- as.factor(BRROdata$Subbasin)
BRROdata$Huc6_Vahu6 <- as.factor(BRROdata$Huc6_Vahu6)
BRROdata$FDT_STA_ID <- as.factor(BRROdata$FDT_STA_ID)

saveRDS(BRROdata,'data/BRROdata2013toNov2019.RDS')
