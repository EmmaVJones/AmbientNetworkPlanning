# Libraries
library(tidyverse)
library(rgdal)
library(readxl)


# Statewide Assessment Layer
state <- readOGR('C:/GIS/EmmaGIS/WGS84projectionsforLeaflet','AssessmentRegions_VA84')

BRRO <- subset(state, ASSESS_REG=="BRRO")
BRRO1 <- BRRO@data
writeOGR(BRRO, dsn="data", layer="BRRO", driver="ESRI Shapefile")

# Bring in 2018 IR data pull (2011-2016 data), will still need to bring in 2017 sites
# Goal: filter out all BRRO sites to get a list of which sites were sampled each year
#  and frequency if possible
conventionals <- read_excel('C:/HardDriveBackup/IR/IR2018/CONVENTIONALS_20171010.xlsx') # I wanted to use read_csv bc so much faster than read_excel but the datetime columsn could not be easily fixed using read_csv

unique(conventionals$Deq_Region) # some NA's so make sure figure out which ones are in BRRO before discarding data
naRegion <- filter(conventionals,is.na(Deq_Region)) %>%
  filter(STA_REC_CODE %in% c("SCRO","WCRO"))

BRROdata <- filter(conventionals, Deq_Region== "Blue Ridge")
BRROdata <- rbind(BRROdata,naRegion) %>%
  fill(Deq_Region) %>% # fill in BRRO for all NA's
  select(FDT_STA_ID:FDT_DATE_TIME, STA_LV2_CODE:STA_CBP_NAME)%>%# Drop each site data bc don't care about that for this exercise
  mutate(FDT_DATE = as.POSIXct(as.character(FDT_DATE_TIME),format="%Y-%m-%d")) # strip off HMS information
BRROdata$FDT_YEAR <- lubridate::year(BRROdata$FDT_DATE_TIME) # can't do in mutate statement for some reason

# split and run metrics to get unique stations and sampling stats associated
BRROdatalist <- split(BRROdata,f=BRROdata$FDT_STA_ID) # Split before FDT_STA_ID becomes factor

BRROdata_sites <- data.frame(FDT_STA_ID=NA,STA_LV3_CODE=NA,STA_LV1_CODE=NA,STA_REC_CODE=NA,Deq_Region=NA,STA_DESC=NA,FDT_SSC_CODE=NA,FDT_SPG_CODE=NA,    
                       STA_LV2_CODE=NA,Latitude=NA,Longitude=NA,Majorbasincode=NA,Majorbasinname=NA,Basin=NA,Subbasin=NA,Huc6_Huc_8=NA,
                       Huc6_Huc_8_Name=NA,Huc6_Name=NA,Huc6_Huc_12=NA,Huc6_Huc_12_Name=NA,Huc6_Vahu5=NA,Huc6_Vahu6=NA,
                       STA_CBP_NAME=NA,nObservations=NA,yearsSampled=NA,nYearsSampled=NA)

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

saveRDS(BRROdata_sites,'data/BRROdata2011to2016_sites.RDS')



# Change certain variables to factor
BRROdata$STA_LV3_CODE <- as.factor(BRROdata$STA_LV3_CODE)
BRROdata$STA_LV1_CODE <- as.factor(BRROdata$STA_LV1_CODE)
BRROdata$STA_REC_CODE <- as.factor(BRROdata$STA_REC_CODE)
BRROdata$FDT_SPG_CODE <- as.factor(BRROdata$FDT_SPG_CODE)
BRROdata$Basin <- as.factor(BRROdata$Basin)
BRROdata$Subbasin <- as.factor(BRROdata$Subbasin)
BRROdata$Huc6_Vahu6 <- as.factor(BRROdata$Huc6_Vahu6)
BRROdata$FDT_STA_ID <- as.factor(BRROdata$FDT_STA_ID)

saveRDS(BRROdata,'data/BRROdata2011to2016.RDS')


