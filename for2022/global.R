library(tidyverse)
library(shiny)
library(shinybusy)
library(leaflet)
library(leaflet.extras)
library(inlmisc)
library(DT)
library(config)
library(sf)
library(plotly)
library(lubridate)
library(pool)
library(geojsonsf)
library(pins)
library(dbplyr)

# Server connection things
conn <- config::get("connectionSettings") # get configuration settings


board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))

## Connect to ODS production
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#Driver = "SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

# For deployment on the R server: Set up pool connection to production environment
# pool <- dbPool(
#   drv = odbc::odbc(),
#   Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
#   # Production Environment
#   Server= "DEQ-SQLODS-PROD,50000",
#   dbname = "ODS",
#   UID = conn$UID_prod,
#   PWD = conn$PWD_prod,
#   #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
#   #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
#   # Test environment
#   #Server= "WSQ04151,50000",
#   #dbname = "ODS_test",
#   #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
#   #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
#   trusted_connection = "yes"
# )

onStop(function() {
  poolClose(pool)
})

WQM_Stations_Spatial <- pin_get("ejones/WQM-Stations-Spatial", board = "rsconnect") %>%
  rename("Basin_Name" = "Basin_Code")   # can't have same name different case when using sqldf
WQM_Stations_Spatial_BRRO <-  filter(WQM_Stations_Spatial, ASSESS_REG == 'BRRO')

conventionals <- readRDS('data/IR2022conventionalsBRRO.RDS')  
windowSampleInfoFinalSummary <- readRDS('data/processedData/AllStations.RDS')
x2015 <- readRDS('data/processedData/stations2015.RDS')
x2016 <- readRDS('data/processedData/stations2016.RDS')
x2017 <- readRDS('data/processedData/stations2017.RDS')
x2018 <- readRDS('data/processedData/stations2018.RDS')
x2019 <- readRDS('data/processedData/stations2019.RDS')
x2020 <- readRDS('data/processedData/stations2020.RDS')
x2021 <- readRDS('data/processedData/stations2021.RDS')
ir2022 <- readRDS('data/processedData/vahu62022.RDS')
ir2024 <- readRDS('data/processedData/vahu62024.RDS')
ir2026 <- readRDS('data/processedData/vahu62026.RDS')
