httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))


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
library(readxl)

# Server connection things
conn <- config::get("connectionSettings") # get configuration settings


board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))

WQM_Stations_Spatial <- pin_get("ejones/WQM-Stations-Spatial", board = "rsconnect") %>%
  rename("Basin_Name" = "Basin_Code")   # can't have same name different case when using sqldf


# function to create fake data based on user input
fakeDataFunction <- function(stationPlan, userUpload, windowInfo){
  zz <- left_join(dplyr::select(stationPlan, StationID),  
                  dplyr::select(userUpload, `Monitoring Year`, StationID, `Station Type`,`Annual Frequency`), 
                  by = 'StationID')
  dataOut <- windowInfo
  for(i in 1:nrow(zz)){
    dataOut <- bind_rows(dataOut,
                         tibble(Fdt_Sta_Id = zz[i,]$StationID, Fdt_Date_Time = as.Date(paste0(zz[i,]$`Monitoring Year`, '-01-01 12:01:01')),
                                Fdt_Spg_Code = zz[i,]$`Station Type`, fakeData = 1:zz[i,]$`Annual Frequency`))  }
  return(dataOut)
}
#windowInfoNew <- fakeDataFunction(stationPlan, userUpload, windowInfo)


# Update every two years
#function to reanalyze data by vahu6
vahu6Layout <- function(conventionals, # most recent conventionals dataset
                        windowInfo, # extra data needed not covered in conventionals dataset
                        WQM_Stations_Spatial,
                        stationPlan ){
  windowSampleInfo <- conventionals %>% 
    dplyr::select(FDT_STA_ID, FDT_SPG_CODE, FDT_DATE_TIME ) %>% 
    bind_rows(
      dplyr::select(windowInfo, FDT_STA_ID = Fdt_Sta_Id, FDT_SPG_CODE = Fdt_Spg_Code, FDT_DATE_TIME = Fdt_Date_Time)) %>% 
    mutate(Year = year(FDT_DATE_TIME)) %>% #paste0(year(FDT_DATE_TIME)), ' Sample n') %>% 
    group_by(FDT_STA_ID, Year) %>% 
    summarise(`Sample n` = n(),
              `SPG codes` = paste(unique(FDT_SPG_CODE),  collapse = ' | '),
              `SPG codes First` = as.factor(unique(FDT_SPG_CODE)[1])) %>% # keep this first one for mapping purposes
    mutate(
           IR2024 = case_when(Year %in% c(2017:2022) ~ 'IR 2024',
                              TRUE ~ as.character(NA)),
           IR2026 = case_when(Year %in% c(2019:2024) ~ 'IR 2026',
                              TRUE ~ as.character(NA)),
           IR2028 = case_when(Year %in% c(2021:2026) ~ 'IR 2028',
                              TRUE ~ as.character(NA)),
           IR2030 = case_when(Year %in% c(2023:2028) ~ 'IR 2030',
                              TRUE ~ as.character(NA))) %>% 
    group_by(FDT_STA_ID, IR2024 ) %>% 
    mutate(`IR2024 Sample n` = case_when(IR2024 == 'IR 2024' ~ sum(`Sample n`))) %>% 
    ungroup() %>%  group_by(FDT_STA_ID, IR2026 ) %>% 
    mutate(`IR2026 Sample n` = case_when(IR2026 == 'IR 2026' ~ sum(`Sample n`))) %>% 
    ungroup() %>%  group_by(FDT_STA_ID, IR2028 ) %>% 
    mutate(`IR2028 Sample n` = case_when(IR2028 == 'IR 2028' ~ sum(`Sample n`))) %>% 
    ungroup() %>% group_by(FDT_STA_ID, IR2030 ) %>% 
    mutate(`IR2030 Sample n` = case_when(IR2030 == 'IR 2030' ~ sum(`Sample n`))) %>% 
    ungroup() %>%
    dplyr::select(-c(IR2024, IR2026, IR2028, IR2030)) 
  
  
  windowSampleInfoSummary <- dplyr::select(windowSampleInfo, -c(`IR2024 Sample n`, `IR2026 Sample n`,
                                                                `IR2028 Sample n`, `IR2030 Sample n`)) %>% 
    group_by(FDT_STA_ID) %>% 
    pivot_wider(names_from = 'Year', names_prefix = "Year", values_from = c('Sample n', 'SPG codes', 'SPG codes First')) 
  
  
  
  #old skool rename of columns
  names(windowSampleInfoSummary)[2:length(names(windowSampleInfoSummary))] <-
    paste(strsplit(names(windowSampleInfoSummary)[2:length(names(windowSampleInfoSummary))], '_') %>% 
            map_chr(2),
          strsplit(names(windowSampleInfoSummary)[2:length(names(windowSampleInfoSummary))], '_') %>% 
            map_chr(1), sep = ' ')
  
  WQM_Stations_Spatial_New <- bind_rows(WQM_Stations_Spatial, stationPlan) %>% distinct(StationID, .keep_all = T)
  
  # rearrange column order to make sense
  windowSampleInfoFinalSummary <- windowSampleInfoSummary %>% 
    select(sort(tidyselect::peek_vars())) %>% 
    left_join(dplyr::select(windowSampleInfo, FDT_STA_ID, `IR2024 Sample n`, 
                            `IR2026 Sample n`, `IR2028 Sample n`, `IR2030 Sample n`) %>% 
                pivot_longer(cols = c(`IR2024 Sample n`, 
                                      `IR2026 Sample n`, `IR2028 Sample n`, `IR2030 Sample n`), names_to = 'window', values_to = 'value') %>% 
                group_by(FDT_STA_ID, window) %>%
                filter(value > 0) %>% ungroup() %>% 
                group_by(FDT_STA_ID, window) %>% 
                mutate(n = 1:n()) %>% 
                filter(n == 1) %>% dplyr::select(-n) %>% 
                pivot_wider(names_from = 'window', values_from = 'value'), by = 'FDT_STA_ID') %>% 
    left_join(dplyr::select(WQM_Stations_Spatial_New, StationID:Sta_Desc, ASSESS_REG, VAHU6), by = c('FDT_STA_ID' = 'StationID')) %>% 
    dplyr::select( FDT_STA_ID, Sta_Desc, ASSESS_REG, VAHU6, `IR2024 Sample n`, 
                   `IR2026 Sample n`,  `IR2028 Sample n`, `IR2030 Sample n`, everything()) %>% 
    st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
             remove = T, # don't remove these lat/lon cols from df
             crs = 4326)
  return(windowSampleInfoFinalSummary)}
# newMonitoring <- vahu6Layout(conventionals, windowInfoNew, WQM_Stations_Spatial, stationPlan) %>% ungroup() %>% st_as_sf()
