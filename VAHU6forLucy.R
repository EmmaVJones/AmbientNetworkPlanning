library(tidyverse)
library(sf)
library(pins)
library(readxl)

pins::board_register(board = "rsconnect")

# if the above doesn't work, try this
# use API key to register board
#library(config)
#conn <- config::get("connectionSettings") # get configuration settings

#board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
#                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


stations <- pin_get('ejones/WQM-Station-Full')

larryStations <- read_excel('C:/Users/wmu43954/Downloads/TMIPSummary_2021.xlsx')

larryStationsJoin <- larryStations %>%
  left_join(stations, by = c('Station' = 'WQM_STA_ID')) %>%
  distinct(Station, .keep_all = T) %>%
  dplyr::select(names(larryStations), BASINS_VAHU6)
# some still missing, which is weird since this dataset should compile all available WQM stations