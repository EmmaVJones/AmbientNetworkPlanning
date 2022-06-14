regionData <- readRDS('data/BRROregionInfo.RDS')

userUpload <- read_excel('uploadData/Draft  MonPlan 2023-2024EVJ.xlsx', sheet = 'Sample Plan') 


stationRaw <- dplyr::select(userUpload, StationID, lat = Latitude, lng = Longitude) %>% # make new lat/lng fields in case user info here
  filter(!is.na(StationID)) %>%  # drop extra excel rows 
  left_join(WQM_Stations_Spatial, by = 'StationID')
if(nrow(filter(stationRaw, is.na(Latitude) | is.na(Longitude))) > 0 ){
  # Get spatial info for new stations
  fix <- filter(stationRaw, is.na(Latitude) | is.na(Longitude)) %>% 
    st_as_sf(coords = c("lng", "lat"),  # make spatial layer using these columns
             remove = F, # don't remove these lat/lon cols from df
             crs = 4326) %>% 
    st_intersection(dplyr::select(unrestrictedAssessmentRegionVAHU6Subbasin, ASSESS_REG, VAHU6)) %>% 
    mutate(ASSESS_REG = ASSESS_REG.1,
           VAHU6 = VAHU6.1,
           Latitude = lat,
           Longitude = lng) %>% 
    dplyr::select(-c(ASSESS_REG.1, VAHU6.1, lat,lng))
  stationPlan <- bind_rows(
    filter(stationRaw, !StationID %in% fix$StationID),
    fix %>% st_drop_geometry()) 
} else {
  stationPlan <- stationRaw
}


windowInfoNew <- fakeDataFunction(stationPlan, userUpload, regionData$windowInfo)

newMonitoring <- vahu6Layout(regionData$conventionals, windowInfoNew, WQM_Stations_Spatial, stationPlan) %>% ungroup()


newIR2026 <- left_join(st_as_sf(regionData$ir2022) %>% 
                     dplyr::select(VAHU6, ASSESS_REG), 
                   newMonitoring %>% 
                     filter(!is.na(`IR2026 Sample n`)) %>% 
                     group_by(VAHU6) %>% 
                     summarise(`n Stations` = length(unique(FDT_STA_ID))) %>% 
                     left_join(
                       newMonitoring %>% 
                         group_by(VAHU6) %>% 
                         summarise(`n Samples` = sum(`IR2026 Sample n`, na.rm = T)) , by = 'VAHU6'  ),
                   by = 'VAHU6') %>% 
           mutate(`IR Sampling Status` = as.factor(case_when(is.na(`n Stations`) ~ 'Need Station', 
                                               between(`n Samples`, 1, 9) ~ '< 10 samples',
                                               `n Samples` >9 ~ 'Fine'))) %>% 
    st_as_sf()


newIR2028 <- left_join(st_as_sf(regionData$ir2022) %>% 
                     dplyr::select(VAHU6, ASSESS_REG), 
                   newMonitoring %>% 
                     filter(!is.na(`IR2028 Sample n`)) %>% 
                     group_by(VAHU6) %>% 
                     summarise(`n Stations` = length(unique(FDT_STA_ID))) %>% 
                     left_join(
                       newMonitoring %>% 
                         group_by(VAHU6) %>% 
                         summarise(`n Samples` = sum(`IR2028 Sample n`, na.rm = T)) , by = 'VAHU6'  ),
                   by = 'VAHU6') %>% 
           mutate(`IR Sampling Status` = as.factor(case_when(is.na(`n Stations`) ~ 'Need Station', 
                                               between(`n Samples`, 1, 9) ~ '< 10 samples',
                                               `n Samples` >9 ~ 'Fine'))) %>% 
    st_as_sf() 


newYear <- filter_at(newMonitoring, c(length(newMonitoring)-2), all_vars(!is.na(.))) %>% # grab second to last column, which should be the new year sample counts
    st_as_sf() %>% 
    mutate(UID = paste0(FDT_STA_ID,' ', unique(userUpload$`Monitoring Year`)))
