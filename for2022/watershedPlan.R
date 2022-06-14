library(tidyverse)
library(sf)
library(readxl)

assessmentRegionsVAHU6 <- st_read( 'data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  filter(ASSESS_REG == 'BRRO') %>%
  dplyr::select(VAHU6, ASSESS_REG)

larryPlan <- read_excel('C:/HardDriveBackup/Ambient Sites/2017 Ambient/20117-18 MONPLAN 11012016_LW.xlsx',
                        sheet = 'Watershed plan through 2020')[,1:20]
names(larryPlan) <- c('Region',	'VAHU6', 'Type',	'ProbMon',	paste0('x', 2005:2020))


# does 2021 look like 2015?
x2021 <- readRDS('data/processedData/stations2021.RDS') %>% 
  dplyr::select(FDT_STA_ID, VAHU6, `Year2021 SPG codes`) %>% 
  st_drop_geometry()
x2021summary <- dplyr::select(x2021, -FDT_STA_ID) %>% 
  group_by(VAHU6) %>% 
  summarise(x2021 = paste(unique(`Year2021 SPG codes`),  collapse = ' | '))


larryPlan2 <- left_join(larryPlan, x2021summary, by = 'VAHU6') %>% 
  filter(VAHU6 %in% assessmentRegionsVAHU6$VAHU6) # only keep current BRRO VAHU6

# are all the new VHAu6s in larry list?
unique(larryPlan2$VAHU6) %in% unique(assessmentRegionsVAHU6$VAHU6)
unique(assessmentRegionsVAHU6$VAHU6)[!unique(assessmentRegionsVAHU6$VAHU6) %in% unique(larryPlan2$VAHU6)]

larryPlan2 <- bind_rows(larryPlan2, tibble(VAHU6 = c('JU16', 'NE89', 'NE90'))) %>% 
  arrange(VAHU6)

write_csv(larryPlan2, 'watershedplan.csv', na = "")
