source('global.R')

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
assessmentRegionsVAHU6 <- st_read( 'data/GIS/AssessmentRegions_VA84_basins.shp') %>% 
  filter(ASSESS_REG == 'BRRO') %>% 
  dplyr::select(VAHU6, ASSESS_REG)





shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
})