source('global.R')


load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}

BRRO <- readRDS('data/BRROdata2011to2016.RDS')


shinyServer(function(input, output, session) {
  
  # display the loading feature until data
  load_data()
  
  
})