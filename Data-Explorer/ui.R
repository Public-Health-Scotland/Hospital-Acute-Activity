
### UI
### Data Explorer script 4 of 5
###
### Original Author: Jaime Villacampa
### Original Date: October 2017
### Last edited by: Mark Hamilton
### Last edited on: 06 March 2019
###
### Written to be run on RStudio Desktop
library(here)

source(here("Data-Explorer/ui_code.R"))

### Visual interface ----
if (credentials$authorisation=="Private"){

secure_app(ui_code)
  
} else if (credentials$authorisation=="Public"){
  
 ui_code
  
}

### END OF SCRIPT ###
