################################################################
### Functions
### Data Explorer script 1 of 5
###
### Original Author: Jack Hannah
### Original Date: 01 June 2018
### Last edited by: Jack Hannah
### Last edited on: 01 June 2018
###
### Written to be run on RStudio Desktop
###
### Packages required:
### dplyr (for data manipulation)
###
### This script defines several functions to be called
### in the next script (data_preparation.R) which will
### be used to manipulate a series of base files



### Section 1: Housekeeping ----


# 1 - Load libraries
library(dplyr)


# 2 - Define base filepath
base_filepath <- paste("//stats/pub_incubator/01 Acute Activity",
                       "/wrangling/data/base_files/",
                       sep = "")



### Section 2: Functions ----


# Function 1 - Residence files
# This function contains the data manipulation steps
# to be applied to all residence (res) files
res <- function(file){
  file %>%
  filter(!(loc_name %in% c("Scotland",
                           "Golden Jubilee National Hospital"))) %>%
    mutate(geo_type = case_when(
      hb_name %in% c("Other", "Null") | loc_name == "Other"
      ~ "Other",
      substr(loc_code, 1, 3) == "S12"
      ~ "Council area of residence",
      TRUE
      ~ "Health board of residence"
    )) #%>%
    #mutate_if(is.character, as.factor) %>%
    #droplevels()
}