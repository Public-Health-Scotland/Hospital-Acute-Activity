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


# Section 2.1: Residence files ----
# This function contains the data manipulation steps
# to be applied to all residence (res) files
res <- function(df){
  df %>%
    
  # Exclude Scotland and Golden Jubilee
  # to avoid duplicates when merging
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



# Section 2.2: Treatment files ----
# This function contains the data manipulation steps
# to be applied to all treatment (treat) files
treat <- function(df){
  df %>%
    mutate(geo_type = case_when(
      substr(loc_code, 1, 3) == "S08"
      ~ "Health board of treatment",
      loc_code == "scot"
      ~ "Scotland",
      hb_name %in% c("Other", "Null") | loc_name == "Other"
      ~ "Other",
      TRUE
      ~ "Hospital of treatment"
    ))
}



# Section 2.3: Combining inpatient files ----
# This function contains the steps for combining
# and manipulating inpatient files
inp <- function(df_1, df_2){
  bind_rows(df_1, df_2) %>%
    mutate(avlos = round(avlos, 1),
           file = "Inpatients/Day Cases") %>%
    drop_na(measure)
}