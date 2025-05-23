############################################################
## Code name - functions.R
## Data Release - Quarterly Data Explorer
## Latest Update: Ruth Gordon, February 2022
##
## Written/run on - R Studio SERVER
## R version - 3.6.1
##
## This script defines several functions to be called in the 
## script 'data_preparation.R', which will be used to 
## manipulate and combine a series of base files

############################################################
### Section 1: Housekeeping ----

# 1.1 - Load libraries
library(dplyr)
library(tidyr)
library(stringi)
library(lubridate)

############################################################
### Section 2: Functions ----

# Section 2.1: Residence files ----
# This function contains the data manipulation steps
# to be applied to all residence (res) files
res <- function(df) {
  df %>%
# Exclude Scotland and Golden Jubilee to avoid duplicates when merging
  filter(!(loc_name %in% c("Scotland",
                           "Golden Jubilee National Hospital"))) %>%
    mutate(geo_type = case_when(
      hb_name %in% c("Other", "Null") | loc_name == "Other"
      ~ "Other",
      substr(loc_code, 1, 3) == "S12"
      ~ "Council area of residence",
      TRUE
      ~ "NHS Board of residence"
    ))
}

# Section 2.2: Treatment files ----
# This function contains the data manipulation steps
# to be applied to all treatment (treat) files
treat <- function(df) {
  df %>%
    mutate(geo_type = case_when(
      substr(loc_code, 1, 3) == "S08" | loc_code %in% c("SB0801","SN0811")
      | hb_name %in% ("Unknown Health Board")
        ~ "NHS Board of treatment",
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
comb_inp <- function(df_1, df_2) {
  bind_rows(df_1, df_2) %>%
    mutate(avlos = round(avlos, 1),
           file = "Inpatients/day cases") %>%
# Remove a few random null measure cases
    drop_na(measure)
}

# Section 2.4: Combining outpatient files ----
# This function contains the steps for combining
# and manipulating outpatient files
comb_outp <- function(df_1, df_2) {
  bind_rows(df_1, df_2) %>%
    rename(measure = appt_type) %>%
    mutate(file = "Outpatients",
           dna_rate = round(dna_rate, 1))
}

# Section 2.5: Combining inpatient and outpatient files ----
# This function contains the steps for combining
# the inpatient and outpatient files created using
# the previous two functions
comb_all <- function(df_1, df_2) {
  bind_rows(df_1, df_2) %>%
# Recode measure into a more informative descrition and/or to 
# remove capital letters from mid-sentence words
    mutate(measure = recode(
      measure,
      "DNA" = "Did not attend outpatient appointments",
      "Transfers" = "Inpatient transfers",
      "All" = "All outpatients",
      "New" = "New outpatients",
      "Return" = "Return outpatients",
      "All Inpatients" = "All inpatients",
      "All Inpatients and Day cases" = "All inpatients and day cases",
      "All Day cases" = "All day cases",
      "Emergency Inpatients" = "Emergency inpatients",
      "Elective Inpatients" = "Elective inpatients",
      "Not Specified" = "Not specified - inpatients"
    ))
}

# Section 2.6: Formatting dates ----
# The correct date format for files in this instance is dd/mm/yyyy
# Some files contain dates in the form dd-mm-yy
# This function converts those dates into the correct format
convert_dates <- function(df) {
  df %>%
    mutate(quarter_date = gsub("-", "/", quarter_date),
           quarter_date = stri_replace_last_fixed(
             quarter_date, "/", "/20"
           ))
}


############################################################

### END OF SCRIPT ###