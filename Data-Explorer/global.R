############################################################
## Code name - global.R
## Data Release - Quarterly Data Explorer
## Latest Update: Ruth Gordon, February 2022
##
## Written/run on - R Studio SERVER
## R version - 3.6.1
##
## This script includes all packages, datasets and other
## values used by both the UI and server files
##
############################################################
### Section 1: Housekeeping ----

# 1.1 - Load libraries
library(shiny)
library(DT)
library(googleVis)
library(htmltools)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(zoo)
library(lubridate)
library(shinyWidgets)
library(shinymanager)

## Update each quarter
data_up_to <- "30-september-2021"

# 1.2 - Define filepath
# Note that this is the same folder as is specified in the data_preparation
# script, but the path is defined differently due to the way shiny handles
# working directories
rds_filepath <- ("./data/")

# 1.3 - Credentials
# Delete any existing credentials
if(exists("credentials")) rm(credentials)

# Expected location of credentials
credential_filepath = paste0(rds_filepath,"admin/credentials.csv")

# Read in credentials file and set auth to private
credentials <- list(authorisation = "Private")
credentials <- merge(credentials, read_csv(credential_filepath,
                                           col_types = cols()))

# Uncomment this line to remove authentication:
# credentials <- list(authorisation = "Public")

# Publication URL
pub_url = paste0("https://publichealthscotland.scot/publications/",
                 "acute-hospital-activity-and-nhs-beds-information-quarterly/",
                 "acute-hospital-activity-and-nhs-beds-information-quarterly-quarter-ending-",
                 data_up_to, "/")

############################################################
### Section 2: Loading Data ----

# Beds data
data_bed <- readRDS(paste0(rds_filepath, "beds.rds"))

# Specialty data
data_spec <- readRDS(paste0(rds_filepath, "spec.rds"))

# SIMD data
data_simd <- readRDS(paste0(rds_filepath, "simd.rds"))

# Time Trend data
data_trend <- readRDS(paste0(rds_filepath, "trend.rds")) %>%
  mutate(quarter_middle = dmy(quarter_date) - ddays(45))

# Population Pyramid data
data_pyramid <- readRDS(paste0(rds_filepath, "pyramid.rds"))

# Map data (outpatients)
# Not in use at the moment
# data_map_op <- readRDS(paste0(rds_filepath, "map_op.rds"))

# Cross-Boundary data
data_cbf <- readRDS(paste0(rds_filepath, "cbf.rds"))


############################################################
### Section 3: Other Values ----

# All of these values are used for dropdown selections in different
# parts of the app

# 3.1 - Cross-Boundary diagram
# Time trend data is used to get the two types of patients, but any
# of the datasets with a 'file' column could have been used
data_type <- data_trend %>%
  distinct(file) %>%
  pull(file)

# 3.2 - Latest financial quarter
# Again use time trend data
latest_quarter <- data_trend %>%
  filter(quarter_date_last == max(quarter_date_last)) %>%
  distinct(quarter_date_last, quarter_name) %>%
  pull(quarter_name)

# 3.3 - Dropdown choices in 'Table' tab
file_types <-  c("Beds",
                 "Inpatients/Day cases - Age/sex",
                 "Inpatients/Day cases - Cross boundary flow",
                 "Inpatients/Day cases - Time trend",
                 "Inpatients/Day cases - Specialty",
                 "Inpatients/Day cases - Deprivation (SIMD)",
                 "Outpatients - Age/sex",
                 "Outpatients - Cross boundary flow",
                 "Outpatients - Time trend",
                 "Outpatients - Specialty",
                 "Outpatients - Deprivation (SIMD)")

# 3.4 - Geography types

# 3.4.1 - Excluding 'other' from SIMD tab
# no longer want to present other in the SIMD tab
# this just takes all the types of geographies hbres,hbt,
# hospital, scotland for use in drop downs.
geo_type <- data_trend %>%
  distinct(geo_type) %>%
  pull(geo_type)

# 3.4.2 - Excluding 'Other'
# Used in 'Time Trend' tab, as, at present, 'Other' is not
# included as part of the time trend data
geo_type_trend <- data_trend %>%
  distinct(geo_type) %>%
  pull(geo_type)

# 3.5 - Time Trend services
trend_service <- data_trend %>%
  distinct(measure) %>%
  pull(measure)

# 3.6 - Time Trend measures
trend_measure <- c("Number of stays / appointments",
                   "Total length of stay (days)",
                   "Mean length of stay (days)",
                   "Did not attend rate (%)")

# 3.7 - Pyramid services
pyramid_service <- data_pyramid %>%
  distinct(measure) %>%
  pull(measure)


############################################################
### Section 4: Plot Parameters ----

# 4.1 - Time Trend colour palette
trend_pal <- c("#004785", "#4c7ea9", "#99b5ce",
               "#00a2e5", "#4cbeed", "#99daf5")

# 4.2 - Cross-Boundary colour palette for Sankey diagram
# in the Cross-boundary tab
colours_node <- rep("CornflowerBlue", times = 28)

colours_node_array <- paste0("[", paste0("'", colours_node,
                                         "'", collapse = ','),"]")

opts <- paste0("{
               node: { colours: ", colours_node_array ," }
               }" )


############################################################

### END OF SCRIPT ###