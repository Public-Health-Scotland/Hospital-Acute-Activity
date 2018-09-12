################################################################
### Global
### Data Explorer script 3 of 5
###
### Original Author: Jaime Villacampa
### Original Date: October 2017
### Last edited by: Jack Hannah
### Last edited on: 12 September 2018
###
### Written to be run on RStudio Desktop
###
### Packages required:
### shiny (for the interactive framework);
### DT (for data tables);
### googleVis (for Sankey charts);
### htmltools (for tooltips);
### leaflet (for maps);
### plotly (for interactive plots);
### rgdal (for reading shapefiles);
### dplyr and tidyr (for data manipulation);
### readr (for writing csv files);
### zoo and lubridate (for dates)
###
### This script includes all packages, datasets and other
### values used by both the UI and server files



### Section 1: Housekeeping ----


# 1.1 - Load libraries
library(shiny)
library(DT)
library(googleVis)
library(htmltools)
library(leaflet)
library(plotly)
# library(rgdal) (Only needed for mapping - not in use at present)
library(dplyr)
library(tidyr)
library(readr)
library(zoo)
library(lubridate)

# 1.2 - Define base filepath
base_filepath <- paste0("//stats/SecondaryCare/Quarterly Publication/TPP",
                        "/1_Dec17/data/output/")



### Section 2: Loading Data ----


# Beds data
data_bed <- readRDS(paste0(
  base_filepath,
  "R files/beds.rds"))

# Specialty data
data_spec <- readRDS(paste0(
  base_filepath,
  "R files/spec.rds"))

# SIMD data
data_simd <- readRDS(paste0(
  base_filepath,
  "R files/simd.rds"))

# Time Trend data
data_trend <- readRDS(paste0(
  base_filepath,
  "R files/trend.rds"))

# Population Pyramid data
data_pyramid <- readRDS(paste0(
  base_filepath,
  "R files/pyramid.rds"))

# Map data (outpatients)
# Not in use at the moment
# data_map_op <- readRDS(paste0(
#   base_filepath,
#   "R files/map_op.rds"))

# Cross-Boundary data

# Inpatients
data_cbf_ip <- readRDS(paste0(
  base_filepath,
  "R files/cbf_ip.rds"))

# Outpatients
data_cbf_op <- readRDS(paste0(
  base_filepath,
  "R files/cbf_op.rds"))



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


# 3.4.1 - Including 'Other'
# Used in most tabs
# Use SIMD dataset, as this contains 'Other' type
geo_type <- data_simd %>%
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



### Section 4: Plot Parameters ----


# 4.1 - Time Trend colour palette
trend_pal <- c("#004785", "#4c7ea9", "#99b5ce",
               "#00a2e5", "#4cbeed", "#99daf5")


# 4.2 - Cross-Boundary colour palette for Sankey diagram
# in the Cross-boundary tab
colours_node <- rep("CornflowerBlue", times = 28)

colours_node_array <- paste0("[", paste0("'", colours_node,
                                         "'", collapse = ','),
                             "]")

opts <- paste0("{
               node: { colours: ", colours_node_array ," }
               }" )



### END OF SCRIPT ###