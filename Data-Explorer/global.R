############################################################
## Code name - global.R
## Data Release - Quarterly Data Explorer
## Latest Update: James Fixter, December 2022
##
## Written/run on - R Studio SERVER
## R version - 3.6.1
##
## This script includes all packages, datasets and other
## values used by both the UI and server files
##
############################################################
### Section 1: Housekeeping ----

# 1.1 - Load libraries (latest version used shown next to each package)
library(shiny) # v. 1.7.3
library(DT) # v. 0.26
library(googleVis) # v. 0.7.0
library(htmltools) # v. 0.5.2
library(plotly) # v. 4.10.0
library(dplyr) # v. 1.0.9
library(tidyr) # v. 1.2.0
library(readr) # v. 2.1.2
library(zoo) # v. 1.8-10
library(lubridate) # v. 1.8.0
library(shinyWidgets) # v. 0.7.0
library(shinymanager) # v. 1.0.400

## Update each quarter
data_up_to <- "30-september-2022"

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
 #credentials <- list(authorisation = "Public")

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
data_trend <- readRDS(paste0(rds_filepath, "trend.rds")) 

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
                 "Inpatients/day cases - Age/sex",
                 "Inpatients/day cases - Cross-boundary flow",
                 "Inpatients/day cases - Time trend",
                 "Inpatients/day cases - Specialty",
                 "Inpatients/day cases - Deprivation (SIMD)",
                 "Outpatients - Age/sex",
                 "Outpatients - Cross-boundary flow",
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
  filter(measure != "Not specified – inpatients") %>%
  pull(measure)

# 3.6 - Time Trend measures
trend_measure <- c("Number of stays/appointments",
                   "Total length of stay (days)",
                   "Mean length of stay (days)",
                   "Did not attend rate (%)")

# 3.7 - Pyramid services
pyramid_service <- data_pyramid %>%
  distinct(measure) %>%
  filter(measure != "Not specified – inpatients") %>%
  pull(measure)


############################################################
### Section 4: Plot Parameters ----

# 4.1 - Time Trend colour palette (updated to reflect PHS corporate colours)
trend_pal <- c("#9B4393", "#3F3685", "#0078D4", "#83BB26", "#948DA3",
               "#1E7F84", "#6B5C85", "#C73918", "#655E9D", "#9F9BC2",
               "#C5C3DA", "#ECEBF3", "#AF69A9", "#CDA1C9", "#E1C7DF",
               "#F5ECF4", "#3393DD", "#80BCEA", "#B3D7F2", "#E6F2FB",
               "#9CC951", "#C1DD93", "#DAEBBE", "#F3F8E9", "#A9A4B5",
               "#CAC6D1", "#DFDDE3", "#F4F4F6", "#4B999D", "#8FBFC2",
               "#BCD9DA", "#E9F2F3", "#897D9D", "#B5AEC2", "#D3CEDA",
               "#F0EFF3", "#D26146", "#E39C8C", "#EEC4BA", "#F9EBE8")

# 4.2 - Cross-Boundary colour palette for Sankey diagram
# in the Cross-boundary tab
colours_node_array <- paste0("[", paste0("'", trend_pal,
                                         "'", collapse = ','),"]")

opts <- paste0("{
               node: { colors: ", colours_node_array ,"}",
               "}"
               )

# 4.3 - Defining a vline function for Plotly to add a vertical dashed line
# to time series charts indicating the start of the pandemic
vline <- function(x = 0, color = "red") {
    list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash= "dash")
    )
}

# 4.4 - Defining an annotation about COVID-19 for time series charts
covid_label = list(
    x = "2020-04-10",
    y = max("y"),
    font = list(size = 14, color = "red"),
    text = "Start of COVID-19 emergency \nmeasures in March 2020",
    xref = "x",
    yref = "paper",
    xanchor = "left",
    yanchor = "top",
    align = "left",
    showarrow = F
)
covid_arrow = list(
                   xref = "x",
                   yref = "paper",
                   axref = 'x',                 
                   ayref = 'paper',
                   x = dmy("30-03-2021"),
                   y = 0.9,
                   ax = dmy("30-03-2020"),
                   ay = 0.9,
                   text = '',  
                   font = list(size = 14, color = "red"),
                   showarrow = TRUE,
                   arrowhead = 2,
                   arrowsize = 1.5,
                   arrowcolor = 'red')

############################################################

### END OF SCRIPT ###