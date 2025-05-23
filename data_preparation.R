############################################################
## Code name - data_preparation.R
## Data Release - Quarterly Data Explorer
## Latest Update: James Fixter, December 2022
##
## Written/run on - R Studio SERVER
## R version - 3.6.1
##
## This script creates the files used in the data explorer by 
## manipulating a series of base files
##
## With the exception of the Beds, Map and Cross-
## Boundary data, data manipulation is conducted in
## the following format and order:
##
## Section X: Data
##
## X.1 - Inpatient data
## X.1.1 - Residence data
## X.1.2 - Treatment data
## X.1.3 - Combine inpatient files
##
## X.2 - Outpatient data
## X.2.1 - Residence data
## X.2.2 - Treatment data
## X.2.3 - Combine outpatient files
##
## X.3 - Combine inpatient and outpatient files
##
## X.4 - Delete all intermediate files
##
## The Beds data aren't split by inpatient and outpatient
## (or residence and treatment), and the Map and Cross-
## Boundary data aren't split by residence and treatment
##
##
###########################################################
### Section 1: Housekeeping ----

# 1.1 - Load functions script
source("Data-Explorer/functions.R")

# 1.2 - Load required libraries
# Note - dplyr, tidyr, lubridate and stringi are loaded by sourcing the functions script
library(readr)
library(zoo)
library(janitor)

# 1.3 - Define filepaths - update pub_date each quarter.
base_filepath <- "../../data/output/"
rds_filepath <- "Data-Explorer/data/"
pub_date <- ("-to-December-2024")

# Create data folder
dir.create(rds_filepath, showWarnings = FALSE)


############################################################
### Section 2: Beds Data ----

data_bed <- as_tibble(read_csv(paste0(
  base_filepath, "Beds-by-NHS-Board-of-Treatment-and-Specialty", pub_date,".csv")))

data_bed <- data_bed %>%
    select(-c(quarter_date, hb_code, loc_code)) %>%
    mutate_at(c("asb", "aob", "p_occ"), list(~round_half_up(., 1))) %>%
    # Creating new quarter name variable for dropdowns and plots
    mutate(quarter_name = paste0(substr(quarter_name, 1, 3), "-",
                                 substr(quarter_name, 7, 9), " 20",
                                 substr(quarter_name, 11, 12)
                                 )
           )

# Save file
saveRDS(data_bed, paste0(rds_filepath, "beds.rds"))


############################################################
### Section 3: Specialty Data ----

# 3.1 - Inpatient data

# 3.1.1 - Residence data
data_spec_ip_res <- read_csv(paste0(
  base_filepath, 
  "Inpatient-and-Daycase-Spells-by-NHS-Board-of-Residence-",
  "and-Specialty", pub_date,".csv")) %>%
  res()

# 3.1.2 - Treatment data
data_spec_ip_treat <- read_csv(paste0(
  base_filepath, 
  "Inpatient-and-Daycase-Spells-by-NHS-Board-of-Treatment-",
  "and-Specialty", pub_date,".csv")) %>%
  treat()

# 3.1.3 - Combine inpatient files
data_spec_ip <- comb_inp(data_spec_ip_treat,
                         data_spec_ip_res)

# 3.2 - Outpatient data

# 3.2.1 - Residence data
data_spec_op_res <- read_csv(paste0(
  base_filepath, 
  "Outpatients-by-NHS-Board-of-Residence-and-Specialty", pub_date,".csv")) %>%
  res() 

# 3.2.2 - Treatment data
data_spec_op_treat <- read_csv(paste0(
  base_filepath, 
  "Outpatients-by-NHS-Board-of-Treatment-and-Specialty", pub_date,".csv")) %>%
  treat()

# 3.2.3 - Combine outpatient files
data_spec_op <- comb_outp(data_spec_op_treat,
                          data_spec_op_res)

# 3.3 - Combine inpatient and outpatient files, and
#       exclude 'Other' as there are duplicates from both files
data_spec <- comb_all(data_spec_op,
                      data_spec_ip) %>%
    filter(geo_type != "Other") %>%
    # Creating new quarter end and name variables for dropdowns and plots
    mutate(quarter_end = dmy(quarter_date),
           quarter_name = paste0(substr(quarter_name, 1, 3), "-",
                                 substr(quarter_name, 7, 9), " ",
                                 year(quarter_end)
                                 )
           )

# Save file
saveRDS(data_spec, paste0(rds_filepath, "spec.rds"))

# 3.4 - Delete all intermediate files
rm(data_spec_ip_res, data_spec_ip_treat,
   data_spec_ip, data_spec_op_res,
   data_spec_op_treat, data_spec_op)


############################################################
### Section 4: SIMD Data ----

# 4.1 - Inpatient data

# 4.1.1 - Residence data
data_simd_ip_res <- read_csv(paste0(
  base_filepath, 
  "Inpatient-and-Daycase-Stays-by-NHS-Board-of-Residence-",
  "and-SIMD", pub_date,".csv")) %>%
  res()

# 4.1.2 - Treatment data
data_simd_ip_treat <- read_csv(paste0(
  base_filepath, 
  "Inpatient-and-Daycase-Stays-by-NHS-Board-of-Treatment-",
  "and-SIMD", pub_date,".csv")) %>%
  treat()

# 4.1.3 - Combine inpatient files
data_simd_ip <- comb_inp(data_simd_ip_res, data_simd_ip_treat) %>%
  rename(count = stays)

# 4.2 - Outpatient data

# 4.2.1 - Residence data
data_simd_op_res <- read_csv(paste0(
  base_filepath, 
  "Outpatients-by-NHS-Board-of-Residence-and-SIMD", pub_date,".csv")) %>%
  res() 

# 4.2.2 - Treatment data
data_simd_op_treat <- read_csv(paste0(
  base_filepath, 
  "Outpatients-by-NHS-Board-of-Treatment-and-SIMD", pub_date,".csv"))
  
# Exclude three location codes which have no name
data_simd_op_treat <- data_simd_op_treat %>% 
  filter(!(loc_code %in% c('s217H', "s217v", "S127v"))) %>%
  treat()

# 4.2.3 - Combine outpatient files
data_simd_op <- comb_outp(data_simd_op_treat, data_simd_op_res) %>%
  pivot_longer(cols = c(attendances, dna_count), names_to = "type", values_to = "count") %>%
  mutate(measure = paste0(measure, "-", type),
         measure = recode(measure,
                          "All-attendances" = "All outpatient attendences",
                          "All-dna_count" = "All outpatient Did Not Attend (DNAs)",
                          "New-attendances" = "New outpatient attendences",
                          "New-dna_count" = "New outpatient Did Not Attend (DNAs)",
                          "Return-attendances" = "Return outpatient attendences",
                          "Return-dna_count" = "Return outpatient Did Not Attend (DNAs)")) %>%
  select(-type)


# 4.3 - Combine inpatient and outpatient files, and
#       Exclude null values, and
#       Recode to explain level of SIMD
data_simd <- comb_all(data_simd_op, data_simd_ip) %>%
    filter(loc_name != "Null") %>%
    drop_na(simd) %>%
    mutate(simd = recode(
        as.character(simd),
        "1" = "1 - Most deprived",
        "5" = "5 - Least deprived"),
        # Creating new quarter end and name variables for dropdowns and plots
        quarter_end = dmy(quarter_date),
        quarter_name = paste0(substr(quarter_name, 1, 3), "-",
                              substr(quarter_name, 7, 9), " ",
                              year(quarter_end)
                              )
        )

# Save file
saveRDS(data_simd, paste0(rds_filepath, "simd.rds"))

# 4.4 - Delete all intermediate files
rm(data_simd_ip_res, data_simd_ip_treat,
   data_simd_ip, data_simd_op_res,
   data_simd_op_treat, data_simd_op)


############################################################
### Section 5: Time Trend Data ----

# 5.1 - Inpatient data

# 5.1.1 - Residence data
data_trend_ip_res <- read_csv(paste0(
  base_filepath, 
  "Inpatient-and-Daycase-Stays-by-NHS-Board-of-Residence", pub_date,".csv")) %>%
  res()

# 5.1.2 - Treatment data
data_trend_ip_treat <- read_csv(paste0(
  base_filepath, 
  "Inpatient-and-Daycase-Stays-by-NHS-Board-of-Treatment", pub_date,".csv")) %>%
  treat()

# 5.1.3 - Combine inpatient files
data_trend_ip <- comb_inp(data_trend_ip_treat, data_trend_ip_res) %>%
  rename(count = stays)

# 5.2 - Outpatient data

# 5.2.1 - Residence data
data_trend_op_res <- read_csv(paste0(
  base_filepath, 
  "Outpatients-by-NHS-Board-of-Residence", pub_date,".csv")) %>%
  res()

# 5.2.2 - Treatment data
data_trend_op_treat <- read_csv(paste0(
  base_filepath, 
  "Outpatients-by-NHS-Board-of-Treatment", pub_date,".csv")) %>%
  treat()

# 5.2.3 - Combine outpatient files
data_trend_op <- comb_outp(data_trend_op_treat, data_trend_op_res) %>%
  rename(count = attendances)

# 5.3 - Combine inpatient and outpatient files, and 
#       Exclude null values and non-aggregated 'Other' values
data_trend <- comb_all(data_trend_op, data_trend_ip) %>%
  filter(!(loc_name %in% c("Null", "Other") &
             geo_type == "Other")) %>%
# Create a new column representing the last month
# in each financial quarter
# This will be used as the x-axis in a shiny plot
    mutate(quarter_date_last = as.yearmon(quarter_date, "%d/%m/%Y"),
           # Creating new quarter end and name variables for dropdowns and plots
           quarter_end = dmy(quarter_date),
           quarter_name = paste0(substr(quarter_name, 1, 3), "-",
                                 substr(quarter_name, 7, 9), " ",
                                 year(quarter_end)),
           # Adding in a fix for hospitals listed under multiple health boards in ISD(S)1.
           # When 'Return' appointments are selected in the explorer, this causes
           # each affected quarter to be plotted twice, which Plotly can't handle.
           hb_name = case_when(loc_name %in% "Gilbert Bain Hospital" ~ "NHS Shetland",
                               loc_name %in% "The Balfour" ~ "NHS Orkney",
                               loc_name %in% "Glasgow Royal Infirmary" ~ "NHS Greater Glasgow & Clyde",
                               loc_name %in% "Queen Margaret Hospital" ~ "NHS Fife",
                               loc_name %in% "Raigmore Hospital" ~ "NHS Highland",
                               TRUE ~ hb_name)
           ) %>%
    group_by(quarter_date, loc_name, measure, geo_type, file) %>% 
    mutate(count = sum(count)) %>% 
    ungroup() %>%
    distinct() %>%
# Arrange by date for later plotting
  arrange(quarter_date_last)

# Save file
saveRDS(data_trend, paste0(rds_filepath, "trend.rds"))

# 5.4 - Delete all intermediate files
rm(data_trend_ip_res, data_trend_ip_treat,
   data_trend_ip, data_trend_op_res,
   data_trend_op_treat, data_trend_op)


############################################################
### Section 6: Population Pyramid Data ----

# 6.1 - Inpatient data

# 6.1.1 - Residence data
data_pyramid_ip_res <- read_csv(paste0(
  base_filepath, 
  "Inpatient-and-Daycase-Stays-by-NHS-Board-of-Residence-",
  "Age-and-Sex", pub_date,".csv")) %>%
  res() %>%
  
# Split sex and age into two columns
  separate(sex_age, c("sex", "age"), "\\s") %>%
  select(-c(hb_code, hb_name, loc_code))

# 6.1.2 - Treatment data
data_pyramid_ip_treat <- read_csv(paste0(
  base_filepath, 
  "Inpatient-and-Daycase-Stays-by-NHS-Board-of-Treatment-",
  "Age-and-Sex", pub_date,".csv")) %>%
  treat() %>%
  
# Split sex and age into two columns
  separate(sex_age, c("sex", "age"), "\\s") %>%
  select(-c(hb_code, hb_name, loc_code))

# 6.1.3 - Combine inpatient files
data_pyramid_ip <- comb_inp(data_pyramid_ip_treat, data_pyramid_ip_res) %>%
  rename(count = stays)

# 6.2 - Outpatient data

# 6.2.1 - Residence data
data_pyramid_op_res <- read_csv(paste0(
  base_filepath, 
  "Outpatients-by-NHS-Board-of-Residence-Age-and-Sex", pub_date,".csv")) %>%
  res() %>%
  
# Split sex and age into two columns
  separate(sex_age, c("sex", "age"), "\\s") %>%
  select(-c(hb_code, hb_name, loc_code))

# 6.2.2 - Treatment data
data_pyramid_op_treat <- read_csv(paste0(
  base_filepath, 
  "Outpatients-by-NHS-Board-of-Treatment-Age-and-Sex", pub_date,".csv")) %>%
  treat() %>%
 
# Split sex and age into two columns
  separate(sex_age, c("sex", "age"), "\\s") %>%
  select(-c(hb_code, hb_name, loc_code))

# 6.2.3 - Combine outpatient files
data_pyramid_op <- comb_outp(data_pyramid_op_treat, data_pyramid_op_res) %>%
  pivot_longer(cols = c(attendances, dna_count), names_to = "type", values_to = "count") %>%
  mutate(measure = paste0(measure, "-", type),
         measure = recode(measure,
                          "All-attendances" = "All outpatient attendences",
                          "All-dna_count" = "All outpatient Did Not Attend (DNAs)",
                          "New-attendances" = "New outpatient attendences",
                          "New-dna_count" = "New outpatient Did Not Attend (DNAs)",
                          "Return-attendances" = "Return outpatient attendences",
                          "Return-dna_count" = "Return outpatient Did Not Attend (DNAs)")) %>%
  select(-type)

# 6.3 - Combine inpatient and outpatient files, and
#       Exclude null values
data_pyramid <- comb_all(data_pyramid_op, data_pyramid_ip) %>%
    filter(loc_name != "Null") %>%
  
# Create a new column representing the last month
# in each financial quarter
    mutate(quarter_date_last = as.yearmon(quarter_date, "%d/%m/%Y"),
           # Creating new quarter end and name variables for dropdowns and plots
           quarter_end = dmy(quarter_date),
           quarter_name = paste0(substr(quarter_name, 1, 3), "-",
                                 substr(quarter_name, 7, 9), " ",
                                 year(quarter_end)
                                 )
           )

# Save file
saveRDS(data_pyramid, paste0(rds_filepath, "pyramid.rds"))

# 6.4 - Delete all intermediate files
rm(data_pyramid_ip_res, data_pyramid_ip_treat,
   data_pyramid_ip, data_pyramid_op_res,
   data_pyramid_op_treat, data_pyramid_op)


# ############################################################
# ### Section 7: Map Data ----
# 
# # 7.1 - Inpatient data
# data_map_ipdc <- read_csv(paste0(
#   base_filepath, 
#   "Inpatient-and-Daycase-Stays-by-NHS-Board-of-Residence", pub_date,".csv")) %>%
#   
# # Exclude Scotland, Golden Jubilee and non-territorial codes
#   filter(!(loc_name %in% c("Scotland", "Other") |
#             hb_name %in% c("Scotland", "Other"))) %>%
#   
# # Create a dummy variable for crude rate, and labels for the map tooltip
#   mutate(crude_rate = row_number(),
#          labs = paste0(
#            loc_name, '</br>', 'Admissions: ',
#            stays, '</br>', 'Crude rate: ',
#            crude_rate)) %>%
#   
# # Convert to long format to allow multiple selections in the map
#   gather("value_type", "value", c("stays", "crude_rate")) %>%
#   mutate(value_type = recode(
#     value_type,
#     "stays" = "Admissions",
#     "crude_rate" = "Crude rate"
#   ))
# 
# # Save file
# saveRDS(data_map_ipdc, paste0(rds_filepath, "map_ipdc.rds"))
# 
# # 7.2 - Outpatient data
# data_map_op <- read_csv(paste0(
#   base_filepath, 
#   "Outpatients-by-NHS-Board-of-Residence", pub_date,".csv")) %>%
#   
# # Exclude Scotland, Golden Jubilee and non-territorial codes
#   filter(!(loc_name %in% c("Scotland", "Other") |
#             hb_name %in% c("Scotland", "Other"))) %>%
#   
# # Create a dummy variable for crude rate, and labels for the map tooltip
#   mutate(crude_rate = row_number(),
#          labs = paste0(
#            loc_name, '</br>', 'Appointments: ',
#            count, '</br>', 'Rate: ',
#            crude_rate)) %>%
#   
# # Convert to long format to allow multiple selections in the map
#   gather("value_type", "value", c("count", "rate", "crude_rate")) %>%
#   drop_na(value) %>%
#   mutate(value_type = recode(
#     value_type,
#     "count" = "Appointments",
#     "rate" = "DNA rate",
#     "crude_rate" = "Crude rate"
#   ))
# 
# # Save file
# saveRDS(data_map_op, paste0(rds_filepath, "map_op.rds"))
# 

############################################################
### Section 8: Cross-Boundary Data ----

# 8.1 - Inpatient data
data_cbf_ip <- read_csv(paste0(
  base_filepath, 
  "Inpatient-and-Daycase-Cross-Boundary-Flow", pub_date,".csv")) %>%
  mutate(file = "Inpatients/day cases") %>%
  
# Select health boards only
  filter(hbtreat_name != "Non-NHS Provider" &
           hbres_name != "Other") %>%
  select(-c(hbtreat_currentdate, hbres_currentdate, episodes)) %>%
  rename(count = stays) %>%
  
# Create a duplicate variable of hbtreat_name but add a space at the end
# This is to create unique labels for from/to for the Sankey diagram
  mutate(hb_treat_space = paste0(hbtreat_name, " ")) %>%
  
# To reduce the length of the tooltip, remove 'NHS' from the name of 
# all health board variables
  mutate_at(vars(contains("hb")),
            funs(stri_replace_first_fixed(., "NHS ", "")))

# 8.2 - Outpatient data
data_cbf_op <-  read_csv(paste0(
  base_filepath, 
  "Outpatients-Cross-Boundary-Flow", pub_date,".csv")) %>%
  mutate(file = "Outpatients") %>%
  
# Select health boards only
  filter(!(hbtreat_name %in% c("Non-NHS Provider", "Null")) &
           hbres_name != "Other") %>%
  select(-c(hbtreat_currentdate, hbres_currentdate)) %>%
  rename(count = attendances) %>%
  
# Create a duplicate variable of hbtreat_name but add a space at the end
# This is to create unique labels for from/to for the Sankey diagram
  mutate(hb_treat_space = paste0(hbtreat_name, " ")) %>%
  
# To reduce the length of the tooltip, remove 'NHS' from the name of 
# all health board variables
  mutate_at(vars(contains("hb")),
            funs(stri_replace_first_fixed(., "NHS ", "")))

# 8.3 - Combine inpatient and outpatient files
data_cbf <- bind_rows(data_cbf_ip, data_cbf_op) %>% 
    # Creating new quarter end and name variables for dropdowns and plots
    mutate(quarter_end = dmy(quarter_date),
           quarter_name = paste0(substr(quarter_name, 1, 3), "-",
                                 substr(quarter_name, 7, 9), " ",
                                 year(quarter_end)),
           count.tooltip = paste0(hbres_name, " patients treated in ",
                                  hbtreat_name, ": <b>",
                                  prettyNum(count, big.mark = ","), "</b>"
                                  )) %>% 
            filter(count > 0)
           
# Save file
saveRDS(data_cbf, paste0(rds_filepath, "cbf.rds"))

# 8.4 - Delete intermediate files
rm(data_cbf_ip, data_cbf_op)


############################################################

### END OF SCRIPT ###
