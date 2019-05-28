################################################################
### Data Preparation
### Data Explorer script 2 of 5
###
### Original Author: Jaime Villacampa
### Original Date: December 2017
### Last edited by: Mark Hamilton
### Last edited on: 06 March 2019
###
### Written to be run on RStudio Desktop
###
### Packages required:
### readr (for reading csv files);
### dplyr and tidyr (for data manipulation);
### zoo (for dates);
### stringi (for string manipulation)
###
### This script creates the files used in the data
### explorer by manipulating a series of base files
###
### With the exception of the Beds, Map and Cross-
### Boundary data, data manipulation is conducted in
### the following format and order:
###
### Section X: Data
###
###
### X.1 - Inpatient data
###
###
### X.1.1 - Residence data
###
### X.1.2 - Treatment data
###
### X.1.3 - Combine inpatient files
###
###
### X.2 - Outpatient data
###
###
### X.2.1 - Residence data
###
### X.2.2 - Treatment data
###
### X.2.3 - Combine outpatient files
###
###
### X.3 - Combine inpatient and outpatient files
###
###
### X.4 - Delete all intermediate files
###
###
### The Beds data aren't split by inpatient and outpatient
### (or residence and treatment), and the Map and Cross-
### Boundary data aren't split by residence and treatment



### Section 1: Housekeeping ----


# 1.1 - Load functions script
source("./Data-Explorer/functions.R")


# 1.2 - Load libraries
# Note - dplyr, tidyr and stringi are loaded by sourcing the functions script
library(readr)
library(zoo)


# 1.3 - Define filepaths
base_filepath <- paste0("//stats/SecondaryCare/Quarterly Publication/TPP",
                        "/6_May19/data/output/")

rds_filepath <- ("./Data-Explorer/data/")



### Section 2: Beds Data ----


data_bed <- read_csv(paste0(
  base_filepath,
  "20190528_Beds_by_Health_Board_of_Treatment_and_Specialty.csv"),
  
  # Some values in aasb and tobd columns have decimal points, so explicitly
  # set them as type double in case read_csv doesn't do it automatically
  col_types = cols(aasb = col_double(),
                   tobd = col_double())) %>%
  select(-c(quarter_date, hb_code, hb_name, loc_code)) %>%
  mutate_at(c("aasb", "tobd"), funs(round)) %>%
  mutate_at(c("asb", "aob", "p_occ"), funs(round(., 1)))

# Save file
saveRDS(data_bed, paste0(
  rds_filepath,
  "beds.rds"))



### Section 3: Specialty Data ----


# 3.1 - Inpatient data


# 3.1.1 - Residence data
data_spec_ip_res <- read_csv(paste0(
  base_filepath,
  "20190528_Inpatient_and_Daycase_Stays_by_Health_Board_of_Residence_",
  "and_Specialty.csv")) %>%
  res()


# 3.1.2 - Treatment data
data_spec_ip_treat <- read_csv(paste0(
  base_filepath,
  "20190528_Inpatient_and_Daycase_Stays_by_Health_Board_of_Treatment_",
  "and_Specialty.csv")) %>%
  treat()


# 3.1.3 - Combine inpatient files
data_spec_ip <- comb_inp(data_spec_ip_treat,
                         data_spec_ip_res)


# 3.2 - Outpatient data


# 3.2.1 - Residence data
data_spec_op_res <- read_csv(paste0(
  base_filepath,
  "20190528_Outpatients_by_Health_Board_of_Residence_and_Specialty.csv")) %>%
  res() %>%
  convert_dates()


# 3.2.2 - Treatment data
data_spec_op_treat <- read_csv(paste0(
  base_filepath,
  "20190528_Outpatients_by_Health_Board_of_Treatment_and_Specialty.csv")) %>%
  treat()


# 3.2.3 - Combine outpatient files
data_spec_op <- comb_outp(data_spec_op_treat,
                          data_spec_op_res)


# 3.3 - Combine inpatient and outpatient files
data_spec <- comb_all(data_spec_op,
                      data_spec_ip) %>%
  
  # Exclude others as there are
  # duplicates from both files
  filter(geo_type != "Other")

# Save file
saveRDS(data_spec, paste0(
  rds_filepath,
  "spec.rds"))


# 3.4 - Delete all intermediate files
rm(data_spec_ip_res, data_spec_ip_treat,
   data_spec_ip, data_spec_op_res,
   data_spec_op_treat, data_spec_op)



### Section 4: SIMD Data ----


# 4.1 - Inpatient data


# 4.1.1 - Residence data
data_simd_ip_res <- read_csv(paste0(
  base_filepath,
  "20190528_Inpatient_and_Daycase_Stays_by_Health_Board_of_Residence_",
  "and_SIMD.csv")) %>%
  res()


# 4.1.2 - Treatment data
data_simd_ip_treat <- read_csv(paste0(
  base_filepath,
  "20190528_Inpatient_and_Daycase_Stays_by_Health_Board_of_Treatment_",
  "and_SIMD.csv")) %>%
  treat()


# 4.1.3 - Combine inpatient files
data_simd_ip <- comb_inp(data_simd_ip_res,
                         data_simd_ip_treat) %>%
  rename(count = stays)


# 4.2 - Outpatient data


# 4.2.1 - Residence data
data_simd_op_res <- read_csv(paste0(
  base_filepath,
  "20190528_Outpatients_by_Health_Board_of_Residence_and_SIMD.csv")) %>%
  res() %>%
  convert_dates()


# 4.2.2 - Treatment data
data_simd_op_treat <- read_csv(paste0(
  base_filepath,
  "20190528_Outpatients_by_Health_Board_of_Treatment_and_SIMD.csv")) %>%
  
  # Exclude three location codes which have no name
  filter(!(loc_code %in% c('s217H', "s217v", "S127v"))) %>%
  treat() %>%
  convert_dates()


# 4.2.3 - Combine outpatient files
data_simd_op <- comb_outp(data_simd_op_treat,
                          data_simd_op_res)


# 4.3 - Combine inpatient and outpatient files
data_simd <- comb_all(data_simd_op,
                      data_simd_ip) %>%
  
  # Exclude null values
  filter(loc_name != "Null") %>%
  drop_na(simd) %>%
  
  # Recode to explain meaning of SIMD
  mutate(simd = recode(
    as.character(simd),
    "1" = "1 - Most deprived",
    "5" = "5 - Least deprived"
    ))

# Save file
saveRDS(data_simd, paste0(
  rds_filepath,
  "simd.rds"))


# 4.4 - Delete all intermediate files
rm(data_simd_ip_res, data_simd_ip_treat,
   data_simd_ip, data_simd_op_res,
   data_simd_op_treat, data_simd_op)



### Section 5: Time Trend Data ----


# 5.1 - Inpatient data


# 5.1.1 - Residence data
data_trend_ip_res <- read_csv(paste0(
  base_filepath,
  "20190528_Inpatient_and_Daycase_Stays_by_Health_Board_of_Residence.csv")) %>%
  res()


# 5.1.2 - Treatment data
data_trend_ip_treat <- read_csv(paste0(
  base_filepath,
  "20190528_Inpatient_and_Daycase_Stays_by_Health_Board_of_Treatment.csv")) %>%
  treat()


# 5.1.3 - Combine inpatient files
data_trend_ip <- comb_inp(data_trend_ip_treat,
                          data_trend_ip_res) %>%
  rename(count = stays)


# 5.2 - Outpatient data


# 5.2.1 - Residence data
data_trend_op_res <- read_csv(paste0(
  base_filepath,
  "20190528_Outpatients_by_Health_Board_of_Residence.csv")) %>%
  res() %>%
  convert_dates()


# 5.2.2 - Treatment data
data_trend_op_treat <- read_csv(paste0(
  base_filepath,
  "20190528_Outpatients_by_Health_Board_of_Treatment.csv")) %>%
  treat()


# 5.2.3 - Combine outpatient files
data_trend_op <- comb_outp(data_trend_op_treat,
                           data_trend_op_res)


# 5.3 - Combine inpatient and outpatient files
data_trend <- comb_all(data_trend_op,
                       data_trend_ip) %>%
  
  # Exclude null values and non-aggregated
  # 'Other' values
  filter(!(loc_name %in% c("Null", "Other") &
             geo_type == "Other")) %>%
  
  # Create a new column representing the last month
  # in each financial quarter
  # This will be used as the x-axis in a shiny plot
  mutate(quarter_date_last = as.yearmon(quarter_date,
                                        "%d/%m/%Y")) %>%
  
  # Arrange by date for later plotting
  arrange(quarter_date_last)

# Save file
saveRDS(data_trend, paste0(
  rds_filepath,
  "trend.rds"))


# 5.4 - Delete all intermediate files
rm(data_trend_ip_res, data_trend_ip_treat,
   data_trend_ip, data_trend_op_res,
   data_trend_op_treat, data_trend_op)



### Section 6: Population Pyramid Data ----


# 6.1 - Inpatient data


# 6.1.1 - Residence data
data_pyramid_ip_res <- read_csv(paste0(
  base_filepath,
  "20190528_Inpatient_and_Daycase_Stays_by_Health_Board_of_Residence_",
  "Age_and_Sex.csv")) %>%
  res() %>%
  
  # Split sex and age into two columns
  separate(sex_age, c("sex", "age"), "\\s") %>%
  select(-c(hb_code, hb_name, loc_code))


# 6.1.2 - Treatment data
data_pyramid_ip_treat <- read_csv(paste0(
  base_filepath,
  "20190528_Inpatient_and_Daycase_Stays_by_Health_Board_of_Treatment_",
  "Age_and_Sex.csv")) %>%
  treat() %>%
  
  # Split sex and age into two columns
  separate(sex_age, c("sex", "age"), "\\s") %>%
  select(-c(hb_code, hb_name, loc_code))


# 6.1.3 - Combine inpatient files
data_pyramid_ip <- comb_inp(data_pyramid_ip_treat,
                            data_pyramid_ip_res) %>%
  rename(count = stays)


# 6.2 - Outpatient data


# 6.2.1 - Residence data
data_pyramid_op_res <- read_csv(paste0(
  base_filepath,
  "20190528_Outpatients_by_Health_Board_of_Residence_Age_and_Sex.csv")) %>%
  res() %>%
  convert_dates() %>%
  
  # Split sex and age into two columns
  separate(sex_age, c("sex", "age"), "\\s") %>%
  select(-c(hb_code, hb_name, loc_code))


# 6.2.2 - Treatment data
data_pyramid_op_treat <- read_csv(paste0(
  base_filepath,
  "20190528_Outpatients_by_Health_Board_of_Treatment_Age_and_Sex.csv")) %>%
  treat() %>%
  convert_dates() %>%
  
  # Split sex and age into two columns
  separate(sex_age, c("sex", "age"), "\\s") %>%
  select(-c(hb_code, hb_name, loc_code))


# 6.2.3 - Combine outpatient files
data_pyramid_op <- comb_outp(data_pyramid_op_treat,
                             data_pyramid_op_res)


# 6.3 - Combine inpatient and outpatient files
data_pyramid <- comb_all(data_pyramid_op,
                         data_pyramid_ip) %>%
  
  # Exclude null values
  filter(loc_name != "Null") %>%
  
  # Create a new column representing the last month
  # in each financial quarter
  mutate(quarter_date_last = as.yearmon(quarter_date,
                                        "%d/%m/%Y"))

# Save file
saveRDS(data_pyramid, paste0(
  rds_filepath,
  "pyramid.rds"))


# 6.4 - Delete all intermediate files
rm(data_pyramid_ip_res, data_pyramid_ip_treat,
   data_pyramid_ip, data_pyramid_op_res,
   data_pyramid_op_treat, data_pyramid_op)



### Section 7: Map Data ----


# 7.1 - Inpatient data
data_map_ipdc <- read_csv(paste0(
  base_filepath,
  "20190528_Inpatient_and_Daycase_Stays_by_Health_Board_of_Residence.csv")) %>%
  
  # Exclude Scotland, Golden Jubilee and
  # non-territorial codes
  filter(!(loc_name %in% c("Scotland", "Other") |
             hb_name %in% c("Scotland", "Other"))) %>%
  
  # Create a dummy variable for crude rate, and
  # labels for the map tooltip
  mutate(crude_rate = row_number(),
         labs = paste0(
           loc_name, '</br>', 'Admissions: ',
           stays, '</br>', 'Crude rate: ',
           crude_rate)) %>%
  
  # Convert to long format to allow multiple selections in
  # the map
  gather("value_type", "value", c("stays", "crude_rate")) %>%
  mutate(value_type = recode(
    value_type,
    "stays" = "Admissions",
    "crude_rate" = "Crude rate"
  ))

# Save file
saveRDS(data_map_ipdc, paste0(
  rds_filepath,
  "map_ipdc.rds"))


# 7.2 - Outpatient data
data_map_op <- read_csv(paste0(
  base_filepath,
  "20190528_Outpatients_by_Health_Board_of_Residence.csv")) %>%
  convert_dates() %>%
  
  # Exclude Scotland, Golden Jubilee and
  # non-territorial codes
  filter(!(loc_name %in% c("Scotland", "Other") |
             hb_name %in% c("Scotland", "Other"))) %>%
  
  # Create a dummy variable for crude rate, and
  # labels for the map tooltip
  mutate(crude_rate = row_number(),
         labs = paste0(
           loc_name, '</br>', 'Appointments: ',
           count, '</br>', 'Rate: ',
           crude_rate)) %>%
  
  # Convert to long format to allow multiple selections in
  # the map
  gather("value_type", "value", c("count", "rate", "crude_rate")) %>%
  drop_na(value) %>%
  mutate(value_type = recode(
    value_type,
    "count" = "Appointments",
    "rate" = "DNA rate",
    "crude_rate" = "Crude rate"
  ))

# Save file
saveRDS(data_map_op, paste0(
  rds_filepath,
  "map_op.rds"))



### Section 8: Cross-Boundary Data ----


# 8.1 - Inpatient data
data_cbf_ip <- read_csv(paste0(
  base_filepath,
  "20190528_Inpatient_and_Daycase_Cross_Boundary_Flow.csv")) %>%
  mutate(file = "Inpatients/Day Cases") %>%
  
  # Select health boards only
  filter(hbtreat_name != "Non-NHS Provider" &
           hbres_name != "Other") %>%
  
  select(-c(hbtreat_currentdate, hbres_currentdate,
            episodes)) %>%
  rename(count = stays) %>%
  
  # Create a duplicate variable of hbtreat_name
  # but add a space at the end
  # This is to create unique labels for from/to
  # for the Sankey diagram
  mutate(hb_treat_space = paste0(hbtreat_name, " ")) %>%
  
  # To reduce the length of the tooltip, remove 'NHS'
  # from the name of all health board variables
  mutate_at(vars(contains("hb")),
            funs(stri_replace_first_fixed(., "NHS ", "")))


# 8.2 - Outpatient data
data_cbf_op <-  read_csv(paste0(
  base_filepath,
  "20190528_Outpatients_Cross_Boundary_Flow.csv")) %>%
  convert_dates() %>%
  mutate(file = "Outpatients") %>%
  
  # Select health boards only
  filter(!(hbtreat_name %in% c("Non-NHS Provider", "Null")) &
           hbres_name != "Other") %>%
  
  select(-c(hbtreat_currentdate, hbres_currentdate)) %>%
  rename(count = attendances) %>%
  
  # Create a duplicate variable of hbtreat_name
  # but add a space at the end
  # This is to create unique labels for from/to
  # for the Sankey diagram
  mutate(hb_treat_space = paste0(hbtreat_name, " ")) %>%
  
  # To reduce the length of the tooltip, remove 'NHS'
  # from the name of all health board variables
  mutate_at(vars(contains("hb")),
            funs(stri_replace_first_fixed(., "NHS ", "")))


# 8.3 - Combine inpatient and outpatient files
data_cbf <- bind_rows(data_cbf_ip, data_cbf_op)

# Save file
saveRDS(data_cbf, paste0(
  rds_filepath,
  "cbf.rds"))


# 8.4 - Delete intermediate files
rm(data_cbf_ip, data_cbf_op)



### END OF SCRIPT ###