################################################################
### Data Preparation
### Data Explorer script 2 of 5
###
### Original Author: Jaime Villacampa
### Original Date: December 2017
### Last edited by: Jack Hannah
### Last edited on: 06 June 2018
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



### Section 1: Housekeeping ----


# 1.1 - Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(stringi)


# 1.2 - Define base filepath
base_filepath <- paste("//stats/pub_incubator/01 Acute Activity",
                       "/wrangling/data/base_files/",
                       sep = "")



### Section 2: Beds Data ----


data_bed <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_beds.csv",
  sep="")) %>%
  select(-c(quarter_date, hb_code, hb_name, loc_code)) %>%
  mutate_at(c("asb", "aob", "p_occ"), funs(round(., 1)))

# saveRDS(data_bed, "./data/beds.rds")



### Section 3: Specialty Data ----


# 3.1 - Inpatient data


# 3.1.1 - Residence data
data_spec_ip_res <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_IPDC_stays_res_spec.csv",
  sep="")) %>%
  res()


# 3.1.2 - Treatment data
data_spec_ip_treat <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_IPDC_stays_treat_spec.csv",
  sep="")) %>%
  treat()


# 3.1.3 - Combine inpatient files
data_spec_ip <- comb_inp(data_spec_ip_treat,
                         data_spec_ip_res)

# saveRDS(data_spec_ip, "./data/spec_IP.rds")


# 3.2 - Outpatient data


# 3.2.1 - Residence data
data_spec_op_res <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_OP_res_spec.csv",
  sep="")) %>%
  res()


# 3.2.2 - Treatment data
# NOTE - the original code relating to this file is slightly
# different to that which is defined in the 'treat' function
# The end result, however, should be exactly the same
data_spec_op_treat <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_OP_treat_spec.csv",
  sep="")) %>%
  treat()


# 3.2.3 - Combine outpatient files
data_spec_op <- comb_outp(data_spec_op_treat,
                          data_spec_op_res)

# saveRDS(data_spec_op, "./data/spec_OP.rds")


# 3.3 - Combine inpatient and outpatient files
data_spec <- comb_all(data_spec_op,
                      data_spec_ip) %>%
  
  # Exclude others as there are
  # duplicates from both files
  filter(geo_type != "Other")

# saveRDS(data_spec, "./data/spec_IPOP.rds")


# 3.4 - Delete all intermediate files
rm(data_spec_ip_res, data_spec_ip_treat,
   data_spec_ip, data_spec_op_res,
   data_spec_op_treat, data_spec_op)



### Section 4: SIMD Data ----


# 4.1 - Inpatient data


# 4.1.1 - Residence data
data_simd_ip_res <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_IPDC_stays_res_simd.csv",
  sep="")) %>%
  res()


# 4.1.2 - Treatment data
data_simd_ip_treat <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_IPDC_stays_treat_simd.csv",
  sep="")) %>%
  treat()


# 4.1.3 - Combine inpatient files
data_simd_ip <- comb_inp(data_simd_ip_res,
                         data_simd_ip_treat) %>%
  rename(count = stays)

# saveRDS(data_simdip, "./data/SIMD_IP.rds")


# 4.2 - Outpatient data


# 4.2.1 - Residence data
data_simd_op_res <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_OP_res_simd.csv",
  sep="")) %>%
  res()


# 4.2.2 - Treatment data
data_simd_op_treat <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_OP_treat_simd.csv",
  sep="")) %>%
  
  # Exclude three location codes which have no name
  filter(!(loc_code %in% c('s217H', "s217v", "S127v"))) %>%
  treat()


# 4.2.3 - Combine outpatient files
data_simd_op <- comb_outp(data_simd_op_treat,
                          data_simd_op_res)

# saveRDS(data_simd_op, "./data/SIMD_OP.rds")


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

# saveRDS(data_simd, "./data/SIMD_IPOP.rds")


# 4.4 - Delete all intermediate files
rm(data_simd_ip_res, data_simd_ip_treat,
   data_simd_ip, data_simd_op_res,
   data_simd_op_treat, data_simd_op)



### Section 5: Time Trend Data ----


# 5.1 - Inpatient data


# 5.1.1 - Residence data
data_trend_ip_res <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_IPDC_stays_res_all.csv",
  sep="")) %>%
  res()


# 5.1.2 - Treatment data
data_trend_ip_treat <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_IPDC_stays_treat_all.csv",
  sep="")) %>%
  treat()


# 5.1.3 - Combine inpatient files
data_trend_ip <- comb_inp(data_trend_ip_treat,
                          data_trend_ip_res) %>%
  rename(count = stays)
  
# saveRDS(data_trend_ip, "./data/trend_IP.rds")


# 5.2 - Outpatient data


# 5.2.1 - Residence data
data_trend_op_res <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_OP_res_all.csv",
  sep="")) %>%
  res() %>%
  
  # The quarter dates in this file have dashes rather
  # than forward slashes, and don't have the '20' prefix
  # for years, so re-format for consistency with the
  # other files
  mutate(quarter_date = gsub("-", "/", quarter_date),
         quarter_date = stri_replace_last_fixed(
           quarter_date, "/", "/20"
         ))


# 5.2.2 - Treatment data
data_trend_op_treat <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_OP_treat_all.csv",
  sep="")) %>%
  treat()


# 5.2.3 - Combine outpatient files
data_trend_op <- comb_outp(data_trend_op_treat,
                           data_trend_op_res)

# saveRDS(data_trend_op, "./data/trend_OP.rds")


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

# saveRDS(data_trend, "./data/trend_IPOP.rds")


# 5.4 - Delete all intermediate files
rm(data_trend_ip_res, data_trend_ip_treat,
   data_trend_ip, data_trend_op_res,
   data_trend_op_treat, data_trend_op)



### Section 6: Population Pyramid Data ----


# 6.1 - Inpatient data


# 6.1.1 - Residence data
data_pyramid_ip_res <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_IPDC_stays_res_agesex.csv",
  sep="")) %>%
  res() %>%
  
  # Split sex and age into two columns
  separate(sex_age, c("sex", "age"), "\\s") %>%
  select(-c(hb_code, hb_name, loc_code))


# 6.1.2 - Treatment data
data_pyramid_ip_treat <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_IPDC_stays_treat_agesex.csv",
  sep="")) %>%
  treat() %>%
  
  # Split sex and age into two columns
  separate(sex_age, c("sex", "age"), "\\s") %>%
  select(-c(hb_code, hb_name, loc_code))


# 6.1.3 - Combine inpatient files
data_pyramid_ip <- comb_inp(data_pyramid_ip_treat,
                            data_pyramid_ip_res) %>%
  rename(count = stays)

# saveRDS(data_pyramid_ip, "./data/pyramid_IP.rds")


# 6.2 - Outpatient data


# 6.2.1 - Residence data
data_pyramid_op_res <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_OP_res_agesex.csv",
  sep="")) %>%
  res() %>%
  
  # Split sex and age into two columns
  separate(sex_age, c("sex", "age"), "\\s") %>%
  select(-c(hb_code, hb_name, loc_code))


# 6.2.2 - Treatment data
data_pyramid_op_treat <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_OP_treat_agesex.csv",
  sep="")) %>%
  treat() %>%
  
  # Split sex and age into two columns
  separate(sex_age, c("sex", "age"), "\\s") %>%
  select(-c(hb_code, hb_name, loc_code))


# 6.2.3 - Combine outpatient files
data_pyramid_op <- comb_outp(data_pyramid_op_treat,
                             data_pyramid_op_res)

# saveRDS(data_pyramid_op, "./data/pyramid_OP.rds")


# 6.3 - Combine inpatient and outpatient files
data_pyramid <- comb_all(data_pyramid_op,
                         data_pyramid_ip) %>%
  
  # Exclude null values
  filter(loc_name != "Null") %>%
  
  # Create a new column representing the last month
  # in each financial quarter
  mutate(quarter_date_last = as.yearmon(quarter_date,
                                        "%d/%m/%Y"))

# saveRDS(data_pyramid, "./data/pyramid_IPOP.rds")


# 6.4 - Delete all intermediate files
rm(data_pyramid_ip_res, data_pyramid_ip_treat,
   data_pyramid_ip, data_pyramid_op_res,
   data_pyramid_op_treat, data_pyramid_op)



### Section 7: Map Data ----


# 7.1 - Inpatient data
data_map_ipdc <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_IPDC_stays_res_all.csv",
  sep="")) %>%
  
  # Exclude Scotland, Golden Jubilee and
  # non-territorial codes
  # NOTE - the original code removed rows where
  # loc_name == "Others", but there are no entries
  # with this value, so assuming that meant either
  # "Scotland" or "Other"
  filter(!(loc_name %in% c("Scotland", "Other") |
           hb_name %in% c("Scotland", "Other"))) %>%
  
  # Create a dummy variable for crude rate, and
  # labels for the map tooltip
  # NOTE - in the original code, this file had
  # only 5,047 rows
  mutate(crude_rate = seq(1:5799),
         labs = paste0(
           loc_name, '</br>', 'Admissions: ',
           stays, '</br>', 'Crude rate: ',
           crude_rate)) %>%
  
  # Convert to long format to allow multiple selections in
  # the map
  gather("value_type", "value", c("stays","crude_rate")) %>%
  mutate(value_type = recode(
    value_type,
    "stays" = "Admissions",
    "crude_rate" = "Crude rate"
  ))
  
# saveRDS(data_map_ipdc, "./data/ipdc_map.rds")