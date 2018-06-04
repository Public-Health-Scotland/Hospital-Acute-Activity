################################################################
### Data Preparation
### Data Explorer script 2 of 5
###
### Original Author: Jaime Villacampa
### Original Date: December 2017
### Last edited by: Jack Hannah
### Last edited on: 04 June 2018
###
### Written to be run on RStudio Desktop
###
### Packages required:
### readr (for reading csv files);
### dplyr and tidyr (for data manipulation)
###
### This script creates the files used in the data
### explorer by manipulating a series of base files



### Section 1: Housekeeping ----


# 1.1 - Load libraries
library(readr)
library(dplyr)
library(tidyr)


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
  treat() %>%
  mutate(rate = as.numeric(rate))


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
