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


# 1 - Load libraries
library(readr)
library(dplyr)
library(tidyr)


# 2 - Define base filepath
base_filepath <- paste("//stats/pub_incubator/01 Acute Activity",
                       "/wrangling/data/base_files/",
                       sep = "")



### Section 2: Beds Data ----


data_bed <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_beds.csv",
  sep="")) %>%
  select(-c(quarter_date, hb_code, hb_name, loc_code)) %>%
  mutate_at(c("asb", "aob", "p_occ"), funs(round(.,1)))

# saveRDS(data_bed, "./data/beds.rds")



### Section 3: Specialty Data ----


# 1 - Inpatient data


# 1.1 - Residence data
data_spec_ip_res <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_IPDC_stays_res_spec.csv",
  sep="")) %>%
  res()


# 1.2 - Treatment data
data_spec_ip_treat <- read_csv(paste(
  base_filepath,
  "QAcute_Dec17_IPDC_stays_treat_spec.csv",
  sep="")) %>%
  treat()


# 1.3 - Combine inpatient files
data_spec_ip <- inp(data_spec_ip_treat,
                    data_spec_ip_res)

# saveRDS(data_spec_ip, "./data/spec_IP.rds")


# 2 - Outpatient data


# 2.1 - Residence data
data_spec_op_res <- read_csv(paste(
  basefile_path,
  "QAcute_Dec17_OP_res_spec.csv",
  sep="")) %>%
  res()


