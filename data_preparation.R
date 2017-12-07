#Syntax to create the data explorer
#In this file include data transformation needed to create data files for data explorer
#This code does not interfere with the shiny app, it just prepares the data the app will use.
#Jaime Villacampa December 17

#TODO:
#see server syntax

############################.
##Packages ----
############################.
library(reshape2) #for melting and dcast
library(stringr) #manipulation strings
library(tidyverse) #data manipulation, etc
library (zoo) #for dates

############################.
##Filepath ----
############################.
# basefile_path <- "//stats/pub_incubator/01 Acute Activity/wrangling/data/base_files/" #for desktop
basefile_path <- "/conf/pub_incubator/01 Acute Activity/wrangling/data/base_files/" #for server

##############################################.             
##############Beds data ----   
##############################################. 
#Data from SPSS method - developed by SCT
data_bed <- read_csv(paste(basefile_path, "beds_final.csv", sep="")) %>%
  mutate(value = round(value, 1)) %>% 
  mutate_if(is.character, factor) %>% #converting characters into factors
  select(-c(lookup)) %>% 
  mutate(ind = recode(ind, "Average Available Staffed Beds" = "aasb", "% Occupancy" = "p_occ")) %>% 
  dcast(QTR + location + specname ~ ind,  fun.aggregate = sum) %>% 
  rename(quarter_name = QTR, loc_name = location)

saveRDS(data_bed, "./data/beds.rds")
data_bed <- readRDS("./data/beds.rds") 

#Data from R method - developed by TPP
# data_bed <- read_csv(paste(basefile_path, "QAcute_Dec17_beds.csv", sep="")) %>%
#   select(-c(quarter_date, hb_code, hb_name, loc_code)) %>%
#   mutate_at(c("asb", "aob", "p_occ"), funs(round(.,1))) %>%
#   mutate_if(is.character, factor) #converting characters into factors
# 
# saveRDS(data_bed, "./data/beds.rds")
# data_bed <- readRDS("./data/beds.rds") 

##############################################.             
##############Specialty data ----   
##############################################.     
#################Ip data
data_specip_res <- read_csv(paste(basefile_path, "QAcute_Dec17_IPDC_stays_res_spec.csv", sep="")) %>%
  # Excluding Scotland and Golden Jubilee (so no duplicate when merging).
  subset(!(loc_name %in% c("Scotland", 'Golden Jubilee National Hospital'))) %>%
  mutate(geo_type = ifelse(hb_name == "Other" | hb_name == "Null" | loc_name == "Other", "Other ",
                           ifelse(substr(loc_code, 1, 3) == "S12", "Council area of residence",
                                  "Health board of residence"))) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

data_specip_treat <- read_csv(paste(basefile_path, "QAcute_Dec17_IPDC_stays_treat_spec.csv", sep="")) %>%
    # Excluding Golden Jubilee hb code (to avoid duplication)
    subset(!loc_code == "S08100001") %>%
    mutate(geo_type = ifelse(substr(loc_code, 1, 3) == "S08",
                             "Health board of treatment",
                             ifelse(loc_code == "scot", "Scotland",
                                    ifelse(hb_name == "Null" | hb_name == "Other"
                                           | loc_name == "Other", "Other", "Hospital of treatment")))) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

data_specip <- rbind(data_specip_treat, data_specip_res) %>%
  mutate(avlos = round(avlos, 1)) %>%
  subset(!(is.na(measure))) %>% # a few random null measure cases
  mutate(file="Inpatients/Day Cases") 

saveRDS(data_specip, "./data/spec_IP.rds")
data_specip <- readRDS("./data/spec_IP.rds") 

# ###########################.
# ##################Op data
data_specop_res <- read_csv(paste(basefile_path, "QAcute_Dec17_OP_res_spec.csv", sep="")) %>%
  # Excluding Scotland and Golden Jubilee(so no duplicate when merging)
  subset(!(loc_name %in% c("Scotland", 'Golden Jubilee National Hospital'))) %>%
  mutate(geo_type = ifelse(hb_name == "Other" | hb_name == "Null" | loc_name == "Other", "Other",
                           ifelse(substr(loc_code, 1, 3) == "S12", "Council area of residence",
                                  "Health board of residence"))) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

data_specop_treat <- read_csv(paste(basefile_path, "QAcute_Dec17_OP_treat_spec.csv", sep="")) %>%
  # Excluding Golden Jubilee hb code (to avoid duplication)
  subset(!(loc_code == "board" & hb_code == "S08100001")) %>%
  mutate(geo_type = ifelse(substr(hb_code, 1, 3) == "S08" & loc_code == "board",
                           "Health board of treatment",
                           ifelse(loc_code == "scot", "Scotland",
                                  ifelse(hb_name == "Null", "Other", "Hospital of treatment")))) %>%
  mutate(rate = as.numeric(rate)) %>% 
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

data_specop <- rbind(data_specop_treat, data_specop_res) %>%
  rename(measure = appt_type) %>%
  mutate(file="Outpatients") %>%
  mutate(rate = round(rate, 1))

saveRDS(data_specop, "./data/spec_OP.rds")

#Merging both datasets, ip and op.
data_spec <- bind_rows(data_specop, data_specip) %>% 
  #excluding others as there are duplicates from both files
  subset(geo_type != "Other")

data_spec$measure <- as.character(data_spec$measure)
data_spec$measure[data_spec$measure=="New"] <- "New appointments"
data_spec$measure[data_spec$measure=="Return"] <- "Return appointments"
data_spec$measure <- as.factor(data_spec$measure)
data_spec$file <- as.factor(data_spec$file)

saveRDS(data_spec, "./data/spec_IPOP.rds")
data_spec <- readRDS("./data/spec_IPOP.rds") 

##############################################.             
##############SIMD data ----   
##############################################.     
#################Ip data
data_simdip_res <- read_csv(paste(basefile_path, "QAcute_Dec17_IPDC_stays_res_simd.csv", sep="")) %>%
  # Excluding Scotland and Golden Jubilee (so no duplicate when merging).
  subset(!(loc_name %in% c("Scotland", 'Golden Jubilee National Hospital'))) %>%
  mutate(geo_type = ifelse(hb_name == "Other" | hb_name == "Null" | loc_name == "Other", "Other",
                           ifelse(substr(loc_code, 1, 3) == "S12", "Council area of residence",
                                  "Health board of residence"))) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

data_simdip_treat <- read_csv(paste(basefile_path, "QAcute_Dec17_IPDC_stays_treat_simd.csv", sep="")) %>%
  # Excluding Golden Jubilee hb code (to avoid duplication)
  subset(!loc_code == "S08100001") %>%
  mutate(geo_type = ifelse(substr(loc_code, 1, 3) == "S08",
                           "Health board of treatment",
                           ifelse(loc_code == "scot", "Scotland",
                                  ifelse(hb_name == "Null" | hb_name == "Other"
                                         | loc_name == "Other", "Other", "Hospital of treatment")))) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

data_simdip <- rbind(data_simdip_treat, data_simdip_res) %>%
  mutate(avlos = round(avlos, 1)) %>%
  mutate(file="Inpatients/Day Cases") %>%
  subset(!(is.na(measure))) %>% # a few random null measure cases
  rename(count=stays)

saveRDS(data_simdip, "./data/SIMD_IP.rds")
data_simdip <- readRDS("./data/SIMD_IP.rds")

##########################.
#################Op data
data_simdop_res <- read_csv(paste(basefile_path, "QAcute_Dec17_OP_res_simd.csv", sep="")) %>%
  # Excluding Scotland and Golden Jubilee(so no duplicate when merging)
  subset(!(loc_name %in% c("Scotland", 'Golden Jubilee National Hospital'))) %>%
  mutate(geo_type = ifelse(hb_name == "Other" | hb_name == "Null" | loc_name == "Other", "Other",
                           ifelse(substr(loc_code, 1, 3) == "S12", "Council area of residence",
                               "Health board of residence"))) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

data_simdop_treat <- read_csv(paste(basefile_path, "QAcute_Dec17_OP_treat_simd.csv", sep="")) %>%
  # Excluding Golden Jubilee hb code (to avoid duplication)
  subset(!(loc_code == "board" & hb_code == "S08100001")) %>%
  #Excluding three codes with no name, giving issues
  subset(!(loc_code %in% c('s217H', "s217v", "S127v"))) %>%
  mutate(geo_type = ifelse(substr(hb_code, 1, 3) == "S08" & loc_code == "board",
                           "Health board of treatment",
                           ifelse(loc_code == "scot", "Scotland",
                                  ifelse(hb_name == "Null" | hb_name == "Other"
                                         | loc_name == "Other", "Other", "Hospital of treatment")))) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

data_simdop <- rbind(data_simdop_treat, data_simdop_res) %>%
  rename(measure = appt_type) %>%
  mutate(rate = round(rate, 1)) %>%
  mutate(file="Outpatients")

saveRDS(data_simdop, "./data/SIMD_OP.rds")
data_simdop <- readRDS("./data/SIMD_OP.rds")

# Merging both datasets, ip and op.
data_simd <- bind_rows(data_simdop, data_simdip) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  subset(loc_name != "Null" & !is.na(simd)) %>% #Excluding Null values
  droplevels()

data_simd$measure <- as.character(data_simd$measure)
data_simd$measure[data_simd$measure=="New"] <- "New appointments"
data_simd$measure[data_simd$measure=="Return"] <- "Return appointments"
data_simd$measure <- as.factor(data_simd$measure)
data_simd$file <- as.factor(data_simd$file)
data_simd$simd <- as.factor(data_simd$simd)
data_simd$simd <- recode(data_simd$simd, "1" = "1 - Most deprived", "5" = "5 - Least deprived")

saveRDS(data_simd, "./data/SIMD_IPOP.rds")
data_simd <- readRDS("./data/SIMD_IPOP.rds")

##############################################.             
##############Time trend data ----   
##############################################.     
#################Ip data
data_trendipres <- read_csv(paste(basefile_path, "QAcute_Dec17_IPDC_stays_res_all.csv", sep="")) %>%
  # Excluding Scotland (so no duplicate when merging) and non-territorial codes.
  subset(!(loc_name %in% c("Scotland", 'Golden Jubilee National Hospital'))) %>%
  mutate(geo_type = ifelse(hb_name == "Other" | hb_name == "Null" | loc_name == "Other", "Other",
                             ifelse(substr(loc_code, 1, 3) == "S12", "Council area of residence",
                                    "Health board of residence"))) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

data_trendiptreat <- read_csv(paste(basefile_path, "QAcute_Dec17_IPDC_stays_treat_all.csv", sep="")) %>%
  # Excluding Golden Jubilee hb code (to avoid duplication)
  subset(!loc_code == "S08100001") %>%
  mutate(geo_type = ifelse(substr(loc_code, 1, 3) == "S08",
                             "Health board of treatment",
                             ifelse(loc_code == "scot", "Scotland",
                                    ifelse(hb_name == "Null" | hb_name == "Other"
                                           | loc_name == "Other", "Other", "Hospital of treatment")))) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

data_trendip <- rbind(data_trendiptreat, data_trendipres) %>%
  mutate(avlos = round(avlos, 1)) %>%
  mutate(file="Inpatients/Day Cases") %>%
  subset(!(is.na(measure))) %>% # a few random null measure cases
  rename(count = stays)

#For some reason IP and OP have different formatting numbers for dates
data_trendip$quarter_date2 <- as.yearmon(data_trendip$quarter_date, "%d/%m/%Y") #date format

saveRDS(data_trendip, "./data/trend_IP.rds")
data_trendip <- readRDS("./data/trend_IP.rds")

##########################.
#################Op data
data_trendopres <- read_csv(paste(basefile_path, "QAcute_Dec17_OP_res_all.csv", sep="")) %>%
  # Excluding Scotland (so no duplicate when merging) and non-territorial codes.
  subset(!(loc_name %in% c("Scotland", 'Golden Jubilee National Hospital'))) %>%
    mutate(geo_type = ifelse(hb_name == "Other" | hb_name == "Null" | loc_name == "Other", "Other",
                             ifelse(substr(loc_code, 1, 3) == "S12", "Council area of residence",
                                 "Health board of residence"))) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

data_trendoptreat <- read_csv(paste(basefile_path, "QAcute_Dec17_OP_treat_all.csv", sep="")) %>%
  # Excluding Golden Jubilee hb code (to avoid duplication)
  subset(!(loc_code == "board" & hb_code == "S08100001")) %>%
  #Excluding  codes with no name, giving issues
  subset(!(is.na(loc_name) | loc_name == "NULL")) %>%
  mutate(geo_type = ifelse(substr(loc_code, 1, 3) == "S08" | loc_code == "board",
                             "Health board of treatment",
                             ifelse(loc_code == "scot", "Scotland",
                                    ifelse(hb_name == "Null" | hb_name == "Other"
                                           | loc_name == "Other", "Other", "Hospital of treatment")))) %>%
  mutate(rate = as.numeric(rate)) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

# For some reason this file is slightly different in date formatting
data_trendoptreat$quarter_date <- as.factor(gsub("/", "-", data_trendoptreat$quarter_date))
data_trendoptreat$quarter_date <- as.factor(gsub("20", "", data_trendoptreat$quarter_date))

data_trendop <- rbind(data_trendoptreat, data_trendopres) %>%
  rename(measure = appt_type)  %>%
  mutate(rate = round(rate, 1)) %>%
  mutate(file = "Outpatients")

data_trendop$measure <- as.character(data_trendop$measure)
data_trendop$measure[data_trendop$measure=="New"] <- "New appointments"
data_trendop$measure[data_trendop$measure=="Return"] <- "Return appointments"
data_trendop$measure <- as.factor(data_trendop$measure)

data_trendop$quarter_date2 <- as.yearmon(data_trendop$quarter_date, "%d-%m-%y") #date format

saveRDS(data_trendop, "./data/trend_OP.rds")
data_trendop <- readRDS("./data/trend_OP.rds")

#Merging both datasets, ip and op.
data_trend <- bind_rows(data_trendop, data_trendip) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  subset(loc_name != "Null") %>% #Excluding Null values
  droplevels()

#Long format, each measure type a row
# data_trend <- data_trend %>% melt() %>% subset(!is.na(value))
# data_trend$variable <- data_trend$variable %>%
#   recode("count" = "Number", "rate" = "DNA rate", "los" = "Total length of stay",
#          "avlos" = "Mean length of stay")

data_trend <- data_trend %>% arrange(quarter_date2) #sorting by date, so no odd plotting issues

saveRDS(data_trend, "./data/trend_IPOP.rds")
data_trend <- readRDS("./data/trend_IPOP.rds") 

##############################################.             
##############Population pyramid data ----   
##############################################.     
#################Ip data
data_pyramid_ipres <- read_csv(paste(basefile_path, "QAcute_Dec17_IPDC_stays_res_agesex.csv", sep="")) %>%
  # Excluding Scotland (so no duplicate when merging) and non-territorial codes.
  subset(!(loc_name %in% c("Scotland", 'Golden Jubilee National Hospital'))) %>%
    mutate(geo_type = ifelse(hb_name == "Other" | hb_name == "Null" | loc_name == "Other", "Other",
                             ifelse(substr(loc_code, 1, 3) == "S12", "Council area of residence",
                                    "Health board of residence"))) %>%
  separate(sex_age, c("sex", "age"), " ") %>% #Splitting into 2 columns
  mutate_if(is.character, factor) %>% #converting characters into factors
  select(-c(hb_code, hb_name, loc_code)) %>%
  droplevels()

data_pyramid_iptreat <- read_csv(paste(basefile_path, "QAcute_Dec17_IPDC_stays_treat_agesex.csv", sep="")) %>%
  # Excluding Golden Jubilee hb code (to avoid duplication)
  subset(!loc_code == "S08100001") %>%
    mutate(geo_type = ifelse(substr(loc_code, 1, 3) == "S08",
                             "Health board of treatment",
                             ifelse(loc_code == "scot", "Scotland",
                                    ifelse(hb_name == "Null" | hb_name == "Other"
                                           | loc_name == "Other", "Other", "Hospital of treatment")))) %>%
  separate(sex_age, c("sex", "age"), " ") %>% #Splitting into 2 columns
  mutate_if(is.character, factor) %>% #converting characters into factors
  select(-c(hb_code, hb_name, loc_code)) %>%
  droplevels()

data_pyramidip <- rbind(data_pyramid_iptreat, data_pyramid_ipres) %>%
  rename(count = stays) %>%
  subset(!(is.na(measure))) %>% # a few random null measure cases
  mutate(file = "Inpatients/Day Cases")

saveRDS(data_pyramidip, "./data/pyramid_IP.rds")
data_pyramidip <- readRDS("./data/pyramid_IP.rds")

###########################.
##################Op data
data_pyramidopres <- read_csv(paste(basefile_path, "QAcute_Dec17_OP_res_agesex.csv", sep="")) %>%
  # Excluding Scotland (so no duplicate when merging) and non-territorial codes.
  subset(!(loc_name %in% c("Scotland", 'Golden Jubilee National Hospital'))) %>%
  mutate(geo_type = ifelse(hb_name == "Other" | hb_name == "Null" | loc_name == "Other", "Other",
                             ifelse(substr(loc_code, 1, 3) == "S12", "Council area of residence",
                                 "Health board of residence"))) %>%
  separate(sex_age, c("sex", "age"), " ") %>% #Splitting into 2 columns
  mutate_if(is.character, factor) %>% #converting characters into factors
  select(-c(hb_code, hb_name, loc_code)) %>%
  droplevels()

data_pyramidoptreat <- read_csv(paste(basefile_path, "QAcute_Dec17_OP_treat_agesex.csv", sep="")) %>%
  # Excluding Golden Jubilee hb code (to avoid duplication)
  subset(!(loc_code == "board" & hb_code == "S08100001")) %>%
  #Excluding  codes with no name, giving issues
  subset(!(is.na(loc_name) | loc_name == "NULL")) %>%
  mutate(geo_type = ifelse(substr(hb_code, 1, 3) == "S08" & loc_code == "board",
                             "Health board of treatment",
                             ifelse(loc_code == "scot", "Scotland",
                                    ifelse(hb_name == "Null" | hb_name == "Other"
                                           | loc_name == "Other", "Other", "Hospital of treatment")))) %>%
  separate(sex_age, c("sex", "age"), " ") %>% #Splitting into 2 columns
  mutate_if(is.character, factor) %>% #converting characters into factors
  select(-c(hb_code, hb_name, loc_code)) %>%
  droplevels()

data_pyramidop <- rbind(data_pyramidoptreat, data_pyramidopres) %>%
  rename(measure = appt_type) %>%
  mutate(rate = round(rate, 1)) %>% 
  mutate(file="Outpatients")

saveRDS(data_pyramidop, "./data/pyramid_OP.rds")
data_pyramidop <- readRDS("./data/pyramid_OP.rds")

#Merging both datasets, ip and op.
data_pyramid <- bind_rows(data_pyramidop, data_pyramidip) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  subset(loc_name != "Null") %>% #Excluding Null values
  droplevels()
data_pyramid$quarter_date2 <- as.yearmon(data_pyramid$quarter_date, "%d-%m-%y") #date format

data_pyramid$measure <- as.character(data_pyramid$measure)
data_pyramid$measure[data_pyramid$measure=="New"] <- "New appointments"
data_pyramid$measure[data_pyramid$measure=="Return"] <- "Return appointments"
data_pyramid$measure <- as.factor(data_pyramid$measure)
data_pyramid$avlos <- round(data_pyramid$avlos,1)

saveRDS(data_pyramid, "./data/pyramid_IPOP.rds")
data_pyramid <- readRDS("./data/pyramid_IPOP.rds")

##############################################.             
##############Map data ----   
##############################################.     
#################Ip data
data_mapipdc <- read_csv(paste(basefile_path, "QAcute_Dec17_IPDC_stays_res_all.csv", sep="")) %>%
  subset(!(loc_name %in% c("Others"))) %>%  # Excluding Scotland, Golden Jubilee and non-territorial codes.
  subset(!(hb_name %in% c("Scotland", "Other"))) %>%  # Excluding Scotland, Golden Jubilee and non-territorial codes.
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

## Temporal solution for crude rate - dummy variable
#data_mapipdc$cruderate <- round(data_mapipdc$cruderate, 0) # rounding to 0
data_mapipdc$cruderate <- 1:5047 #dummy variable for now
#data_mapipdc$quarter_date <- as.yearmon(data_mapipdc$quarter_date, "%d-%m-%y") #date format

#Creating labels for map tooltip
data_mapipdc$labs <- paste0( data_mapipdc$loc_name, '</br>', 'Admissions: ', data_mapipdc$stays,
                     '</br>', 'Crude rate: ', data_mapipdc$cruderate)

#Long format to allow multiple selections in map
data_mapipdc <- data_mapipdc %>% melt(measure.vars = c("stays","cruderate"), variable.name="value_type")
data_mapipdc$value_type <- data_mapipdc$value_type %>% recode("stays" = "Admissions", "cruderate" = "Crude rate")

saveRDS(data_mapipdc, "./data/ipdc_map.rds")
data_mapipdc <- readRDS("./data/ipdc_map.rds")

###########################.
##################Op data
data_mapop <- read_csv(paste(basefile_path, "QAcute_Dec17_OP_res_all.csv", sep="")) %>%
  subset(!(loc_name %in% c("Other", "Scotland"))) %>%  # Excluding Scotland, Golden Jubilee and non-territorial codes.
  subset(!(hb_name %in% c("Scotland", "Other"))) %>%  # Excluding Scotland, Golden Jubilee and non-territorial codes.
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels()

# ## Temporal solution for crude rate - dummy variable
data_mapop$cruderate <- 1:3496 #dummy variable for now
#data_mapop$rate[is.na(data$rate)] <- sample(10:100, size=sum(is.na(data_mapop$rate)), replace=T)
# data_mapop$quarter_date <- as.yearmon(data_mapop$quarter_date, "%d-%m-%y") #date format

# #Creating labels for map tooltip
data_mapop$labs <- paste0( data_mapop$loc_name, '</br>', 'Appointments: ', data_mapop$count,
                     '</br>', 'Rate: ', data_mapop$cruderate)

#Long format to allow multiple selections in map
data_mapop <- data_mapop %>% melt(measure.vars = c("count","rate", "cruderate"),
                      variable.name="value_type") %>%
  filter(!is.na(value))

data_mapop$value_type <- data$value_type %>%
  recode("count" = "Appointments", "rate" = "DNA rate", "cruderate"="Crude rate")

saveRDS(data, "./data/op_map.rds")

data_mapop <- readRDS("./data/op_map.rds")

##############################################.             
##############Cross-boundary data ----   
##############################################.     
#Ip data
data_cbfip <- read_csv(paste(basefile_path, "QAcute_Dec17_IPDC_cbf.csv", sep="")) %>%
  subset(!(hbtreat_name %in% c("Non-NHS Provider")) #only Health boards
         & !(hbres_name %in% c("Other"))) %>%  #only Health boards
  select(-c(hbtreat_currentdate, hbres_currentdate, episodes)) %>%
  rename(count=stays) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
  droplevels() #dropping missing factor levels

#The Sankey diagram dislike duplicates - so set unique labels for from/to
data_cbfip$hbtreat_name2 <- paste0(data_cbfip$hbtreat_name,' ')

#Taking out the NHS part from the names, to reduce length of tooltips
data_cbfip$hbtreat_name2 <- data_cbfip$hbtreat_name2  %>% str_replace("NHS ", "")
data_cbfip$hbtreat_name <- data_cbfip$hbtreat_name  %>% str_replace("NHS ", "")
data_cbfip$hbres_name <- data_cbfip$hbres_name  %>% str_replace("NHS ", "")

saveRDS(data_cbfip, "./data/ipdc_crossbf.rds")
data_cbfip <- readRDS("./data/ipdc_crossbf.rds") 

#Me trying to modify correctly the dates.
# data_cbfip$quarter_name2 <- data_cbfip$quarter_name #This variable is created to be displayed in the table correctly
# data_cbfip$quarter_date2 <- as.yearmon(data_cbfip$quarter_date, "%b-%y")
# 
# data_cbfip[sort(order(data_cbfip$quarter_date)[])]

#Op data
data_cbfop <-  read_csv(paste(basefile_path, "QAcute_Dec17_outpats_cbf.csv", sep="")) %>%
    subset(!(hbtreat_name %in% c("Non-NHS Provider", "Null")) #only Health boards
           & !(hbres_name %in% c("Other"))) %>%  #only Health boards
    select(-c(hbtreat_currentdate, hbres_currentdate)) %>%
  mutate_if(is.character, factor) %>% #converting characters into factors
    rename(count=attendances) %>%
    droplevels()

#The Sankey diagram dislike duplicates - so set unique labels for from/to
data_cbfop$hbtreat_name2 <- paste0(data_cbfop$hbtreat_name,' ')

#Taking out the NHS part from the names
data_cbfop$hbtreat_name <- data_cbfop$hbtreat_name  %>% str_replace("NHS ", "")
data_cbfop$hbtreat_name2 <- data_cbfop$hbtreat_name2  %>% str_replace("NHS ", "")
data_cbfop$hbres_name <- data_cbfop$hbres_name  %>% str_replace("NHS ", "")

saveRDS(data_cbfop, "./data/op_crossbf.rds")
data_cbfop <- readRDS("./data/op_crossbf.rds") 

####################   END   #######################################.