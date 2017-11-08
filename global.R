#Syntax to create the data explorer
#In this file include packages, datasets and anyting that will be used both by UI and server
#Jaime Villacampa October 17

#TODO:
#see server syntax

############################.
##Packages ----
############################.
#library(data.table) #for fast deading of csvs. maybe not needed (readr)
library(DT) #data table
library(googleVis) #charts
library(htmltools) #tooltips
library(leaflet) # mapping package
library(plotly) #interactive visualizations
library (rgdal) #reading shapefiles
library(reshape2) #for melting and dcast
library(shiny)
#library(shinydashboard) #forlayout
#library(shinythemes) #for styling theme
library(stringr) #manipulation strings
library(tidyverse) #data manipulation, etc
library (zoo) #for dates


############################.
##Data ----
############################.
data_type <- c("Outpatients", "Inpatients/Day cases")

#for table selection (table tab)
file_types <-  c("Inpatients/Day cases - Cross boundary flow",
"Inpatients/Day cases - Map and time trend","Outpatients - Cross boundary flow",
"Outpatients - Map and time trend")

geo_types <- c("Scotland", "Health board of treatment", "Health board of residence", 
               "Council area of residence", "Hospital", "Other")

##############################################.             
##############Time trend data ----   
##############################################.     
#################Ip data
# data_trendipres <- read_csv("./data/QAcute_Dec17_IPDC_stays_res_all.csv") %>% 
#   # Excluding Scotland (so no duplicate when merging) and non-territorial codes.
#   subset(!(loc_name %in% c("Scotland", 'Golden Jubilee National Hospital'))) %>% 
#   mutate(geo_type = ifelse(substr(loc_code, 1, 3) == "S08", "Health board of residence", 
#                            ifelse(substr(loc_code, 1, 3) == "S12", "Council area of residence",
#                                   "Other"))) %>% 
#   mutate_if(is.character, factor) %>% #converting characters into factors
#   droplevels() 
# 
# data_trendiptreat <- read_csv("./data/QAcute_Dec17_IPDC_stays_treat_all.csv") %>% 
#   # Excluding Golden Jubilee hb code (to avoid duplication)
#   subset(!loc_code == "S08100001") %>% 
#   mutate(geo_type = ifelse(substr(loc_code, 1, 3) == "S08", "Health board of treatment",
#                            ifelse(loc_code == "scot", "Scotland", "Hospital"))) %>% 
#   mutate_if(is.character, factor) %>% #converting characters into factors
#   droplevels() 
# 
# data_trendip <- rbind(data_trendiptreat, data_trendipres) 
# saveRDS(data_trendip, "./data/trend_IP.rds")

data_trendip <- readRDS("./data/trend_IP.rds")
data_trendip$quarter_date2 <- as.yearmon(data_trendip$quarter_date, "%d-%m-%y") #date format

###########################.
##################Op data
# data_trendopres <- read_csv("./data/QAcute_Dec17_OP_res_all.csv") %>% 
#   # Excluding Scotland (so no duplicate when merging) and non-territorial codes.
#   subset(!(loc_name %in% c("Scotland", 'Golden Jubilee National Hospital'))) %>% 
#   mutate(geo_type = ifelse(substr(hb_code, 1, 3) == "S08" & loc_code == "board",
#                            "Health board of residence", 
#                            ifelse(substr(loc_code, 1, 3) == "S12", "Council area of residence",
#                                   "Other"))) %>% 
#   mutate_if(is.character, factor) %>% #converting characters into factors
#   droplevels() 
# 
# data_trendoptreat <- read_csv("./data/QAcute_Dec17_OP_treat_all.csv") %>%
#   # Excluding Golden Jubilee hb code (to avoid duplication)
#   subset(!(loc_code == "board" & hb_code == "S08100001")) %>% 
#   mutate(geo_type = ifelse(substr(hb_code, 1, 3) == "S08" & loc_code == "board", 
#                            "Health board of treatment",
#                            ifelse(loc_code == "scot", "Scotland", 
#                                   ifelse(hb_name == "Null", "Other", "Hospital")))) %>% 
#   mutate_if(is.character, factor) %>% #converting characters into factors
#   droplevels() 
# 
# data_trendop <- rbind(data_trendoptreat, data_trendopres) %>% 
#   rename(measure = appt_type)  

# data_trendop$measure <- as.character(data_trendop$measure)
# data_trendop$measure[data_trendop$measure=="New"] <- "New appointments"
# data_trendop$measure[data_trendop$measure=="Return"] <- "Return appointments"
# data_trendop$measure <- as.factor(data_trendop$measure)
# 
# saveRDS(data_trendop, "./data/trend_OP.rds")

data_trendop <- readRDS("./data/trend_OP.rds")

data_trendop$quarter_date2 <- as.yearmon(data_trendop$quarter_date, "%d-%m-%y") #date format

trend_measure <- c(as.character(unique(data_trendip$measure)), 
                   as.character(unique(data_trendop$measure)))

##############################################.             
##############Population pyramid data ----   
##############################################.     
#################Ip data
# data_pyramid_ipres <- read_csv("./data/QAcute_Dec17_IPDC_stays_res_agesex.csv") %>%
#   # Excluding Scotland (so no duplicate when merging) and non-territorial codes.
#   subset(!(loc_name %in% c("Scotland", 'Golden Jubilee National Hospital'))) %>% 
#   mutate(geo_type = ifelse(substr(loc_code, 1, 3) == "S08", "Health board of residence", 
#                            ifelse(substr(loc_code, 1, 3) == "S12", "Council area of residence",
#                                   "Other"))) %>% 
#   separate(sex_age, c("sex", "age"), " ") %>% #Splitting into 2 columns
#   mutate_if(is.character, factor) %>% #converting characters into factors
#   select(-c(hb_code, hb_name, loc_code, los, avlos)) %>% 
#   droplevels() 
# 
# data_pyramid_iptreat <- read_csv("./data/QAcute_Dec17_IPDC_stays_treat_agesex.csv") %>% 
#   # Excluding Golden Jubilee hb code (to avoid duplication)
#   subset(!loc_code == "S08100001") %>% 
#   mutate(geo_type = ifelse(substr(loc_code, 1, 3) == "S08", "Health board of treatment",
#                            ifelse(loc_code == "scot", "Scotland", "Hospital"))) %>% 
#   separate(sex_age, c("sex", "age"), " ") %>% #Splitting into 2 columns
#   mutate_if(is.character, factor) %>% #converting characters into factors
#   select(-c(hb_code, hb_name, loc_code, los, avlos)) %>% 
#   droplevels() 
# 
# data_pyramidip <- rbind(data_pyramid_iptreat, data_pyramid_ipres) 
# saveRDS(data_pyramidip, "./data/pyramid_IP.rds")

# data_pyramidip <- readRDS("./data/pyramid_IP.rds") %>% 
#   rename(count = stays)
# data_pyramidip$quarter_date2 <- as.yearmon(data_pyramidip$quarter_date, "%d-%m-%y") #date format

###########################.
##################Op data
# data_pyramidopres <- read_csv("./data/QAcute_Dec17_OP_res_agesex.csv") %>% 
#   # Excluding Scotland (so no duplicate when merging) and non-territorial codes.
#   subset(!(loc_name %in% c("Scotland", 'Golden Jubilee National Hospital'))) %>% 
#   mutate(geo_type = ifelse(substr(hb_code, 1, 3) == "S08" & loc_code == "board",
#                            "Health board of residence", 
#                            ifelse(substr(loc_code, 1, 3) == "S12", "Council area of residence",
#                                   "Other"))) %>% 
#   separate(sex_age, c("sex", "age"), " ") %>% #Splitting into 2 columns
#   mutate_if(is.character, factor) %>% #converting characters into factors
#   select(-c(hb_code, hb_name, loc_code, rate)) %>% 
#   droplevels() 
# 
# data_pyramidoptreat <- read_csv("./data/QAcute_Dec17_OP_treat_agesex.csv") %>%
#   # Excluding Golden Jubilee hb code (to avoid duplication)
#   subset(!(loc_code == "board" & hb_code == "S08100001")) %>% 
#   mutate(geo_type = ifelse(substr(hb_code, 1, 3) == "S08" & loc_code == "board", 
#                            "Health board of treatment",
#                            ifelse(loc_code == "scot", "Scotland", 
#                                   ifelse(hb_name == "Null", "Other", "Hospital")))) %>% 
#   separate(sex_age, c("sex", "age"), " ") %>% #Splitting into 2 columns
#   mutate_if(is.character, factor) %>% #converting characters into factors
#   select(-c(hb_code, hb_name, loc_code, rate)) %>% 
#   droplevels() 
# 
# data_pyramidop <- rbind(data_pyramidoptreat, data_pyramidopres) 
# 
# saveRDS(data_pyramidop, "./data/pyramid_OP.rds")

# data_pyramidop <- readRDS("./data/pyramid_OP.rds")%>% 
#   rename(measure = appt_type)
# 
# #Merging both datasets, ip and op.
# data_pyramid <- rbind(data_pyramidop, data_pyramidip)
# data_pyramid$quarter_date2 <- as.yearmon(data_pyramid$quarter_date, "%d-%m-%y") #date format
# 
# data_pyramid$measure <- as.character(data_pyramid$measure)
# data_pyramid$measure[data_pyramid$measure=="New"] <- "New appointments"
# data_pyramid$measure[data_pyramid$measure=="Return"] <- "Return appointments"
# data_pyramid$measure <- as.factor(data_pyramid$measure)
# 
# saveRDS(data_pyramid, "./data/pyramid_IPOP.rds")

data_pyramid <- readRDS("./data/pyramid_IPOP.rds")

##############################################.             
##############Map data ----   
##############################################.     
#################Ip data
# data_mapipdc <- read.csv("./data/QAcute_Dec17_IPDC_stays_res_all.csv", na.strings=c(""," ","NA")) %>% 
#   subset(!(loc_name %in% c("Others"))) %>%  # Excluding Scotland, Golden Jubilee and non-territorial codes.
#   subset(!(hb_name %in% c("Scotland", "Other"))) %>%  # Excluding Scotland, Golden Jubilee and non-territorial codes.
#   droplevels() 
# 
# ## Temporal solution for crude rate - dummy variable
# #data_mapipdc$cruderate <- round(data_mapipdc$cruderate, 0) # rounding to 0
# data_mapipdc$cruderate <- 1:5047 #dummy variable for now
# #data_mapipdc$quarter_date <- as.yearmon(data_mapipdc$quarter_date, "%d-%m-%y") #date format
#  
# #Creating labels for map tooltip
# data_mapipdc$labs <- paste0( data_mapipdc$loc_name, '</br>', 'Admissions: ', data_mapipdc$stays,
#                      '</br>', 'Crude rate: ', data_mapipdc$cruderate) 
# 
# #Long format to allow multiple selections in map
# data_mapipdc <- data_mapipdc %>% melt(measure.vars = c("stays","cruderate"), variable.name="value_type")
# data_mapipdc$value_type <- data_mapipdc$value_type %>% recode("stays" = "Admissions", "cruderate" = "Crude rate")
# 
# saveRDS(data_mapipdc, "./data/ipdc_map.rds") 
data_mapipdc <- readRDS("./data/ipdc_map.rds")  

###########################.
##################Op data
# data_mapop <- read.csv("./data/QAcute_Dec17_OP_res_all.csv", na.strings=c(""," ","NA")) %>% 
#   subset(!(loc_name %in% c("Other", "Scotland"))) %>%  # Excluding Scotland, Golden Jubilee and non-territorial codes.
#   subset(!(hb_name %in% c("Scotland", "Other"))) %>%  # Excluding Scotland, Golden Jubilee and non-territorial codes.
#   droplevels() 

# # ## Temporal solution for crude rate - dummy variable
# data_mapop$cruderate <- 1:3496 #dummy variable for now
# #data_mapop$rate[is.na(data$rate)] <- sample(10:100, size=sum(is.na(data_mapop$rate)), replace=T)
# # data_mapop$quarter_date <- as.yearmon(data_mapop$quarter_date, "%d-%m-%y") #date format
# 
# # #Creating labels for map tooltip
# data_mapop$labs <- paste0( data_mapop$loc_name, '</br>', 'Appointments: ', data_mapop$count,
#                      '</br>', 'Rate: ', data_mapop$cruderate) 
# 
# #Long format to allow multiple selections in map
# data_mapop <- data_mapop %>% melt(measure.vars = c("count","rate", "cruderate"), 
#                       variable.name="value_type") %>% 
#   filter(!is.na(value)) 
# 
# data_mapop$value_type <- data$value_type %>% 
#   recode("count" = "Appointments", "rate" = "DNA rate", "cruderate"="Crude rate")
# 
# saveRDS(data, "./data/op_map.rds") 

data_mapop <- readRDS("./data/op_map.rds") 


##########.
#Shapefile data
ca_bound<-readOGR("./shapefiles","CA_simpl") #Reading file with council shapefiles
hb_bound<-readOGR("./shapefiles","HB_simpl") #Reading file with health board shapefiles

##############################################.             
##############Cross-boundary data ----   
##############################################.     
#Ip data
# data <- as_tibble(fread("./data/QAcute_Dec17_IPDC_cbf.csv", stringsAsFactors=TRUE)) %>% 
#   subset(!(hbtreat_name %in% c("Non-NHS Provider")) #only Health boards
#          & !(hbres_name %in% c("Other")) #only Health boards
#          & boundary_ind == 1) %>%  #only including flows out of the HB
#   select(-c(boundary_ind, hbtreat_currentdate, hbres_currentdate, episodes)) %>%
#   rename(count=stays) %>% 
#   droplevels() #dropping missing factor levels 
# 
# #The Sankey diagram dislike duplicates - so set unique labels for from/to
# data$hbtreat_name <- paste0(data$hbtreat_name,' ')
# 
# #Taking out the NHS part from the names
# data$hbtreat_name <- data$hbtreat_name  %>% str_replace("NHS ", "")
# data$hbres_name <- data$hbres_name  %>% str_replace("NHS ", "")
# 
# saveRDS(data, "./data/ipdc_crossbf.rds")

data_cbfip <- readRDS("./data/ipdc_crossbf.rds") %>% 
  droplevels()

#Me trying to modify correctly the dates.
# data_cbfip$quarter_name2 <- data_cbfip$quarter_name #This variable is created to be displayed in the table correctly
# data_cbfip$quarter_date2 <- as.yearmon(data_cbfip$quarter_date, "%b-%y")
# 
# data_cbfip[sort(order(data_cbfip$quarter_date)[])]

#Op data
# data_cbfop <- as_tibble(fread("./data/QAcute_Dec17_outpats_cbf.csv", 
#                               stringsAsFactors=TRUE)) %>% 
#     subset(!(hbtreat_name %in% c("Non-NHS Provider", "Null")) #only Health boards
#            & !(hbres_name %in% c("Other")) #only Health boards
#            & boundary_ind == 1) %>%  #only including flows out of the HB
#     select(-c(boundary_ind, hbtreat_currentdate, hbres_currentdate)) %>% 
#     rename(count=attendances) %>% 
#     droplevels() 
# 
# #The Sankey diagram dislike duplicates - so set unique labels for from/to
# data_cbfop$hbtreat_name <- paste0(data_cbfop$hbtreat_name,' ')
# 
# #Taking out the NHS part from the names
# data_cbfop$hbtreat_name <- data_cbfop$hbtreat_name  %>% str_replace("NHS ", "")
# data_cbfop$hbres_name <- data_cbfop$hbres_name  %>% str_replace("NHS ", "")
# 
# saveRDS(data_cbfop, "./data/op_crossbf.rds")

data_cbfop <- readRDS("./data/op_crossbf.rds") %>% 
  droplevels()


############################.
##Plot parameters ----
############################.
##Time trend ----
trend_pal <- c("#004785", "#4c7ea9", "#99b5ce", "#00a2e5", "#4cbeed", "#99daf5")  

##Cross-boundary----
#Defining colors 
colors_node <- c('CornflowerBlue', 'CornflowerBlue', 'CornflowerBlue', 'CornflowerBlue', 'CornflowerBlue', "CornflowerBlue", "CornflowerBlue", 
                 "CornflowerBlue", "CornflowerBlue", "CornflowerBlue", "CornflowerBlue", "CornflowerBlue", "CornflowerBlue", "CornflowerBlue",
                 'CornflowerBlue', 'CornflowerBlue', 'CornflowerBlue', 'CornflowerBlue', 'CornflowerBlue', "CornflowerBlue", "CornflowerBlue", 
                 "CornflowerBlue", "CornflowerBlue", "CornflowerBlue", "CornflowerBlue", "CornflowerBlue", "CornflowerBlue", "CornflowerBlue")

colors_node_array <- paste0("[", paste0("'", colors_node,"'", collapse = ','), "]")
colors_link <-c('blue', 'yellow','brown','lightblue', 'red', 'black',  "gray", "green", 
                "orange", "gray", "pink", "lightgreen", "purple", "cyan")
colors_link_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")

opts <- paste0("{
               link: { colorMode: 'source',
               colors: ", colors_link_array ," },
               node: { colors: ", colors_node_array ," }
               }" )
##END