#Syntax to create the data explorer
#In this file include packages, datasets and anyting that will be used both by UI and server
#Jaime Villacampa October 17

#TODO:
#see server syntax

############################.
##Packages ----
############################.
library(DT) #data tables
library(googleVis) #charts - sankey
library(htmltools) #tooltips
library(leaflet) # mapping package
library(plotly) #interactive visualizations
#library (rgdal) #reading shapefiles. Only needed for map, not in use at the moment
library(reshape2) #for melting and dcast
library(shiny) #interactive framework
library(stringr) #manipulation strings
library(tidyverse) #data manipulation, etc
library (zoo) #for dates

############################.
##Data ----
############################.
data_type <- c("Outpatients", "Inpatients/Day cases")

#for table selection (table tab)
file_types <-  c("Beds", "Inpatients/Day cases - Age/sex", 
"Inpatients/Day cases - Cross boundary flow", "Inpatients/Day cases - Time trend", 
"Inpatients/Day cases - Specialty", "Inpatients/Day cases - Deprivation (SIMD)", 
"Outpatients - Age/sex", "Outpatients - Cross boundary flow", 
"Outpatients - Time trend", "Outpatients - Specialty", 
"Outpatients - Deprivation (SIMD)")

geo_types <- c("Scotland", "Health board of treatment", "Health board of residence", 
               "Council area of residence", "Hospital of treatment", "Other")

##############Beds data ----   
data_bed <- readRDS("./data/beds.rds") 

##############Specialty data ----   

data_spec <- readRDS("./data/spec_IPOP.rds") 
data_spec$file <- as.factor(data_spec$file)

##############SIMD data ----   

data_simd <- readRDS("./data/SIMD_IPOP.rds")

##############Time trend data ----   
data_trend <- readRDS("./data/trend_IPOP.rds") %>% 
  #Excluding this, as it is not aggregated and creates issues
  subset(!(loc_name == "Other" & geo_type == "Other"))

trend_service <- c(as.character(unique(data_trend$measure)))
trend_measure <- c("Number", "DNA rate", "Total length of stay", "Mean length of stay")

##############Population pyramid data ----   

data_pyramid <- readRDS("./data/pyramid_IPOP.rds")

pyramid_service <- c(as.character(unique(data_pyramid$measure)))


##############Map data ----   
# data_mapop <- readRDS("./data/op_map.rds") 
# 
# ##########.
# #Shapefile data
# ca_bound<-readOGR("./shapefiles","CA_simpl") #Reading file with council shapefiles
# hb_bound<-readOGR("./shapefiles","HB_simpl") #Reading file with health board shapefiles

##############Cross-boundary data ----   
data_cbfip <- readRDS("./data/ipdc_crossbf.rds") 
data_cbfop <- readRDS("./data/op_crossbf.rds") 

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

opts <- paste0("{
               node: { colors: ", colors_node_array ," }
               }" )
##END