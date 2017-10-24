#Syntax to create the data explorer
#Jaime Villacampa October 17

#TODO:

#Layout/design/architecture
#What levels of geography will be present? Hospital?
# Use as_tibble(fread("./filename"))
############################.
##Packages ----
############################.
library(dplyr) #data manipulation
library(googleVis) #charts
library(shiny)
library(DT) #data table
library (zoo) #for dates
library(leaflet) # mapping package
library (rgdal) #reading shapefiles
library(reshape2) #for melting and dcast
library(htmltools)

############################.
##Data ----
############################.

##############################################.             
##############Map ipdc data ----   
##############################################.     
data_mapipdc <- readRDS("./ipdc_map.rds")  #to be used once rates are in

#To select most recent quarter, fiddly solution there might be something easier
last_quarter <- unique(data_mapipdc %>% subset(quarter_date==max(quarter_date)) %>%
                         select(quarter_name) %>% droplevels())
last_quarter <- last_quarter$quarter_name

##########.
#Shapefile data
ca_bound<-readOGR("./shapefiles","CA_simpl") #Reading file with council shapefiles
hb_bound<-readOGR("./shapefiles","HB_simpl") #Reading file with health board shapefiles

##############################################.             
##############Cross-boundary data ----   
##############################################.     
data_cbfip <- readRDS("./ipdc_crossbf.rds") %>% 
  subset(substr(quartername, 5, 6) != "17") %>% #to be removed in future
  droplevels()

data_cbfip$quartername2 <- data_cbfip$quartername #This variable is created to be displayed in the table correctly
data_cbfip$quartername <- as.yearmon(data_cbfip$quartername, "%b-%y")


geo_types <- c("Scotland", "Health Board of treatment", "Health Board of residence", 
               "Council", "Hospital")
geo_names <- c("Scotland", "Fife", "Glasgow", "Royal Infirmary")
quarter_data <- c("Dec-16", "Dec-17")
data_type <- c("Outpatients", "Inpatients")
chart_type <- c("Flow between Health boards", "Regional differences - map",
                "Time trend", "Age and sex composition")

############################.
##Plot parameters ----
############################.
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

############################.
## Visual interface ----
############################.

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(style="width: 100%; height: 100%; max-width: 1200px; ",
   titlePanel("Data explorer"),    # Application title
   navbarPage("", # Navigation bar
              
##############################################.             
##############Introduction tab ----   
##############################################.     
    tabPanel("Introduction", icon = icon("info"), style="float: top; height: 95%; 
              width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
      p("Welcome to the data explorer of the hospital acute care statistics")
    ),

##############################################.             
##############Time trend tab ----   
##############################################.     
tabPanel("Time trend", icon = icon("area-chart"), style="float: top; height: 95%; 
          width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
         column(4,
                selectInput("geotype_expl", label = "Geography type", choices = geo_types),
                selectInput("geoname_expl", label = "Geography name", choices = geo_names)
         ),
         column(4,
                selectInput("datatype_expl", label = "Data type", choices = data_type),
                selectInput("charttype_expl", label = "Chart type", choices = chart_type)
         ),
         column(4,
                selectInput("quarter_expl", label = "Quarter ending", choices = quarter_data),
                downloadButton('download_flow', 'Download data', style="margin: 25px 10px 25px 10px ")  #For downloading the data
         ),
         textOutput("text")
),
##############################################.             
##############Age-sex tab ----   
##############################################.     
tabPanel("Age/sex", icon = icon("bar-chart"), style="float: top; height: 95%; 
          width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
         column(4,
                selectInput("geotype_expl", label = "Geography type", choices = geo_types),
                selectInput("geoname_expl", label = "Geography name", choices = geo_names)
         ),
         column(4,
                selectInput("datatype_expl", label = "Data type", choices = data_type),
                selectInput("charttype_expl", label = "Chart type", choices = chart_type)
         ),
         column(4,
                selectInput("quarter_expl", label = "Quarter ending", choices = quarter_data),
                downloadButton('download_flow', 'Download data', style="margin: 25px 10px 25px 10px ")  #For downloading the data
         ),
         textOutput("text")
),
##############################################.             
##############Map tab ----   
##############################################.     
tabPanel("Map", icon = icon("globe"), style="float: top; height: 95%; width: 95%; 
          background-color:#ffffff; border: 0px solid #ffffff;",
         h4(textOutput("title_map")),
         p("There are marked differences among Scottish regions in the numbers of patients 
              admitted to hospital or treated as day cases. This map allows to explore this regional
              differences in total and relative volume of hospital acute care activity in Scotland."),
         div(style="float:right; height: 80%; width: 30%; max-width: 200px; background-color:#ffffff; border: 0px solid #ffffff;",
          selectInput("measure_map", label = "Select category:", 
                      choices=unique(data_mapipdc$measure), width= "95%"),
          selectInput("value_map", label = "Select measure:", 
                choices=c("Episodes", "Crude rate"), width= "95%"),
          selectInput("quarter_map", label = "Select quarter:", 
                      choices =(unique(data_mapipdc$quarter_name)), 
                selected=last_quarter, width= "95%"),
          downloadButton('download_map', 'Download data', width= "95%"),  #For downloading the data
          shiny::hr(),
          h5(style="font-weight: bold;", "Percentile", width= "95%"),
          img(src='legend.png', width= "95%", style="vertical-align:middle")
        ),
        div(style="float:left; height: 80%; width: 62%;  max-width: 500px; background-color:#ffffff; border: 0px solid #ffffff;",
          leafletOutput("map"),
          HTML("<button data-toggle='collapse' data-target='#demo' class='btn btn-primary'>
                  <strong>Show table</strong></button>"),
          HTML("<div id='demo' class='collapse'> "),
          DT::dataTableOutput("table_map", width="95%"),
          HTML("</div>"),
          p("To embed this graphic in your website include this code:"),
          p("<iframe src='https://scotland.shinyapps.io/hospcare_ipdcmap/' style='border: none; 
              width: 100%; height: 100%;'></iframe>")
        )
),
##############################################.             
##############Cross boundary tab ----   
##############################################.     
tabPanel("Cross-boundary", icon = icon("share-alt"), style="float: top; height: 95%; 
          width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
         h4("Where are patients treated? - Flow of patients between Health Boards"),
         p("The majority of patients are treated in a hospital located in their own local 
           NHS Board area. However, around 1 in 8 admissions (12%) are to hospitals within 
           other NHS board areas. Patients are treated outside of their residence area for varied
           and valid reasons. These include where specific services are available, where the 
           emergency occurred, or the ‘catchment’ area of a particular hospital, being the closest
           to the patient."),
         p("The flow of patients between NHS boards varies depending on whether the admission is 
           an emergency or not. Overall, about one in four planned admissions were to hospitals 
           outwith the patient’s NHS board area, compared with only 7% of emergency admissions."),
         p("In addition, all NHS boards refer some patients to the Golden Jubilee National Hospital (GJNH)
           in Clydebank. The GJNH provides a range of national and regional services. It is 
           also a national resource to help meet the demand for planned procedures from across Scotland." 
         ),
         div(style="float: top; height: 95%; width: 95%; background-color:#ffffff; 
             border: 0px solid #ffffff;",
             column(4,  
                    selectInput("quarter_flow", label = "Quarter ending", choices = sort(unique(data_cbfip$quartername)), 
                                selected=max(data_cbfip$quartername))
             ),
             column(4,  
                    selectInput("hbres_flow", label = "Select NHS Board of residence", 
                                choices = unique(data_cbfip$hbres_name))
             ),
             column(4,
                    downloadButton('download_flow', 'Download data', 
                                   style="margin: 25px 10px 25px 10px ")  #For downloading the data
             )
         ),
         div(style="float: bottom; height: 75%; width: 95%; 
             background-color:#ffffff; border: 0px solid #ffffff;",
             column(6,  
                    htmlOutput("sankey_all", width="48%")
             ),
             column(6,
                    htmlOutput("sankey_one", width="48%")
             )
         ),
         div(style="width:95%; height:5%",
             HTML("<button data-toggle='collapse' data-target='#demo' 
                  class='btn btn-primary' style=padding: 6px 12px;>
                  <strong>Show table</strong></button>"),
             HTML("<div id='demo' class='collapse'> "),
             DT::dataTableOutput("table_crossb", width="95%"),
             HTML("</div>")
             ),
         div(style="width:95%; height:5%",
             p("To embed this graphic in your website include this code:                                                                                    "),
             p("<iframe src='https://scotland.shinyapps.io/hospcare_crossboundary/' 
               width=95% height=95%></iframe>")
             )
),
##############################################.             
##############Table tab ----   
##############################################.     
tabPanel("Table", icon = icon("table"), style="float: top; height: 95%; width: 95%; 
          background-color:#ffffff; border: 0px solid #ffffff;",
         column(4,
                selectInput("geotype_expl", label = "Geography type", choices = geo_types),
                selectInput("geoname_expl", label = "Geography name", choices = geo_names)
         ),
         column(4,
                selectInput("datatype_expl", label = "Data type", choices = data_type),
                selectInput("charttype_expl", label = "Chart type", choices = chart_type)
         ),
         column(4,
                selectInput("quarter_expl", label = "Quarter ending", choices = quarter_data),
                downloadButton('download_flow', 'Download data', style="margin: 25px 10px 25px 10px ")  #For downloading the data
         ),
         textOutput("text")
)
   )
))

############################.
## Server ----
############################.

server <- shinyServer(function(input, output) {
  
  dataSource <- reactive({switch(input$charttype_expl,
                                 "Flow between Health boards" = geo_names,
                                 "Regional differences - map" = geo_types,
                                 "Time trend" = quarter_data,
                                 "Age and sex composition" = data_type)})
  
  output$text <- renderText({
    dataSource()
  })
  
  
  ##############################################.             
  ##############Map ----   
  ##############################################.     
  
  #Merging shapefile with dynamic selection of data
  #First for HB
  hb_pol <- reactive({merge(hb_bound, 
                       data_mapipdc %>% subset(quarter_name==input$quarter_map
                                       & measure==input$measure_map
                                       & value_type == input$value_map
                                       & substr(loc_name,1,3)=="NHS") %>% #selecting only HB 
                         droplevels() %>% #dropping missing factor levels to allow merging
                         rename(HBCode=loc_code), 
                       by='HBCode') 
  })   
  
  #Now for CA
  ca_pol <- reactive({merge(ca_bound, 
                            data_mapipdc %>% subset(quarter_name==input$quarter_map
                                            & measure==input$measure_map
                                            & value_type == input$value_map
                                            & substr(loc_name,1,3)!="NHS") %>% #selecting only HB 
                              droplevels() %>% #dropping missing factor levels to allow merging
                              rename(GSS_COD=loc_code) , 
                            by='GSS_COD')
  }) 
  
  
  #Palettes for map.
  pal_hb <- reactive({
    colorQuantile(c('#004785','#00a2e5', '#99b5ce',  '#99daf5'), hb_pol()$value, n=4)
  }) 
  pal_ca <- reactive({
    colorQuantile(c('#004785','#00a2e5', '#99b5ce',  '#99daf5'), ca_pol()$value, n=4)
  }) 
  
  #title
  output$title_map <- renderText(paste("Hospital acute care activity - ", input$measure_map, 
                                       " during ", input$quarter_map))
  
  #Plotting map
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(-5, 56.33148888, zoom = 10) %>% # setting initial view point
      fitBounds(-7.0, 60.5, 0.25, 55.8)  %>% #limits of map
      setMaxBounds(-9.0, 62.5, 2.25, 53)  %>% #limits of map
      addProviderTiles(providers$CartoDB.Positron) %>% #background map
      #Adding layer control
      addLayersControl( 
        baseGroups = c("Health Board", "Council Area"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  #Creating observer to avoid redrawing of map everytime 
  #a different option (time, measure) is chosen
  observe({
    
    leafletProxy("map") %>%
      clearShapes() %>% 
      #Adding polygons with HB
      addPolygons(data=hb_pol(),  group="Health Board",
                  color = "#444444", weight = 2, smoothFactor = 0.5, 
                  label = sprintf(hb_pol()$labs) %>% lapply(HTML), #tooltip for hovering
                  labelOptions = labelOptions(direction = "left"),
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal_hb()(value), # palette
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = FALSE) #For highlighting the selection while hovering
      )%>% 
      #Adding polygons with HB rate 
      addPolygons(data=ca_pol(), group="Council Area",
                  color = "#444444", weight = 2, smoothFactor = 0.5, 
                  label = sprintf(ca_pol()$labs) %>% lapply(HTML), #tooltip for hovering
                  labelOptions = labelOptions(direction = "left"),
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal_ca()(value), # palette
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = FALSE) #For highlighting the selection while hovering
      ) 
  })
  
  #####################################.    
  #### Table ----
  
  #Table data
  table_data <- reactive({
    data_mapipdc %>% subset(quarter_name==input$quarter_map & measure==input$measure_map) %>% 
      select(loc_name, quarter_name, measure, value_type, value) %>% 
      dcast(loc_name+quarter_name+measure~value_type, fun.aggregate=sum)
    
  })
  
  #Actual table.
  output$table_map <- DT::renderDataTable({
    DT::datatable(table_data(),style = 'bootstrap', class = 'table-bordered table-condensed', rownames = FALSE,
                  options = list(pageLength = 14, dom = 'tip'),
                  colnames = c("Location", "Quarter", "Measure", "Episodes", "Crude Rate")  
    )
  })
  
  #####################################.    
  #### Downloading data ----
  output$download_map <- downloadHandler(
    filename =  'map_data.csv',
    content = function(file) {
      write.csv(data_mapipdc, file) 
    }
  )
  
##############################################.             
##############Cross-boundary ----   
##############################################.     
  #Reactive data
  #For all HB
  flow_all <- reactive({
    data_cbfip %>% subset(quartername==input$quarter_flow & episodes>9) #%>% 
    #       group_by(hbres_name, quartername) %>% 
    #       top_n(5, episodes)
    #& episodes>9) Not right now, see TODO
    
  })   
  
  #For only selected HB
  flow_one <- reactive({
    data_cbfip %>% subset(quartername==input$quarter_flow & 
                      hbres_name==input$hbres_flow)
  })   
  
  ############################.
  #Visualizations
  #This one with all HB at the same time.
  output$sankey_all <- renderGvis({
    
    options(gvis.plot.tag=NULL) #if changed to chart you will get the html code
    gvisSankey(flow_all()[,c('hbres_name','hbtreat_name','episodes')],
               options = list(width = "automatic", sankey=opts
               ))
    
  })
  
  #This one with only the selected
  output$sankey_one <- renderGvis({
    
    gvisSankey(flow_one()[,c('hbres_name','hbtreat_name','episodes')],
               options = list(width = "automatic",
                              gvis.plot.tag=NULL))#if changed to chart you will get the html code
    
  })
  
  
  #####################################.    
  #### Table ----
  
  #Table data
  table_data <- reactive({
    data_cbfip %>% subset(quartername==input$quarter_flow) %>% 
      select(hbres_name, hbtreat_name, quartername2, episodes)
  })
  
  #Actual table.
  output$table_crossb <- DT::renderDataTable({
    DT::datatable(table_data(),style = 'bootstrap', class = 'table-bordered table-condensed', rownames = FALSE,
                  options = list(pageLength = 10, dom = 'tip'),
                  colnames = c("Residence board", "Treatment board",  "Quarter", "Episodes")  
    )
  })
  
  #####################################.    
  #### Downloading data ----
  #for downloading data
  output$download_flow <- downloadHandler(
    filename =  'flow_data.csv',
    content = function(file) {
      write.csv(flow_all(), file) 
    }
  )
  
  
  
   
})

############################.
## Calling app ----
############################.

# Run the application 
shinyApp(ui = ui, server = server)

