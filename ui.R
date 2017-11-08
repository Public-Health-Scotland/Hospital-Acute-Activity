#Syntax to create the data explorer
#Jaime Villacampa October 17

#TODO:
#see server syntax

############################.
## Visual interface ----
############################.

# Define UI for application that draws a histogram
fluidPage(style="width: 100%; height: 100%; max-width: 1200px; ",
   navbarPage("", # Navigation bar
              
##############################################.             
##############Introduction tab ----   
##############################################.     
    tabPanel("Introduction", icon = icon("info"), style="float: top; height: 95%; 
              width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
      p(tags$b("Welcome to the data explorer of the hospital acute care statistics."), 
         "This tool is desgined to allow users to investigate the data in detail and to
        obtain the exact piece of information they were after. It includes information
        regarding hospital admissions, day cases and outpatient appointments."),
      p("If you have any trouble accessing any information on this site or have
        any further questions relating to the data or the tool, then please contact us at: ",
        tags$b(tags$a(href="mailto:snss.isdSCT@nhs.net ", "snss.isdSCT@nhs.net ")),
        "and we will be happy to help."),
      shiny::hr(), 
      h4("Content of the explorer", style="color: black;"), # Notes
      tags$ul( 
        tags$li(tags$b("Time trend"), " - Evolution of the hospital activity during time."),
        tags$li(tags$b("Age/sex"), " - To explore the differences in age and sex composition."),
        tags$li(tags$b("Map"), " - To look at the regional differences in volume and rates of activity."),
        tags$li(tags$b("Cross-boundary flow"), " - To explore the number of patients treated outside 
                their own area."),
        tags$li(tags$b("Table"), " - To extract the raw data files behind the visualizations 
                of the whole explorer.")
        )
    ),
##############################################.             
##############Time trend tab ----   
##############################################.     
tabPanel("Time trend", icon = icon("area-chart"), style="height: 95%; 
          width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
         wellPanel(tags$style(".well {background-color:#ffffff; border: 0px solid #336699;}"),
                   column(6, uiOutput("geotype_ui_trend")),  
                   column(6, uiOutput("locname_ui_trend")),
                selectInput("measure_trend", label = "Measure", multiple=TRUE, 
                            choices = trend_measure, selectize=TRUE,
                            selected = c("All Inpatients and Daycases", "All Appointments")),
                column(6, downloadButton(outputId = 'download_trendip', 
                            label = 'Download inpatient/daycase data', width= "95%")),  #For downloading the data
                column(6, downloadButton(outputId = 'download_trendop', 
                             label = 'Download outpatient data', width= "95%"))  #For downloading the data
         ),
         mainPanel(width=12,
         plotlyOutput("trend_plot_ip"),
         HTML("<button data-toggle='collapse' href='#trendip' class='btn btn-primary'>
                  <strong>Show/hide table</strong></button>"),
         HTML("<div id='trendip' class='collapse'> "),
         DT::dataTableOutput("table_trendip", width="95%"),
         HTML("</div>"),
         shiny::hr(),
         plotlyOutput("trend_plot_op"),
         HTML("<button data-toggle='collapse' href='#trendop' class='btn btn-primary'>
                  <strong>Show/hide table</strong></button>"),
         HTML("<div id='trendop' class='collapse'> "),
         DT::dataTableOutput("table_trendop", width="95%"),
         HTML("</div>")
         )
),
##############################################.             
##############Age-sex tab ----   
##############################################.     
tabPanel("Age/sex", icon = icon("bar-chart"), style="float: top; height: 95%; 
          width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
         wellPanel(
                   column(4, uiOutput("geotype_ui_pyramid")),  
                   column(4, uiOutput("locname_ui_pyramid")),
                   column(4, selectInput("quarter_pyramid", label = "Quarter", 
                               choices = unique(data_pyramid$quarter_name), 
                               selected=tail(data_pyramid$quarter_name, n=1), width= "95%")), #to be fixed properly
                   column(9, selectInput("measure_pyramid", label = "Measure", 
                               choices = trend_measure, selectize=TRUE,
                               selected = c("All Inpatients and Daycases"))),
                   column(3, downloadButton(outputId = 'download_pyramid', 
                                            label = 'Download data', width= "95%"))  #For downloading the data
         ),
         mainPanel(width=12,
                   plotlyOutput("pyramid_plot"),
                   HTML("<button data-toggle='collapse' href='#pyramid' class='btn btn-primary'>
                        <strong>Show/hide table</strong></button>"),
                   HTML("<div id='pyramid' class='collapse'> "),
                   DT::dataTableOutput("table_pyramid", width="95%"),
                   HTML("</div>")
                   )
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
         div(style="height: 15%; width: 95%;  background-color:#ffffff; border: 0px solid #ffffff;",
             column(4,
              selectInput("datatype_map", label = "Select type of patients:", choices = data_type),
              selectInput("measure_map", label = "Select category:", 
                      choices=unique(data_mapipdc$measure), width= "95%"),
             downloadButton(outputId = 'download_map', label = 'Download data', 
                            width= "95%")  #For downloading the data
             ),
          column(4,   
            selectInput("value_map", label = "Select measure:", 
                choices=c("Admissions", "Crude rate"), width= "95%"),
            selectInput("quarter_map", label = "Select quarter:", 
                        choices = unique(data_cbfip$quarter_name), 
                        selected=tail(data_cbfip$quarter_name, n=1), width= "95%") #to be fixed properly
            ),
          column(4,
            h5(style="font-weight: bold;", "Percentile", width= "95%"),
            img(src='legend.png', width= "95%", style="vertical-align:middle; max-width:200px")
        )),
        div(style="float:left; height: 80%; width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
          leafletOutput("map"),
          HTML("<button data-toggle='collapse' data-target='#map_collaps' class='btn btn-primary'>
                  <strong>Show/hide table</strong></button>"),
          HTML("<div id='map_collaps' class='collapse'> "),
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
           NHS Board area. However, patients are treated outside of their residence area for varied
           and valid reasons. These include where specific services are available, where the 
           emergency occurred, or the ‘catchment’ area of a particular hospital, being the closest
           to the patient."),
         p("In addition, all NHS boards refer some patients to the Golden Jubilee National Hospital (GJNH)
           in Clydebank. The GJNH provides a range of national and regional services. It is 
           also a national resource to help meet the demand for planned procedures from across Scotland." 
         ),
         div(style="float: top; height: 95%; width: 95%; background-color:#ffffff; 
             border: 0px solid #ffffff;",
             column(3,
                    selectInput("datatype_flow", label = "Type of patient:", 
                                choices = data_type)
             ),
             column(3,  
                    selectInput("quarter_flow", label = "Quarter:", 
                    choices = unique(data_cbfip$quarter_name), 
                                selected=tail(data_cbfip$quarter_name, n=1)) #to be fixed properly
             ),
             column(3,  
                    selectInput("hbres_flow", label = "Board of residence", 
                                choices = unique(data_cbfip$hbres_name))
             ),
             column(3,
                    downloadButton(outputId = 'download_flow', label = 'Download data',
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
             HTML("<button data-toggle='collapse' data-target='#crossb' 
                  class='btn btn-primary' style=padding: 6px 12px;>
                  <strong>Show/hide table</strong></button>"),
             HTML("<div id='crossb' class='collapse'> "),
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
         column(8,
          selectInput("filename_table", label = "Data set:", 
                     choices = file_types, width="95%")
          ),
         column(4,
           downloadButton(outputId = 'download_table', label = 'Download data',
                        style="margin: 25px 10px 25px 10px ")
         ),  #For downloading the data
        DT::dataTableOutput("table_explorer", width="95%")
   )
  )
)


