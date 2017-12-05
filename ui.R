#Syntax to create the data explorer
#Jaime Villacampa October 17

#TODO:
#see server syntax

############################.
### Visual interface ----
############################.

# Define UI for application that draws a histogram
fluidPage(style="width: 100%; height: 100%; max-width: 1200px; ", 
          tags$head( 
            tags$style( #to avoid error messages
              type="text/css", ".shiny-output-error { visibility: hidden; }", 
              ".shiny-output-error:before { visibility: hidden; }"
              ) 
            ),

   navbarPage("", # Navigation bar
              
##############################################.             
##############Introduction tab ----   
##############################################.     
    tabPanel("Introduction", icon = icon("medkit"), style="float: top; height: 95%; 
              width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
      column(2,
             h3("Data explorer")),
      column(10,
             p("The data explorer allows you to find the exact piece of data you want. 
               The five sections allow you to see the data in different ways. Within 
               each there are filters that let you select the slice of data you are interested in"),
      tags$ul( 
        tags$li(tags$b("Time trend"), " - shows the data across time and allows you to see shifts in practice."),
        tags$li(tags$b("Age/sex"), " - shows the differences in age and sex composition for this data."),
        tags$li(tags$b("Deprivation"), " - shows the differences in activity across the deprivation gradient."),
        tags$li(tags$b("Cross-boundary flow"), " - shows movements between where patients live and where they are treated."),
        tags$li(tags$b("Table"), " - allows you to select a slice of data in a table format.")
        ),
      p("For each section you can download the data or view it as a table if you prefer. "),
      p("A disclosure control method has been applied so the figures might not be additive."),
      p("If you have any trouble using this tool or you have further questions relating to the data, please contact us at: ",
        tags$b(tags$a(href="mailto:snss.isdSCT@nhs.net ", "snss.isdSCT@nhs.net ")),
        "and we will be happy to help.")
      )
    ), 
##############################################.             
##############Time trend tab ----   
##############################################.     
tabPanel("Time trend", icon = icon("area-chart"), style="height: 95%; 
          width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
          h3("Time trend"),
         p("The time trend section allows you to see changes over time. You can use 
           the filters to select the slice of data you are interested in. In the type of 
           activity box you can select the measures you want to compare. You can see a 
           table of the data selected using the ‘show/hide table’ button. You can also
           download the data using the download buttons. If you hover over the chart you 
           will see a number of buttons, to the top right of the chart, these will allow 
           you to select parts of the chart, zoom in or out, save as an image and other functions."),
         wellPanel(tags$style(".well {background-color:#ffffff; border: 0px solid #336699;}"),
                   column(6, uiOutput("geotype_ui_trend")),  
                   column(6, uiOutput("locname_ui_trend")),
                   column(6, 
                selectInput("service_trend", label = "Select type of activity", multiple=TRUE, 
                            choices = trend_service, selectize=TRUE,
                            selected = c("All Inpatients and Daycases"))),
                column(6, selectInput("measure_trend", label = "Select measure", 
                                      choices = trend_measure, selected = "Number")),
                column(6, downloadButton(outputId = 'download_trend', 
                            label = 'Download data', width= "95%"))  #For downloading the data
         ),
         mainPanel(width=12,
         plotlyOutput("trend_plot"),
         HTML("<button data-toggle='collapse' href='#trend' class='btn btn-primary'>
                  <strong>Show/hide table</strong></button>"),
         HTML("<div id='trend' class='collapse'> "),
         DT::dataTableOutput("table_trend", width="95%"),
         HTML("</div>")
         )
),
##############################################.             
##############Age-sex tab ----   
##############################################.     
tabPanel("Age/sex", icon = icon("bar-chart"), style="float: top; height: 95%; 
          width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
         h3("Age/sex"),
         p("The age/sex section allows you to see the differences in numbers for 
           different age groups or between each sex. You can use the filters to select 
           the slice of data you are interested in. In the type of activity box you can 
           select the measures you want to compare. You can see a table of the data 
           selected using the ‘show/hide table’ button. You can also download the data 
           using the download buttons. If you hover over the chart you will see a number 
           of buttons, to the top right of the chart, these will allow you to select parts 
           of the chart, zoom in or out, save as an image and other functions."),
         wellPanel(
                   column(4, uiOutput("geotype_ui_pyramid")),  
                   column(4, uiOutput("locname_ui_pyramid")),
                   column(4, selectInput("quarter_pyramid", label = "Select the time period", 
                               choices = unique(data_pyramid$quarter_name), 
                               selected=tail(data_pyramid$quarter_name, n=1), width= "95%")), #to be fixed properly
                   column(9, selectInput("measure_pyramid", label = "Select the type of activity", 
                               choices = trend_service, selectize=TRUE,
                               selected = c("All Inpatients and Daycases"))),
                   column(3, br(), downloadButton(outputId = 'download_pyramid', 
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
##############Deprivation (SIMD) tab ----   
##############################################.     
tabPanel("Deprivation", icon = icon("male"), style="float: top; height: 95%; 
         width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
         h3("Deprivation"),
         p("The deprivation section lets you see the difference in activity by the 
           five SIMD quintiles. You can use the filters to select the slice of data 
           you are interested in. In the type of activity box you can select the measures 
           you want to compare. You can see a table of the data selected using the 
           ‘show/hide table’ button. You can also download the data using the download 
           buttons. If you hover over the chart you will see a number of buttons, to the 
           top right of the chart, these will allow you to select parts of the chart, 
           zoom in or out, save as an image and other functions."),
         wellPanel(
           column(4, uiOutput("geotype_ui_simd")),  
           column(4, uiOutput("locname_ui_simd")),
           column(4, selectInput("quarter_simd", label = "Select the time period", 
                                 choices = unique(data_simd$quarter_name), 
                                 selected=tail(data_simd$quarter_name, n=1), width= "95%")), #to be fixed properly
           column(9, selectInput("measure_simd", label = "Select the type of activity", 
                                 choices = trend_service, selectize=TRUE,
                                 selected = c("All Inpatients and Daycases"))),
           column(3, downloadButton(outputId = 'download_simd', 
                                    label = 'Download data', width= "95%"))  #For downloading the data
         ),
         mainPanel(width=12,
                   plotlyOutput("simd_plot"),
                   HTML("<button data-toggle='collapse' href='#simd' class='btn btn-primary'>
                        <strong>Show/hide table</strong></button>"),
                   HTML("<div id='simd' class='collapse'> "),
                   DT::dataTableOutput("table_simd", width="95%"),
                   HTML("</div>")
                   )
),
##############################################.             
##############Map tab ----   
##############################################.     
# tabPanel("Map", icon = icon("globe"), style="float: top; height: 95%; width: 95%; 
#           background-color:#ffffff; border: 0px solid #ffffff;",
#          h4(textOutput("title_map")),
#          p("There are marked differences among Scottish regions in the numbers of patients 
#               admitted to hospital or treated as day cases. This map allows to explore this regional
#               differences in total and relative volume of hospital acute care activity in Scotland."),
#          div(style="height: 15%; width: 95%;  background-color:#ffffff; border: 0px solid #ffffff;",
#              column(4,
#               selectInput("datatype_map", label = "Select type of patients:", choices = data_type),
#               selectInput("measure_map", label = "Select category:", 
#                       choices=unique(data_mapipdc$measure), width= "95%"),
#              downloadButton(outputId = 'download_map', label = 'Download data', 
#                             width= "95%")  #For downloading the data
#              ),
#           column(4,   
#             selectInput("value_map", label = "Select measure:", 
#                 choices=c("Admissions", "Crude rate"), width= "95%"),
#             selectInput("quarter_map", label = "Select quarter:", 
#                         choices = unique(data_cbfip$quarter_name), 
#                         selected=tail(data_cbfip$quarter_name, n=1), width= "95%") #to be fixed properly
#             ),
#           column(4,
#             h5(style="font-weight: bold;", "Percentile", width= "95%"),
#             img(src='legend.png', width= "95%", style="vertical-align:middle; max-width:200px")
#         )),
#         div(style="float:left; height: 80%; width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
#           leafletOutput("map"),
#           HTML("<button data-toggle='collapse' data-target='#map_collaps' class='btn btn-primary'>
#                   <strong>Show/hide table</strong></button>"),
#           HTML("<div id='map_collaps' class='collapse'> "),
#           DT::dataTableOutput("table_map", width="95%"),
#           HTML("</div>"),
#           p("To embed this graphic in your website include this code:"),
#           p("<iframe src='https://scotland.shinyapps.io/hospcare_ipdcmap/' style='border: none; 
#               width: 100%; height: 100%;'></iframe>")
#         )
# ),
##############################################.             
##############Cross boundary tab ----   
##############################################.     
tabPanel("Cross-boundary", icon = icon("exchange"), style="float: top; height: 95%; 
          width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
         h3("Cross-boundary flow"),
         p("The cross-boundary section allows you to see where patients are treated.  
           The majority are treated in their local board. There are a number of reasons 
           why they would be treated in another board. These include where specific services 
           are not available locally this includes nationally provided services (such as 
           the services provided by the Golden Jubilee Hospital) or where an emergency has 
           occurred or when the ‘catchment’ area of a particular hospital is closer than others. 
           You can use the filters to select the slice of data you are interested in. 
           In the type of activity box you can select the hospital services you want to explore.  
           You can see a table of the data selected using the ‘show/hide table’ button. 
           You can also download the data using the download buttons. "),
         wellPanel(
           column(4,
                    selectInput("datatype_flow", label = "Select the hospital service", 
                                choices = data_type),
                  checkboxInput("checkbox_flow", label = "Include flows within same board?", value = TRUE)
             ),
             column(4,  
                    selectInput("hb_flow", label = "Select the board of interest", 
                                choices = unique(data_cbfip$hbres_name)),
                    downloadButton(outputId = 'download_flow', label = 'Download data')  #For downloading the data
             ),
             column(4,
                    selectInput("quarter_flow", label = "Select the time period", 
                                choices = unique(data_cbfip$quarter_name), 
                                selected=tail(data_cbfip$quarter_name, n=1)) #to be fixed properly
             )
         ),
         mainPanel(width=12,
                   htmlOutput("sankey_all", width="96%"),
                   br(),
             column(6,  
                    htmlOutput("crossb_restext"),
                    htmlOutput("sankey_res", width="48%")
             ),
             column(6,
                    htmlOutput("crossb_treattext"),
                    htmlOutput("sankey_treat", width="48%")
             ),
         div(style="width:95%; height:5%",
             HTML("<button data-toggle='collapse' data-target='#crossb' 
                  class='btn btn-primary' style=padding: 6px 12px;>
                  <strong>Show/hide table</strong></button>"),
             HTML("<div id='crossb' class='collapse'> "),
             DT::dataTableOutput("table_crossb", width="95%"),
             HTML("</div>")
             )
         )
),
##############################################.             
##############Table tab ----   
##############################################.     
tabPanel("Table", icon = icon("table"), style="float: top; height: 95%; width: 95%; 
          background-color:#ffffff; border: 0px solid #ffffff;",
         h3("Table"),
         p("The table section allows you to see all the data for a particular data set. 
           You can use the filters to select the slice of data you are interested in. 
           You can also download the data using the download buttons"),
         column(8,
          selectInput("filename_table", label = "Select the data file", 
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


