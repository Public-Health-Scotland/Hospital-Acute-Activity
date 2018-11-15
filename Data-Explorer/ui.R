################################################################
### UI
### Data Explorer script 4 of 5
###
### Original Author: Jaime Villacampa
### Original Date: October 2017
### Last edited by: Jack Hannah
### Last edited on: 15 November 2018
###
### Written to be run on RStudio Desktop
###
### This script creates the user interface of the
### data explorer
###
###
### NOTE - the hyperlinks to the data quality page in the first and last tabs
### need to be updated every quarter




### Visual interface ----


fluidPage(style = "width: 100%; height: 100%; max-width: 1200px;", tags$head(
  
  # Prevent red text error messages from appearing
  # throughout the app
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }", 
    ".shiny-output-error:before { visibility: hidden; }"
    ) 
  ),
  
  # The following chunk of code does three things: 
  # 1. Paints the ribbon that contains the tab headers white
  # 2. Highlights the header of the active tab in blue
  # 3. Sets the font size for the sentence that appears above the 
  #    cross-boundary flow diagram.
  
  tags$style(HTML(".tabbable > .nav > li > a {  
                          color: #000000;  
                          } 
                          
                          .tabbable > .nav > li[class = active] > a { 
                          background-color: #0072B2; 
                          color: #FFFFFF; 
                          } 
                          
                          #flow_text { 
                          font-size: 15px; 
                          }")),
  
  # We are going to divide our UI into discrete sections,
  # called tab panels 
  # To do this, we need the layout "tabsetPanel()" 
  
  tabsetPanel(
    id = "Panels", 
    
    
    ### Tab 1: Introduction ----   
    
    tabPanel("Introduction", icon = icon("info-circle"),
           style = "float: top; height: 95%; 
                     width: 95%; background-color: #ffffff;
                     border: 0px solid #ffffff;",
           column(2,
                  h3("Data explorer")),
           column(10,
                  tags$br(),
                  p("The data explorer allows you to find the data you want",
                    "and visualise it in different ways. Within each of the",
                    "following six sections there are filters that let you",
                    "select the data you are interested in:"),
                  tags$ul( 
                    tags$li(
                      tags$b(
                        actionLink(
                          "tabpanel_trend_location",
                          "Time trend (location comparison)")),
                      icon("line-chart"),
                      " - shows the data over time."),
                    
                    tags$li(
                      tags$b(
                        actionLink(
                          "tabpanel_trend_activity",
                          "Time trend (activity comparison)")),
                      icon("line-chart"),
                      " - shows the data over time."),
                    
                    tags$li(
                      tags$b(
                        actionLink(
                          "tabpanel_age_sex",
                          "Age/sex")),
                      icon("bar-chart"),
                      " - shows the age and sex distribution of", 
                      "the data."),
                    
                    tags$li(
                      tags$b(
                        actionLink(
                          "tabpanel_deprivation",
                          "Deprivation")),
                      icon("user-circle-o"),
                      " - shows activity across different levels",
                      "of deprivation."),
                    
                    tags$li(
                      tags$b(
                        actionLink(
                          "tabpanel_cbf",
                          "Cross-boundary flow")),
                      icon("exchange"),
                      " - shows the relationship between where", 
                      "patients live and where they are treated."),
                    
                    tags$li(
                      tags$b(
                        actionLink(
                          "tabpanel_table",
                          "Table")), 
                      icon("table"), 
                      "- allows you to view the", 
                      "data in a table.")
                  ),
                  
                  p("When using the data explorer please take the", 
                    "following factors into consideration:"),
                  tags$ul(
                    tags$li("There are issues with the quality of the data", 
                            "presented. Known issues are summarised in the", 
                            tags$a(
                              href = paste0("https://www.isdscotland.org/",
                                            "Health-Topics/Hospital-Care/",
                                            "Publications/2018-12-18/",
                                            "Acute-Hospital-Publication/",
                                            "data-quality/"),
                              "data quality"), " section."),
                    tags$li("The data for the most recent quarter are", 
                            "provisional. Provisional data are subject to", 
                            "change in future publications as submissions", 
                            "may be updated to reflect a more accurate and", 
                            "complete set of data from NHS Boards."),
                    tags$li("Missing data points within graphs / missing",
                            "rows of data within the tables means no data", 
                            "/ activity."),
                    tags$li("Disclosure control methods have been applied", 
                            "to the data in order to protect patient", 
                            "confidentiality therefore figures may not", 
                            "be additive.")),
                  p("If you have any trouble using this tool or you have",
                    "further questions relating to the data, please contact", 
                    "us at: ",
                    tags$b(tags$a(
                      href = "mailto:NSS.isdQualityIndicators@nhs.net",
                      "NSS.isdQualityIndicators@nhs.net")),
                    " and we will be happy to help.")
                  )
           ),
  
  
  
  ### Tab 2: Time trend for multiple location ----   
  
  
  tabPanel("Time trend (location comparison)",
           icon = icon("line-chart"),
           style = "height: 95%; width: 95%; background-color: #ffffff; 
                    border: 0px solid #ffffff;",
           h3("Time trend"),
           p("This section allows you to see changes over time. You can use", 
             "the filters to select the data you are interested in. You can", 
             "visualise multiple locations at the same time. To view the data", 
             "in a table use the ‘show/hide table’ button. To download your", 
             "data selection as a csv file use the ‘download data’ button."),
           tags$ul(tags$li(tags$b("Download plot as a png"),  
                           icon("camera"),  
                           " - click this button to save the graph as an", 
                           "image (please note that Internet Explorer does", 
                           "not support this function)."),
                   tags$li(tags$b("Zoom"),  
                           icon("search"),  
                           " - zoom into the graph by clicking this button", 
                           "and then clicking and dragging your mouse over", 
                           "the area of the graph you are interested in."),   
                   tags$li(tags$b("Pan"),  
                           icon("move", lib = "glyphicon"),  
                           " - adjust the axes of the graph by clicking this", 
                           "button and then clicking and moving your mouse", 
                           "in any direction you want."),   
                   tags$li(tags$b("Reset axes"),  
                           icon("home"),  
                           " - click this button to return the axes to their",   
                           "default range.") 
           ),
           wellPanel(tags$style(".well {background-color: #ffffff;
                                        border: 0px solid #336699;}"),
                     column(6, uiOutput("geotype_ui_trend")),  
                     column(6, uiOutput("locname_ui_trend")),
                     column(6, pickerInput(
                       "service_trend",
                       label = "Select type of activity",
                       choices = trend_service,
                       selected = c("All inpatients and daycases"))),
                     column(6, pickerInput(
                       "measure_trend",
                       label = "Select measure", 
                       choices = trend_measure,
                       selected = "Number")),
                     column(6, downloadButton(outputId = 'download_trend',
                                              label = 'Download data',
                                              width = "95%"))  
           ),
           mainPanel(width = 12,
                     plotlyOutput("trend_plot"),
                     
                     # Button to show / hide div where data table is
                     HTML("<button data-toggle = 'collapse' href = '#trend' 
                            class='btn btn-primary'>
                            <strong>Show/hide table</strong></button>"),
                     HTML("<div id = 'trend' class = 'collapse'> "),
                     dataTableOutput("table_trend",
                                     width = "95%"),
                     HTML("</div>")
           )
  ),
  
  ### Tab 3: Time trend for multiple activity ----   
  
  
  tabPanel("Time trend (activity comparison)",
           icon = icon("line-chart"),
           style = "height: 95%; width: 95%; background-color: #ffffff;
                    border: 0px solid #ffffff;",
           h3("Time trend"),
           p("This section allows you to see changes over time. You can", 
             "use the filters to select the data you are interested in.", 
             "You can visualise multiple activities at the same time.", 
             "To view the data in a table use the ‘show/hide table’ button.", 
             "To download your data selection as a csv file use the", 
             "‘download data’ button."),
           tags$ul( 
             tags$li(tags$b("Download plot as a png"),  
                     icon("camera"),  
                     " - click this button to save the graph as an image", 
                     "(please note that Internet Explorer does not support", 
                     "this function)."), 
             tags$li(tags$b("Zoom"),  
                     icon("search"),  
                     " - zoom into the graph by clicking this button and then", 
                     "clicking and dragging your mouse over the area of the",
                     "graph you are interested in."),   
             tags$li(tags$b("Pan"),  
                     icon("move",
                          lib = "glyphicon"),  
                     " - adjust the axes of the graph by clicking this button",  
                     "and then clicking and moving your mouse in any", 
                     "direction you want."),   
             tags$li(tags$b("Reset axes"),  
                     icon("home"),  
                     " - click this button to return the axes to their", 
                     "default range.") 
           ),
           wellPanel(tags$style(".well {background-color: #ffffff;
                                        border: 0px solid #336699;}"),
                     column(6, uiOutput("geotype_ui_trend_2")),  
                     column(6, uiOutput("locname_ui_trend_2")),
                     column(6, pickerInput(
                       "service_trend_2",
                       label = paste0("Select type of activity", 
                                      "(multiple selections allowed)"),
                       choices = trend_service,
                       multiple = TRUE,
                       options = list(
                         `selected-text-format` = "count > 1"
                       ),
                       selected = c("All inpatients and daycases"))),
                     column(6, pickerInput("measure_trend_2",
                                           label = "Select measure", 
                                           choices = trend_measure,
                                           selected = "Number")),
                     column(6,
                            downloadButton(outputId = 'download_trend_2',
                                           label = 'Download data',
                                           width = "95%"))  
           ),
           mainPanel(width = 12,
                     plotlyOutput("trend_plot_2"),
                     
                     # Button to show / hide div where data table is
                     HTML("<button data-toggle = 'collapse' href = '#trend2' 
                            class='btn btn-primary'>
                            <strong>Show/hide table</strong></button>"),
                     HTML("<div id = 'trend2' class = 'collapse'> "),
                     dataTableOutput("table_trend_2",
                                     width = "95%"),
                     HTML("</div>")
           )
  ),
  
  
  ### Tab 4: Population pyramid ----
  
  
  tabPanel("Age/sex",
           icon = icon("bar-chart"),
           style="float: top; height: 95%; 
                     width: 95%; background-color:#ffffff; border: 0px solid #ffffff;",
           h3("Age/sex"),
           p("This section allows you to explore the age and 
                       sex distribution of the data. You can use the 
                       filters to select the data you are interested 
                       in. To view the data in a table use the ‘show/hide 
                       table’ button. To download your data selection 
                       as a csv file use the ‘download data’ button. 
                       "),
           tags$ul( 
             tags$li(tags$b("Download plot as a png"),  
                     icon("camera"),  
                     " - click this button to save the graph as an image  
                               (please note that Internet Explorer does not support this  
                               function)."), 
             tags$li(tags$b("Zoom"),  
                     icon("search"),  
                     " - zoom into the graph by clicking this button and then  
                               clicking and dragging your mouse over the area of the  
                               graph you are interested in."),   
             tags$li(tags$b("Pan"),  
                     icon("move", lib = "glyphicon"),  
                     " - adjust the axes of the graph by clicking this button  
                               and then clicking and moving your mouse in any direction 
                               you want."),   
             tags$li(tags$b("Reset axes"),  
                     icon("home"),  
                     " - click this button to return the axes to their  
                               default range.") 
           ),
           wellPanel(
             column(4, uiOutput("geotype_ui_pyramid")),  
             column(4, uiOutput("locname_ui_pyramid")),
             column(4,
                    selectInput("quarter_pyramid",
                                label = "Select the time period",
                                choices = data_pyramid %>%
                                  distinct(quarter_name) %>%
                                  pull(quarter_name),
                                selected = latest_quarter,
                                width = "95%")), 
             column(9,
                    selectInput("measure_pyramid",
                                label = "Select the type of activity",
                                choices = pyramid_service,
                                selectize=TRUE,
                                selected = c("All inpatients 
                                                       and daycases"))),
             column(3,
                    br(),
                    
                    # For downloading the data
                    downloadButton(outputId = 'download_pyramid',
                                   label = 'Download data',
                                   width= "95%"))
           ),
           mainPanel(width = 12,
                     plotlyOutput("pyramid_plot"),
                     
                     # Button to show hide div where data table is
                     HTML("<button data-toggle='collapse' href='#pyramid' 
                                    class='btn btn-primary'>
                                    <strong>Show/hide table</strong></button>"),
                     HTML("<div id='pyramid' class='collapse'> "),
                     dataTableOutput("table_pyramid",
                                     width = "95%"),
                     HTML("</div>")
           )
  ),
  
  
  
  ### Tab 5: Deprivation (SIMD) ----   
  
  
  tabPanel("Deprivation",
           icon = icon("user-circle-o"),
           style = "float: top; height: 95%; width: 95%;
                     background-color:#ffffff; border: 0px solid #ffffff;",
           h3("Deprivation"),
           p("This section allows you to explore the data by 
                       different levels of",
             tags$a(href="http://www.gov.scot/Topics/Statistics/SIMD", 
                    "deprivation"),
             ". You can use the filters to select the data you are 
                       interested in. To view the data in a table use the 
                       ‘show/hide table’ button. To download your data selection 
                       as a csv file use the ‘download data’ button.
                       "),
           tags$ul( 
             tags$li(tags$b("Download plot as a png"),  
                     icon("camera"),  
                     " - click this button to save the graph as an image  
                               (please note that Internet Explorer does not support this  
                               function)."), 
             tags$li(tags$b("Zoom"),  
                     icon("search"),  
                     " - zoom into the graph by clicking this button and then  
                               clicking and dragging your mouse over the area of the  
                               graph you are interested in."),   
             tags$li(tags$b("Pan"),  
                     icon("move", lib = "glyphicon"),  
                     " - adjust the axes of the graph by clicking this button  
                               and then clicking and moving your mouse in any direction 
                               you want."),   
             tags$li(tags$b("Reset axes"),  
                     icon("home"),  
                     " - click this button to return the axes to their  
                               default range.") 
           ),
           wellPanel(
             column(4, uiOutput("geotype_ui_simd")),  
             column(4, uiOutput("locname_ui_simd")),
             column(4,
                    selectInput("quarter_simd",
                                label = "Select the time period",
                                choices = data_simd %>%
                                  distinct(quarter_name) %>%
                                  pull(quarter_name),
                                selected = latest_quarter,
                                width = "95%")), 
             column(9,
                    selectInput("measure_simd",
                                label = "Select the type of activity",
                                choices = pyramid_service,
                                selectize = TRUE,
                                selected = c("All inpatients
                                                       and daycases"))),
             
             # For downloading the data
             column(3,
                    downloadButton(outputId = 'download_simd', 
                                   label = 'Download data',
                                   width = "95%"))),
           
           
           mainPanel(width = 12,
                     plotlyOutput("simd_plot"),
                     
                     # Button to show/hide div where data table is
                     HTML("<button data-toggle='collapse' href='#simd' 
                                    class='btn btn-primary'>
                                    <strong>Show/hide table</strong></button>"),
                     HTML("<div id='simd' class='collapse'> "),
                     dataTableOutput("table_simd",
                                     width = "95%"),
                     HTML("</div>")
           )
  ),
  
  
  
  ### Tab 6: Map ----   
  ### SECTION NOT IN USE AT THE MOMENT, STILL REQUIRES WORK AND RATE DATA  
  # 
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
  
  
  
  ### Tab 7: Cross boundary ----   
  
  tabPanel("Cross-boundary",
           icon = icon("exchange"),
           style = "float: top; height: 95%; width: 95%;
                     background-color:#ffffff; border: 0px solid #ffffff;",
           h3("Cross-boundary flow"),
           p("This section allows you to see where patients are treated.
                       The top chart shows where patients living in each NHS 
                       Board are treated. The bottom charts show data specific to 
                       the NHS Board selected in the ‘Board of interest’ filter. 
                       The left chart shows where patients living in the NHS Board 
                       of interest are treated. The right chart shows where 
                       patients treated in the NHS Board of interest come from. 
                       To include patients treated in their own NHS Board in the 
                       charts, use the ‘flows within own Health Board’ tick box."),
           p("You can use the filters to select the data you are interested 
                       in. To view the data in a table use the ‘show/hide table’ 
                       button. To download your data selection as a csv use the 
                       ‘download data’ button."),
           wellPanel(
             column(4,
                    selectInput("datatype_flow",
                                label = "Select the hospital service", 
                                choices = data_type),
                    checkboxInput("checkbox_flow",
                                  label = "Include flows within same board?",
                                  value = FALSE)),
             column(4,  
                    selectInput("hb_flow",
                                label = "Select the board of interest", 
                                choices = data_cbf %>%
                                  distinct(hbres_name) %>%
                                  pull(hbres_name)),
                    downloadButton(outputId = 'download_flow',
                                   label = 'Download data') 
             ),
             column(4,
                    selectInput("quarter_flow", 
                                label = "Select the time period", 
                                choices = data_cbf %>%
                                  arrange(dmy(quarter_date)) %>%
                                  distinct(quarter_name) %>%
                                  pull(quarter_name),
                                selected = latest_quarter) 
             )
           ),
           mainPanel(width = 12,
                     htmlOutput("sankey_all", 
                                width = "96%"),
                     br(),
                     column(6,  
                            htmlOutput("crossb_restext"),
                            htmlOutput("sankey_res", 
                                       width = "48%")
                     ),
                     column(6,
                            htmlOutput("crossb_treattext"),
                            htmlOutput("sankey_treat", 
                                       width = "48%")
                     ),
                     div(style = "width:95%; height:5%",
                         
                         # Button to show/hide div where data table is
                         HTML("<button data-toggle='collapse' data-target='#crossb' 
                                        class='btn btn-primary' style=padding: 6px 12px;>
                                        <strong>Show/hide table</strong></button>"),
                         HTML("<div id='crossb' class='collapse'> "),
                         dataTableOutput("table_crossb",
                                         width = "95%"),
                         HTML("</div>")
                     )
           )
  ),
  
  
  
  ### Tab 8: Table ----   
  
  tabPanel("Table",
           icon = icon("table"),
           style="float: top; height: 95%; width: 95%; 
                     background-color:#ffffff; border: 0px solid #ffffff;",
           h3("Table"),
           p("This section allows you to view the data in table format.
                       You can use the filters to select the  data you are
                       interested in. You can also download the data as a csv 
                       using the download buttons."),
           column(8,
                  selectInput("filename_table",
                              label = "Select the data file",
                              choices = file_types,
                              width = "95%")
           ),
           column(4,
                  downloadButton(outputId = 'download_table',
                                 label = 'Download data',
                                 style = "margin: 25px 10px 25px 10px ")
           ),  
           dataTableOutput("table_explorer", width = "95%"),
           
           # Insert footnote for Tayside beds data
           conditionalPanel(
             condition = "input.filename_table == 'Beds'",
             p("Beds data for NHS Tayside from May/June 2016 onwards 
                         are inaccurate due to a system issue with their patient 
                         management system. Please bear this in mind when looking 
                         at the data. Further detail about this can be found in the ",
               tags$a(href = paste0("https://www.isdscotland.org/",
                                    "Health-Topics/Hospital-Care/",
                                    "Publications/2018-12-18/",
                                    "Acute-Hospital-Publication/",
                                    "data-quality/"),
                      "data quality"),
               "section.")
           )
  )
)
)



### END OF SCRIPT ###