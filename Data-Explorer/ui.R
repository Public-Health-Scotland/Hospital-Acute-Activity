
### UI
### Data Explorer script 4 of 5
###
### Original Author: Jaime Villacampa
### Original Date: October 2017


#source("Data-Explorer/ui_code.R")

### Visual interface ----
#if (credentials$authorisation=="Private"){
###Update each quarter
publication_date<-"23-february-2021"

#secure_app(
  tagList( #needed for shinyjs
  navbarPage(id = "Panels", # id used for jumping between tabs
             title = div(style = "position: relative; top: -5px;"),  
             #title for browser tab
             windowTitle = "Acute Hospital Activity Data Explorer",
             # When the app is run locally  ie. the 
             # "Run app" button is pressed "Data-Explorer/" may need
             # to be added to the "www/styles.css" filepath for the 
             # app to run correctly.
             # However, it MUST be removed again for the deployment
             # to work.
             header = tags$head(includeCSS("www/styles.css"), # CSS styles
                                #Icon for browser tab
                                tags$link(rel="shortcut icon", 
                                          href="favicon_phs.ico")), 
             
             # We are going to divide our UI into discrete sections,
             # called tab panels 
             # To do this, we need the layout "tabsetPanel()" 
             
             ### Tab 1: Introduction ----   
             
             tabPanel("Introduction", 
                      icon = icon("info-circle"),
                      h2("Data explorer"),
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
                                 " - shows the data over time by multiple locations."),
                               tags$li(
                                 tags$b(
                                   actionLink(
                                     "tabpanel_trend_activity",
                                     "Time trend (activity comparison)")),
                                 icon("line-chart"),
                                 " - shows the data over time by multiple activity types."),
                               
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
                             
                             tags$br(),
                             p("When using the data explorer please take the", 
                               "following factors into consideration:"),
                             tags$ul(
                               tags$li("Please note this release includes Scotland going into emergency measures", 
                                       "due to COVID-19, which will impact on the volume", 
                                       "of hospital activity and trends observed."),
                               tags$li("NHS Louisa Jordan hospital has been included within this publication. ",
                                       "Outpatient activity across a few specialties has been undertaken since July 2020."),
                               tags$li("This release does not include specific ",
                                       "COVID-19 information. For PHS Coronavirus ",
                                       "information, visit our ", 
                                       tags$a(
                                         href = paste0("https://www.publichealthscotland.scot/",
                                                       "our-areas-of-work/covid-19/covid-19-data-and-intelligence/"),
                                         "COVID-19 webpages", target = "_blank",
                                         class = "special-link"), ".",
                                       "Coronavirus in Scotland information can also ",
                                       "be found on the ",
                                       tags$a(
                                         href = paste0("https://www.gov.scot/coronavirus-covid-19/"),
                                         "Scottish Government website", target = "_blank",
                                         class = "special-link"), "."),
                               tags$li("There are issues with the quality of the data", 
                                       "presented. Known issues are summarised in the", 
                                       tags$a(
                                         href = paste0("https://beta.isdscotland.org/",
                                                       "find-publications-and-data/",
                                                       "health-services/hospital-care/",
                                                       "acute-hospital-activity-and-nhs-beds-information-quarterly/",
                                                       publication_date, "/data-quality/"),
                                         "Data Quality", target = "_blank",
                                         class = "special-link"), " and ",
                                       tags$a(
                                         href = paste0("https://beta.isdscotland.org/",
                                                       "find-publications-and-data/",
                                                       "health-services/hospital-care/",
                                                       "acute-hospital-activity-and-nhs-beds-information-quarterly/",
                                                       publication_date,"/trend-data/"),
                                         "Data Trends", target = "_blank", 
                                         class = "special-link"), "sections."),
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
                                 href = "mailto:phs.qualityindicators@phs.scot",
                                 "phs.qualityindicators@phs.scot")),
                               " and we will be happy to help.")
                      )
             ),
             
             
             
             ### Tab 2: Time trend for multiple location ----   
             
             
             tabPanel("Time trend (location comparison)",
                      icon = icon("line-chart"),
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
                                           border: 0px solid #0078D4;}"),
                                column(6, uiOutput("geotype_ui_trend")),  
                                column(6, uiOutput("locname_ui_trend")),
                                column(6, uiOutput("service_ui_trend")),
                                column(6, uiOutput("measure_ui_trend")),
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
                                           border: 0px solid #0078D4;}"),
                                column(6, uiOutput("geotype_ui_trend_2")),  
                                column(6, uiOutput("locname_ui_trend_2")),
                                column(6, uiOutput("service_ui_trend_2")),
                                column(6, uiOutput("measure_ui_trend_2")),
                                column(6, downloadButton(outputId = 'download_trend_2',
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
                        column(4, uiOutput("quarter_ui_pyramid")), 
                        column(9, uiOutput("measure_ui_pyramid")), 
                        column(3, 
                               br(),
                               
                               # For downloading the data
                               downloadButton(outputId = 'download_pyramid',
                                              label = 'Download data',
                                              width = "95%"))
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
                      h3("Deprivation"),
                      p("This section allows you to explore the data by 
                        different levels of",
                        tags$a(href=paste0("https://www.gov.scot/collections/",
                                           "scottish-index-of-multiple-deprivation-2020/"), 
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
                                           selected = "All inpatients and daycases")),
                        
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
                                           choices = data_type)),
                        column(4,  
                               selectInput("hb_flow",
                                           label = "Select the board of interest", 
                                           choices = data_cbf %>%
                                             arrange(hbres_name) %>%
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
                        ),
                        checkboxInput("checkbox_flow",
                                      label = "Include flows within same board?",
                                      value = FALSE)
                      ),
                      mainPanel(width = 12,
                                htmlOutput("sankey_all", 
                                           width = "96%"),
                                br(),
                                fluidRow(
                                  column(6,
                                         htmlOutput("crossb_restext")
                                  ),
                                  column(6,
                                         htmlOutput("crossb_treattext")
                                  )),
                                fluidRow(
                                  column(6, htmlOutput("sankey_res", 
                                                       width = "48%")),
                                  column(6, htmlOutput("sankey_treat", 
                                                       width = "48%"))),
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
                      p("This section allows you to view the data in table format.
                        You can use the filters to select the  data you are
                        interested in. You can also download the data as a csv 
                        using the download buttons."),
             p("Specialty breakdowns are measured in episodes and spells rather 
               than stays. For more information, please see the ",
               tags$a(
                 href = paste0("https://beta.isdscotland.org/",
                               "find-publications-and-data/health-services/",
                               "hospital-care/",
                               "acute-hospital-activity-and-nhs-beds-information-quarterly",
                               publication_date,
                               "methods-used-to-produce-this-data/"),
                         "methodology section", target = "_blank",
                         class = "special-link"), "."),
             br(),
             column(8,
                    selectInput("filename_table",
                                label = "Select the data file",
                                choices = file_types,
                                width = "95%")
             ),
             column(4,
                    downloadButton(outputId = 'download_table',
                                   label = 'Download data')
             ),  
             dataTableOutput("table_explorer", width = "95%")
             
             )
               )
    )
#)
  
#} else if (credentials$authorisation=="Public"){
  
  tagList( #needed for shinyjs
    navbarPage(id = "Panels", # id used for jumping between tabs
               title = div(style = "position: relative; top: -5px;"),  
               #title for browser tab
               windowTitle = "Acute Hospital Activity Data Explorer",
               # When the app is run locally  ie. the 
               # "Run app" button is pressed "Data-Explorer/" may need
               # to be added to the "www/styles.css" filepath for the 
               # app to run correctly.
               # However, it MUST be removed again for the deployment
               # to work.
               header = tags$head(includeCSS("www/styles.css"), # CSS styles
                                  #Icon for browser tab
                                  tags$link(rel="shortcut icon", 
                                            href="favicon_phs.ico")), 
               
               # We are going to divide our UI into discrete sections,
               # called tab panels 
               # To do this, we need the layout "tabsetPanel()" 
               
               ### Tab 1: Introduction ----   
               
               tabPanel("Introduction", 
                        icon = icon("info-circle"),
                        h2("Data explorer"),
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
                                   " - shows the data over time by multiple locations."),
                                 tags$li(
                                   tags$b(
                                     actionLink(
                                       "tabpanel_trend_activity",
                                       "Time trend (activity comparison)")),
                                   icon("line-chart"),
                                   " - shows the data over time by multiple activity types."),
                                 
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
                               
                               tags$br(),
                               p("When using the data explorer please take the", 
                                 "following factors into consideration:"),
                               tags$ul(
                                 tags$li("Please note this release includes Scotland going into emergency measures", 
                                         "due to COVID-19, which will impact on the volume", 
                                         "of hospital activity and trends observed."),
                                 tags$li("NHS Louisa Jordan hospital has been included within this publication. ",
                                         "Outpatient activity across a few specialties has been undertaken since July 2020."),
                                 tags$li("This release does not include specific ",
                                         "COVID-19 information. For PHS Coronavirus ",
                                         "information, visit our ", 
                                         tags$a(
                                           href = paste0("https://www.publichealthscotland.scot/",
                                                         "our-areas-of-work/covid-19/covid-19-data-and-intelligence/"),
                                           "COVID-19 webpages", target = "_blank",
                                           class = "special-link"), ".",
                                         "Coronavirus in Scotland information can also ",
                                         "be found on the ",
                                         tags$a(
                                           href = paste0("https://www.gov.scot/coronavirus-covid-19/"),
                                           "Scottish Government website", target = "_blank",
                                           class = "special-link"), "."),
                                 tags$li("There are issues with the quality of the data", 
                                         "presented. Known issues are summarised in the", 
                                         tags$a(
                                           href = paste0("https://beta.isdscotland.org/",
                                                         "find-publications-and-data/",
                                                         "health-services/hospital-care/",
                                                         "acute-hospital-activity-and-nhs-beds-information-quarterly/",
                                                         publication_date, "/data-quality/"),
                                           "Data Quality", target = "_blank",
                                           class = "special-link"), " and ",
                                         tags$a(
                                           href = paste0("https://beta.isdscotland.org/",
                                                         "find-publications-and-data/",
                                                         "health-services/hospital-care/",
                                                         "acute-hospital-activity-and-nhs-beds-information-quarterly/",
                                                         publication_date,"/trend-data/"),
                                           "Data Trends", target = "_blank", 
                                           class = "special-link"), "sections."),
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
                                   href = "mailto:phs.qualityindicators@phs.scot",
                                   "phs.qualityindicators@phs.scot")),
                                 " and we will be happy to help.")
                        )
               ),
               
               
               
               ### Tab 2: Time trend for multiple location ----   
               
               
               tabPanel("Time trend (location comparison)",
                        icon = icon("line-chart"),
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
                                             border: 0px solid #0078D4;}"),
                                  column(6, uiOutput("geotype_ui_trend")),  
                                  column(6, uiOutput("locname_ui_trend")),
                                  column(6, uiOutput("service_ui_trend")),
                                  column(6, uiOutput("measure_ui_trend")),
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
                                             border: 0px solid #0078D4;}"),
                                  column(6, uiOutput("geotype_ui_trend_2")),  
                                  column(6, uiOutput("locname_ui_trend_2")),
                                  column(6, uiOutput("service_ui_trend_2")),
                                  column(6, uiOutput("measure_ui_trend_2")),
                                  column(6, downloadButton(outputId = 'download_trend_2',
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
                          column(4, uiOutput("quarter_ui_pyramid")), 
                          column(9, uiOutput("measure_ui_pyramid")), 
                          column(3, 
                                 br(),
                                 
                                 # For downloading the data
                                 downloadButton(outputId = 'download_pyramid',
                                                label = 'Download data',
                                                width = "95%"))
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
                        h3("Deprivation"),
                        p("This section allows you to explore the data by 
                          different levels of",
                          tags$a(href=paste0("https://www.gov.scot/collections/",
                                             "scottish-index-of-multiple-deprivation-2020/"), 
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
                                             selected = "All inpatients and daycases")),
                          
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
                                             choices = data_type)),
                          column(4,  
                                 selectInput("hb_flow",
                                             label = "Select the board of interest", 
                                             choices = data_cbf %>%
                                               arrange(hbres_name) %>%
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
                          ),
                          checkboxInput("checkbox_flow",
                                        label = "Include flows within same board?",
                                        value = FALSE)
                        ),
                        mainPanel(width = 12,
                                  htmlOutput("sankey_all", 
                                             width = "96%"),
                                  br(),
                                  fluidRow(
                                    column(6,
                                           htmlOutput("crossb_restext")
                                    ),
                                    column(6,
                                           htmlOutput("crossb_treattext")
                                    )),
                                  fluidRow(
                                    column(6, htmlOutput("sankey_res", 
                                                         width = "48%")),
                                    column(6, htmlOutput("sankey_treat", 
                                                         width = "48%"))),
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
                        p("This section allows you to view the data in table format.
                          You can use the filters to select the  data you are
                          interested in. You can also download the data as a csv 
                          using the download buttons."),
             p("Specialty breakdowns are measured in episodes and spells rather 
               than stays. For more information, please see the ",
               tags$a(
                 href = paste0("https://beta.isdscotland.org/",
                               "find-publications-and-data/health-services/",
                               "hospital-care/",
                               "acute-hospital-activity-and-nhs-beds-information-quarterly",
                               publication_date,
                               "methods-used-to-produce-this-data/"),
                         "methodology section", target = "_blank",
                         class = "special-link"), "."),
             br(),
             column(8,
                    selectInput("filename_table",
                                label = "Select the data file",
                                choices = file_types,
                                width = "95%")
             ),
             column(4,
                    downloadButton(outputId = 'download_table',
                                   label = 'Download data')
             ),  
             dataTableOutput("table_explorer", width = "95%")
             
             )
               )
    )

  
#}

### END OF SCRIPT ###
