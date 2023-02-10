############################################################
## Code name - ui.R
## Data Release - Quarterly Data Explorer
## Latest Update: James Fixter, December 2022
##
## Written/run on - R Studio SERVER
## R version - 3.6.1
##
## This script controls the way the dashboard looks, sets up
## the tabs etc.
##
############################################################
### Visual interface ----
ui_code <- tagList( #needed for shinyjs
  navbarPage(
    id = "Panels", # id used for jumping between tabs
    title = div(style = "position: relative; top: -5px;"),
    #title for browser tab
    windowTitle = "Acute Hospital Activity Data Explorer",
    header = tags$head(
      includeCSS("www/styles.css"), # CSS styles
      #Icon for browser tab
      tags$link(rel="shortcut icon",
                href="favicon_phs.ico")
    ),
    
    # We are going to divide our UI into discrete sections, called tab panels
    # To do this, we need the layout "tabsetPanel()"
    
############################################################
### Tab 1: Introduction ----
    tabPanel("Introduction",
             icon = icon("info-circle"),
             h2("Data explorer"),
             column(10,
                    tags$br(),
                    p("Use the data explorer to find the data you want",
                      "and visualise those data in different ways. Within each of the",
                      "following six sections, there are filters for",
                      "selecting the data you are interested in:"),
                    tags$ul(
                      tags$li(
                        tags$b(
                          actionLink(
                            "tabpanel_trend_location",
                            "Time trend (location comparison)")),
                        icon("line-chart"),
                        " – shows the data over time by multiple locations"),
                      
                      tags$li(
                        tags$b(
                          actionLink(
                            "tabpanel_trend_activity",
                            "Time trend (activity comparison)")),
                        icon("line-chart"),
                        " – shows the data over time by multiple activity types"),
                      
                      tags$li(
                        tags$b(
                          actionLink(
                            "tabpanel_age_sex",
                            "Age/sex")),
                        icon("bar-chart"),
                        " – shows the age and sex distribution of the data"),
                      
                      tags$li(
                        tags$b(
                          actionLink(
                            "tabpanel_deprivation",
                            "Deprivation")),
                        icon("circle-user", lib = "font-awesome"),
                        " – shows activity across different levels of deprivation in accordance with the
                        Scottish Index of Multiple Deprivation (SIMD)"),
                      
                      tags$li(
                        tags$b(
                          actionLink(
                            "tabpanel_cbf",
                            "Cross-boundary flow")),
                        icon("exchange"),
                        " – shows the relationship between where",
                        "patients live and where they are treated"),
                      
                      tags$li(
                        tags$b(
                          actionLink(
                            "tabpanel_table",
                            "Table")),
                        icon("table"),
                        "– allows you to view the data in a table")
                    ),
                    
                    tags$br(),
                    p("When using the data explorer, please consider the",
                      "following:"),
                    tags$ul(
                      tags$li("please note that the volume of hospital activity", 
                              "and trends observed continue to be impacted post-pandemic",
                              "and during the recovery phase"),
                      tags$li("NHS Louisa Jordan hospital has been included within this publication: outpatient activity across",
                              "several specialties was undertaken from July 2020 and day case activity across several specialties",
                              "from January 2021 before NHS Louisa Jordan hospital closed at the end of March 2021"),
                      tags$li("this release does not include specific ",
                              "COVID-19 information: visit our ",
                              tags$a(
                                href = paste0("https://www.publichealthscotland.scot/",
                                              "our-areas-of-work/covid-19/covid-19-data-and-intelligence/"),
                                "COVID-19 webpages",target = "_blank",class = "special-link"), 
                              "for PHS COVID-19 information or the ",
                              tags$a(
                                href = paste0("https://www.gov.scot/coronavirus-covid-19/"),
                                "Scottish Government website (external website)",target = "_blank",class = "special-link"),
                              " for information on COVID-19 in Scotland"
                              ),
                      tags$li("there are issues with the quality of the data",
                              "presented, which are summarised in the",
                              tags$a(
                                href = paste0(pub_url, "data-quality/"),
                                "Data quality", target = "_blank",
                                class = "special-link"), "and",
                              tags$a(
                                href = paste0(pub_url, "trend-data/"),
                                "Trend data", target = "_blank",
                                class = "special-link"), "sections"),
                      tags$li("the data for the most recent quarter are",
                              "provisional and subject to",
                              "change in future publications, as submissions",
                              "may be updated to reflect a more accurate and",
                              "complete set of data from NHS Boards"),
                      tags$li("missing data points within graphs or missing",
                              "rows of data within the tables indicate ",
                              "‘no data’ or ‘no activity’"),
                      tags$li("disclosure control methods have been applied",
                              "to the data to protect patient",
                              "confidentiality: therefore, figures may not",
                              "be additive")),
                    tags$br(),
                    p("If you have any trouble using this tool, or if you have",
                      "further questions relating to the data, please contact",
                      "us at ",
                      tags$b(tags$a(
                        href = "mailto:phs.qualityindicators@phs.scot",
                        "phs.qualityindicators@phs.scot,")),
                      " and we will be happy to help.")
             )
    ),
    
    
############################################################
### Tab 2: Time trend for multiple location ----
    tabPanel("Time trend (location comparison)",
             icon = icon("line-chart"),
             h3("Time trend"),
             p("This section shows changes over time. Use the filters",
               "to select the data you are interested in (you can visualise",
               "multiple locations at the same time). To view the data",
               "in a table, use the ‘Show/hide table’ button. To download your",
               "data selection as a .csv file, use the ‘Download data’ button."),
             tags$ul(
               tags$li(
                 tags$b("Download plot as a .png file"),
                 icon("camera"),
                 " – click this button to save the graph as an",
                 "image (please note that Internet Explorer does",
                 "not support this function)."),
               tags$li(
                 tags$b("Zoom"),
                 icon("search"),
                 " – zoom into the graph by clicking this button",
                 "and then clicking and dragging your mouse over",
                 "the area of the graph you are interested in."),
               tags$li(
                 tags$b("Pan"),
                 icon("move", lib = "glyphicon"),
                 " – adjust the axes of the graph by clicking this",
                 "button and then clicking and moving your mouse",
                 "in any direction you want."),
               tags$li(
                 tags$b("Reset axes"),
                 icon("home"),
                 " – click this button to return the axes to their",
                 "default range.")),
             
             wellPanel(tags$style(".well {background-color: #ffffff;
          border: 0px solid #0078D4;}"),
                       column(6, uiOutput("geotype_ui_trend")),
                       column(6, uiOutput("locname_ui_trend")),
                       column(6, uiOutput("service_ui_trend")),
                       column(6, uiOutput("measure_ui_trend")),
                       column(6, downloadButton(outputId = 'download_trend',
                                                label = 'Download data',
                                                width = "95%")
                       )
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
    
############################################################
### Tab 3: Time trend for multiple activity ----
    tabPanel("Time trend (activity comparison)",
             icon = icon("line-chart"),
             h3("Time trend"),
             p("This section shows changes over time. Use the filters",
               "to select the data you are interested in (you can visualise",
               "multiple activities at the same time). To view the data",
               "in a table, use the ‘Show/hide table’ button. To download your",
               "data selection as a .csv file, use the ‘Download data’ button."),
             tags$ul(
               tags$li(
                 tags$b("Download plot as a .png file"),
                 icon("camera"),
                 " – click this button to save the graph as an image",
                 "(please note that Internet Explorer does not support",
                 "this function)."),
               tags$li(
                 tags$b("Zoom"),
                 icon("search"),
                 " – zoom into the graph by clicking this button and then",
                 "clicking and dragging your mouse over the area of the",
                 "graph you are interested in."),
               tags$li(tags$b("Pan"),
                       icon("move",
                            lib = "glyphicon"),
                       " – adjust the axes of the graph by clicking this button",
                       "and then clicking and moving your mouse in any",
                       "direction you want."),
               tags$li(
                 tags$b("Reset axes"),
                 icon("home"),
                 " – click this button to return the axes to their",
                 "default range.")),
             
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
    
############################################################
### Tab 4: Population pyramid ----
    tabPanel("Age/sex",
             icon = icon("bar-chart"),
             h3("Age/sex"),
             p("In this section, you can explore the age and
           sex distribution of the data. Use the filters
           to select the data you are interested
           in. To view the data in a table, use the ‘Show/hide
           table’ button. To download your data selection
           as a .csv file, use the ‘Download data’ button."),
             tags$ul(
               tags$li(
                 tags$b("Download plot as a .png file"),
                 icon("camera"),
                 " – click this button to save the graph as an image
           (please note that Internet Explorer does not support this
           function)."),
               tags$li(
                 tags$b("Zoom"),
                 icon("search"),
                 " – zoom into the graph by clicking this button and then
           clicking and dragging your mouse over the area of the
           graph you are interested in."),
               tags$li(
                 tags$b("Pan"),
                 icon("move", lib = "glyphicon"),
                 " – adjust the axes of the graph by clicking this button
           and then clicking and moving your mouse in any direction
           you want."),
               tags$li(
                 tags$b("Reset axes"),
                 icon("home"),
                 " – click this button to return the axes to their
           default range.")),
             
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
                       HTML("</div>"))
    ),
    
############################################################
### Tab 5: Deprivation (SIMD) ----
    tabPanel("Deprivation",
             icon = icon("circle-user", lib = "font-awesome"),
             h3("Deprivation"),
             p("In this section, you can explore the data by
        different levels of deprivation in accordance with the",
               tags$a(href="https://gov.scot/simd",
                      "Scottish Index of Multiple Deprivation (SIMD) (external website)." , target = "_blank",
                      class = "special-link"),
               "Use the filters to select the data you are
          interested in. To view the data in a table, use the
          ‘Show/hide table’ button. To download your data selection
          as a .csv file, use the ‘Download data’ button."),
             tags$ul(
               tags$li(
                 tags$b("Download plot as a .png file"),
                 icon("camera"),
                 " – click this button to save the graph as an image
          (please note that Internet Explorer does not support this
          function)."),
               tags$li(
                 tags$b("Zoom"),
                 icon("search"),
                 " – zoom into the graph by clicking this button and then
          clicking and dragging your mouse over the area of the
          graph you are interested in."),
               tags$li(
                 tags$b("Pan"),
                 icon("move", lib = "glyphicon"),
                 " – adjust the axes of the graph by clicking this button
          and then clicking and moving your mouse in any direction
          you want."),
               tags$li(
                 tags$b("Reset axes"),
                 icon("home"),
                 " – click this button to return the axes to their
          default range.")
             ),
             
             wellPanel(
               column(4, uiOutput("geotype_ui_simd")),
               column(4, uiOutput("locname_ui_simd")),
               column(4, uiOutput("quarter_simd")),
               column(9, uiOutput("measure_simd")),
               # For downloading the data
               column(3,
                      br(),
                      # For downloading the data
                      downloadButton(outputId = 'download_simd',
                                     label = 'Download data',
                                     width = "95%"))
             ),
             mainPanel(width = 12,
                       plotlyOutput("simd_plot"),
                       # Button to show/hide div where data table is
                       HTML("<button data-toggle='collapse' href='#simd'
                        class='btn btn-primary'>
                        <strong>Show/hide table</strong></button>"),
                       HTML("<div id='simd' class='collapse'> "),
                       dataTableOutput("table_simd",
                                       width = "95%"),
                       HTML("</div>"))
    ),
    
############################################################
### Tab 6: Cross boundary ----
    tabPanel("Cross-boundary",
             icon = icon("exchange"),
             h3("Cross-boundary flow"),
             p("This section shows where patients are treated.
           The top chart shows where patients living in each NHS
           Board are treated, while the bottom charts show data specific to
           the NHS Board selected in the ‘Select the board of interest’ dropdown menu.
           The bottom left-hand chart shows where patients living in the NHS Board
           of interest are treated. The bottom right-hand chart shows where
           patients treated in the NHS Board of interest come from.
           Use the ‘Include flows within same board?’ tick box to include patients
           treated in their own NHS Board in the charts."),
             p("Use the filters to select the data you are interested
           in. To view the data in a table, use the ‘Show/hide table’
           button. To download your data selection as a .csv file, use the
           ‘Download data’ button."),
             wellPanel(
               column(4, uiOutput("datatype_flow")),
               column(4, uiOutput("hb_flow"),
                      downloadButton(outputId = 'download_flow',
                                     label = 'Download data')),
               column(4, uiOutput("quarter_flow")),
               checkboxInput("checkbox_flow",
                             label = "Include flows within same board?",
                             value = FALSE)),
             mainPanel(width = 12,
                       htmlOutput("sankey_all",
                                  width = "96%"),
                       br(),
                       fluidRow(
                         column(6,
                                htmlOutput("crossb_restext")),
                         column(6,
                                htmlOutput("crossb_treattext"))
                       ),
                       fluidRow(
                         column(6, htmlOutput("sankey_res",
                                              width = "48%")),
                         column(6, htmlOutput("sankey_treat",
                                              width = "48%"))
                       ),
                       div(style = "width:95%; height:5%",
                           # Button to show/hide div where data table is
                           HTML("<button data-toggle='collapse' data-target='#crossb'
                            class='btn btn-primary' style=padding: 6px 12px;>
                            <strong>Show/hide table</strong></button>"),
                           HTML("<div id='crossb' class='collapse'> "),
                           dataTableOutput("table_crossb",
                                           width = "95%"),
                           HTML("</div>"))
             )
    ),
    
############################################################
### Tab 8: Table ----
    tabPanel("Table",
             icon = icon("table"),
             h3("Table"),
             p("This section presents the data in table format.
              Use the filters to select the data you are interested
               in. To download your data selection as a .csv file, use the
               ‘Download data’ button."),
             uiOutput("table_notes"),
             p("For more information on the data, data variables and definitions,
           please see the ",
               tags$a(
                 href = paste0(pub_url, "data-files/"),
                 "Data files", target = "_blank",
                 class = "special-link"),
               "and",
               tags$a(
                 href = paste0(pub_url, "glossary/"),
                 "Glossary", target = "_blank",
                 class = "special-link"),
               "sections."),
             br(),
             column(8, uiOutput("filename_table")),
             column(4,
                    downloadButton(outputId = 'download_table',
                                   label = 'Download data')),
             DT::dataTableOutput("table_explorer", width = "95%"))))

############################################################
### Credentials ----

if (credentials$authorisation=="Private"){
  # Use secure_app if auth is private
  secure_app(ui_code)
} else if (credentials$authorisation=="Public"){
  # Call ui with no credentials if auth is public
  ui_code
}

############################################################

### END OF SCRIPT ###


