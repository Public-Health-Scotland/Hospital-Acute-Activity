################################################################
### Server
### Data Explorer script 5 of 5
###
### Original Author: Jaime Villacampa
### Original Date: October 2017
### Last edited by: Marios Alexandropoulos
### Last edited on: 12 September 2018
###
### Written to be run on RStudio Desktop
###
### This script controls what the data explorer *does*



# NOTE - the subset function is used in the below code on several
# occasions, rather than using filter
# This is because filter struggles with the '$' operator
# Where possible, filter is still preferred

### TO DO (Jaime): ----


# Fix issues with selection of dates (max date and order)
# Order quarter variable in tables based on date (use date variable instead?)
# Add visualization for specialty (time trend, bar chart)

# Figure out colors scale for map/fix legend image
# Make map outpatients work (need to check raw data, add switch or
# merge datasets)
# Include los, dna rate and avlos in map, deprivation, age-sex?

# Include bed time trend might require merging datasets
# Fix formatting numbers tooltip
# Set labels for time trend (to avoid cases with decimals)

# Alignment download boxes
# Think about color palette for trend
# Include functionality to save cross-boundary plots





### Server ----


function(input, output) {
  

  ### Tab 2: Time trend----   
    
  # Reactive dropdowns for this tab
  # They will provide a list of locations filtered by geography type
  output$geotype_ui_trend <- renderUI({
    selectInput("geotype_trend",
                label = "Select the type of location", 
                choices = geo_type_trend,
                selected =  "Scotland")
  })
  
  output$locname_ui_trend <- renderUI({
    pickerInput("locname_trend",
                "Select the location",
                choices = data_trend %>%
                  subset(geo_type == input$geotype_trend) %>%
                  distinct(loc_name) %>%
                  arrange(loc_name) %>%
                  pull(loc_name),
                multiple = TRUE,
                options = list("max-options" = 2),
                #selectize = TRUE,
                selected = "Scotland")
  })
  
  # Reactive datasets
  # Reactive dataset for the trend plot

  data_trend_plot <- reactive({
    data_trend %>% 
      subset(loc_name == input$locname_trend &
               measure %in% input$service_trend &
               geo_type == input$geotype_trend) %>% 
      rename("Total length of stay (days)" = los,
             "Mean length of stay (days)" = avlos,
             "Number of stays / appointments" = count,
             "Did not attend rate (%)" = rate)
      })

  # Table data
  table_trend_data <- reactive({
    data_trend %>% 
      subset(loc_name == input$locname_trend &
               measure %in% input$service_trend &
               geo_type == input$geotype_trend) %>% 
      select(loc_name, quarter_name, measure, count, rate, los, avlos)
  })

    # Plotting 
  output$trend_plot <- renderPlotly({
    
    # If no data are available for that quarter then plot message
    # saying data are missing
    if ((is.data.frame(data_trend_plot()) &&
         nrow(data_trend_plot()) == 0) |
        (input$measure_trend == "Did not attend rate (%)" & 
         !("Did not attend outpatient appointments" %in% input$service_trend)
         )|
         (is.data.frame(data_trend_plot()) &&
         nrow(data_trend_plot()) == 0) | 
         (input$measure_trend == "Total length of stay (days)" & 
          !(any(c("Elective inpatients",
                  "Emergency inpatients",
                  "Inpatient transfers",
                  "All inpatients and daycases",
                  "All inpatients",
                  "All daycases") %in% input$service_trend ))
         )|
         (is.data.frame(data_trend_plot()) &&
         nrow(data_trend_plot()) == 0) | 
         (input$measure_trend == "Mean length of stay (days)" & 
          !(any(c("Elective inpatients",
                  "Emergency inpatients",
                  "Inpatient transfers",
                  "All inpatients and daycases",
                  "All inpatients",
                  "All daycases") %in% input$service_trend ))
         )
          
       )
    {
      # Plotting empty plot just with text
      text_na <- list(x = 5,
                      y = 5,
                      text = "No data available" ,
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE)
      
      plot_ly() %>%
        layout(annotations = text_na,
               
               # Empty layout
               yaxis = list(showline = FALSE,
                            showticklabels = FALSE,
                            showgrid = FALSE),
               xaxis = list(showline = FALSE,
                            showticklabels = FALSE,
                            showgrid = FALSE)) %>%
        
        # Take out plotly logo and collaborate button
        config(displayModeBar = FALSE)
      
    } else {

    # Text for tooltip
    tooltip <- c(paste0(data_trend_plot()$measure, "<br>",
                        data_trend_plot()$quarter_name, "<br>",
                        input$measure_trend, ": ",
                        prettyNum(data_trend_plot()[[input$measure_trend]],
                                  big.mark = ",")))

    # Plotting time trend
    plot_ly(data = data_trend_plot(),
            x = ~quarter_date_last, 
            y = ~get(input$measure_trend), 
            text = tooltip,
            hoverinfo = "text",
            type = 'scatter',
            mode = 'lines+markers',
            color = ~measure,
            colors = trend_pal) %>%
      
      # Layout
      layout(annotations = list(), # It needs this due to a buggy behaviour
             showlegend = TRUE,
             yaxis = list(fixedrange = TRUE,
                          title = input$measure_trend,
                          rangemode = "tozero"),
             
             # Axis parameter
             xaxis = list(fixedrange = TRUE,
                          title = "Time period"),
             
             # To get hover compare mode as default
             hovermode = 'closest') %>%
      
      # Take out plotly logo and collaborate button
      config(displaylogo = FALSE,
             collaborate = FALSE,
             editable = FALSE)
    }
    
    }) 

  # Table
  output$table_trend <- renderDataTable({
    datatable(table_trend_data(),
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(pageLength = 16,
                             dom = 'tip'),
              colnames = c("Location", "Quarter", "Type of activity",
                           "Number", "DNA rate",
                           "Total length of stay", "Mean length of stay")  
    )
  })
  

      
  # Downloading data
  output$download_trend <- downloadHandler(
    filename =  'trend_data.csv',
    content = function(file) {
      write_csv(data_trend_plot(), file) 
    }
  )
  
  
             
  ### Tab 3: Population pyramid ----   
    
  # Reactive dropdowns for this tab
  # They will provide a list of locations filtered by geography type
  output$geotype_ui_pyramid <- renderUI({
    selectInput("geotype_pyramid",
                label = "Select the type of location", 
                choices = geo_type,
                selected =  "Scotland")
  })
  
  output$locname_ui_pyramid <- renderUI({
    selectInput("locname_pyramid",
                "Select the location",
                choices = data_pyramid %>%
                  subset(geo_type == input$geotype_pyramid) %>%
                  distinct(loc_name) %>%
                  arrange(loc_name) %>%
                  pull(loc_name),
                selectize = TRUE,
                selected = "Scotland")
  })
  
  # Reactive datasets
  data_pyramid_plot <- reactive({data_pyramid %>% 
      subset(loc_name == input$locname_pyramid & 
               measure == input$measure_pyramid &
               geo_type == input$geotype_pyramid &
               quarter_name == input$quarter_pyramid) %>%
      
      # So the graph plots correctly with no stacked bars
      mutate(count = ifelse(sex == "Male",
                            -(count),
                            count))
  })
  
  # Table data
  data_table_pyramid <- reactive({
    data_pyramid_plot() %>% 
      select(loc_name, quarter_name, measure, age, sex, count) %>%
      
      # To go back to positive values
      mutate(count = abs(count))
  })
  
  # Plotting pyramid population chart
  output$pyramid_plot <- renderPlotly({
    
    # If no data available for that quarter then plot message
    # saying data are missing
    if (is.data.frame(data_pyramid_plot()) &&
        nrow(data_pyramid_plot()) == 0)
    {
      
    # Plotting empty plot just with text
    text_na <- list(x = 5,
                    y = 5,
                    text = "No data available",
                    xref = "x",
                    yref = "y", 
                    showarrow = FALSE)
    
    plot_ly() %>%
      layout(annotations = text_na,
             
             # empty layout
             yaxis = list(showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE),
             xaxis = list(showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE)) %>%
      
      # Take out plotly logo and collaborate button
      config(displayModeBar = FALSE)
      
    } else {
    
    # Breaks and labels for plot
    breaks <- round(max(abs(data_pyramid_plot()$count)) / 3)
    max <- round(max(abs(data_pyramid_plot()$count)))
    
    # Calculate breaks and labels for x axis
    brks <- c(seq(-max, 0, breaks),
              seq(breaks, max, breaks))
    lbls <- paste0(as.character(c(-seq(-max, 0, breaks), 
                                  seq(breaks, max, breaks))))
    
    
    # Text for tooltip
    tooltip_pyr <- c(paste0(data_pyramid_plot()$sex, " ",
                            data_pyramid_plot()$age, "<br>",
                           "Number: ",
                           prettyNum(abs(data_pyramid_plot()$count), 
                                     big.mark = ",")))
    
    
    
    plot_ly(data=data_pyramid_plot(),
            x = ~count,
            y = ~age,
            color = ~sex,
            colors = trend_pal,
            text = tooltip_pyr,
            hoverinfo = "text") %>% 
      add_bars(orientation = 'h') %>%
      layout(bargap = 0.1,
             barmode = 'overlay',
             yaxis = list(fixedrange = TRUE,
                          title = "Age"), 
             xaxis = list(fixedrange = TRUE,
                          tickmode = 'array',
                          tickvals = brks,
                          ticktext = lbls,
                          showline = TRUE,
                          title = paste("Number of",
                                        input$measure_pyramid))) %>%
      
      # Take out plotly logo and collaborate button
      config(displaylogo = FALSE,
             collaborate = FALSE,
             editable = FALSE)
    
    }
  }) 

  # Table
  output$table_pyramid <- renderDataTable({
    datatable(data_table_pyramid(),
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(pageLength = 20,
                             dom = 'tip'),
              colnames = c("Location",
                           "Quarter",
                           "Type of activity",
                           "Age",
                           "Sex",
                           "Number")  
    )
  })
  
  
     
  # Downloading data
  output$download_pyramid <- downloadHandler(
    filename = 'agesex_data.csv',
    content = function(file) {
      write_csv(data_pyramid_plot(),
                file) 
    }
  )
  
  
             
  ### Tab 4: Deprivation (SIMD) ----   
  
  # Reactive dropdowns for this tab
  # They will provide a list of locations filtered by geography type
  output$geotype_ui_simd <- renderUI({
    selectInput("geotype_simd",
                label = "Select the type of location", 
                choices = geo_type,
                selected = "Scotland")
  })
  
  output$locname_ui_simd <- renderUI({
    selectInput("locname_simd",
                "Select the location",
                choices = data_simd %>%
                  subset(geo_type == input$geotype_simd) %>%
                  distinct(loc_name) %>%
                  arrange(loc_name) %>%
                  pull(loc_name),
                selectize = TRUE,
                selected = "Scotland")
  })
  
  # Reactive datasets
  # Reactive dataset for the simd plot
  data_simd_plot <- reactive({data_simd %>% 
      subset(loc_name == input$locname_simd & 
               measure == input$measure_simd &
               geo_type == input$geotype_simd &
               quarter_name == input$quarter_simd) 
  })
  
  # Table data
  data_table_simd <- reactive({
    data_simd_plot() %>% 
      select(loc_name, quarter_name, measure, simd, count, rate, avlos)
  })
  
  # Plotting simd bar chart
  output$simd_plot <- renderPlotly({
    
    # If no data available for that quarter then plot message
    # saying data are missing
    if (is.data.frame(data_simd_plot()) &&
        nrow(data_simd_plot()) == 0)
    {
      
      # Plotting empty plot just with text
      text_na <- list(x = 5,
                      y = 5,
                      text = "No data available" ,
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE)
      
      plot_ly() %>%
        layout(annotations = text_na,
               
               # Empty layout
               yaxis = list(showline = FALSE,
                            showticklabels = FALSE,
                            showgrid = FALSE),
               xaxis = list(showline = FALSE,
                            showticklabels = FALSE,
                            showgrid = FALSE)) %>%
        
        # Take out plotly logo and collaborate button
        config(displayModeBar = FALSE)
      
    } else {
    
    # Text for tooltip
    tooltip_simd <- c(paste0("Quintile: ",
                             data_simd_plot()$simd,
                             "<br>",
                            "Number: ",
                            prettyNum(abs(data_simd_plot()$count),
                                      big.mark = ",")))
    
  
    
    plot_ly(data = data_simd_plot(),
            x = ~simd,
            y = ~count,
            text = tooltip_simd,
            hoverinfo = "text") %>% 
      add_bars(marker = list(color = "#004785")) %>%
      layout(bargap = 0.1, 
             yaxis = list(fixedrange = TRUE, title = paste("Number of",
                                        input$measure_simd)), 
             xaxis = list(fixedrange = TRUE, showline = TRUE,
                          title = "Deprivation (SIMD) quintile")) %>%
      
      # Take out plotly logo and collaborate button
      config(displaylogo = FALSE,
             collaborate = FALSE,
             editable = FALSE)
    }
  }) 
  
  # Table
  output$table_simd <- renderDataTable({
    datatable(data_table_simd(),
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(pageLength = 20, dom = 'tip'),
              colnames = c("Location",
                           "Quarter",
                           "Type of activity",
                           "SIMD quintile",
                           "Number",
                           "DNA rate",
                           "Mean length of stay"))
  })
  
   
     
  # Downloading data
  output$download_simd <- downloadHandler(
    filename =  'deprivation_data.csv',
    content = function(file) {
      write_csv(data_simd_plot(),
                file) 
    }
  )
  
  
              
  ### Tab 5: Map ----   
  ### SECTION NOT IN USE AT THE MOMENT, STILL REQUIRES WORK AND RATE DATA  
  
  # Merging shapefile with dynamic selection of data
  # First for HB
#   hb_pol <- reactive({merge(hb_bound, 
#                             data_mapipdc %>% subset(quarter_name==input$quarter_map
#                                                     & measure==input$measure_map
#                                                     & value_type == input$value_map
#                                                     & substr(loc_name,1,3)=="NHS") %>% #selecting only HB 
#                               droplevels() %>% #dropping missing factor levels to allow merging
#                               rename(HBCode=loc_code), 
#                             by='HBCode') 
#   })   
#   
#   #Now for CA
#   ca_pol <- reactive({merge(ca_bound, 
#                             data_mapipdc %>% subset(quarter_name==input$quarter_map
#                                                     & measure==input$measure_map
#                                                     & value_type == input$value_map
#                                                     & substr(loc_name,1,3)!="NHS") %>% #selecting only HB 
#                               droplevels() %>% #dropping missing factor levels to allow merging
#                               rename(GSS_COD=loc_code) , 
#                             by='GSS_COD')
#   }) 
#   
#   
#   #Palettes for map.
#   pal_hb <- reactive({
#     colorQuantile(c('#004785','#00a2e5', '#99b5ce',  '#99daf5'), hb_pol()$value, n=4)
#   }) 
#   pal_ca <- reactive({
#     colorQuantile(c('#004785','#00a2e5', '#99b5ce',  '#99daf5'), ca_pol()$value, n=4)
#   }) 
#   
#   #title
#   output$title_map <- renderText(paste("Hospital acute care activity - ", input$measure_map, 
#                                        " during ", input$quarter_map))
#   
#   #Plotting map
#   
#   output$map <- renderLeaflet({
#     leaflet() %>% 
#       setView(-5, 56.33148888, zoom = 10) %>% # setting initial view point
#       fitBounds(-7.0, 60.5, 0.25, 55.8)  %>% #limits of map
#       setMaxBounds(-9.0, 62.5, 2.25, 53)  %>% #limits of map
#       addProviderTiles(providers$CartoDB.Positron) %>% #background map
#       #Adding layer control
#       addLayersControl( 
#         baseGroups = c("Health Board", "Council Area"),
#         options = layersControlOptions(collapsed = FALSE)
#       )
#   })
#   #Creating observer to avoid redrawing of map everytime 
#   #a different option (time, measure) is chosen
#   observe({
#     
#     leafletProxy("map") %>%
#       clearShapes() %>% 
#       #Adding polygons with HB
#       addPolygons(data=hb_pol(),  group="Health Board",
#                   color = "#444444", weight = 2, smoothFactor = 0.5, 
#                   label = sprintf(hb_pol()$labs) %>% lapply(HTML), #tooltip for hovering
#                   labelOptions = labelOptions(direction = "left"),
#                   opacity = 1.0, fillOpacity = 0.5,
#                   fillColor = ~pal_hb()(value), # palette
#                   highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                       bringToFront = FALSE) #For highlighting the selection while hovering
#       )%>% 
#       #Adding polygons with HB rate 
#       addPolygons(data=ca_pol(), group="Council Area",
#                   color = "#444444", weight = 2, smoothFactor = 0.5, 
#                   label = sprintf(ca_pol()$labs) %>% lapply(HTML), #tooltip for hovering
#                   labelOptions = labelOptions(direction = "left"),
#                   opacity = 1.0, fillOpacity = 0.5,
#                   fillColor = ~pal_ca()(value), # palette
#                   highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                       bringToFront = FALSE) #For highlighting the selection while hovering
#       ) 
#   })
#   
#   #####################################.    
#   #### Table
#   
#   #Table data
#   table_mapdata <- reactive({
#     data_mapipdc %>% subset(quarter_name==input$quarter_map & measure==input$measure_map) %>% 
#       select(loc_name, quarter_name, measure, value_type, value) %>% 
#       dcast(loc_name+quarter_name+measure~value_type, fun.aggregate=sum)
#     
#   })
#   
#   #Actual table.
#   output$table_map <- DT::renderDataTable({
#     DT::datatable(table_mapdata(),style = 'bootstrap', class = 'table-bordered table-condensed', rownames = FALSE,
#                   options = list(pageLength = 14, dom = 'tip'),
#                   colnames = c("Location", "Quarter", "Measure", "Admissions", "Crude Rate")  
#     )
#   })
#   
#   #####################################.    
#   #### Downloading data
#   output$download_map <- downloadHandler(
#     filename =  'map_data.csv',
#     content = function(file) {
#       write.csv(data_mapipdc, file) 
#     }
#   )
  
  
               
  ### Tab 6: Cross-boundary ----   
     
  # Reactive data
  # Creating dynamic selection of dataset.
  data_flow <- reactive({switch(input$datatype_flow,
                                "Inpatients/Day cases" = data_cbf_ip,
                                "Outpatients" = data_cbf_op
  )})
  
  # For all HB
  flow_all <- reactive({
    if(input$checkbox_flow == FALSE) {
      data_flow() %>% 
        subset(quarter_name == input$quarter_flow & 
                 count > 9 & 
                 boundary_ind == 1)
      
    } else {
      
      data_flow() %>% 
        subset(quarter_name == input$quarter_flow & 
                 count > 9)
    }
  })   
  
  # For only selected HB of residence
  flow_res <- reactive({
    if(input$checkbox_flow == FALSE) {
      data_flow() %>% 
        subset(quarter_name == input$quarter_flow & 
                 hbres_name == input$hb_flow & 
                 boundary_ind == 1)
      
    } else {
      
      data_flow() %>% 
        subset(quarter_name == input$quarter_flow & 
                 hbres_name == input$hb_flow)
    }
  })   
  
  # For only selected HB of treatment
  flow_treat <- reactive({
    if(input$checkbox_flow == FALSE) {
      data_flow() %>% 
        subset(quarter_name == input$quarter_flow &
                 hbtreat_name == input$hb_flow &
                 boundary_ind == 1)
      
    } else {
      
      data_flow() %>%
        subset(quarter_name == input$quarter_flow &
                 hbtreat_name == input$hb_flow)
    }
  })   
  
  
  # Text
  output$crossb_restext <- renderText({
    flow_textres <- data_flow() %>% 
      subset(quarter_name == input$quarter_flow &
               hbres_name == input$hb_flow)
    
    # Percentage of people treated in their own health board
    value_res <- flow_textres %>%
      summarise(round(count[boundary_ind == 0]
                      / sum(count) * 100, 1)) %>%
      pull()
    
    paste0("<b>",
           value_res,
           "</b>",
           "% of the patients from ",
           input$hb_flow,
           " were treated in their own health board area.")
  })
  
  output$crossb_treattext <- renderText({
    flow_texttreat <- data_flow() %>% 
      subset(quarter_name == input$quarter_flow &
               hbtreat_name == input$hb_flow)
    
    # Percentage of people treated in this hb coming from other hb
    value_treat <- flow_texttreat %>%
      summarise(round(sum(count[boundary_ind == 1])
                      / sum(count) * 100, 1)) %>%
      pull()
    
    paste0("<b>",
           value_treat,
           "</b>",
           "% of the patients treated in ",
           input$hb_flow,
           " live in other health board areas.")
  })
  
  
  # Visualizations
  
  # This one has all the health boards at the same time
  output$sankey_all <- renderGvis({
    
    # Note - if this line of code is changed to chart
    # you will get the html code
    options(gvis.plot.tag = NULL)
    
    gvisSankey(flow_all()[, c('hbres_name',
                              'hb_treat_space',
                              'count')],
               options = list(width = "automatic",
                              sankey = opts
               ))
    
  })
  
    
  
  # This one has only the selected health board of residence
  output$sankey_res <- renderGvis({
    
    gvisSankey(flow_res()[, c('hbres_name',
                              'hb_treat_space',
                              'count')],
               options = list(width = "automatic",
                              
                              # Change to chart for the html code
                              gvis.plot.tag = NULL))
    
  })
  
  # This one has only the selected health board of treatment
  output$sankey_treat <- renderGvis({
    
    gvisSankey(flow_treat()[, c('hbres_name',
                                'hb_treat_space',
                                'count')],
               options = list(width = "automatic",
                              
                              # Change to chart for the html code
                              gvis.plot.tag = NULL))
  })
  
      
  # Table
  
  # Selecting the Table data
  table_cbfdata <- reactive({
    
    # This if statement selects what data to show depending on
    # the checkbox status
    if(input$checkbox_flow == FALSE){
      data_flow() %>% 
        subset(quarter_name == input$quarter_flow  &
                 boundary_ind == 1 &
                 (hbtreat_name == input$hb_flow |
                    hbres_name == input$hb_flow)) %>% 
        select(hbres_name, hbtreat_name,
               quarter_name, count)
      
    } else {
      
      data_flow() %>%
        subset(quarter_name == input$quarter_flow &
                 (hbtreat_name == input$hb_flow |
                    hbres_name == input$hb_flow)) %>% 
        select(hbres_name, hbtreat_name,
               quarter_name, count)
    }
  })
  
  # Creating the table
  output$table_crossb <- renderDataTable({
    datatable(table_cbfdata(),
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(pageLength = 10,
                             dom = 'tip'),
              colnames = c("Residence board",
                           "Treatment board",
                           "Quarter",
                           "Number")  
    )
  })
  
      
  # Downloading data
  output$download_flow <- downloadHandler(
    filename =  'crossb_flow_data.csv',
    content = function(file) {
      write_csv(table_cbfdata(),
                file) 
    }
  )
  
  
             
  ### Tab 7: Table----   
    
  # Switch function is used to select correct dataset based on
  # user input
  #
  # Each dataset is formatted to make it suitable to be
  # presented in the data table
  #
  # Each dataset requires slightly different formatting, so no
  # custom functions have been created
  #
  # Note: character variables are converted to factors in each
  # dataset for use in the table
  # This is because dropdown prompts on the table filters only
  # appear for factors
  data_table <- reactive({switch(
    input$filename_table,
    
    # 7.1 - Beds Data
    "Beds" = data_bed %>%
      
      # Create temporary year quarter variable to allow time period dropdown
      # to be displayed chronologically
      mutate(quarter = as.yearqtr(quarter_name, format = "%b - %b-%y"),
             quarter_name = forcats::fct_reorder(
               quarter_name, quarter
             )) %>%
      select(-quarter) %>%
      rename(Area_name = loc_name,
             Specialty = spec_name,
             Time_period = quarter_name,
             Occupancy_percentage = p_occ,
             All_available_beds = aasb,
             Total_occupied_beds = tobd,
             Average_available_staffed_beds = asb,
             Average_occupied_beds = aob) %>%
      mutate_if(is.character, as.factor),
    
    
    # 7.2 - Specialty Data
    
    
    # 7.2.1 - Inpatient Data
    "Inpatients/Day cases - Specialty" = data_spec %>% 
      filter(file == "Inpatients/Day Cases") %>%
      mutate(quarter_name = forcats::fct_reorder(
        quarter_name, dmy(quarter_date)
      )) %>%
      select(geo_type, loc_name, measure, specialty,
             quarter_name, stays, los, avlos) %>% 
      rename(Geography_level = geo_type,
             Area_name = loc_name,
             Type_case = measure,
             Specialty = specialty,
             Time_period = quarter_name,
             Stays = stays,
             Total_length_stay = los,
             Mean_length_stay = avlos) %>%
      mutate_if(is.character, as.factor),
    
    
    # 7.2.2 - Outpatient Data
    "Outpatients - Specialty" = data_spec %>% 
      filter(file == "Outpatients") %>%
      mutate(quarter_name = forcats::fct_reorder(
        quarter_name, dmy(quarter_date)
      )) %>%
      select(geo_type, loc_name, measure, specialty,
             quarter_name, count, rate) %>% 
      rename(Geography_level = geo_type,
             Area_name = loc_name,
             Type_case = measure,
             Specialty = specialty,
             Time_period = quarter_name,
             Appointments = count,
             DNA_rate = rate) %>%
      mutate_if(is.character, as.factor),
    
    
    # 7.3 - SIMD Data
    
    
    # 7.3.1 - Inpatient Data
    "Inpatients/Day cases - Deprivation (SIMD)" = data_simd %>% 
      filter(file == "Inpatients/Day Cases") %>%
      mutate(quarter_name = forcats::fct_reorder(
        quarter_name, dmy(quarter_date)
      )) %>%
      select(geo_type, loc_name, measure, simd,
             quarter_name, count, los, avlos) %>% 
      rename(Geography_level = geo_type,
             Area_name = loc_name,
             Type_case = measure,
             SIMD_quintile = simd,
             Time_period = quarter_name,
             Stays = count,
             Total_length_stay = los,
             Mean_length_stay = avlos) %>%
      mutate_if(is.character, as.factor),
    
    
    # 7.3.2 - Outpatient Data
    "Outpatients - Deprivation (SIMD)" = data_simd %>% 
      filter(file == "Outpatients") %>%
      mutate(quarter_name = forcats::fct_reorder(
        quarter_name, dmy(quarter_date)
      )) %>%
      select(geo_type, loc_name, measure, simd,
             quarter_name, count, rate) %>% 
      rename(Geography_level = geo_type,
             Area_name = loc_name,
             Type_case = measure,
             SIMD_quintile = simd,
             Time_period = quarter_name,
             Appointments = count,
             DNA_rate = rate) %>%
      mutate_if(is.character, as.factor),
    
    
    # 7.4 - Time Trend Data
    
    
    # 7.4.1 - Inpatient Data
    "Inpatients/Day cases - Time trend" = data_trend %>% 
      filter(file == "Inpatients/Day Cases") %>%
      mutate(quarter_name = forcats::fct_reorder(
        quarter_name, quarter_date_last
      )) %>%
      select(geo_type, loc_name, measure,
             quarter_name, count, los, avlos) %>% 
      rename(Geography_level = geo_type,
             Area_name = loc_name,
             Type_case = measure,
             Time_period = quarter_name,
             Stays = count,
             Total_length_stay = los,
             Mean_length_stay = avlos) %>%
      mutate_if(is.character, as.factor),
    
    
    # 7.4.2 - Outpatient Data
    "Outpatients - Time trend" = data_trend %>% 
      filter(file == "Outpatients") %>%
      mutate(quarter_name = forcats::fct_reorder(
        quarter_name, quarter_date_last
      )) %>%
      select(geo_type,loc_name, quarter_name,
             measure, count, rate) %>% 
      rename(Geography_level = geo_type,
             Area_name = loc_name,
             Time_period = quarter_name,
             Type_case = measure,
             Appointments = count,
             DNA_rate = rate) %>%
      mutate_if(is.character, as.factor),
    
    
    # 7.5 - Population Pyramid Data
    
    
    # 7.5.1 - Inpatient Data
    "Inpatients/Day cases - Age/sex" = data_pyramid %>% 
      filter(file == "Inpatients/Day Cases") %>%
      mutate(quarter_name = forcats::fct_reorder(
        quarter_name, quarter_date_last
      )) %>%
      select(geo_type, loc_name, measure, sex, age,
             quarter_name, count, los, avlos) %>% 
      rename(Geography_level = geo_type,
             Area_name = loc_name,
             Type_case = measure,
             Sex = sex,
             Age_group = age,
             Time_period = quarter_name,
             Stays = count,
             Total_length_stay = los,
             Mean_length_stay = avlos) %>% 
      mutate(Stays = abs(Stays)) %>%
      mutate_if(is.character, as.factor),
    
    
    # 7.5.2 - Outpatient Data
    "Outpatients - Age/sex" = data_pyramid %>% 
      filter(file == "Outpatients") %>%
      mutate(quarter_name = forcats::fct_reorder(
        quarter_name, quarter_date_last
      )) %>%
      select(geo_type, loc_name, measure, sex, age,
             quarter_name, count, rate) %>% 
      rename(Geography_level = geo_type,
             Area_name = loc_name,
             Type_case = measure,
             Sex = sex,
             Age_group = age,
             Time_period = quarter_name,
             Appointments = count,
             DNA_rate = rate) %>% 
      mutate(Appointments = abs(Appointments)) %>%
      mutate_if(is.character, as.factor),
    
    
    # 7.6 - Cross-Boundary Data
    
    
    # 7.6.1 - Inpatient Data
    "Inpatients/Day cases - Cross boundary flow" = data_cbf_ip %>%
      mutate(quarter_name = forcats::fct_reorder(
        quarter_name, dmy(quarter_date)
      )) %>%
      select(hbres_name, hbtreat_name,
             quarter_name, count) %>%
      arrange(quarter_name) %>%
      rename(Health_board_residence = hbres_name,
             Health_board_treatment = hbtreat_name,
             Time_period = quarter_name,
             Stays = count) %>%
      mutate_if(is.character, as.factor),
    
    
    # 7.6.2 - Outpatient Data
    "Outpatients - Cross boundary flow" = data_cbf_op %>%
      mutate(quarter_name = forcats::fct_reorder(
        quarter_name, dmy(quarter_date)
      )) %>%
      select(hbres_name, hbtreat_name,
             quarter_name, count) %>%
      arrange(quarter_name) %>%
      rename(Health_board_residence = hbres_name,
             Health_board_treatment = hbtreat_name,
             Time_period = quarter_name,
             Appointments = count) %>%
      mutate_if(is.character, as.factor)
    
    
  )})
  
  
  
  # Creating the table
  output$table_explorer <- renderDataTable({
    
    # Remove the underscore from column names in the table
    table_colnames  <-  gsub("_", " ", colnames(data_table()))
    
    datatable(data_table(),
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(pageLength = 20,
                             dom = 'tip',
                             autoWidth = TRUE),
              filter = "top",
              colnames = table_colnames
    )
  })
  
     
  # Downloading data
  # The downloaded data are those selected by the user using
  # the data table filters
  output$download_table <- 
    downloadHandler(filename = "table_data.csv",
                    content = function(file){
                      write_csv(data_table()
                                [input[["table_explorer_rows_all"]], ],
                                file)
                    }
    )
  
}



### END OF SCRIPT ###