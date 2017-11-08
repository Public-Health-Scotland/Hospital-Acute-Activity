#Syntax to create the data explorer
#Jaime Villacampa October 17

#TODO:
#Clean files used to limit them to the variables that are getting used
#Tidy table outputs

#Talk with Demian regards the style
#Fix issues with selection of dates (max date, etc)

#Figure out colors scale for map/fix legend image
# Make map outpatients work (need to check raw data, add switch or merge datasets)
# Include los and avlos in map?

# Include Lenght of stay and DNA rate in time trends
# Include bed time trend
#Fix formatting numbers tooltip

#Round numbers table (avlos)
#Add age and sex tables to table tab
#Add specialty and deprivation ones to table tab?

############################.
## Server ----
############################.

function(input, output) {
  
  ##############################################.             
  ##############Time trend----   
  ##############################################.  
  #Reactive dropdowns for this tab
  output$geotype_ui_trend <- renderUI({
    selectInput("geotype_trend", label = "Geography type", 
                choices = geo_types, selected =  "Scotland")
  })
  
  output$locname_ui_trend <- renderUI({
    selectInput("locname_trend", "Location name", 
                choices =unique(data_trendip$loc_name[data_trendip$geo_type == input$geotype_trend]),
                selectize = TRUE, selected = "Scotland")
  })
  
  #Reactive datasets
  #reactive dataset for the ip plot
  data_trendip_plot <- reactive({data_trendip %>% 
      subset(loc_name == input$locname_trend & measure %in% input$measure_trend &
               geo_type == input$geotype_trend)
    })
  
  data_trendop_plot <- reactive({data_trendop %>% 
      subset(loc_name == input$locname_trend & measure %in% input$measure_trend &
               geo_type == input$geotype_trend)
  })
  
  #Table data
  table_trenddata_ip <- reactive({
    data_trendip_plot() %>% 
      select(loc_name, quarter_name, measure, stays, los, avlos)
  })
  
  table_trenddata_op <- reactive({
    data_trendop_plot() %>% 
      select(loc_name, quarter_name, measure, count, rate)
  })
  
  
  #Plotting ip
  output$trend_plot_ip <- renderPlotly({
    #Text for tooltip
    tooltip_ip <- c(paste0(data_trendip_plot()$measure, "<br>",
                           data_trendip_plot()$quarter_name, "<br>",
                           "Number: ", format(data_trendip_plot()$stays), big.mark="," ))
    
    plot_ly(data=data_trendip_plot(), x=data_trendip_plot()$quarter_date2, 
            y = data_trendip_plot()$stays, text=tooltip_ip, hoverinfo="text",
            type = 'scatter', mode = 'lines+markers',
            color=data_trendip_plot()$measure, colors = trend_pal) %>% 
      #Layout
      layout(annotations = list(), #It needs this because of a buggy behaviour
             yaxis = list(title = "Stays", rangemode="tozero"), 
             xaxis = list(title = "Quarter"),  #axis parameter
             hovermode = 'false') %>%  # to get hover compare mode as default
      config(displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
    
    }) 
  
  #Plotting op
  output$trend_plot_op <- renderPlotly({
    #Text for tooltip
    tooltip_op <- c(paste0(data_trendop_plot()$measure, "<br>",
                           data_trendop_plot()$quarter_name, "<br>",
                           "Number: ", format(data_trendop_plot()$count), big.mark="," ))
    
    plot_ly(data=data_trendop_plot(), x=~quarter_date2, 
            y = ~count, text=tooltip_op, hoverinfo="text",
            type = 'scatter', mode = 'lines+markers',
            color=~measure, colors = trend_pal) %>% 
      #Layout
      layout(annotations = list(), #It needs this because of a buggy behaviour
             yaxis = list(title = "Appointments", rangemode="tozero"), 
             xaxis = list(title = "Quarter"),  #axis parameter
             hovermode = 'false') %>%  # to get hover compare mode as default
      config(displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
  }) 

  ######Table
  output$table_trendip <- DT::renderDataTable({
    DT::datatable(table_trenddata_ip(), style = 'bootstrap', class = 'table-bordered table-condensed', rownames = FALSE,
                  options = list(pageLength = 16, dom = 'tip'),
                  colnames = c("Location", "Quarter", "Measure", "Stays", 
                               "Total length of stay", "Mean length of stay")  
    )
  })
  
  output$table_trendop <- DT::renderDataTable({
    DT::datatable(table_trenddata_op(), style = 'bootstrap', class = 'table-bordered table-condensed', rownames = FALSE,
                  options = list(pageLength = 16, dom = 'tip'),
                  colnames = c("Location", "Quarter", "Measure", "Appointments", 
                               "Rate (only DNA)")  
    )
  })
  
  #####################################.    
  #### Downloading data ----
  #for downloading data ipdc
  output$download_trendip <- downloadHandler(
    filename =  'trendip_data.csv',
    content = function(file) {
      write.csv(data_trendip_plot(), file) 
    }
  )
  
  #for downloading data op
  output$download_trendop <- downloadHandler(
    filename =  'trendop_data.csv',
    content = function(file) {
      write.csv(data_trendop_plot(), file) 
    }
  )
  
  ##############################################.             
  ##############Population pyramid----   
  ##############################################.  
  #Reactive dropdowns for this tab
  output$geotype_ui_pyramid <- renderUI({
    selectInput("geotype_pyramid", label = "Geography type", 
                choices = geo_types, selected =  "Scotland")
  })
  
  output$locname_ui_pyramid <- renderUI({
    selectInput("locname_pyramid", "Location name", 
                choices =unique(data_pyramid$loc_name[data_pyramid$geo_type == input$geotype_pyramid]),
                selectize = TRUE, selected = "Scotland")
  })
  
  #Reactive datasets
  #reactive dataset for the ip plot
  data_pyramid_plot <- reactive({data_pyramid %>% 
      subset(loc_name == input$locname_pyramid & 
               measure == input$measure_pyramid &
               geo_type == input$geotype_pyramid &
               quarter_name == input$quarter_pyramid) %>% 
      mutate(count = ifelse(sex=="Male", -(count), count))
  })
  
  #Table data
  data_table_pyramid <- reactive({
    data_pyramid_plot() %>% 
      select(loc_name, quarter_name, measure, age, sex, count)
  })
  
  #Plotting ip
  output$pyramid_plot <- renderPlotly({

    #Breaks and labels for plot
    
    breaks <- round(max(abs(data_pyramid_plot()$count))/3)
    max <- round(max(abs(data_pyramid_plot()$count)))
    
    #Calculate breaks and labels for x axis
    brks <- c(seq(-max, 0, breaks), seq(breaks, max, breaks))
    lbls <- paste0(as.character(c(-seq(-max, 0, breaks), 
                                  seq(breaks, max, breaks))))
    
    #Text for tooltip
    tooltip_pyr <- c(paste0(data_pyramid_plot()$sex, " ", data_pyramid_plot()$age, "<br>",
                           "Number: ", abs(data_pyramid_plot()$count)))
    
    plot_ly(data=data_pyramid_plot(), x= ~count, y=~age, color=~sex, colors = trend_pal,
            text=tooltip_pyr, hoverinfo="text") %>% 
      add_bars(orientation = 'h') %>%
      layout(bargap = 0.1, barmode = 'overlay',
             yaxis = list(title = "Age"), 
             hovermode = 'false',
             xaxis = list(tickmode = 'array', tickvals = brks,
                          ticktext = lbls, showline = TRUE,
                          title = "Number")) %>% 
      config(displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button

      #Layout
#       layout(annotations = list(), #It needs this because of a buggy behaviour
#              yaxis = list(title = "Stays", rangemode="tozero"), 
#              xaxis = list(title = "Quarter"),  #axis parameter
#              hovermode = 'false') %>%  # to get hover compare mode as default
    
  }) 
  

  ######Table
  output$table_pyramid <- DT::renderDataTable({
    DT::datatable(data_table_pyramid(), style = 'bootstrap', class = 'table-bordered table-condensed', rownames = FALSE,
                  options = list(pageLength = 20, dom = 'tip'),
                  colnames = c("Location", "Quarter", "Measure", "Age", "Sex", "Count")  
    )
  })
  
  
  #####################################.    
  #### Downloading data ----
  #for downloading data ipdc
  output$download_pyramid <- downloadHandler(
    filename =  'agesex_data.csv',
    content = function(file) {
      write.csv(data_pyramid_plot(), file) 
    }
  )
  
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
  table_mapdata <- reactive({
    data_mapipdc %>% subset(quarter_name==input$quarter_map & measure==input$measure_map) %>% 
      select(loc_name, quarter_name, measure, value_type, value) %>% 
      dcast(loc_name+quarter_name+measure~value_type, fun.aggregate=sum)
    
  })
  
  #Actual table.
  output$table_map <- DT::renderDataTable({
    DT::datatable(table_mapdata(),style = 'bootstrap', class = 'table-bordered table-condensed', rownames = FALSE,
                  options = list(pageLength = 14, dom = 'tip'),
                  colnames = c("Location", "Quarter", "Measure", "Admissions", "Crude Rate")  
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
  #Creating dynamic selection of dataset.
  data_flow <- reactive({switch(input$datatype_flow,
                                "Inpatients/Day cases" = data_cbfip,
                                "Outpatients" = data_cbfop
  )})
  
  
  #For all HB
  flow_all <- reactive({
    data_flow() %>% subset(quarter_name==input$quarter_flow & count>9) #%>% 
    #       group_by(hbres_name, quarter_name) %>% 
    #       top_n(5, episodes)
    #& episodes>9) Not right now, see TODO
    
  })   
  
  #For only selected HB
  flow_one <- reactive({
    data_flow() %>% subset(quarter_name==input$quarter_flow & 
                             hbres_name==input$hbres_flow)
  })   
  
  ############################.
  #Visualizations
  #This one with all HB at the same time.
  output$sankey_all <- renderGvis({
    
    options(gvis.plot.tag=NULL) #if changed to chart you will get the html code
    gvisSankey(flow_all()[,c('hbres_name','hbtreat_name','count')],
               options = list(width = "automatic", sankey=opts
               ))
    
  })
  
  #This one with only the selected
  output$sankey_one <- renderGvis({
    
    gvisSankey(flow_one()[,c('hbres_name','hbtreat_name','count')],
               options = list(width = "automatic",
                              gvis.plot.tag=NULL))#if changed to chart you will get the html code
    
  })
  
  #####################################.    
  ### Table ----
  
  #Table data
  table_cbfdata <- reactive({
    data_flow() %>% subset(quarter_name==input$quarter_flow) %>% 
      select(hbres_name, hbtreat_name, quarter_name, count)
  })
  
  #Actual table.
  output$table_crossb <- DT::renderDataTable({
    DT::datatable(table_cbfdata(),style = 'bootstrap', class = 'table-bordered table-condensed', rownames = FALSE,
                  options = list(pageLength = 10, dom = 'tip'),
                  colnames = c("Residence board", "Treatment board",  "Quarter", "Count")  
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
  
  
  ##############################################.             
  ##############Table----   
  ##############################################.  
  #Make very very simple approach, changing file
  #Table data
  data_table <- reactive({switch(input$filename_table,
                                "Inpatients/Day cases - Cross boundary flow" = data_cbfip %>% 
                                  select(hbres_name, hbtreat_name, quarter_name, count) %>% 
                                  rename(Health_board_residence=hbres_name,
                                         Health_board_treatment=hbtreat_name,
                                         Quarter=quarter_name,
                                         Value=count),
                                "Inpatients/Day cases - Map and time trend" = data_mapipdc %>% 
                                  select(loc_name, measure, quarter_name, value_type, 
                                         value, los, avlos) %>% 
                                  rename(Area_name = loc_name,
                                         Type_case = measure,
                                         Quarter = quarter_name,
                                         Value_type = value_type,
                                         Total_length_stay = los,
                                         Mean_length_stay = avlos),
                                "Outpatients - Cross boundary flow" = data_cbfop %>% 
                                  select(hbres_name, hbtreat_name, quarter_name, count) %>% 
                                  rename(Health_board_residence=hbres_name,
                                         Health_board_treatment=hbtreat_name,
                                         Quarter=quarter_name,
                                         Value=count),
                                "Outpatients - Map and time trend" = data_mapop %>% 
                                  select(loc_name, measure, quarter_name, value_type, 
                                         value, los, avlos) %>% 
                                  rename(Area_name = loc_name,
                                         Type_case = appt_type,
                                         Quarter = quarter_name,
                                         Value_type = value_type,
                                         Total_length_stay = los,
                                         Mean_length_stay = avlos)
  )})
  
  #Actual table.
  output$table_explorer <- DT::renderDataTable({
    DT::datatable(data_table(),style = 'bootstrap', class = 'table-bordered table-condensed', 
                  rownames = FALSE, options = list(pageLength = 20, dom = 'tip'), 
                  filter = "top"
    )
  })
  
  #####################################.    
  #### Downloading data ----
  #for downloading data
  output$download_flow <- downloadHandler(
    filename =  'table_data.csv',
    content = function(file) {
      write.csv(data_table(), file) 
    }
  )
  
}

###END

