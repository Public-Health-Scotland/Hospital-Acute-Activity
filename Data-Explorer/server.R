############################################################
## Code name - server.R
## Data Release - Quarterly Data Explorer
## Latest Update: James Fixter, December 2022
##
## Written/run on - R Studio SERVER
## R version - 3.6.1
##
## This script controls what the data explorer *does*, i.e. it
## creates all of the dropdowns, figures, tables etc.
##
## NOTE - the subset function is used in the code below on several
## occasions, rather than using filter
## This is because filter struggles with the '$' operator
## Where possible, filter is still preferred
##
############################################################
### Server ----
function(input, output, session) {

# Check credentials (only runs if secure_app is called in ui)
res_auth <- secure_server(
    check_credentials = check_credentials(credentials))

############################################################
### Tab 1: Introduction ----

# Add hyperlinks to each tab to the corresponding text on the introduction
# tab
observeEvent(input$tabpanel_trend_location, {
  updateTabsetPanel(session,
                    "Panels",
                    selected = "Time trend (location comparison)")})

observeEvent(input$tabpanel_trend_activity, {
  updateTabsetPanel(session,
                    "Panels",
                    selected = "Time trend (activity comparison)")})

observeEvent(input$tabpanel_age_sex, {
  updateTabsetPanel(session,
                    "Panels",
                    selected = "Age/sex")})

observeEvent(input$tabpanel_deprivation, {
  updateTabsetPanel(session,
                    "Panels",
                    selected = "Deprivation")})

observeEvent(input$tabpanel_cbf, {
  updateTabsetPanel(session,
                    "Panels",
                    selected = "Cross-boundary")})

observeEvent(input$tabpanel_table, {
  updateTabsetPanel(session,
                    "Panels",
                    selected = "Table")})

############################################################
### Tab 2: Time trend for multiple location ----

# Reactive dropdowns for this tab
# They will provide a list of locations filtered by geography type
output$geotype_ui_trend <- renderUI({
  shinyWidgets::pickerInput("geotype_trend",
                            label = "Select type of location",
                            choices = sort(geo_type_trend, decreasing = TRUE),
                            selected =  "Scotland")})

output$locname_ui_trend <- renderUI({
  shinyWidgets::pickerInput("locname_trend",
                            label = "Select location (multiple selections allowed)",
                            choices = sort(unique(
                              data_trend$loc_name
                              [data_trend$geo_type %in% input$geotype_trend])),
                            multiple = TRUE,
                            options = list(`selected-text-format` = "count > 1"),
                            selected = case_when(
                              input$geotype_trend == "Scotland" ~ "Scotland",
                              input$geotype_trend == "Council area of residence"
                              ~ "Aberdeen City",
                              input$geotype_trend == "Hospital of treatment"
                              ~ "Aberdeen Royal Infirmary",
                              input$geotype_trend %in% c("NHS board of residence",
                                                         "NHS board of treatment")
                              ~ "NHS Ayrshire & Arran"))})

output$service_ui_trend <- renderUI({
  shinyWidgets::pickerInput("service_trend",
                            label = "Select type of activity",
                            choices = trend_service,
                            selected = "All inpatients and day cases")})

output$measure_ui_trend <- renderUI({
  shinyWidgets::pickerInput("measure_trend",
                            label = "Select measure",
                            choices = trend_measure,
                            selected = "Number of stays/appointments")})

# Reactive datasets
# Reactive dataset for the trend plot
data_trend_plot <- reactive({
  data_trend %>%
    subset(loc_name %in% input$locname_trend &
             measure %in% input$service_trend &
             geo_type %in% input$geotype_trend) %>%
    rename("Total length of stay (days)" = los,
           "Mean length of stay (days)" = avlos,
           "Number of stays/appointments" = count,
           "Did not attend rate (%)" = rate)})

# Plotting
output$trend_plot <- renderPlotly({
  # If no data are available for that quarter then plot message
  # saying data are missing
  if (!isTruthy(!(
    (is.data.frame(data_trend_plot()) &&
     nrow(data_trend_plot()) == 0) |
    (input$measure_trend == "Did not attend rate (%)" &
     !("Did not attend outpatient appointments" %in% input$service_trend)
    )|
    (is.data.frame(data_trend_plot()) &&
     nrow(data_trend_plot()) == 0) |
    (input$measure_trend == "Total length of stay (days)" &
     !(any(c("Elective inpatients",
             "Emergency inpatients",
             "Not specified – inpatients",
             "All inpatients and day cases",
             "All inpatients",
             "All day cases") %in% input$service_trend ))
    )|
    (is.data.frame(data_trend_plot()) &&
     nrow(data_trend_plot()) == 0) |
    (input$measure_trend == "Mean length of stay (days)" &
     !(any(c("Elective inpatients",
             "Emergency inpatients",
             "Not specified – inpatients",
             "All inpatients and day cases",
             "All inpatients",
             "All day cases") %in% input$service_trend ))))))
  {
    # Plotting empty plot just with text
    text_na <- list(x = 5,
                    y = 5,
                    text = "No data available" ,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE)
    plot_ly(type = 'scatter',
            mode = 'lines+markers') %>%
      layout(annotations = text_na,
             # Empty layout
             yaxis = list(showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE),
             xaxis = list(showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE)) %>%
      # Take out plotly logo and collaborate button
      config(displayModeBar = FALSE)} 
  else {
    
    # Text for tooltip
    tooltip <- c(paste0(
      data_trend_plot()$measure, "<br>",
      data_trend_plot()$quarter_name, "<br>",
      data_trend_plot()$loc_name, "<br>",
      input$measure_trend, ": ",
      prettyNum(data_trend_plot()[[input$measure_trend]],
                big.mark = ",")))
    
    # Plotting time trend
    plot_ly(data = data_trend_plot(),
            x = ~quarter_end,
            y = ~get(input$measure_trend),
            text = tooltip,
            hoverinfo = "text",
            type = 'scatter',
            mode = 'lines+markers',
            color = ~loc_name,
            colors = trend_pal[1:(length(unique(data_trend_plot()$loc_name)))])%>%
     
      # Layout
      layout(showlegend = TRUE,
             yaxis = list(
               fixedrange = FALSE,
               title = input$measure_trend,
               rangemode = "tozero"),
             
             # Axis parameter
             xaxis = list(fixedrange = FALSE,
                          title = "Quarter",
                          type = "date",
                          ticktext = data_trend_plot()$quarter_name,
                          tickvals = data_trend_plot()$quarter_end,
                          tickmode = "array",
                          tickangle = -45),
             shapes = list(vline(x=dmy("30-03-2020"))),
             annotations = list(covid_label,covid_arrow)) %>%
      
      # Remove unnecessary buttons from the modebar.
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = list('select2d', 'lasso2d',
                                           'zoomIn2d',
                                           'zoomOut2d',
                                           'autoScale2d',
                                           'toggleSpikelines',
                                           'hoverCompareCartesian',
                                           'hoverClosestCartesian'),
             displaylogo = F, editable = F)}})

# Table
# Table data
table_trend_data <- reactive({
  data_trend %>%
    subset(loc_name %in% input$locname_trend &
             measure %in% input$service_trend &
             geo_type %in% input$geotype_trend) %>%
    select(loc_name, quarter_name, measure, count, rate, los, avlos)})

output$table_trend <- renderDataTable({
  datatable(table_trend_data(),
            style = 'bootstrap',
            class = 'table-bordered table-condensed',
            rownames = FALSE,
            options = list(pageLength = 16,
                           dom = 'tip'),
            colnames = c("Location", "Quarter", "Type of activity",
                         "Number", "DNA rate",
                         "Total length of stay", "Mean length of stay"))})

# Downloading data
output$download_trend <- downloadHandler(
  filename = 'trend_data_multiple_location.csv',
  content = function(file) {
    write.table(table_trend_data(), file, row.names = FALSE,
                col.names = c("Location", "Quarter", "Type of activity",
                              "Number", "DNA rate",
                              "Total length of stay", "Mean length of stay"),
                sep = ",")})

############################################################
### Tab 3: Time trend for multiple activity----

# Reactive dropdowns for this tab
# They will provide a list of locations filtered by geography type
output$geotype_ui_trend_2 <- renderUI({
  shinyWidgets::pickerInput("geotype_trend_2",
                            label = "Select type of location",
                            choices = sort(geo_type_trend, decreasing = TRUE),
                            selected =  "Scotland")})

output$locname_ui_trend_2 <- renderUI({
  shinyWidgets::pickerInput(
    "locname_trend_2",
    label = "Select location",
    choices = sort(unique(
      data_trend$loc_name
      [data_trend$geo_type %in% input$geotype_trend_2])),
    selected = case_when(
      input$geotype_trend == "Scotland" ~ "Scotland",
      input$geotype_trend == "Council area of residence"
      ~ "Aberdeen City",
      input$geotype_trend == "Hospital of treatment"
      ~ "Aberdeen Royal Infirmary",
      input$geotype_trend %in% c("NHS board of residence",
                                 "NHS board of treatment")
      ~ "NHS Ayrshire & Arran"))})

output$service_ui_trend_2 <- renderUI({
  shinyWidgets::pickerInput(
    "service_trend_2",
    label = paste0("Select type of activity ",
                   "(multiple selections allowed)"),
    choices = trend_service,
    multiple = TRUE,
    options = list(
      `selected-text-format` = "count > 1"),
    selected = "All inpatients and day cases")})

output$measure_ui_trend_2 <- renderUI({
  shinyWidgets::pickerInput(
    "measure_trend_2",
    label = "Select measure",
    choices = trend_measure,
    selected = "Number of stays/appointments")})

# Reactive datasets
# Reactive dataset for the trend plot
data_trend_plot_2 <- reactive({
  data_trend %>%
    subset(loc_name %in% input$locname_trend_2 &
             measure %in% input$service_trend_2 &
             geo_type %in% input$geotype_trend_2) %>%
    rename("Total length of stay (days)" = los,
           "Mean length of stay (days)" = avlos,
           "Number of stays/appointments" = count,
           "Did not attend rate (%)" = rate)})

# Plotting
output$trend_plot_2 <- renderPlotly({
  # If no data are available for that quarter then plot message
  # saying data are missing
  if (!isTruthy(!(
    (is.data.frame(data_trend_plot_2()) &&
     nrow(data_trend_plot_2()) == 0) |
    (input$measure_trend_2 == "Did not attend rate (%)" &
     !("Did not attend outpatient appointments" %in% input$service_trend_2)
    )|
    (is.data.frame(data_trend_plot_2()) &&
     nrow(data_trend_plot_2()) == 0) |
    (input$measure_trend_2 == "Total length of stay (days)" &
     !(any(c("Elective inpatients",
             "Emergency inpatients",
             "Not specified – inpatients",
             "All inpatients and day cases",
             "All inpatients",
             "All day cases") %in% input$service_trend_2 ))
    )|
    (is.data.frame(data_trend_plot_2()) &&
     nrow(data_trend_plot_2()) == 0) |
    (input$measure_trend_2 == "Mean length of stay (days)" &
     !(any(c("Elective inpatients",
             "Emergency inpatients",
             "Not specified – inpatients",
             "All inpatients and day cases",
             "All inpatients",
             "All day cases") %in% input$service_trend_2 )))
  )))
  {
    # Plotting empty plot just with text
    text_na <- list(x = 5,
                    y = 5,
                    text = "No data available" ,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE)
    
    plot_ly(type = 'scatter',
            mode = 'lines+markers') %>%
      layout(annotations = text_na,
             
             # Empty layout
             yaxis = list(showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE),
             xaxis = list(showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE)) %>%
      
      # Take out plotly logo and collaborate button
      config(displayModeBar = FALSE)} 
  else {
    
    # Text for tooltip
    tooltip <- c(paste0(
      data_trend_plot_2()$measure, "<br>",
      data_trend_plot_2()$quarter_name, "<br>",
      data_trend_plot_2()$loc_name, "<br>",
      input$measure_trend_2, ": ",
      prettyNum(data_trend_plot_2()[[input$measure_trend_2]],
                big.mark = ","))
    )
    
    # Plotting time trend
    plot_ly(data = data_trend_plot_2(),
            x = ~quarter_end,
            y = ~get(input$measure_trend_2),
            text = tooltip,
            hoverinfo = "text",
            type = 'scatter',
            mode = 'lines+markers',
            color = ~measure,
            colors = trend_pal[1:(length(unique(data_trend_plot_2()$measure)))])%>%
      
      # Layout
      layout(
        showlegend = TRUE,
        yaxis = list(fixedrange = TRUE,
                     title = input$measure_trend_2,
                     rangemode = "tozero"),
        
        # Axis parameter
        xaxis = list(fixedrange = FALSE,
                     title = "Quarter",
                     type = "date",
                     ticktext = data_trend_plot_2()$quarter_name, 
                     tickvals = data_trend_plot_2()$quarter_end,
                     tickmode = "array",
                     tickangle = -45),
        shapes = list(vline(x=dmy("30-03-2020"))),
        annotations = list(covid_label,covid_arrow)) %>%
      
      # Remove unnecessary buttons from the modebar.
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = list('select2d', 'lasso2d',
                                           'zoomIn2d',
                                           'zoomOut2d',
                                           'autoScale2d',
                                           'toggleSpikelines',
                                           'hoverCompareCartesian',
                                           'hoverClosestCartesian'),
             displaylogo = F, editable = F)
  }
}
)

# Table
# Table data
table_trend_data_2 <- reactive({
  data_trend %>%
    subset(loc_name %in% input$locname_trend_2 &
             measure %in% input$service_trend_2 &
             geo_type %in% input$geotype_trend_2) %>%
    select(loc_name, quarter_name, measure, count, rate, los, avlos)})

output$table_trend_2 <- renderDataTable({
  datatable(table_trend_data_2(),
            style = 'bootstrap',
            class = 'table-bordered table-condensed',
            rownames = FALSE,
            options = list(pageLength = 16,
                           dom = 'tip'),
            colnames = c("Location", "Quarter", "Type of activity",
                         "Number", "DNA rate",
                         "Total length of stay", "Mean length of stay"))})

# Downloading data
output$download_trend_2 <- downloadHandler(
  filename =  'trend_data_multiple_activity.csv',
  content = function(file) {
    write.table(table_trend_data_2(), file, row.names = FALSE,
                col.names = c("Location", "Quarter", "Type of activity",
                              "Number", "DNA rate",
                              "Total length of stay", "Mean length of stay"),
                sep = ",")})

############################################################
### Tab 4: Population pyramid ----

# Reactive dropdowns for this tab
# They will provide a list of locations filtered by geography type
output$geotype_ui_pyramid <- renderUI({
  selectInput("geotype_pyramid",
              label = "Select the type of location",
              choices = sort(geo_type_trend, decreasing = TRUE),
              selected =  "Scotland")})

output$locname_ui_pyramid <- renderUI({
  selectInput("locname_pyramid",
              label = "Select the location",
              choices = data_pyramid %>%
                subset(geo_type %in% input$geotype_pyramid) %>%
                distinct(loc_name) %>%
                arrange(loc_name) %>%
                pull(loc_name),
              selectize = TRUE,
              selected = "Scotland")})

output$quarter_ui_pyramid <- renderUI({
  selectInput("quarter_pyramid",
              label = "Select the time period",
              choices = data_pyramid %>%
                  arrange(dmy(quarter_date)) %>%
                  distinct(quarter_name) %>%
                  pull(quarter_name),
              selected = latest_quarter,
              width = "95%")})

output$measure_ui_pyramid <- renderUI({
  selectInput("measure_pyramid",
              label = "Select the type of activity",
              choices = pyramid_service,
              selectize = TRUE,
              selected = "All inpatients and day cases")})

# Reactive datasets
data_pyramid_plot <- reactive({data_pyramid %>%
    subset(loc_name %in% input$locname_pyramid &
             measure %in% input$measure_pyramid &
             geo_type %in% input$geotype_pyramid &
             quarter_name %in% input$quarter_pyramid) %>%
    # So the graph plots correctly with no stacked bars
    mutate(count = ifelse(sex == "Male",
                          -(count),
                          count))})

# Table data
data_table_pyramid <- reactive({
  data_pyramid_plot() %>%
    select(loc_name, quarter_name, measure, age, sex, count) %>%
    # To go back to positive values
    mutate(count = abs(count))})

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
    
    plot_ly(type="bar") %>%
      layout(annotations = text_na,
             # empty layout
             yaxis = list(showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE),
             xaxis = list(showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE)) %>%
      # Take out plotly logo and collaborate button
      config(displayModeBar = FALSE)} 
  else {
    
# Breaks and labels for plot
    max <- round(max(abs(data_pyramid_plot()$count)))
    
# Calculate breaks and labels for x axis
    xlab_female <- round(seq(0, max, length.out = 4))
    xlab_male   <- -c(xlab_female[4], xlab_female[3], xlab_female[2])
    brks <- c(xlab_male, xlab_female)
    lbls <- as.character(c(-xlab_male, xlab_female))
    
# Text for tooltip
    tooltip_pyr <- c(paste0(
      data_pyramid_plot()$sex, " ",
      data_pyramid_plot()$age, "<br>",
      "Number: ",
      prettyNum(abs(data_pyramid_plot()$count),
                big.mark = ",")))

    plot_ly(data = data_pyramid_plot(),
            x = ~count,
            y = ~age,
            color = ~sex,
            colors = trend_pal[2:1],
            text = tooltip_pyr,
            hoverinfo = "text") %>%
      add_bars(orientation = 'h') %>%
      layout(bargap = 0.1,
             barmode = 'overlay',
             yaxis = list(fixedrange = FALSE,
                          title = "Age"),
             xaxis = list(fixedrange = FALSE,
                          tickmode = 'array',
                          tickvals = brks,
                          ticktext = lbls,
                          showline = TRUE,
                          title = paste("Number of",
                                        input$measure_pyramid))) %>%
# Remove unnecessary buttons from the modebar
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = list('select2d', 'lasso2d',
                                           'zoomIn2d',
                                           'zoomOut2d',
                                           'autoScale2d',
                                           'toggleSpikelines',
                                           'hoverCompareCartesian',
                                           'hoverClosestCartesian'),
             displaylogo = F, editable = F)}})

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
                         "Number"))})

# Downloading data
output$download_pyramid <- downloadHandler(
  filename = 'agesex_data.csv',
  content = function(file) {
    write_csv(data_pyramid_plot(),
              file)})

############################################################
### Tab 5: Deprivation (SIMD) ----

# Reactive dropdowns for this tab
# They will provide a list of locations filtered by geography type
output$geotype_ui_simd <- renderUI({
  selectInput("geotype_simd",
              label = "Select the type of location",
              choices = sort(geo_type_trend, decreasing = TRUE),
              selected = "Scotland")})

output$locname_ui_simd <- renderUI({
  selectInput("locname_simd",
              label = "Select the location",
              choices = data_simd %>%
                subset(geo_type %in% input$geotype_simd) %>%
                distinct(loc_name) %>%
                arrange(loc_name) %>%
                pull(loc_name),
              selectize = TRUE,
              selected = "Scotland")})

output$quarter_simd <- renderUI({
    selectInput("quarter_simd",
            label = "Select the time period",
            choices = data_simd %>%
                arrange(dmy(quarter_date)) %>%
                distinct(quarter_name) %>%
                pull(quarter_name),
            selected = latest_quarter,
            width = "95%")})

output$measure_simd <- renderUI({
    selectInput("measure_simd",
            label = "Select the type of activity",
            choices = pyramid_service,
            selectize = TRUE,
            selected = "All inpatients and day cases")})

# Reactive datasets
# Reactive dataset for the simd plot
data_simd_plot <- reactive({data_simd %>%
    subset(loc_name %in% input$locname_simd &
             measure %in% input$measure_simd &
             geo_type %in% input$geotype_simd &
             quarter_name %in% input$quarter_simd)})

# Table data
data_table_simd <- reactive({
  data_simd_plot() %>%
    select(loc_name, quarter_name, measure, simd, count, rate, avlos)})

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
    
    plot_ly(type="bar") %>%
      layout(annotations = text_na,
             # Empty layout
             yaxis = list(showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE),
             xaxis = list(showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE)) %>%
      
      # Take out plotly logo and collaborate button
      config(displayModeBar = FALSE)} 
  else {
    
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
      add_bars(marker = list(color = trend_pal[1])) %>%
      layout(bargap = 0.1,
             yaxis = list(fixedrange = FALSE, title = paste("Number of",
                                                            input$measure_simd)),
             xaxis = list(fixedrange = FALSE, showline = TRUE,
                          title = "Deprivation (SIMD) quintile")) %>%
      
      # Remove unnecessary buttons from the modebar.
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = list('select2d', 'lasso2d',
                                           'zoomIn2d',
                                           'zoomOut2d',
                                           'autoScale2d',
                                           'toggleSpikelines',
                                           'hoverCompareCartesian',
                                           'hoverClosestCartesian'),
             displaylogo = F, editable = F)}})

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
                         "Mean length of stay"))})

# Downloading data
output$download_simd <- downloadHandler(
  filename =  'deprivation_data.csv',
  content = function(file) {
    write_csv(data_simd_plot(),
              file)})


############################################################
### Tab 6: Cross-boundary ----

# Reactive dropdowns for this tab
# They will provide a list of health boards and time periods
output$datatype_flow <- renderUI({
    selectInput("datatype_flow",
                label = "Select the hospital service",
                choices = data_type)})

output$hb_flow <- renderUI({
    selectInput("hb_flow",
            label = "Select the board of interest",
            choices = data_cbf %>%
                arrange(hbres_name) %>%
                distinct(hbres_name) %>%
                pull(hbres_name))})

output$quarter_flow <- renderUI({
selectInput("quarter_flow",
            label = "Select the time period",
            choices = data_cbf %>%
                arrange(dmy(quarter_date)) %>%
                distinct(quarter_name) %>%
                pull(quarter_name),
            selected = latest_quarter)})

# Reactive data
# Creating dynamic selection of dataset.
data_flow <- reactive({data_cbf %>%
    subset(file == input$datatype_flow)})

# For all HB
flow_all <- reactive({
  if(input$checkbox_flow == FALSE) {
    data_flow() %>%
      subset(quarter_name == input$quarter_flow &
               count > 9 &
               boundary_ind == 1)} 
  else {
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
               boundary_ind == 1)} 
  else {
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
               boundary_ind == 1)} 
  else {
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
    summarise(
      round(count[boundary_ind == 0]
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
                            'count',
                            'count.tooltip')],
             options = list(width = "automatic",
                            sankey = opts)
             )
  })

# This one has only the selected health board of residence
output$sankey_res <- renderGvis({
    gvisSankey(flow_res()[, c('hbres_name',
                              'hb_treat_space',
                              'count',
                              'count.tooltip')],
               options = list(width = "automatic",
                              # Change to chart for the html code
                              gvis.plot.tag = NULL,
                              sankey = opts)
               )
    })


# This one has only the selected health board of treatment
output$sankey_treat <- renderGvis({
    gvisSankey(flow_treat()[, c('hbres_name',
                                'hb_treat_space',
                                'count',
                                'count.tooltip')],
               options = list(width = "automatic",
                              # Change to chart for the html code
                              gvis.plot.tag = NULL,
                              sankey = opts)
               )
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
             quarter_name, count)} 
  else {
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
                         "Number"))
})

# Downloading data
output$download_flow <- downloadHandler(
  filename =  'crossb_flow_data.csv',
  content = function(file) {
    write_csv(table_cbfdata(),
              file)})

############################################################
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

output$filename_table <- renderUI({
    selectInput("filename_table",
                label = "Select the data file",
                choices = file_types,
                width = "95%")})

data_table <- reactive({switch(
  input$filename_table,
  
  # 7.1 - Beds Data
  "Beds" = data_bed %>%
    # Create temporary year quarter variable to allow time period dropdown
    # to be displayed chronologically
    mutate(quarter = as.yearqtr(quarter_name, format = "%b - %b-%y"),
           quarter_name = forcats::fct_reorder(
             quarter_name, quarter)) %>%
    select(-quarter) %>%
    rename(Geography_Level = hb_name,
           Area_name = loc_name,
           Specialty = spec_name,
           Time_period = quarter_name,
           Percentage_occupancy = p_occ,
           Quarterly_available_staffed_bed_days = aasb,
           Quarterly_occupied_bed_days = tobd,
           Daily_average_available_staffed_beds = asb,
           Daily_average_occupied_beds = aob) %>%
    mutate_if(is.character, as.factor),
  
  # 7.2 - Specialty Data
  # 7.2.1 - Inpatient Data
  "Inpatients/day cases - Specialty" = data_spec %>%
    filter(file == "Inpatients/day cases") %>%
    mutate(quarter_name = forcats::fct_reorder(
      quarter_name, dmy(quarter_date))) %>%
    select(geo_type, loc_name, measure, spec_name,
           quarter_name, spells, los, avlos) %>%
    rename(Geography_level = geo_type,
           Area_name = loc_name,
           Type_case = measure,
           Specialty = spec_name,
           Time_period = quarter_name,
           Spells = spells,
           Total_length_stay = los,
           Mean_length_stay = avlos) %>%
    mutate_if(is.character, as.factor),
  
  # 7.2.2 - Outpatient Data
  "Outpatients - Specialty" = data_spec %>%
    filter(file == "Outpatients") %>%
    mutate(quarter_name = forcats::fct_reorder(
      quarter_name, dmy(quarter_date))) %>%
    select(geo_type, loc_name, measure, spec_name,
           quarter_name, count, rate) %>%
    rename(Geography_level = geo_type,
           Area_name = loc_name,
           Type_case = measure,
           Specialty = spec_name,
           Time_period = quarter_name,
           Appointments = count,
           DNA_rate = rate) %>%
    mutate_if(is.character, as.factor),
  
  # 7.3 - SIMD Data
  # 7.3.1 - Inpatient Data
  "Inpatients/day cases - Deprivation (SIMD)" = data_simd %>%
    filter(file == "Inpatients/day cases") %>%
    mutate(quarter_name = forcats::fct_reorder(
      quarter_name, dmy(quarter_date))) %>%
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
      quarter_name, dmy(quarter_date))) %>%
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
  "Inpatients/day cases - Time trend" = data_trend %>%
    filter(file == "Inpatients/day cases") %>%
    mutate(quarter_name = forcats::fct_reorder(
      quarter_name, quarter_date_last)) %>%
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
      quarter_name, quarter_date_last)) %>%
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
  "Inpatients/day cases - Age/sex" = data_pyramid %>%
    filter(file == "Inpatients/day cases") %>%
    mutate(quarter_name = forcats::fct_reorder(
      quarter_name, quarter_date_last)) %>%
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
      quarter_name, quarter_date_last)) %>%
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
  "Inpatients/day cases - Cross-boundary flow" = data_cbf %>%
    filter(file == "Inpatients/day cases") %>%
    mutate(quarter_name = forcats::fct_reorder(
      quarter_name, dmy(quarter_date))) %>%
    select(hbres_name, hbtreat_name,
           quarter_name, count) %>%
    arrange(quarter_name) %>%
    rename(Health_board_residence = hbres_name,
           Health_board_treatment = hbtreat_name,
           Time_period = quarter_name,
           Stays = count) %>%
    mutate_if(is.character, as.factor),
  
  # 7.6.2 - Outpatient Data
  "Outpatients - Cross-boundary flow" = data_cbf %>%
    filter(file == "Outpatients") %>%
    mutate(quarter_name = forcats::fct_reorder(
      quarter_name, dmy(quarter_date))) %>%
    select(hbres_name, hbtreat_name,
           quarter_name, count) %>%
    arrange(quarter_name) %>%
    rename(Health_board_residence = hbres_name,
           Health_board_treatment = hbtreat_name,
           Time_period = quarter_name,
           Appointments = count) %>%
    mutate_if(is.character, as.factor))
})

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
            colnames = table_colnames)})

# Notes text above table
output$table_notes <- renderUI({
  if(input$filename_table == "Beds"){
    return(p("Due to the way specialties are recorded for beds data, it is not
               possible to use the PHS beds statistics to estimate the total
               number of beds available for use by different services
               and/or departments. For example, selecting the Paediatric
               specialty grouping will only provide a partial picture of
               the staffed beds that are used for children’s services.
               This is because many beds used for children are not recorded
               under paediatric specialties and are instead recorded under
               more specific specialties such as Haematology, Neurology and
               Respiratory Medicine. Furthermore, the specialty recorded for
               a bed depends partly on what the patient is being treated for:
               therefore, the mix of specialties may change over time for some wards.
               Similarly, analysis of the Accident & Emergency specialty bed
               usage will only provide inpatient and day case information on
               beds within the Accident & Emergency specialty (for example, Accident &
               Emergency ward beds and observation beds staffed overnight). It
               will not provide information on the services/capacity within
               Accident & Emergency departments as a whole."))
  } else if(input$filename_table == "Inpatients/day cases - Specialty"){
    return(p("Specialty breakdowns are measured in episodes and spells rather
                        than stays. For more information, please see the ",
             tags$a(
               href = paste0(pub_url, "methods-used-to-produce-this-data-release/"),
               "Methods used to produce this data release", target = "_blank",
               class = "special-link"), "section."))
  } else {
    return("")
  }
})

# Downloading data
# The downloaded data are those selected by the user using
# the data table filters
output$download_table <-
  downloadHandler(filename = "table_data.csv",
                  content = function(file){
                    write_csv(data_table()
                              [input[["table_explorer_rows_all"]], ],
                              file)})
}

############################################################

### END OF SCRIPT ###