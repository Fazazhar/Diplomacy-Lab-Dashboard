# Load the required libraries
library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(tidyverse)
library(bslib)
library(plotly)

# Load the dataset from local (to minimize load time)
acled_data <- readRDS("acled_data_extended.rds")

# Define default values
default_region <- "North America"
default_country <- "United States"
default_event_types <- "Protests"
default_sub_event_types <- "Peaceful protest"
default_end_date <- max(as.Date(acled_data$event_date))
default_start_date <- default_end_date - 365

# Define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Conflicts", tabName = "conflicts", icon = icon("globe")),
    menuSubItem("Conflict Overview", tabName = "conflict_overview")
  )
)

# Define UI
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "ACLED Conflict Data Dashboard",
    titleWidth = 450
  ),
  sidebar = sidebar,
  dashboardBody(
    tabItems(
      tabItem(tabName = "conflict_overview",
              h2("ACLED Conflict Data Dashboard"),
              fluidRow(
                box(
                  title = "ACLED Data Description",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  "The Armed Conflict Location & Event Data Project (ACLED) is a disaggregated data collection, analysis, and crisis mapping project. ACLED collects the dates, actors, types of violence, locations, fatalities, and other related information on reported political violence and protest events worldwide. The graphs below utilize a pre-downloaded subset of ACLED data from 2010 until June of 2024. The visualizations focus on 'Events' and 'Sub-Events' of which there are six and twenty-five, respectively. These events and sub-events are categorized by three overarching disorder types: ‘Political violence’, ‘Demonstrations’, and ‘Strategic developments’."
                )
              ),
              fluidRow(
                column(width = 3,
                       selectInput("region_select_snapshot", "Select Region:", choices = unique(acled_data$region), selected = default_region)),
                column(width = 3,
                       selectInput("country_select_snapshot", "Select Country:", choices = NULL, selected = default_country)),
                column(width = 3,
                       dateInput("start_date_snapshot", "Start Date:", value = default_start_date)),
                column(width = 3,
                       dateInput("end_date_snapshot", "End Date:", value = default_end_date))
              ),
              fluidRow(
                valueBoxOutput("total_events_acled"),
                valueBoxOutput("total_fatalities_acled")
              ),
              fluidRow(
                box(
                  title = "ACLED Data Summary",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput("acled_table")
                )
              ),
              fluidRow(
                column(width = 3,
                       selectInput("region_select_acled", "Select Region:", choices = unique(acled_data$region), multiple = FALSE)),
                column(width = 3,
                       selectInput("country_select_acled", "Select Country:", choices = NULL, multiple = FALSE)),
                column(width = 3,
                       dateInput("start_date_acled", "Start Date:", value = min(as.Date(acled_data$event_date)))),
                column(width = 3,
                       dateInput("end_date_acled", "End Date:", value = max(as.Date(acled_data$event_date))))
              ),
              fluidRow(
                box(
                  title = "Crisis Events (ACLED)",
                  status = "primary",
                  plotOutput("acled_plot")
                ),
                box(
                  title = "Crisis Sub-events (ACLED)",
                  status = "primary",
                  plotOutput("acled_plot2")
                )
              ),
              h2("Conflict Time Series"),
              fluidRow(
                column(width = 3,
                       selectInput("region_select_ts", "Select Region:", choices = unique(acled_data$region), multiple = FALSE)),
                column(width = 3,
                       selectInput("country_select_ts", "Select Country:", choices = NULL, multiple = FALSE)),
                column(width = 3,
                       dateInput("start_date_ts", "Start Date:", value = default_start_date)),
                column(width = 3,
                       dateInput("end_date_ts", "End Date:", value = default_end_date))
              ),
              fluidRow(
                column(width = 6,
                       selectInput("event_type_select_ts", "Select Event Type:", choices = unique(acled_data$event_type), selected = default_event_types)),
                column(width = 6,
                       selectInput("sub_event_type_select_ts", "Select Sub-event Type:", choices = unique(acled_data$sub_event_type), selected = default_sub_event_types))
              ),
              fluidRow(
                box(
                  title = "Event Type Time Series",
                  status = "primary",
                  plotlyOutput("ts_event_plot")
                ),
                box(
                  title = "Sub-event Type Time Series",
                  status = "primary",
                  plotlyOutput("ts_sub_event_plot")
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Update country options based on selected region
  observeEvent(input$region_select_snapshot, {
    updateSelectInput(session, "country_select_snapshot", 
                      choices = unique(acled_data$country[acled_data$region == input$region_select_snapshot]),
                      selected = default_country)
  })
  
  observeEvent(input$region_select_acled, {
    updateSelectInput(session, "country_select_acled", 
                      choices = unique(acled_data$country[acled_data$region == input$region_select_acled]),
                      selected = NULL)
  })
  
  observeEvent(input$region_select_ts, {
    updateSelectInput(session, "country_select_ts", 
                      choices = unique(acled_data$country[acled_data$region == input$region_select_ts]),
                      selected = NULL)
  })
  
  # Extract the date range from the dataset
  min_date <- min(as.Date(acled_data$event_date))
  max_date <- max(as.Date(acled_data$event_date))
  
  # Update the date input range
  updateDateInput(session, "start_date_snapshot", min = min_date, max = max_date, value = default_start_date)
  updateDateInput(session, "end_date_snapshot", min = min_date, max = max_date, value = default_end_date)
  updateDateInput(session, "start_date_acled", min = min_date, max = max_date, value = min_date)
  updateDateInput(session, "end_date_acled", min = min_date, max = max_date, value = max_date)
  updateDateInput(session, "start_date_ts", min = min_date, max = max_date, value = default_start_date)
  updateDateInput(session, "end_date_ts", min = min_date, max = max_date, value = default_end_date)
  
  # Value Boxes for Home Tab
  output$total_events_acled <- renderValueBox({
    filtered_data <- acled_data %>%
      filter(region == input$region_select_snapshot,
             country == input$country_select_snapshot,
             as.Date(event_date) >= input$start_date_snapshot,
             as.Date(event_date) <= input$end_date_snapshot)
    total_events <- nrow(filtered_data)
    valueBox(
      format(total_events, big.mark = ","), "Total ACLED Events", icon = icon("globe"), color = "blue"
    )
  })
  
  output$total_fatalities_acled <- renderValueBox({
    filtered_data <- acled_data %>%
      filter(region == input$region_select_snapshot,
             country == input$country_select_snapshot,
             as.Date(event_date) >= input$start_date_snapshot,
             as.Date(event_date) <= input$end_date_snapshot)
    total_fatalities <- sum(filtered_data$fatalities, na.rm = TRUE)
    valueBox(
      format(total_fatalities, big.mark = ","), "Total ACLED Fatalities", icon = icon("skull"), color = "red"
    )
  })
  
  # Tables for Home Tab
  output$acled_table <- renderTable({
    acled_data %>%
      group_by(event_type) %>%
      summarise(
        Total_Events = n(),
        Total_Fatalities = sum(fatalities, na.rm = TRUE)
      ) %>%
      arrange(desc(Total_Events)) %>%
      rename(
        `Event Type` = event_type,
        `Total Events` = Total_Events,
        `Total Fatalities` = Total_Fatalities
      )
  }, rownames = FALSE)
  
  # ACLED Data Filtering and Plotting for Overview Tab
  filtered_acled <- reactive({
    filtered_data <- acled_data
    
    if (!is.null(input$region_select_acled) && length(input$region_select_acled) > 0) {
      filtered_data <- filtered_data %>%
        filter(region %in% input$region_select_acled)
    }
    
    if (!is.null(input$country_select_acled) && length(input$country_select_acled) > 0) {
      filtered_data <- filtered_data %>%
        filter(country %in% input$country_select_acled)
    }
    
    filtered_data <- filtered_data %>%
      filter(as.Date(event_date) >= input$start_date_acled & as.Date(event_date) <= input$end_date_acled)
    
    # event counts for the first plot
    event_counts <- filtered_data %>%
      group_by(event_type) %>%
      summarise(event_count = n()) %>%
      ungroup()
    
    # sub-event counts for the second plot
    sub_event_counts <- filtered_data %>%
      group_by(sub_event_type) %>%
      summarise(sub_event_count = n()) %>%
      ungroup()
    
    list(event_counts = event_counts, sub_event_counts = sub_event_counts)
  })
  
  output$acled_plot <- renderPlot({
    event_counts <- filtered_acled()$event_counts
    if (nrow(event_counts) == 0) {
      ggplot() +
        geom_text(aes(0.5, 0.5, label = "No data available for the selected filters"), size = 6, color = "red") +
        theme_void()
    } else {
      ggplot(event_counts, aes(x = event_type, y = event_count, fill = event_type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = event_count), position = position_dodge(width = 0.9), vjust = -0.5) + 
        scale_fill_viridis_d() +  # Change the palette here
        labs(
          title = paste("Event Types Between", input$start_date_acled, "and", input$end_date_acled, "in", paste(input$country_select_acled, collapse = ", ")),
          x = "Event Type",
          y = "Number of Events",
          fill = "Event Type"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$acled_plot2 <- renderPlot({
    sub_event_counts <- filtered_acled()$sub_event_counts
    if (nrow(sub_event_counts) == 0) {
      ggplot() +
        geom_text(aes(0.5, 0.5, label = "No data available for the selected filters"), size = 6, color = "red") +
        theme_void()
    } else {
      ggplot(sub_event_counts, aes(x = sub_event_type, y = sub_event_count, fill = sub_event_type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = sub_event_count), position = position_dodge(width = 0.9), vjust = -0.5) + 
        scale_fill_viridis_d() +  # Change the palette here
        labs(
          title = paste("Sub Event Types Between", input$start_date_acled, "and", input$end_date_acled, "in", paste(input$country_select_acled, collapse = ", ")),
          x = "Sub Event Type",
          y = "Number of Events"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
    }
  })
  
  # ACLED Data Filtering and Plotting for Time Series
  filtered_acled_ts <- reactive({
    filtered_data <- acled_data
    
    if (!is.null(input$region_select_ts) && length(input$region_select_ts) > 0) {
      filtered_data <- filtered_data %>%
        filter(region %in% input$region_select_ts)
    }
    
    if (!is.null(input$country_select_ts) && length(input$country_select_ts) > 0) {
      filtered_data <- filtered_data %>%
        filter(country %in% input$country_select_ts)
    }
    
    filtered_data <- filtered_data %>%
      filter(as.Date(event_date) >= input$start_date_ts & as.Date(event_date) <= input$end_date_ts)
    
    # event type time series
    event_ts <- filtered_data %>%
      filter(event_type == input$event_type_select_ts) %>%
      group_by(event_date, country) %>%
      summarise(event_count = n()) %>%
      ungroup()
    
    # sub-event type time series
    sub_event_ts <- filtered_data %>%
      filter(sub_event_type == input$sub_event_type_select_ts) %>%
      group_by(event_date, country) %>%
      summarise(sub_event_count = n()) %>%
      ungroup()
    
    list(event_ts = event_ts, sub_event_ts = sub_event_ts)
  })
  
  output$ts_event_plot <- renderPlotly({
    event_ts <- filtered_acled_ts()$event_ts
    if (nrow(event_ts) == 0) {
      plot_ly() %>%
        add_annotations(text = "No data available for the selected filters",
                        x = 0.5, y = 0.5, showarrow = FALSE,
                        font = list(size = 20, color = "red"))
    } else {
      plot_ly(event_ts, x = ~as.Date(event_date), y = ~event_count, type = 'scatter', mode = 'lines+markers', text = ~paste("Country:", country, "<br>Number of Events:", event_count, "<br>Date:", as.Date(event_date)), hoverinfo = 'text') %>%
        layout(title = "Event Type Time Series",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Number of Events"))
    }
  })
  
  output$ts_sub_event_plot <- renderPlotly({
    sub_event_ts <- filtered_acled_ts()$sub_event_ts
    if (nrow(sub_event_ts) == 0) {
      plot_ly() %>%
        add_annotations(text = "No data available for the selected filters",
                        x = 0.5, y = 0.5, showarrow = FALSE,
                        font = list(size = 20, color = "red"))
    } else {
      plot_ly(sub_event_ts, x = ~as.Date(event_date), y = ~sub_event_count, type = 'scatter', mode = 'lines+markers', text = ~paste("Country:", country, "<br>Number of Events:", sub_event_count, "<br>Date:", as.Date(event_date)), hoverinfo = 'text') %>%
        layout(title = "Sub-event Type Time Series",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Number of Events"))
    }
  })
}

shinyApp(ui, server)
