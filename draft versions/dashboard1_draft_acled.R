
library(censusapi)
library(tidyverse)
library(fredr)
library(ggplot2)
library(acled.api)
library(bslib)
library(shinydashboard)
library(shiny)

# clear environment
rm(list = ls())

#install.packages("acled.api")

# acled.api(
#   email.address = Sys.getenv("ACLED_EMAIL_ADDRESS"),
#   access.key = Sys.getenv("ACLED_API_KEY"),
#   country = NULL,
#   region = NULL,
#   start.date = NULL,
#   end.date = NULL,
#   add.variables = NULL,
#   all.variables = FALSE,
#   dyadic = FALSE,
#   interaction = NULL,
#   other.query = NULL)

today<-Sys.Date()

#ACLED data
my.data.frame <- acled.api( # stores an ACLED sample in object my.data.frame
  email.address = Sys.getenv("ACLED_EMAIL_ADDRESS"),
  access.key = Sys.getenv("ACLED_API_KEY"),
  region = c("South Asia", "Central America"), 
  start.date = "2015-09-01", 
  end.date = "2020-01-31")

date_start <- "2019-01-01"
date_end <- today

#########
###APP###
#########
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("dashboard")),
    menuItem("Foreign Exchange", icon = icon("th"), tabName = "forex"),
    menuItem("Labor Market", icon = icon("signal"), tabName = "labor"), 
    menuItem("Geopolitical Data", tabName = "acled", icon = icon("globe"))
  )
)

body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "acled",
            h2("ACLED Data Analysis"),
            fluidRow(
              column(6,
                     selectInput("region_select", "Select Region:", choices = unique(my.data.frame$region), multiple = TRUE),
                     selectInput("country_select", "Select Country:", choices = unique(my.data.frame$country), multiple = TRUE),
                     dateInput("start_date", "Start Date:", value = "2019-01-01"),
                     dateInput("end_date", "End Date:", value = Sys.Date())
              )
            ),
            fluidRow(
              box(
                title = "Crisis Events (ACLED)",
                status = "primary",
                plotOutput("acled_plot")
              )
            ),
            fluidRow(
              box(
                title = "Crisis Sub-events (ACLED)",
                status = "primary",
                plotOutput("acled_plot2")
              )
            )
    )
  )
)

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "U.S. and Global Economy Dashboard",
    titleWidth = 450
  ),
  sidebar,
  body
)

server <- function(input, output, session) {
  
   filtered_acled <- reactive({
    filtered_data <- my.data.frame
    
    if (!is.null(input$region_select) && length(input$region_select) > 0) {
      filtered_data <- filtered_data %>%
        filter(region %in% input$region_select)
    }
    
    if (!is.null(input$country_select) && length(input$country_select) > 0) {
      filtered_data <- filtered_data %>%
        filter(country %in% input$country_select)
    }
    
    filtered_data <- filtered_data %>%
      filter(event_date >= input$start_date & event_date <= input$end_date)
    
    # event counts for the first plot
    event_counts <- filtered_data %>%
      group_by(event_type) %>%
      summarise(event_count = n()) %>%
      ungroup()
    
    #  sub-event counts for the second plot
    sub_event_counts <- filtered_data %>%
      group_by(sub_event_type) %>%
      summarise(sub_event_count = n()) %>%
      ungroup()
    
    list(event_counts = event_counts, sub_event_counts = sub_event_counts)
  })
  
  output$acled_plot <- renderPlot({
    ggplot(filtered_acled()$event_counts, aes(x = "", y = event_count, fill = event_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = event_count), position = position_dodge(width = 0.9), vjust = -0.5) + 
      labs(
        title = paste("Event Types Between", input$start_date, "and", input$end_date, "in", paste(input$country_select, collapse = ", ")),
        x = NULL,
        y = "Number of Events",
        fill = "Event Type"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))
  })
  
  output$acled_plot2 <- renderPlot({
    ggplot(filtered_acled()$sub_event_counts, aes(x = sub_event_type, y = sub_event_count, fill = sub_event_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = sub_event_count), position = position_dodge(width = 0.9), vjust = -0.5) + 
      labs(
        title = paste("Sub Event Types Between", input$start_date, "and", input$end_date, "in", paste(input$country_select, collapse = ", ")),
        x = "Sub Event Type",
        y = "Number of Events"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.position = "none")  
  })
  
  observeEvent(input$update_plot, {
    updatePlotOutput(session, "acled_plot")
  })
  
}

shinyApp(ui, server)
