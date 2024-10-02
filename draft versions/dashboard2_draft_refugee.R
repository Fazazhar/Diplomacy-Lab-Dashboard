
library(shiny)
library(shinyjs)
library(refugees)
library(ggplot2)
library(dplyr)
library(shinydashboard)

population_data <- refugees::population
asylumapp_data <- refugees::asylum_applications

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "U.S. and Global Economy Dashboard",
    titleWidth = 450
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Foreign Exchange", icon = icon("th"), tabName = "forex"),
      menuItem("Labor Market", icon = icon("signal"), tabName = "labor"),
      menuItem("ACLED Data", tabName = "acled", icon = icon("globe")),
      menuItem("Refugees Data", tabName = "refugees", icon = icon("users"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "acled",
        h2("ACLED Data Analysis"),
        fluidRow(
          column(6,
            selectInput("region_select_acled", "Select Region:", choices = unique(population_data$region), multiple = TRUE),
            selectInput("country_select_acled", "Select Country:", choices = unique(population_data$country), multiple = TRUE),
            dateInput("start_date_acled", "Start Date:", value = "2019-01-01"),
            dateInput("end_date_acled", "End Date:", value = Sys.Date())
          )
        ),
        fluidRow(
          box(
            title = "ACLED Data Visualization",
            status = "primary",
            plotOutput("acled_plot")
          )
        )
      ),
      tabItem(tabName = "refugees",
        h2("Refugees Data Analysis"),
        fluidRow(
          column(4,
            actionButton("toggle_button", "Toggle Refugees Data"),
            div(id = "refugees_data",
              hidden(
                selectInput("region_select", "Select Region:", choices = unique(population_data$region), multiple = TRUE),
                selectInput("country_select", "Select Country:", choices = unique(population_data$country), multiple = TRUE),
                dateInput("start_date", "Start Date:", value = "2019-01-01"),
                dateInput("end_date", "End Date:", value = Sys.Date())
              )
            )
          ),
          column(8,
            tabsetPanel(
              tabPanel("Total Refugee Population", plotOutput("totalRefugeesPlot")),
              tabPanel("Top 10 Countries of Asylum", plotOutput("topAsylumCountriesPlot")),
              tabPanel("Refugees by Country of Origin", plotOutput("refugeesByOriginPlot"))
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {

  observeEvent(input$toggle_button, {
    toggle("refugees_data")
  })

  output$totalRefugeesPlot <- renderPlot({
    total_refugees_by_year <- population_data %>%
      group_by(year) %>%
      summarise(total_refugees = sum(refugees, na.rm = TRUE))
    
    ggplot(total_refugees_by_year, aes(x = year, y = total_refugees)) +
      geom_line(color = "blue") +
      labs(title = "Total Refugee Population Over the Years",
           x = "Year", y = "Number of Refugees") +
      theme_minimal()
  })

  output$topAsylumCountriesPlot <- renderPlot({
    most_recent_year <- max(population_data$year, na.rm = TRUE)
    recent_data <- population_data %>%
      filter(year == most_recent_year) %>%
      group_by(coa_name) %>%
      summarise(total_refugees = sum(refugees, na.rm = TRUE)) %>%
      top_n(10, total_refugees) %>%
      arrange(desc(total_refugees))
    
    ggplot(recent_data, aes(x = reorder(coa_name, -total_refugees), y = total_refugees)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste("Top 10 Countries of Asylum in", most_recent_year),
           x = "Country of Asylum", y = "Number of Refugees") +
      theme_minimal() +
      coord_flip()
  })
  
  output$refugeesByOriginPlot <- renderPlot({
    total_refugees_by_coo <- population_data %>%
      group_by(coo_name) %>%
      summarise(total_refugees = sum(refugees, na.rm = TRUE)) %>%
      arrange(desc(total_refugees))
    
    ggplot(total_refugees_by_coo[1:20, ], aes(x = reorder(coo_name, -total_refugees), y = total_refugees)) +
      geom_bar(stat = "identity", fill = "darkorange") +
      labs(title = "Distribution of Refugees by Country of Origin (Top 20)",
           x = "Country of Origin", y = "Number of Refugees") +
      theme_minimal() +
      coord_flip()
  })
}

shinyApp(ui = ui, server = server)


