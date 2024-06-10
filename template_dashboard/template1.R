library(censusapi)
library(tidyverse)
library(fredr)

library(ggplot2)


library(bslib)
# clear environment
rm(list = ls())


#install.packages("shinydashboard")
library(shinydashboard)
library(shiny)











today<-Sys.Date()
date_36_months_ago <- seq.Date(today, length = 2, by = "-36 months")[2]
print(date_36_months_ago)


urate<-fredr(series_id="UNRATE",observation_start=as.Date(date_36_months_ago))

# Filter the data to keep only the observations with the most recent date
most_recent_urate <- urate %>%
  filter(date == max(urate$date))%>%
  mutate(value = round(value, 1))%>%
  select(date, value)


# Inflation, CPI all urban, change vs 1 year ago
cpi_change<-fredr(series_id="CPIAUCSL",observation_start=as.Date(date_36_months_ago), units="pc1")

# Filter the data to keep only the observations with the most recent date
most_recent_cpi_rate <- cpi_change %>%
  filter(date == max(cpi_change$date))%>%
  mutate(value = round(value, 1))%>%
  select(date, value) 

# GDP growth, seasonally adjusted annual rate (BEA headline number)
# be careful with the change here and nominal vs real to match press release
# https://www.bea.gov/news/2024/gross-domestic-product-first-quarter-2024-advance-estimate
# BEA headline press release will convert quarterly change into seasonally adjusted annual rate
# on a compounded basis, we need to tell fredr() to get this number
gdp_change<-fredr(series_id="GDP",observation_start=as.Date(date_36_months_ago), units="pca")

# Filter the data to keep only the observations with the most recent date
most_recent_gdp_rate <- gdp_change %>%
  filter(date == max(gdp_change$date))%>%
  mutate(value = round(value, 1))%>%
  select(date, value) 




sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("dashboard")),
    menuItem("Foreign Exchange", icon = icon("th"), tabName = "forex"),
    menuItem("Labor Market", icon = icon("signal"), tabName = "labor")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h2("The Money, it's gots to go up!",icon("money-bill-trend-up",class = "fa-1x")),
            fluidRow(
              # static valueBox
              valueBox(paste0(most_recent_gdp_rate$value,"%"), "GDP Growth Rate", icon = icon("stats", lib = "glyphicon")),
              
              valueBox(paste0(most_recent_urate$value,"%"), "Unemployment Rate", icon = icon("stats", lib = "glyphicon")),
              
              valueBox(paste0(most_recent_cpi_rate$value,"%"), "Annual Inflation Rate", icon = icon("stats", lib = "glyphicon"))
              
            ),
            fluidRow(
              # Some more content
              box(plotOutput("plot1", height = 250)),
              box(plotOutput("plot2", height = 250))
            )
    ),
    
    tabItem(tabName = "forex",
            h2("Forex tab content"),
            fluidRow(
              box(
                title = "Box title",
                status = "primary",
                icon("yen", lib = "glyphicon",class = "fa-3x")
              ),
              box(
                status = "primary"
              )
            )
    ),
    
    tabItem(tabName = "labor",
            h2("Labor market tab content"),
            fluidRow(
              box(
                title = "Box title",
                status = "primary",
                icon("yen", lib = "glyphicon",class = "fa-3x")
              ),
              box(
                status = "primary"
              )
            )
    )
  )
)

# Put them together into a dashboardPage


ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "U.S. and Global Economy Dashboard",
                  titleWidth = 450
  ),
  sidebar,
  body
)







server <- function(input, output) {
  output$plot1 <- renderPlot({ggplot(urate, aes(x = date, y = value)) +
    geom_line(color = "blue") +
    labs(title = "Unemployment Rate", x = "Date", y = "Unemployment Rate (%)") +
    theme_minimal() +
    theme(legend.position = "none")
  })
  
  output$plot2 <- renderPlot({ggplot(cpi_change, aes(x = date, y = value)) +
      geom_line(color = "blue") +
      labs(title = "Inflation Rate (CPI, 12mo change %)", x = "Date", y = "Inflation Rate (%)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
}

shinyApp(ui, server)

















