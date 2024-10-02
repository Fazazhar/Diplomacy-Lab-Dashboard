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


## app.R ##


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    ),
    fluidRow(
      box(plotOutput("plot2", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider2", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })  
  output$plot2 <- renderPlot({
      data <- histdata[seq_len(input$slider2)]
      hist(data)  
  })
}

shinyApp(ui, server)
