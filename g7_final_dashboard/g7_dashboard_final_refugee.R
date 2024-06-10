library(shiny)
library(shinyjs)
library(refugees)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(plotly)
library(tidyr)
library(scales)

# Load datasets
population_data <- refugees::population
idmc_data <- refugees::idmc
asylum_applications_data <- refugees::asylum_applications
asylum_decisions_data <- refugees::asylum_decisions
demographics_data <- refugees::demographics
solutions_data <- refugees::solutions
unrwa_data <- refugees::unrwa
flows_data <- refugees::flows

# Function to get max and min year
max_min_years <- function(data) {
  data %>%
    summarise(
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE)
    )
}

# Get the year ranges for each dataset
population_years <- max_min_years(population_data)
idmc_years <- max_min_years(idmc_data)
asylum_applications_years <- max_min_years(asylum_applications_data)
asylum_decisions_years <- max_min_years(asylum_decisions_data)
demographics_years <- max_min_years(demographics_data)
solutions_years <- max_min_years(solutions_data)
unrwa_years <- max_min_years(unrwa_data)
flows_years <- max_min_years(flows_data)

# Get unique countries of origin and arrival for UNRWA datasets
unrwa_unique_coo <- paste(unique(unrwa_data$coo_name), collapse = ", ")
unrwa_unique_coa <- paste(unique(unrwa_data$coa_name), collapse = ", ")

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Diplomacy Lab Dashboard Group 7", titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Geopolitical & Social Indicators", tabName = "geopolitical_social", icon = icon("globe"),
               menuSubItem("UNHCR Refugee Data", tabName = "unhcr_refugee_data", icon = icon("dashboard")),
               menuSubItem("ACLED Conflict Data", tabName = "acled_conflict_data", icon = icon("warning"))
      )
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      h1 {
        font-size: 32px;
        color: #333333;
      }
      .dropdown-content {
        font-size: 18px;
        color: #333333;
      }
      .instruction-text {
        color: #1E90FF; /* DodgerBlue color */
      }
    "))),
    tabItems(
      tabItem(tabName = "unhcr_refugee_data",
              h1("UNHCR Refugee Data Dashboard"),
              div(
                class = "dropdown-content",
                style = "margin-bottom: 20px;",
                helpText(HTML(
                  "<strong>Understanding UNHCR Refugee Data</strong><br>
            The UNHCR Refugee Data provides detailed information about forcibly displaced populations, such as refugees, asylum-seekers, internally displaced people, and stateless individuals. This extensive dataset is gathered from three primary sources:<br>
            <ul>
              <li>Data collected from UNHCR’s annual statistical activities since 1951.</li>
              <li>Information from the United Nations Relief and Works Agency for Palestine Refugees in the Near East (UNRWA) for registered Palestine refugees.</li>
              <li>Data from the Internal Displacement Monitoring Centre (IDMC) on individuals displaced within their countries due to conflict or violence.</li>
            </ul>
            This data collection helps in providing a comprehensive overview of global displacement trends and demographic details of the affected populations."
                ))
              ),
              fluidRow(
                column(6,
                       selectizeInput("coo_name", "Country of Origin (2 Max):", choices = unique(population_data$coo_name), selected = c("Rwanda", "Sudan"), multiple = TRUE, options = list(maxItems = 2))
                ),
                column(6,
                       selectizeInput("coa_name", "Country of Arrival (1 Max):", choices = unique(population_data$coa_name), selected = "United States of America", options = list(maxItems = 1))
                )
              ),
              fluidRow(
                box(
                  title = "Population Data", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  numericInput("year_population_start", "Start Year:", value = 2013, min = min(population_data$year), max = max(population_data$year)),
                  numericInput("year_population_end", "End Year:", value = 2023, min = min(population_data$year), max = max(population_data$year)),
                  helpText(paste("Available years: ", population_years$min_year, "-", population_years$max_year)),
                  checkboxGroupInput("population_vars", "Variables to display:", choices = list("Refugees" = "refugees", "Asylum Seekers" = "asylum_seekers", "Returned Refugees" = "returned_refugees"), selected = c("refugees", "asylum_seekers")),
                  plotlyOutput("plot_population"),
                  helpText("Data on forcibly displaced and stateless persons by year, including refugees, asylum-seekers, internally displaced people (IDPs) and stateless people.")
                ),
                box(
                  title = "Asylum Applications Data", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  numericInput("year_asylum_applications_start", "Start Year:", value = 2013, min = min(asylum_applications_data$year), max = max(asylum_applications_data$year)),
                  numericInput("year_asylum_applications_end", "End Year:", value = 2023, min = min(asylum_applications_data$year), max = max(asylum_applications_data$year)),
                  helpText(paste("Available years: ", asylum_applications_years$min_year, "-", asylum_applications_years$max_year)),
                  checkboxGroupInput("asylum_applications_vars", "Variables to display:", choices = list("Applied" = "applied"), selected = "applied"),
                  plotlyOutput("plot_asylum_applications"),
                  helpText("Data on asylum applications including the procedure type and application type.")
                )
              ),
              fluidRow(
                box(
                  title = "Asylum Decisions Data", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  numericInput("year_asylum_decisions_start", "Start Year:", value = 2013, min = min(asylum_decisions_data$year), max = max(asylum_decisions_data$year)),
                  numericInput("year_asylum_decisions_end", "End Year:", value = 2023, min = min(asylum_decisions_data$year), max = max(asylum_decisions_data$year)),
                  helpText(paste("Available years: ", asylum_decisions_years$min_year, "-", asylum_decisions_years$max_year)),
                  checkboxGroupInput("asylum_decisions_vars", "Variables to display:", choices = list("Total Decisions" = "dec_total", "Rejected" = "dec_rejected", "Other" = "dec_other", "Closed" = "dec_closed"), selected = "dec_total"),
                  plotlyOutput("plot_asylum_decisions"),
                  helpText("Data on asylum decisions, including recognitions, rejections, and administrative closures.")
                ),
                box(
                  title = "Demographics Data", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  numericInput("year_demographics_start", "Start Year:", value = 2013, min = min(demographics_data$year), max = max(demographics_data$year)),
                  numericInput("year_demographics_end", "End Year:", value = 2023, min = min(demographics_data$year), max = max(demographics_data$year)),
                  helpText(paste("Available years: ", demographics_years$min_year, "-", demographics_years$max_year)),
                  checkboxGroupInput("demographics_vars", "Variables to display:", choices = list("Male Total" = "m_total", "Female Total" = "f_total"), selected = c("m_total", "f_total")),
                  plotlyOutput("plot_demographics"),
                  helpText("Demographic and sub-national data, where available, including disaggregation by age and sex.")
                )
              ),
              fluidRow(
                box(
                  title = "Solutions Data", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  numericInput("year_solutions_start", "Start Year:", value = 2013, min = min(solutions_data$year), max = max(solutions_data$year)),
                  numericInput("year_solutions_end", "End Year:", value = 2023, min = min(solutions_data$year), max = max(solutions_data$year)),
                  helpText(paste("Available years: ", solutions_years$min_year, "-", solutions_years$max_year)),
                  checkboxGroupInput("solutions_vars", "Variables to display:", choices = list("Returned Refugees" = "returned_refugees", "Resettlement" = "resettlement", "Naturalisation" = "naturalisation", "Returned IDPs" = "returned_idps"), selected = c("resettlement", "naturalisation")),
                  plotlyOutput("plot_solutions"),
                  helpText("Data on durable solutions for refugees and IDPs.")
                ),
                box(
                  title = "Flows Data", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  numericInput("year_flows_start", "Start Year:", value = 2013, min = min(flows_data$year), max = max(flows_data$year)),
                  numericInput("year_flows_end", "End Year:", value = 2023, min = min(flows_data$year), max = max(flows_data$year)),
                  helpText(paste("Available years: ", flows_years$min_year, "-", flows_years$max_year)),
                  checkboxGroupInput("flows_vars", "Variables to display:", choices = list("Refugees" = "refugees", "Asylum Seekers" = "asylum_seekers", "Returned Refugees" = "returned_refugees"), selected = "asylum_seekers"),
                  plotlyOutput("plot_flows"),
                  helpText("Numbers of the people forced to flee during each of the years since 1962.")
                )
              ),
              fluidRow(
                box(
                  title = "IDMC Data", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  numericInput("year_idmc_start", "Start Year:", value = 2013, min = min(idmc_data$year), max = max(idmc_data$year)),
                  numericInput("year_idmc_end", "End Year:", value = 2023, min = min(idmc_data$year), max = max(idmc_data$year)),
                  helpText(paste("Available years: ", idmc_years$min_year, "-", idmc_years$max_year)),
                  helpText(class = "instruction-text", "To display IDMC Data, please indicate the same country for both country of origin and country of arrival."),
                  checkboxGroupInput("idmc_vars", "Variables to display:", choices = list("Total Displaced" = "total"), selected = "total"),
                  plotlyOutput("plot_idmc"),
                  helpText("Data from the Internal Displacement Monitoring Centre on the total number of IDPs displaced due to conflict and violence.")
                ),
                box(
                  title = "UNRWA Data", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  numericInput("year_unrwa_start", "Start Year:", value = 2013, min = min(unrwa_data$year), max = max(unrwa_data$year)),
                  numericInput("year_unrwa_end", "End Year:", value = 2023, min = min(unrwa_data$year), max = max(unrwa_data$year)),
                  helpText(paste("Available years: ", unrwa_years$min_year, "-", unrwa_years$max_year)),
                  div(class = "collapse show", id = "unrwa_countries",
                      h4("Available countries of origin:"),
                      p(unrwa_unique_coo),
                      h4("Available countries of arrival:"),
                      p(unrwa_unique_coa)
                  ),
                  checkboxGroupInput("unrwa_vars", "Variables to display:", choices = list("Total" = "total"), selected = "total"),
                  plotlyOutput("plot_unrwa"),
                  helpText("Data on registered Palestine refugees under UNRWA’s mandate.")
                )
              )
      ),
      tabItem(tabName = "acled_conflict_data",
              h1("ACLED Conflict Data"),
              helpText("This section will contain ACLED Conflict Data.")
      )
    )
  )
)

server <- function(input, output, session) {
  output$coo_name <- renderUI({
    selected_iso <- input$coo_name
    selected_names <- sapply(selected_iso, get_country_name)
    tagList(h3("Selected Countries of Origin:"), p(paste(selected_names, collapse = ", ")))
  })
  
  output$coa_name <- renderUI({
    selected_iso <- input$coa_name
    selected_names <- sapply(selected_iso, get_country_name)
    tagList(h3("Selected Countries of Arrival:"), p(paste(selected_names, collapse = ", ")))
  })
  
  filter_data <- reactive({
    data <- list(
      population = population_data,
      idmc = idmc_data,
      asylum_applications = asylum_applications_data,
      asylum_decisions = asylum_decisions_data,
      demographics = demographics_data,
      solutions = solutions_data,
      unrwa = unrwa_data,
      flows = flows_data
    )
    
    if (!is.null(input$coo_name)) {
      data <- lapply(data, function(df) df %>% filter(coo_name %in% input$coo_name))
    }
    
    if (!is.null(input$coa_name)) {
      data <- lapply(data, function(df) df %>% filter(coa_name %in% input$coa_name))
    }
    
    if (!is.null(input$year_population_start) & !is.null(input$year_population_end)) {
      data$population <- data$population %>% filter(year >= input$year_population_start & year <= input$year_population_end)
    }
    
    if (!is.null(input$year_idmc_start) & !is.null(input$year_idmc_end)) {
      data$idmc <- data$idmc %>% filter(year >= input$year_idmc_start & year <= input$year_idmc_end)
    }
    
    if (!is.null(input$year_asylum_applications_start) & !is.null(input$year_asylum_applications_end)) {
      data$asylum_applications <- data$asylum_applications %>% filter(year >= input$year_asylum_applications_start & year <= input$year_asylum_applications_end)
    }
    
    if (!is.null(input$year_asylum_decisions_start) & !is.null(input$year_asylum_decisions_end)) {
      data$asylum_decisions <- data$asylum_decisions %>% filter(year >= input$year_asylum_decisions_start & year <= input$year_asylum_decisions_end)
    }
    
    if (!is.null(input$year_demographics_start) & !is.null(input$year_demographics_end)) {
      data$demographics <- data$demographics %>% filter(year >= input$year_demographics_start & year <= input$year_demographics_end)
    }
    
    if (!is.null(input$year_solutions_start) & !is.null(input$year_solutions_end)) {
      data$solutions <- data$solutions %>% filter(year >= input$year_solutions_start & year <= input$year_solutions_end)
    }
    
    if (!is.null(input$year_unrwa_start) & !is.null(input$year_unrwa_end)) {
      data$unrwa <- data$unrwa %>% filter(year >= input$year_unrwa_start & year <= input$year_unrwa_end)
    }
    
    if (!is.null(input$year_flows_start) & !is.null(input$year_flows_end)) {
      data$flows <- data$flows %>% filter(year >= input$year_flows_start & year <= input$year_flows_end)
    }
    
    return(data)
  })
  
  plot_graph <- function(data, x_var, y_vars, title, x_label, y_label, variables) {
    if (nrow(data) == 0) {
      p <- ggplot() +
        geom_text(aes(0.5, 0.5, label = "No data available for the selected filters"), size = 6, color = "red") +
        theme_void()
      return(ggplotly(p))
    }
    
    data_long <- data %>% pivot_longer(cols = all_of(y_vars), names_to = "variable", values_to = "value")
    data_long$interaction <- interaction(data_long$coo_name, data_long$variable, sep = ", ", lex.order = TRUE)
    
    unique_vars <- unique(data_long$interaction)
    num_colors <- length(unique_vars)
    colors <- scales::hue_pal()(num_colors)
    linetypes <- rep(c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), length.out = num_colors)
    
    # Capitalize and remove underscores in legend
    data_long$interaction <- gsub("_", " ", data_long$interaction)
    data_long$interaction <- sapply(data_long$interaction, function(x) {
      s <- strsplit(x, ", ")[[1]]
      paste(s[1], gsub("\\b(\\w)", "\\U\\1", s[2], perl=TRUE))
    })
    
    p <- ggplot(data_long, aes_string(x = x_var, y = "value", color = "interaction", linetype = "interaction")) +
      geom_line(size = 1.2) +
      scale_color_manual(values = colors) +
      scale_linetype_manual(values = linetypes) +
      labs(title = title, x = x_label, y = y_label, color = "Variable Type", linetype = "Variable Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
      scale_y_continuous(labels = comma)
    
    ggplotly(p)
  }
  
  output$plot_population <- renderPlotly({
    data <- filter_data()$population
    plot_graph(data, "year", input$population_vars, "Population Data", "Year", "Number of People", input$population_vars)
  })
  
  output$plot_idmc <- renderPlotly({
    data <- filter_data()$idmc
    plot_graph(data, "year", input$idmc_vars, "IDMC Data", "Year", "Total Displaced", input$idmc_vars)
  })
  
  output$plot_asylum_applications <- renderPlotly({
    data <- filter_data()$asylum_applications
    plot_graph(data, "year", input$asylum_applications_vars, "Asylum Applications Data", "Year", "Number of Applications", input$asylum_applications_vars)
  })
  
  output$plot_asylum_decisions <- renderPlotly({
    data <- filter_data()$asylum_decisions
    plot_graph(data, "year", input$asylum_decisions_vars, "Asylum Decisions Data", "Year", "Number of Decisions", input$asylum_decisions_vars)
  })
  
  output$plot_demographics <- renderPlotly({
    data <- filter_data()$demographics
    plot_graph(data, "year", input$demographics_vars, "Demographics Data", "Year", "Total Population", input$demographics_vars)
  })
  
  output$plot_solutions <- renderPlotly({
    data <- filter_data()$solutions
    plot_graph(data, "year", input$solutions_vars, "Solutions Data", "Year", "Number of People", input$solutions_vars)
  })
  
  output$plot_unrwa <- renderPlotly({
    data <- filter_data()$unrwa
    plot_graph(data, "year", input$unrwa_vars, "UNRWA Data", "Year", "Total Refugees", input$unrwa_vars)
  })
  
  output$plot_flows <- renderPlotly({
    data <- filter_data()$flows
    plot_graph(data, "year", input$flows_vars, "Flows Data", "Year", "Number of People", input$flows_vars)
  })
}

shinyApp(ui, server)
