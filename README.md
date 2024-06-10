# Diplomacy Lab Dashboard Group 7

## Description

This project is part of the US State Department's initiative to develop a Data Dashboard for Foreign Service Officers at Embassies around the world. This repository includes the code for the refugee tab, which uses various datasets to provide insights into global displacement trends, demographic details, and solutions for refugees. The complete dashboard can be accessed [here](https://kylehandley.shinyapps.io/DipLabDashboard/).

## Table of Contents

- [Installation and Setup](#installation-and-setup)
- [Usage](#usage)
- [Data Sources](#data-sources)
- [Features and Code Snippets](#features-and-code-snippets)
- [Final Code](#final-code)
- [Contributors](#contributors)
- [License](#license)

## Installation and Setup

To get started with this project, follow these steps:

1. Clone the repository:
    ```sh
    git clone git@github.com:Fazazhar/Diplomacy-Lab-Dashboard.git
    ```

2. Change into the project directory:
    ```sh
    cd Diplomacy-Lab-Dashboard
    ```

3. Install the necessary packages:
    ```r
    install.packages(c("shiny", "shinyjs", "refugees", "ggplot2", "dplyr", "shinydashboard", "plotly", "tidyr", "scales"))
    ```

4. Run the Shiny app:
    ```r
    shiny::runApp()
    ```

## Usage

The dashboard provides an interactive interface to explore various datasets related to refugees, asylum seekers, and internally displaced persons. Navigate through the different tabs to visualize data trends and demographic information.

## Data Sources

The data utilized in this dashboard comes from multiple sources, including:

- [Refugees R Package](https://github.com/PopulationStatistics/refugees)
- [UNHCR Refugee Population Statistics Database](https://www.unhcr.org/refugee-statistics/methodology/)
- [Internal Displacement Monitoring Centre (IDMC)](https://www.internal-displacement.org/)
- [UNRWA](https://www.unrwa.org/)

For detailed information about the data, refer to the project memo:

### Memo Excerpt

The Refugee Tab utilizes the Refugee Population Statistics Database published by the Office of The United Nations High Commissioner for Refugees (UNHCR). The data is maintained and updated annually and semi-annually by the Expert Group on Refugee and IDP Statistics (EGRIS) and the Inter-agency Group on Statelessness Estimation, both commissioned by the UN Statistical Commission (UNSC).

## Features and Code Snippets

### Population Data Visualization

The Population Data tab displays the number of forcibly displaced and stateless people by year, including refugees and asylum seekers. Here is a code snippet used to generate the population data plot:

```r
output$plot_population <- renderPlotly({
  data <- filter_data()$population
  plot_graph(data, "year", input$population_vars, "Population Data", "Year", "Number of People", input$population_vars)
})
```

### Asylum Applications Data

The Asylum Applications tab shows the number of applications from those seeking asylum. Below is a snippet to generate the asylum applications plot:

```r
output$plot_asylum_applications <- renderPlotly({
  data <- filter_data()$asylum_applications
  plot_graph(data, "year", input$asylum_applications_vars, "Asylum Applications Data", "Year", "Number of Applications", input$asylum_applications_vars)
})
```

## Final Code

The final code for this project can be found [here](https://github.com/Fazazhar/Diplomacy-Lab-Dashboard/blob/main/g7_final_dashboard/g7_dashboard_final_acled.R).

## Contributors
- [Farrel Azhar](https://www.linkedin.com/in/farrel-azhar-6b8179236/)
- [Adam Rosario](https://www.linkedin.com/in/adam-rosario/)
- [Payton Morlock](https://www.linkedin.com/in/payton-morlock-b474721b5/)
- [Sibo Zheng](https://www.linkedin.com/in/sibo-zheng-9b83b9248/)

## License

This project is licensed under the MIT License - see the [LICENSE](https://github.com/Fazazhar/Diplomacy-Lab-Dashboard/blob/main/LICENSE.md) file for details.