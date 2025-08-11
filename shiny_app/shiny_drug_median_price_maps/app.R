#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(readr)

# Load GitHub-hosted CSV
summary_table <- read_csv(url("https://raw.githubusercontent.com/wu-msds-capstones/project-writeup-ellann-travis-capstone/main/data/summary_table_ONLY_FOR_MEDIAN_PRICE_MAPS.csv"))


######## SHINY APP ##########
library(shiny)
library(tigris)
library(sf)
library(ggplot2)
library(dplyr)

# Load static spatial data
options(tigris_use_cache = TRUE)
counties_sf <- tigris::counties(cb = TRUE, class = "sf")

# Define UI
ui <- fluidPage(
  titlePanel("Medicare Median Cost by Drug and County"),
  
  # Dropdown
  selectInput(
    inputId = "drug_select",
    label = "Choose a Drug:",
    choices = sort(unique(summary_table$drug_name_clean)),
    selected = "Adalimumab"
  ),
  
  # Plot appears directly below dropdown
  plotOutput("drug_map", height = "700px")
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive filter for selected drug
  filtered_data <- reactive({
    summary_table %>%
      mutate(COUNTY_CODE = sprintf("%05s", COUNTY_CODE)) %>%
      filter(drug_name_clean == input$drug_select)
  })
  
  # Join spatial and drug data
  joined_data <- reactive({
    counties_sf %>%
      left_join(filtered_data(), by = c("NAME" = "COUNTY", "STUSPS" = "STATE"))
  })
  
  # Plot
  output$drug_map <- renderPlot({
    bbox <- st_bbox(c(xmin = -125, xmax = -66, ymin = 23, ymax = 50), crs = st_crs(counties_sf))
    
    map_data <- joined_data() %>%
      st_crop(bbox)
    
    ggplot(map_data) +
      geom_sf(aes(fill = median_cost)) +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
      labs(
        title = paste("Medicare Median Cost of 30-day Supply of", input$drug_select),
        fill = "Median Cost"
      ) +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)