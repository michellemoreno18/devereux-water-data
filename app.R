# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(leaflet)

# Load and clean data
df <- read_csv("data/water_quality_data.csv", show_col_types = FALSE) %>%
  rename(
    Date = "Date",
    Site = "Site",
    Depth = "Depth (cm)",
    Temperature = "Temperature (C)",
    Salinity = "Salinity (ppt)",
    DO = "Dissolved Oxygen (mg/L)",
    Turbidity = "Turbidity (cm)"
  ) %>%
  mutate(
    Date = mdy(Date),
    Depth = as.numeric(Depth),
    Temperature = as.numeric(Temperature),
    Salinity = as.numeric(Salinity),
    DO = as.numeric(DO),
    Turbidity = as.numeric(Turbidity),
    Month = month(Date),
    Year = year(Date),
    DepthLayer = case_when(
      Site != "PIER" & Depth <= 20 ~ "Surface",
      Site != "PIER" & Depth > 20 ~ "Bottom",
      Site == "PIER" ~ as.character(Depth)
    )
  ) %>%
  filter(Site %in% c("MO1", "CUL1", "VBR1", "PIER")) %>%
  drop_na(Date, Site)

# Site coordinates
site_locations <- tibble::tibble(
  Site = c("MO1", "CUL1", "VBR1", "PIER"),
  Latitude = c(34.410235, 34.413928, 34.417485, 34.411906),
  Longitude = c(-119.878968, -119.873961, -119.874138, -119.877158)
)

# UI
ui <- tagList(
  fluidPage(
    titlePanel("Explore Water Quality Trends"),
    
    tabsetPanel(
      tabPanel("Overview",
               br(),
               h3("Devereux Slough Water Quality Monitoring"),
               p("This Shiny application helps visualize trends in water quality at multiple monitoring sites in Devereux Slough."),
               p("Use the Trends tab to explore time series and seasonal patterns for parameters such as temperature, salinity, dissolved oxygen, and turbidity.")
      ),
      
      tabPanel("Trends",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("site", "Monitoring Site:", choices = unique(df$Site)),
                   
                   uiOutput("depthSelector"),
                   
                   selectInput("parameter", "Parameter:",
                               choices = c("Temperature", "Salinity", "DO", "Turbidity")),
                   
                   sliderInput("yearRange", "Year Range:",
                               min = min(df$Year, na.rm = TRUE),
                               max = max(df$Year, na.rm = TRUE),
                               value = c(min(df$Year, na.rm = TRUE), max(df$Year, na.rm = TRUE)),
                               sep = ""),
                   
                   sliderInput("monthRange", "Months:",
                               min = 1, max = 12, value = c(1, 12), step = 1)
                 ),
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Time Series", plotOutput("timePlot")),
                     tabPanel("Seasonal Patterns", plotOutput("seasonPlot"))
                   )
                 )
               )
      ),
      
      # --- Map Tab ---
      tabPanel("Map",
               br(),
               h3("Monitoring Site Locations"),
               checkboxInput("showLegend", "Show Legend", value = TRUE),
               leafletOutput("map", height = "600px")
      )
    )
  ),
  
  # --- Footer with logos ---
  div(
    style = "text-align: center; padding: 20px; border-top: 1px solid #ccc; margin-top: 30px;",
    img(src = "nrs_logo.png", height = "60px", style = "margin: 10px;"),
    img(src = "COPR_logo.png", height = "60px", style = "margin: 10px;")
  )
)

# Server
server <- function(input, output, session) {
  
  # Dynamic depth input
  output$depthSelector <- renderUI({
    if (input$site == "PIER") {
      selectInput("depth", "Select Pier Depth (cm):",
                  choices = c(10, 50, 100, 150, 200, 250),
                  selected = 50)
    } else {
      selectInput("depth", "Select Depth Layer:",
                  choices = c("Surface", "Bottom"))
    }
  })
  
  # Reactive filtered data
  filteredData <- reactive({
    df %>%
      filter(Site == input$site,
             Year >= input$yearRange[1],
             Year <= input$yearRange[2],
             Month >= input$monthRange[1],
             Month <= input$monthRange[2],
             if (input$site == "PIER") {
               Depth == as.numeric(input$depth)
             } else {
               DepthLayer == input$depth
             })
  })
  
  # Time series plot
  output$timePlot <- renderPlot({
    ggplot(filteredData(), aes(x = Date, y = .data[[input$parameter]])) +
      geom_line(color = "#0072B2") +
      geom_point(alpha = 0.6, size = 1.2) +
      labs(title = paste(input$parameter, "at", input$site),
           x = "Date", y = input$parameter) +
      theme_minimal(base_size = 14)
  })
  
  # Seasonal plot
  output$seasonPlot <- renderPlot({
    ggplot(filteredData(), aes(x = month(Date, label = TRUE), y = .data[[input$parameter]])) +
      geom_boxplot(fill = "#56B4E9") +
      labs(title = paste("Seasonal Patterns of", input$parameter, "at", input$site),
           x = "Month", y = input$parameter) +
      theme_minimal(base_size = 14)
  })
  
  # Leaflet map using awesomeIcons per site
  output$map <- renderLeaflet({
    # Define individual awesomeIcons for each site
    icon_list <- list(
      "MO1" = awesomeIcons(icon = "tint", iconColor = "white", library = "fa", markerColor = "orange"),
      "CUL1" = awesomeIcons(icon = "tint", iconColor = "white", library = "fa", markerColor = "blue"),
      "VBR1" = awesomeIcons(icon = "tint", iconColor = "white", library = "fa", markerColor = "green"),
      "PIER" = awesomeIcons(icon = "tint", iconColor = "white", library = "fa", markerColor = "red")
    )
    
    # Start base map
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE)) %>%
      addMiniMap(toggleDisplay = TRUE, position = "bottomright")
    
    # Add markers per site with corresponding icon
    for (site in names(icon_list)) {
      site_data <- filter(site_locations, Site == site)
      map <- map %>%
        addAwesomeMarkers(
          data = site_data,
          lng = ~Longitude,
          lat = ~Latitude,
          icon = icon_list[[site]],
          label = ~Site,
          popup = ~paste0(
            "<strong>Site:</strong> ", Site, "<br>",
            "<strong>Lat:</strong> ", round(Latitude, 6), "<br>",
            "<strong>Lon:</strong> ", round(Longitude, 6)
          )
        )
    }
    
    # Add legend conditionally
    if (input$showLegend) {
      map <- map %>%
        addLegend(
          position = "bottomright",
          colors = c("orange", "blue", "green", "red"),
          labels = c("MO1", "CUL1", "VBR1", "PIER"),
          title = "Monitoring Sites",
          opacity = 1
        )
    }
    
    map
  })
}

# Run app
shinyApp(ui = ui, server = server)
