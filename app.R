# app HI its sam
# needed libraries
library(shiny)

# shiny manager only needed if we use the log in authentication 
library(shinymanager)
library(googlesheets4)
library(dplyr)
library(leaflet)
library(DT)
library(bslib)

theme = bs_theme(
  bootswatch = 'flatly',
  primary = "#0077b6",   # water blue
  secondary = "#90e0ef", # aqua
  success = "#ffb703",   # sand yellow
  font_scale = 1.1
)

# Google Sheets URL links
# water_url is the monitoring data google sheet.
water_url <- "https://docs.google.com/spreadsheets/d/1LaykUvvC-X_0qoWZECgNDUoqTZga9fd3RvZvrvK6Ilo/edit?usp=sharing"
# wse_url is the Devereux Slough WSE Staff Gauge Record
wse_url   <- "https://docs.google.com/spreadsheets/d/15aaqUO3jvyxnQ63wGaMqlPKpvB9fArSAJatNZyZu_eU/edit?usp=sharing"

ui <- navbarPage(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#0077b6", 
    secondary = "#90e0ef", 
    success = "#ffb703",
    font_scale = 1.1
  ),
  "Devereux Slough Water Monitoring",
  
  # Overview Tab
  tabPanel("Overview",
           h2("About This App"),
           p("This dashboard displays real-time water monitoring data for Devereux Slough at Coal Oil Point Reserve."),
           p("Data updates automatically from Google Sheets."),
           p("Use the Map, Trends, and Data tabs to explore the information.")
  ),
  
  # Map Tab
  tabPanel("Map",
           sidebarLayout(
             sidebarPanel(
               dateRangeInput("date_range", "Select Date Range"),
               selectInput("site", "Select Site", choices = NULL),
               selectInput("parameter", "Select Parameter", 
                           choices = c("Temperature", "Salinity", "pH", "Dissolved_Oxygen"))
             ),
             mainPanel(
               leafletOutput("map", height = "600px")
             )
           )
  ),
  
  # Trends Tab
  tabPanel("Trends",
           plotOutput("time_series")
  ),
  
  # Data Table Tab
  tabPanel("Data Table",
           DTOutput("data_table")
  ),
  
  # Data Entry Tab
  tabPanel("Data Entry",
           h3("Add New Reading"),
           selectInput("entry_site", "Site", choices = NULL),
           selectInput("entry_param", "Parameter", choices = c("Temperature", "Salinity", "pH", "Dissolved_Oxygen")),
           numericInput("entry_value", "Value", value = NA),
           actionButton("submit_entry", "Submit Data")
  )
)

server <- function(input, output, session) {
# Log in authentication
res_auth <- secure_server(check_credentials = check_credentials(credentials))
# Reactive: Load live data from Google Sheets
 water_data <- reactive({
    req(water_url)
    df <- read_sheet(water_url)
    df <- df %>% mutate(Date = as.Date(Date))
    df
  })
  
# Update site choices and date range dynamically
  observe({
    sites <- unique(water_data()$Site)
    updateSelectInput(session, "site", choices = sites)
    updateSelectInput(session, "entry_site", choices = sites)
    
    updateDateRangeInput(session, "date_range",
                         start = min(water_data()$Date, na.rm = TRUE),
                         end   = max(water_data()$Date, na.rm = TRUE))
  })
  
# Filtered data based on user input
  filtered_data <- reactive({
    req(input$site, input$date_range)
    water_data() %>%
      filter(Site == input$site,
             Date >= input$date_range[1],
             Date <= input$date_range[2])
  })

# Map
  output$map <- renderLeaflet({
    df <- filtered_data()
    req(nrow(df) > 0) # make sure there is data
    leaflet(df) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        color = "blue",
        popup = ~paste0(
          "Date: ", Date,
          "<br>Parameter: ", input$parameter,
          "<br>Value: ", get(input$parameter)
        )
      )
  })

# Time series plot
  output$time_series <- renderPlot({
    df <- filtered_data()
    req(input$parameter, nrow(df) > 0)
    plot(df$Date, df[[input$parameter]], type = "l",
         xlab = "Date", ylab = input$parameter,
         main = paste(input$parameter, "over Time"))
  })
  
# Data table
  output$data_table <- renderDT({
    df <- filtered_data()
    datatable(df)
  })
  
# Data entry to Google Sheets
  observeEvent(input$submit_entry, {
    req(input$entry_site, input$entry_param, input$entry_value)
    new_row <- data.frame(
      Date = Sys.Date(),
      Site = input$entry_site,
      Parameter = input$entry_param,
      Value = input$entry_value
    )
    sheet_append(water_url, new_row)
    showNotification("Data successfully added to Google Sheet.", type = "message")
  })
}

shinyApp(ui = ui, server = server)

