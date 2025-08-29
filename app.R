# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(leaflet)
library(bslib)

# Load and clean data
df <- read_csv("data/water_quality_data.csv", show_col_types = FALSE) %>%
  rename(
    Date = "Date",
    Site = "Site",
    Depth = "Depth (cm)",
    Temperature = "Temperature (C)",
    Salinity = "Salinity (ppt)",
    DO = "Dissolved Oxygen (mg/L)",
    Turbidity = "Turbidity (cm)",
    Conductivity = "Conductivity-specific (mS/cm)",
    DO_percent = "Dissolved Oxygen (%)"
  ) %>%
  mutate(
    Date = mdy(Date),
    Depth = as.numeric(Depth),
    Temperature = as.numeric(Temperature),
    Salinity = as.numeric(Salinity),
    DO = as.numeric(DO),
    Turbidity = as.numeric(Turbidity),
    Conductivity = as.numeric(Conductivity),
    DO_percent = as.numeric(DO_percent),
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

# Pretty labels -> internal names
param_choices <- c(
  "Temperature (C)"               = "Temperature",
  "Salinity (ppt)"                = "Salinity",
  "Dissolved Oxygen (mg/L)"       = "DO",
  "Turbidity (cm)"                = "Turbidity",
  "Conductivity-specific (mS/cm)" = "Conductivity",
  "Dissolved Oxygen (%)"          = "DO_percent"
)

# Site coordinates
site_locations <- tibble::tibble(
  Site = c("MO1", "CUL1", "VBR1", "PIER"),
  Latitude = c(34.410235, 34.413928, 34.417485, 34.411906),
  Longitude = c(-119.878968, -119.873961, -119.874138, -119.877158)
)

# --- UCSB brand theme ---
ucsb_theme <- bs_theme(
  version = 5,
  primary   = "#003660",  # UCSB Navy
  secondary = "#FEBC11",  # UCSB Gold
  base_font    = font_google("Nunito Sans"),
  heading_font = font_google("Nunito Sans")
)

# UCSB CSS (+ colorful overview cards)
ucsb_css <- HTML("
  body { font-family: 'Nunito Sans', sans-serif; color: #003660; }
  h1, h2, h3 { font-weight: 700; color: #003660; }
  h1, h2 { border-bottom: 3px solid #FEBC11; padding-bottom: 5px; margin-bottom: 15px; }
  .nav-tabs > li > a { font-weight: 600; }
  .nav-tabs > li.active > a { background-color: #003660 !important; color: #fff !important; }

  /* Sidebar styling */
  .ucsb-sidebar {
    background: #F5F9FC; border: 1px solid #DCE7F3; border-left: 6px solid #FEBC11;
    border-radius: 8px; padding: 12px 16px; margin-bottom: 10px;
  }
  .ucsb-sidebar .ucsb-sidebar-header {
    font-weight: 800; color: #003660; text-transform: uppercase;
    letter-spacing: 0.5px; margin-bottom: 10px;
  }
  .ucsb-sidebar .control-label, .ucsb-sidebar label { color: #003660; font-weight: 600; }
  .ucsb-sidebar .form-control, .ucsb-sidebar .form-select { border-color: rgba(0,54,96,0.25); }
  .ucsb-sidebar .btn-primary { background-color: #003660; border-color: #003660; }
  .ucsb-sidebar .irs-bar, .ucsb-sidebar .irs-single, .ucsb-sidebar .irs-handle > i:first-child {
    background: #003660; border-color: #003660;
  }
  .ucsb-sidebar .irs-line { background: #DCE7F3; }

  /* Badges */
  .badge {
    display: inline-block; padding: 6px 10px; margin: 4px 6px 0 0; font-weight: 700;
    border-radius: 999px; background: #003660; color: #fff;
  }
  .badge.badge-gold { background: #FEBC11; color: #003660; }

  /* Data notes */
  .data-notes {
    background: #F5F9FC; border-left: 5px solid #FEBC11; padding: 10px 15px;
    margin-top: 20px; border-radius: 6px; font-size: 0.95em;
  }

  /* Overview: hero banner + info cards */
  .hero {
    background: #003660;
    color: #ffffff;
    padding: 16px 18px;
    border-radius: 8px;
    margin-bottom: 16px;
  }
  .hero h3 { color: #ffffff; border: none; margin: 0 0 6px 0; }
  .info-card {
    border-radius: 8px; padding: 12px 14px; margin-bottom: 10px;
    border: 1px solid #DCE7F3;
  }
  .info-card.gold { background: #FFF6D6; border-left: 6px solid #FEBC11; }
  .info-card.blue { background: #F0F6FB; border-left: 6px solid #003660; }
")

# UI
ui <- tagList(
  fluidPage(
    theme = ucsb_theme,
    tags$head(tags$style(ucsb_css)),
    titlePanel("Explore Water Quality Trends"),
    
    tabsetPanel(
      # --- Overview (COPR / Devereux Slough) ---
      tabPanel("Overview",
               br(),
               div(class = "hero",
                   h3("Coal Oil Point Reserve — Devereux Slough Water Quality Monitoring"),
                   div("An interactive dashboard to explore long-term monitoring data and seasonal patterns across multiple sites and depths.")
               ),
               fluidRow(
                 column(
                   7,
                   div(class = "info-card gold",
                       h4("What you can do"),
                       tags$ul(
                         tags$li("Analyze parameters with units: Temperature (C), Salinity (ppt), Dissolved Oxygen (mg/L, %), Turbidity (cm), Conductivity-specific (mS/cm)."),
                         tags$li("Filter by site, depth layer (or fixed pier depth), years, and months."),
                         tags$li("Switch the map between Street and Satellite views; inspect popups for coordinates.")
                       )
                   )
                 ),
                 column(
                   5,
                   div(class = "info-card blue",
                       h4("At a glance"),
                       div(class = "badge", "Sites: MO1"),
                       div(class = "badge", "CUL1"),
                       div(class = "badge", "VBR1"),
                       div(class = "badge", "PIER"),
                       br(),
                       div(class = "badge badge-gold", "Surface & Bottom (non-PIER)"),
                       div(class = "badge badge-gold", "Fixed depths at PIER")
                   )
                 )
               ),
               br(),
               h4("How to use this app"),
               tags$ol(
                 tags$li(strong("Trends → Time Series:"), " select a parameter to view changes through time at a site and depth."),
                 tags$li(strong("Trends → Seasonal Patterns:"), " compare monthly distributions across years."),
                 tags$li(strong("Map:"), " locate monitoring sites and toggle basemaps.")
               ),
               br(),
               h4("Data Notes"),
               div(class = "data-notes",
                   paste0("Dataset covers ",
                          format(min(df$Date, na.rm = TRUE), "%B %Y"), " to ",
                          format(max(df$Date, na.rm = TRUE), "%B %Y"),
                          " (", nrow(df), " observations across ",
                          length(unique(df$Site)), " sites).")
               )
      ),
      
      tabPanel("Trends",
               sidebarLayout(
                 sidebarPanel(
                   div(class = "ucsb-sidebar",
                       div(class = "ucsb-sidebar-header", "Filters"),
                       selectInput("site", "Monitoring Site:", choices = unique(df$Site)),
                       uiOutput("depthSelector"),
                       selectInput("parameter", "Parameter:", choices = param_choices),
                       sliderInput("yearRange", "Year Range:",
                                   min = min(df$Year, na.rm = TRUE),
                                   max = max(df$Year, na.rm = TRUE),
                                   value = c(min(df$Year, na.rm = TRUE), max(df$Year, na.rm = TRUE)),
                                   sep = ""
                       ),
                       sliderInput("monthRange", "Months:", min = 1, max = 12, value = c(1, 12), step = 1),
                       br(),
                       downloadButton("download_csv", "Download filtered data (CSV)", class = "btn-primary")
                   )
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Time Series", plotOutput("timePlot")),
                     tabPanel("Seasonal Patterns", plotOutput("seasonPlot"))
                   )
                 )
               ),
               # --- UCSB Footer ---
               div(class = "ucsb-footer",
                   "Coal Oil Point Reserve • UCSB • Devereux Slough Monitoring"
               )
      ),
      
      # --- Map Tab ---
      tabPanel("Map",
               br(),
               h3("Monitoring Site Locations"),
               checkboxInput("showLegend", "Show Legend", value = TRUE),
               leafletOutput("map", height = "600px"),
               
               # --- UCSB Footer ---
               div(class = "ucsb-footer",
                   "Coal Oil Point Reserve • UCSB • Devereux Slough Monitoring"
               )

      ),
      
      # --- Methods & FAQ ---
      tabPanel("Methods & FAQ",
               br(),
               h3("Methods & Frequently Asked Questions"),
               tags$details(
                 tags$summary("Sampling design"),
                 p("Sites include MO1, CUL1, VBR1, and PIER within the Devereux Slough system. Non-pier sites are summarized by ",
                   em("Surface (≤20 cm)"),
                   " and ",
                   em("Bottom (>20 cm)"),
                   " depth layers. PIER measurements use fixed depths (e.g., 10–250 cm).")
               ),
               tags$details(
                 tags$summary("Parameters & units"),
                 tags$ul(
                   tags$li("Temperature (C) — water temperature in degrees Celsius."),
                   tags$li("Salinity (ppt) — practical salinity in parts per thousand."),
                   tags$li("Dissolved Oxygen (mg/L) — concentration by mass."),
                   tags$li("Dissolved Oxygen (%) — percent saturation."),
                   tags$li("Turbidity (cm) — Secchi tube (higher cm = clearer water)."),
                   tags$li("Conductivity-specific (mS/cm) — temperature-corrected conductivity.")
                 )
               ),
               tags$details(
                 tags$summary("Seasonality vs. interannual trends"),
                 p("Use the Time Series view to explore long-term changes at a site/depth, and Seasonal Patterns to compare distributions by month across years.")
               ),
               tags$details(
                 tags$summary("Data quality & caveats"),
                 p("Values are plotted as provided after basic type-cleaning. Consider calibration records, instrument changes, and field conditions when interpreting extremes."),
                 p("Filters (years/months) apply to both plots and may change distributions.")
               ),
               tags$details(
                 tags$summary("Contact & attribution"),
                 p("Developed for the Coal Oil Point Reserve (UCSB). For questions or to report issues, please contact the COPR team via email: https://copr.nrs.ucsb.edu/contact/.")
               )
      )
    )
  ),
  
  # --- Footer with logos ---
  div(
    style = "text-align: center; padding: 20px; border-top: 1px solid #ccc; margin-top: 30px;",
    img(src = "nrs_logo.png", height = "60px", style = "margin: 10px;"),
    img(src = "COPR_logo.png", height = "60px", style = "margin: 10px;"),
    img(src = "ucsb_logo.png", height = "60px", style = "margin: 10px;")
  )
)

# Server
server <- function(input, output, session) {
  # Label helper for plots (unitized)
  param_label <- reactive({
    names(param_choices)[match(input$parameter, param_choices)]
  })
  
  # Dynamic depth input
  output$depthSelector <- renderUI({
    if (input$site == "PIER") {
      selectInput("depth", "Select Pier Depth (cm):",
                  choices = c(10, 50, 100, 150, 200, 250), selected = 50
      )
    } else {
      selectInput("depth", "Select Depth Layer:", choices = c("Surface", "Bottom"))
    }
  })
  
  # Reactive filtered data
  filteredData <- reactive({
    df %>%
      filter(
        Site == input$site,
        Year >= input$yearRange[1],
        Year <= input$yearRange[2],
        Month >= input$monthRange[1],
        Month <= input$monthRange[2],
        if (input$site == "PIER") Depth == as.numeric(input$depth) else DepthLayer == input$depth
      )
  })
  
  # Download filtered CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("devereux_filtered_",
             input$site, "_",
             input$parameter, "_",
             input$yearRange[1], "-", input$yearRange[2], ".csv")
    },
    content = function(file) {
      readr::write_csv(filteredData(), file, na = "")
    }
  )
  
  # Time series plot (UCSB Navy)
  output$timePlot <- renderPlot({
    ggplot(filteredData(), aes(x = Date, y = .data[[input$parameter]])) +
      geom_line(color = "#003660") +
      geom_point(alpha = 0.6, size = 1.2, color = "#003660") +
      labs(title = paste(param_label(), "at", input$site),
           x = "Date", y = param_label()) +
      theme_minimal(base_size = 14)
  })
  
  # Seasonal plot (UCSB Gold)
  output$seasonPlot <- renderPlot({
    ggplot(filteredData(), aes(x = month(Date, label = TRUE), y = .data[[input$parameter]])) +
      geom_boxplot(fill = "#FEBC11") +
      labs(title = paste("Seasonal Patterns of", param_label(), "at", input$site),
           x = "Month", y = param_label()) +
      theme_minimal(base_size = 14)
  })
  
  # Leaflet map with basemap toggle
  output$map <- renderLeaflet({
    icon_list <- list(
      "MO1" = awesomeIcons(icon = "tint", iconColor = "white", library = "fa", markerColor = "orange"),
      "CUL1" = awesomeIcons(icon = "tint", iconColor = "white", library = "fa", markerColor = "blue"),
      "VBR1" = awesomeIcons(icon = "tint", iconColor = "white", library = "fa", markerColor = "green"),
      "PIER" = awesomeIcons(icon = "tint", iconColor = "white", library = "fa", markerColor = "red")
    )
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("Street Map", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE)) %>%
      addMiniMap(toggleDisplay = TRUE, position = "bottomright")
    
    for (site in names(icon_list)) {
      site_data <- dplyr::filter(site_locations, Site == site)
      map <- map %>%
        addAwesomeMarkers(
          data = site_data,
          lng = ~Longitude, lat = ~Latitude,
          icon = icon_list[[site]],
          label = ~Site,
          popup = ~paste0(
            "<strong>Site:</strong> ", Site, "<br>",
            "<strong>Lat:</strong> ", round(Latitude, 6), "<br>",
            "<strong>Lon:</strong> ", round(Longitude, 6)
          )
        )
    }
    
    if (input$showLegend) {
      map <- map %>%
        addLegend(position = "bottomright",
                  colors = c("orange", "blue", "green", "red"),
                  labels = c("MO1", "CUL1", "VBR1", "PIER"),
                  title = "Monitoring Sites", opacity = 1)
    }
    map
  })
}

# Run app
shinyApp(ui = ui, server = server)