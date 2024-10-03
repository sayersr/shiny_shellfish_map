library(shiny)
library(leaflet)
library(sf)
library(httr)
library(jsonlite)

ui <- fluidPage(
  titlePanel("GACU Shellfish GIS Layer Explorer"),
  sidebarLayout(
    sidebarPanel(
      textInput("feature_url", "Feature Service URL:", 
                value = "https://services6.arcgis.com/DZHaqZm9cxOD4CWM/ArcGIS/rest/services/Shellfish/FeatureServer/0"),
      textInput("where_clause", "Where Clause:", value = "1=1"),
      numericInput("max_features", "Max Features:", value = 1000, min = 1, max = 1000),
      sliderInput("opacity", "Layer Opacity:", min = 0, max = 1, value = 0.7, step = 0.1),
      textInput("search", "Search for a place:", value = ""),
      actionButton("go", "Search"),
      actionButton("refresh", "Refresh Data")
    ),
    mainPanel(
      leafletOutput("map"),
      verbatimTextOutput("service_info"),
      verbatimTextOutput("error_log")
    )
  )
)

server <- function(input, output, session) {
  # Function to fetch GeoJSON data from ArcGIS Feature Service
  fetch_geojson <- function(url, where_clause, max_features) {
    tryCatch({
      response <- GET(paste0(url, "/query"), query = list(
        where = where_clause,
        outFields = "*",
        returnGeometry = "true",
        f = "geojson",
        resultRecordCount = max_features
      ))
      
      if (status_code(response) == 200) {
        content <- content(response, "text", encoding = "UTF-8")
        parsed <- fromJSON(content, simplifyVector = FALSE)
        return(list(parsed = parsed, raw = content, error = NULL))
      } else {
        error_content <- content(response, "text", encoding = "UTF-8")
        stop(paste("HTTP error:", status_code(response), "\nResponse:", error_content))
      }
    }, error = function(e) {
      message("Error fetching data: ", e$message)
      list(parsed = NULL, raw = NULL, error = e$message)
    })
  }

  # Fetch data
  geojson_data <- reactiveVal(NULL)
  
  observeEvent(c(input$refresh, 1), {
    geojson_data(fetch_geojson(input$feature_url, input$where_clause, input$max_features))
  })
  
  # Convert GeoJSON to sf object
  shellfish_sf <- reactive({
    req(geojson_data())
    if (is.null(geojson_data()$parsed)) return(NULL)
    tryCatch({
      sf_object <- st_read(jsonlite::toJSON(geojson_data()$parsed), quiet = TRUE)
      if (nrow(sf_object) == 0) {
        return(NULL)
      }
      sf_object
    }, error = function(e) {
      message("Error converting GeoJSON to sf: ", e$message)
      NULL
    })
  })

  # Render map
  output$map <- renderLeaflet({
    if (is.null(shellfish_sf())) {
      leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 4)  # Center on USA
    } else {
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = shellfish_sf(), fillOpacity = input$opacity, weight = 1,
                    fillColor = "blue", color = "black", group = "Shellfish")
    }
  })

  # Update map when opacity changes
  observe({
    req(shellfish_sf())
    leafletProxy("map") %>%
      clearGroup("Shellfish") %>%
      addPolygons(data = shellfish_sf(), fillOpacity = input$opacity, weight = 1,
                  fillColor = "blue", color = "black", group = "Shellfish")
  })

  # Search functionality
  observeEvent(input$go, {
    req(input$search)
    geocode_url <- paste0("https://nominatim.openstreetmap.org/search?format=json&q=", URLencode(input$search))
    geocode_result <- fromJSON(geocode_url)
    
    if (length(geocode_result) > 0) {
      lat <- as.numeric(geocode_result$lat[1])
      lon <- as.numeric(geocode_result$lon[1])
      
      leafletProxy("map") %>%
        setView(lng = lon, lat = lat, zoom = 10) %>%
        addMarkers(lng = lon, lat = lat, popup = input$search)
    }
  })

  # Display service info
  output$service_info <- renderPrint({
    cat("Service Description: GACU's most commonly used GIS layers\n")
    cat("Service ItemId: f0468fb69aed423cbdb7fa8290e5adf5\n")
    cat("Has Versioned Data: false\n")
    cat("Max Record Count: 1000\n")
    cat("Supported query Formats: JSON\n")
    cat("Supports applyEdits with GlobalIds: False\n\n")
    
    if (is.null(geojson_data())) {
      cat("No data fetched yet. Click 'Refresh Data' to try again.\n")
    } else if (is.null(shellfish_sf())) {
      cat("Query successful, but error converting to sf object.\n")
      cat("Layer Name: Shellfish\n")
      cat("Number of Features in GeoJSON:", length(geojson_data()$parsed$features), "\n")
    } else {
      cat("Layer Name: Shellfish\n")
      cat("Geometry Type:", st_geometry_type(shellfish_sf()[1], by_geometry = FALSE), "\n")
      cat("Number of Features:", nrow(shellfish_sf()), "\n")
      cat("Number of Features in GeoJSON:", length(geojson_data()$parsed$features), "\n")
      
      if (length(geojson_data()$parsed$features) > 0) {
        cat("\nSample Feature Properties:\n")
        sample_properties <- geojson_data()$parsed$features[[1]]$properties
        str(sample_properties, max.level = 1, give.attr = FALSE)
      }
    }
  })

  # Error logging
  output$error_log <- renderPrint({
    if (is.null(geojson_data())) {
      cat("No data fetched yet. Click 'Refresh Data' to try again.\n")
    } else if (!is.null(geojson_data()$error)) {
      cat("Error fetching GeoJSON data:\n", geojson_data()$error, "\n")
    } else if (is.null(shellfish_sf())) {
      if (length(geojson_data()$parsed$features) == 0) {
        cat("Query successful, but no features returned. Try adjusting your query parameters.\n")
      } else {
        cat("Error converting GeoJSON to sf object. Details:\n")
        tryCatch({
          sf_object <- st_read(jsonlite::toJSON(geojson_data()$parsed), quiet = TRUE)
          cat("Conversion succeeded, but resulted in NULL. Check the structure of the GeoJSON.\n")
        }, error = function(e) {
          cat("Error message:", e$message, "\n")
        })
      }
    } else {
      cat("Data successfully loaded and processed.\n")
    }
  })
}

shinyApp(ui, server)
