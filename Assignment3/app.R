library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(rvest)
library(readr)
library(leaflet.extras)
library(DT)

# Function for scraping data from the website
scrape_covid_data <- function() {
  url <- "https://www.worldometers.info/coronavirus/#main_table"
  page <- read_html(url)
  
  # Scrape the table containing coronavirus data
  table <- html_table(html_nodes(page, "table#main_table_countries_today"), header = TRUE)[[1]]
  
  # The first 8 and the last 8 rows contain information about continents,
  # which is not needed for the map visualization.
  # Remove the first 8 rows
  table <- table[-c(1:8), ]
  
  # Remove the last 8 rows
  table <- table[-c((nrow(table) - 7):nrow(table)), ]
  
  # Keep only the necessary columns
  table <- table[, c(2, 3, 5, 13, 15)]
  
  # Function to remove thousands separators from character strings
  removeCommas <- function(x) {
    gsub(",", "", x)
  }
  
  # Column indices for conversion to numeric values
  colIndices <- c(2, 3, 4, 5)
  
  # Remove commas and convert to numeric values
  for (colIndex in colIndices) {
    table[[colIndex]] <- as.numeric(removeCommas(table[[colIndex]]))
  }
  
  # Change the name of the column
  colnames(table)[1] <- "country"
  colnames(table)[2] <- "TotalCases"
  colnames(table)[3] <- "TotalDeaths"
  colnames(table)[4] <- "TotalTests"
  colnames(table)[5] <- "Population"
  
  # Return the data
  table
}

# Function for geodata
getgeodata <- function() {
  # Source: https://www.kaggle.com/datasets/paultimothymooney/latitude-and-longitude-for-every-country-and-state
  # Import Geo data
  geodata <- read_csv("world_country_and_usa_states_latitude_and_longitude_values.csv")
  geodata <- geodata[, c("country", "latitude", "longitude")]
  # Return the data
  geodata
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Karte", tabName = "map_tab"),
      menuItem("Heatmap", tabName = "heatmap_tab")
    )
  ),
  dashboardBody(
    tabItems(
      # Map Tab
      tabItem(
        tabName = "map_tab",
        fluidRow(
          box(
            title = "Selected countries",
            width = 6,
            pickerInput(
              inputId = "selected_countries",
              choices = NULL,
              options = list(`actions-box` = TRUE),
              multiple = TRUE
            )
          ),
          box(
            title = "Selected value",
            width = 6,
            selectInput(
              inputId = "selected_column",
              label = "Select column",
              choices = c("TotalCases", "TotalDeaths", "TotalTests", "Population")
            )
          )
        ),
        fluidRow(
          box(
            title = "Karte",
            width = 12,
            leafletOutput("map")
          )
        ),
        fluidRow(
          box(
            title = "Number of countries",
            width = 6,
            valueBoxOutput("selected_countries_count", width = 12)
          ),
          box(
            title = "Mean value",
            width = 6,
            valueBoxOutput("average_value", width = 12)
          ),
          box(
            title = "Maximum value",
            width = 6,
            valueBoxOutput("max_value", width = 12)
          ),
          box(
            title = "Minimum value",
            width = 6,
            valueBoxOutput("min_value", width = 12)
          )
        )
      ),
      # Heatmap Tab
      tabItem(
        tabName = "heatmap_tab",
        fluidRow(
          box(
            title = "Selected value",
            width = 6,
            selectInput(
              inputId = "selected_column_heatmap",
              label = "Select column",
              choices = c("TotalCases", "TotalDeaths", "TotalTests", "Population")
            )
          )
        ),
        fluidRow(
          box(
            title = "Heatmap",
            width = 12,
            leafletOutput("heatmap")
          )
        ),
        fluidRow(
          box(
            title = "Top 10 Werte",
            width = 12,
            dataTableOutput("top_values_table")
          )
        )
      )
    )
  ),
  skin = "yellow"
)

# Server
server <- function(input, output, session) {
  # Reactive function for scraping data
  data <- reactive({
    table <- scrape_covid_data()
    geodata <- getgeodata()
    
    # Merge the tables based on the "country" column
    merged_data <- merge(table, geodata, by = "country", all.x = TRUE)
    
    # Remove rows with NA values
    merged_data <- na.omit(merged_data)
    
    # Return the merged data
    merged_data
  })
  
  # Update dropdown menus based on the data
  observe({
    merged_data <- data()
    
    # Dropdown menu for selected countries (Map Tab)
    updatePickerInput(
      session = session,
      inputId = "selected_countries",
      choices = unique(merged_data$country),
      selected = unique(merged_data$country)
    )
  })
  
  # Leaflet map (Map Tab)
  output$map <- renderLeaflet({
    merged_data <- data()
    
    # Filter the data based on the selected countries
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    # Create Leaflet map
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = selected_data,
        lat = ~latitude,
        lng = ~longitude,
        label = ~paste0(input$selected_column, ": ", get(input$selected_column)),
        popup = ~paste0("<b>Country:</b> ", country, "<br>",
                        "<b>", input$selected_column, ":</b> ", get(input$selected_column)),
        color = "red",
        fillOpacity = 0.7
      )
  })
  
  # Show the number of selected countries (Statistics Tab)
  output$selected_countries_count <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    valueBox(
      value = length(input$selected_countries),
      subtitle = "Selected countries",
      icon = icon("globe"),
      color = "yellow"
    )
  })
  
  # Show the average value of the selected countries (Statistics Tab)
  output$average_value <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    valueBox(
      value = round(mean(selected_data[[input$selected_column]], na.rm = TRUE), 2),
      subtitle = "Mean value",
      icon = icon("calculator"),
      color = "green"
    )
  })
  
  # Show the maximum value and the country name (Statistics Tab)
  output$max_value <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    max_value <- max(selected_data[[input$selected_column]], na.rm = TRUE)
    country_with_max_value <- selected_data$country[selected_data[[input$selected_column]] == max_value]
    
    valueBox(
      value = max_value,
      subtitle = paste("Maximum value (", country_with_max_value, ")"),
      icon = icon("arrow-up"),
      color = "red"
    )
  })
  
  # Show the minimum value and the country name (Statistics Tab)
  output$min_value <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    

    min_value <- min(selected_data[[input$selected_column]], na.rm = TRUE)
    
    country_with_min_value <- selected_data$country[selected_data[[input$selected_column]] == min_value]
    
    valueBox(
      value = min_value,
      subtitle = paste("Minimum value (", country_with_min_value, ")"),
      icon = icon("arrow-down"),
      color = "blue"
    )
  })
  
  # Create circles with variable color and size (Heatmap Tab)
  output$heatmap <- renderLeaflet({
    merged_data <- data()
    
    # Filter the data based on the selected column
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    # Normalize the values for color and size
    values <- selected_data[[input$selected_column_heatmap]]
    values <- na.omit(values)  # Entfernen der fehlenden Werte
    normalized_values <- scales::rescale(values, to = c(0, 1))
    
    # Manually define the value range for the color palette
    min_value <- min(values, na.rm = TRUE)
    max_value <- max(values, na.rm = TRUE)
    
    # Define the color palette
    color_palette <- colorNumeric(
      palette = c("orange", "red"),  # Farbpalette anpassen
      domain = c(min_value, max_value)
    )
    
    # Create Leaflet map
    leaflet(data = selected_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~latitude,
        lng = ~longitude,
        radius = ~sqrt(normalized_values) * 10,  # Größe der Kreise anpassen
        fillColor = ~color_palette(values),  # Farbe der Kreise anpassen
        color = "red",
        fillOpacity = 0.8,
        stroke = FALSE,
        label = ~paste0(input$selected_column_heatmap, ": ", get(input$selected_column_heatmap))
      )
  })

  # Create a table with the top 10 values (Heatmap Tab)
  output$top_values_table <- DT::renderDataTable({
    merged_data <- data()
    
    # Sort the data based on the selected column in descending order
    sorted_data <- merged_data[order(merged_data[[input$selected_column_heatmap]], decreasing = TRUE), ]
    
    # Limit to the top 10 values
    top_values <- head(sorted_data, 10)
    
    # Create DataTable object
    datatable(top_values, options = list(pageLength = 10))
  })
}

# Start the app
shinyApp(ui, server)
