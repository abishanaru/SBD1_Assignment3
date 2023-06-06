library(shiny)
library(shinydashboard)
library(leaflet)
library(rvest)
library(readr)
library(tidyverse)
library(htmlwidgets)
library(leaflet.extras)

# Funktion zum Scrapen der Daten von der Website
scrape_covid_data <- function() {
  url <- "https://www.worldometers.info/coronavirus/#main_table"
  page <- read_html(url)
  
  # Scrape the table containing coronavirus data
  table <- html_table(html_nodes(page, "table#main_table_countries_today"), header = TRUE)[[1]]
  
  # Die ersten 9 und die letzten 8 Zeilen beinhalten Angaben zu den Kontinenten,
  # die für die Kartenvisualisierung nicht benötigt werden.
  # Lösche die ersten 9 Zeilen
  table <- table[-c(1:9), ]
  
  # Lösche die letzten 8 Zeilen
  table <- table[-c((nrow(table) - 7):nrow(table)), ]
  
  # Speichere nur die benötigten Spalten
  table <- table[, c(2, 3, 5)]
  
  # Konvertiere die ausgewählte Spalte in numerische Werte
  table[, 2] <- lapply(table[, 2], function(x) {
    # Remove commas from values
    x <- gsub(",", "", x)
    ifelse(is.na(as.numeric(x)), x, as.numeric(x))
  })
  
  # Convert TotalDeaths column to numeric
  table$TotalDeaths <- as.numeric(gsub(",", "", table$TotalDeaths, fixed = TRUE))
  
  # Replace invalid numeric values with NA
  table$TotalDeaths[is.na(table$TotalDeaths) | is.infinite(table$TotalDeaths)] <- NA
  
  # Change the name of the first column
  colnames(table)[1] <- "country"
  
  # Rückgabe der Daten
  table
}

getgeodata <- function() {
  # Quelle: https://www.kaggle.com/datasets/paultimothymooney/latitude-and-longitude-for-every-country-and-state
  # Geo Daten Importieren
  geodata <- read_csv("world_country_and_usa_states_latitude_and_longitude_values.csv")
  geodata <- geodata[, c("country", "latitude", "longitude")]
  # Rückgabe der Daten
  geodata
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        h2("COVID-19 Dashboard"),
        selectInput("value", "Werte anzeigen:", choices = c("TotalCases", "TotalDeaths")),
        leafletOutput("map")
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Reaktive Funktion zum Scrapen der Daten
  data <- reactive({
    table <- scrape_covid_data()
    geodata <- getgeodata()
    
    # Verbinde die Tabellen basierend auf der "country"-Spalte
    merged_data <- merge(table, geodata, by = "country", all.x = TRUE)
    
    # Entferne Zeilen mit NA-Werten
    merged_data <- na.omit(merged_data)
    
    # Rückgabe der kombinierten Daten
    merged_data
  })
  
  # Leaflet-Karte mit COVID-19-Daten
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addCircleMarkers(
        data = data(),
        lng = ~longitude,  # Verwende Längengrad aus kombinierten Daten
        lat = ~latitude,  # Verwende Breitengrad aus kombinierten Daten
        radius = 10,
        color = "#FF0000",
        fillOpacity = 0.7,
        label = NULL,  # Setze das Label anfangs auf NULL
        labelOptions = labelOptions(noHide = TRUE, riseOnHover = TRUE, riseOffset = 300)
      )
  })
  
  # Anpassung des Popups
  observeEvent(input$map_marker_click, {
    event <- input$map_marker_click
    if (!is.null(event)) {
      popup_content <- paste0("<strong>", event$popup$country, "</strong><br>",
                              "Total ", input$value, ": ", event$popup[[input$value]], sep = " ")
      leafletProxy("map") %>%
        clearPopups() %>%
        addPopups(
          lng = event$lng,
          lat = event$lat,
          options = popupOptions(closeButton = FALSE),
          popup = HTML(popup_content)
        )
    }
  })
  
}

# App starten
shinyApp(ui, server)
