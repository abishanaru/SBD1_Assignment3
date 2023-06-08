library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(rvest)
library(readr)
library(leaflet.extras)
library(DT)

# Funktion zum Scrapen der Daten von der Website
scrape_covid_data <- function() {
  url <- "https://www.worldometers.info/coronavirus/#main_table"
  page <- read_html(url)
  
  # Scrape the table containing coronavirus data
  table <- html_table(html_nodes(page, "table#main_table_countries_today"), header = TRUE)[[1]]
  
  # Die ersten 8 und die letzten 8 Zeilen beinhalten Angaben zu den Kontinenten,
  # die für die Kartenvisualisierung nicht benötigt werden.
  # Lösche die ersten 8 Zeilen
  table <- table[-c(1:8), ]
  
  # Lösche die letzten 8 Zeilen
  table <- table[-c((nrow(table) - 7):nrow(table)), ]
  
  # Speichere nur die benötigten Spalten
  table <- table[, c(2, 3, 5, 13, 15)]
  
  # Funktion zum Entfernen von Tausendertrennzeichen aus Zeichenketten
  removeCommas <- function(x) {
    gsub(",", "", x)
  }
  
  # Spaltenindizes für die Umwandlung in numerische Werte
  colIndices <- c(2, 3, 4, 5)
  
  # Entferne Kommas und wandele in numerische Werte um
  for (colIndex in colIndices) {
    table[[colIndex]] <- as.numeric(removeCommas(table[[colIndex]]))
  }
  
  # Change the name of the column
  colnames(table)[1] <- "country"
  colnames(table)[2] <- "TotalCases"
  colnames(table)[3] <- "TotalDeaths"
  colnames(table)[4] <- "TotalTests"
  colnames(table)[5] <- "Population"
  
  # Rückgabe der Daten
  table
}

# Funktion für Geodaten
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
      menuItem("Karte", tabName = "map_tab"),
      menuItem("Heatmap", tabName = "heatmap_tab")
    )
  ),
  dashboardBody(
    tabItems(
      # Karte Tab
      tabItem(
        tabName = "map_tab",
        fluidRow(
          box(
            title = "Ausgewählte Länder",
            width = 6,
            pickerInput(
              inputId = "selected_countries",
              choices = NULL,
              options = list(`actions-box` = TRUE),
              multiple = TRUE
            )
          ),
          box(
            title = "Ausgewählter Wert",
            width = 6,
            selectInput(
              inputId = "selected_column",
              label = "Spalte auswählen",
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
            title = "Statistik",
            width = 6,
            valueBoxOutput("selected_countries_count")
          ),
          box(
            title = "Mittelwert",
            width = 6,
            valueBoxOutput("average_value")
          ),
          box(
            title = "Maximaler Wert",
            width = 6,
            valueBoxOutput("max_value")
          ),
          box(
            title = "Minimaler Wert",
            width = 6,
            valueBoxOutput("min_value")
          )
        )
      ),
      # Heatmap Tab
      tabItem(
        tabName = "heatmap_tab",
        fluidRow(
          box(
            title = "Ausgewählter Wert",
            width = 6,
            selectInput(
              inputId = "selected_column_heatmap",
              label = "Spalte auswählen",
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
  
  # Update der Dropdown-Menüs basierend auf den Daten
  observe({
    merged_data <- data()
    
    # Dropdown-Menü für ausgewählte Länder (Karte Tab)
    updatePickerInput(
      session = session,
      inputId = "selected_countries",
      choices = unique(merged_data$country),
      selected = unique(merged_data$country)
    )
  })
  
  # Leaflet-Karte (Karte Tab)
  output$map <- renderLeaflet({
    merged_data <- data()
    
    # Filtern der Daten basierend auf den ausgewählten Ländern
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    # Leaflet-Karte erstellen
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = selected_data,
        lat = ~latitude,
        lng = ~longitude,
        label = ~paste0(input$selected_column, ": ", get(input$selected_column)),
        popup = ~paste0("<b>Land:</b> ", country, "<br>",
                        "<b>", input$selected_column, ":</b> ", get(input$selected_column)),
        color = "red",
        fillOpacity = 0.7
      )
  })
  
  # Anzahl der ausgewählten Länder anzeigen (Statistik Tab)
  output$selected_countries_count <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    valueBox(
      value = length(input$selected_countries),
      subtitle = "Ausgewählte Länder",
      icon = icon("globe"),
      color = "yellow"
    )
  })
  
  # Mittelwert der Werte der ausgewählten Länder anzeigen (Statistik Tab)
  output$average_value <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    valueBox(
      value = round(mean(selected_data[[input$selected_column]], na.rm = TRUE), 2),
      subtitle = "Mittelwert",
      icon = icon("calculator"),
      color = "green"
    )
  })
  
  # Maximaler Wert und Name des Landes anzeigen (Statistik Tab)
  output$max_value <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    max_value <- max(selected_data[[input$selected_column]], na.rm = TRUE)
    country_with_max_value <- selected_data$country[selected_data[[input$selected_column]] == max_value]
    
    valueBox(
      value = max_value,
      subtitle = paste("Maximaler Wert (", country_with_max_value, ")"),
      icon = icon("arrow-up"),
      color = "red"
    )
  })
  
  # Minimaler Wert und Name des Landes anzeigen (Statistik Tab)
  output$min_value <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    

    min_value <- min(selected_data[[input$selected_column]], na.rm = TRUE)
    
    country_with_min_value <- selected_data$country[selected_data[[input$selected_column]] == min_value]
    
    valueBox(
      value = min_value,
      subtitle = paste("Minimaler Wert (", country_with_min_value, ")"),
      icon = icon("arrow-down"),
      color = "blue"
    )
  })
  
  # Kreise mit variabler Farbe und Größe erstellen (Heatmap Tab)
  output$heatmap <- renderLeaflet({
    merged_data <- data()
    
    # Filtern der Daten basierend auf der ausgewählten Spalte
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    # Normalisieren der Werte für Farbe und Größe
    values <- selected_data[[input$selected_column]]
    values <- na.omit(values)  # Entfernen der fehlenden Werte
    normalized_values <- scales::rescale(values, to = c(0, 1))
    
    # Manuelle Festlegung des Wertebereichs für die Farbpalette
    min_value <- min(values, na.rm = TRUE)
    max_value <- max(values, na.rm = TRUE)
    
    # Farbpalette definieren
    color_palette <- colorNumeric(
      palette = c("orange", "red"),  # Farbpalette anpassen
      domain = c(min_value, max_value)
    )
    
    # Leaflet-Karte erstellen
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
        label = ~paste0(input$selected_column, ": ", get(input$selected_column))
      )
  })

  
  # Tabelle mit den Top 10 Werten erstellen (Heatmap Tab)
  output$top_values_table <- DT::renderDataTable({
    merged_data <- data()
    
    # Sortieren der Daten nach der ausgewählten Spalte in absteigender Reihenfolge
    sorted_data <- merged_data[order(merged_data[[input$selected_column_heatmap]], decreasing = TRUE), ]
    
    # Begrenzen auf die Top 10 Werte
    top_values <- head(sorted_data, 10)
    
    # DataTable-Objekt erstellen
    datatable(top_values, options = list(pageLength = 10))
  })
}

# App starten
shinyApp(ui, server)
