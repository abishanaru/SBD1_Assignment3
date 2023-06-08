library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)


# Funktion zum Scrapen der Daten von der Website
scrape_covid_data <- function() {
  url <- "https://www.worldometers.info/coronavirus/#main_table"
  page <- read_html(url)
  
  # Scrape the table containing coronavirus data
  table <- html_table(html_nodes(page, "table#main_table_countries_today"), header = TRUE)[[1]]
  
  # Die ersten 9 und die letzten 8 Zeilen beinhalten Angaben zu den Kontinenten,
  # die für die Kartenvisualisierung nicht benötigt werden.
  # Lösche die ersten 9 Zeilen
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
      menuItem("Karte", tabName = "map_tab")
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
            width = 12,
            valueBoxOutput("selected_countries_count")
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
}

# App starten
shinyApp(ui, server)

