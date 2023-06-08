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

getgeodata <- function() {
  # Quelle: https://www.kaggle.com/datasets/paultimothymooney/latitude-and-longitude-for-every-country-and-state
  # Geo Daten Importieren
  geodata <- read_csv("world_country_and_usa_states_latitude_and_longitude_values.csv")
  geodata <- geodata[, c("country", "latitude", "longitude")]
  # Rückgabe der Daten
  geodata
}

js_code <- '
function copyToClipboard(text) {
    var textarea = document.createElement("textarea");
    textarea.value = text;
    document.body.appendChild(textarea);
    textarea.select();
    document.execCommand("copy");
    document.body.removeChild(textarea);
    alert("Text copied to clipboard: " + text);
}'

css <- "
custom-cursor:hover {
    cursor: pointer;
}
"

# UI
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Graphs", tabName = "Graphs"),
      selectInput("value", "Werte anzeigen:", choices = c("TotalCases", "TotalDeaths", "TotalTests", "Population"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Dashboard",
              fluidRow(
                valueBoxOutput("box_1")
              ),
              fluidRow(
                leafletOutput("map")
              )
      ),
      # Second tab content
      tabItem(tabName = "Graphs",
              fluidRow(
                div(style = "overflow-x: auto", DT::DTOutput("graph"))
              )
      )
    )
  ),
  tags$head(
    tags$script(HTML(js_code)),
    tags$style(HTML(css))
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
  
  # Load and filter data
  data_filtered <- reactive({
    merged_data <- data()
    if (!is.null(input$value)) {
      merged_data <- merged_data %>% filter(country %in% input$countryInput)
    }
    merged_data
  })
  
  # Render Box 1
  output$box_1 <- renderValueBox({
    valueBox(nrow(data_filtered()), subtitle = "Ausgewählte Länder", icon = icon(name = "table-list"), color = "purple")
  })
  
  
  # Leaflet-Karte mit COVID-19-Daten
  output$map <- renderLeaflet({
    leaflet(data_filtered()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addCircleMarkers(
        data = data_filtered(),
        lng = ~longitude,
        lat = ~latitude,
        radius = 10,
        color = "#FF0000",
        fillOpacity = 0.7
      )
  })
  
  # PickerInput for country selection
  observe({
    updatePickerInput(
      session = session,
      inputId = "countryInput",
      choices = unique(data()$country),
      selected = unique(data()$country)
    )
  })
}

# App starten
shinyApp(ui, server)
