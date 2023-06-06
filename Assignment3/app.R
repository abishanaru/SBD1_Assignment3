library(shiny)
library(shinydashboard)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Mein Shiny-Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Startseite", tabName = "home", icon = icon("home")),
      menuItem("Daten", tabName = "data", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        h2("Startseite"),
        # Hier kannst du den Inhalt für die Startseite platzieren
      ),
      tabItem(
        tabName = "data",
        h2("Daten"),
        # Hier kannst du den Inhalt für die Daten-Seite platzieren
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Hier kannst du Server-Logik hinzufügen, wenn erforderlich
}

# App starten
shinyApp(ui, server)
