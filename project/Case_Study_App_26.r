library(shiny)
library(ggplot2)

if( !require(leaflet)){
  install.packages("leaflet")
}
library(leaflet)

if( !require(rgdal)){
  install.packages("rgdal")
}
library(rgdal)

library(dplyr)

# Load manufacturing info with geo data
load("Datensatz_tidy.RData")
# Filter rows to display only distinct ID_Fahrzeug values: fahrzeuge
fahrzeuge <- einz_komp_fahrz_fehlerhaft_111[!duplicated(einz_komp_fahrz_fehlerhaft_111$ID_Fahrzeug),]

# Shiny UI
ui <- fluidPage(
  mainPanel(
    width="100%",
    titlePanel("Darstellung 1: Zeitlicher Zulassungsverlauf nach Gemeinden // Darstellung 2: Heatmap mit Fahrzeug-Suche und Bauteil-Suche + Darstellung des Lieferwegs"),
    wellPanel(
      fluidRow(
        column(12,
          fluidRow(
            column(12,
              titlePanel("Karte aller betroffenen Fahrzeuge"),
              fluidRow(
                column(
                  2, 
                  "betroffene Gemeinden"
                ),
                column(
                  8,
                  fluidRow(
                    column(8, 
                      titlePanel(h6("Zum Anzeigen von Fahrzeuginformationen hineinzoomen und/oder auf die Markierungen klicken")),
                      leafletOutput(outputId = "map", width = 550, height = 550)),
                      column(8, "Bottombox")
                  )
                ),
                column(2,
                  fluidRow("Betroffene Fahrzeuge"),
                  fluidRow("Betroffene Bauteile")
                )
              )
            )
          )
        )
      )
    )
  )
)




# Shiny Server
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 10.46, lat = 51.15, zoom = 6.25) %>%
      addTiles() %>%
      addMarkers(data = fahrzeuge, ~Längengrad, ~Breitengrad, 
                 #display large amounts of markers as clusters
                 clusterOptions = markerClusterOptions(),
                 popup = ~paste("<center><h5>Betroffenes Fahrzeug</h5></center>",
                                 "ID_Fahrzeug: ", ID_Fahrzeug, "<br/>",
                                 "ID_Sitz: ", ID_Sitze, "<br/>",
                                 "Baujahr: ", format(as.Date(Produktionsdatum_Fahrzeug),"%Y"), "<br/>",
                                 "Zulassung am: ", format(as.Date(Zulassungsdatum),"%d.%m.%Y"), "<br/>",
                                 "Zugelassen in: ", PLZ, " ", Gemeinde)
                 )
  })
  observeEvent(input$reset, {
    
    leafletProxy("map")%>%
      setView(lng = 10.46, lat = 51.15, zoom = 6.25)
    
  })
  
}

# Shiny App starten
shinyApp(ui, server)