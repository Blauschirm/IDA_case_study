library(shiny)
library(ggplot2)

if( !require(leaflet)){
  install.packages("leaflet")
}
library(leaflet)

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
                      titlePanel("Karte"),
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
      addTiles()
  })
  observeEvent(input$reset, {
    
    leafletProxy("map")%>%
      setView(lng = 10.46, lat = 51.15, zoom = 6.25)
    
  })
  
}

# Shiny App starten
shinyApp(ui, server)