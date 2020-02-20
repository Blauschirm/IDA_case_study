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
    titlePanel("Örtliche und zeitliche Verteilung von Fahrzeugen mit verbauten defekten Ledersitzen"),
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
                      leafletOutput(outputId = "map", width = 1000, height = 1000)),
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
      setView(lng = 13.4, lat = 52.52, zoom = 7) %>%
      addTiles()
  })
  observeEvent(input$reset, {
    
    leafletProxy("map")%>%
      setView(lng = 13.4, lat = 52.52, zoom = 11)
    
  })
  
}

# Shiny App starten
shinyApp(ui, server)