library(shiny)
library(ggplot2)

if( !require(leaflet)){
  install.packages("leaflet")
}
library(leaflet)

# Shiny Server
shinyApp(ui, server)

# Shiny UI
ui <- fluidPage(
  mainPanel(
    leafletOutput(outputId = "map", width = 1000, height = 1000)
  ),
  sidebarPanel(
    actionButton(inputId = "reset", "RESET")
  )
)
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