library(shiny)
library(ggplot2)
library(DT)

# scales to be able to use dates as ggplot limits
if (!require(scales)){
  installed.packages("scales")
}
library(scales)

if (!require(lubridate)){
  installed.packages("lubridate")
}
library(lubridate)

if( !require(leaflet)){
  install.packages("leaflet")
}
library(leaflet)
library(leaflet.extras)


if( !require(dplyr)){
  install.packages("dplyr")
}
library(dplyr)

# Load manufacturing info with geo data
# Um mit der Console zu arbeiten muss man den Pfad ändern: load("./project/Datensatz_tidy.RData") oder getwd() versuchen
load("Datensatz_tidy.RData")
#load("./project/Datensatz_tidy.RData")

# Data preperation
#
# for debugging: reducing the amount of data to be loaded
#final_joined <- head(final_joined, n = 500)

# Filter rows to display only distinct ID_Fahrzeug values: fahrzeuge
fahrzeuge <- final_joined[!duplicated(final_joined$ID_Fahrzeug),]
start_end_dates <- c( min(fahrzeuge$Zulassungsdatum), max(fahrzeuge$Zulassungsdatum) )
#start_end_dates <- c( as.Date("2009-1-1", "Y-m-d"),as.Date("2017-1-1", "Y-m-d") )
print(start_end_dates)
#
# Create a search/autocomplete vector with ID_Einzelteil, ID_Komponente and ID_Fahrzeug: inputID
#inputIDs <- c(fahrzeuge$ID_Fahrzeug, final_joined$ID_Einzelteil, final_joined$ID_Sitze)
#inputIDs <- fahrzeuge$ID_Fahrzeug[1:10]
fahrzeuge_subset <- fahrzeuge[1:40,]
#
# Cerate a search/autocomplete list as group options: inputIDs_grouped
#inputIDs_grouped <- list(ID_Einzelteil = final_joined$ID_Einzelteil, ID_Komponente = final_joined$ID_Sitze, ID_Fahrzeug = fahrzeuge$ID_Fahrzeug)
#
# Cerate a subset of IDs (due to performance issues)
subset_size = 120000
subset_distribution_size <- subset_size / 3
inputIDs_grouped <- list(ID_Einzelteil = final_joined$ID_Einzelteil[1:subset_distribution_size], ID_Komponente = final_joined$ID_Sitze[1:subset_distribution_size], ID_Fahrzeug = fahrzeuge$ID_Fahrzeug[1:subset_distribution_size])
#str(inputIDs_grouped)
# Cerate a subset of 30k IDs (due to performance issues)
#inputIDs_grouped <- list(ID_Einzelteil = final_joined$ID_Einzelteil[1:10000], ID_Komponente = final_joined$ID_Sitze[1:10000], ID_Fahrzeug = fahrzeuge$ID_Fahrzeug[1:10000])


# Shiny UI
ui <- fluidPage(
  mainPanel(
    width="100%",
    wellPanel(
      titlePanel("Darstellung 1: Zeitlicher Zulassungsverlauf"),
      plotOutput("plot_zulassungsverlauf")
    ),
    wellPanel(
      titlePanel("Darstellung 2: Heatmap mit Gemeinde-Suche und Bauteil-Suche"),
      fluidRow(
        column(12,
          fluidRow(
            column(12,
              fluidRow(
                actionButton("reset_filters", "Alle Filter zurücksetzen")
              ),
              fluidRow(
                column(3, 
                  (h4("Betroffene Gemeinden")),
                  
                  # Display Betroffene Gemeinden as data table
                  dataTableOutput('datatable_gemeinden'),
                  
                  #datatable(datatable_gemeinde), # slow
                  # depreciated due to client-side rendering!
                  # use instead:
                  # client-side: dataTableOutput('my_table')
                  # server-side: output$'my_table' <- renderDataTable(my_df)
                  
                ),
                column(7,
                  fluidRow(
                    
                    # Heatmap with search bar section
                    column(10,
                      (h4("Betroffene Bauteile")),
                      
                      # Display ID-search by ID_einzelteile & ID_sitze
                      dataTableOutput('datatable_bauteile'),
                      
                      # Select map type
                      selectizeInput(
                        'e1', 'Wählen Sie den Kartentyp aus',
                        choices = c("Fahrzeuginfo", "Lieferweg des Bauteils")
                      ),

                      # Display the heatmap  with car markers
                      leafletOutput(outputId = "map", width = '100%', height = 600)
                      ),
                      column(10, "Zum Anzeigen von Fahrzeuginformationen hineinzoomen und/oder auf die Markierungen klicken")
                  )
                ),
                # column(2,
                #   fluidRow("Betroffene Fahrzeuge"),
                #   
                #     # Adds 'Betrofene Fahrzeuge' table 
                #     column(12,
                #          tableOutput('table_fahrzeuge'),
                #   ),
                #   
                #   fluidRow("Betroffene Bauteile"),
                #   
                #     # Adds 'Betrofene Bauteile' table
                #     column(12,
                #          tableOutput('table_bauteile'),
                #   ),
                # )
              )
            )
          )
        )
      )
    )
  )
)




# Shiny Server
server <- function(input, output, session) {

  # Filter rows to display only distinct ID_Fahrzeug values: fahrzeuge
  fahrzeuge <- final_joined[!duplicated(final_joined$ID_Fahrzeug),]
  
  zulassungen <- reactive({
    if(length(input$datatable_gemeinden_rows_selected)){
      zulassungen_out <- filter(fahrzeuge, Gemeinde %in% gemeinden[input$datatable_gemeinden_rows_selected,]$Gemeinde)
    } else {
      zulassungen_out <- fahrzeuge
    }
    
    
    zulassungen_out <- zulassungen_out %>%
      mutate(Monat = as.Date(format.Date(zulassungen_out$Zulassungsdatum, "%Y-%m-1"), "%Y-%m-%d")) %>%
      group_by(Monat, Gemeinde) %>%
      summarise(anzahl = n()) %>%
      ungroup()

    zulassungen_out
    
  })
  
  # Plot für zeitlichen Zulassungsverlauf vorbereiten
  output$plot_zulassungsverlauf <- renderPlot({
    ggplot(zulassungen(), aes(x = Monat, y = anzahl)) +
    geom_bar(stat = "identity") +
    scale_x_date(breaks = scales::breaks_width("1 month"), 
                 labels = scales::label_date_short(),
                 limits = start_end_dates) +
    scale_y_continuous(breaks= pretty_breaks()) +
    theme(axis.text.x = element_text(angle=45, hjust = 1))
  })
  
  
  # Render data tables: gemeinden / bauteile
  # https://shiny.rstudio.com/reference/shiny/latest/renderTable.html
  # https://shiny.rstudio.com/reference/shiny/0.12.1/tableOutput.html
  
  gemeinden <- fahrzeuge %>% 
    select(Gemeinde, PLZ) %>%
    group_by(Gemeinde, PLZ) %>%
    summarise(Zulassungen = length(PLZ)) %>%
    arrange(Gemeinde) %>%
    ungroup()
  
  # Render data table: gemeinden
  output$datatable_gemeinden <- renderDataTable({
                                                input$reset_filters
                                                gemeinden}) # Betroffene Gemeinde
  # Render data table: bauteile
  # fehlt noch: input$reset_filters
  output$datatable_bauteile <- renderDataTable(final_joined[final_joined$Fehlerhaft_Einzelteil == 1 | final_joined$Fehlerhaft_Komponente == 1, c(1, 2, 4, 7, 11, 13)], # [1:30,c(1,4)],  # Betroffene Bauteile
                                               options = list(
                                                 lengthMenu = list(c(3, 6, -1), c('3', '6', 'All')),
                                                 pageLength = 3
                                               ),
                                               rownames = FALSE) # suppress row names / index numbers
  
  filtered_vehicles <- reactive({
    if(length(input$datatable_gemeinden_rows_selected)){
      fahrzeuge %>%
        filter(PLZ %in% gemeinden[input$datatable_gemeinden_rows_selected,]$PLZ)
    } else {
      fahrzeuge
    }
  })
  
  # Render the heatmap with markers: map
  output$map <- renderLeaflet({
    leaflet() %>%
      #setView(lng = 10.46, lat = 51.15, zoom = 6.25) %>%
      addTiles() %>%
      fitBounds(min(final_joined$Längengrad, na.rm = TRUE),min(final_joined$Breitengrad, na.rm = TRUE),max(final_joined$Längengrad, na.rm = TRUE),max(final_joined$Breitengrad, na.rm = TRUE)) %>%
      addMarkers(data = filtered_vehicles(), ~Längengrad, ~Breitengrad, 
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