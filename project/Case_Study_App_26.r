library(shiny)
library(ggplot2)
library(DT)

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
final_joined <- head(final_joined, n = 500)
# Filter rows to display only distinct ID_Fahrzeug values: fahrzeuge
fahrzeuge <- final_joined[!duplicated(final_joined$ID_Fahrzeug),]
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

#str(inputIDs_grouped) # Stats
# Shiny UI
ui <- fluidPage(
  mainPanel(
    width="100%",
    titlePanel("Darstellung 1: Zeitlicher Zulassungsverlauf nach Gemeinden"),
    wellPanel(
      fluidRow(
        column(12,
          fluidRow(
            column(12,
              titlePanel("Darstellung 2: Heatmap mit Fahrzeug-Suche und Bauteil-Suche + Darstellung des Lieferwegs"),
              fluidRow(
                actionButton("reset_filters", "Alle Filter zurücksetzen")
              ),
              fluidRow(
                column(
                  3, 
                  "betroffene Gemeinden",
                  
                  # Display Betroffene Gemeinden as data table
                  DT::dataTableOutput('datatable_gemeinden'),
                  
                  #datatable(datatable_gemeinde), # slow
                  # depreciated due to client-side rendering!
                  # use instead:
                  # client-side: dataTableOutput('my_table')
                  # server-side: output$'my_table' <- renderDataTable(my_df)
                  
                ),
                column(
                  7,
                  fluidRow(
                    
                    # Heatmap with search bar section
                    column(8, 
                      titlePanel(h6("Zum Anzeigen von Fahrzeuginformationen hineinzoomen und/oder auf die Markierungen klicken")),
                      
                      # Create search input bar with autocomplete: selectizeInput()
                      # from: https://shiny.rstudio.com/gallery/selectize-examples.html
                      # if needed: toggle/hide Dropdown: https://rdrr.io/cran/shinyWidgets/man/toggleDropdownButton.html
                      # if neeeed: performance boost: check selectizeInput('x2'... https://shiny.rstudio.com/gallery/option-groups-for-selectize-input.html
                      
                      # selectizeInput() (depreciated)
                      selectizeInput(
                        'e1', 'Wählen Sie den Kartentyp aus',
                        choices = c("Fahrzeuginfo", "Lieferweg des Bauteils")
                      ),
                      # selectizeInput(
                      #   'e2', 'Wählen Sie eine oder mehrere Fahrzeug- oder Bauteil-IDs aus',
                      #   choices = inputIDs_grouped, multiple = TRUE, options = list(maxOptions = 6, placeholder = 'Filter für ID_Bauteil(e) und/oder ID_Fahrzeug (AND Verknüpfung)'
                      # ),
                      
                      # Client-side rendering of updateSelectizeInput(session, 'search_by_ID2', ...),
                      selectizeInput('search_by_ID', 'Wählen Sie eine oder mehrere Fahrzeug- oder Bauteil-IDs aus',
                                     choices = NULL,
                                     multiple = TRUE,
                      ),

                      
                      # Highlight the text and use CTRL + SHIFT + C to (un)comment multiple lines in Windows. Or, command + SHIFT + C in OS-X.
                      # selectizeInput(
                      #   'e3', '3. Item creation', choices = inputIDs,
                      #   options = list(create = TRUE)
                      # ),
                      # selectizeInput(
                      #   'e4', '4. Max number of options to show (3)', choices = inputIDs,
                      #   options = list(maxOptions = 3)
                      # ),
                      # selectizeInput(
                      #   'e5', '5. Max number of items to select', choices = inputIDs,
                      #   multiple = TRUE, options = list(maxItems = 2)
                      # ),
                      
                      # Display the heatmap  with car markers
                      leafletOutput(outputId = "map", width = 600, height = 600)
                      ),
                      column(8, "Bottombox")
                  )
                ),
                column(2,
                  fluidRow("Betroffene Fahrzeuge"),
                  
                    # Adds 'Betrofene Fahrzeuge' table 
                    column(12,
                         tableOutput('table_fahrzeuge'),
                  ),
                  
                  fluidRow("Betroffene Bauteile"),
                  
                    # Adds 'Betrofene Bauteile' table
                    column(12,
                         tableOutput('table_bauteile'),
                  ),
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
server <- function(input, output, session) {
  
  # Render dropdown menu: selectizeInput()
  # with inputID: 'search_by_ID'
  # https://shiny.rstudio.com/gallery/option-groups-for-selectize-input.html
  # https://shiny.rstudio.com/reference/shiny/0.14/updateSelectInput.html
  updateSelectizeInput(session, 'search_by_ID', 
                            choices = inputIDs_grouped,
                            #multiple = TRUE, 
                            options = list(
                              maxOptions = 6,
                              placeholder = 'Filter für ID_Bauteil(e) und/oder ID_Fahrzeug (AND Verknüpfung)',
                              selected = NULL)
  )
  
  # Render table: datatable_gemeinde, table_fahrzeuge, table_bauteile
  # https://shiny.rstudio.com/reference/shiny/latest/renderTable.html
  # https://shiny.rstudio.com/reference/shiny/0.12.1/tableOutput.html
  
  gemeinden <- final_joined %>% 
    select(Gemeinde, PLZ) %>%
    group_by(Gemeinde, PLZ) %>%
    summarise(Zulassungen = length(PLZ)) %>%
    arrange(Gemeinde) %>%
    ungroup()
  
  output$datatable_gemeinden <- renderDataTable({
                                                input$reset_filters
                                                gemeinden}) # Betroffene Gemeinde
  output$table_fahrzeuge <- renderTable(fahrzeuge_subset[1:10, "ID_Fahrzeug"]) # Betroffene Fahrzeuge
  output$table_bauteile <- renderTable(final_joined[1:30, "ID_Einzelteil"]) # Betroffene Bauteile
  
  
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