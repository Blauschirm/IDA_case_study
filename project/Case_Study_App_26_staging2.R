library(shiny)
library(ggplot2)
library(DT)

if( !require(leaflet)){
  install.packages("leaflet")
}
library(leaflet)

if( !require(dplyr)){
  install.packages("dplyr")
}
library(dplyr)

# Load manufacturing info with geo data
# Um mit der Console zu arbeiten muss man den Pfad ändern: load("./project/Datensatz_tidy.RData") oder getwd() versuchen
load("Datensatz_tidy.RData")
#final_joined <- head(final_joined, 3000000)
#load("./project/Datensatz_tidy.RData")
# Data preperation
#
# Filter rows to display only distinct ID_Fahrzeug values: fahrzeuge
fahrzeuge <- final_joined[!duplicated(final_joined$ID_Fahrzeug),]
#sitze <- final_joined[!duplicated(final_joined$ID_Sitze),]
#
# Create subset to display for test in table
fahrzeuge_subset <- fahrzeuge[1:40,]
#
# Create 3 input vectors for dropdown menus: inputIDs_sitze, inputIDs_einzelteile, inputIDs_Fahrzeug
 inputIDs_einzelteile <- final_joined$ID_Einzelteil[final_joined$Fehlerhaft_Einzelteil == 1]
 inputIDs_sitze <- fahrzeuge$ID_Sitze[fahrzeuge$Fehlerhaft_Komponente == 1]
 inputIDs_fahrzeuge <- fahrzeuge$ID_Fahrzeug
#
# Create a search/autocomplete vector with ID_Einzelteil, ID_Komponente and ID_Fahrzeug: inputID
#inputIDs <- c(fahrzeuge$ID_Fahrzeug, final_joined$ID_Einzelteil, final_joined$ID_Sitze)
#
# Cerate a search/autocomplete list (only ID_Einzelteil & ID_Komponente that are defective) as group options: inputIDs_grouped
 # inputIDs_grouped <- list(
 #          ID_Einzelteil = final_joined$ID_Einzelteil[final_joined$Fehlerhaft_Einzelteil == 1],
 #          ID_Komponente = fahrzeuge$ID_Sitze[fahrzeuge$Fehlerhaft_Komponente == 1]
 #          )
 #  str(inputIDs_grouped) # Stats
#
# Cerate a search/autocomplete list as group options: inputIDs_grouped
#inputIDs_grouped <- list(ID_Einzelteil = final_joined$ID_Einzelteil, ID_Komponente = final_joined$ID_Sitze, ID_Fahrzeug = fahrzeuge$ID_Fahrzeug)
#
# Cerate a fixed subset of 30k IDs (due to performance issues): inputIDs_grouped
#inputIDs_grouped <- list(ID_Einzelteil = final_joined$ID_Einzelteil[1:10000], ID_Komponente = final_joined$ID_Sitze[1:10000], ID_Fahrzeug = fahrzeuge$ID_Fahrzeug[1:10000])
#
# Cerate a variable subset of [subset_size] IDs (due to performance issues)
#subset_size = 450000 # only multiples of 3 allowed
#subset_distribution_size <- subset_size / 3
#inputIDs_grouped <- list(ID_Einzelteil = final_joined$ID_Einzelteil[1:subset_distribution_size], ID_Komponente = final_joined$ID_Sitze[1:subset_distribution_size], ID_Fahrzeug = fahrzeuge$ID_Fahrzeug[1:subset_distribution_size])
#
# IDs size analysis
#
# inputIDs_einzelteil <- final_joined$ID_Einzelteil
 str(inputIDs_einzelteile)
# inputIDs_sitze <- final_joined$ID_Sitze
# str(inputIDs_sitze)
# inputIDs_sitze_unique <- fahrzeuge$ID_Sitze[fahrzeuge$Fehlerhaft_Komponente]
# str(inputIDs_sitze_unique)
#
# End of data preparation

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
                column(2, 
                  (h4("Betroffene Gemeinden")),
                  
                  # Display Betroffene Gemeinden as data table
                  dataTableOutput('datatable_gemeinde'),
                  
                  #datatable(datatable_gemeinde), # slow
                  # depreciated due to client-side rendering!
                  # use instead:
                  # client-side: dataTableOutput('my_table')
                  # server-side: output$'my_table' <- renderDataTable(my_df)
                  
                ),
                column(
                  8,
                  fluidRow(
                    
                    # Heatmap with search bar section
                    column(8, 
                      (h4("Betroffene Bauteile")),
                      
                      # Create search input bar with autocomplete: selectizeInput()
                      # from: https://shiny.rstudio.com/gallery/selectize-examples.html
                      # if needed: toggle/hide Dropdown: https://rdrr.io/cran/shinyWidgets/man/toggleDropdownButton.html
                      # if neeeed: performance boost: check selectizeInput('x2'... https://shiny.rstudio.com/gallery/option-groups-for-selectize-input.html
                      
                      # selectizeInput(
                      #   'e2', 'Wählen Sie eine oder mehrere Fahrzeug- oder Bauteil-IDs aus',
                      #   choices = inputIDs_grouped, multiple = TRUE, options = list(maxOptions = 6, placeholder = 'Filter für ID_Bauteil(e) und/oder ID_Fahrzeug (AND Verknüpfung)'
                      # ),
                      
                      # Client-side rendering of updateSelectizeInput(session, 'search_by_ID2', ...),
                      # selectizeInput('search_by_ID_einzelteile', 'Selectize = TRUE, server-side: Wählen Sie eine oder mehrere Einzelteil-IDs aus',
                      #                choices = NULL,
                      #                multiple = TRUE,
                      # ),
                      # 
                      # hr(), # adds horizontal line
                      # 
                      # selectizeInput('search_by_ID_sitze', 'Selectize = TRUE, server-side: Wählen Sie eine oder mehrere Komponenten-IDs aus (selectizeInput, langsamer)',
                      #                choices = NULL,
                      #                multiple = TRUE,
                      # ),
                      # selectInput('e1', 'Selectize = FALSE, client-side: Wählen Sie eine oder mehrere Komponenten-IDs aus (selectInput)',
                      #             choices = inputIDs_sitze,
                      #             #multiple = TRUE
                      #             selectize = FALSE
                      # ),
                      # 
                      # hr(), # adds horizontal line
                      # 
                      # selectizeInput('search_by_ID_fahrzeuge', 'Selectize = TRUE, server-side: Wählen Sie eine oder mehrere Fahrzeug-IDs aus',
                      #                choices = NULL,
                      #                multiple = TRUE,
                      # ),
                      # 
                      # selectInput('x4', 'Selectize = FALSE, client-side: Wählen Sie eine oder mehrere Fahrzeug-IDs aus',
                      #             choices = inputIDs_fahrzeuge,
                      #             selectize = FALSE
                      # ),
                      
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
                      
                      # Display ID-search by ID_einzelteile & ID_sitze
                      dataTableOutput('datatable_bauteile'),
                      
                      # Selection of map type by "Fahrzeuginfo" or "Lieferweg des Bauteils"
                      selectizeInput(
                        'e1', 'Wählen Sie den Kartentyp aus',
                        choices = c("Fahrzeuginfo", "Lieferweg des Bauteils")
                      ),
                      
                      # Display the heatmap  with car markers
                      leafletOutput(outputId = "map", width = '100%', height = 550)
                      ),
                      column(8, "Zum Anzeigen von Fahrzeuginformationen hineinzoomen und/oder auf die Markierungen klicken")
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
  inputIDs_subset_value <- 900 # don't put more than 9k without strong machine
  # updateSelectizeInput(session, 'search_by_ID_einzelteile', 
  #                           choices = inputIDs_einzelteile[1:inputIDs_subset_value],
  #                           #multiple = TRUE, 
  #                           options = list(
  #                             maxOptions = 6,
  #                             placeholder = 'Filter für ID_Einzelteile',
  #                             selected = NULL)
  # )
  # 
  # updateSelectizeInput(session, 'search_by_ID_sitze',
  #                      choices = inputIDs_sitze[1:inputIDs_subset_value],
  #                      #multiple = TRUE,
  #                      options = list(
  #                        maxOptions = 6,
  #                        placeholder = 'Filter für ID_Komponente',
  #                        selected = NULL)
  # )
  # 
  # updateSelectizeInput(session, 'search_by_ID_fahrzeuge', 
  #                      choices = inputIDs_fahrzeuge[1:inputIDs_subset_value],
  #                      #multiple = TRUE, 
  #                      options = list(
  #                        maxOptions = 6,
  #                        placeholder = 'Filter für ID_Fahrzeuge',
  #                        selected = NULL)
  # )

  # Render table: datatable_gemeinde, table_fahrzeuge, table_bauteile
  # https://shiny.rstudio.com/reference/shiny/latest/renderTable.html
  # https://shiny.rstudio.com/reference/shiny/0.12.1/tableOutput.html
  output$datatable_gemeinde <- renderDataTable(final_joined[,c(14,16)]) # Betroffene Gemeinde
  output$table_fahrzeuge <- renderTable(fahrzeuge_subset[1:10,11]) # Betroffene Fahrzeuge
  output$table_bauteile <- renderTable(final_joined[1:30,1]) # Betroffene Bauteile
  
  
  output$datatable_bauteile <- renderDataTable(final_joined[final_joined$Fehlerhaft_Einzelteil == 1 | final_joined$Fehlerhaft_Komponente == 1, c(1, 2, 4, 7, 11, 13)], # [1:30,c(1,4)],  # Betroffene Bauteile
                                               options = list(
                                                 lengthMenu = list(c(3, 6, -1), c('3', '6', 'All')),
                                                 pageLength = 3
                                               ),
                                               rownames = FALSE # suppress row names / index numbers
                                               
  )
                                               #filter = list(position = "top"), 
                                               #pageLength = 3,
                                               #style = "bootstrap",
                                               #escape = FALSE,
                                               #filter="top",
                                               #autoWidth = TRUE
                                               #selection="multiple",
                                               # options = list(
                                               #                sDom  = '<"top">lrt<"bottom">ip',
                                               #                pageLength = 3,
                                               #                )
                                               #options = list(
                                                              # 
                                                              #searching = FALSE)
                                               
  
  
  # Render the heatmap with markers: map
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