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
n <- 1200 # Test size
max <- 230000 # Number of observations
beispiel <- floor(runif(n, min=1, max = max))
str(beispiel)
#auswahl <- seq(x, n, by=x) # subset data

# Subset the data
final_joined <- final_joined[beispiel, ]

str(final_joined)
# Filter rows to display only distinct ID_Fahrzeug values: fahrzeuge
fahrzeuge <- final_joined[!duplicated(final_joined$ID_Fahrzeug),]
start_end_dates <- c( min(fahrzeuge$Zulassungsdatum), max(fahrzeuge$Zulassungsdatum) )
#start_end_dates <- c( as.Date("2009-1-1", "Y-m-d"),as.Date("2017-1-1", "Y-m-d") )
print(start_end_dates)
#

ui <- fluidPage( # theme = "bootstrap.min.css" # shinythemes::shinytheme("cerulean"),
  
  mainPanel(
    # Link CSS file to main panel
    #tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css"),
    
    width="100%",
    wellPanel(
      
      img(src='https://www.qw.tu-berlin.de/fileadmin/_processed_/8/8d/csm_QW_ohne_Text_print_a4670877cd.jpg',
          align = "right"),
      #img(src='https://www.qw.tu-berlin.de/fileadmin/Aperto_design/img/logo_01.gif',
          #align = "left"),
      titlePanel("Case_Study_App_26"),
      
      fluidRow(
        column(12,
               "Schadensschwerpunkte und Lieferwege von betroffenen Bauteilen",
               style='font-size: 32px; color: #c50e1f;'
               # CSS no linebrake in data table column
        )
      )
    ),
    wellPanel(
      titlePanel("Zeitlicher Zulassungsverlauf der betroffenen Fahrzeuge aufgeteilt nach OEM-Werken"),
      fluidRow(
        column(12,
               plotOutput("plot_zulassungsverlauf")
        )
      )
    ),
    wellPanel(
      titlePanel("Interaktive Karte mit Gemeinde-Suche und Bauteilsuche"),
      
      fluidRow(
        column(12,
            
               fluidRow(
                 column(12,
                        fluidRow(
                          column(3, 
                                 (h4("Betroffene Gemeinden")),
                                 
                                 # Display Betroffene Gemeinden as data table
                                 dataTableOutput('datatable_gemeinden')
                          ),
                          
                          # Heatmap with search bar section
                          column(9,
                                 (h4("Betroffene Bauteile")),
                                 
                                 # Display ID-search by ID_einzelteile & ID_Komponente
                                 dataTableOutput('datatable_bauteile'),
                                 
                                 # Select map type
                                 selectizeInput(
                                   'e1', 'Wählen Sie den Kartentyp aus',
                                   choices = c("Fahrzeuginfo", "Lieferweg des Bauteils")
                                 ),
                                 
                                 # Reset search filter section
                                 column(4, offset = 0, align = 'left', #style = 'border: 1px solid lightgray; border-radius: 3px',
                                        "Zum Anzeigen von Fahrzeuginformationen hineinzoomen und/oder auf die Markierungen klicken.",
                                 ),
                                 
                                 # Info text map
                                 column(6, offset= 2, align = 'right', #style = 'border: 1px solid lightgray; border-radius: 3px',
                                        "Man kann Bautteile und Gemeinden auswählen um die Ergebnisse auf der Karte zu filtern.",
                                        actionButton("reset_filters", "Alle Filter zurücksetzen"),
                                 ),
                                 # Display the heatmap  with car markers
                                 leafletOutput(outputId = "map", width = '100%', height = 600),
                                 "Zum Anzeigen von Fahrzeuginformationen hineinzoomen und/oder auf die Markierungen klicken"),
                          
                          column(12,
                                 "Bottombox: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."
                          ),
                        )
                 )
               )
        )
      )
    ),
    wellPanel(
      titlePanel("Datenbank"),
      fluidRow(
        column(12,
               dataTableOutput('datatable_final_joined'),
               style='white-space: nowrap;' # CSS no linebrake in data table column
               
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
      mutate(Monat = as.Date(format.Date(zulassungen_out$Zulassungsdatum, "%Y-%m-1"), "%Y-%m-%d"), defekt= (Fehlerhaft_Komponente > 0 | Fehlerhaft_Einzelteil > 0)) %>%
      group_by(Monat, Gemeinde, Werksnummer_Fahrzeug) %>%
      summarise(anzahl = n()) %>%
      ungroup()
    
    zulassungen_out
  })
  
  # Plot für zeitlichen Zulassungsverlauf vorbereiten
  output$plot_zulassungsverlauf <- renderPlot({
    ggplot(zulassungen(), aes(x = Monat, y = anzahl, fill=factor(Werksnummer_Fahrzeug))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values=c("#c50e1f", "#7CAE00", "#00BFC4", "#C77CFF")) +
      guides(fill = guide_legend(title="Werknummer der OEM")) + 
      scale_x_date(breaks = scales::breaks_width("1 month"), 
                   labels = scales::label_date_short(format = c("%Y, %b")),
                   limits = start_end_dates) +
      scale_y_continuous(breaks= pretty_breaks()) +
      theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position="bottom")
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
    datatable(
      gemeinden,
      options = list(
        lengthMenu = list(c(3, 6, 20, 100), c('3', '6', '20', '100')),
        pageLength = 3
      ),
      rownames = FALSE
    )
  })
  
  # Render data table: bauteile
  # fehlt noch: input$reset_filters
  output$datatable_bauteile <- renderDataTable({
    input$reset_filters
    datatable(final_joined[final_joined$Fehlerhaft_Einzelteil == 1 | final_joined$Fehlerhaft_Komponente == 1, c(1, 2, 4, 7, 11, 13)], # [1:30,c(1,4)],  # Betroffene Bauteile
              options = list(
                lengthMenu = list(c(3, 6, 100), c('3', '6', '100')),
                pageLength = 3
              ),
              rownames = FALSE)
  })
  
  filtered_vehicles <- reactive({
    if(length(input$datatable_gemeinden_rows_selected)){
      fahrzeuge %>%
        filter(PLZ %in% gemeinden[input$datatable_gemeinden_rows_selected,]$PLZ)
    } else {
      fahrzeuge
    }
  })
  filtered_data_dots <- reactive({
    if(length(input$datatable_gemeinden_rows_selected)){
      data_dots[beispiel, ] %>%
        filter(lng_begin %in% gemeinden[input$datatable_gemeinden_rows_selected,]$Längengrad_Einzelteil)
    } else {
      data_dots
    }
  })
  
  
  # Data Preparation for the rendering of heatmap with markers and supply routes
  #
  # Create datapoints for the heatmap
  treshold_fehleranzahl <- 1
  #treshold_fehleranzahl <- 10
  datapoints_heat <- final_joined %>%
    group_by(Längengrad, Breitengrad) %>%
    summarise(fehleranzahl = n())  %>%
    ungroup()  %>%
    #select(-Gemeinde)  %>%
    filter(fehleranzahl > treshold_fehleranzahl)
  #
  # Create data frame with n supply routes: data_dots
  #load("./project/Datensatz_tidy.RData")
  #n <- 1000
  supply_routes <- final_joined
  
  data_dots = data.frame(id = 1:length(beispiel),
                         lat_begin = supply_routes$Breitengrad_Einzelteil,
                         lat_via = supply_routes$Breitengrad_Komponente,
                         lat_end = supply_routes$Breitengrad,
                         lng_begin = supply_routes$Längengrad_Einzelteil,
                         lng_via = supply_routes$Längengrad_Komponente,
                         lng_end = supply_routes$Längengrad,
                         ID_Fahrzeug = supply_routes$ID_Fahrzeug)
  data
  # Altenate displayed routes
  selected_routes <- beispiel
  beispiel
  #
  # Render map
  output$map <- renderLeaflet({
    leaflet(final_joined) %>%
      setView(lng = 10.46, lat = 51.15, zoom = 6.25) %>% # centered to Germany map
      addTiles() %>%
      addHeatmap(data = datapoints_heat, lng = ~Längengrad, lat = ~Breitengrad,
                 intensity = ~fehleranzahl, blur = 14, max = 15, radius = 24) %>% # intensity = ~fehleranzahl, blur = 14, max = 60, radius = 12) %>%
      fitBounds(min(final_joined$Längengrad, na.rm = TRUE),min(final_joined$Breitengrad, na.rm = TRUE),max(final_joined$Längengrad, na.rm = TRUE),max(final_joined$Breitengrad, na.rm = TRUE)) %>% # buggy after scaling
      addMarkers(data = filtered_vehicles(), ~Längengrad, ~Breitengrad,
                 #display large amounts of markers as clusters
                 clusterOptions = markerClusterOptions(),
                 popup = ~paste("<center><h5>Betroffenes Fahrzeug</h5></center>",
                                "ID_Fahrzeug: ", ID_Fahrzeug, "<br/>",
                                "ID_Sitz: ", ID_Komponente, "<br/>",
                                "Baujahr: ", format(as.Date(Produktionsdatum_Fahrzeug),"%Y"), "<br/>",
                                "Zulassung am: ", format(as.Date(Zulassungsdatum),"%d.%m.%Y"), "<br/>",
                                "Zugelassen in: ", PLZ, " ", Gemeinde)
      )  %>%
    
    addMarkers(data = filtered_data_dots(), ~lat_via, ~lng_via,
               #display large amounts of markers as clusters
               clusterOptions = markerClusterOptions(),
               popup = ~paste("<center><h5>Werksinfo Einzelteil</h5></center>",
                              "ID_Fahrzeug: ", ID_Fahrzeug, "<br/>")
    )  %>%
      
      # add dots of supply route
      addCircles(data = filtered_vehicles(), ~Längengrad, ~Breitengrad,
                 color = 'grey', weight = 1, radius = gemeinden$Zulassungen*5000, stroke=FALSE, fillOpacity = 0.3) %>%
      
      # Render the polyroutes supply route
      addPolylines(data = data_dots[1,],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   #color = 'blue',
                   #group = ~ID_Fahrzeug,
                   color = "#c50e1f",
                   weight = 5,
                   opacity = 0.5, fill = FALSE, fillColor = "#c50e1",
                   fillOpacity = 0.2, dashArray = NULL, smoothFactor = 1,
                   noClip = TRUE, popup = ~ID_Fahrzeug, popupOptions = NULL, label = ~ID_Fahrzeug,
                   #labelOptions = NULL, options = pathOptions(),
                   highlightOptions = highlightOptions(color = "white", weight = 2,
                                                       bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[2,],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = 'blue', weight = 5, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 0.5, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2,
                                                       bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[3,],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = 'yellow', weight = 5, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 0.5, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2,
                                                       bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[4,],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = 'pink', weight = 5, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 0.5, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2,
                                                       bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[5,],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = 'pink', weight = 5, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 0.5, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2,
                                                       bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[selected_routes[6],],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = 'pink', weight = 5, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 0.5, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2,
                                                       bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[selected_routes[7],],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = 'pink', weight = 5, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 0.5, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2,
                                                       bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[selected_routes[8],],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = 'pink', weight = 5, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 0.5, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2,
                                                       bringToFront = TRUE)
      )
    
    
    
    
  })
  
  observeEvent(input$reset, {
    
    leafletProxy("map") %>%
      setView(lng = 10.46, lat = 51.15, zoom = 6.25)
    
  })
  
  # Render full database
  output$datatable_final_joined <- renderDataTable({
    input$reset_filters
    datatable(final_joined[final_joined$Fehlerhaft_Einzelteil == 1 | final_joined$Fehlerhaft_Komponente == 1,
                           c('ID_Einzelteil', 'Werksnummer_Einzelteil', 'Fehlerhaft_Einzelteil',
                             'ID_Komponente', 'Werksnummer_Komponente', 'Fehlerhaft_Komponente',
                             'ID_Fahrzeug', 'Werksnummer_Fahrzeug', 'Produktionsdatum_Fahrzeug', 'Zulassungsdatum', 'Gemeinde', 'PLZ') ],
              filter = 'top',
              options = list(
                lengthMenu = list(c(3, 10, 20, 100, 1000, 10000), c('3', '10', '20', '100', '1000', '10000')),
                pageLength = 10,
                
                # Define German translaton of data table UI
                language = list(
                  info = 'Zeige  _START_ bis _END_ von insgesamt _TOTAL_ Ergenissen',
                  paginate = list(first = 'Erste', last = 'Letzte', previous = 'Zurück', `next` = 'Vor'),
                  infoEmpty = 'Keine Daten vorhanden',
                  loadingRecords = 'Lädt...',
                  processing = 'Ergebnisse werden geladen...',
                  lengthMenu = 'Zeige _MENU_ Ergebnisse',
                  search = 'Suche nach betroffenen Bauteilen:')
                  #search = 'Suche nach betroffenen Gemeinden:')
                
              ),
              colnames = c('ID_Werk' = 'Werksnummer_Einzelteil',
                           'ID_Werk' = 'Werksnummer_Komponente',
                           'ID_Werk' =  'Werksnummer_Fahrzeug',
                           'Fehlerhaft' = 'Fehlerhaft_Einzelteil',
                           'Fehlerhaft' = 'Fehlerhaft_Komponente'),
              rownames = FALSE) %>% 
      # Add column grid to visually divide Einzelteil, Komponente and Fahrzeug
      formatStyle(
        c('ID_Komponente', 'ID_Fahrzeug'), `border-left` = 'solid 1px'
      )
  })
  
} 

# Shiny App starten
shinyApp(ui, server)