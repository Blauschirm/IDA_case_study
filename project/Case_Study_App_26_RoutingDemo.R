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
final_joined <- head(final_joined, n = 100)

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
      img(src='https://www.qw.tu-berlin.de/fileadmin/Aperto_design/img/logo_01.gif',
          align = "left"),
      titlePanel("Case_Study_App_26"),
      
      fluidRow(
        column(12,
               "Zulassungsverlauf, Gemeinde-Suche und Bauteil-Suche",
               style='font-size: 26px; color: #c50e1f;'
               # CSS no linebrake in data table column
        )
      )
    ),
    wellPanel(
      titlePanel("Darstellung 1: Zeitlicher Zulassungsverlauf"),
      fluidRow(
        column(12,
               plotOutput("plot_zulassungsverlauf")
        )
      )
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
                style='white-space: nowrap;', # CSS no linebrake in data table column
               
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
        lengthMenu = list(c(3, 6, 20, -1), c('3', '6', '20', 'All')),
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
       lengthMenu = list(c(3, 6, -1), c('3', '6', 'All')),
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
  n <- 4
  routes <- final_joined[1:n, ]
  
  library(tidyverse)
  #craete a column with unique id's per event-location combination
  df <- df %>% mutate( id = row_number() )
  #create a temporaty df with events
  events.df <- df %>% 
    select( id, Event_lat, Event_lon) %>% 
    rename( latitude = Event_lat, longitude = Event_lon)
  #create a temporaty df with locations
  locations.df <- df %>% 
    select( id, Location_latitude, Location_longitude) %>%
    rename( latitude = Location_latitude, longitude = Location_longitude)
  #merge the two temp.df's together
  df.sp <- bind_rows( events.df, locations.df )
  
  
  
  
  #
  data_dots = data.frame(id = 1:n,
                         lat_begin = supply_routes$Breitengrad_Einzelteil,
                         lat_via = supply_routes$Breitengrad_Komponente,
                         lat_end = supply_routes$Breitengrad,
                         lng_begin = supply_routes$Längengrad_Einzelteil,
                         lng_via = supply_routes$Längengrad_Komponente,
                         lng_end = supply_routes$Längengrad,
                         ID_Fahrzeug = supply_routes$ID_Fahrzeug)
  #
  # Render map
  output$map <- renderLeaflet({
    leaflet(final_joined) %>%
      setView(lng = 10.46, lat = 51.15, zoom = 6.25) %>% # centered to Germany map
      addTiles() %>%
      addHeatmap(data = datapoints_heat, lng = ~Längengrad, lat = ~Breitengrad,
                 intensity = ~fehleranzahl, blur = 14, max = 20, radius = 12) %>% # intensity = ~fehleranzahl, blur = 14, max = 60, radius = 12) %>%
      #fitBounds(min(final_joined$Längengrad, na.rm = TRUE),min(final_joined$Breitengrad, na.rm = TRUE),max(final_joined$Längengrad, na.rm = TRUE),max(final_joined$Breitengrad, na.rm = TRUE)) %>% # buggy after scaling
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
      
    
      # add dots of supply route
    addCircles(data = data_dots, ~c(lng_begin, lng_via, lng_end) , ~c(lat_begin, lat_via, lat_end), 
               weight = 5, stroke=FALSE, fillOpacity = 0.7) %>% 
      # Render the polyroutes supply route
      addPolylines(data = data_dots,
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
                   highlightOptions = NULL
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
      lengthMenu = list(c(3, 10, 20, 100, 1000,  -1), c('3', '10', '20', '100', '1000', 'All')),
      pageLength = 10
    ),
    colnames = c('ID_Werk' = 'Werksnummer_Einzelteil',
                'ID_Werk' = 'Werksnummer_Komponente',
                'ID_Werk' =  'Werksnummer_Fahrzeug',
                'Fehlerhaft' = 'Fehlerhaft_Einzelteil',
                'Fehlerhaft' = 'Fehlerhaft_Komponente'),
    rownames = FALSE) %>% 
      # Add column grid to visually divide Einzelteil, Komponente and Fahrzeug
      formatStyle(
        c('ID_Komponente', 'ID_Fahrzeug'), `border-left` = "solid 1px"
      )
  })
    
} 

# Shiny App starten
shinyApp(ui, server)