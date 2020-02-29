library(shiny)
library(ggplot2)
library(DT)

# scales to be able to use dates as ggplot limits
if (!require(scales)){
  install.packages("scales")
}
library(scales)

if (!require(lubridate)){
  install.packages("lubridate")
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

if( !require(glue)){
  install.packages("glue")
}
library(glue)

# Load manufacturing info with geo data
# Um mit der Console zu arbeiten muss man den Pfad ändern: load("./project/Datensatz_tidy.RData") oder getwd() versuchen
load("Datensatz_tidy.RData")
#load("./project/Datensatz_tidy.RData")

# Data preperation
#
# for debugging: reducing the amount of data to be loaded

# Error: n <- 15680
#n <- 15680
#n <- 15679 # Test size
max <- 322075 # Number of observations
n <-   3220
radius_factor <- 15000 # 700

beispiel <- floor(runif(8, min=1, max = n))
#beispiel <- c(1:8)
print("Beispiel Set: "); str(beispiel)

#auswahl <- seq(x, n, by=x) # subset data

#final_joined_error <- final_joined[c(n-1, n, n+1), ]

# Subset the data
#final_joined <- final_joined[beispiel, ]
#final_joined <- final_joined[c(beispiel, 1:(n-8)), ]
final_joined <- final_joined[sample(nrow(final_joined), 10000),]

# Filter rows to display only distinct ID_Fahrzeug values: fahrzeuge
all_vehicles <- final_joined[!duplicated(final_joined$ID_Fahrzeug), ]

start_end_dates <- c( min(all_vehicles$Zulassungsdatum) - 28, max(all_vehicles$Zulassungsdatum) + 28 )


ui <- fluidPage( # theme = "bootstrap.min.css" # shinythemes::shinytheme("cerulean"),
  
  mainPanel(
    width="100%",
    # Link CSS file to main panel
    #tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css"),
    tabsetPanel(type = "tabs",
      tabPanel("Extern",
               wellPanel(
                 titlePanel("Ist mein Fahrzeug betroffen?"),
                 fluidRow(
                   column(
                     8,
                     textInput(
                       'vehicle_id_input', "FahrzeugID eingeben um Details zu sehen", value = "", width = "100%",
                       placeholder = 'Fahrzeug ID')
                   ),
                   column(
                     4,
                     actionButton('vehicle_filter_submit', 'Suchen')
                   ),
                 ),
                 fluidRow(
                   verbatimTextOutput("result_text"),
                   verbatimTextOutput("vehicle_info_text"),
                   tableOutput('components_list'),
                   tableOutput('parts_list')
                 )
               )
      ),
      tabPanel("Intern",
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
          plotOutput("plot_zulassungsverlauf"),
         

          sliderInput("slider_zulassungsperiode", "Wählen Sie die Zulassungsperiode",
                      min(all_vehicles$Zulassungsdatum), max(all_vehicles$Zulassungsdatum),
                      value = c(min(all_vehicles$Zulassungsdatum), max(all_vehicles$Zulassungsdatum))
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
                   # selectizeInput(
                   #   'e1', 'Wählen Sie den Kartentyp aus',
                   #   choices = c("Fahrzeuginfo", "Lieferweg des Bauteils")
                   # ),
                   
                   
                   #verbatimTextOutput("value"),
                   #checkboxInput("lieferwege", "Lieferwege anzeigen", FALSE),
                   #verbatimTextOutput("value2"),
                   
                   # Reset search filter section
                   column(6, offset = 0, align = 'left', #style = 'border: 1px solid lightgray; border-radius: 3px',
                          "Für mehr Informationen hineinzoomen und/oder auf die Markierungen klicken.",
                          actionButton(inputId = "reset", "Position zurücksetzen")
                          
                   ),
                   
                   # Info text map
                   column(6, offset= 0, align = 'right', #style = 'border: 1px solid lightgray; border-radius: 3px',
                          "Zum Filtern der Ergebnisse Bautteile und/oder Gemeinden auswählen.",
                          actionButton("reset_filters", "Alle Filter zurücksetzen"),
                   )
            )
          )
        ),
        wellPanel(
          titlePanel("Interaktive Karte mit Gemeinde-Suche und Bauteilsuche"),
          
          fluidRow(
            column(12,
                   checkboxGroupInput("checkbox_fahrzeuge", "Kartenebenen auswählen", 
                                      inline = TRUE,
                                      choices = c('Heatmap (Schadensschwerpunkte)', "fehlerhafte Fahrzeuge", "Lieferwege", "Standorte (Lieferwege)")),
                   
            )
          ),
          # Display the heatmap  with car markers
          leafletOutput(outputId = "map", width = '100%', height = 600),
          "Zum Anzeigen von Fahrzeuginformationen hineinzoomen und/oder auf die Markierungen klicken"
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
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  
  # Filter parts with the three datatables
  filtered_parts <- reactive({
    tmp <- final_joined
    if(length(input$datatable_gemeinden_rows_selected)){
      tmp <- filter(tmp, (PLZ %in% gemeinden[input$datatable_gemeinden_rows_selected,]$PLZ))
    }
    if(length(input$datatable_final_joined_rows_selected)){
      tmp <- tmp[input$datatable_final_joined_rows_selected,]
    }
    if(length(input$datatable_bauteile_rows_selected)){
      tmp <- tmp[input$datatable_bauteile_rows_selected,]
    }
    
    print("                FILTERED PARTS:                      ")
    print(str(tmp))
    
    tmp
  })
  
  # Calculate the vehicles from the filteres parts
  filtered_vehicles <- reactive({
    filtered_parts()[!duplicated(filtered_parts()$ID_Fahrzeug), ]
  })
  
  # Filter the Zulassungen so only the ones corresponding to selected Gemeinden in the Gemeinden Datatable are displayed
  zulassungen <- reactive({
    # first check wether any rows in the table are selected right now. 
    # Selected rows can be checked by appending __rows__selected to the name of a data table and using that as an input
    # This returns the indices of the selected rows in the table, which then need to be mapped to the actual data used in the table
    if(length(input$datatable_gemeinden_rows_selected)){
      zulassungen_out <- filter(all_vehicles, Gemeinde %in% gemeinden[input$datatable_gemeinden_rows_selected,]$Gemeinde)
    } else {
      # If no rows are selected we use all data
      zulassungen_out <- all_vehicles
    }
    
    # before returning the filtered Zulassungen we are preparing them for use in the bar plot, by bundeling the data by Month, by setting all
    # dates to the first of their month and then grouping them by month, Gemeinde and Fahrzeug ID
    zulassungen_out <- zulassungen_out %>%
      mutate(Monat = as.Date(format.Date(zulassungen_out$Zulassungsdatum, "%Y-%m-1"), "%Y-%m-%d"), defekt= (Fehlerhaft_Komponente > 0 | Fehlerhaft_Einzelteil > 0)) %>%
      group_by(Monat, Gemeinde, Werksnummer_Fahrzeug) %>%
      summarise(Anzahl = n()) %>%
      ungroup()
    
    # returning the filtered data
    zulassungen_out
  })
  
  # Plot für zeitlichen Zulassungsverlauf vorbereiten
  output$plot_zulassungsverlauf <- renderPlot({
    ggplot(zulassungen(), aes(x = Monat, y = Anzahl, fill=factor(Werksnummer_Fahrzeug))) +
      geom_bar(stat = "identity", width = 20) +
      scale_fill_manual(values=c("#c50e1f", "#7CAE00", "#00BFC4", "#C77CFF")) +
      guides(fill = guide_legend(title="Werknummer der OEM")) + 
      scale_x_date(breaks = breaks_width("3 month"),
                   labels = date_format(format = "%Y-%b", tz = "ECT"),
                   limits = start_end_dates
      ) + 
      scale_y_continuous(breaks=pretty_breaks()) +
      theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position="bottom")
  })
  
  # Render data tables: gemeinden / bauteile
  # https://shiny.rstudio.com/reference/shiny/latest/renderTable.html
  # https://shiny.rstudio.com/reference/shiny/0.12.1/tableOutput.html
  
  gemeinden <- all_vehicles %>% 
    select(Gemeinde, PLZ, Werksnummer_Komponente) %>%
    group_by(Gemeinde, PLZ) %>%
    summarise(Zulassungen = length(PLZ)) %>%
    arrange(Gemeinde) %>%
    ungroup()
  
  tier1_werke <- final_joined[beispiel, ] %>% 
    select(ID_Fahrzeug, Fehlerhaft_Einzelteil, Werksnummer_Einzelteil, Breitengrad_Einzelteil, Längengrad_Einzelteil) %>%
    group_by(Werksnummer_Einzelteil, Breitengrad_Einzelteil, Längengrad_Einzelteil) %>%
    summarise(Einzelteile_fehlerhaft = sum(Fehlerhaft_Einzelteil),
              Einzelteile_hergestellt = n() ) %>%
    ungroup() %>%
    arrange(Einzelteile_fehlerhaft)
  #summary(tier1_werke)
  
  
  tier2_werke <- final_joined[beispiel, ] %>% 
    select(ID_Fahrzeug, ID_Fahrzeug, ID_Komponente, Fehlerhaft_Einzelteil, Fehlerhaft_Komponente, Werksnummer_Komponente, Breitengrad_Komponente, Längengrad_Komponente) %>%
    group_by(Werksnummer_Komponente, Breitengrad_Komponente, Längengrad_Komponente) %>%
    summarise(Einzelteile_fehlerhaft = sum(Fehlerhaft_Einzelteil),
              Einzelteile_hergestellt = n(),
              Sitze_fehlerhaft = sum(Fehlerhaft_Komponente),
              Sitze_hergestellt = sum(!duplicated(ID_Fahrzeug)),
              Komponenten = toString(c(ID_Komponente)) ) %>%
    #arrange(Sitze_fehlerhaft) %>%
    ungroup()
  
  summary(tier2_werke)
  sum(tier2_werke$Sitze_hergestellt)
  sum(tier2_werke$fehlerhafte_Einzelteile_erhalten)

  
  # Render data table: gemeinden
  output$datatable_gemeinden <- renderDataTable({
    input$reset_filters
    datatable(
      gemeinden,
      options = list(
        lengthMenu = list(c(3, 6, 20, 50), c('3', '6', '20', '50')), # layout breaks with three digit numbers in the list
        pageLength = 3
      ),
      rownames = FALSE
    )
  })
  
  
  # show defective parts bools as factors: Ja / Nein 
  final_joined$Fehlerhaft_Einzelteil = factor(final_joined$Fehlerhaft_Einzelteil, c(0, 1), c('Nein', 'Ja'))
  final_joined$Fehlerhaft_Komponente = factor(final_joined$Fehlerhaft_Komponente, c(0, 1), c('Nein', 'Ja'))
  
  # Render data table: bauteile
  output$datatable_bauteile <- renderDataTable({
    input$reset_filters
    datatable(
      final_joined[ ,c('ID_Einzelteil', 'Werksnummer_Einzelteil', 'Fehlerhaft_Einzelteil', 'ID_Komponente', 'Fehlerhaft_Komponente', 'ID_Fahrzeug')], 
      
      #filter = list(position = 'bottom', clear = TRUE),
      
      options = list(
        pageLength = 3,
        lengthMenu = list(c(3, 6, 10, 20, 100, 1000, 10000), c('3', '6', '10', '20', '100', '1000', '10000')),
        
        # Search wit regex Ja/Nein
        search = list(regex = TRUE, caseInsensitive = FALSE, search = ""), # 'ä=ae, ö=oe, ü=ue'
        
        # Define German translaton of data table UI
        language = list(
          info = 'Zeige  _START_ bis _END_ von insgesamt _TOTAL_ Ergebnissen',
          paginate = list(first = 'Erste', last = 'Letzte', previous = 'Zurück', `next` = 'Vor'),
          infoEmpty = 'Keine Daten vorhanden',
          loadingRecords = 'Lädt...',
          processing = 'Ergebnisse werden geladen...',
          lengthMenu = 'Zeige _MENU_ Ergebnisse',
          infoFiltered =  '| Gefiltert von _MAX_ Einträgen',
          search = 'Suche nach betroffenen Bauteilen:')
      ),
      
      colnames = c('ID_Werk' = 'Werksnummer_Einzelteil',
                   'Fehlerhaft' = 'Fehlerhaft_Einzelteil',
                   'Fehlerhaft' = 'Fehlerhaft_Komponente'),
      
      rownames = FALSE
    ) %>% 
      formatStyle(
        c('ID_Komponente', 'ID_Fahrzeug'), `border-left` = 'solid 1px'
      )
  })
  
  # filtered_data_dots <- reactive({
  #   if(length(input$datatable_gemeinden_rows_selected)){
  #     data_dots[beispiel, ] %>%
  #       filter(lng_begin %in% gemeinden[input$datatable_gemeinden_rows_selected,]$Längengrad_Einzelteil)
  #   } else {
  #     data_dots
  #   }
  # })
  
  
  #### Data Preparation for the rendering of (1) heatmap with (2) markers and (3) supply routes (and circles)
  
  # 1. Create datapoints for the heatmap
  treshold_fehleranzahl <- 1 # recommended values: 1, 10, 20, 40, ...
  datapoints_heat <- final_joined %>%
    group_by(Längengrad, Breitengrad) %>%
    summarise(fehleranzahl = n())  %>%
    ungroup()  %>%
    #select(-Gemeinde)  %>%
    filter(fehleranzahl > treshold_fehleranzahl)
  
  # 2. 
  # Not finished: Filter supply_routes data linked to table selections
  supply_routes <- final_joined

  data_dots = data.frame(id = 1:nrow(final_joined), # 1:length(beispiel)
                         lat_begin = supply_routes$Breitengrad_Einzelteil,
                         lat_via = supply_routes$Breitengrad_Komponente,
                         lat_end = supply_routes$Breitengrad,
                         lng_begin = supply_routes$Längengrad_Einzelteil,
                         lng_via = supply_routes$Längengrad_Komponente,
                         lng_end = supply_routes$Längengrad,
                         ID_Fahrzeug = supply_routes$ID_Fahrzeug)
  
  # Altenate displayed routes
  # Not finished: Filter supply_routes data linked to table selections
  selected_routes <- beispiel # <- Hier einfügen: Gefilterte Auwahl der Tabellenauswahl
  
  # Custom icons for markers of facilities
  tier1Icon <- makeIcon(
    iconUrl = "https://pngimage.net/wp-content/uploads/2018/05/facility-png-2.png",
    iconWidth = 40, iconHeight = 25)
  carIcon <- makeIcon(
    iconUrl = "https://gkv.com/wp-content/uploads/leaflet-maps-marker-icons/map_marker-red1.png",
    iconAnchorX = 18, iconAnchorY = 40,
    iconWidth = 35, iconHeight = 40)
  
  # Colors for the supply routes
  colors_polyline <- c("#c50e1f", "#7CAE00", "#00BFC4", "#C77CFF")

  #output$value <- renderText({ input$all_vehicles })
  #output$value1 <- renderText({ input$lieferwege })
  
  # Conditionally adding markers to map
  # Select supply route 
  
  # if(input$map == checkbox_fahrzeuge){
  #   mapdata = data1
  #   mapcol = rgb(215,48,39, max = 255)
  # }
  # if(input$map == "2"){
  #   mapdata = data2
  #   mapcol = rgb(....blah)
  # }
  
  
  # Render map
  output$map <- renderLeaflet({
    leaflet(final_joined) %>%
      setView(lng = 10.46, lat = 51.15, zoom = 6.25) %>% # centered to Germany map
      # do not use:
      #fitBounds(min(final_joined$Längengrad, na.rm = TRUE),min(final_joined$Breitengrad, na.rm = TRUE),max(final_joined$Längengrad, na.rm = TRUE),max(final_joined$Breitengrad, na.rm = TRUE)) %>% # buggy after scaling
      addTiles() %>%
      
      # Layer 1: Heatmap
      addHeatmap(data = datapoints_heat, lng = ~Längengrad, lat = ~Breitengrad,
                 intensity = ~fehleranzahl, blur = 12, max = 100, radius = 14) %>% # intensity = ~fehleranzahl, blur = 14, max = 60, radius = 12) %>%
      # END Layer 1
      
      # Layer 2: fehlerhafte Fahrzeuge
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
      # END Layer 2
    
    #filtered_faclities_tier1() <- filtered_vehicles()[!duplicated(Werksnummer_Einzelteil),]
    #filtered_facilites_tier2() <- filtered_vehicles()[!duplicated(filtered_vehicles()$Werksnummer_Komponente),]
      
      # Layer 3: Lieferwege
      # Render the polyroutes supply route 
      addPolylines(data = data_dots[selected_routes[1],],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   #color = 'blue',
                   #group = ~ID_Fahrzeug,
                   color = colors_polyline[1],
                   weight = 4,
                   opacity = 0.5, fill = FALSE, fillColor = "#c50e1",
                   fillOpacity = 0.5, dashArray = NULL, smoothFactor = 1,
                   noClip = TRUE, popup = ~ID_Fahrzeug, popupOptions = NULL, label = ~ID_Fahrzeug,
                   #labelOptions = NULL, options = pathOptions(),
                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      
      # Render the polyroutes supply route
      addPolylines(data = data_dots[selected_routes[2],],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = colors_polyline[2], weight = 4, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 1, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[selected_routes[3],], # Hier die Conditionals einfügen
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = colors_polyline[3], weight = 4, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 1, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[selected_routes[4],],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = colors_polyline[4], weight = 4, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 1, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[selected_routes[5],],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = colors_polyline[1], weight = 4, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 1, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[selected_routes[6],],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = colors_polyline[2], weight = 4, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 1, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[selected_routes[7],],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = colors_polyline[3], weight = 4, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 1, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      # Render the polyroutes supply route
      addPolylines(data = data_dots[selected_routes[8],],
                   lng= ~ c(lng_begin, lng_via, lng_end),
                   lat= ~ c(lat_begin, lat_via, lat_end),
                   color = colors_polyline[4], weight = 4, opacity = 0.5, fillColor = "#c50e1", fillOpacity = 1, smoothFactor = 4,
                   popup = ~ID_Fahrzeug, label = ~ID_Fahrzeug,
                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      )  %>%
    
      # Layer 4: Standorte
      
      # Add circles of facility
      
      # Einzelteil-Werk: Number of production errors Einzelteile hergestellt (schwarz)
      addCircles(data = tier1_werke, ~Längengrad_Einzelteil, ~Breitengrad_Einzelteil,
                 color = 'black', weight = 0, stroke=FALSE, fillOpacity = 0.5,
                 radius = tier1_werke$Einzelteile_hergestellt*radius_factor) %>%
      
      # Einzelteil-Werk: Number of production errors Einzelteile fehlerhaft (rot)
      addCircles(data = tier1_werke, ~Längengrad_Einzelteil, ~Breitengrad_Einzelteil,
                 color = 'red', stroke=TRUE, fillOpacity = 0.5, weight = 5, opacity = 0.1,
                 radius = tier1_werke$Einzelteile_fehlerhaft*radius_factor) %>%
      
      # # Komponenten-Werk Number of production errors: Einzelteile hergestellt (weiß)
      # addCircles(data = tier2_werke, ~Längengrad_Komponente, ~Breitengrad_Komponente,
      #            color = 'weiß', weight = 1, stroke=FALSE, fillOpacity = 0.3,
      #            radius = tier2_werke$Einzelteile_hergestellt*radius_factor/3) %>%
      # 
      # # Komponenten-Werk Number of production errors: Einzelteile fehlerhaft (rot)
      # addCircles(data = tier2_werke, ~Längengrad_Komponente, ~Breitengrad_Komponente,
      #            color = 'blue', weight = 1, stroke=FALSE, fillOpacity = 0.3,
      #            radius = tier2_werke$Einzelteile_fehlerhaft*radius_factor/3) %>%
      
      # Komponenten-Werk Number of production errors: Sitze hergestellt (schwarz)
      addCircles(data = tier2_werke, ~Längengrad_Komponente, ~Breitengrad_Komponente,
                 stroke=FALSE, fillOpacity = 0.5, color = 'black', weight = 1,
                 radius = tier2_werke$Sitze_hergestellt*radius_factor/3) %>%
      
      # Komponenten-Werk Number of production errors: Sitze fehlerhaft (rot)
      addCircles(data = tier2_werke, ~Längengrad_Komponente, ~Breitengrad_Komponente,
                 stroke=TRUE, fillOpacity = 0.5, color = 'red', weight = 5, opacity = 0.1,
                 radius = tier2_werke$Sitze_fehlerhaft*radius_factor/3) %>%

      
      
      

      
      
      
      

      
      # Display tier1 facilities with custom icon
      addMarkers(data = tier1_werke, ~Längengrad_Einzelteil, ~Breitengrad_Einzelteil, icon = tier1Icon, # filtered_data_dots(), ~lat_via, ~lng_via,
                 #display large amounts of markers as clusters
                 #clusterOptions = markerClusterOptions(freezeAtZoom = 7),
                 popup = ~paste("<center><h5>Einzelteil-Werk</h5></center>",
                                "Werksnummer: ", Werksnummer_Einzelteil, "<br/>",
                                "Einzelteile geliefert: ", Einzelteile_hergestellt, "<br/>",
                                "davon fehlerhaft laut Einzelteil-Werk: ", Einzelteile_fehlerhaft, "<br/>")
      )  %>% 
      
      # Display tier2 facilities with custom icon
      addMarkers(data = tier2_werke, ~Längengrad_Komponente, ~Breitengrad_Komponente, icon = tier1Icon,# filtered_data_dots(), ~lat_via, ~lng_via,
                 #display large amounts of markers as clusters
                 #clusterOptions = markerClusterOptions(freezeAtZoom = 2),
                 popup = ~paste("<center><h5>Komponenten-Werk</h5></center>",
                                "Werksnummer: ", Werksnummer_Komponente, "<br/>",
                                "betroffene Komponenten: ", Komponenten, "<br/>",
                                "Einzelteile erhalten: ", Einzelteile_hergestellt, "<br/>",
                                "davon fehlerhaft laut Einzelteile-Werk: ", Einzelteile_fehlerhaft, "<br/>",
                                "defekte Sitze hergestellt: ", Sitze_hergestellt, "<br/>",
                                "davon fehlerhaft laut Komponenten-Werk: ", Sitze_fehlerhaft, "<br/>")
                                
                 
      )  %>%
      
      # Add marker for car location
      addMarkers(data = filtered_vehicles()[selected_routes[1], ], ~Längengrad, ~Breitengrad, icon = carIcon,
               #display large amounts of markers as clusters
               clusterOptions = markerClusterOptions(),
               popup = ~paste("<center><h5>Betroffenes Fahrzeug</h5></center>",
                              "ID_Fahrzeug: ", ID_Fahrzeug, "<br/>",
                              "ID_Sitz: ", ID_Komponente, "<br/>",
                              "Baujahr: ", format(as.Date(Produktionsdatum_Fahrzeug),"%Y"), "<br/>",
                              "Zulassung am: ", format(as.Date(Zulassungsdatum),"%d.%m.%Y"), "<br/>",
                              "Zugelassen in: ", PLZ, " ", Gemeinde)
    )  %>%
      # Add marker for car location
      addMarkers(data = final_joined[selected_routes[2], ], ~Längengrad, ~Breitengrad,  icon = carIcon,
                   #display large amounts of markers as clusters
                   clusterOptions = markerClusterOptions(),
                   popup = ~paste("<center><h5>Betroffenes Fahrzeug</h5></center>",
                                  "ID_Fahrzeug: ", ID_Fahrzeug, "<br/>",
                                  "ID_Sitz: ", ID_Komponente, "<br/>",
                                  "Baujahr: ", format(as.Date(Produktionsdatum_Fahrzeug),"%Y"), "<br/>",
                                  "Zulassung am: ", format(as.Date(Zulassungsdatum),"%d.%m.%Y"), "<br/>",
                                  "Zugelassen in: ", PLZ, " ", Gemeinde)
        )  %>%
      # Add marker for car location
      addMarkers(data = final_joined[selected_routes[3], ], ~Längengrad, ~Breitengrad,  icon = carIcon,
                   #display large amounts of markers as clusters
                   clusterOptions = markerClusterOptions(),
                   popup = ~paste("<center><h5>Betroffenes Fahrzeug</h5></center>",
                                  "ID_Fahrzeug: ", ID_Fahrzeug, "<br/>",
                                  "ID_Sitz: ", ID_Komponente, "<br/>",
                                  "Baujahr: ", format(as.Date(Produktionsdatum_Fahrzeug),"%Y"), "<br/>",
                                  "Zulassung am: ", format(as.Date(Zulassungsdatum),"%d.%m.%Y"), "<br/>",
                                  "Zugelassen in: ", PLZ, " ", Gemeinde)
        )  %>%
      # Add marker for car location
        addMarkers(data = final_joined[selected_routes[4], ], ~Längengrad, ~Breitengrad, icon = carIcon,
                   #display large amounts of markers as clusters
                   clusterOptions = markerClusterOptions(),
                   popup = ~paste("<center><h5>Betroffenes Fahrzeug</h5></center>",
                                  "ID_Fahrzeug: ", ID_Fahrzeug, "<br/>",
                                  "ID_Sitz: ", ID_Komponente, "<br/>",
                                  "Baujahr: ", format(as.Date(Produktionsdatum_Fahrzeug),"%Y"), "<br/>",
                                  "Zulassung am: ", format(as.Date(Zulassungsdatum),"%d.%m.%Y"), "<br/>",
                                  "Zugelassen in: ", PLZ, " ", Gemeinde)
        )  %>%
      # Add marker for car location
        addMarkers(data = final_joined[selected_routes[5], ], ~Längengrad, ~Breitengrad, icon = carIcon,
                   #display large amounts of markers as clusters
                   clusterOptions = markerClusterOptions(),
                   popup = ~paste("<center><h5>Betroffenes Fahrzeug</h5></center>",
                                  "ID_Fahrzeug: ", ID_Fahrzeug, "<br/>",
                                  "ID_Sitz: ", ID_Komponente, "<br/>",
                                  "Baujahr: ", format(as.Date(Produktionsdatum_Fahrzeug),"%Y"), "<br/>",
                                  "Zulassung am: ", format(as.Date(Zulassungsdatum),"%d.%m.%Y"), "<br/>",
                                  "Zugelassen in: ", PLZ, " ", Gemeinde)
        )  %>%
      
      # Add marker for car location
      addMarkers(data = final_joined[selected_routes[6], ], ~Längengrad, ~Breitengrad, icon = carIcon,
                 #display large amounts of markers as clusters
                 clusterOptions = markerClusterOptions(),
                 popup = ~paste("<center><h5>Betroffenes Fahrzeug</h5></center>",
                                "ID_Fahrzeug: ", ID_Fahrzeug, "<br/>",
                                "ID_Sitz: ", ID_Komponente, "<br/>",
                                "Baujahr: ", format(as.Date(Produktionsdatum_Fahrzeug),"%Y"), "<br/>",
                                "Zulassung am: ", format(as.Date(Zulassungsdatum),"%d.%m.%Y"), "<br/>",
                                "Zugelassen in: ", PLZ, " ", Gemeinde)
      )  %>%
      
      # Add marker for car location
      addMarkers(data = final_joined[selected_routes[7], ], ~Längengrad, ~Breitengrad, icon = carIcon,
                 #display large amounts of markers as clusters
                 clusterOptions = markerClusterOptions(),
                 popup = ~paste("<center><h5>Betroffenes Fahrzeug</h5></center>",
                                "ID_Fahrzeug: ", ID_Fahrzeug, "<br/>",
                                "ID_Sitz: ", ID_Komponente, "<br/>",
                                "Baujahr: ", format(as.Date(Produktionsdatum_Fahrzeug),"%Y"), "<br/>",
                                "Zulassung am: ", format(as.Date(Zulassungsdatum),"%d.%m.%Y"), "<br/>",
                                "Zugelassen in: ", PLZ, " ", Gemeinde)
      )  %>%
      # Add marker for car location
        addMarkers(data = final_joined[8, ], ~Längengrad, ~Breitengrad, icon = carIcon,
               #display large amounts of markers as clusters
               clusterOptions = markerClusterOptions(),
               popup = ~paste("<center><h5>Betroffenes Fahrzeug</h5></center>",
                              "ID_Fahrzeug: ", ID_Fahrzeug, "<br/>",
                              "ID_Sitz: ", ID_Komponente, "<br/>",
                              "Baujahr: ", format(as.Date(Produktionsdatum_Fahrzeug),"%Y"), "<br/>",
                              "Zulassung am: ", format(as.Date(Zulassungsdatum),"%d.%m.%Y"), "<br/>",
                              "Zugelassen in: ", PLZ, " ", Gemeinde)
    )
    
    
  })
  
  observeEvent(input$reset, {
    
    leafletProxy("map") %>%
      setView(lng = 10.46, lat = 51.15, zoom = 6.25)
    
  })
  
  observe({
    input$reset
    leafletProxy("map") %>%
      setView(lng = 10.46, lat = 51.15, zoom = 6.25)
  })
  
  
  # Render full database
  output$datatable_final_joined <- renderDataTable({
    input$reset_filters
    datatable(
      final_joined[, c('ID_Einzelteil', 'Werksnummer_Einzelteil', 'Fehlerhaft_Einzelteil',
                       'ID_Komponente', 'Werksnummer_Komponente', 'Fehlerhaft_Komponente',
                       'ID_Fahrzeug',   'Werksnummer_Fahrzeug',   'Produktionsdatum_Fahrzeug', 'Zulassungsdatum', 'Gemeinde', 'PLZ') ],
      
              filter = list(position = 'top', clear = TRUE),
      
              options = list(
                pageLength = 10,
                lengthMenu = list(c(3, 10, 20, 100, 1000, 10000), c('3', '10', '20', '100', '1000', '10000')),
                
                # Search wit regex Ja/Nein
                search = list(regex = TRUE, caseInsensitive = FALSE, search = ""), # 'ä=ae, ö=oe, ü=ue'
                
                # Define German translaton of data table UI
                language = list(
                  info = 'Zeige  _START_ bis _END_ von insgesamt _TOTAL_ Ergebnissen',
                  paginate = list(first = 'Erste', last = 'Letzte', previous = 'Zurück', `next` = 'Vor'),
                  infoEmpty = 'Keine Daten vorhanden',
                  loadingRecords = 'Lädt...',
                  processing = 'Ergebnisse werden geladen...',
                  lengthMenu = 'Zeige _MENU_ Ergebnisse',
                  infoFiltered =  '| Gefiltert von _MAX_ Einträgen',
                  search = 'Suche nach betroffenen Bauteilen:')
              ),
      
              colnames = c('ID_Werk' = 'Werksnummer_Einzelteil',
                           'ID_Werk' = 'Werksnummer_Komponente',
                           'ID_Werk' =  'Werksnummer_Fahrzeug',
                           'Fehlerhaft' = 'Fehlerhaft_Einzelteil',
                           'Fehlerhaft' = 'Fehlerhaft_Komponente'),
                           
              rownames = FALSE
      ) %>% 
      formatStyle(
        c('ID_Komponente', 'ID_Fahrzeug'), `border-left` = 'solid 1px',
      )
  })

  output$result_text <- renderText({"Geben Sie ihre Fahrzeug ID in die Suche ein um zu überprüfen ob ihr Fahrzeug betroffen ist."})
  
  observeEvent(input$vehicle_filter_submit, {
    
    vehicle_parts <- filter(final_joined, ID_Fahrzeug == input$vehicle_id_input)
    if (dim(vehicle_parts)[1] >= 1){
      output$result_text <- renderText({"Ihr Fahrzeug ist betroffen"})
      # Welche Daten interessieren einen Fahrzeughalter?
      
      # Zulassungsdatum
      # PLZ, Gemeinde
      # Werksnummer_Fahrzeug
      # Produktionsdatum_Fahrzeug
      #
      #
      # ID_Einzelteil
      # Werksnummer_Einzelteil
      #
      #
      # ID_Komponente
      # Werksnummer_Komponente
      
      # Fahrzeuginfos
      vehicle <- vehicle_parts[!duplicated(vehicle_parts$ID_Fahrzeug)]
      vehicle_info_string <- glue("Ihr Fahrzeug ({vehicle$ID_Fahrzeug}), zugelassen am {vehicle$Zulassungsdatum} in {vehicle$PLZ} {vehicle$Gemeinde},
wurde am {vehicle$Produktionsdatum_Fahrzeug} im Werk {vehicle$Werksnummer_Fahrzeug} gebaut.
Folgend finden sie die Auflistung zu der verbauten Sitzgruppe, sowie zu den dafür verwendeten Einzelteilen
zusammen mit den Werksnummern bei denen Ihre Servicewerkstatt Ersatzteile anfordern kann.")
      output$vehicle_info_text <- renderText(vehicle_info_string)
    
      # components
      output$components_list <- renderTable(vehicle_parts[,c("ID_Komponente","Fehlerhaft_Komponente", "Werksnummer_Komponente")])
      
      # parts
      output$parts_list <- renderTable(vehicle_parts[,c("ID_Einzelteil", "Fehlerhaft_Einzelteil", "Werksnummer_Einzelteil")])
      
    } else {
      output$result_text <- renderText({"ID exisitiert nicht."})
      output$vehicle_info_text <- NULL
      output$components_list <- NULL
      output$parts_list <- NULL
    }
  })
} 

# Shiny App starten
shinyApp(ui, server)