# set cran mirror for package installs
local({r <- getOption("repos")
  r["CRAN"] <- "http://cran.r-project.org" 
  options(repos=r)
})

if (!require(shiny)) {
  install.packages("shiny")
}
library(shiny)

if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!require(stringr)) {
  install.packages("stringr")
}
library(stringr)

if (!require(DT)) {
  install.packages("DT")
}
library(DT)

if (!require(scales)) {
  install.packages("scales")
}
library(scales)

if (!require(lubridate)) {
  install.packages("lubridate")
}
library(lubridate)

if (!require(leaflet)) {
  install.packages("leaflet")
}
library(leaflet)

if (!require(leaflet.extras)) {
  install.packages("leaflet.extras")
}
library(leaflet.extras)

if (!require(leafpop)) {
  install.packages("leafpop")
}
library(leafpop)

if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)

if (!require(glue)) {
  install.packages("glue")
}
library(glue)

# Load manufacturing info with geo data
load("Final_Data_Group_26.RData")

# Subset the data for debugging: reducing the amount of data to be loaded

# final_joined <- final_joined[c(sample(nrow(final_joined), 1001)), ] # shouldn't not be lower than filter limit for markers

# Filter rows to display only distinct ID_Fahrzeug values: fahrzeuge (used in both UI and Server)
all_vehicles <- final_joined[!duplicated(final_joined$ID_Fahrzeug),]

ui <- fluidPage(mainPanel(
  width = "100%",
  
  tags$head(tags$style(
    HTML(
      "
      @import url('//fonts.googleapis.com/css?family=Ubuntu|Cabin:400,700');

      body {
        font-family: 'Ubuntu';
      }

      h2 {
        font-family: 'Ubuntu';
        font-weight: 500;
        line-height: 1.1;
        color: #428BCA;
      }

      #DataTables_Table_1_paginate {
        margin-top: 25px
      }

    "
    )
  )),
  
  # Header panel
  wellPanel(titlePanel("Case_Study_App_26"),
            fluidRow(
              column(
                11,
                "Schadensschwerpunkte und Lieferwege von betroffenen Bauteilen",
                style = 'font-size: 36px; color: #c50e1f;'
              ),
              column(1,
                     imageOutput("Logo", height = 1))
            )),
  
  # Seperate user interface into two tabs for different user groups: owner and manufacturer
  tabsetPanel(
    type = "tabs",
    # Tab with ui for owners
    tabPanel(
      "Für Fahrzeughalter",
      wellPanel(
        titlePanel("Ist mein Fahrzeug betroffen?"),
        fluidRow(
          column(
            4,
            textInput(
              'vehicle_id_input',
              "Fahrzeug ID eingeben um Details zu sehen",
              value = "",
              width = "100%",
              placeholder = 'Fahrzeug ID'
            )
          ),
          column(8,
                 actionButton('vehicle_filter_submit', 'Suchen'),
                 style = "margin-top: 25px"),
        ),
        fluidRow(
          column(
            12,
            verbatimTextOutput("result_text"),
            verbatimTextOutput("vehicle_info_text"),
            tableOutput('components_list'),
            tableOutput('parts_list')
          )
        )
      )
    ),
    # Tab with ui for manufacturer
    tabPanel(
      "Für Fahrzeughersteller",
      
      # Bar plot for Zulassungsverlauf
      wellPanel(
        titlePanel(
          "Zeitlicher Zulassungsverlauf der betroffenen Fahrzeuge aufgeteilt nach OEM-Werken"
        ),
        plotOutput("plot_zulassungsverlauf"),
      ),
      
      # Filter section for bar plot and heat map
      wellPanel(
        titlePanel("Suchfilter zum Anpassen des Balkendiagramms und der Karte"),
        # Reset all filters
        fluidRow(
          column(
            12,
            offset = 0,
            align = 'right',
            #style = 'border: 1px solid lightgray; border-radius: 3px',
            "Zum Filtern der Ergebnisse Bautteile und/oder Gemeinden auswählen",
            actionButton("reset_filters", "Alle Filter zurücksetzen"),
          )
        ),
        
        # Sliderinput filtering the time period for bar plot
        sliderInput(
          "slider_zulassungsperiode",
          "Wählen Sie den Zeitraum der Zulassungen aus",
          min(all_vehicles$Zulassungsdatum),
          max(all_vehicles$Zulassungsdatum),
          value = c(
            min(all_vehicles$Zulassungsdatum),
            max(all_vehicles$Zulassungsdatum)
          )
        ),
        
        # Fluidrow for gemeinden und bauteil datatables incl. search boxes
        fluidRow(column(
          4,
          (h4("Betroffene Gemeinden")),
          
          # Display Betroffene Gemeinden as data table
          dataTableOutput('datatable_gemeinden')
        ),
        column(
          8,
          (h4("Betroffene Bauteile")),
          
          # Display ID-search by ID_einzelteile & ID_Komponente
          dataTableOutput('datatable_bauteile')
        ))
      ),
      
      # heat map with check boxes and cluster markers and Lieferwege on map
      wellPanel(
        titlePanel(
          "Interaktive Karte mit Schadensschwerpunkten, betroffenen Fahrzeugen und Lieferwegen für Ersatzteile"
        ),
        
        fluidRow(
          style = "margin-bottom: 10px;",
          # Reset map position
          column(
            12,
            offset = 0,
            align = 'right',
            "Für mehr Informationen hineinzoomen und/oder auf die Markierungen klicken",
            actionButton(inputId = "reset", "Position zurücksetzen")
          )
        ),
        
        # Display the heatmap with car markers
        leafletOutput(
          outputId = "map",
          width = '100%',
          height = 600
        ),
        #"Bottombox: Zum Anzeigen von Fahrzeuginformationen hineinzoomen und/oder auf die Markierungen klicken"
      ),
      
      # full dataset in a datatable
      wellPanel(titlePanel("Datenbank"),
                fluidRow(
                  column(12,
                         dataTableOutput('datatable_final_joined'),
                         style = 'white-space: nowrap;' # CSS no linebrake in data table column)
                  ))
      )
    )
  )
))
  
# Shiny Server
server <- function(input, output, session) {
  # Render QW_logo: Send a pre-rendered image, and don't delete the image after sending it
  output$Logo <- renderImage({
    # Return a list containining the filename
    list(src = './Zusaetzliche_Dateien/QW_logo.png',
         width = 76,
         height = 56)
  }, deleteFile = FALSE)
  
  
  
  # save start and date of all zulassungen in a vector
  start_end_dates <-
    c(min(all_vehicles$Zulassungsdatum) - 28,
      max(all_vehicles$Zulassungsdatum) + 28)
  
  # Filter parts with the three datatables
  filtered_parts <- reactive({
    # the selections of the tables add to the filtered dataset
    gemeinden_filtered <-
      filter(final_joined, (PLZ %in% gemeinden[input$datatable_gemeinden_rows_selected, ]$PLZ))
    bigtable_filtered <-
      final_joined[input$datatable_final_joined_rows_selected, ]
    bauteiltable_filtered <-
      final_joined[input$datatable_bauteile_rows_selected, ]
    
    final_filtered <-
      bind_rows(gemeinden_filtered,
                bigtable_filtered,
                bauteiltable_filtered)
    
    if (nrow(final_filtered) == 0) {
      final_filtered <- final_joined
    } else {
      # finally the filtered dataset is reduced by the time slider
      final_filtered <-
        subset(
          final_filtered,
          Zulassungsdatum >= input$slider_zulassungsperiode[1] &
            Zulassungsdatum <= input$slider_zulassungsperiode[2]
        )
    }
    
    final_filtered
  })
  
  # Only draw the polylines and overlays for the first n parts
  filtered_parts_limited <- reactive({
    if (nrow(filtered_parts()) <= 1000) {
      out <- filtered_parts()
    } else {
      out <- NULL
    }
    out
  })
  
  # Calculate the vehicles from the filteres parts
  filtered_vehicles <- reactive({
    filtered_parts()[!duplicated(filtered_parts()$ID_Fahrzeug),]
  })
  
  # Filter the Zulassungen so only the ones corresponding to selected Gemeinden in the Gemeinden Datatable are displayed
  zulassungen <- reactive({
    # create subset based on slider input for zulassungen period
    zulassungen_out <-
      subset(
        all_vehicles,
        Zulassungsdatum >= input$slider_zulassungsperiode[1] &
          Zulassungsdatum <= input$slider_zulassungsperiode[2]
      )
    
    # first check wether any rows in the table are selected right now.
    # Selected rows can be checked by appending __rows__selected to the name of a data table and using that as an input
    # This returns the indices of the selected rows in the table, which then need to be mapped to the actual data used in the table
    if (length(input$datatable_gemeinden_rows_selected)) {
      zulassungen_out <-
        filter(all_vehicles, Gemeinde %in% gemeinden[input$datatable_gemeinden_rows_selected, ]$Gemeinde)
    } else {
      # If no rows are selected we use all data
      zulassungen_out <- all_vehicles
    }
    
    # before returning the filtered Zulassungen we are preparing them for use in the bar plot, by bundeling the data by Month, by setting all
    # dates to the first of their month and then grouping them by month, Gemeinde and Fahrzeug ID
    zulassungen_out <- zulassungen_out %>%
      mutate(
        Monat = as.Date(
          format.Date(zulassungen_out$Zulassungsdatum, "%Y-%m-1"),
          "%Y-%m-%d"
        ),
        defekt = (Fehlerhaft_Komponente > 0 |
                    Fehlerhaft_Einzelteil > 0)
      ) %>%
      group_by(Monat, Gemeinde, Werksnummer_Fahrzeug) %>%
      summarise(Anzahl = n()) %>%
      ungroup()
    
    # returning the filtered data
    zulassungen_out
  })
  
  # Render Plot for zeitlichen
  output$plot_zulassungsverlauf <- renderPlot({
    # reactive function to get smaller breaks on x-axis when date range on sliderInput gets smaller
    responsive_break_x <- reactive({
      # difference between sliderInputs is date range in number of days
      if (input$slider_zulassungsperiode[2] - input$slider_zulassungsperiode[1] < 700) {
        responsive_break_x <- breaks_width("1 month")
      } else {
        responsive_break_x <- breaks_width("3 month")
      }
      responsive_break_x
    })
    
    ggplot(zulassungen(), aes(
      x = Monat,
      y = Anzahl,
      fill = factor(Werksnummer_Fahrzeug)
    )) +
      geom_bar(stat = "identity", width = 20) +
      # add fixed colours to fills for each OEM
      scale_fill_manual(values = c(
        "11" = "#c50e1f",
        "12" = "#7CAE00",
        "21" = "#00BFC4",
        "22" = "#C77CFF"
      )) +
      guides(fill = guide_legend(title = "Werksnummer der OEM")) +
      scale_x_date(
        breaks = responsive_break_x(),
        labels = date_format(format = "%Y-%b", tz = "ECT"),
        limits = c(
          input$slider_zulassungsperiode[1] - 40,
          input$slider_zulassungsperiode[2] + 40
        )
      ) +
      # inline function to force breaks to integer values (https://stackoverflow.com/questions/44182709/r-shiny-dashboardsidebar-breaks-graphs-and-tables-width-when-toggling)
      scale_y_continuous(
        breaks = function(x)
          unique(floor(pretty(x)))
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.position = "bottom"
      )
  })
  
  # Render data tables: gemeinden / bauteile
  
  gemeinden <- all_vehicles %>%
    select(Gemeinde, PLZ, Werksnummer_Komponente) %>%
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
        lengthMenu = list(
          c(3, 6, 10, 20, 100, 1000),
          c('3', '6', '10', '20', '100', '1000')
        ),
        # layout breaks with three digit numbers in the list
        pageLength = 3,
        
        # Search wit regex Ja/Nein
        search = list(
          regex = FALSE,
          caseInsensitive = TRUE,
          search = "Koeln"
        ),
        # ä=ae, ö=oe, ü=ue
        
        # Define German translaton of data table UI
        language = list(
          info = 'Zeige  _START_ bis _END_ von insgesamt _TOTAL_ Ergebnissen',
          paginate = list(
            first = 'Erste',
            last = 'Letzte',
            previous = 'Zurück',
            `next` = 'Vor'
          ),
          infoEmpty = 'Keine Daten vorhanden',
          zeroRecords = 'Keine Ergebnisse gefunden',
          loadingRecords = 'Lädt...',
          processing = 'Ergebnisse werden geladen...',
          lengthMenu = 'Zeige _MENU_ Ergebnisse',
          infoFiltered =  '| Gefiltert von _MAX_ Einträgen',
          search = 'Suche:'
        )
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
      final_joined[, c(
        'ID_Einzelteil',
        'Werksnummer_Einzelteil',
        'Fehlerhaft_Einzelteil',
        'ID_Komponente',
        'Fehlerhaft_Komponente',
        'ID_Fahrzeug'
      )],
      
      options = list(
        pageLength = 3,
        lengthMenu = list(
          c(3, 6, 10, 20, 100, 1000),
          c('3', '6', '10', '20', '100', '1000')
        ),
        
        # Search options
        search = list(
          regex = FALSE,
          caseInsensitive = TRUE,
          search = ""
        ),
        # ä=ae, ö=oe, ü=ue
        
        # Define German translaton of data table UI
        language = list(
          info = 'Zeige  _START_ bis _END_ von insgesamt _TOTAL_ Ergebnissen',
          paginate = list(
            first = 'Erste',
            last = 'Letzte',
            previous = 'Zurück',
            `next` = 'Vor'
          ),
          infoEmpty = 'Keine Daten vorhanden',
          zeroRecords = 'Keine Ergebnisse gefunden',
          loadingRecords = 'Lädt...',
          processing = 'Ergebnisse werden geladen...',
          lengthMenu = 'Zeige _MENU_ Ergebnisse',
          infoFiltered =  '| Gefiltert von _MAX_ Einträgen',
          search = 'Suche:'
        )
      ),
      
      colnames = c(
        'ID_Werk' = 'Werksnummer_Einzelteil',
        'Fehlerhaft' = 'Fehlerhaft_Einzelteil',
        'Fehlerhaft' = 'Fehlerhaft_Komponente'
      ),
      
      rownames = FALSE
    ) %>%
      formatStyle(c('ID_Komponente', 'ID_Fahrzeug'), `border-left` = 'solid 1px')
  })
  
  #### Data Preparation for the rendering of (1) heatmap with (2) markers and (3) supply routes
  
  # 1. Create datapoints for the heatmap

  threshold_fehleranzahl <- 10 # recommended values: 1, 10, 20, 40, ...
  datapoints_heat <- reactive({
    subset(
      final_joined,
      Zulassungsdatum >= input$slider_zulassungsperiode[1] &
        Zulassungsdatum <= input$slider_zulassungsperiode[2]
    ) %>%
      group_by(Längengrad, Breitengrad) %>%
      summarise(fehleranzahl = n())  %>%
      ungroup()  %>%
      #select(-Gemeinde)  %>%
      filter(fehleranzahl > threshold_fehleranzahl)
  })
  
  # 2.
  # NFilter supply_routes data linked to table selections
  data_dots <- reactive({
    df = data.frame()
    
    if (!is.null(filtered_parts_limited())) {
      supply_routes <- filtered_parts_limited()
      
      df = data.frame(
        id = 1:nrow(supply_routes),
        lat_begin = supply_routes$Breitengrad_Einzelteil,
        lat_via = supply_routes$Breitengrad_Komponente,
        lat_end = supply_routes$Breitengrad,
        lng_begin = supply_routes$Längengrad_Einzelteil,
        lng_via = supply_routes$Längengrad_Komponente,
        lng_end = supply_routes$Längengrad,
        ID_Fahrzeug = supply_routes$ID_Fahrzeug
      )
      #df
    }
  })
  
  # Statistics for tier1 facility
  tier1_werke <- reactive({
    filtered_parts_limited() %>%
      select(
        ID_Fahrzeug,
        ID_Einzelteil,
        Fehlerhaft_Einzelteil,
        Werksnummer_Einzelteil,
        Breitengrad_Einzelteil,
        Längengrad_Einzelteil
      ) %>%
      group_by(Werksnummer_Einzelteil,
               Breitengrad_Einzelteil,
               Längengrad_Einzelteil) %>%
      summarise(
        'Einzelteile geliefert' = n(),
        
        'fehlerhaft laut Einzelteil-Werk' = length(Fehlerhaft_Einzelteil[Fehlerhaft_Einzelteil == 'Ja']),
        Einzelteile = substring(str_c(
          glue('<br>{ID_Einzelteil}'), collapse = ""
        ), 5),
        Fehlerhaft = toString(Fehlerhaft_Einzelteil) # if not converted from boolean then: toString(ifelse(Fehlerhaft_Einzelteil == 1, 'Ja', 'Nein'))
      ) %>%
      ungroup() %>%
      arrange(Fehlerhaft)
  })
  
  # Statistics for tier2 facility
  tier2_werke <- reactive({
    filtered_parts_limited() %>%
      select(
        ID_Fahrzeug,
        ID_Fahrzeug,
        ID_Komponente,
        Fehlerhaft_Einzelteil,
        Fehlerhaft_Komponente,
        Werksnummer_Komponente,
        Breitengrad_Komponente,
        Längengrad_Komponente
      ) %>%
      group_by(Werksnummer_Komponente,
               Breitengrad_Komponente,
               Längengrad_Komponente) %>%
      summarise(
        'Einzelteile erhalten' = n(),
        'fehlerhaft laut Einzelteil-Werk' = length(Fehlerhaft_Einzelteil[Fehlerhaft_Einzelteil == 'Ja']),
        'Defekte Sitze hergestellt' = sum(!duplicated(ID_Fahrzeug)),
        'fehlerhaft laut Komponenten-Werk' = length(Fehlerhaft_Komponente[Fehlerhaft_Komponente == 'Ja']),
        Komponenten = substring(str_c(
          glue('<br>{ID_Komponente}'), collapse = ""
        ), 5),
        Fehlerhaft = toString(Fehlerhaft_Komponente) # if not converted from boolean then: toString(ifelse(Fehlerhaft_Komponente == 1, 'Ja', 'Nein'))
      ) %>%
      ungroup()
  })
  
  # Colors for the supply routes (n = 10)
  colors_polyline <-
    c(
      "#c50e1f",
      "#7CAE00",
      "#00BFC4",
      "#C77CFF",
      "#D5BF86",
      "#541C52",
      "#CA6000",
      "#CAAC00",
      "#000000",
      "#FFFFFF"
    )
  
  # Render map
  output$map <- renderLeaflet({
    filtered_data_dots <- data_dots()
    leaflet_map <- leaflet(final_joined) %>%
      setView(lng = 10.46,
              lat = 51.15,
              zoom = 6.25) %>% # centered to Germany map
      # do not use:
      #fitBounds(min(final_joined$Längengrad, na.rm = TRUE),min(final_joined$Breitengrad, na.rm = TRUE),max(final_joined$Längengrad, na.rm = TRUE),max(final_joined$Breitengrad, na.rm = TRUE)) %>% # buggy after scaling
      addTiles() %>%
      
      # Layer 1: Heatmap
      addHeatmap(
        data = datapoints_heat(),
        lng = ~ Längengrad,
        lat = ~ Breitengrad,
        intensity = ~ fehleranzahl,
        blur = 12,
        max = 500,
        radius = 16,
        group = "Heatmap"
      ) %>% # intensity = ~fehleranzahl, blur = 14, max = 60, radius = 12) %>%
      
      
      # Layer 2: fehlerhafte Fahrzeuge
      addMarkers(
        data = filtered_vehicles(),
        ~ Längengrad,
        ~ Breitengrad,
        group = "Cluster Marker",
        #display large amounts of markers as clusters
        clusterOptions = markerClusterOptions(),
        popup = ~ paste(
          "<center><h5>Betroffenes Fahrzeug</h5></center>",
          "ID_Fahrzeug: ",
          ID_Fahrzeug,
          "<br/>",
          "ID_Sitz: ",
          ID_Komponente,
          "<br/>",
          "Baujahr: ",
          format(as.Date(Produktionsdatum_Fahrzeug), "%Y"),
          "<br/>",
          "Zulassung am: ",
          format(as.Date(Zulassungsdatum), "%d.%m.%Y"),
          "<br/>",
          "Zugelassen in: ",
          PLZ,
          " ",
          Gemeinde
        )
      )
    
    # Layer 3: Lieferwege
    # Render the polyroutes supply route
    if (!is.null(filtered_data_dots)) {
      for (i in 1:nrow(filtered_data_dots)) {
        leaflet_map <-
          addPolylines(
            leaflet_map,
            data = filtered_data_dots[i, ],
            group = "Lieferwege",
            lng = ~ c(lng_begin, lng_via, lng_end),
            lat = ~ c(lat_begin, lat_via, lat_end),
            color = colors_polyline[i %% 10],
            # iterates the 10 colors over and over again
            weight = 4,
            opacity = 0.5,
            fillColor = "#c50e1",
            fillOpacity = 0.5,
            smoothFactor = 1,
            popup = ~ ID_Fahrzeug,
            label = ~ ID_Fahrzeug,
            #labelOptions = NULL, options = pathOptions(),
            highlightOptions = highlightOptions(
              color = "white",
              weight = 2,
              bringToFront = TRUE
            )
          )
      }
      
      
      # Layer 3: Lieferwege
      
      # Add circles of facility
      facitily_group_name = "Lieferwege"
      # Einzelteil-Werk: Number of production errors Einzelteile hergestellt (schwarz)
      leaflet_map <- leaflet_map %>%
        
        #Display tier1 facilities with custom icon
        addMarkers(
          data = tier1_werke(),
          ~ Längengrad_Einzelteil,
          ~ Breitengrad_Einzelteil,
          group = facitily_group_name,
          icon = ~ icons(
            iconUrl = './Zusaetzliche_Dateien/facility_icon.png',
            iconWidth = 40,
            iconHeight = 25,
          ),
          #display large amounts of markers as clusters
          #clusterOptions = markerClusterOptions(freezeAtZoom = 7),
          popup = ~ paste(
            "<center><h5>Einzelteil-Werk</h5></center>",
            popupTable(
              tier1_werke(),
              feature.id = FALSE,
              row.numbers = FALSE,
              zcol = c(
                'Werksnummer_Einzelteil',
                'Einzelteile geliefert',
                'fehlerhaft laut Einzelteil-Werk',
                "Einzelteile",
                "Fehlerhaft"
              )
            )
          ),
          popupOptions = popupOptions(minWidth = 320, maxHeight = 500)
          
        )  %>%
        
        # Display tier2 facilities with custom icon
        addMarkers(
          data = tier2_werke(),
          ~ Längengrad_Komponente,
          ~ Breitengrad_Komponente,
          group = facitily_group_name,
          icon = ~ icons(
            iconUrl = './Zusaetzliche_Dateien/facility_icon.png',
            iconWidth = 40,
            iconHeight = 25,
          ),
          popup = ~ paste(
            "<center><h5>Komponenten-Werk</h5></center>",
            popupTable(
              tier2_werke(),
              feature.id = FALSE,
              row.numbers = FALSE,
              zcol = c(
                'Werksnummer_Komponente',
                'Einzelteile erhalten',
                'fehlerhaft laut Einzelteil-Werk',
                'Defekte Sitze hergestellt',
                'fehlerhaft laut Komponenten-Werk',
                'Komponenten',
                'Fehlerhaft'
              )
            )
          ),
          popupOptions = popupOptions(minWidth = 360, maxHeight = 500)
        )
      
      # Add marker for car location
      filtered_vehicles_tmp <- filtered_parts_limited()
      
      if (!is.null(filtered_parts_limited)) {
        for (i in 1:nrow(filtered_vehicles_tmp)) {
          leaflet_map <-
            addMarkers(
              leaflet_map,
              data = filtered_vehicles_tmp[i,],
              ~ Längengrad,
              ~ Breitengrad,
              group = "Lieferwege",
              icon = ~ icons(
                iconUrl = './Zusaetzliche_Dateien/marker_icon.png',
                iconWidth = 35,
                iconHeight = 40,
                iconAnchorX = 17,
                iconAnchorY = 40
              ),
              #display large amounts of markers as clusters
              clusterOptions = markerClusterOptions(),
              popup = ~ paste(
                "<center><h5>Betroffenes Fahrzeug</h5></center>",
                "ID_Fahrzeug: ",
                ID_Fahrzeug,
                "<br/>",
                "ID_Sitz: ",
                ID_Komponente,
                "<br/>",
                "Baujahr: ",
                format(as.Date(Produktionsdatum_Fahrzeug), "%Y"),
                "<br/>",
                "Zulassung am: ",
                format(as.Date(Zulassungsdatum), "%d.%m.%Y"),
                "<br/>",
                "Zugelassen in: ",
                PLZ,
                " ",
                Gemeinde
              )
            )
        }
      }
    }
    
    # Layer control
    leaflet_map <- leaflet_map %>%
      addLayersControl(
        overlayGroups = c("Heatmap", "Cluster Marker", "Lieferwege"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Heatmap")
    
    # Return leaflet_map with all layers to render_leaflet()
    leaflet_map
  })
  
  # Render full database
  output$datatable_final_joined <- renderDataTable({
    input$reset_filters
    datatable(
      final_joined[, c(
        'ID_Einzelteil',
        'Werksnummer_Einzelteil',
        'Fehlerhaft_Einzelteil',
        'ID_Komponente',
        'Werksnummer_Komponente',
        'Fehlerhaft_Komponente',
        'ID_Fahrzeug',
        'Werksnummer_Fahrzeug',
        'Produktionsdatum_Fahrzeug',
        'Zulassungsdatum',
        'Gemeinde',
        'PLZ'
      )],
      
      filter = list(position = 'top', clear = TRUE),
      
      options = list(
        pageLength = 10,
        lengthMenu = list(
          c(3, 10, 20, 100, 1000, 10000),
          c('3', '10', '20', '100', '1000', '10000')
        ),
        
        # Search wit regex Ja/Nein
        search = list(
          regex = FALSE,
          caseInsensitive = TRUE,
          search = ""
        ),
        # ä=ae, ö=oe, ü=ue
        
        # Define German translaton of data table UI
        language = list(
          info = 'Zeige  _START_ bis _END_ von insgesamt _TOTAL_ Ergebnissen',
          paginate = list(
            first = 'Erste',
            last = 'Letzte',
            previous = 'Zurück',
            `next` = 'Vor'
          ),
          infoEmpty = 'Keine Daten vorhanden',
          zeroRecords = 'Keine Ergebnisse gefunden',
          loadingRecords = 'Lädt...',
          processing = 'Ergebnisse werden geladen...',
          lengthMenu = 'Zeige _MENU_ Ergebnisse',
          infoFiltered =  '| Gefiltert von _MAX_ Einträgen',
          search = 'Suche:'
        )
      ),
      
      colnames = c(
        'ID_Werk' = 'Werksnummer_Einzelteil',
        'ID_Werk' = 'Werksnummer_Komponente',
        'ID_Werk' =  'Werksnummer_Fahrzeug',
        'Fehlerhaft' = 'Fehlerhaft_Einzelteil',
        'Fehlerhaft' = 'Fehlerhaft_Komponente'
      ),
      
      rownames = FALSE
    ) %>%
      formatStyle(c('ID_Komponente', 'ID_Fahrzeug'), `border-left` = 'solid 1px',)
  })
  
  # resetting map position
  observe({
    input$reset
    leafletProxy("map") %>%
      setView(lng = 10.46,
              lat = 51.15,
              zoom = 6.25)
  })
  
  
  # reset sliderInput for zulassungen period
  observeEvent(
    input$reset_filters,
    updateSliderInput(session, 'slider_zulassungsperiode',
                      value = c(
                        min(all_vehicles$Zulassungsdatum),
                        max(all_vehicles$Zulassungsdatum)
                      ))
  )
  
  # Overview for car owner with infos about their vehicle and the defective parts
  output$result_text <-
    renderText({
      "Geben Sie ihre Fahrzeug ID in die Suche ein um zu überprüfen ob ihr Fahrzeug betroffen ist."
    })
  
  observeEvent(input$vehicle_filter_submit, {
    vehicle_parts <- filter(final_joined, ID_Fahrzeug == input$vehicle_id_input)
    if (nrow(vehicle_parts) >= 1) {
      output$result_text <- renderText({"Ihr Fahrzeug ist betroffen"})
      #vehicle <- vehicle_parts[!duplicated(vehicle_parts$ID_Fahrzeug)]
      vehicle <- vehicle_parts[1,]
      vehicle_info_string <-
      glue(
        "Ihr Fahrzeug (ID: {vehicle$ID_Fahrzeug}), zugelassen am {format(vehicle$Zulassungsdatum, '%d.%m.%Y')} in {vehicle$PLZ} {vehicle$Gemeinde},
wurde am {format(vehicle$Produktionsdatum_Fahrzeug, '%d.%m.%Y')} im Werk {vehicle$Werksnummer_Fahrzeug} gebaut.
Folgend finden sie die Auflistung zu der verbauten Sitzgruppe, sowie zu den dafür verwendeten Einzelteilen
zusammen mit den Werksnummern bei denen Ihre Servicewerkstatt Ersatzteile anfordern kann."
        )
      output$vehicle_info_text <- renderText(vehicle_info_string)
      
      # components
      output$components_list <-
        renderTable(vehicle_parts[, c("ID_Komponente",
                                      "Fehlerhaft_Komponente",
                                      "Werksnummer_Komponente")], digits = 0)
      # parts
      output$parts_list <-
        renderTable(vehicle_parts[, c("ID_Einzelteil",
                                      "Fehlerhaft_Einzelteil",
                                      "Werksnummer_Einzelteil")], digits = 0)
      
    } else {
      output$result_text <- renderText({
        "ID exisitiert nicht."
      })
      output$vehicle_info_text <- NULL
      output$components_list <- NULL
      output$parts_list <- NULL
    }
  })
}

# Shiny App starten
shinyApp(ui, server)