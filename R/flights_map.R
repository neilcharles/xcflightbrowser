#  --------------- UI ----------------------------------------------------------

# Map UI Output
flightsMapUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
      leaflet::leafletOutput(ns("map"))
  )
}

# Controls UI output
flightsMapFiltersUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    column(10,
    shiny::uiOutput(ns("select_dates"))
    ),
    shiny::uiOutput(ns("select_distance")),
    shiny::uiOutput(ns("select_site")),
    shiny::uiOutput(ns("select_en")),
    shiny::uiOutput(ns("select_pilot")),
    shiny::uiOutput(ns("select_wing")),
    shiny::uiOutput(ns("select_flight_type")),
    shiny::textOutput(ns("filter_warning"))
  )
}


#  --------------- server ------------------------------------------------------

flightsMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    map_limit <- 5000

    ns <- session$ns

    flights_filtered <- reactive({

      req(input$ui_select_dates)

      flights_filtered <- flights_sf |>
        dplyr::filter(!is.na(distance_via_turnpoints))

      # Date range
      flights_filtered <- flights_filtered |>
        dplyr::filter(flight_date >= as.character(input$ui_select_dates[1]),
                      flight_date <= as.character(input$ui_select_dates[2]))

      # Distance
      flights_filtered <- flights_filtered |>
        dplyr::filter(distance_via_turnpoints >= input$ui_select_distance)

      # Pilot
      if(!is.null(input$ui_select_pilot)){
        flights_filtered <- flights_filtered |>
          dplyr::filter(pilot_name %in% input$ui_select_pilot)
      }

      # EN
      if(!is.null(input$ui_select_en)){
        flights_filtered <- flights_filtered |>
          dplyr::filter(en_rating %in% input$ui_select_en)
      }

      # Site
      if(!is.null(input$ui_select_site)){
        flights_filtered <- flights_filtered |>
          dplyr::filter(takeoff_name %in% input$ui_select_site)
      }

      # Wing
      if(!is.null(input$ui_select_wing)){
        flights_filtered <- flights_filtered |>
          dplyr::filter(glider_name %in% input$ui_select_wing)
      }

      # Flight type
      if(!is.null(input$ui_select_flight_type)){
        flights_filtered <- flights_filtered |>
          dplyr::filter(flight_type %in% input$ui_select_flight_type)
      }

      validate(
        need(nrow(flights_filtered)>0, "No flights.")
      )


      flights_filtered
    })

    # Main map
    output$map <- leaflet::renderLeaflet({

      pal <-
        leaflet::colorBin(c("#4D4D4D", "darkorange", "red", "blue"), bins = c(0,50,100,200,1000), reverse = FALSE)

      map_out <- flights_filtered() |>
        dplyr::top_n(map_limit, distance_via_turnpoints) |>
        leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldShadedRelief, options = leaflet::providerTileOptions(opacity = 1)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, options = leaflet::providerTileOptions(opacity = 0.6)) |>
        leaflet::addPolylines(
          weight = 1.5,
          opacity = 0.6,
          color = ~ pal(distance_via_turnpoints),
          popup = ~ map_popup,
          highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, opacity = 1, weight = 3, sendToBack = FALSE, color = "blue")
        )

      map_out

    })

    # Controls
    output$select_dates <- shiny::renderUI({
      shiny::sliderInput(ns("ui_select_dates"), "Date Range", value = c(lubridate::floor_date(max(as.Date(flights_sf$flight_date)), 'year') - lubridate::years(1), max(as.Date(flights_sf$flight_date))), min = min(as.Date(flights_sf$flight_date)), max = max(as.Date(flights_sf$flight_date)))
    })

    output$select_distance <- shiny::renderUI({
      shiny::sliderInput(ns("ui_select_distance"), "Minimum Distance", value = 0, min = 0, max = max(flights_sf$distance_via_turnpoints, na.rm = TRUE))
    })

    output$select_site <- shiny::renderUI({
      shiny::selectInput(ns("ui_select_site"), "Takeoff Site", sort(unique(flights_sf$takeoff_name)), multiple = TRUE)
    })

    output$select_en <- shiny::renderUI({
      shiny::selectInput(ns("ui_select_en"), "EN Rating",  sort(unique(flights_sf$en_rating)), multiple = TRUE)
    })

    output$select_pilot <- shiny::renderUI({
      shiny::selectInput(ns("ui_select_pilot"), "Pilot",  sort(unique(flights_sf$pilot_name)), multiple = TRUE, selectize = TRUE)
    })

    output$select_wing <- shiny::renderUI({
      shiny::selectInput(ns("ui_select_wing"), "Wing", sort(unique(flights_sf$glider_name)), multiple = TRUE)
    })

    output$select_flight_type <- shiny::renderUI({
      shiny::selectInput(ns("ui_select_flight_type"), "Flight Type", sort(unique(flights_sf$flight_type)), multiple = TRUE)
    })

    output$filter_warning <- shiny::renderText({

      if(nrow(flights_filtered())>map_limit){
        return(glue::glue("Too many flights to display, limiting map to the top {map_limit} longest distances."))
      } else {
        return("")
      }
    })

  })
}
