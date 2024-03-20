library(shiny)
library(bslib)
library(waiter)
library(sf) #loading prevents a weird clash with dplyr

run_app <- function(...) {
  ui <- page_sidebar(

    use_waiter(),

    autoWaiter(),

    theme = bs_theme(version = 5, bootswatch = "zephyr"),

    title = "XC Flight Browser",

    sidebar = sidebar(flightsMapFiltersUI("mainMap"), position = "right", open = TRUE, width = 350),

    flightsMapUI("mainMap")
  )

  server <- function(input, output, session) {
    flightsMapServer("mainMap")
  }

  shinyApp(ui, server)
}

