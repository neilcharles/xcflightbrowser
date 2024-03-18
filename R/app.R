library(shiny)
library(bslib)
library(waiter)

run_app <- function(...) {
  ui <- page_sidebar(

    use_waiter(),

    autoWaiter(),

    theme = bs_theme(version = 5, bootswatch = "zephyr"),

    title = "XC Flight Browser",

    sidebar = sidebar(flightsMapFiltersUI("mainMap"), position = "right", open = TRUE),

    flightsMapUI("mainMap")
  )

  server <- function(input, output, session) {
    flightsMapServer("mainMap")
  }

  shinyApp(ui, server)
}

