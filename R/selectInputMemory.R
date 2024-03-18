selectInputMemoryUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("select"))
  )
}

selectInputMemoryServer <- function(id, name, choices, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$select <- shiny::renderUI({
        selected <- if(is.null(input$select)){
          NULL
        } else if (all(input$select %in% choices)) {
          input$select
        } else {
          NULL
        }

        shiny::selectInput(ns("select"), name, choices = choices, selected = selected, ...)

      })

    }
  )
}
