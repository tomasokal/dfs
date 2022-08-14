box::use(
    shiny[moduleServer, NS, reactive, uiOutput, numericInput, renderUI],
    dplyr[select, pull]
)

box::use(
  app/logic/slateInformation[get_data],
)

#' @export
ui <- function(id, data) {

  ns <- NS(id)
  numericInput(
    NS(id, "input_value"),
    label = "Projected Points",
    value = 0,
    min = 0,
    max = 99,
    step = 0.1
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$input_value)
  })
}
