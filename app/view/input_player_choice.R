box::use(
    shiny[moduleServer, NS, reactive, uiOutput, selectInput, renderUI],
    dplyr[select, pull]
)

box::use(
  app/logic/slateInformation[get_data],
)

#' @export
ui <- function(id, data) {

  dt_p <- get_data() |> select(shortName)

  ns <- NS(id)
  selectInput(
    NS(id, "input_player"),
    label = "Player",
    choices = dt_p,
    multiple = FALSE,
    selected = NULL
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$input_player)
  })
}
