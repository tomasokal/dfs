box::use(
  shiny[bootstrapPage, moduleServer, NS, reactive, titlePanel, div],
)
box::use(
  app/logic/players,
  app/view[chart, table],
)

grid <- function(...) div(class = "grid", ...)
card <- function(...) div(class = "card", ...)
main <- function(...) div(class = "main", ...)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    main(
      titlePanel("Application Title"),
      grid(
        card(table$ui(ns("table"))),
        card(chart$ui(ns("chart")))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(players$get_data())
    table$server("table", data)
    chart$server("chart", data)
  })
}