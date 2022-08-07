box::use(
  shiny[bootstrapPage, moduleServer, NS, reactive, titlePanel, div, img],
)
box::use(
  app/logic/slateInformation[get_data],
  app/view[table],
)

grid <- function(...) div(class = "grid", ...)
card <- function(...) div(class = "card", ...)
main <- function(...) div(class = "main", ...)
logo <- function(...) div(class = "logo", ...)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    main(
      titlePanel("DFS Linear Programming Application Redesign"),
      grid(
        card(table$ui(ns("table")))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(get_data())
    table$server("table", data)
  })
}
