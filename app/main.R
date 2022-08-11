box::use(
  shiny[bootstrapPage, moduleServer, NS, reactive, titlePanel, div, img, tags],
)
box::use(
  app/logic/slateInformation[get_data, get_games],
  app/view[table],
  app/view[selections],
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
      tags$input(
        type = "checkbox",
        id = "check1",
        class = "toggle",
        label = "check?"
      ),
      tags$label(
        `for` = "check1",
        "Checked?"
      ),
      grid(
        card(selections$ui(ns("filters_games"))),
        card(table$ui(ns("table")))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(get_data())
    games <- reactive(get_games())
    selections$server("filters_games", games)
    table$server("table", data)
  })
}
