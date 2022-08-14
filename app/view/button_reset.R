box::use(
    shiny[moduleServer, NS, reactive, actionButton],
)

#' @export
ui <- function(id) {
  actionButton(NS(id, "button_reset"), label = "Reset value")
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$button_reset)
  })
}
