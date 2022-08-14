box::use(
    shiny[moduleServer, NS, reactive, actionButton],
)

#' @export
ui <- function(id) {
  actionButton(NS(id, "button_edit"), label = "Edit value")
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$button_edit)
  })
}
