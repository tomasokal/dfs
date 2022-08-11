box::use(
  dplyr[pull],
  reactable[reactableOutput, renderReactable],
  shiny[NS, actionButton, fluidRow, moduleServer, renderUI, tagList, tags, uiOutput],
)

box::use(
  app/logic/slateInformation[get_games],
)

#' @export
ui <- function(id, label) {
  ns <- NS(id)
  uiOutput(ns("filter_games"))
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    output$filter_games <- renderUI(
      tags$div(
        tags$button("test1"),
        tags$button("test2"),
        tags$button(
          id="button1",
          "test3"
        )
      )
    )
  })
}
