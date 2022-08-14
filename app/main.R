box::use(
  shiny[bootstrapPage, moduleServer, NS, reactive, titlePanel, div, img, tags, actionButton, fluidRow], #nolint
)
box::use(
  app/logic/slateInformation[get_data, get_games],
  app/view/button_edit,
  app/view/button_reset,
  app/view/button_reset_all,
  app/view/input_player_choice,
  app/view/input_player_value,
  app/view/table,
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
        card(
          input_player_choice$ui(ns("selections_player")),
          input_player_value$ui(ns("selections_value")),
          button_edit$ui(ns("selections_edit")),
          button_reset$ui(ns("selections_reset")),
          button_reset_all$ui(ns("selections_reset_all"))
        ),
        card(table$ui(ns("table")))
      )
    )
  )
}

#' @export
server <- function(id) {

  moduleServer(id, function(input, output, session) {

    player_data <- get_data()
    selections_player <- input_player_choice$server("selections_player")
    selections_value <- input_player_value$server("selections_value")
    selections_edit <- button_edit$server("selections_edit")
    selections_reset <- button_reset$server("selections_reset")
    selections_reset_all <- button_reset_all$server("selections_reset_all")

    table$server(
      "table",
      player_data,
      selections_player,
      selections_value,
      selections_edit,
      selections_reset,
      selections_reset_all
    )

  })

}
