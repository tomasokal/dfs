box::use(
  reactable[reactableOutput, renderReactable, updateReactable],
  shiny[moduleServer, NS, observe, observeEvent, invalidateLater, showModal, modalDialog, reactiveValues], # nolint
)

box::use(
  app/logic/playerTable[table],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  reactableOutput(ns("table"))
}

#' @export
server <- function(id, data, selections_player, selections_value, selections_edit, selections_reset, selections_reset_all) { # nolint
  moduleServer(id, function(input, output, session) {

    # Get player data and set to reactiveValue object
    player_data <- reactiveValues(data = data)

    # Generate table with reactiveValue data
    output$table <- renderReactable(
      table(player_data$data)
    )

    # If user presses edit, set value of players projection to numeric value they have chosen.
    observeEvent(selections_edit(), {
      player_name <- selections_player()
      player_value <- selections_value()
      player_data$data[shortName == player_name]$projections_edit <- player_value
      updateReactable("table", data = player_data$data)
    })

    # If user presses reset, set value back to stored true value.
    observeEvent(selections_reset(), {
      player_name <- selections_player()
      player_data$data[shortName == player_name]$projections_edit <- player_data$data[shortName == player_name]$projections_true # nolint
      updateReactable("table", data = player_data$data)
    })

    # If user presses reset all, set value of all players back to stored true values.
    observeEvent(selections_reset_all(), {
      player_data$data$projections_edit <- player_data$data$projections_true
      updateReactable("table", data = player_data$data)
    })

  })
}
