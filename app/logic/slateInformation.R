box::use(
  data.table[fread],
  dplyr[select, mutate],
)

#' @export
get_data <- function() {
  data <- fread(
    "https://raw.githubusercontent.com/tomasokal/dfs/production/Output/week1_2022.csv" # nolint
  )[1:10, ] |>
  mutate(
    projections_true = jdt_players_points,
    projections_edit = jdt_players_points
  )

  return(data)
}

#' @export
get_games <- function() {
  games <- get_data() |>
    select(name) |>
    unique() |>
    as.data.frame()

  return(games)
}
