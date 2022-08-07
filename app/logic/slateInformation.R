box::use(
  data.table[fread],
  dplyr[select],
)

#' @export
get_data <- function() {
  data <- fread(
    "https://raw.githubusercontent.com/tomasokal/dfs/production/Output/week1_2022.csv" # nolint
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
