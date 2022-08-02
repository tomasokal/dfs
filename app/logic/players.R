box::use(
    data.table,
    echarts4r,
    htmlwidgets[JS],
    reactable[reactable],
)

 #' @export
get_data <- function() {

    .d <- `[`

    data.table::fread(
        "https://raw.githubusercontent.com/tomasokal/dfs/production/Output/salaries_projections_main_slate.csv" # nolint
    )  |>
    .d(, .(PLAYER, TEAM, POSITION, POINTS_DK, SALARY_DK))

}

#' @export
table <- function(data) {
    data |>
        reactable()
}

#' @export
chart <- function(data) {
    data |>
        echarts4r$e_chart(POINTS_DK) |>
        echarts4r$e_scatter(SALARY_DK) |>
        echarts4r$e_y_axis(SALARY_DK, formatter = JS("App.formatSalary")) |>
        echarts4r$e_tooltip()
}
