box::use(
  data.table[fcase],
  dplyr[select],
  htmltools[span],
  htmlwidgets[JS],
  reactable[reactable, colDef, colFormat],
  shiny[img, tagList, div],
)

#' @export
turn_team_to_logo <- function(value) {
  value <- fcase(
    value == "ARI", "static/logo/nfl-arizona-cardinals-team-logo-2.png",
    value == "ATL", "static/logo/nfl-atlanta-falcons-team-logo-2.png",
    value == "BAL", "static/logo/nfl-baltimore-ravens-team-logo-2.png",
    value == "BUF", "static/logo/nfl-buffalo-bills-team-logo-2.png",
    value == "CAR", "static/logo/nfl-carolina-panthers-team-logo-2.png",
    value == "CHI", "static/logo/nfl-chicago-bears-team-logo-2.png",
    value == "CIN", "static/logo/nfl-cincinnati-bengals-team-logo.png",
    value == "CLE", "static/logo/nfl-cleveland-browns-team-logo-2.png",
    value == "DAL", "static/logo/nfl-dallas-cowboys-team-logo-2.png",
    value == "DEN", "static/logo/nfl-denver-broncos-team-logo-2.png",
    value == "DET", "static/logo/nfl-detroit-lions-team-logo-2.png",
    value == "GB", "static/logo/nfl-green-bay-packers-team-logo-2.png",
    value == "HOU", "static/logo/nfl-houston-texans-team-logo-2.png",
    value == "IND", "static/logo/nfl-indianapolis-colts-team-logo-2.png",
    value == "JAX", "static/logo/nfl-jacksonville-jaguars-team-logo-2.png",
    value == "KC", "static/logo/nfl-kansas-city-chiefs-team-logo-2.png",
    value == "LA", "static/logo/los-angeles-rams-2020-logo.png",
    value == "LAC", "static/logo/nfl-los-angeles-chargers-team-logo-2.png",
    value == "LV", "static/logo/nfl-oakland-raiders-team-logo.png",
    value == "MIA", "static/logo/nfl-miami-dolphins-logo-2018.png",
    value == "MIN", "static/logo/nfl-minnesota-vikings-team-logo-2.png",
    value == "NE", "static/logo/nfl-new-england-patriots-team-logo-2.png",
    value == "NO", "static/logo/nfl-new-orleans-saints-team-logo-2.png",
    value == "NYG", "static/logo/nfl-new-york-giants-team-logo-2.png",
    value == "NYJ", "static/logo/nfl-new-york-jets-team-logo.png",
    value == "PHI", "static/logo/nfl-philadelphia-eagles-team-logo-2.png",
    value == "PIT", "static/logo/nfl-pittsburgh-steelers-team-logo-2.png",
    value == "SEA", "static/logo/nfl-seattle-seahawks-team-logo-2.png",
    value == "SF", "static/logo/nfl-san-francisco-49ers-team-logo-2.png",
    value == "TB", "static/logo/tampa-bay-buccaneers-2020-logo.png",
    value == "TEN", "static/logo/nfl-tennessee-titans-team-logo-2.png",
    value == "WAS", "static/logo/washington-commanders-logo.png",
    default = "static/logo/nfl-league-logo.png"
  )

  return(value)
}

#' @export
turn_team_to_team_name <- function(value) {
  value <- fcase(
    value == "ARI", "Cardinals",
    value == "ATL", "Falcons",
    value == "BAL", "Baltimore ",
    value == "BUF", "Bills",
    value == "CAR", "Panthers",
    value == "CHI", "Bears",
    value == "CIN", "Bengals",
    value == "CLE", "Browns",
    value == "DAL", "Cowboys",
    value == "DEN", "Broncos",
    value == "DET", "Lions",
    value == "GB",  "Packers",
    value == "HOU", "Texans",
    value == "IND", "Colts",
    value == "JAX", "Jaguars",
    value == "KC",  "Chiefs",
    value == "LA",  "Rams",
    value == "LAC", "Chargers",
    value == "LV",  "Raiders",
    value == "MIA", "Dolphins",
    value == "MIN", "Vikings",
    value == "NE",  "Patriots",
    value == "NO",  "Saints",
    value == "NYG", "Giants",
    value == "NYJ", "Jets",
    value == "PHI", "Eagles",
    value == "PIT", "Steelers",
    value == "SEA", "Seahawks",
    value == "SF",  "49ers",
    value == "TB",  "Buccaneers",
    value == "TEN", "Titans",
    value == "WAS", "Commanders",
    default = "NULL Team"
  )

  return(value)
}

#' @export
table <- function(data) {
  data$team_name <- turn_team_to_team_name(data$teamAbbreviation)
  data$team_image <- turn_team_to_logo(data$teamAbbreviation)

  data <- data |>
    select(shortName, position, team_name, team_image, name, salary, jdt_players_points)

  reactable(
    data,
    searchable = TRUE,
    resizable = TRUE,
    paginationType = "simple",
    highlight = TRUE,
    compact = TRUE,
    height = 500,
    columns = list(
      shortName = colDef(
        name = "Player",
        cell = function(value, index) {
          team_image <- data$team_image[index]
          image <- img(
            src = sprintf(team_image),
            style = "height: 24px;",
            alt = team_image
          )
          tagList(
            div(style = "display: inline-block; width: 45px", image),
            value
          )
        }
      ),
      position = colDef(
        name = "Position"
      ),
      team_name = colDef(
        show = FALSE
      ),
      team_image = colDef(
        show = FALSE
      ),
      name = colDef(
        name = "Game"
      ),
      salary = colDef(
        name = "Salary",
        format = colFormat(
          prefix = "$",
          separators = TRUE,
          digits = 0
        )
      ),
      jdt_players_points = colDef(
        name = "Exp. Pts.",
        format = colFormat(
          digits = 0
        )
      )
    )
  )
}
