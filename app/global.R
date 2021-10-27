library(shiny)

checkboxinput <- function(id) {
  shiny::checkboxInput(inputId = id, label = id, value = FALSE)
}

schedule <- readRDS('data/schedule.rds')

get_games <- function() {

    # Get date
    v_date <- Sys.Date()

    # Pull all games
    d_schedule <- data.table::as.data.table(schedule)
    d_schedule <- d_schedule[DATE >= v_date, ]
    d_schedule <- d_schedule[WEEK == min(WEEK), ]

    d_games <- d_schedule[, GAMES := paste0(TEAM_AWAY, ' @ ', TEAM_HOME)][, GAMES]

    return(d_games)

}

d_games <- get_games()
l_checkboxinput_games <- lapply(d_games, checkboxinput)

ui <- fluidPage(
  
  fluidRow(
    
    checkboxinput('thursday'),
    checkboxinput('sunday_noon'),
    checkboxinput('sunday_afternoon'),
    checkboxinput('sunday_night'),
    checkboxinput('monday')
    
  ),

  fluidRow(

    l_checkboxinput_games

  )
  
)

server <- function(input, output) {}

shinyApp(ui, server)