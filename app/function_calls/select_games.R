select_games <- function() {

    # Get date
    v_date <- Sys.Date()

    # Pull all games
    d_schedule <- data.table::as.data.table(schedule)
    d_schedule <- d_schedule[DATE <= v_date, ]
    d_schedule <- d_schedule[WEEK == min(WEEK), ]

    d_teams_playing <- c(d_schedule$TEAM_AWAY, d_schedule$TEAM_HOME)

    return(d_teams_playing)

}

get_games <- function() {

    # Get date
    v_date <- Sys.Date()

    # Pull all games
    d_schedule <- data.table::as.data.table(schedule)
    d_schedule <- d_schedule[DATE <= v_date, ]
    d_schedule <- d_schedule[WEEK == min(WEEK), ]

    d_games <- d_schedule[, GAMES := paste0(TEAM_AWAY, ' @ ', TEAM_HOME)][, GAMES]

    return(d_games)

}
    
