function_scrape_schedule <- function( x ) {
  
  # Source of data
  url <- paste0("https://nflgamedata.com/schedule.php?season=2021&amp;week=", x)
  page <- rvest::html_table(xml2::read_html(url)
                            , header = TRUE
                            , fill = TRUE)
  
  # Parse data
  scrape_schedule <- data.table::data.table(page[[6]])
  
  # Remove headers
  scrape_schedule <- scrape_schedule[-1, ]
  
  # Change column names
  colnames(scrape_schedule) <- paste0("v", 1:ncol(scrape_schedule))
  
  # Change NA to 0
  scrape_schedule$v12[is.na(scrape_schedule$v12)] <- 0
  scrape_schedule$v14[is.na(scrape_schedule$v14)] <- 0
  
  # Select features
  scrape_schedule <- scrape_schedule[, .(DAY = v4
                                         , DATE = v5
                                         , TIME = v6
                                         , TEAM_AWAY = v9
                                         , TEAM_HOME = v17
                                         , OVERUNDER = v20
                                         , ODDS_HOME = v14 - v12
                                         )
                                     ]
  
  return(scrape_schedule)
  
  
}
