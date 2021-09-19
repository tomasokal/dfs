function_scrape_schedule <- function( x ) {
  
  # Source of data
  url <- paste0("https://nflgamedata.com/schedule.php?season=2021&week=", x)
  page <- rvest::html_table(xml2::read_html(url)
                            , header = TRUE
                            , fill = TRUE)
  
  # Parse data
  scrape_schedule <- data.table::data.table(page[[6]])
  
  # Change column names
  colnames(scrape_schedule) <- paste0("v", 1:ncol(scrape_schedule))
  
  # Change NA to 0
  scrape_schedule$v12[is.na(scrape_schedule$v12) | scrape_schedule$v12 == "PK"] <- 0
  scrape_schedule$v14[is.na(scrape_schedule$v14) | scrape_schedule$v14 == "PK"] <- 0
  
  # Force numeric
  scrape_schedule <- scrape_schedule[, v14 := as.numeric(as.character(v14))]
  
  # Select features
  scrape_schedule <- scrape_schedule[, ODDS_HOME := rowSums(.SD, na.rm = TRUE), .SDcols = c(12, 14)]
  scrape_schedule <- scrape_schedule[, .(WEEK = paste0('WEEK ', x)
                                         , DAY = v4
                                         , DATE = as.Date(v5, format = "%m/%d")
                                         , TIME = v6
                                         , TEAM_AWAY = v9
                                         , TEAM_HOME = v17
                                         , OVERUNDER = v20
                                         , ODDS_HOME
                                         )
                                     ]
  
  # Remove teams with byes
  scrape_schedule[scrape_schedule==''] <- NA
  scrape_schedule <- scrape_schedule[!is.na(DAY), ]
  
  return(scrape_schedule)
  
}

# Load over all weeks and filter out rows that are in the past. 
scrape_schedule <- data.table::rbindlist(lapply(seq(1:18), function_scrape_schedule))[DATE>=(Sys.Date()), ]

# Write to csv file
data.table::fwrite(scrape_schedule, 'data/_export/schedule.csv')

