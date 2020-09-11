library(data.table)
library(xml2)
library(rvest)

# Generate list of webpages to scrape

web_page_generate <- function(position, week) {
  
  web_page <- paste0("https://www.fantasypros.com/nfl/projections/"
                     , position
                     , ".php?week="
                     , week)
  
  return(web_page)
  
}

position_list <- list(c("qb", "rb", "wr", "te", "k", "dst"))

web_pages_list <- lapply(position_list, web_page_generate, week = 1)
web_pages_list <- as.list(unlist(web_pages_list))

# Generate list of scraped tables

scrape_projections <- function(web_page) {
  
  projections_table <- rvest::html_table(xml2::read_html(web_page)
                                         , header = TRUE
                                         , fill = TRUE)
  
}

projections_table_list <- lapply(web_pages_list, scrape_projections)

# Reshape and combine list of scraped webpages

combine_projections <- function(projection_table) {
  
  projection_table <- data.table::data.table(projection_table[[1]])
  
  if (projection_table[1, 1] == "Player") {
    
    projection_table <- projection_table[-1, ]
    
  }
  
  else {
    
    projection_table <- projection_table
    
  }
  
  if (ncol(projection_table) == 10) {
    
    projection_table <- projection_table[
      
      ,
      j = .(PLAYER = projection_table[, 1]
            , POINTS = projection_table[, 10])
      
      ]
    
  }
  
  else {
    
    last_column <- ncol(projection_table)
    
    projection_table <- projection_table[
      
      ,
      j = .(PLAYER = projection_table[, 1]
            , POINTS = projection_table[, ncol(projection_table), with = FALSE])
      
      ]
    
  }
  
  return(projection_table)
  
}

projection_table_list1 <- lapply(projections_table_list, combine_projections)
position_vec <- c("QB", "RB", "WR", "TE", "K", "DST")
projection_table_list2 <- mapply(cbind, projection_table_list1, "POSITION" = position_vec, SIMPLIFY = F)

projection_table_combined <- data.table::rbindlist(projection_table_list2)

LEFT = function(x,n){
  substring(x,1,nchar(x)-n)
}

RIGHT = function(x,n){
  substring(x,nchar(x)-n+1)
}

DST = function(x) {
  
  x <- switch(x
         , "Buffalo Bills" = "BUF"
         , "New England Patriots" = "NE"
         , "Pittsburgh Steelers" = "PIT"
         , "Philadelphia Eagles" = "PHI"
         , "Baltimore Ravens" = "BAL"
         , "Indianpolis Colts" = "IND"
         , "San Francisco 49ers" = "SF"
         , "Kansas City Chiefs" = "KC"
         , "Denver Broncos" = "DEN"
         , "Los Angeles Chargers" = "LAC"
         , "Cincinnati Bengals" = "CIN"
         , "Tennessee Titans" = "TEN"
         , "Las Vegas Raiders" = "LV"
         , "New York Jets" = "NYJ"
         , "Arizona Cardinals" = "ARI"
         , "Atlanta Falcons" = "ATL"
         , "Green Bay Packers" = "GB"
         , "Detroit Lions" = "DET"
         , "Los Angeles Rams" = "LAR"
         , "Miami Dolphins" = "MIA"
         , "Chicago Bears" = "CHI"
         , "Dallas Cowboys" = "DAL"
         , "Seattle Seahawks" = "SEA"
         , "Washington Football Team" = "WAS"
         , "Caroline Panthers" = "CAR"
         , "Cleveland Browns" = "CLE"
         , "Minnesota Vikings" = "MIN"
         , "New Orleans Saints" = "NO"
         , "Jacksonville Jaguars" = "JAC"
         , "Tampa Bay Buccaneers" = "TB"
         , "New York Giants" = "NYG"
         , "Houston Texans" = "HOU")
  
  return(class(x))
  
}

DST("Buffalo Bills")

final_projections <- projection_table_combined[
  
  ,
  j = .(POSITION
        , PLAYER_NAME = ifelse(POSITION == "DST", PLAYER.V1, trimws(LEFT(PLAYER.V1, 3)))
        , TEAM = ifelse(POSITION == "DST", sapply(PLAYER.V1
                                                  , switch
                                                  , "Buffalo Bills" = "BUF"
                                                  , "New England Patriots" = "NE"
                                                  , "Pittsburgh Steelers" = "PIT"
                                                  , "Philadelphia Eagles" = "PHI"
                                                  , "Baltimore Ravens" = "BAL"
                                                  , "Indianpolis Colts" = "IND"
                                                  , "San Francisco 49ers" = "SF"
                                                  , "Kansas City Chiefs" = "KC"
                                                  , "Denver Broncos" = "DEN"
                                                  , "Los Angeles Chargers" = "LAC"
                                                  , "Cincinnati Bengals" = "CIN"
                                                  , "Tennessee Titans" = "TEN"
                                                  , "Las Vegas Raiders" = "LV"
                                                  , "New York Jets" = "NYJ"
                                                  , "Arizona Cardinals" = "ARI"
                                                  , "Atlanta Falcons" = "ATL"
                                                  , "Green Bay Packers" = "GB"
                                                  , "Detroit Lions" = "DET"
                                                  , "Los Angeles Rams" = "LAR"
                                                  , "Miami Dolphins" = "MIA"
                                                  , "Chicago Bears" = "CHI"
                                                  , "Dallas Cowboys" = "DAL"
                                                  , "Seattle Seahawks" = "SEA"
                                                  , "Washington Football Team" = "WAS"
                                                  , "Caroline Panthers" = "CAR"
                                                  , "Cleveland Browns" = "CLE"
                                                  , "Minnesota Vikings" = "MIN"
                                                  , "New Orleans Saints" = "NO"
                                                  , "Jacksonville Jaguars" = "JAC"
                                                  , "Tampa Bay Buccaneers" = "TB"
                                                  , "New York Giants" = "NYG"
                                                  , "Houston Texans" = "HOU"), trimws(RIGHT(PLAYER.V1, 3)))
        , POINTS = POINTS.MISC)
  
  ]
