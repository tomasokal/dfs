library(data.table)
library(xml2)
library(rvest)
library(tidyr)

# Extract salary data from FantasyPros

web_page <- "https://dknation.draftkings.com/2020/8/7/21359012/fantasy-football-dfs-salaries-week-1-nfl-lamar-jackson-christian-mccaffrey-quarterbacks"

page <- rvest::html_table(xml2::read_html(web_page)
                          , header = TRUE
                          , fill = TRUE)

data <- data.table::data.table(page[[1]])
colnames(data) <- as.character(data[1, ])
data <- data[-1, ]

salaries <- data[
  
  ,
  j = .(POSITION = Position
        , PLAYER_NAME = ifelse(Position == "DST"
                               , sapply(Player
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
                                        , "Houston Texans" = "HOU")
                               , Player)
        , TEAM = Team
        , SALARY = Salary)
  
]

projections_salaries <- merge(final_projections, salaries, by = c("PLAYER_NAME", "POSITION"), all.x = TRUE, all.y = TRUE)[
  
  !is.na(SALARY)
  ,
  j = .(PLAYER_NAME
        , POSITION
        , TEAM = ifelse(is.na(TEAM.x), TEAM.y, TEAM.x)
        , POINTS = ifelse(is.na(POINTS), 0, POINTS)
        , SALARY
        , DAY
        , TIME)
  
  ]