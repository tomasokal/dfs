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
        , PLAYER_NAME1 = ifelse(Position == "DST"
                                , sapply(Name
                                         , switch
                                         , "49ers" = "San Francisco 49ers"
                                         , "Bears" = "Chicago Bears"
                                         , "Bengals" = "Cincinnati Bengals"
                                         , "Bills" = "Buffalo Bills"
                                         , "Browns" = "Cleveland Browns"
                                         , "Buccaneers" = "Tampa Bay Buccaneers"
                                         , "Cardinals" = "Arizona Cardinals"
                                         , "Chargers" = "Los Angeles Chargers"
                                         , "Colts" = "Indianapolis Colts"
                                         , "Dolphins" = "Miami Dolphins"
                                         , "Eagles" = "Philadelphia Eagles"
                                         , "Falcons" = "Atlanta Falcons"
                                         , "Jaguars" = "Jacksonville Jaguars"
                                         , "Jets" = "New York Jets"
                                         , "Lions" = "Detroit Lions"
                                         , "Packers" = "Green Bay Packers"
                                         , "Panthers" = "Carolina Panthers"
                                         , "Patriots" = "New England Patriots"
                                         , "Raiders" = "Las Vegas Raiders"
                                         , "Ravens" = "Baltimore Ravens"
                                         , "Saints" = "New Orleans Saints"
                                         , "Seahawks" = "Seattle Seahawks"
                                         , "Vikings" = "Minnesota Vikings"
                                         , "WAS Football Team" = "Washington Football Team"
                                         )
                                , ifelse(Name %in% c("Willie Snead IV", "Todd Gurley II", "Ted Ginn Jr.", "Steven Sims Jr.", "Stanley Morgan", "Scotty Miller",
                                                     "Robert Griffin III", "Richie James Jr.", "P.J. Walker", "Phillip Dorsett II", "Mitchell Trubisky", "Matthew Slater",
                                                     "Marvin Jones Jr.", "KhaDarel Hodge", "John Ross III", "Jeff Wilson Jr.", "JJ Arcega-Whiteside", "Dwayne Haskins Jr.",
                                                     "Donald Parham Jr.", "DK Metcalf", "DJ Moore", "DJ Chark Jr.", "Chris Herndon", "C.J. Ham", "Allen Robinson II", "AJ Dillon")
                                         , sapply(Name
                                                  , switch
                                                  , "Willie Snead IV" = "Willie Snead"
                                                  , "Todd Gurley II" = "Todd Gurley"
                                                  , "Ted Ginn Jr." = "Ted Ginn"
                                                  , "Steven Sims Jr." = "Steven Sims"
                                                  , "Stanley Morgan" = "Stanley Morgan Jr."
                                                  , "Scotty Miller" = "Scott Miller"
                                                  , "Robert Griffin III" = "Robert Griffin"
                                                  , "Richie James Jr." = "Richie James"
                                                  , "P.J. Walker" = "Phillip Walker"
                                                  , "Phillip Dorsett II" = "Phillip Dorsett"
                                                  , "Mitchell Trubisky" = "Mitch Trubisky"
                                                  , "Matthew Slater" = "Matt Slater"
                                                  , "Marvin Jones Jr." = "Marvin Jones"
                                                  , "KhaDarel Hodge" = "Khadarel Hodge"
                                                  , "John Ross III" = "John Ross"
                                                  , "Jeff Wilson Jr." = "Jeff Wilson"
                                                  , "JJ Arcega-Whiteside" = "J.J. Arcega-Whiteside"
                                                  , "Dwayne Haskins Jr." = "Dwayne Haskins"
                                                  , "Donald Parham Jr." = "Donald Parham"
                                                  , "DK Metcalf" = "D.K. Metcalf"
                                                  , "DJ Moore" = "D.J. Moore"
                                                  , "DJ Chark Jr." = "D.J. Chark"
                                                  , "Chris Herndon" = "Chris Herndon IV"
                                                  , "C.J. Ham" = "CJ Ham"
                                                  , "Allen Robinson II" = "Allen Robinson"
                                                  , "AJ Dillon" = "A.J. Dillon"
                                                  , as.character(Name))
                                         , Name))
        , TEAM = Team
        , SALARY = as.numeric(Salary))
  
  ][, PLAYER_NAME := as.character(PLAYER_NAME1)][, -2]

projections_salaries <- merge(final_projections, salaries, by = c("PLAYER_NAME", "POSITION"), all.x = TRUE, all.y = TRUE)[
  
  !TEAM1 %in% c("DAL", "LAR", "PIT", "NYG", "HOU", "KC", "TEN", "DEN")
  ,
  
  ][!is.na(SALARY)][!is.na(POINTS)]
