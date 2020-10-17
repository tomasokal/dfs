library(data.table)
library(xml2)
library(rvest)
library(tidyr)

# Extract salary data from FantasyPros

web_page <- "https://www.fantasypros.com/daily-fantasy/nfl/draftkings-salary-changes.php"

page <- rvest::html_table(xml2::read_html(web_page)
                          , header = TRUE
                          , fill = TRUE)

data <- data.table::data.table(page[[1]])[
  
  ,
  j = .(Player
        , SALARY = as.numeric(gsub("[\\$,]","", `This Week`))
        , DAY = toupper(substring(Kickoff, 1, 3))
        , TIME = trimws(substring(Kickoff, 5, 11)))
  
  ] %>% tidyr::extract(Player, c("PLAYER_NAME", "TEAM", "POSITION"),
                       "(.+)\\s\\(([A-Z]+)\\s\\-\\s([A-Z]+)\\)$")

projections_salaries <- merge(final_projections, data, by = c("PLAYER_NAME", "POSITION"), all.x = TRUE, all.y = TRUE)[
  
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

saveRDS(projections_salaries, paste0("Data/salaries_week", week_number, "_", Sys.Date(), ".RDS"))
salaries <- projections_salaries
projections_salaries <- merge(final_projections, salaries, by = c("PLAYER_NAME", "POSITION"), all.x = TRUE, all.y = TRUE)[!is.na(SALARY)][DAY == "SUN", ][!TIME == "8:20PM"]


