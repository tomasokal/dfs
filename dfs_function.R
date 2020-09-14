library(data.table)
library(rvest)
library(xml2)
library(reshape2)

# Salaries
salaries_draftkings <- data.table::fread("https://rotogrinders.com/projected-stats/nfl.csv?site=draftkings")
salaries_fanduel <- data.table::fread("https://rotogrinders.com/projected-stats/nfl.csv?site=fanduel")
salaries_yahoo <- data.table::fread("https://rotogrinders.com/projected-stats/nfl.csv?site=yahoo")

# Projections

## Daily Fantasy Sports
url <- "https://www.numberfire.com/nfl/daily-fantasy/daily-football-projections"
page <- rvest::html_table(xml2::read_html(url)
                          , header = TRUE
                          , fill = TRUE)
projections_dfs <- data.table::data.table(page[[4]])
colnames(projections_dfs) <- as.character(projections_dfs[1, ])
projections_dfs <- projections_dfs[-1, ][, gsub1 := gsub("\t", "", Player), by = Player][, gsub2 := gsub("\n", "_", gsub1), by = gsub1]
projections_dfs <- projections_dfs[, c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8") := tstrsplit(gsub2, "_", fixed = TRUE)]
projections_dfs <- projections_dfs[, .(PLAYER = v3
                                       , POSITION = v4
                                       , POINTS = FP
                                       , SALARY = as.numeric(gsub("\\$", "", gsub(",", "", projections_dfs$Cost)))
                                       , GAME = v6
                                       , TIME = v8)]

## PPR Scoring
url <- "https://www.numberfire.com/nfl/fantasy/fantasy-football-ppr-projections"
page <- rvest::html_table(xml2::read_html(url)
                          , header = TRUE
                          , fill = TRUE)
projections1 <- data.table::data.table(page[[1]])
projections2 <- data.table::data.table(page[[2]])
projections_ppr <- cbind(projections1, projections2)
colnames(projections_ppr) <- as.character(projections_ppr[1, ])
projections_ppr <- projections_ppr[-1, ]

## Normal Scoring
url <- "https://www.numberfire.com/nfl/fantasy/fantasy-football-projections"
page <- rvest::html_table(xml2::read_html(url)
                          , header = TRUE
                          , fill = TRUE)
projections1 <- data.table::data.table(page[[1]])
projections2 <- data.table::data.table(page[[2]])
projections_std <- cbind(projections1, projections2)

## Defense
url <- "https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/d"
page <- rvest::html_table(xml2::read_html(url)
                          , header = TRUE
                          , fill = TRUE)
projections1 <- data.table::data.table(page[[1]])
projections2 <- data.table::data.table(page[[2]])
projections_dst <- cbind(projections1, projections2)

# Merge together

check1 <- projections_dfs
check2 <- salaries_draftkings[, .(player_id, PLAYER = name, POSITION = pos, SALARY = salary)]

check <- merge(check1, check2, by = c("PLAYER", "POSITION"), all.x = TRUE, all.y = TRUE)
