# Necessary libraries
library(data.table)
library(rvest)
library(xml2)
library(lpSolve)

## Offense 
url <- "https://www.numberfire.com/nfl/fantasy/fantasy-football-projections"
page <- rvest::html_table(xml2::read_html(url)
                          , header = TRUE
                          , fill = TRUE)
projections1 <- data.table::data.table(page[[1]])
projections2 <- data.table::data.table(page[[2]])
projections_dfs <- cbind(projections1, projections2)
colnames(projections_dfs) <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23", "v24")
projections_dfs <- projections_dfs[-1, ]
projections_dfs <- projections_dfs[, gsub1 := gsub("\t", "", v1), by = v1][, gsub2 := gsub("\n", "_", gsub1), by = gsub1]
projections_dfs <- projections_dfs[, c("v25", "v26", "v27") := tstrsplit(gsub2, "_", fixed = TRUE)]
projections_dfs <- projections_dfs[, .(PLAYER = v25
                                       , POSITION = substr(gsub(".*\\((.*)\\).*", "\\1", v27), 1, 2)
                                       , TEAM = trimws(substr(gsub(".*\\((.*)\\).*", "\\1", v27), 4, nchar(gsub(".*\\((.*)\\).*", "\\1", v27))))
                                       , POINTS_DK = as.numeric(gsub("[\\$,]","", v19)))]

## Defense
url <- "https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/d"
page <- rvest::html_table(xml2::read_html(url)
                          , header = TRUE
                          , fill = TRUE)
projections1 <- data.table::data.table(page[[1]])
projections2 <- data.table::data.table(page[[2]])
projections_dst <- cbind(projections1, projections2)
colnames(projections_dst) <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21")
projections_dst <- projections_dst[-1, ]
projections_dst <- projections_dst[, gsub1 := gsub("\t", "", v1), by = v1][, gsub2 := gsub("\n", "_", gsub1), by = gsub1]
projections_dst <- projections_dst[, c("v22", "v23", "v24") := tstrsplit(gsub2, "_", fixed = TRUE)]
projections_dst <- projections_dst[, .(PLAYER = v22
                                       , POSITION = "DST"
                                       , TEAM = trimws(substr(gsub(".*\\((.*)\\).*", "\\1", v24), 4, nchar(gsub(".*\\((.*)\\).*", "\\1", v24))))
                                       , POINTS_DK = as.numeric(gsub("[\\$,]","", v16)))]

## Kickers
url <- "https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/k"
page <- rvest::html_table(xml2::read_html(url)
                          , header = TRUE
                          , fill = TRUE)
projections1 <- data.table::data.table(page[[1]])
projections2 <- data.table::data.table(page[[2]])
projections_k <- cbind(projections1, projections2)
colnames(projections_k) <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23")
projections_k <- projections_k[-1, ]
projections_k <- projections_k[, gsub1 := gsub("\t", "", v1), by = v1][, gsub2 := gsub("\n", "_", gsub1), by = gsub1]
projections_k <- projections_k[, c("v24", "v25", "v26") := tstrsplit(gsub2, "_", fixed = TRUE)]
projections_k <- projections_k[, .(PLAYER = v24
                                       , POSITION = "K"
                                       , TEAM = trimws(substr(gsub(".*\\((.*)\\).*", "\\1", v26), 4, nchar(gsub(".*\\((.*)\\).*", "\\1", v26))))
                                       , POINTS_DK = as.numeric(gsub("[\\$,]","", v18)))]

## Combine
merge_full <- data.table::rbindlist(list(projections_dfs, projections_dst, projections_k))

## MIN @ SEA

## Salaries
salaries <- data.table::fread("Data/Showdown Draftkings/draftkings_sea_min_w5.csv")[
  
  i = `Roster Position` == "FLEX"
  ,
  j = .(PLAYER = as.character(ifelse(!`Name` %in% merge_full$PLAYER
                                     , sapply(`Name`
                                              , switch
                                              , "DK Metcalf" = "D.K. Metcalf"
                                              , "Seahawks" = "Seattle D/ST"
                                              , "Vikings" = "Minnesota D/ST"
                                              , "Olabisi Johnson" = "Bisi Johnson"
                                              , "Phillip Dorsett II" = "Phillip Dorsett")
                                     , `Name`))
        , POSITION = `Position`
        , TEAM = `TeamAbbrev`
        , SALARY_FLEX = as.numeric(as.integer(Salary))
        , SALARY_CPT = 1.5 * as.numeric(as.integer(Salary)))
  
  ][!PLAYER == "NULL"]

## Combine
merge_full <- merge(merge_full, salaries, by.x = c("PLAYER", "POSITION", "TEAM"), by.y = c("PLAYER", "POSITION", "TEAM"))[
  
  ,
  j = .(PLAYER
        , POSITION
        , TEAM
        , POINTS_FLEX = POINTS_DK
        , POINTS_CPT = 1.5 * POINTS_DK
        , SALARY_FLEX
        , SALARY_CPT)
  
]

## Optimization

picks <- list()

for (row_cpt in 1:nrow(merge_full)) {
  
  player_pool <- merge_full[-row_cpt]
  salary_cpt <- merge_full[row_cpt, SALARY_CPT]
  points_cpt <- merge_full[-row_cpt, POINTS_CPT]
  
  flex_points <- player_pool[, .(POINTS = POINTS_FLEX)]
  flex_rules <- player_pool[, j = .(ppPLAYER = 1
                                    , ppHOME = ifelse(TEAM == "SEA", 1, 0)
                                    , ppAWAY = ifelse(TEAM == "MIN", 1, 0))]
  flex_con_pl <- t(cbind(player_pool[, SALARY_FLEX], flex_rules))
  colnames(flex_con_pl) <- player_pool$PLAYER
  
  f.dir <- rep(0, nrow(flex_con_pl))
  f.rhs <- rep(0, nrow(flex_con_pl))
  
  f.dir[1] <- "<="
  f.rhs[1] <- 50000 - salary_cpt
  
  f.dir[2:nrow(flex_con_pl)] <- c("=", ">=", ">=")
  f.rhs[2:nrow(flex_con_pl)] <- c(5, 1, 1)
  
  opt <- lp("max", flex_points, flex_con_pl, f.dir, f.rhs, all.bin = TRUE)
  opt_picks <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_FLEX, SALARY = SALARY_FLEX, ROLE = "FLEX")]
  
  cpt_picks <- player_pool <- merge_full[row_cpt][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_CPT, SALARY = SALARY_CPT, ROLE = "CPT")]
  
  picks[[row_cpt]] <- c(data.table::rbindlist(list(cpt_picks, opt_picks))[, PLAYER]
                         , sum(data.table::rbindlist(list(cpt_picks, opt_picks))[, SALARY])
                         , sum(data.table::rbindlist(list(cpt_picks, opt_picks))[, POINTS]))
  
}

lineups <- data.table::as.data.table(t(as.data.frame(picks)))
names(lineups) <- c("CAPTAIN", "FLEX1", "FLEX2", "FLEX3", "FLEX4", "FLEX5", "SALARY", "POINTS")
lineups$SALARY <- as.numeric(as.character(lineups$SALARY))
lineups$POINTS <- as.numeric(as.character(lineups$POINTS))
lineups <- lineups[order(-POINTS)][1:10, ]

# Export
data.table::fwrite(lineups, "Output/picks_dk_sea_min_w5.csv")

## NYG @ DAL
merge_full <- data.table::rbindlist(list(projections_dfs, projections_dst, projections_k))

## Salaries
salaries <- data.table::fread("Data/Showdown Draftkings/draftkings_nyg_dal_w5.csv")[
  
  i = `Roster Position` == "FLEX"
  ,
  j = .(PLAYER = as.character(ifelse(!`Name` %in% merge_full$PLAYER
                                     , sapply(`Name`
                                              , switch
                                              , "Cowboys" = "Dallas D/ST"
                                              , "Giants" = "New York Giants D/ST"
                                              , "Wayne Gallman Jr." = "Wayne Gallman")
                                     , `Name`))
        , POSITION = `Position`
        , TEAM = `TeamAbbrev`
        , SALARY_FLEX = as.numeric(as.integer(Salary))
        , SALARY_CPT = 1.5 * as.numeric(as.integer(Salary)))
  
  ][!PLAYER == "NULL"]

## Combine
merge_full <- merge(merge_full, salaries, by.x = c("PLAYER", "POSITION", "TEAM"), by.y = c("PLAYER", "POSITION", "TEAM"))[
  
  ,
  j = .(PLAYER
        , POSITION
        , TEAM
        , POINTS_FLEX = POINTS_DK
        , POINTS_CPT = 1.5 * POINTS_DK
        , SALARY_FLEX
        , SALARY_CPT)
  
  ]

## Optimization

picks <- list()

for (row_cpt in 1:nrow(merge_full)) {
  
  player_pool <- merge_full[-row_cpt]
  salary_cpt <- merge_full[row_cpt, SALARY_CPT]
  points_cpt <- merge_full[-row_cpt, POINTS_CPT]
  
  flex_points <- player_pool[, .(POINTS = POINTS_FLEX)]
  flex_rules <- player_pool[, j = .(ppPLAYER = 1
                                    , ppHOME = ifelse(TEAM == "NYG", 1, 0)
                                    , ppAWAY = ifelse(TEAM == "DAL", 1, 0))]
  flex_con_pl <- t(cbind(player_pool[, SALARY_FLEX], flex_rules))
  colnames(flex_con_pl) <- player_pool$PLAYER
  
  f.dir <- rep(0, nrow(flex_con_pl))
  f.rhs <- rep(0, nrow(flex_con_pl))
  
  f.dir[1] <- "<="
  f.rhs[1] <- 50000 - salary_cpt
  
  f.dir[2:nrow(flex_con_pl)] <- c("=", ">=", ">=")
  f.rhs[2:nrow(flex_con_pl)] <- c(5, 1, 1)
  
  opt <- lp("max", flex_points, flex_con_pl, f.dir, f.rhs, all.bin = TRUE)
  opt_picks <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_FLEX, SALARY = SALARY_FLEX, ROLE = "FLEX")]
  
  cpt_picks <- player_pool <- merge_full[row_cpt][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_CPT, SALARY = SALARY_CPT, ROLE = "CPT")]
  
  picks[[row_cpt]] <- c(data.table::rbindlist(list(cpt_picks, opt_picks))[, PLAYER]
                        , sum(data.table::rbindlist(list(cpt_picks, opt_picks))[, SALARY])
                        , sum(data.table::rbindlist(list(cpt_picks, opt_picks))[, POINTS]))
  
}

lineups <- data.table::as.data.table(t(as.data.frame(picks)))
names(lineups) <- c("CAPTAIN", "FLEX1", "FLEX2", "FLEX3", "FLEX4", "FLEX5", "SALARY", "POINTS")
lineups$SALARY <- as.numeric(as.character(lineups$SALARY))
lineups$POINTS <- as.numeric(as.character(lineups$POINTS))
lineups <- lineups[order(-POINTS)][1:10, ]

# Export
data.table::fwrite(lineups, "Output/picks_dk_nyg_dal_w5.csv")

## CAR @ ATL
merge_full <- data.table::rbindlist(list(projections_dfs, projections_dst, projections_k))

## Salaries
salaries <- data.table::fread("Data/Showdown Draftkings/draftkings_car_atl_w5.csv")[
  
  i = `Roster Position` == "FLEX"
  ,
  j = .(PLAYER = as.character(ifelse(!`Name` %in% merge_full$PLAYER
                                     , sapply(`Name`
                                              , switch
                                              , "Falcons" = "Atlanta D/ST"
                                              , "Panthers" = "Carolina D/ST"
                                              , "Todd Gurley II" = "Todd Gurley"
                                              , "DJ Moore" = "DJ. Moore")
                                     , `Name`))
        , POSITION = `Position`
        , TEAM = `TeamAbbrev`
        , SALARY_FLEX = as.numeric(as.integer(Salary))
        , SALARY_CPT = 1.5 * as.numeric(as.integer(Salary)))
  
  ][!PLAYER == "NULL"]

## Combine
merge_full <- merge(merge_full, salaries, by.x = c("PLAYER", "POSITION", "TEAM"), by.y = c("PLAYER", "POSITION", "TEAM"))[
  
  ,
  j = .(PLAYER
        , POSITION
        , TEAM
        , POINTS_FLEX = POINTS_DK
        , POINTS_CPT = 1.5 * POINTS_DK
        , SALARY_FLEX
        , SALARY_CPT)
  
  ]

## Optimization

picks <- list()

for (row_cpt in 1:nrow(merge_full)) {
  
  player_pool <- merge_full[-row_cpt]
  salary_cpt <- merge_full[row_cpt, SALARY_CPT]
  points_cpt <- merge_full[-row_cpt, POINTS_CPT]
  
  flex_points <- player_pool[, .(POINTS = POINTS_FLEX)]
  flex_rules <- player_pool[, j = .(ppPLAYER = 1
                                    , ppHOME = ifelse(TEAM == "CAR", 1, 0)
                                    , ppAWAY = ifelse(TEAM == "ATL", 1, 0))]
  flex_con_pl <- t(cbind(player_pool[, SALARY_FLEX], flex_rules))
  colnames(flex_con_pl) <- player_pool$PLAYER
  
  f.dir <- rep(0, nrow(flex_con_pl))
  f.rhs <- rep(0, nrow(flex_con_pl))
  
  f.dir[1] <- "<="
  f.rhs[1] <- 50000 - salary_cpt
  
  f.dir[2:nrow(flex_con_pl)] <- c("=", ">=", ">=")
  f.rhs[2:nrow(flex_con_pl)] <- c(5, 1, 1)
  
  opt <- lp("max", flex_points, flex_con_pl, f.dir, f.rhs, all.bin = TRUE)
  opt_picks <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_FLEX, SALARY = SALARY_FLEX, ROLE = "FLEX")]
  
  cpt_picks <- player_pool <- merge_full[row_cpt][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_CPT, SALARY = SALARY_CPT, ROLE = "CPT")]
  
  picks[[row_cpt]] <- c(data.table::rbindlist(list(cpt_picks, opt_picks))[, PLAYER]
                        , sum(data.table::rbindlist(list(cpt_picks, opt_picks))[, SALARY])
                        , sum(data.table::rbindlist(list(cpt_picks, opt_picks))[, POINTS]))
  
}

lineups <- data.table::as.data.table(t(as.data.frame(picks)))
names(lineups) <- c("CAPTAIN", "FLEX1", "FLEX2", "FLEX3", "FLEX4", "FLEX5", "SALARY", "POINTS")
lineups$SALARY <- as.numeric(as.character(lineups$SALARY))
lineups$POINTS <- as.numeric(as.character(lineups$POINTS))
lineups <- lineups[order(-POINTS)][1:10, ]

# Export
data.table::fwrite(lineups, "Output/picks_dk_car_atl_w5.csv")
