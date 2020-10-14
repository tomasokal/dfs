# Necessary libraries
library(data.table)
library(rvest)
library(xml2)
library(lpSolve)

## Daily Fantasy Sports
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
                                       , POINTS_FD = as.numeric(gsub("[\\$,]","", v16))
                                       , POINTS_DK = as.numeric(gsub("[\\$,]","", v19))
                                       , POINTS_YH = as.numeric(gsub("[\\$,]","", v22))
                                       , SALARY_FD = as.numeric(gsub("[\\$,]","", v17))
                                       , SALARY_DK = as.numeric(gsub("[\\$,]","", v20))
                                       , SALARY_YH = as.numeric(gsub("[\\$,]","", v23)))]


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
                                       , POINTS_FD = as.numeric(gsub("[\\$,]","", v13))
                                       , POINTS_DK = as.numeric(gsub("[\\$,]","", v16))
                                       , POINTS_YH = as.numeric(gsub("[\\$,]","", v19))
                                       , SALARY_FD = as.numeric(gsub("[\\$,]","", v14))
                                       , SALARY_DK = as.numeric(gsub("[\\$,]","", v17))
                                       , SALARY_YH = as.numeric(gsub("[\\$,]","", v20)))]

# Slate information

slate_function <- function(x) {
  
  if ("2020-10-05" < x & x < "2020-10-13") {
    
    # WEEK 5
    exclude <- c("TBB", "CHI", "MIN", "SEA", "LAC", "NOS", "BUF", "TEN")
    
  }
  
  else if ("2020-10-12" < x & x < "2020-10-20") {
    
    # WEEK 6
    exclude <- c("KCC", "BUF", "LAR", "SFO", "ARI", "DAL")
    
  }
  
  else if ("2020-10-19" < x & x < "2020-10-27") {
    
    # WEEK 7
    exclude <- c("NYG", "PHI", "TBB", "LVR", "CHI", "LAR")
    
  }
  
  else if ("2020-10-26" < x & x < "2020-11-03") {
    
    # WEEK 8
    exclude <- c("ATL", "CAR", "DAL", "PHI", "TBB", "NYG")
    
  }
  
  else if ("2020-11-02" < x & x < "2020-11-10") {
    
    # WEEK 9
    exclude <- c("GBP", "SFO", "NOS", "TBP", "NEP", "NYJ")
    
  }
  
  else if ("2020-11-09" < x & x < "2020-11-17") {
    
    # WEEK 10
    exclude <- c("IND", "TEN", "BAL", "NEP", "MIN", "CHI")
    
  }
  
  else if ("2020-11-16" < x & x < "2020-11-24") {
    
    # WEEK 11
    exclude <- c("ARI", "SEA", "KCC", "LVR", "LAR", "TBB")
    
  }
  
  else if ("2020-11-23" < x & x < "2020-12-01") {
    
    # WEEK 12
    exclude <- c("HOU", "DET", "WAS", "DAL", "BAL", "PIT", "CHI", "GBP", "SEA", "PHI")
    
  }
  
  else if ("2020-11-30" < x & x < "2020-12-08") {
    
    # WEEK 13
    exclude <- c("DAL", "BAL", "DEN", "KCC", "BUF", "SFO")
    
  }
  
  else if ("2020-12-07" < x & x < "2020-12-15") {
    
    # WEEK 14
    exclude <- c("NEP", "LAR", "PIT", "BUF", "BAL", "CLE")
    
  }
  
  else if ("2020-12-14" < x & x < "2020-12-22") {
    
    # WEEK 15
    exclude <- c("NEP", "LAR", "PIT", "BUF", "BAL", "CLE")
    
  }
  
  else if ("2020-12-21" < x & x < "2020-12-29") {
    
    # WEEK 16
    exclude <- c("NEP", "LAR", "PIT", "BUF", "BAL", "CLE")
    
  }
  
  else if ("2020-12-28" < x & x < "2021-01-05") {
    
    # WEEK 17
    exclude <- c("NONE")
    
  }
  
  else {
    
    # WEEK 15
    exclude <- c("NONE")
    
  }
  
  return(exclude)
  
}

SLATE_CHECK <- slate_function(Sys.Date())

# Merging together

merge_full <- data.table::rbindlist(list(projections_dfs, projections_dst))[, SLATE_MAIN := ifelse(TEAM %chin% SLATE_CHECK, 0, 1)]
slate_main <- merge_full[SLATE_MAIN == 1]

# Export data 
data.table::fwrite(merge_full, "Output/salaries_projections_scraped_script.csv")

# Setting time
time <- as.POSIXct(Sys.time(), "Etc/GMT+5")
save(time, file = "Output/time.RData")

# Optimum difference

## DraftKings
player_pool <- slate_main[!is.na(SALARY_DK)]
obj_points <- player_pool[, .(POINTS = POINTS_DK)]
position_dt <- player_pool[, j = .(ppQB = ifelse(POSITION == "QB", 1, 0),
                                   ppRB = ifelse(POSITION == "RB", 1, 0),
                                   ppWR = ifelse(POSITION == "WR", 1, 0),
                                   ppTE = ifelse(POSITION == "TE", 1, 0),
                                   ppDST = ifelse(POSITION == "DST", 1, 0),
                                   ppFlex = ifelse(POSITION %in% c("RB", "WR", "TE"), 1, 0))]

con_players <- t(cbind(SALARY = player_pool[, SALARY_DK], position_dt))
colnames(con_players) <- player_pool$PLAYER

f.dir <- rep(0, nrow(con_players))
f.rhs <- rep(0, nrow(con_players))

f.dir[1] <- "<="
f.rhs[1] <- 50000

f.dir[2:nrow(con_players)] <- c("=", ">=", ">=", ">=", "=", "=")
f.rhs[2:nrow(con_players)] <- c(1, 2, 3, 1, 1, 7)

opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
picks_base <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_DK, SALARY = SALARY_DK)]

new_points <- rep(0, nrow(player_pool))

for (i in 1:nrow(player_pool)) {
  
  if (!as.character(player_pool[i, 1]) %in% picks_base$PLAYER) {
    
    obj_points <- player_pool[, .(POINTS = POINTS_DK)]
    exp_points <- obj_points[i]
    
    picks_new <- picks_base
    
    print("Not in optimal lineup")
    
    repeat {
      
      exp_points <- exp_points + 0.25
      new_points[i] <-  exp_points
      
      obj_points[i] <- exp_points
      
      opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
      picks_new <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_DK, SALARY = SALARY_DK)]
      
      if (as.character(player_pool[i, 1]) %in% picks_new$PLAYER) {
        
        break
        
      }
      
    }
    
  }
  
  else {
    
    print("In optimal lineup")
    
    new_points[i] <- obj_points[i]
    
  }
  
}

eval_dk <- data.table::data.table(PLAYER = player_pool$PLAYER
                                  , TEAM = player_pool$TEAM
                                  , POSITION = player_pool$POSITION
                                  , PROJECTED_POINTS = player_pool$POINTS_DK
                                  , NEEDED_POINTS = unlist(new_points)
)[, DIFF_DK := NEEDED_POINTS - PROJECTED_POINTS][, .(PLAYER, TEAM, POSITION, DIFF_DK)]

## Fanduel
player_pool <- slate_main[!is.na(SALARY_FD)]
obj_points <- player_pool[, .(POINTS = POINTS_FD)]
position_dt <- player_pool[, j = .(ppQB = ifelse(POSITION == "QB", 1, 0),
                                   ppRB = ifelse(POSITION == "RB", 1, 0),
                                   ppWR = ifelse(POSITION == "WR", 1, 0),
                                   ppTE = ifelse(POSITION == "TE", 1, 0),
                                   ppDST = ifelse(POSITION == "DST", 1, 0),
                                   ppFlex = ifelse(POSITION %in% c("RB", "WR", "TE"), 1, 0))]

con_players <- t(cbind(SALARY = player_pool[, SALARY_FD], position_dt))
colnames(con_players) <- player_pool$PLAYER

f.dir <- rep(0, nrow(con_players))
f.rhs <- rep(0, nrow(con_players))

f.dir[1] <- "<="
f.rhs[1] <- 50000

f.dir[2:nrow(con_players)] <- c("=", ">=", ">=", ">=", "=", "=")
f.rhs[2:nrow(con_players)] <- c(1, 2, 3, 1, 1, 7)

opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
picks_base <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_FD, SALARY = SALARY_FD)]

new_points <- rep(0, nrow(player_pool))

for (i in 1:nrow(player_pool)) {
  
  if (!as.character(player_pool[i, 1]) %in% picks_base$PLAYER) {
    
    obj_points <- player_pool[, .(POINTS = POINTS_FD)]
    exp_points <- obj_points[i]
    
    picks_new <- picks_base
    
    print("Not in optimal lineup")
    
    repeat {
      
      exp_points <- exp_points + 0.25
      new_points[i] <-  exp_points
      
      obj_points[i] <- exp_points
      
      opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
      picks_new <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_FD, SALARY = SALARY_FD)]
      
      if (as.character(player_pool[i, 1]) %in% picks_new$PLAYER) {
        
        break
        
      }
      
    }
    
  }
  
  else {
    
    print("In optimal lineup")
    
    new_points[i] <- obj_points[i]
    
  }
  
}

eval_fd <- data.table::data.table(PLAYER = player_pool$PLAYER
                                  , TEAM = player_pool$TEAM
                                  , POSITION = player_pool$POSITION
                                  , PROJECTED_POINTS = player_pool$POINTS_FD
                                  , NEEDED_POINTS = unlist(new_points)
)[, DIFF_FD := NEEDED_POINTS - PROJECTED_POINTS][, .(PLAYER, TEAM, POSITION, DIFF_FD)]

## Yahoo
player_pool <- slate_main[!is.na(SALARY_YH)]
obj_points <- player_pool[, .(POINTS = POINTS_YH)]
position_dt <- player_pool[, j = .(ppQB = ifelse(POSITION == "QB", 1, 0),
                                   ppRB = ifelse(POSITION == "RB", 1, 0),
                                   ppWR = ifelse(POSITION == "WR", 1, 0),
                                   ppTE = ifelse(POSITION == "TE", 1, 0),
                                   ppDST = ifelse(POSITION == "DST", 1, 0),
                                   ppFlex = ifelse(POSITION %in% c("RB", "WR", "TE"), 1, 0))]

con_players <- t(cbind(SALARY = player_pool[, SALARY_YH], position_dt))
colnames(con_players) <- player_pool$PLAYER

f.dir <- rep(0, nrow(con_players))
f.rhs <- rep(0, nrow(con_players))

f.dir[1] <- "<="
f.rhs[1] <- 50000

f.dir[2:nrow(con_players)] <- c("=", ">=", ">=", ">=", "=", "=")
f.rhs[2:nrow(con_players)] <- c(1, 2, 3, 1, 1, 7)

opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
picks_base <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_YH, SALARY = SALARY_YH)]

new_points <- rep(0, nrow(player_pool))

for (i in 1:nrow(player_pool)) {
  
  if (!as.character(player_pool[i, 1]) %in% picks_base$PLAYER) {
    
    obj_points <- player_pool[, .(POINTS = POINTS_YH)]
    exp_points <- obj_points[i]
    
    picks_new <- picks_base
    
    print("Not in optimal lineup")
    
    repeat {
      
      exp_points <- exp_points + 0.25
      new_points[i] <-  exp_points
      
      obj_points[i] <- exp_points
      
      opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
      picks_new <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_YH, SALARY = SALARY_YH)]
      
      if (as.character(player_pool[i, 1]) %in% picks_new$PLAYER) {
        
        break
        
      }
      
    }
    
  }
  
  else {
    
    print("In optimal lineup")
    
    new_points[i] <- obj_points[i]
    
  }
  
}

eval_yh <- data.table::data.table(PLAYER = player_pool$PLAYER
                                  , TEAM = player_pool$TEAM
                                  , POSITION = player_pool$POSITION
                                  , PROJECTED_POINTS = player_pool$POINTS_YH
                                  , NEEDED_POINTS = unlist(new_points)
)[, DIFF_YH := NEEDED_POINTS - PROJECTED_POINTS][, .(PLAYER, TEAM, POSITION, DIFF_YH)]

# Merging together
merge1 <- merge(slate_main, eval_dk, by = c("PLAYER", "TEAM", "POSITION"), all.x = TRUE)
merge2 <- merge(merge1, eval_fd, by = c("PLAYER", "TEAM", "POSITION"), all.x = TRUE)
merge3 <- merge(merge2, eval_yh, by = c("PLAYER", "TEAM", "POSITION"), all.x = TRUE)

merge3 <- merge3[order(-POINTS_DK)]

# Export data 
data.table::fwrite(merge3, "Output/salaries_projections_main_slate.csv")

# Baseline picks

## Yahoo
player_pool <- slate_main[!is.na(SALARY_YH)]
obj_points <- player_pool[, .(POINTS = POINTS_YH)]
position_dt <- player_pool[, j = .(ppQB = ifelse(POSITION == "QB", 1, 0),
                                   ppRB = ifelse(POSITION == "RB", 1, 0),
                                   ppWR = ifelse(POSITION == "WR", 1, 0),
                                   ppTE = ifelse(POSITION == "TE", 1, 0),
                                   ppDST = ifelse(POSITION == "DST", 1, 0),
                                   ppFlex = ifelse(POSITION %in% c("RB", "WR", "TE"), 1, 0))]

con_players <- t(cbind(SALARY = player_pool[, SALARY_YH], position_dt))
colnames(con_players) <- player_pool$PLAYER

f.dir <- rep(0, nrow(con_players))
f.rhs <- rep(0, nrow(con_players))

f.dir[1] <- "<="
f.rhs[1] <- 200

f.dir[2:nrow(con_players)] <- c("=", ">=", ">=", ">=", "=", "=")
f.rhs[2:nrow(con_players)] <- c(1, 2, 3, 1, 1, 7)

opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
picks_yh <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_YH, SALARY = SALARY_YH)]

data.table::fwrite(picks_yh, "Output/picks_yh.csv")

## FanDuel
player_pool <- slate_main[!is.na(SALARY_FD)]
obj_points <- player_pool[, .(POINTS = POINTS_FD)]
position_dt <- player_pool[, j = .(ppQB = ifelse(POSITION == "QB", 1, 0),
                                   ppRB = ifelse(POSITION == "RB", 1, 0),
                                   ppWR = ifelse(POSITION == "WR", 1, 0),
                                   ppTE = ifelse(POSITION == "TE", 1, 0),
                                   ppDST = ifelse(POSITION == "DST", 1, 0),
                                   ppFlex = ifelse(POSITION %in% c("RB", "WR", "TE"), 1, 0))]

con_players <- t(cbind(SALARY = player_pool[, SALARY_FD], position_dt))
colnames(con_players) <- player_pool$PLAYER

f.dir <- rep(0, nrow(con_players))
f.rhs <- rep(0, nrow(con_players))

f.dir[1] <- "<="
f.rhs[1] <- 60000

f.dir[2:nrow(con_players)] <- c("=", ">=", ">=", ">=", "=", "=")
f.rhs[2:nrow(con_players)] <- c(1, 2, 3, 1, 1, 7)

opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
picks_fd <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_FD, SALARY = SALARY_FD)]

data.table::fwrite(picks_fd, "Output/picks_fd.csv")

## DraftKings
player_pool <- slate_main[!is.na(SALARY_DK)]
obj_points <- player_pool[, .(POINTS = POINTS_DK)]
position_dt <- player_pool[, j = .(ppQB = ifelse(POSITION == "QB", 1, 0),
                                   ppRB = ifelse(POSITION == "RB", 1, 0),
                                   ppWR = ifelse(POSITION == "WR", 1, 0),
                                   ppTE = ifelse(POSITION == "TE", 1, 0),
                                   ppDST = ifelse(POSITION == "DST", 1, 0),
                                   ppFlex = ifelse(POSITION %in% c("RB", "WR", "TE"), 1, 0))]

con_players <- t(cbind(SALARY = player_pool[, SALARY_DK], position_dt))
colnames(con_players) <- player_pool$PLAYER

f.dir <- rep(0, nrow(con_players))
f.rhs <- rep(0, nrow(con_players))

f.dir[1] <- "<="
f.rhs[1] <- 50000

f.dir[2:nrow(con_players)] <- c("=", ">=", ">=", ">=", "=", "=")
f.rhs[2:nrow(con_players)] <- c(1, 2, 3, 1, 1, 7)

opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
picks_dk <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_DK, SALARY = SALARY_DK)]

data.table::fwrite(picks_dk, "Output/picks_dk.csv")
