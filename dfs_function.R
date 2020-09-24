library(data.table)
library(rvest)
library(xml2)
library(reshape2)
library(fuzzyjoin)
library(dummies)

# Set non-slate games
NON_SLATE <- <- c("MIA", "JAC", "KCC", "BAL", "GBP", "NOS")

# Salaries

## Scrape ROTOGRINDERS for salaries for DraftKings, Fanduel, and Yahoo salaries
salaries_draftkings <- data.table::fread("https://rotogrinders.com/projected-stats/nfl.csv?site=draftkings")
salaries_fanduel <- data.table::fread("https://rotogrinders.com/projected-stats/nfl.csv?site=fanduel")
salaries_yahoo <- data.table::fread("https://rotogrinders.com/projected-stats/nfl.csv?site=yahoo")

## Combine these salaries to have PLAYER_ID, NAME, TEAM, POSITION, SALARY
salaries <- Reduce(function(x, y) merge(x, y, by = "player_id"), list(salaries_draftkings, salaries_fanduel, salaries_yahoo))[
  
  i = salary < 1000
  ,
  j = .(PLAYER_ID = as.numeric(player_id)
        , PLAYER = gsub(" Jr.", "", name.x)
        , TEAM = team.x
        , POSITION = pos.x
        , SALARY_DK = salary.x
        , SALARY_FD = salary.y
        , SALARY_YH = salary)
  
]

salaries <- salaries[!TEAM %in% NON_SLATE]

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
projections_dfs <- projections_dfs[, .(PLAYER = gsub(" Jr.", "", v3)
                                       , POSITION = v4
                                       , POINTS = FP
                                       , TIME = v8)]

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
                                       , POINTS_FD = as.numeric(gsub("[\\$,]","", v13))
                                       , POINTS_DF = as.numeric(gsub("[\\$,]","", v16))
                                       , POINTS_YH = as.numeric(gsub("[\\$,]","", v19)))]
projections_dst <- projections_dst[, PLAYER_ID := as.numeric(switch(as.character(PLAYER)
                                                                    , 'Arizona D/ST' = '18415'
                                                                    , 'Atlanta D/ST' = '18416'
                                                                    , 'Baltimore D/ST' = '18446'
                                                                    , 'Buffalo D/ST' = '18417'
                                                                    , 'Carolina D/ST' = '18418'
                                                                    , 'Chicago D/ST' = '18419'
                                                                    , 'Cincinnati D/ST' = '18420'
                                                                    , 'Cleveland D/ST' = '18421'
                                                                    , 'Dallas D/ST' = '18422'
                                                                    , 'Denver D/ST' = '18423'
                                                                    , 'Detroit D/ST' = '18424'
                                                                    , 'Green Bay D/ST' = '18425'
                                                                    , 'Houston D/ST' = '18426'
                                                                    , 'Indianapolis D/ST' = '18427'
                                                                    , 'Jacksonville D/ST' = '18428'
                                                                    , 'Kansas City D/ST' = '18429'
                                                                    , 'Las Vegas D/ST' = '18436'
                                                                    , 'Los Angeles Chargers D/ST' = '18439'
                                                                    , 'Los Angeles Rams D/ST' = '18442'
                                                                    , 'Miami D/ST' = '18430'
                                                                    , 'Minnesota D/ST' = '18431'
                                                                    , 'New England D/ST' = '18432'
                                                                    , 'New Orleans D/ST' = '18433'
                                                                    , 'New York Giants D/ST' = '18434'
                                                                    , 'New York Jets D/ST' = '18435'
                                                                    , 'Philadelphia D/ST' = '18437'
                                                                    , 'Pittsburgh D/ST' = '18438'
                                                                    , 'San Francisco D/ST' = '18440'
                                                                    , 'Seattle D/ST' = '18441'
                                                                    , 'Tampa Bay D/ST' = '18443'
                                                                    , 'Tennessee D/ST' = '18444'
                                                                    , 'Washington D/ST' = '18445'))
                                   , by = PLAYER]

# Merging together

## Merging Non-DST
f <- Vectorize(function(x,y) agrepl(x, y,
                                    ignore.case=TRUE,
                                    max.distance = 0.05, useBytes = TRUE))
merge_nondst <- fuzzyjoin::fuzzy_join(x = projections_dfs
                                      , y = salaries[!POSITION == "DST"]
                                      , by = "PLAYER"
                                      , match_fun = f
                                      , mode = "full")
merge_nondst <- data.table::data.table(merge_nondst)[!is.na(PLAYER.x), ][!is.na(PLAYER.y)][, j = .(PLAYER = PLAYER.x
                                                                                                   , POSITION = POSITION.x
                                                                                                   , POINTS
                                                                                                   , SALARY_DK
                                                                                                   , SALARY_FD
                                                                                                   , SALARY_YH)]

## Merging DST
merge_dst <- merge(projections_dst, salaries[POSITION == "DST"], by = "PLAYER_ID")[!is.na(PLAYER.x), ][!is.na(PLAYER.y)][, j = .(PLAYER = PLAYER.x
                                                                                                                                 , POSITION = POSITION.x
                                                                                                                                 , POINTS = POINTS_DF
                                                                                                                                 , SALARY_DK
                                                                                                                                 , SALARY_FD
                                                                                                                                 , SALARY_YH)]

## Merge together
merge_full <- data.table::rbindlist(list(merge_nondst, merge_dst))

# Optimize

optimize_dfs <- function(site) {
  
  if (site == "DK") {
    
    fd <- merge_full[order(merge_full[, "POSITION"]), ]
    Position.Mat <- dummies::dummy(fd[, POSITION])
    Position.Mat <- cbind(Position.Mat, fdFlex = rowSums(Position.Mat[, c(3, 4, 5)]))
    
    f.obj <- fd[, POINTS]
    
    f.con <- t(cbind(SALARY = fd[, SALARY_DK], Position.Mat))
    colnames(f.con) <- fd$PLAYER
    
    f.dir <- rep(0, nrow(f.con))
    f.rhs <- rep(0, nrow(f.con))
    
    f.dir[1] <- "<="
    f.rhs[1] <- 50000
    
    f.dir[2] <- "="
    f.rhs[2] <- 1
    
    f.dir[3:nrow(f.con)] <- c("=", ">=", ">=", ">=", "=")
    f.rhs[3:nrow(f.con)] <- c(1, 2, 1, 3, 7)
    
    opt <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
    picks <- fd[which(opt$solution == 1), ]
    
    print(sum(as.numeric(picks$POINTS)))
    
    return(picks)
    
  }
  
  if (site == "FD") {
    
    fd <- merge_full[order(merge_full[, "POSITION"]), ]
    Position.Mat <- dummies::dummy(fd[, POSITION])
    Position.Mat <- cbind(Position.Mat, fdFlex = rowSums(Position.Mat[, c(3, 4, 5)]))
    
    f.obj <- fd[, POINTS]
    
    f.con <- t(cbind(SALARY = fd[, SALARY_FD], Position.Mat))
    colnames(f.con) <- fd$PLAYER
    
    f.dir <- rep(0, nrow(f.con))
    f.rhs <- rep(0, nrow(f.con))
    
    f.dir[1] <- "<="
    f.rhs[1] <- 60000
    
    f.dir[2] <- "="
    f.rhs[2] <- 1
    
    f.dir[3:nrow(f.con)] <- c("=", ">=", ">=", ">=", "=")
    f.rhs[3:nrow(f.con)] <- c(1, 2, 1, 3, 7)
    
    opt <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
    picks <- fd[which(opt$solution == 1), ]
    
    print(sum(as.numeric(picks$POINTS)))
    
    return(picks)
    
  }
  
  if (site == "YH") {
    
    fd <- merge_full[order(merge_full[, "POSITION"]), ]
    Position.Mat <- dummies::dummy(fd[, POSITION])
    Position.Mat <- cbind(Position.Mat, fdFlex = rowSums(Position.Mat[, c(3, 4, 5)]))
    
    f.obj <- fd[, POINTS]
    
    f.con <- t(cbind(SALARY = fd[, SALARY_YH], Position.Mat))
    colnames(f.con) <- fd$PLAYER
    
    f.dir <- rep(0, nrow(f.con))
    f.rhs <- rep(0, nrow(f.con))
    
    f.dir[1] <- "<="
    f.rhs[1] <- 200
    
    f.dir[2] <- "="
    f.rhs[2] <- 1
    
    f.dir[3:nrow(f.con)] <- c("=", ">=", ">=", ">=", "=")
    f.rhs[3:nrow(f.con)] <- c(1, 2, 1, 3, 7)
    
    opt <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
    picks <- fd[which(opt$solution == 1), ]
    
    print(sum(as.numeric(picks$POINTS)))
    
    return(picks)

  }
  
  else {
    
    print("Select a correct DFS site please.")
    
  }
  
}

optimize_dfs("DK")
optimize_dfs("FD")
optimize_dfs("YH")
