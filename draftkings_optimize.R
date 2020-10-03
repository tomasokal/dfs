library(data.table)
library(lpSolve)

# Optimize

optimize_dk <- function(df_full, df_include, df_exclude) {
  
  if (dim(df_include)[1] > 0) {
    
    check_inclusion <- df_include[, .N, by = POSITION]
    check_qb <- ifelse(length(check_inclusion[POSITION == "QB"][[2]]) == 0, 0, check_inclusion[POSITION == "QB"][[2]])
    check_rb <- ifelse(length(check_inclusion[POSITION == "RB"][[2]]) == 0, 0, check_inclusion[POSITION == "RB"][[2]])
    check_wr <- ifelse(length(check_inclusion[POSITION == "WR"][[2]]) == 0, 0, check_inclusion[POSITION == "WR"][[2]])
    check_te <- ifelse(length(check_inclusion[POSITION == "TE"][[2]]) == 0, 0, check_inclusion[POSITION == "TE"][[2]])
    check_dst <- ifelse(length(check_inclusion[POSITION == "DST"][[2]]) == 0, 0, check_inclusion[POSITION == "DST"][[2]])
    check_flex <- ifelse(dim(check_inclusion[POSITION %in% c("RB", "WR", "TE")])[1] == 0, 0, sum(check_inclusion[POSITION %in% c("RB", "WR", "TE"), 2]))
    
    if (check_qb > 1 | check_rb > 3 | check_wr > 4 | check_te > 2 | check_dst > 1 | check_flex > 7) {
      
      print("You are dumb.")
      
    }
    
    else {
      
      salary_inclusion <- sum(df_include$SALARY_DK)
      
      player_pool <- df_full[!PLAYER %in% df_include$PLAYER][!PLAYER %in% df_exclude$PLAYER]
      position_dt <- player_pool[, j = .(ppQB = ifelse(POSITION == "QB", 1, 0),
                                         ppRB = ifelse(POSITION == "RB", 1, 0),
                                         ppWR = ifelse(POSITION == "WR", 1, 0),
                                         ppTE = ifelse(POSITION == "TE", 1, 0),
                                         ppDST = ifelse(POSITION == "DST", 1, 0),
                                         ppFlex = ifelse(POSITION %in% c("RB", "WR", "TE"), 1, 0))]
      
      obj_points <- player_pool[, POINTS_DK]
      con_players <- t(cbind(SALARY = player_pool[, SALARY_DK], position_dt))
      colnames(con_players) <- player_pool$POINTS_DK
      
      f.dir <- rep(0, nrow(con_players))
      f.rhs <- rep(0, nrow(con_players))
      
      f.dir[1] <- "<="
      f.rhs[1] <- 50000 - salary_inclusion
      
      f.dir[2:nrow(con_players)] <- c("=", ">=", ">=", ">=", "=", "=")
      f.rhs[2:nrow(con_players)] <- c(1, 2, 3, 1, 1, 7)
      
      f.rhs[7] <- 7 - check_flex
      
      if (check_qb == 1) {
        
        f.dir[2] <- "="
        f.rhs[2] <- 0
        
      }
      
      else {
        
        f.rhs <- f.rhs
        
      }
      
      if (check_rb == 1) {
        
        f.dir[3] <- ">="
        f.rhs[3] <- 1
        
      }
      
      if (check_rb == 2) {
        
        f.dir[3] <- ">="
        f.rhs[3] <- 0
        
      }
      
      if (check_rb == 3) {
        
        f.dir[3] <- "="
        f.rhs[3] <- 0
        
      }
      
      else {
        
        f.dir <- f.dir
        f.rhs <- f.rhs
        
      }
      
      if (check_wr == 1) {
        
        f.dir[4] <- ">="
        f.rhs[4] <- 2
        
      }
      
      if (check_wr == 2) {
        
        f.dir[4] <- ">="
        f.rhs[4] <- 1
        
      }
      
      if (check_wr == 3) {
        
        f.dir[4] <- ">="
        f.rhs[4] <- 0
        
      }
      
      if (check_wr == 4) {
        
        f.dir[4] <- "="
        f.rhs[4] <- 0
        
      }
      
      else {
        
        f.dir <- f.dir
        f.rhs <- f.rhs
        
      }
      
      if (check_te == 1) {
        
        f.dir[5] <- ">="
        f.rhs[5] <- 0
        
      }
      
      if (check_te == 2) {
        
        f.dir[5] <- "="
        f.rhs[5] <- 0
        
      }
      
      else {
        
        f.dir <- f.dir
        f.rhs <- f.rhs
        
      }
      
      if (check_dst == 1) {
        
        f.dir[6] <- "="
        f.rhs[6] <- 0
        
      }
      
      else {
        
        f.dir <- f.dir
        f.rhs <- f.rhs
        
      }
      
      opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
      picks <- player_pool[which(opt$solution == 1), ]
      
      return(picks)
      
    }
    
  }
  
  else {
    
    player_pool <- df_full[!PLAYER %in% df_exclude$PLAYER]
    position_dt <- player_pool[, j = .(ppQB = ifelse(POSITION == "QB", 1, 0),
                                       ppRB = ifelse(POSITION == "RB", 1, 0),
                                       ppWR = ifelse(POSITION == "WR", 1, 0),
                                       ppTE = ifelse(POSITION == "TE", 1, 0),
                                       ppDST = ifelse(POSITION == "DST", 1, 0),
                                       ppFlex = ifelse(POSITION %in% c("RB", "WR", "TE"), 1, 0))]
    
    obj_points <- player_pool[, POINTS_DK]
    con_players <- t(cbind(SALARY = player_pool[, SALARY_DK], position_dt))
    colnames(con_players) <- player_pool$POINTS_DK
    
    f.dir <- rep(0, nrow(con_players))
    f.rhs <- rep(0, nrow(con_players))
    
    f.dir[1] <- "<="
    f.rhs[1] <- 50000
    
    f.dir[2:nrow(con_players)] <- c("=", ">=", ">=", ">=", "=", "=")
    f.rhs[2:nrow(con_players)] <- c(1, 2, 3, 1, 1, 7)
    
    opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
    picks <- player_pool[which(opt$solution == 1), ]
    
    return(picks)
    
  }
  
}

df_full <- data.table::fread("Output/salaries_projections_scraped_script.csv")[!TEAM %in% NON_SLATE][!is.na(SALARY_YH)]
    
df_include <- df_full[PLAYER %in% c("Kyler Murray", "George Kittle", "Curtis Samuel")]
df_exclude <- df_full[PLAYER %in% c("asdfasdfa")]
optimize_dk(df_full, df_include, df_exclude)  

df_include <- df_full[PLAYER %in% c("Kyler Murray", "George Kittle", "Jordan Reed")]
df_exclude <- df_full[PLAYER %in% c("Kenyan Drake")]
optimize_dk(df_full, df_include, df_exclude)  

df_include <- df_full[PLAYER %in% c("Miles Sanders", "Kenyan Drake", "Chris Carson")]
df_exclude <- df_full[PLAYER %in% c("Chris Herndon")]
optimize_dk(df_full, df_include, df_exclude)  

df_include <- df_full[PLAYER %in% c("Carolina D/ST")]
df_exclude <- df_full[PLAYER %in% c("Braxton Berrios", "Chris Herndon")]
optimize_dk(df_full, df_include, df_exclude)  

df_include <- df_full[PLAYER %in% c("ASDFASDFAS")]
df_exclude <- df_full[PLAYER %in% c("Larry Fitzgerald", "Davante Adams")]
optimize_dk(df_full, df_include, df_exclude) 

df_include <- df_full[PLAYER %in% c("ASDFASDFAS")]
df_exclude <- df_full[PLAYER %in% c("asdfasdfas")]
optimize_dk(df_full, df_include, df_exclude) 
