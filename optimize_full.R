optimize_full <- function(df_to_use, df_include, df_exclude, site) {
  
  
  inc_players <- df_include()
  exc_players <- df_exclude()
  
  
  salary_full <- switch(input$sourcegroup, 
                        "DraftKings" = 50000, 
                        "FanDuel" = 60000, 
                        "Yahoo Sports" = 200
  )
  
  salary_switch <- switch(input$sourcegroup, "DraftKings" = "SALARY_DK", "FanDuel" = "SALARY_FD", "Yahoo Sports" = "SALARY_YH")
  points_switch <- switch(input$sourcegroup, "DraftKings" = "POINTS_DK", "FanDuel" = "POINTS_FD", "Yahoo Sports" = "POINTS_YH")
  
  # salary_full <- switch(testsite, 
  #                       "DraftKings" = 50000, 
  #                       "FanDuel" = 60000, 
  #                       "Yahoo Sports" = 200
  # )
  
  # salary_switch <- switch(testsite, "DraftKings" = "SALARY_DK", "FanDuel" = "SALARY_FD", "Yahoo Sports" = "SALARY_YH")
  # points_switch <- switch(testsite, "DraftKings" = "POINTS_DK", "FanDuel" = "POINTS_FD", "Yahoo Sports" = "POINTS_YH")
  
  df_full <- df_to_use %>%
    dplyr::select(PLAYER, POSITION, TEAM, salary_switch, points_switch) %>%
    `colnames<-`(c("Player", "Position", "Salary", "Points")) %>%
    dplyr::filter(!is.na(Salary))
  
  # df_full <- full_salaries %>%
  #   dplyr::select(PLAYER, POSITION, TEAM, salary_switch, points_switch) %>%
  #   `colnames<-`(c("Player", "Position", "Team", "Salary", "Points")) %>%
  #   dplyr::filter(!is.na(Salary))
  
  
  if (dim(df_include)[1] > 0) {
    
    check_inclusion <- df_include[, .N, by = POSITION]
    check_qb <- ifelse(length(check_inclusion[POSITION == "QB"][[2]]) == 0, 0, check_inclusion[POSITION == "QB"][[2]])
    check_rb <- ifelse(length(check_inclusion[POSITION == "RB"][[2]]) == 0, 0, check_inclusion[POSITION == "RB"][[2]])
    check_wr <- ifelse(length(check_inclusion[POSITION == "WR"][[2]]) == 0, 0, check_inclusion[POSITION == "WR"][[2]])
    check_te <- ifelse(length(check_inclusion[POSITION == "TE"][[2]]) == 0, 0, check_inclusion[POSITION == "TE"][[2]])
    check_dst <- ifelse(length(check_inclusion[POSITION == "DST"][[2]]) == 0, 0, check_inclusion[POSITION == "DST"][[2]])
    check_flex <- ifelse(dim(check_inclusion[POSITION %in% c("RB", "WR", "TE")])[1] == 0, 0, sum(check_inclusion[POSITION %in% c("RB", "WR", "TE"), 2]))
    
    if (check_qb > 1 | check_rb > 3 | check_wr > 4 | check_te > 2 | check_dst > 1 | check_flex > 7 | sum(df_include$SALARY_DK) > 50000) {
      
      print("You are dumb.")
      
    }
    
    else {
      
      
      salary_inclusion <- sum(df_full[Player %in% inc_players$PLAYER][["Salary"]])
      
      
      # salary_inclusion <- sum(df_include$SALARY_DK)
      
      player_pool <- df_full[!Player %in% inc_players$PLAYER][!Player %in% exc_players$PLAYER]
      
      position_dt <- player_pool[, j = .(ppQB = ifelse(Position == "QB", 1, 0),
                                         ppRB = ifelse(Position == "RB", 1, 0),
                                         ppWR = ifelse(Position == "WR", 1, 0),
                                         ppTE = ifelse(Position == "TE", 1, 0),
                                         ppDST = ifelse(Position == "DST", 1, 0),
                                         ppFlex = ifelse(Position %in% c("RB", "WR", "TE"), 1, 0))]
      
      obj_points <- player_pool[, Points]
      
      con_players <- t(cbind(SALARY = player_pool[, Salary], position_dt))
      colnames(con_players) <- player_pool$Points
      
      f.dir <- rep(0, nrow(con_players))
      f.rhs <- rep(0, nrow(con_players))
      
      f.dir[1] <- "<="
      f.rhs[1] <- salary_full - salary_inclusion
      
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
    
    player_pool <- df_full[!Player %in% exc_players$PLAYER]
    position_dt <- player_pool[, j = .(ppQB = ifelse(Position == "QB", 1, 0),
                                       ppRB = ifelse(Position == "RB", 1, 0),
                                       ppWR = ifelse(Position == "WR", 1, 0),
                                       ppTE = ifelse(Position == "TE", 1, 0),
                                       ppDST = ifelse(Position == "DST", 1, 0),
                                       ppFlex = ifelse(Position %in% c("RB", "WR", "TE"), 1, 0))]
    
    obj_points <- player_pool[, Points]
    con_players <- t(cbind(SALARY = player_pool[, Salary], position_dt))
    colnames(con_players) <- player_pool$Points
    
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
