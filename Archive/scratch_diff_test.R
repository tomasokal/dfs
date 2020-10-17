library(lpSolve)
library(data.table)

df <- data.table::fread("Output/salaries_projections_main_slate.csv")

slate_main <- df

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

eval_slate <- data.table::data.table(PLAYER = player_pool$PLAYER
                                     , SALARY = player_pool$SALARY_DK
                                     , PROJECTED_POINTS = player_pool$POINTS_DK
                                     , NEEDED_POINTS = unlist(new_points)
)[, DIFF := NEEDED_POINTS - PROJECTED_POINTS]


PLAYER_EVAL <- "Tanner Hudson"

picks_new <- picks_base

for (i in 1:20) {
  
  if () {
    
    repeat {
      
      picks_new <- picks_base
      obj_points <- player_pool[, .(POINTS = POINTS_DK)]
      exp_points <- obj_points[i]
      
      exp_points <- exp_points + 0.01
      print(exp_points)
      
      if (exp_points > 10) {
        
        break
        
      }
      
    }
    
  }
  
  else {
    
    asdf
    
  }
  
}
  
  
  picks_new <- picks_base
  obj_points <- player_pool[, .(POINTS = POINTS_DK)]
  exp_points <- obj_points[i]
  
  repeat {
    
    exp_points <- exp_points + 0.01
    print(exp_points)
    
    if (exp_points > 10) {
      
      break
      
    }
    
  }
  
  
}

  
  picks_new <- picks_base
  
  obj_points <- player_pool[, .(POINTS = POINTS_DK)]
  
  exp_points <- obj_points[300]
  
  repeat {
    
    exp_points <- exp_points + 0.01
    print(exp_points)
    
    if (exp_points > 10) {
      
      break
      
    }
    
  }


while (!PLAYER_EVAL %in% picks_new$PLAYER) {
  
  obj_points <- player_pool[, .(POINTS = POINTS_DK)]
  obj_points[300, 5] <- obj_points[300, 5] + 0.1
  print(obj_points[300, 5])
  
  con_players <- t(cbind(SALARY = player_pool[, SALARY_DK], position_dt))
  colnames(con_players) <- player_pool$PLAYER
  
  opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
  
  picks_new <- player_pool[which(opt$solution == 1), ][, .(PLAYER, POSITION, TEAM, POINTS = POINTS_DK, SALARY = SALARY_DK)]
  
}


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
