# Necessary libraries
library(data.table)
library(lpSolve)

# Setting the non-slate games
NON_SLATE <- c("NYJ", "DEN", "PHI", "SF", "ATL", "GB")

  # Needs to be automated.

# Optimize
player_pool <- merge_full[!TEAM %in% NON_SLATE][!is.na(SALARY_DK)]
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

# Export data 
data.table::fwrite(picks_dk, "Output/picks_dk.csv")