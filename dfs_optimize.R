library(dummies)
library(lpSolve)

fd <- projections_salaries[order(projections_salaries[, "POSITION"]), ]
Position.Mat <- dummies::dummy(fd[, POSITION])
Position.Mat <- cbind(Position.Mat, fdFlex = rowSums(Position.Mat[, c(3, 4, 5)]))

f.obj <- fd[, POINTS]

f.con <- t(cbind(SALARY = fd[, SALARY], Position.Mat))
colnames(f.con) <- fd$PLAYER_NAME

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
sum(picks$SALARY)
