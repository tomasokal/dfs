colorfunc <- colorRamp(c("#FFA500", "white", "#19BDFF"))


check1 <- rgb(colorfunc(check), maxColorValue = 255)

check <- (df$DIFF_DK[!is.na(df$DIFF_DK)]-min(df$DIFF_DK[!is.na(df$DIFF_DK)]))/(max(df$DIFF_DK[!is.na(df$DIFF_DK)])-min(df$DIFF_DK[!is.na(df$DIFF_DK)]))


(df$DIFF_DK[!is.na(df$DIFF_DK)]-min(df$DIFF_DK[!is.na(df$DIFF_DK)]))/(max(df$DIFF_DK[!is.na(df$DIFF_DK)])-min(df$DIFF_DK[!is.na(df$DIFF_DK)]))

View(check)

check <- ifelse(is.na(df$DIFF_DK), NA, rgb(colorfunc(df$DIFF_DK), maxColorValue = 255))
