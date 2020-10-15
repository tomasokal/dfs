colorfunc <- colorRamp(c("#FFA500", "white", "#19BDFF"))

df <- data.table::fread("https://raw.githubusercontent.com/tomasokal/dfs/production/Output/salaries_projections_main_slate.csv")

check <- (df$DIFF_DK[!is.na(df$DIFF_DK)]-min(df$DIFF_DK[!is.na(df$DIFF_DK)]))/(max(df$DIFF_DK[!is.na(df$DIFF_DK)])-min(df$DIFF_DK[!is.na(df$DIFF_DK)]))

check1 <- rgb(colorfunc(check), maxColorValue = 255)




(df$DIFF_DK[!is.na(df$DIFF_DK)]-min(df$DIFF_DK[!is.na(df$DIFF_DK)]))/(max(df$DIFF_DK[!is.na(df$DIFF_DK)])-min(df$DIFF_DK[!is.na(df$DIFF_DK)]))

View(check)

check <- ifelse(is.na(df$DIFF_DK), NA, rgb(colorfunc(df$DIFF_DK), maxColorValue = 255))
