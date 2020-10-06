library(data.table)
library(lpSolve)

NON_SLATE <- c("NYJ", "DEN", "PHI", "SFO", "ATL", "GBP")

df_full <- slate_main[!is.na(SALARY_DK)]
df_include <- slate_main[PLAYER %in% c("Kyler Murray")]
df_exclude <- df_full[PLAYER %in% c("Kareem Hunt")]
optimize_dk(df_full, df_include, df_exclude)  
