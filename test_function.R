library(data.table)
library(lpSolve)

NON_SLATE <- c("NYJ", "DEN", "PHI", "SFO", "ATL", "GBP")

df_full <- data.table::fread("Output/salaries_projections_scraped_script.csv")[!TEAM %in% NON_SLATE][!is.na(SALARY_YH)]
df_include <- df_full[PLAYER %in% c("Kyler Murray", "Lamar Jackson", "Curtis Samuel")]
df_exclude <- df_full[PLAYER %in% c("asdfasdfa")]
optimize_dk(df_full, df_include, df_exclude)  