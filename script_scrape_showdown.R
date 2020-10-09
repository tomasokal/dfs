# Necessary libraries
library(data.table)
library(rvest)
library(xml2)
library(lpSolve)

## Offense 
url <- "https://www.numberfire.com/nfl/fantasy/fantasy-football-projections"
page <- rvest::html_table(xml2::read_html(url)
                          , header = TRUE
                          , fill = TRUE)
projections1 <- data.table::data.table(page[[1]])
projections2 <- data.table::data.table(page[[2]])
projections_dfs <- cbind(projections1, projections2)
colnames(projections_dfs) <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23", "v24")
projections_dfs <- projections_dfs[-1, ]
projections_dfs <- projections_dfs[, gsub1 := gsub("\t", "", v1), by = v1][, gsub2 := gsub("\n", "_", gsub1), by = gsub1]
projections_dfs <- projections_dfs[, c("v25", "v26", "v27") := tstrsplit(gsub2, "_", fixed = TRUE)]
projections_dfs <- projections_dfs[, .(PLAYER = v25
                                       , POSITION = substr(gsub(".*\\((.*)\\).*", "\\1", v27), 1, 2)
                                       , TEAM = trimws(substr(gsub(".*\\((.*)\\).*", "\\1", v27), 4, nchar(gsub(".*\\((.*)\\).*", "\\1", v27))))
                                       , POINTS_FD = as.numeric(gsub("[\\$,]","", v16))
                                       , POINTS_DK = as.numeric(gsub("[\\$,]","", v19))
                                       , POINTS_YH = as.numeric(gsub("[\\$,]","", v22))
                                       , SALARY_FD = as.numeric(gsub("[\\$,]","", v17))
                                       , SALARY_DK = as.numeric(gsub("[\\$,]","", v20))
                                       , SALARY_YH = as.numeric(gsub("[\\$,]","", v23)))]

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
                                       , TEAM = trimws(substr(gsub(".*\\((.*)\\).*", "\\1", v24), 4, nchar(gsub(".*\\((.*)\\).*", "\\1", v24))))
                                       , POINTS_FD = as.numeric(gsub("[\\$,]","", v13))
                                       , POINTS_DK = as.numeric(gsub("[\\$,]","", v16))
                                       , POINTS_YH = as.numeric(gsub("[\\$,]","", v19))
                                       , SALARY_FD = as.numeric(gsub("[\\$,]","", v14))
                                       , SALARY_DK = as.numeric(gsub("[\\$,]","", v17))
                                       , SALARY_YH = as.numeric(gsub("[\\$,]","", v20)))]

## Kickers
url <- "https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/k"
page <- rvest::html_table(xml2::read_html(url)
                          , header = TRUE
                          , fill = TRUE)
projections1 <- data.table::data.table(page[[1]])
projections2 <- data.table::data.table(page[[2]])
projections_k <- cbind(projections1, projections2)
colnames(projections_k) <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23")
projections_k <- projections_k[-1, ]
projections_k <- projections_k[, gsub1 := gsub("\t", "", v1), by = v1][, gsub2 := gsub("\n", "_", gsub1), by = gsub1]
projections_k <- projections_k[, c("v24", "v25", "v26") := tstrsplit(gsub2, "_", fixed = TRUE)]
projections_k <- projections_k[, .(PLAYER = v24
                                       , POSITION = "K"
                                       , TEAM = trimws(substr(gsub(".*\\((.*)\\).*", "\\1", v26), 4, nchar(gsub(".*\\((.*)\\).*", "\\1", v26))))
                                       , POINTS_DK = as.numeric(gsub("[\\$,]","", v18)))]

#