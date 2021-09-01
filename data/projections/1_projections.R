# Source of data
url <- "https://www.numberfire.com/nfl/fantasy/fantasy-football-projections"
page <- rvest::html_table(xml2::read_html(url)
                          , header = TRUE
                          , fill = TRUE)

# Parse data
scrape_ids <- data.table::data.table(page[[1]])
scrape_data <- data.table::data.table(page[[2]])
scrape_projections <- cbind(scrape_ids, scrape_data)

# Change column names
colnames(scrape_projections) <- paste0("v", 1:ncol(scrape_projections))

# Remove headers
scrape_projections <- scrape_projections[-1, ]

# Select features
scrape_projections <- scrape_projections[, gsub1 := gsub("\t", "", v1), by = v1][, gsub2 := gsub("\n", "_", gsub1), by = gsub1]
scrape_projections <- scrape_projections[, c("v25", "v26", "v27") := tstrsplit(gsub2, "_", fixed = TRUE)]
scrape_projections <- scrape_projections[, .(PLAYER = v25
                                             , POSITION = substr(gsub(".*\\((.*)\\).*", "\\1", v27), 1, 2)
                                             , TEAM = trimws(substr(gsub(".*\\((.*)\\).*", "\\1", v27), 4, nchar(gsub(".*\\((.*)\\).*", "\\1", v27))))
                                             , POINTS_FD = as.numeric(gsub("[\\$,]","", v16))
                                             , POINTS_DK = as.numeric(gsub("[\\$,]","", v19))
                                             , SALARY_FD = as.numeric(gsub("[\\$,]","", v17))
                                             , SALARY_DK = as.numeric(gsub("[\\$,]","", v20))
                                             , RUN_ATT = as.numeric(v10)
                                             , RUN_YDS = as.numeric(v11)
                                             , RUN_TDS = as.numeric(v12)
                                             , REC_ATT = as.numeric(v13)
                                             , REC_YDS = as.numeric(v14)
                                             , REC_TDS = as.numeric(v15)
                                             , PASS_YDS = as.numeric(v7)
                                             , PASS_TDS = as.numeric(v8)
                                             , PASS_INT = as.numeric(v9))]
                                             
