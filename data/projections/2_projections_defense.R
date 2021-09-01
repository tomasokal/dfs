# Source of data
url <- "https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/d"
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
scrape_projections <- scrape_projections[-1, ]
scrape_projections <- scrape_projections[, gsub1 := gsub("\t", "", v1), by = v1][, gsub2 := gsub("\n", "_", gsub1), by = gsub1]
scrape_projections <- scrape_projections[, c("v22", "v23", "v24") := tstrsplit(gsub2, "_", fixed = TRUE)]
scrape_projections <- scrape_projections[, .(PLAYER = v22
                                             , POSITION = "DST"
                                             , TEAM = trimws(substr(gsub(".*\\((.*)\\).*", "\\1", v24), 4, nchar(gsub(".*\\((.*)\\).*", "\\1", v24))))
                                             , POINTS_FD = as.numeric(gsub("[\\$,]","", v13))
                                             , POINTS_DK = as.numeric(gsub("[\\$,]","", v16))
                                             , SALARY_FD = as.numeric(gsub("[\\$,]","", v14))
                                             , SALARY_DK = as.numeric(gsub("[\\$,]","", v17)))]

