## PPR Scoring
url <- "https://www.numberfire.com/nfl/fantasy/fantasy-football-ppr-projections"
page <- rvest::html_table(xml2::read_html(url)
                          , header = TRUE
                          , fill = TRUE)
projections1 <- data.table::data.table(page[[1]])
projections2 <- data.table::data.table(page[[2]])
projections_ppr <- cbind(projections1, projections2)
colnames(projections_ppr) <- as.character(projections_ppr[1, ])
projections_ppr <- projections_ppr[-1, ]
projections_ppr <- projections_ppr[-1, ][, gsub1 := gsub("\t", "", Player), by = Player][, gsub2 := gsub("\n", "_", gsub1), by = gsub1]
projections_ppr <- projections_ppr[, c("v1", "v2", "v3") := tstrsplit(gsub2, "_", fixed = TRUE)]

## Normal Scoring
url <- "https://www.numberfire.com/nfl/fantasy/fantasy-football-projections"
page <- rvest::html_table(xml2::read_html(url)
                          , header = TRUE
                          , fill = TRUE)
projections1 <- data.table::data.table(page[[1]])
projections2 <- data.table::data.table(page[[2]])
projections_std <- cbind(projections1, projections2)