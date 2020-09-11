web_page_generate <- function(position, week) {
  
  web_page <- paste0("https://www.fantasypros.com/nfl/projections/"
                     , position
                     , ".php?week="
                     , week)
  
  return(web_page)
  
}

position_list <- list(c("qb", "rb", "wr", "te", "k", "dst"))

web_pages_list <- lapply(position_list, web_page_generate, week = 1)
web_pages_list <- as.list(unlist(web_pages_list))

scrape_projections <- function(web_page) {
  
  projections_table <- rvest::html_table(xml2::read_html(web_page)
                                         , header = TRUE
                                         , fill = TRUE)
  
}

projections_table_list <- lapply(web_pages_list, scrape_projections)

combine_projections <- function(projection_table) {
  
  projection_table <- data.table::data.table(projection_table[[1]])
  
  if (projection_table[1, 1] == "Player") {
    
    projection_table <- projection_table[-1, ]
    
  }
  
  else {
    
    projection_table <- projection_table
    
  }
  
  return(projection_table)
  
}

check <- lapply(projections_table_list, combine_projections)
