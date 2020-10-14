library(shiny)
library(DT)

data(mpg, package = "ggplot2")

callback <- c(
  "table.on('click', 'td', function(){",
  "  var index = table.cell(this).index();",
  "  Shiny.setInputValue('cell', index, {priority: 'event'});",
  "});"
)

ui <- fluidPage(
  br(),
  DTOutput("tbl")
)

server <- function(input, output, session){
  
  dat <- mpg
  
  output[["tbl"]] <- renderDT({
    datatable(
      dat,
      callback = JS(callback)
    )
  })
  
  filteredData <- eventReactive(input[["cell"]], {
    i <- input[["cell"]]$row + 1
    j <- input[["cell"]]$column
    if(j > 0){
      dat[dat[[j]] == dat[i,j], , drop = FALSE]
    }else{
      NULL
    }
  })
  
  output[["tblfiltered"]] <- renderDT({
    datatable(
      filteredData(),
      fillContainer = TRUE, 
      options = list(
        pageLength = 5
      )
    )
  })
  
  observeEvent(filteredData(), {
    showModal(
      modalDialog(
        DTOutput("tblfiltered"), 
        size = "l", 
        easyClose = TRUE
      )
    )
  })
  
}

shinyApp(ui, server)