library(shiny)
library(DT)



shinyApp(
  ui = fluidPage(DT::dataTableOutput("mtcarsTable")),
  server = function(input, output) {
    
    output$mtcarsTable <- renderDataTable({
      DT::datatable(datasets::mtcars, 
                    options = list(rowCallback = JS(
                      "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                      "$('td:eq(3)', nRow).css('background-color', '#9BF59B');",
                      "var full_text = aData[3]",
                      "$('td:eq(3)', nRow).attr('title', full_text);",
                      "$('td:eq(3)', nRow).tooltip({",
                      "'delay': 0,",
                      "'track': true,",
                      "'fade': 250,",
                      "});",
                      
                      
                      "}")
                    )
      )
      
    })
  }
)