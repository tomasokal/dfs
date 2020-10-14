library(shiny)
library(DT)

shinyApp(
  ui = fluidPage(
    dataTableOutput('table'),
    verbatimTextOutput('hoverIndex'),
    verbatimTextOutput('clickIndex'),
    verbatimTextOutput('dblclickIndex')
  ),
  
  server = function(server, input, output) {
    
    output$hoverIndex <- renderText({
      UI_out <- input$hoverIndexJS
      return(paste("hover column info", UI_out))
    })
    output$clickIndex <- renderText({
      UI_out <- input$clickIndexJS
      return(paste("click cell info:", UI_out))
    })
    output$dblclickIndex <- renderText({
      UI_out <- input$dblclickIndexJS
      return(paste("dblclick row info:", UI_out))
    })
    
    output$table <- renderDataTable({
      DT_out <- data.frame(`A` = 1:5, `B` = 11:15, `C` = LETTERS[1:5])
      DT_out <- datatable(DT_out
                          ,rownames = F
                          ,callback = JS("
                                                /* code for columns on hover */
                                                table.on('mouseenter', 'td', function() {
                                                    var td = $(this);
                                                    var info_out = table.cell( this ).index().columnVisible;
                                                    Shiny.onInputChange('hoverIndexJS', info_out);
                                                });
                                                /* code for cell content on click */
                                                table.on('click', 'td', function() {
                                                   var td = $(this);
                                                   var info_out = table.cell( this ).data();
                                                   Shiny.onInputChange('clickIndexJS', info_out);
                                                });
                                                /* code for columns on doubleclick */
                                                table.on('dblclick', 'td', function() {
                                                    var td = $(this);
                                                    var info_out = table.cell( this ).index().row;
                                                    Shiny.onInputChange('dblclickIndexJS', info_out);
                                                });"
                                         
                          )
      )
      return(DT_out)
    })
  }
)