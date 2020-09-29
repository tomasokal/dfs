library(shiny)
library(shinyWidgets)
library(shinyjs)
library(data.table)
library(DT)

full_salaries <- data.table::fread("https://raw.githubusercontent.com/tomasokal/dfs/production/Output/salaries_projections_scraped_script.csv")



# Define UI for application that draws a histogram
ui <- 

    navbarPage(id = "fullpage",
               #shinyjs::useShinyjs(),
               title = "DFS Linear Optimization App",
               header = tags$head(includeCSS("www/styles.css")),
               
               tabPanel(title = "Optimize",
                   
                        fluidRow(
                            
                            
                            column(5,
                                   
                                   column(1),
                                   column(10,
                                          
                                          p("Select a salary source:"),
                                          radioGroupButtons(
                                              inputId = "source-group",
                                              size = "lg",
                                              choices = c("DraftKings", "FanDuel", " Yahoo Sports"),
                                              individual = TRUE,
                                              width = "100%",
                                              status = 'testbutton',
                                              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                          ),
                                          p(HTML("<i>Salaries last updated: 9/25/2020 3:01PM</i>")),
                                          actionButton(inputId = "runbutton",
                                                       label = "Optimize",
                                                       class = "btncolor"),
                                          actionButton(inputId = "reset",
                                                       label = "Reset selections",
                                                       class = "btncolor"),
                                          p(HTML("<br>Optimizes for Sunday slate. More options coming soon.<br>"))),
                                   column(1)),
                            column(7,
                                   
                                   column(10,
                                          wellPanel(class = "optiwell",
                                                        p("Your optimized lineup:"),
                                                    style = "height:25vh;")
                                          ),
                                   )
                                   
                        ),
                        fluidRow(
                            column(5,
                                   column(1),
                                   column(11,
                                          wellPanel(class = "wellclass",
                                                    style = "height:60vh;",
                                          div(br(), DT::DTOutput(outputId = "player_list_table"))
                                          ))#,
                                   #column(1)
                                   
                                   
                                   ),
                            column(3,
                                   wellPanel(class = "wellclass",
                                             style = "height:60vh",
                                             textOutput(outputId = "myText"))
                                   ),
                            column(3,
                                   wellPanel(class = "wellclass",
                                             style = "height:60vh",
                                             textOutput(outputId = "myText2") )
                                   )
                        )
                   
               ),
               
               tabPanel(title = "Methodology"
                   
                   
               )
               
               
               
   )
    


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    myValue <- reactiveValues(check = '')
    
    myValue2 <- reactiveValues(check = '')
    
    shinyInput <- function(FUN, len, id, ...) {
        inputs <- character(len)
        for (i in seq_len(len)) {
            inputs[i] <- as.character(FUN(paste0(id, i), ...))
        }
        inputs
    }


    # babey's first reactive table
    
    player_list <- reactive({
        tibble::tibble(
            Player = full_salaries$PLAYER,
            Position = full_salaries$POSITION,
            'Expected Points' = full_salaries$POINTS,
            Include = shinyInput(actionButton, nrow(full_salaries),
                                 'button_',
                                 label = "Include",
                                 class = "include",
                                 onclick = paste0('Shiny.onInputChange( \"select_button\" , this.id)') 
            ),
            Exclude = shinyInput(actionButton, nrow(full_salaries),
                                          'button_',
                                          label = "Exclude",
                                 class = "exclude",
                                          onclick = paste0('Shiny.onInputChange( \"select_button2\" , this.id)') 
            ) 
        )
    })
    
    
    output$player_list_table <- DT::renderDT({
        player_list()
    },
    escape = FALSE,
    selection = "none",
    options = list(
        pageLength = 50,
        scrollY = "35vh",
        scroller = TRUE,
        lengthMenu = list(c(50, 100, -1), c("50", "100", "All")),
        initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().container()).css({'font-size': '90%'});",
            "}"),
        columnDefs = list(list(className = 'dt-center', targets = 2:5),
                          list(width = '18%', targets = 3))
        
    ))
    
    observeEvent(input$select_button, {
        selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
        myValue$check <<- paste(player_list()[selectedRow,1])
    })
    
    
    output$myText <- renderText({
        myValue$check
    })
    
    observeEvent(input$select_button2, {
        selectedRow <- as.numeric(strsplit(input$select_button2, "_")[[1]][2])
        myValue2$check <<- paste(player_list()[selectedRow,1])
    })
    
    
    output$myText2 <- renderText({
        myValue2$check
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
