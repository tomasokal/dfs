library(shiny)
library(shinyWidgets)
library(shinyjs)

#full_salaries <- read.csv("Output/salaries_projections_scraped_script.csv")



# Define UI for application that draws a histogram
ui <- 

    navbarPage(id = "fullpage",
               title = "DFS Linear Optimization App",
               header = tags$head(includeCSS("www/styles.css")),
               
               tabPanel(title = "Optimize",
                   
                        fluidRow(
                            
                            column(4,
                                   
                                   column(1),
                                   column(10,
                                          
                                          p("Select a salary source:"),
                                          radioGroupButtons(
                                              inputId = "source-group",
                                              size = "lg",
                                              choices = c("DraftKings", "Fantasy Pros", " Yahoo Sports"),
                                              individual = TRUE,
                                              width = "100%",
                                              #status = 'btn-testbutton',
                                              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                          ),
                                          p(HTML("<i>Salaries last updated: 9/25/2020 3:01PM</i>")),
                                          actionButton(inputId = "runbutton",
                                                       label = "Optimize",
                                                       width = "45%"),
                                          actionButton(inputId = "reset",
                                                       label = "Reset selections",
                                                       width = "45%")),
                                   column(1)),
                            column(8,
                                   
                                   column(1),
                                   column(10,
                                          wellPanel(p("Your optimized lineup:"))
                                          ),
                                   column(1))
                                   
                        )
                   
               ),
               
               tabPanel(title = "Methodology"
                   
                   
               )
               
               
               
   )
    


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
