library(shiny)
library(shinyWidgets)
library(shinyjs)
library(data.table)
library(DT)

full_salaries <- data.table::fread("https://raw.githubusercontent.com/tomasokal/dfs/production/Output/salaries_projections_scraped_script.csv")


load(url("https://github.com/tomasokal/dfs/raw/production/Output/time.RData"))

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
                                              inputId = "sourcegroup",
                                              size = "lg",
                                              choices = c("DraftKings", "FanDuel", "Yahoo Sports"),
                                              individual = TRUE,
                                              width = "100%",
                                              status = 'testbutton',
                                              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                          ),
                                          p(HTML(paste0("<i>Salaries last updated: ", time, "</i>"))),
                                          actionButton(inputId = "runbutton",
                                                       icon = icon("chart-bar"),
                                                       label = "Optimize",
                                                       class = "btncolor"),
                                          actionButton(inputId = "reset",
                                                       label = "Reset selections",
                                                       icon = icon("undo"),
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
                                          div(
                                              br(), 
                                              DT::DTOutput(outputId = "player_list_table"))
                                          ))#,
                                   #column(1)
                                   
                                   
                                   ),
                            column(3,
                                   wellPanel(class = "wellclass",
                                             style = "height:60vh",
                                             div(p("Players to include in the optimized lineup:"),
                                                 br(), 
                                                 DT::DTOutput(outputId = "player_list_include"))
                                             #textOutput(outputId = "myText")
                                             )
                                   ),
                            column(3,
                                   wellPanel(class = "wellclass",
                                             style = "height:60vh",
                                             div(p("Players to exclude from the optimized lineup:"),
                                                 br(), 
                                                 DT::DTOutput(outputId = "player_list_exclude")
                                                 # textOutput(outputId = "myText2") 
                                                 )
                                             
                                             )
                                   )
                        )
                   
               ),
               
               tabPanel(title = "Methodology"
                   
                   
               )
               
               
               
   )
    


# Define server logic required to draw a histogram
server <- function(input, output) {
    

    
    shinyInput <- function(FUN, len, id, ...) {
        inputs <- character(len)
        for (i in seq_len(len)) {
            inputs[i] <- as.character(FUN(paste0(id, i), ...))
        }
        inputs
    }


    # babey's first reactive table
    
    # player_list <- reactive({
    # 
    # 
    #     source_switch <- switch(input$sourcegroup, "DraftKings" = "SALARY_DK", "FanDuel" = "SALARY_FD", "Yahoo Sports" = "SALARY_YH")
    #     points_switch <- switch(input$sourcegroup, "DraftKings" = "POINTS_DK", "FanDuel" = "POINTS_FD", "Yahoo Sports" = "POINTS_YH")
    # 
    # 
    #     players_full <- dplyr::select(full_salaries, PLAYER, POSITION, source_switch, points_switch)
    # 
    # 
    # 
    #     tibble::tibble(
    # 
    # 
    #         Player = full_salaries$PLAYER,
    #         `Pos.` = full_salaries$POSITION,
    #         Salary = players_full[[ncol(players_full)-1]],
    #         Points = players_full[[ncol(players_full)]],
    #         # Points = full_salaries$POINTS_DK,
    #         # Salary = full_salaries$SALARY_DK,
    #         Include = shinyInput(actionButton, nrow(full_salaries),
    #                              'button_',
    #                              label = "",
    #                              icon = icon("check"),
    #                              class = "include",
    #                              onclick = paste0('Shiny.onInputChange( \"select_button\" , this.id)')
    #         ),
    #         Exclude = shinyInput(actionButton, nrow(full_salaries),
    #                                       'button_',
    #                                       label = "",
    #                              icon = icon("times"),
    #                              class = "exclude",
    #                                       onclick = paste0('Shiny.onInputChange( \"select_button2\" , this.id)')
    #         )
    #     )
    # })
    
    
    player_list_dk <- reactive({
        
        
        
        tibble::tibble(
            
            
            Player = full_salaries$PLAYER,
            `Pos.` = full_salaries$POSITION,
            Points = full_salaries$POINTS_DK,
            Salary = full_salaries$SALARY_DK,
            Include = shinyInput(actionButton, nrow(full_salaries),
                                 'button_',
                                 label = "",
                                 icon = icon("check"),
                                 class = "include",
                                 onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'
            ),
            Exclude = shinyInput(actionButton, nrow(full_salaries),
                                 'button_',
                                 label = "",
                                 icon = icon("times"),
                                 class = "exclude",
                                 onclick = 'Shiny.setInputValue(\"select_button2\", this.id, {priority: \"event\"})'
            )
        )
    })
    
    player_list_fd <- reactive({
        
        
        
        tibble::tibble(
            
            
            Player = full_salaries$PLAYER,
            `Pos.` = full_salaries$POSITION,
            Points = full_salaries$POINTS_FD,
            Salary = full_salaries$SALARY_FD,
            Include = shinyInput(actionButton, nrow(full_salaries),
                                 'button_',
                                 label = "",
                                 icon = icon("check"),
                                 class = "include",
                                 onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'
            ),
            Exclude = shinyInput(actionButton, nrow(full_salaries),
                                 'button_',
                                 label = "",
                                 icon = icon("times"),
                                 class = "exclude",
                                 onclick = 'Shiny.setInputValue(\"select_button2\", this.id, {priority: \"event\"})'
            )
        )
    })
    
    
    player_list_yh <- reactive({
        
        
        
        tibble::tibble(
            
            
            Player = full_salaries$PLAYER,
            `Pos.` = full_salaries$POSITION,
            Points = full_salaries$POINTS_YH,
            Salary = full_salaries$SALARY_YH,
            Include = shinyInput(actionButton, nrow(full_salaries),
                                 'button_',
                                 label = "",
                                 icon = icon("check"),
                                 class = "include",
                                 onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'
            ),
            Exclude = shinyInput(actionButton, nrow(full_salaries),
                                 'button_',
                                 label = "",
                                 icon = icon("times"),
                                 class = "exclude",
                                 onclick = 'Shiny.setInputValue(\"select_button2\", this.id, {priority: \"event\"})'
            )
        )
    })
    
    
    proper_tb <- reactiveVal(NULL)
    
    
    observeEvent(input$sourcegroup, {
        

        proper_tb(switch(input$sourcegroup, "DraftKings" = player_list_dk(), "FanDuel" = player_list_fd(), "Yahoo Sports" = player_list_yh()))
        
        # player_include(NULL)
        # player_exclude(NULL)
        
        
    })
    
    
    
    output$player_list_table <- DT::renderDT({
        #player_list()
        proper_tb()
    },
    escape = FALSE,
    selection = "none",
    options = list(
        pageLength = 50,
        scrollY = "37.5vh",
        scroller = TRUE,
        lengthMenu = list(c(50, 100, -1), c("50", "100", "All")),
        initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().container()).css({'font-size': '80%'});",
            "}"),
        #dom = 't', displays table only
        columnDefs = list(list(className = 'dt-center', targets = 2:6),
                          list(width = '18%', targets = 3),
                          list(width = '18%', targets = 4))
        
    ))
    
    
    
    # player_include <- reactiveVal(NULL)
    # player_exclude <- reactiveVal(NULL)
    
    pl_inc <- reactiveVal(NULL)
    pl_exc <- reactiveVal(NULL)
    
    
    observeEvent(input$select_button, {
        selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
        player_new <- paste(proper_tb()[selectedRow,1])
        
        
        old_values <- pl_inc() 
        new_values <- player_new
        
        ifelse(!is.null(pl_inc()),
               new_vec <- c(old_values, new_values),
               new_vec <- new_values)
        
        pl_inc(new_vec)
        
        
        pl_exc_new <- pl_exc()[!pl_exc()==player_new]
        
        if (length(pl_exc_new)==0) {pl_exc(NULL)}
        
        
        else {
            
            #store the result in values variable
            pl_exc(pl_exc_new)
            
        }
        

        
    })
    
    
    observeEvent(input$select_button2, {
        selectedRow <- as.numeric(strsplit(input$select_button2, "_")[[1]][2])
        player_new <- paste(proper_tb()[selectedRow,1])
        
        old_values <- pl_exc() 
        new_values <- player_new
        
        ifelse(!is.null(pl_exc()),
               new_vec <- c(old_values, new_values),
               new_vec <- new_values)
        
        pl_exc(new_vec)
        
        pl_inc_new <- pl_inc()[!pl_inc()==player_new]
        
        if (length(pl_inc_new)==0) {pl_inc(NULL)}
        
        
        else {
            
            #store the result in values variable
            pl_inc(pl_inc_new)
            
        }
        
        
    })
    
    
    

    observeEvent(input$select_button3, {

        selectedRow <- as.numeric(strsplit(input$select_button3, "_")[[1]][2])
        player_new <- paste(df_include()[selectedRow,1])

        pl_inc_new <- pl_inc()[!pl_inc()==player_new]
        
        if (length(pl_inc_new)==0) {pl_inc(NULL)}
        
        
        else {
            
            #store the result in values variable
            pl_inc(pl_inc_new)
            
        }



    })
    
    observeEvent(input$select_button4, {
        
        selectedRow <- as.numeric(strsplit(input$select_button4, "_")[[1]][2])
        player_new <- paste(df_exclude()[[selectedRow,1]])
        
        pl_exc_new <- pl_exc()[!pl_exc()==player_new]
        
        if (length(pl_exc_new)==0) {pl_exc(NULL)}
        
        
        else {
            
            #store the result in values variable
            pl_exc(pl_exc_new)
            
        }
        
        
        
    })
    
    
    df_include <- reactive({
        
        
        
        players <- subset(full_salaries, PLAYER %in% pl_inc())
        
        source_switch <- switch(input$sourcegroup, "DraftKings" = "SALARY_DK", "FanDuel" = "SALARY_FD", "Yahoo Sports" = "SALARY_YH") 
        
        players_fil <- dplyr::select(players, PLAYER, POSITION, source_switch)
        
        tibble::tibble(
            
            
            Player = players_fil$PLAYER,
            Position = players_fil$POSITION,
            Salary = players_fil[[ncol(players_fil)]],
            
            Remove = shinyInput(actionButton, nrow(players_fil),
                                 'button_',
                                label = "",
                                 icon = icon("minus-circle"),
                                 class = "remove",
                                onclick = 'Shiny.setInputValue(\"select_button3\", this.id, {priority: \"event\"})'
                                )
            )
    })
    
    
    df_exclude <- reactive({
        
        
        
        players <- subset(full_salaries, PLAYER %in% pl_exc())
        
        source_switch <- switch(input$sourcegroup, "DraftKings" = "SALARY_DK", "FanDuel" = "SALARY_FD", "Yahoo Sports" = "SALARY_YH") 
        
        players_fil <- dplyr::select(players, PLAYER, POSITION, source_switch)
        
        tibble::tibble(
            
            
            Player = players_fil$PLAYER,
            Position = players_fil$POSITION,
            Salary = players_fil[[ncol(players_fil)]],
            
            Remove = shinyInput(actionButton, nrow(players_fil),
                                'button_',
                                label = "",
                                icon = icon("minus-circle"),
                                class = "remove",
                                onclick = 'Shiny.setInputValue(\"select_button4\", this.id, {priority: \"event\"})'
            )
        )
    })
    
    
    
    output$player_list_include <- DT::renderDT({
        df_include()
    },
    escape = FALSE,
    selection = "none",
    options = list(
        initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().container()).css({'font-size': '80%'});",
            "}"),
        columnDefs = list(list(className = 'dt-center', targets = 2:4),
                          list(width = '18%', targets = 4)),
        dom = 't'
        
    ))
    
    
    output$player_list_exclude <- DT::renderDT({
        df_exclude()
    },
    escape = FALSE,
    selection = "none",
    options = list(
        initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().container()).css({'font-size': '80%'});",
            "}"),
        columnDefs = list(list(className = 'dt-center', targets = 2:4),
                          list(width = '18%', targets = 4)),
        dom = 't'
        
    ))
    
    observeEvent(input$reset, {
        
        pl_inc(NULL)
        
        pl_exc(NULL)
        
    })
    
    
    output$myText <- renderText({
        return(player_include())
    })
    
    output$myText2 <- renderText({
        #return(player_exclude())
        return(input$select_button3)
    })
    
    
    observeEvent(input$runbutton, {
        
        inc_test <- subset(full_salaries, PLAYER %in% pl_inc())[!TEAM %in% "NON_SLATE"][!is.na(SALARY_DK)] #param for source
        
        if (dim(inc_test)[1] > 0) {
            
            check_inclusion <- inc_test[, .N, by = POSITION]
            check_qb <- ifelse(length(check_inclusion[POSITION == "QB"][[2]]) == 0, 0, check_inclusion[POSITION == "QB"][[2]])
            check_rb <- ifelse(length(check_inclusion[POSITION == "RB"][[2]]) == 0, 0, check_inclusion[POSITION == "RB"][[2]])
            check_wr <- ifelse(length(check_inclusion[POSITION == "WR"][[2]]) == 0, 0, check_inclusion[POSITION == "WR"][[2]])
            check_te <- ifelse(length(check_inclusion[POSITION == "TE"][[2]]) == 0, 0, check_inclusion[POSITION == "TE"][[2]])
            check_dst <- ifelse(length(check_inclusion[POSITION == "DST"][[2]]) == 0, 0, check_inclusion[POSITION == "DST"][[2]])
            check_flex <- ifelse(dim(check_inclusion[POSITION %in% c("RB", "WR", "TE")])[1] == 0, 0, sum(check_inclusion[POSITION %in% c("RB", "WR", "TE"), 2]))
            
            
            if (check_qb > 1 | check_rb > 3 | check_wr > 4 | check_te > 2 | check_dst > 1 | check_flex > 7) {
                
                qb_error <- ifelse(check_qb > 1, "\"Select just one \", tags$b(\"QB\"), \" pick.\", tags$br(), ", "")
                rb_error <- ifelse(check_rb > 3, "\"Select fewer than 4 \", tags$b(\"RB\"), \" picks.\", tags$br(), ", "")
                wr_error <- ifelse(check_wr > 4, "\"Select fewer than 5 \", tags$b(\"WR\"), \" picks.\", tags$br(), ", "")
                te_error <- ifelse(check_te > 2, "\"Select fewer than 3 \", tags$b(\"TE\"), \" picks.\", tags$br(), ", "")
                dst_error <- ifelse(check_dst > 1, "\"Select just one \", tags$b(\"defense\"), \".\", tags$br(), ", "")
                flex_error <- ifelse(check_flex > 7, "\"Select fewer than 7 \", tags$b(\"flex\"), \" picks.\", tags$br(), ", "")
                
                error_full <- paste0(qb_error, rb_error, wr_error, te_error, dst_error, flex_error)
                
                text_full <- paste0("tags$span(", error_full, ")")
                
                shinyWidgets::show_alert(
                    #type = "error",
                    title = "Error: Invalid list of players to include",
                    text = eval(parse(text = text_full)),
                    html = TRUE
                    
                )
                
            }
            
            else {
                
                return(NULL)
                
            }
            
        }
        
        else {
            
            return(NULL)
            
        }
        
        #Salary check, 50k DK, 60K FD, 200 YH
            
            
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
