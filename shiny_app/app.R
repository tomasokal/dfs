library(shiny)
library(shinyWidgets)
library(shinyjs)
library(data.table)
library(DT)
library(lpSolve)
library(dplyr)

full_salaries <- data.table::fread("https://raw.githubusercontent.com/tomasokal/dfs/production/Output/salaries_projections_main_slate.csv")



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
                                          p(HTML("<br>Optimizes for Sunday slate. More options coming soon.<br>")),
                                          verbatimTextOutput('hoverIndex')
                                          ),
                                   column(1)),
                            column(7,
                                   
                                   column(10,
                                          wellPanel(class = "optiwell",
                                                    p("Your optimized lineup:"),
                                                    #DT::DTOutput(outputId = "optimized"),
                                                    htmlOutput(outputId = "optimized"),
                                                    style = "height:30vh;")
                                          ),
                                   )
                                   
                        ),
                        fluidRow(
                            column(5,
                                   column(1),
                                   column(11,
                                          wellPanel(class = "wellclass",
                                                    style = "height:55vh;",
                                          div(
                                              br(), 
                                              DT::DTOutput(outputId = "player_list_table"))
                                          ))#,
                                   #column(1)
                                   
                                   
                                   ),
                            column(3,
                                   wellPanel(class = "wellclass",
                                             style = "height:55vh",
                                             div(p("Players to include in the optimized lineup:"),
                                                 br(), 
                                                 DT::DTOutput(outputId = "player_list_include"))
                                             #textOutput(outputId = "myText")
                                             )
                                   ),
                            column(3,
                                   wellPanel(class = "wellclass",
                                             style = "height:55vh",
                                             div(p("Players to exclude from the optimized lineup:"),
                                                 br(), 
                                                 DT::DTOutput(outputId = "player_list_exclude")
                                                 # textOutput(outputId = "myText2") 
                                                 )
                                             
                                             )
                                   )
                        ),
                        fluidRow(
                                 column(11,
                                        p(HTML("Built by <a href=\"https://twitter.com/tomasokal\">Tomas Okal</a> and <a href=\"https://twitter.com/_willdebras\">Will Bonnell</a>, 
                                               inspired by work by <a href=\"https://twitter.com/Troy4MWRD\">Troy Hernandez</a>, 
                                               powered by R Shiny, {data.table}, and Github Actions, with ♥."),
                                          style = "padding-left:100px")),
                                 column(1))
                   
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
        
        sal_dk <- full_salaries[!is.na(full_salaries$SALARY_DK)]
        
        
        
        tibble::tibble(
            
            
            Player = sal_dk$PLAYER,
            `Pos.` = sal_dk$POSITION,
            Points = sal_dk$POINTS_DK,
            Salary = sal_dk$SALARY_DK,
            Diff = sal_dk$DIFF_DK,
            Include = shinyInput(actionButton, nrow(sal_dk),
                                 'button_',
                                 label = "",
                                 icon = icon("check"),
                                 class = "include",
                                 onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'
            ),
            Exclude = shinyInput(actionButton, nrow(sal_dk),
                                 'button_',
                                 label = "",
                                 icon = icon("times"),
                                 class = "exclude",
                                 onclick = 'Shiny.setInputValue(\"select_button2\", this.id, {priority: \"event\"})'
            )
        )
    })
    
    player_list_fd <- reactive({
        
        sal_fd <- full_salaries[!is.na(full_salaries$SALARY_FD)]
        
        tibble::tibble(
            
            
            Player = sal_fd$PLAYER,
            `Pos.` = sal_fd$POSITION,
            Points = sal_fd$POINTS_FD,
            Salary = sal_fd$SALARY_FD,
            Diff = sal_fd$DIFF_FD,
            Include = shinyInput(actionButton, nrow(sal_fd),
                                 'button_',
                                 label = "",
                                 icon = icon("check"),
                                 class = "include",
                                 onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'
            ),
            Exclude = shinyInput(actionButton, nrow(sal_fd),
                                 'button_',
                                 label = "",
                                 icon = icon("times"),
                                 class = "exclude",
                                 onclick = 'Shiny.setInputValue(\"select_button2\", this.id, {priority: \"event\"})'
            )
        )
    })
    
    
    player_list_yh <- reactive({
        
        sal_yh <- full_salaries[!is.na(full_salaries$SALARY_YH)]
        
        tibble::tibble(
            
            
            Player = sal_yh$PLAYER,
            `Pos.` = sal_yh$POSITION,
            Points = sal_yh$POINTS_YH,
            Salary = sal_yh$SALARY_YH,
            Diff = sal_yh$DIFF_YH,
            Include = shinyInput(actionButton, nrow(sal_yh),
                                 'button_',
                                 label = "",
                                 icon = icon("check"),
                                 class = "include",
                                 onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'
            ),
            Exclude = shinyInput(actionButton, nrow(sal_yh),
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
        datatable(proper_tb(),
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
                      columnDefs = list(list(className = 'dt-center', targets = 2:7),
                                        list(width = '10%', targets = 3),
                                        list(width = '18%', targets = 4),
                                        list(visible = FALSE, targets = 5)),
                      # rowCallback = JS('(function(row, data) {var value=data[1]; $(row).css({"background-color":"#FFFF99"});})'),
                      rowCallback = JS("function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                                       "var full_text = 'This player would need to earn an additional ' + aData[5] + ' points to be optimal.' ",
                                       "$('td:eq(3)', nRow).attr('title', full_text);",
                                       "$('td:eq(3)', nRow).css({    'background-color':'lightblue',
                                                                        'target':'cell',
                                                                        'border':'4px solid transparent',
                                                                        'background-clip':'content-box',
                                                                });",
                                       "}")

                  )) #%>%
            # formatStyle(3, backgroundColor = "lightblue",
            #             `padding-top` = '5px',
            #             `padding-bottom` = '5px',
            #             `padding-left` = '10px',
            #             `padding-right` = '10px',
            #             target = 'cell',
            #             border = '5px solid transparent',
            #             `background-clip` = 'content-box')
    })
    
    
    # 'background-color':'#FFFF99';
    # 'target':'cell';
    # 'border':'5px solid transparent';
    # 'background-clip':'content-box';
    
    
    # output$player_list_table <- DT::renderDT({
    #     #player_list()
    #     datatable(full_salaries, 
    #               options = list(columnDefs = list(list(visible=FALSE, targets = 6:10)), 
    #                              rowCallback = JS("function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
    #                                               "var full_text = 'This player needs to earn ' + aData[5] + ' to be considered in an optimal lineup' ",
    #                                               "$('td:eq(3)', nRow).attr('title', full_text);",
    #                                               "}")
    #                              
    #               ))
    #     
    # })
    
    
    
    
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
        
        optimized_lineup(NULL)
        
    })
    
    
    output$myText <- renderText({
        return(player_include())
    })
    
    output$myText2 <- renderText({
        #return(player_exclude())
        return(input$select_button3)
    })
    
    
    optimized_lineup <- reactiveVal(NULL)
    
    
    observeEvent(input$runbutton, {
        
        optimized_lineup(NULL)
        
        #--------------TESTING NOT DOING THIS FUNCTIONALIZED----------------##
        
        inc_players <- df_include()
        exc_players <- df_exclude()
        
        salary_full <- switch(input$sourcegroup, 
                              "DraftKings" = 50000, 
                              "FanDuel" = 60000, 
                              "Yahoo Sports" = 200
        )
        
        salary_switch <- switch(input$sourcegroup, "DraftKings" = "SALARY_DK", "FanDuel" = "SALARY_FD", "Yahoo Sports" = "SALARY_YH")
        points_switch <- switch(input$sourcegroup, "DraftKings" = "POINTS_DK", "FanDuel" = "POINTS_FD", "Yahoo Sports" = "POINTS_YH")
        
        df_full <- full_salaries %>%
            dplyr::select(PLAYER, POSITION, TEAM, salary_switch, points_switch) %>%
            `colnames<-`(c("Player", "Position", "Team", "Salary", "Points")) %>%
            dplyr::filter(!is.na(Salary))
        
        
        #------------------------------------------------------------------##
        
        inc_test <- subset(full_salaries, PLAYER %in% pl_inc())[!is.na(SALARY_DK)] #param for source
        
        salary_full <- switch(input$sourcegroup, 
                              "DraftKings" = 50000, 
                              "FanDuel" = 60000, 
                              "Yahoo Sports" = 200
        )
        
        if (dim(inc_test)[1] > 0) {
            
            check_inclusion <- inc_test[, .N, by = POSITION]
            check_qb <- ifelse(length(check_inclusion[POSITION == "QB"][[2]]) == 0, 0, check_inclusion[POSITION == "QB"][[2]])
            check_rb <- ifelse(length(check_inclusion[POSITION == "RB"][[2]]) == 0, 0, check_inclusion[POSITION == "RB"][[2]])
            check_wr <- ifelse(length(check_inclusion[POSITION == "WR"][[2]]) == 0, 0, check_inclusion[POSITION == "WR"][[2]])
            check_te <- ifelse(length(check_inclusion[POSITION == "TE"][[2]]) == 0, 0, check_inclusion[POSITION == "TE"][[2]])
            check_dst <- ifelse(length(check_inclusion[POSITION == "DST"][[2]]) == 0, 0, check_inclusion[POSITION == "DST"][[2]])
            check_flex <- ifelse(dim(check_inclusion[POSITION %in% c("RB", "WR", "TE")])[1] == 0, 0, sum(check_inclusion[POSITION %in% c("RB", "WR", "TE"), 2]))
            check_salary <- ifelse(sum(df_include()$Salary) > salary_full, TRUE, FALSE)
            
            
            if (check_qb > 1 | check_rb > 3 | check_wr > 4 | check_te > 2 | check_dst > 1 | check_flex > 7 | sum(df_include()$Salary) > salary_full) {
                
                qb_error <- ifelse(check_qb > 1, "\"Select just one \", tags$b(\"QB\"), \" pick.\", tags$br(), ", "")
                rb_error <- ifelse(check_rb > 3, "\"Select fewer than 4 \", tags$b(\"RB\"), \" picks.\", tags$br(), ", "")
                wr_error <- ifelse(check_wr > 4, "\"Select fewer than 5 \", tags$b(\"WR\"), \" picks.\", tags$br(), ", "")
                te_error <- ifelse(check_te > 2, "\"Select fewer than 3 \", tags$b(\"TE\"), \" picks.\", tags$br(), ", "")
                dst_error <- ifelse(check_dst > 1, "\"Select just one \", tags$b(\"defense\"), \".\", tags$br(), ", "")
                flex_error <- ifelse(check_flex > 7, "\"Select fewer than 7 \", tags$b(\"flex\"), \" picks.\", tags$br(), ", "")
                salary_error <- ifelse(check_salary==TRUE, paste0("\"Select a total values of players under \", ", 
                                                                  "tags$b(\"$", prettyNum(salary_full), ".\"),", 
                                                                  " tags$br(), "), "")
                
                error_full <- paste0(qb_error, rb_error, wr_error, te_error, dst_error, flex_error, salary_error)
                
                text_full <- paste0("tags$span(", error_full, ")")
                
                shinyWidgets::show_alert(
                    #type = "error",
                    title = "Error: Invalid list of players to include",
                    text = eval(parse(text = text_full)),
                    html = TRUE
                    
                )
                
                return(NULL)
                
            }
            else {
                
                salary_inclusion <- sum(df_full[Player %in% inc_players$Player][["Salary"]])
                
                
                # salary_inclusion <- sum(df_include$SALARY_DK)
                
                player_pool <- df_full[!Player %in% inc_players$Player][!Player %in% exc_players$Player]
                
                position_dt <- player_pool[, j = .(ppQB = ifelse(Position == "QB", 1, 0),
                                                   ppRB = ifelse(Position == "RB", 1, 0),
                                                   ppWR = ifelse(Position == "WR", 1, 0),
                                                   ppTE = ifelse(Position == "TE", 1, 0),
                                                   ppDST = ifelse(Position == "DST", 1, 0),
                                                   ppFlex = ifelse(Position %in% c("RB", "WR", "TE"), 1, 0))]
                
                obj_points <- player_pool[, Points]
                
                con_players <- t(cbind(SALARY = player_pool[, Salary], position_dt))
                colnames(con_players) <- player_pool$Points
                
                f.dir <- rep(0, nrow(con_players))
                f.rhs <- rep(0, nrow(con_players))
                
                f.dir[1] <- "<="
                f.rhs[1] <- salary_full - salary_inclusion
                
                f.dir[2:nrow(con_players)] <- c("=", ">=", ">=", ">=", "=", "=")
                f.rhs[2:nrow(con_players)] <- c(1, 2, 3, 1, 1, 7)
                
                f.rhs[7] <- 7 - check_flex
                
                if (check_qb == 1) {
                    
                    f.dir[2] <- "="
                    f.rhs[2] <- 0
                    
                }
                
                else {
                    
                    f.rhs <- f.rhs
                    
                }
                
                if (check_rb == 1) {
                    
                    f.dir[3] <- ">="
                    f.rhs[3] <- 1
                    
                }
                
                if (check_rb == 2) {
                    
                    f.dir[3] <- ">="
                    f.rhs[3] <- 0
                    
                }
                
                if (check_rb == 3) {
                    
                    f.dir[3] <- "="
                    f.rhs[3] <- 0
                    
                }
                
                else {
                    
                    f.dir <- f.dir
                    f.rhs <- f.rhs
                    
                }
                
                if (check_wr == 1) {
                    
                    f.dir[4] <- ">="
                    f.rhs[4] <- 2
                    
                }
                
                if (check_wr == 2) {
                    
                    f.dir[4] <- ">="
                    f.rhs[4] <- 1
                    
                }
                
                if (check_wr == 3) {
                    
                    f.dir[4] <- ">="
                    f.rhs[4] <- 0
                    
                }
                
                if (check_wr == 4) {
                    
                    f.dir[4] <- "="
                    f.rhs[4] <- 0
                    
                }
                
                else {
                    
                    f.dir <- f.dir
                    f.rhs <- f.rhs
                    
                }
                
                if (check_te == 1) {
                    
                    f.dir[5] <- ">="
                    f.rhs[5] <- 0
                    
                }
                
                if (check_te == 2) {
                    
                    f.dir[5] <- "="
                    f.rhs[5] <- 0
                    
                }
                
                else {
                    
                    f.dir <- f.dir
                    f.rhs <- f.rhs
                    
                }
                
                if (check_dst == 1) {
                    
                    f.dir[6] <- "="
                    f.rhs[6] <- 0
                    
                }
                
                else {
                    
                    f.dir <- f.dir
                    f.rhs <- f.rhs
                    
                }
                
                opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
                picks <- player_pool[which(opt$solution == 1), ]
                
                picks_list <- paste0("<div><b>", picks$Player, "</b> - $", picks$Salary, "/", picks$Points, "<i>pts.</i>", " </div>", collapse = " ")
                
                html_picks <- paste0("<div class = \"testcontainer\">", picks_list, "</div>")
                
                optimized_lineup(html_picks)
                
                # optimized_lineup(picks)
                
            }
                
        }
            
        
        else {
            

            player_pool <- df_full[!Player %in% exc_players$Player]
            position_dt <- player_pool[, j = .(ppQB = ifelse(Position == "QB", 1, 0),
                                               ppRB = ifelse(Position == "RB", 1, 0),
                                               ppWR = ifelse(Position == "WR", 1, 0),
                                               ppTE = ifelse(Position == "TE", 1, 0),
                                               ppDST = ifelse(Position == "DST", 1, 0),
                                               ppFlex = ifelse(Position %in% c("RB", "WR", "TE"), 1, 0))]
            
            obj_points <- player_pool[, Points]
            con_players <- t(cbind(SALARY = player_pool[, Salary], position_dt))
            colnames(con_players) <- player_pool$Points
            
            f.dir <- rep(0, nrow(con_players))
            f.rhs <- rep(0, nrow(con_players))
            
            f.dir[1] <- "<="
            f.rhs[1] <- 50000
            
            f.dir[2:nrow(con_players)] <- c("=", ">=", ">=", ">=", "=", "=")
            f.rhs[2:nrow(con_players)] <- c(1, 2, 3, 1, 1, 7)
            
            opt <- lp("max", obj_points, con_players, f.dir, f.rhs, all.bin = TRUE)
            picks <- player_pool[which(opt$solution == 1), ]
            
            picks_list <- paste0("<div><b>", picks$Player, "</b> - $", picks$Salary, "/", picks$Points, "<i>pts.</i>", " </div>", collapse = " ")
            
            html_picks <- paste0("<div class = \"testcontainer\">", picks_list, "</div>")
            
            optimized_lineup(html_picks)
            
            # optimized_lineup(picks)
            

            
        }
        
        #Salary check, 50k DK, 60K FD, 200 YH
            
            
        
    })
    
    # output$optimized <- DT::renderDT({
    #     optimized_lineup()
    # },
    # escape = FALSE,
    # selection = "none",
    # options = list(
    #     initComplete = JS(
    #         "function(settings, json) {",
    #         "$(this.api().table().container()).css({'font-size': '50%'});",
    #         "}"),
    #     columnDefs = list(list(className = 'dt-center', targets = 2:4),
    #                       list(width = '18%', targets = 4)),
    #     dom = 't'
    #     
    # ))
    
    output$optimized <- renderText({optimized_lineup()})
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
