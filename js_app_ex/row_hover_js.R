dat <- iris[1:5,1:5]
colours2apply <- sample(x=c(rgb(1, 0, 0 ), rgb(1, 1, 0 ), rgb(0, 1, 1 )), 25, replace = T) %>% 
  matrix(nrow=5) %>% 
  data.frame()
set.seed(1234)
SampleSizesToShowInHover <- matrix(round(runif(n = 25, 10, 1000)), nrow=5)

dat <- cbind(dat, colours2apply)
dat <- cbind(dat, SampleSizesToShowInHover)
dat

full_salaries <- data.table::fread("https://raw.githubusercontent.com/tomasokal/dfs/production/Output/salaries_projections_main_slate.csv")

sal_yh <- full_salaries[!is.na(full_salaries$SALARY_YH)]

test <- tibble::tibble(
  
  
  Player = sal_yh$PLAYER,
  `Pos.` = sal_yh$POSITION,
  Points = sal_yh$POINTS_YH,
  Salary = sal_yh$SALARY_YH,
  Diff = sal_yh$DIFF_YH
)

DT <- datatable(test, 
                options = list(columnDefs = list(list(visible=FALSE, targets = 5)),
                                                 initComplete = JS(
                                                     "function(settings, json) {",
                                                     "$(this.api().table().container()).css({'font-size': '80%'});",
                                                     "}"),
                                                 #dom = 't', displays table only
                                                 rowCallback = JS("function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                                                                  "var full_text = 'This player needs to earn ' + aData[5] + ' to be considered in an optimal lineup' ",
                                                                  "$('td:eq(3)', nRow).attr('title', full_text);",
                                                                  "}")
                  
                  )) %>%
          formatStyle(3, backgroundColor = "lightblue",
                      `padding-top` = '5px',
                      `padding-bottom` = '5px',
                      `padding-left` = '10px',
                      `padding-right` = '10px',
                      target = 'cell',
                      border = '5px solid transparent',
                      `background-clip` = 'content-box')

# datatable(head(iris), 
#           options=list(initComplete = JS(
#             "function(settings) {
#             var table = settings.oInstance.api();
#             var cell = table.cell(2,2);
#             cell.node().setAttribute('title', cell.data());
#             }")))
# 
# datatable(head(iris), 
#           options=list(initComplete = JS("function(settings) {var table=settings.oInstance.api(); table.$('td').each(function(){this.setAttribute('title', $(this).html())})}")))
