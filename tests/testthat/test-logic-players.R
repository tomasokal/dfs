box::use(
  shiny[testServer],
  testthat[...],
)
box::use(
  app/logic/players[...],
)

test_that("Function get_data() returns expected features", {

  test_df <- data.frame(
    PLAYER = 1,
    TEAM = 1, 
    POSITION = 1,
    POINTS_DK = 1,
    SALARY_DK = 1
  )

  app_df <- get_data()

  expect_equal(colnames(test_df), colnames(app_df))
  expect_equal(ncols(test_df), ncols(app_df))

})
