box::use(
  testthat[test_that, expect_equal]
)

box::use(
  app/logic/slateInformation[get_data, get_games],
)

test_that("Function get_data() returns expected features", {
  test_df <- c(
    "shortName",
    "playerId",
    "playerDkId",
    "position",
    "teamAbbreviation",
    "salary",
    "jdt_players_points",
    "name",
    "startTime"
  )

  app_df <- colnames(get_data())

  expect_equal(test_df, app_df)
})

test_that("Function get_games() returns vector of games", {
  app_df <- get_games()

  expect_type(app_df, "list")
})
