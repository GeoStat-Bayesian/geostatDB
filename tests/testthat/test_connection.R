
context("test the connection to WWHYPDA")
test_that("number of dimensions in WWHYPDA is 20523 and 39", {
  df <- geostatDB::getData()
  my_dim <- dim(df)
  expect_equal(my_dim[1], 20523)
  expect_equal(my_dim[2], 39)
})
