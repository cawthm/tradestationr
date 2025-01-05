library(testthat)
library(tradestationr)

test_that("TradeStation client initialization works", {
  client <- TradeStation$new(
    client_id = "test_id",
    client_secret = "test_secret"
  )
  
  expect_true(inherits(client, "TradeStation"))
  expect_false(client$is_authenticated())
})

test_that("Market data handler initialization works", {
  client <- TradeStation$new(
    client_id = "test_id",
    client_secret = "test_secret"
  )
  
  market_data <- MarketData$new(client)
  expect_true(inherits(market_data, "MarketData"))
})

test_that("Storage handler initialization works", {
  # Use temporary file for testing
  tmp_db <- tempfile(fileext = ".sqlite")
  storage <- Storage$new(db_path = tmp_db)
  
  expect_true(inherits(storage, "Storage"))
  expect_true(file.exists(tmp_db))
  
  # Clean up
  storage$finalize()
  unlink(tmp_db)
})

test_that("Data table conversion works", {
  # Sample bar data
  bar_data <- list(
    Bars = data.frame(
      Timestamp = c("2023-01-01T10:00:00", "2023-01-01T10:01:00"),
      Open = c(100, 101),
      High = c(102, 103),
      Low = c(99, 100),
      Close = c(101, 102),
      Volume = c(1000, 1100)
    )
  )
  
  dt <- as.data.table(bar_data$Bars)
  expect_true(inherits(dt, "data.table"))
  expect_equal(nrow(dt), 2)
  expect_equal(ncol(dt), 6)
}) 