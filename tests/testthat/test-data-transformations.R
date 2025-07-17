# Test Data.table Transformations and Performance Functions

library(testthat)
library(data.table)

# Test Column Standardization
test_that("OHLCV column standardization works", {
  # Create test data with Yahoo Finance style column names
  test_dt <- data.table(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    AAPL.Open = c(100, 101),
    AAPL.High = c(102, 103),
    AAPL.Low = c(99, 100),
    AAPL.Close = c(101, 102),
    AAPL.Volume = c(1000, 1100),
    AAPL.Adjusted = c(101, 102),
    symbol = "AAPL"
  )

  result <- standardize_ohlcv_columns(test_dt)

  expect_true("open" %in% names(result))
  expect_true("high" %in% names(result))
  expect_true("low" %in% names(result))
  expect_true("close" %in% names(result))
  expect_true("volume" %in% names(result))
  expect_true("adjusted" %in% names(result))

  expect_equal(result$open, c(100, 101))
  expect_equal(result$close, c(101, 102))
})

test_that("OHLCV transformations are added correctly", {
  test_dt <- data.table(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    open = c(100, 101, 102),
    high = c(102, 103, 104),
    low = c(99, 100, 101),
    close = c(101, 102, 103),
    volume = c(1000, 1100, 1200),
    symbol = "TEST"
  )

  result <- add_ohlcv_transformations(test_dt)

  # Check that new columns are added
  expect_true("typical_price" %in% names(result))
  expect_true("price_range" %in% names(result))
  expect_true("price_change" %in% names(result))
  expect_true("price_change_pct" %in% names(result))
  expect_true("log_close" %in% names(result))
  expect_true("log_return" %in% names(result))
  expect_true("true_range" %in% names(result))

  # Check calculations
  expect_equal(result$typical_price[1], (102 + 99 + 101) / 3)
  expect_equal(result$price_range[1], 102 - 99)
  expect_equal(result$log_close[1], log(101))

  # Check that data is sorted and keyed
  expect_true(is.data.table(result))
  expect_equal(key(result), c("date", "symbol"))
})

test_that("Fast OHLCV processing works", {
  test_dt <- data.table(
    date = as.Date(seq(as.Date("2023-01-01"), by = "day", length.out = 50)),
    open = seq(100, 149, 1),
    high = seq(102, 151, 1),
    low = seq(99, 148, 1),
    close = seq(101, 150, 1),
    volume = seq(1000, 1049, 1),
    symbol = "TEST"
  )

  # Add basic transformations first
  test_dt <- add_ohlcv_transformations(test_dt)

  result <- process_ohlcv_fast(
    test_dt,
    operations = c("returns", "ma", "volatility")
  )

  # Check return calculations
  expect_true("return_1d" %in% names(result))
  expect_true("return_5d" %in% names(result))
  expect_true("return_20d" %in% names(result))

  # Check moving averages
  expect_true("sma_5" %in% names(result))
  expect_true("sma_10" %in% names(result))
  expect_true("sma_20" %in% names(result))
  expect_true("sma_50" %in% names(result))

  # Check volatility measures
  expect_true("volatility_5d" %in% names(result))
  expect_true("volatility_20d" %in% names(result))
  expect_true("atr_14" %in% names(result))

  # Verify calculations are reasonable
  expect_true(all(result$return_1d[2:nrow(result)] > 0, na.rm = TRUE)) # Increasing prices
  expect_true(all(result$sma_5 > result$sma_20, na.rm = TRUE)) # Shorter MA > longer MA for uptrending data
})

test_that("Data aggregation works correctly", {
  # Create daily data for 2 weeks
  test_dt <- data.table(
    date = as.Date(seq(as.Date("2023-01-01"), by = "day", length.out = 14)),
    open = seq(100, 113, 1),
    high = seq(102, 115, 1),
    low = seq(99, 112, 1),
    close = seq(101, 114, 1),
    volume = seq(1000, 1013, 1),
    symbol = "TEST"
  )

  # Test weekly aggregation
  weekly_result <- aggregate_ohlcv(test_dt, period = "weekly")

  expect_true(nrow(weekly_result) < nrow(test_dt))
  expect_true(all(
    c("open", "high", "low", "close", "volume") %in% names(weekly_result)
  ))

  # Check that high is max of period highs (first week has days 1-6)
  expect_gte(weekly_result$high[1], max(test_dt$high[1:6]))

  # Check that low is min of period lows (first week has days 1-6)
  expect_lte(weekly_result$low[1], min(test_dt$low[1:6]))

  # Test monthly aggregation
  monthly_result <- aggregate_ohlcv(test_dt, period = "monthly")
  expect_true(nrow(monthly_result) <= nrow(weekly_result))
})

test_that("Data transformations handle edge cases", {
  # Test with minimal data
  minimal_dt <- data.table(
    date = as.Date("2023-01-01"),
    open = 100,
    high = 102,
    low = 99,
    close = 101,
    volume = 1000,
    symbol = "TEST"
  )

  result <- add_ohlcv_transformations(minimal_dt)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$price_change[1])) # First row should have NA for change
  expect_false(is.na(result$typical_price[1])) # But typical price should be calculated

  # Test with missing values
  na_dt <- data.table(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    open = c(100, NA),
    high = c(102, 103),
    low = c(99, 100),
    close = c(101, 102),
    volume = c(1000, 1100),
    symbol = "TEST"
  )

  result <- add_ohlcv_transformations(na_dt)

  expect_equal(nrow(result), 2)
  expect_false(is.na(result$typical_price[2])) # typical_price doesn't use open, so should not be NA
})
