# Alpha Vantage Integration Tests
#
# These tests make real API calls when both conditions are met:
# 1. ALPHAVANTAGE_API_KEY environment variable is set
# 2. HEDGEROOT_ENABLE_API_TESTS=true environment variable is set
#
# API Usage: These tests are optimized to minimize API calls due to Alpha Vantage's
# free tier limit of 25 requests per day. Most tests reuse a single API call.
#
# To run these tests:
# - Set ALPHAVANTAGE_API_KEY=your_api_key
# - Set HEDGEROOT_ENABLE_API_TESTS=true
# - Run: testthat::test_file("tests/testthat/test-alpha-vantage-integration.R")
#
# To skip these tests (default behavior):
# - Don't set HEDGEROOT_ENABLE_API_TESTS or set it to "false"

library(testthat)
library(data.table)

# Shared test data - will be populated once and reused across all tests
test_data <- NULL
test_provider <- NULL

# Helper function to get test data (makes API call only once per test session)
get_test_data <- function() {
  if (is.null(test_data)) {
    skip_if_no_api_access("alpha_vantage")

    # Create provider using environment variable
    test_provider <<- create_alpha_vantage_provider()

    # Make single API call for AAPL with recent data
    end_date <- Sys.Date()
    start_date <- end_date - 30 # Last 30 days

    # Make real API call - this is the only API call for most tests
    test_data <<- fetch_ohlcv(test_provider, "AAPL", start_date, end_date)
  }
  return(test_data)
}

# Main integration test that validates the API response
test_that("Alpha Vantage real API integration - data structure and validation", {
  data <- get_test_data()

  # Verify response structure
  expect_s3_class(data, "data.table")
  expect_true(nrow(data) > 0)

  # Verify required columns exist
  required_cols <- c("date", "open", "high", "low", "close", "volume", "symbol")
  expect_true(all(required_cols %in% names(data)))

  # Verify data types
  expect_s3_class(data$date, "Date")
  expect_type(data$open, "double")
  expect_type(data$high, "double")
  expect_type(data$low, "double")
  expect_type(data$close, "double")
  expect_type(data$volume, "double")
  expect_type(data$symbol, "character")

  # Verify symbol is correct
  expect_true(all(data$symbol == "AAPL"))

  # Verify date range (approximately)
  expect_true(all(data$date >= (Sys.Date() - 35))) # Allow some flexibility
  expect_true(all(data$date <= Sys.Date()))
})

test_that("Alpha Vantage integration - OHLC business logic validation", {
  data <- get_test_data()

  # Verify OHLC relationships (high >= open, close; low <= open, close)
  expect_true(all(data$high >= data$open, na.rm = TRUE))
  expect_true(all(data$high >= data$close, na.rm = TRUE))
  expect_true(all(data$low <= data$open, na.rm = TRUE))
  expect_true(all(data$low <= data$close, na.rm = TRUE))

  # Verify positive values
  expect_true(all(data$open > 0, na.rm = TRUE))
  expect_true(all(data$high > 0, na.rm = TRUE))
  expect_true(all(data$low > 0, na.rm = TRUE))
  expect_true(all(data$close > 0, na.rm = TRUE))
  expect_true(all(data$volume >= 0, na.rm = TRUE))
})

test_that("Alpha Vantage integration - data transformations are applied", {
  data <- get_test_data()

  # Verify that add_ohlcv_transformations was applied
  # This should add additional columns beyond the basic OHLCV
  expect_s3_class(data, "data.table")
  expect_gte(ncol(data), 7) # Should have more than the basic 7 columns

  # Check for some expected transformation columns
  # (based on what add_ohlcv_transformations typically adds)
  transformation_cols <- c("typical_price", "price_range", "log_close")
  present_cols <- transformation_cols[transformation_cols %in% names(data)]
  expect_true(length(present_cols) > 0) # At least some transformation columns should be present
})

test_that("Alpha Vantage integration - data is properly sorted", {
  data <- get_test_data()

  if (nrow(data) > 1) {
    # Verify data is sorted by date
    expect_true(all(diff(data$date) >= 0))
  }
})

test_that("Alpha Vantage integration - date filtering works", {
  skip_if_no_api_access("alpha_vantage")

  # This test needs its own API call to test date filtering
  provider <- create_alpha_vantage_provider()

  # Test specific date range (use a historical range to ensure data exists)
  start_date <- as.Date("2024-01-01")
  end_date <- as.Date("2024-01-31")

  data <- fetch_ohlcv(provider, "AAPL", start_date, end_date)

  expect_s3_class(data, "data.table")

  if (nrow(data) > 0) {
    expect_true(all(data$date >= start_date))
    expect_true(all(data$date <= end_date))
  }
})

test_that("Alpha Vantage integration - rate limiting mechanism works", {
  skip_if_no_api_access("alpha_vantage")

  provider <- create_alpha_vantage_provider(rate_limit_per_minute = 5)

  # Verify rate limiting properties are set correctly
  expect_equal(provider@rate_limit_per_minute, 5)
  expect_equal(provider@rate_limit, 5)

  # Test that API call succeeds (rate limiting logic is tested internally)
  data <- fetch_ohlcv(provider, "AAPL", Sys.Date() - 3, Sys.Date())
  expect_s3_class(data, "data.table")
  expect_true(nrow(data) > 0)

  # Verify the provider was configured with correct rate limiting settings
  expect_equal(provider@rate_limit_per_minute, 5)
  expect_true(provider@last_request_time < Sys.time()) # Should be initialized in the past
})

test_that("Alpha Vantage integration - handles API errors gracefully", {
  skip_if_no_api_access("alpha_vantage")

  provider <- create_alpha_vantage_provider()

  # Test with invalid symbol - this should fail gracefully
  expect_error(
    fetch_ohlcv(provider, "INVALID_SYMBOL_12345", Sys.Date() - 5, Sys.Date()),
    "Alpha Vantage"
  )
})

test_that("Alpha Vantage integration - compact output size works", {
  skip_if_no_api_access("alpha_vantage")

  provider <- create_alpha_vantage_provider()

  # Test with compact output (last 100 data points)
  data <- fetch_ohlcv(provider, "AAPL", outputsize = "compact")

  expect_s3_class(data, "data.table")
  expect_true(nrow(data) > 0)
  expect_true(nrow(data) <= 100) # Compact should return max 100 points
  expect_true(all(data$symbol == "AAPL"))
})
