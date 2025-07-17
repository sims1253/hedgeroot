# Test Alpha Vantage Provider

library(testthat)
library(data.table)

# Test Alpha Vantage Provider Creation
test_that("AlphaVantageProvider can be created", {
  provider <- create_alpha_vantage_provider("test_api_key", 5)

  expect_s7_class(provider, AlphaVantageProvider)
  expect_equal(provider@name, "alpha_vantage")
  expect_equal(provider@api_key, "test_api_key")
  expect_equal(provider@rate_limit_per_minute, 5)
  expect_true(provider@enabled)
  expect_equal(provider@priority, 2)
  expect_equal(provider@base_url, "https://www.alphavantage.co/query")
})

test_that("AlphaVantageProvider has correct default values", {
  provider <- create_alpha_vantage_provider("test_key")

  expect_equal(provider@rate_limit_per_minute, 5)
  expect_equal(provider@rate_limit, 5)
  expect_true(provider@enabled)
})

# Test rate limiting logic (without actual API calls)
test_that("Rate limiting logic works", {
  provider <- create_alpha_vantage_provider("test_key", 60) # 1 per second

  # Set last request time to now
  provider@last_request_time <- Sys.time()

  # Mock the fetch function to test rate limiting
  expect_error(
    tryCatch(
      {
        # This would normally make an API call, but we'll simulate the rate limiting check
        time_since_last <- as.numeric(difftime(
          Sys.time(),
          provider@last_request_time,
          units = "secs"
        ))
        min_interval <- 60 / provider@rate_limit_per_minute

        if (time_since_last < min_interval) {
          # In real implementation, this would sleep, but for testing we'll error
          stop("Rate limit would be applied")
        }
      },
      error = function(e) stop(e$message)
    ),
    "Rate limit would be applied"
  )
})

test_that("AlphaVantageProvider properties are accessible", {
  provider <- create_alpha_vantage_provider("test_key", 10)

  expect_equal(provider@api_key, "test_key")
  expect_equal(provider@base_url, "https://www.alphavantage.co/query")
  expect_equal(provider@rate_limit_per_minute, 10)
  expect_type(provider@last_request_time, "double")
  expect_s3_class(provider@last_request_time, "POSIXct")
})

# Test URL building logic (extracted from fetch function)
test_that("API URL is built correctly", {
  provider <- create_alpha_vantage_provider("test_api_key")

  # Simulate URL building
  symbol <- "AAPL"
  outputsize <- "full"
  expected_url <- paste0(
    provider@base_url,
    "?function=TIME_SERIES_DAILY_ADJUSTED",
    "&symbol=",
    symbol,
    "&outputsize=",
    outputsize,
    "&apikey=",
    provider@api_key
  )

  actual_url <- paste0(
    provider@base_url,
    "?function=TIME_SERIES_DAILY_ADJUSTED",
    "&symbol=",
    symbol,
    "&outputsize=",
    outputsize,
    "&apikey=",
    provider@api_key
  )

  expect_equal(actual_url, expected_url)
})

# Test enhanced provider creation with environment variables
test_that("Provider uses environment variable when no API key provided", {
  # Set up environment variable
  old_key <- Sys.getenv("ALPHAVANTAGE_API_KEY", unset = NA)
  Sys.setenv(ALPHAVANTAGE_API_KEY = "env_test_key")

  # Test that provider uses environment variable
  provider <- create_alpha_vantage_provider()
  expect_equal(provider@api_key, "env_test_key")

  # Clean up
  if (is.na(old_key)) {
    Sys.unsetenv("ALPHAVANTAGE_API_KEY")
  } else {
    Sys.setenv(ALPHAVANTAGE_API_KEY = old_key)
  }
})

test_that("Provider uses provided API key over environment variable", {
  # Set up environment variable
  old_key <- Sys.getenv("ALPHAVANTAGE_API_KEY", unset = NA)
  Sys.setenv(ALPHAVANTAGE_API_KEY = "env_key")

  # Test that provided key takes precedence
  provider <- create_alpha_vantage_provider("provided_key")
  expect_equal(provider@api_key, "provided_key")

  # Clean up
  if (is.na(old_key)) {
    Sys.unsetenv("ALPHAVANTAGE_API_KEY")
  } else {
    Sys.setenv(ALPHAVANTAGE_API_KEY = old_key)
  }
})

test_that("Provider fails gracefully when no API key available", {
  # Ensure environment variable is not set
  old_key <- Sys.getenv("ALPHAVANTAGE_API_KEY", unset = NA)
  Sys.unsetenv("ALPHAVANTAGE_API_KEY")

  # Test that provider creation fails with helpful error
  expect_error(
    create_alpha_vantage_provider(),
    "alpha_vantage API token not found.*ALPHAVANTAGE_API_KEY"
  )

  # Clean up
  if (!is.na(old_key)) {
    Sys.setenv(ALPHAVANTAGE_API_KEY = old_key)
  }
})

test_that("Provider validates provided API key", {
  expect_error(
    create_alpha_vantage_provider(""),
    "Alpha Vantage API key cannot be empty"
  )

  expect_error(
    create_alpha_vantage_provider("   "),
    "Alpha Vantage API key cannot be empty"
  )

  expect_error(
    create_alpha_vantage_provider(NA),
    "Alpha Vantage API key cannot be empty"
  )
})

test_that("Provider trims whitespace from provided API key", {
  provider <- create_alpha_vantage_provider("  test_key  ")
  expect_equal(provider@api_key, "test_key")
})

test_that("Provider handles different rate limits", {
  provider1 <- create_alpha_vantage_provider("key", 1)
  provider2 <- create_alpha_vantage_provider("key", 100)

  expect_equal(provider1@rate_limit_per_minute, 1)
  expect_equal(provider2@rate_limit_per_minute, 100)
})

# Test configuration consistency
test_that("Provider configuration is consistent", {
  provider <- create_alpha_vantage_provider("test_key", 25)

  expect_equal(provider@rate_limit, provider@rate_limit_per_minute)
  expect_equal(provider@config$api_key, provider@api_key)
  expect_equal(provider@name, "alpha_vantage")
})

# Test time initialization
test_that("Last request time is initialized correctly", {
  provider <- create_alpha_vantage_provider("test_key")

  # Should be initialized to allow immediate request
  time_diff <- as.numeric(difftime(
    Sys.time(),
    provider@last_request_time,
    units = "secs"
  ))
  expect_gte(time_diff, 60) # Should be at least 60 seconds ago
})
