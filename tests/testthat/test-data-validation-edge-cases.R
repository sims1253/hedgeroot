# Test Data Validation Edge Cases

library(testthat)
library(data.table)

# Test edge cases for data validation and anomaly detection
test_that("Data validation handles various data formats", {
  # Test with data.table
  dt_data <- data.table(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    close = c(100, 101),
    volume = c(1000, 1100)
  )
  
  validation_dt <- validate_data(dt_data, "DT_TEST")
  expect_s7_class(validation_dt, DataValidation)
  expect_true(validation_dt@is_valid)
  
  # Test with data.frame
  df_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    close = c(100, 101),
    volume = c(1000, 1100)
  )
  
  validation_df <- validate_data(df_data, "DF_TEST")
  expect_s7_class(validation_df, DataValidation)
  expect_true(validation_df@is_valid)
})

test_that("Data validation handles extreme values", {
  # Test with very large numbers
  large_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    close = c(1e10, 1.1e10),
    volume = c(1e12, 1.1e12)
  )
  
  validation <- validate_data(large_data, "LARGE_TEST")
  expect_s7_class(validation, DataValidation)
  expect_true(validation@is_valid)
  
  # Test with very small numbers
  small_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    close = c(1e-6, 1.1e-6),
    volume = c(1, 2)
  )
  
  validation_small <- validate_data(small_data, "SMALL_TEST")
  expect_s7_class(validation_small, DataValidation)
  expect_true(validation_small@is_valid)
})

test_that("Data validation handles mixed data types", {
  # Test with mixed numeric/character data
  mixed_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    close = c("100", "101"),  # Character numbers
    volume = c(1000, 1100),
    symbol = c("AAPL", "AAPL")
  )
  
  validation <- validate_data(mixed_data, "MIXED_TEST")
  expect_s7_class(validation, DataValidation)
  # Should still be valid but may have warnings
})

test_that("Anomaly detection handles edge cases", {
  # Test with constant prices (no volatility)
  constant_data <- data.frame(
    date = as.Date(seq(as.Date("2023-01-01"), by = "day", length.out = 10)),
    close = rep(100, 10),
    volume = rep(1000, 10)
  )
  
  anomalies <- detect_anomalies(constant_data)
  expect_type(anomalies, "list")
  expect_length(anomalies, 0)  # No anomalies in constant data
  
  # Test with single data point
  single_data <- data.frame(
    date = as.Date("2023-01-01"),
    close = 100,
    volume = 1000
  )
  
  anomalies_single <- detect_anomalies(single_data)
  expect_type(anomalies_single, "list")
  expect_length(anomalies_single, 0)  # Can't detect anomalies with single point
})

test_that("Anomaly detection with extreme price movements", {
  # Test with known extreme movements
  extreme_data <- data.frame(
    date = as.Date(seq(as.Date("2023-01-01"), by = "day", length.out = 5)),
    close = c(100, 100, 200, 100, 100),  # 100% jump and drop
    volume = rep(1000, 5)
  )
  
  anomalies <- detect_anomalies(extreme_data)
  expect_type(anomalies, "list")
  expect_true("extreme_moves" %in% names(anomalies))
  expect_length(anomalies$extreme_moves, 2)  # Should detect 2 extreme moves
})

test_that("Anomaly detection with gradual trends", {
  # Test with gradual uptrend (should not trigger anomalies)
  trend_data <- data.frame(
    date = as.Date(seq(as.Date("2023-01-01"), by = "day", length.out = 20)),
    close = seq(100, 120, length.out = 20),  # Gradual 20% increase
    volume = rep(1000, 20)
  )
  
  anomalies <- detect_anomalies(trend_data)
  expect_type(anomalies, "list")
  # Should not detect anomalies in gradual trend
  expect_length(anomalies, 0)
})

test_that("Cache operations handle various scenarios", {
  # Test caching with different data types
  test_data <- list(
    numeric_data = c(1, 2, 3, 4, 5),
    character_data = c("A", "B", "C"),
    date_data = as.Date(c("2023-01-01", "2023-01-02"))
  )
  
  temp_dir <- tempdir()
  cache_config <- list(dir = temp_dir)
  
  result <- cache_data(test_data, "COMPLEX_TEST", cache_config)
  expect_true(result)
  
  # Verify cache file exists
  cache_file <- file.path(temp_dir, paste0("COMPLEX_TEST_", Sys.Date(), ".rds"))
  expect_true(file.exists(cache_file))
  
  # Verify data can be read back
  cached_data <- readRDS(cache_file)
  expect_equal(cached_data, test_data)
  
  # Clean up
  if (file.exists(cache_file)) {
    file.remove(cache_file)
  }
})

test_that("Cache operations handle errors gracefully", {
  # Test with invalid cache directory (use a path that can't be created)
  invalid_config <- list(dir = paste0(tempdir(), "/", paste(rep("x", 300), collapse = "")))
  
  expect_warning(
    result <- cache_data(list(a = 1), "INVALID_TEST", invalid_config),
    "Failed to cache data"
  )
  expect_false(result)
})

test_that("Data validation quality scoring works correctly", {
  # Test with perfect data
  perfect_data <- data.frame(
    date = as.Date(seq(as.Date("2023-01-01"), by = "day", length.out = 10)),
    close = seq(100, 110, length.out = 10),
    volume = seq(1000, 1100, length.out = 10)
  )
  
  validation <- validate_data(perfect_data, "PERFECT_TEST")
  expect_equal(validation@quality_score, 1.0)
  
  # Test with data containing warnings
  warning_data <- data.frame(
    date = as.Date(seq(as.Date("2023-01-01"), by = "day", length.out = 10)),
    close = c(100, 101, NA, 103, 104, 105, NA, 107, 108, 109),
    volume = seq(1000, 1100, length.out = 10)
  )
  
  validation_warning <- validate_data(warning_data, "WARNING_TEST")
  expect_lt(validation_warning@quality_score, 1.0)
  expect_gte(validation_warning@quality_score, 0.8)  # Should still be high
})

test_that("Null coalescing operator works correctly", {
  # Test the %||% operator used throughout the codebase
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% "default", 0)
  expect_equal(FALSE %||% "default", FALSE)
  expect_equal(c() %||% "default", "default")  # Empty vector should use default
  expect_equal(NA %||% "default", NA)    # NA is not NULL
})

test_that("Data validation handles timezone issues", {
  # Test with different timezone data
  utc_data <- data.frame(
    date = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-02 12:00:00"), tz = "UTC"),
    close = c(100, 101),
    volume = c(1000, 1100)
  )
  
  validation_utc <- validate_data(utc_data, "UTC_TEST")
  expect_s7_class(validation_utc, DataValidation)
  expect_true(validation_utc@is_valid)
  
  # Test with local timezone
  local_data <- data.frame(
    date = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-02 12:00:00")),
    close = c(100, 101),
    volume = c(1000, 1100)
  )
  
  validation_local <- validate_data(local_data, "LOCAL_TEST")
  expect_s7_class(validation_local, DataValidation)
  expect_true(validation_local@is_valid)
})

test_that("Data validation handles duplicate dates", {
  # Test with duplicate date entries
  duplicate_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02")),
    close = c(100, 100.5, 101),
    volume = c(1000, 1050, 1100)
  )
  
  validation <- validate_data(duplicate_data, "DUPLICATE_TEST")
  expect_s7_class(validation, DataValidation)
  # Should still be valid but might have warnings about duplicates
})

test_that("Data validation handles unsorted dates", {
  # Test with unsorted date data
  unsorted_data <- data.frame(
    date = as.Date(c("2023-01-03", "2023-01-01", "2023-01-02")),
    close = c(102, 100, 101),
    volume = c(1200, 1000, 1100)
  )
  
  validation <- validate_data(unsorted_data, "UNSORTED_TEST")
  expect_s7_class(validation, DataValidation)
  expect_true(validation@is_valid)
})

test_that("Anomaly detection threshold sensitivity", {
  # Test with data right at the threshold
  threshold_data <- data.frame(
    date = as.Date(seq(as.Date("2023-01-01"), by = "day", length.out = 10)),
    close = c(100, 100 * exp(0.29), rep(100, 8)),  # Just under 30% threshold
    volume = rep(1000, 10)
  )
  
  anomalies_under <- detect_anomalies(threshold_data)
  expect_type(anomalies_under, "list")
  expect_length(anomalies_under, 0)  # Should not trigger
  
  # Test with data just over the threshold
  over_threshold_data <- data.frame(
    date = as.Date(seq(as.Date("2023-01-01"), by = "day", length.out = 10)),
    close = c(100, 100 * exp(0.31), rep(100, 8)),  # Just over 30% threshold
    volume = rep(1000, 10)
  )
  
  anomalies_over <- detect_anomalies(over_threshold_data)
  expect_type(anomalies_over, "list")
  expect_true("extreme_moves" %in% names(anomalies_over))
})