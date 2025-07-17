# Test Data Provider Classes
test_that("DataProvider class can be created", {
  provider <- DataProvider(
    name = "test",
    config = list(),
    enabled = TRUE,
    priority = 1,
    rate_limit = 100
  )

  expect_s7_class(provider, DataProvider)
  expect_equal(provider@name, "test")
  expect_true(provider@enabled)
  expect_equal(provider@priority, 1)
  expect_equal(provider@rate_limit, 100)
})

test_that("ProviderManager class can be created", {
  provider1 <- DataProvider(
    name = "provider1",
    config = list(),
    enabled = TRUE,
    priority = 1,
    rate_limit = 100
  )
  provider2 <- DataProvider(
    name = "provider2",
    config = list(),
    enabled = FALSE,
    priority = 2,
    rate_limit = 50
  )

  manager <- ProviderManager(
    providers = list(provider1, provider2),
    failover_strategy = "priority",
    circuit_breaker = NULL
  )

  expect_s7_class(manager, ProviderManager)
  expect_length(manager@providers, 2)
  expect_equal(manager@failover_strategy, "priority")
})

# Test Data Validation
test_that("Data validation works with valid data", {
  test_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    close = c(100, 101),
    volume = c(1000, 1100)
  )

  validation <- validate_data(test_data, "TEST")

  expect_s7_class(validation, DataValidation)
  expect_true(validation@is_valid)
  expect_gte(validation@quality_score, 0.8)
  expect_length(validation@errors, 0)
})

test_that("Data validation detects empty data", {
  empty_data <- data.frame()

  validation <- validate_data(empty_data, "TEST")

  expect_s7_class(validation, DataValidation)
  expect_false(validation@is_valid)
  expect_lt(validation@quality_score, 0.5)
  expect_true("No data returned" %in% validation@errors)
})

test_that("Data validation detects NA values", {
  test_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02")),
    close = c(100, NA),
    volume = c(1000, 1100)
  )

  validation <- validate_data(test_data, "TEST")

  expect_s7_class(validation, DataValidation)
  expect_true(validation@is_valid) # Still valid but with warnings
  expect_true("Data contains NA values" %in% validation@warnings)
  expect_lt(validation@quality_score, 1.0)
})

# Test Anomaly Detection
test_that("Anomaly detection works", {
  # Create data with extreme price movement
  test_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    close = c(100, 100.5, 150), # 50% jump on day 3
    volume = c(1000, 1100, 1200)
  )

  anomalies <- detect_anomalies(test_data)

  expect_type(anomalies, "list")
  expect_true("extreme_moves" %in% names(anomalies))
  expect_length(anomalies$extreme_moves, 1)
})

test_that("Anomaly detection handles normal data", {
  test_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    close = c(100, 100.5, 101), # Normal price movements
    volume = c(1000, 1100, 1200)
  )

  anomalies <- detect_anomalies(test_data)

  expect_type(anomalies, "list")
  expect_length(anomalies, 0) # No anomalies detected
})

# Test Caching
test_that("Data caching works", {
  test_data <- data.frame(
    date = as.Date("2023-01-01"),
    close = 100,
    volume = 1000
  )

  # Create temporary cache directory
  temp_dir <- tempdir()
  cache_config <- list(dir = temp_dir)

  result <- cache_data(test_data, "TEST", cache_config)

  expect_true(result)

  # Check if cache file exists
  cache_file <- file.path(temp_dir, paste0("TEST_", Sys.Date(), ".rds"))
  expect_true(file.exists(cache_file))

  # Clean up
  if (file.exists(cache_file)) {
    file.remove(cache_file)
  }
})
