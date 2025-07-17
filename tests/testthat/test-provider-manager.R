# Test Advanced Provider Manager

library(testthat)
library(data.table)

# Helper function to create test providers
create_test_providers <- function() {
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
    enabled = TRUE,
    priority = 2,
    rate_limit = 50
  )

  provider3 <- DataProvider(
    name = "provider3",
    config = list(),
    enabled = FALSE,
    priority = 3,
    rate_limit = 25
  )

  list(provider1, provider2, provider3)
}

# Test Advanced Provider Manager Creation
test_that("AdvancedProviderManager can be created with defaults", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(providers)

  expect_s7_class(manager, AdvancedProviderManager)
  expect_length(manager@providers, 3)
  expect_equal(manager@failover_strategy, "priority")
  expect_equal(manager@circuit_breaker_threshold, 3)
  expect_equal(manager@circuit_breaker_window, 15)
  expect_type(manager@provider_health, "list")
  expect_length(manager@provider_health, 3)
})

test_that("AdvancedProviderManager can be created with custom settings", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(
    providers = providers,
    failover_strategy = "health_based",
    circuit_breaker_threshold = 5,
    circuit_breaker_window = 30,
    retry_config = list(max_retries = 3, backoff_factor = 3),
    cache_config = list(enabled = FALSE, ttl_minutes = 120)
  )

  expect_equal(manager@failover_strategy, "health_based")
  expect_equal(manager@circuit_breaker_threshold, 5)
  expect_equal(manager@circuit_breaker_window, 30)
  expect_equal(manager@retry_config$max_retries, 3)
  expect_equal(manager@retry_config$backoff_factor, 3)
  expect_false(manager@cache_config$enabled)
  expect_equal(manager@cache_config$ttl_minutes, 120)
})

# Test Provider Health Initialization
test_that("Provider health is initialized correctly", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(providers)

  health <- manager@provider_health
  expect_length(health, 3)

  for (provider_name in c("provider1", "provider2", "provider3")) {
    expect_true(provider_name %in% names(health))

    provider_health <- health[[provider_name]]
    expect_equal(provider_health$name, provider_name)
    expect_equal(provider_health$failures, 0)
    expect_null(provider_health$last_failure)
    expect_false(provider_health$circuit_open)
    expect_type(provider_health$last_success, "double")
    expect_length(provider_health$response_times, 0)
  }
})

# Test Provider Order Strategies
test_that("Priority-based provider order works", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(
    providers,
    failover_strategy = "priority"
  )

  order <- get_provider_order(manager)

  expect_equal(order, c("provider1", "provider2", "provider3"))
})

test_that("Round-robin provider order works", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(
    providers,
    failover_strategy = "round_robin"
  )

  order <- get_provider_order(manager)

  expect_length(order, 3)
  expect_true(all(c("provider1", "provider2", "provider3") %in% order))
})

test_that("Health-based provider order works", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(
    providers,
    failover_strategy = "health_based"
  )

  order <- get_provider_order(manager)

  expect_length(order, 3)
  expect_type(order, "character")
})

# Test Provider Retrieval
test_that("get_provider_by_name works", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(providers)

  provider1 <- get_provider_by_name(manager, "provider1")
  expect_s7_class(provider1, DataProvider)
  expect_equal(provider1@name, "provider1")

  provider_none <- get_provider_by_name(manager, "nonexistent")
  expect_null(provider_none)
})

# Test Circuit Breaker Logic
test_that("Circuit breaker starts closed", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(providers)

  expect_false(is_circuit_open(manager, "provider1"))
  expect_false(is_circuit_open(manager, "provider2"))
  expect_false(is_circuit_open(manager, "nonexistent"))
})

test_that("Circuit breaker opens after failures", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(
    providers,
    circuit_breaker_threshold = 2
  )

  # Simulate failures
  manager <- update_provider_health(manager, "provider1", success = FALSE)
  expect_false(is_circuit_open(manager, "provider1"))

  manager <- update_provider_health(manager, "provider1", success = FALSE)
  expect_true(is_circuit_open(manager, "provider1"))
})

test_that("Circuit breaker resets after time window", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(
    providers,
    circuit_breaker_threshold = 1,
    circuit_breaker_window = 0.01 # Very short window for testing
  )

  # Trip circuit breaker
  manager <- update_provider_health(manager, "provider1", success = FALSE)
  expect_true(is_circuit_open(manager, "provider1"))

  # Wait for reset window (0.01 minutes = 0.6 seconds)
  Sys.sleep(0.7)

  # Should reset
  expect_false(is_circuit_open(manager, "provider1"))
})

# Test Health Updates
test_that("Provider health updates on success", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(providers)

  initial_failures <- manager@provider_health$provider1$failures

  manager <- update_provider_health(manager, "provider1", success = TRUE)

  health <- manager@provider_health$provider1
  expect_gte(health$last_success, Sys.time() - 1) # Within last second
  expect_lte(health$failures, initial_failures) # Should not increase
})

test_that("Provider health updates on failure", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(providers)

  initial_failures <- manager@provider_health$provider1$failures

  manager <- update_provider_health(manager, "provider1", success = FALSE)

  health <- manager@provider_health$provider1
  expect_equal(health$failures, initial_failures + 1)
  expect_type(health$last_failure, "double")
})

# Test Response Time Recording
test_that("Response times are recorded", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(providers)

  manager <- record_response_time(manager, "provider1", 0.5)
  manager <- record_response_time(manager, "provider1", 1.0)
  manager <- record_response_time(manager, "provider1", 0.8)

  response_times <- manager@provider_health$provider1$response_times
  expect_length(response_times, 3)
  expect_equal(response_times, c(0.5, 1.0, 0.8))
})

test_that("Response times are limited to last 10", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(providers)

  # Record 15 response times
  for (i in 1:15) {
    manager <- record_response_time(manager, "provider1", i * 0.1)
  }

  response_times <- manager@provider_health$provider1$response_times
  expect_length(response_times, 10)
  expect_equal(response_times, (6:15) * 0.1) # Should keep last 10
})

# Test Cache Configuration
test_that("Cache configuration is stored correctly", {
  providers <- create_test_providers()
  cache_config <- list(enabled = TRUE, ttl_minutes = 30, dir = "test_cache")

  manager <- create_advanced_provider_manager(
    providers,
    cache_config = cache_config
  )

  expect_true(manager@cache_config$enabled)
  expect_equal(manager@cache_config$ttl_minutes, 30)
  expect_equal(manager@cache_config$dir, "test_cache")
})

# Test Retry Configuration
test_that("Retry configuration is stored correctly", {
  providers <- create_test_providers()
  retry_config <- list(max_retries = 5, backoff_factor = 1.5)

  manager <- create_advanced_provider_manager(
    providers,
    retry_config = retry_config
  )

  expect_equal(manager@retry_config$max_retries, 5)
  expect_equal(manager@retry_config$backoff_factor, 1.5)
})

# Test Edge Cases
test_that("Manager handles empty provider list", {
  manager <- create_advanced_provider_manager(list())

  expect_length(manager@providers, 0)
  expect_length(manager@provider_health, 0)

  order <- get_provider_order(manager)
  expect_length(order, 0)
})

test_that("Manager handles nonexistent provider health updates", {
  providers <- create_test_providers()
  manager <- create_advanced_provider_manager(providers)

  # Should not error when updating nonexistent provider
  expect_no_error(update_provider_health(
    manager,
    "nonexistent",
    success = TRUE
  ))
  expect_no_error(record_response_time(manager, "nonexistent", 1.0))
})
