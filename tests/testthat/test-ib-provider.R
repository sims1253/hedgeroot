# Test Interactive Brokers Provider

library(testthat)
library(data.table)

# Test IB Provider Creation
test_that("IBProvider can be created with default values", {
  provider <- create_ib_provider()
  
  expect_s7_class(provider, IBProvider)
  expect_equal(provider@name, "interactive_brokers")
  expect_equal(provider@host, "127.0.0.1")
  expect_equal(provider@port, 7497)
  expect_equal(provider@client_id, 1)
  expect_equal(provider@market_data_type, 3)
  expect_true(provider@enabled)
  expect_equal(provider@priority, 1)
  expect_equal(provider@connection_status, "disconnected")
})

test_that("IBProvider can be created with custom values", {
  provider <- create_ib_provider(
    host = "192.168.1.100",
    port = 7496,
    client_id = 5,
    market_data_type = 1
  )
  
  expect_equal(provider@host, "192.168.1.100")
  expect_equal(provider@port, 7496)
  expect_equal(provider@client_id, 5)
  expect_equal(provider@market_data_type, 1)
})

test_that("IBProvider configuration is consistent", {
  provider <- create_ib_provider("localhost", 4001, 10)
  
  expect_equal(provider@config$host, provider@host)
  expect_equal(provider@config$port, provider@port)
  expect_equal(provider@config$client_id, provider@client_id)
})

# Test connection status management
test_that("Connection status can be checked", {
  provider <- create_ib_provider()
  
  expect_false(is_ib_connected(provider))
  
  # Simulate connection
  provider@connection_status <- "connected"
  expect_true(is_ib_connected(provider))
})

test_that("Disconnection works", {
  provider <- create_ib_provider()
  provider@connection_status <- "connected"
  
  expect_true(is_ib_connected(provider))
  
  # disconnect_ib returns the modified provider since S7 objects are passed by value
  provider <- disconnect_ib(provider)
  expect_equal(provider@connection_status, "disconnected")
  expect_false(is_ib_connected(provider))
})

# Test symbol conversion
test_that("IB to Yahoo symbol conversion works", {
  # Test stock symbol (no change)
  expect_equal(convert_ib_to_yahoo_symbol("AAPL"), "AAPL")
  expect_equal(convert_ib_to_yahoo_symbol("MSFT"), "MSFT")
  
  # Test forex symbol conversion
  expect_equal(convert_ib_to_yahoo_symbol("EUR.USD"), "EURUSD=X")
  expect_equal(convert_ib_to_yahoo_symbol("GBP.USD"), "GBPUSD=X")
  expect_equal(convert_ib_to_yahoo_symbol("USD.JPY"), "USDJPY=X")
})

test_that("Symbol conversion handles edge cases", {
  # Test empty string
  expect_equal(convert_ib_to_yahoo_symbol(""), "")
  
  # Test symbol without dot
  expect_equal(convert_ib_to_yahoo_symbol("GOOGL"), "GOOGL")
  
  # Test multiple dots (should only split on first)
  expect_equal(convert_ib_to_yahoo_symbol("A.B.C"), "AB=X")
})

# Test contract details
test_that("Contract details are returned", {
  provider <- create_ib_provider()
  
  details <- get_ib_contract_details(provider, "AAPL")
  
  expect_type(details, "list")
  expect_equal(details$symbol, "AAPL")
  expect_equal(details$exchange, "SMART")
  expect_equal(details$currency, "USD")
  expect_equal(details$sec_type, "STK")
  expect_equal(details$multiplier, 1)
  expect_equal(details$min_tick, 0.01)
  expect_type(details$market_hours, "list")
})

test_that("Contract details have required fields", {
  provider <- create_ib_provider()
  details <- get_ib_contract_details(provider, "TEST")
  
  required_fields <- c("symbol", "exchange", "currency", "sec_type", 
                      "multiplier", "min_tick", "market_hours")
  
  for (field in required_fields) {
    expect_true(field %in% names(details))
  }
})

test_that("Market hours are properly structured", {
  provider <- create_ib_provider()
  details <- get_ib_contract_details(provider, "AAPL")
  
  market_hours <- details$market_hours
  expect_true("open" %in% names(market_hours))
  expect_true("close" %in% names(market_hours))
  expect_true("timezone" %in% names(market_hours))
  
  expect_equal(market_hours$open, "09:30:00")
  expect_equal(market_hours$close, "16:00:00")
  expect_equal(market_hours$timezone, "US/Eastern")
})

# Test provider properties
test_that("IBProvider has correct default properties", {
  provider <- create_ib_provider()
  
  expect_equal(provider@rate_limit, 50)
  expect_equal(provider@priority, 1) # High priority for IB
  expect_true(provider@enabled)
  expect_equal(provider@name, "interactive_brokers")
})

test_that("IBProvider market data types are valid", {
  provider1 <- create_ib_provider(market_data_type = 1) # Live
  provider2 <- create_ib_provider(market_data_type = 3) # Delayed
  provider3 <- create_ib_provider(market_data_type = 4) # Delayed-frozen
  
  expect_equal(provider1@market_data_type, 1)
  expect_equal(provider2@market_data_type, 3)
  expect_equal(provider3@market_data_type, 4)
})

# Test error handling
test_that("IBProvider handles invalid parameters gracefully", {
  # Should not error with unusual but valid parameters
  expect_no_error(create_ib_provider(host = "0.0.0.0"))
  expect_no_error(create_ib_provider(port = 1))
  expect_no_error(create_ib_provider(client_id = 0))
})

# Test connection simulation
test_that("Connection status changes are tracked", {
  provider <- create_ib_provider()
  
  # Initial state
  expect_equal(provider@connection_status, "disconnected")
  
  # Simulate connection
  provider@connection_status <- "connected"
  expect_equal(provider@connection_status, "connected")
  
  # Simulate disconnection
  provider <- disconnect_ib(provider)
  expect_equal(provider@connection_status, "disconnected")
})