test_that("get_api_token retrieves token from environment", {
  # Set up test environment variable
  old_token <- Sys.getenv("ALPHAVANTAGE_API_KEY", unset = NA)
  on.exit({
    if (is.na(old_token)) {
      Sys.unsetenv("ALPHAVANTAGE_API_KEY")
    } else {
      Sys.setenv(ALPHAVANTAGE_API_KEY = old_token)
    }
  })

  # Test with valid token
  test_token <- "test_api_key_12345"
  Sys.setenv(ALPHAVANTAGE_API_KEY = test_token)

  result <- get_api_token("alpha_vantage")
  expect_equal(result, test_token)
})

test_that("get_api_token handles missing token when required", {
  # Ensure environment variable is not set
  old_token <- Sys.getenv("ALPHAVANTAGE_API_KEY", unset = NA)
  on.exit({
    if (is.na(old_token)) {
      Sys.unsetenv("ALPHAVANTAGE_API_KEY")
    } else {
      Sys.setenv(ALPHAVANTAGE_API_KEY = old_token)
    }
  })

  Sys.unsetenv("ALPHAVANTAGE_API_KEY")

  # Should throw error when required = TRUE (default)
  expect_error(
    get_api_token("alpha_vantage"),
    "alpha_vantage API token not found"
  )
})

test_that("get_api_token returns NULL when not required and missing", {
  # Ensure environment variable is not set
  old_token <- Sys.getenv("ALPHAVANTAGE_API_KEY", unset = NA)
  on.exit({
    if (is.na(old_token)) {
      Sys.unsetenv("ALPHAVANTAGE_API_KEY")
    } else {
      Sys.setenv(ALPHAVANTAGE_API_KEY = old_token)
    }
  })

  Sys.unsetenv("ALPHAVANTAGE_API_KEY")

  # Should return NULL when required = FALSE
  result <- get_api_token("alpha_vantage", required = FALSE)
  expect_null(result)
})

test_that("get_api_token handles empty token", {
  # Set up test with empty token
  old_token <- Sys.getenv("ALPHAVANTAGE_API_KEY", unset = NA)
  on.exit({
    if (is.na(old_token)) {
      Sys.unsetenv("ALPHAVANTAGE_API_KEY")
    } else {
      Sys.setenv(ALPHAVANTAGE_API_KEY = old_token)
    }
  })

  # Test with empty string
  Sys.setenv(ALPHAVANTAGE_API_KEY = "")
  expect_error(
    get_api_token("alpha_vantage"),
    "alpha_vantage API token not found"
  )

  # Test with whitespace only
  Sys.setenv(ALPHAVANTAGE_API_KEY = "   ")
  expect_error(
    get_api_token("alpha_vantage"),
    "alpha_vantage API token is empty"
  )
})

test_that("get_api_token handles unknown provider", {
  expect_error(
    get_api_token("unknown_provider"),
    "Unknown provider: unknown_provider"
  )
})

test_that("get_api_token trims whitespace from token", {
  # Set up test environment variable with whitespace
  old_token <- Sys.getenv("ALPHAVANTAGE_API_KEY", unset = NA)
  on.exit({
    if (is.na(old_token)) {
      Sys.unsetenv("ALPHAVANTAGE_API_KEY")
    } else {
      Sys.setenv(ALPHAVANTAGE_API_KEY = old_token)
    }
  })

  test_token <- "  test_api_key_12345  "
  expected_token <- "test_api_key_12345"
  Sys.setenv(ALPHAVANTAGE_API_KEY = test_token)

  result <- get_api_token("alpha_vantage")
  expect_equal(result, expected_token)
})
