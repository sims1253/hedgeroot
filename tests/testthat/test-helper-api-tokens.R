test_that("is_api_token_available returns TRUE when token exists", {
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

  result <- is_api_token_available("alpha_vantage")
  expect_true(result)
})

test_that("is_api_token_available returns FALSE when token is missing", {
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

  result <- is_api_token_available("alpha_vantage")
  expect_false(result)
})

test_that("is_api_token_available returns FALSE when token is empty", {
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
  result <- is_api_token_available("alpha_vantage")
  expect_false(result)

  # Test with whitespace only
  Sys.setenv(ALPHAVANTAGE_API_KEY = "   ")
  result <- is_api_token_available("alpha_vantage")
  expect_false(result)
})

test_that("is_api_token_available handles unknown provider gracefully", {
  result <- is_api_token_available("unknown_provider")
  expect_false(result)
})

test_that("skip_if_no_api_token behavior when token is missing", {
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

  # Test the underlying function that determines skip behavior
  expect_false(is_api_token_available("alpha_vantage"))

  # Note: We can't directly test skip_if_no_api_token here because
  # testthat::skip() immediately skips the test rather than throwing a catchable error
})

test_that("skip_if_no_api_token does not skip when token is available", {
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

  # This should not skip (should return invisibly)
  result <- skip_if_no_api_token("alpha_vantage")
  expect_null(result)
})

test_that("skip_if_no_api_token custom message behavior", {
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

  # Test that the underlying detection works correctly
  expect_false(is_api_token_available("alpha_vantage"))

  # Note: We can't test the custom message directly because testthat::skip()
  # immediately skips the test. The custom message functionality is tested
  # through integration tests that actually use skip_if_no_api_token.
})
