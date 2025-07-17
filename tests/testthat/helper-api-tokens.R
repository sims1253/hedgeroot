#' Skip Test If No API Token Available
#'
#' This function checks if an API token is available for the specified provider
#' and skips the test if the token is missing. This allows tests to run
#' conditionally based on whether API credentials are configured.
#'
#' @param provider_name Character name of the provider (e.g., "alpha_vantage")
#' @param message Optional custom message for the skip reason
#' @return Invisible NULL if token is available, otherwise skips the test
skip_if_no_api_token <- function(
  provider_name = "alpha_vantage",
  message = NULL
) {
  # Check if token is available without throwing an error
  token_available <- is_api_token_available(provider_name)

  if (!token_available) {
    if (is.null(message)) {
      message <- paste0(
        "API token for ",
        provider_name,
        " not available. ",
        "Set environment variable to run this test."
      )
    }
    testthat::skip(message)
  }

  invisible(NULL)
}

#' Skip Test If API Testing Is Disabled
#'
#' This function checks if real API testing is enabled via environment variable
#' and skips the test if disabled. This helps conserve API quota by allowing
#' developers to disable real API calls during development.
#'
#' @param message Optional custom message for the skip reason
#' @return Invisible NULL if API testing is enabled, otherwise skips the test
skip_if_api_testing_disabled <- function(message = NULL) {
  api_testing_enabled <- Sys.getenv("HEDGEROOT_ENABLE_API_TESTS", "false")

  if (tolower(api_testing_enabled) != "true") {
    if (is.null(message)) {
      message <- paste0(
        "Real API testing disabled. ",
        "Set HEDGEROOT_ENABLE_API_TESTS=true to enable real API calls."
      )
    }
    testthat::skip(message)
  }

  invisible(NULL)
}

#' Skip Test If No API Token Available or API Testing Disabled
#'
#' Convenience function that combines both API token and API testing checks.
#' This is the recommended function to use for tests that make real API calls.
#'
#' @param provider_name Character name of the provider (e.g., "alpha_vantage")
#' @param message Optional custom message for the skip reason
#' @return Invisible NULL if both conditions are met, otherwise skips the test
skip_if_no_api_access <- function(
  provider_name = "alpha_vantage",
  message = NULL
) {
  # First check if API testing is enabled
  skip_if_api_testing_disabled()

  # Then check if token is available
  skip_if_no_api_token(provider_name, message)

  invisible(NULL)
}

#' Check If API Token Is Available
#'
#' Checks whether an API token is available for the specified provider
#' without throwing an error if it's missing.
#'
#' @param provider_name Character name of the provider (e.g., "alpha_vantage")
#' @return Logical TRUE if token is available and valid, FALSE otherwise
is_api_token_available <- function(provider_name = "alpha_vantage") {
  tryCatch(
    {
      # Try to get the token with required = FALSE
      token <- get_api_token(provider_name, required = FALSE)

      # Return TRUE if token exists and is not NULL
      !is.null(token)
    },
    error = function(e) {
      # If any error occurs, assume token is not available
      FALSE
    }
  )
}
