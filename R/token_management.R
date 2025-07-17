#' Get API Token from Environment
#'
#' Retrieves API tokens from environment variables with proper error handling
#'
#' @param provider_name Character name of the provider (e.g., "alpha_vantage")
#' @param required Logical indicating if the token is required (default TRUE)
#' @return Character API token or NULL if not required and missing
#' @export
get_api_token <- function(provider_name = "alpha_vantage", required = TRUE) {
  # Define environment variable mappings for different providers
  env_var_map <- list(
    alpha_vantage = "ALPHAVANTAGE_API_KEY"
  )

  # Get the environment variable name for the provider
  env_var_name <- env_var_map[[provider_name]]

  if (is.null(env_var_name)) {
    stop(
      "Unknown provider: ",
      provider_name,
      ". Supported providers: ",
      paste(names(env_var_map), collapse = ", ")
    )
  }

  # Get the token from environment
  token <- Sys.getenv(env_var_name, unset = "")

  # Handle missing token
  if (token == "" || is.na(token)) {
    if (required) {
      stop(
        provider_name,
        " API token not found. ",
        "Please set the ",
        env_var_name,
        " environment variable. ",
        "For setup instructions, see the package documentation."
      )
    } else {
      return(NULL)
    }
  }

  # Basic validation - ensure token is not just whitespace
  token <- trimws(token)
  if (nchar(token) == 0) {
    if (required) {
      stop(
        provider_name,
        " API token is empty. ",
        "Please set a valid ",
        env_var_name,
        " environment variable."
      )
    } else {
      return(NULL)
    }
  }

  return(token)
}
