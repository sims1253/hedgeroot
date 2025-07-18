#' Alpha Vantage Data Provider
#'
#' S7 class for Alpha Vantage data provider
#'
#' @param name Character name of the provider
#' @param config List of configuration settings
#' @param enabled Logical indicating if provider is enabled
#' @param priority Numeric priority for failover ordering
#' @param rate_limit Numeric rate limit for requests
#' @param api_key Character API key for Alpha Vantage
#' @param base_url Character base URL for Alpha Vantage API
#' @param rate_limit_per_minute Numeric rate limit per minute
#' @param last_request_time Last request timestamp
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
AlphaVantageProvider <- S7::new_class(
  "AlphaVantageProvider",
  properties = list(
    name = S7::class_character,
    config = S7::class_list,
    enabled = S7::class_logical,
    priority = S7::class_numeric,
    rate_limit = S7::class_numeric,
    api_key = S7::class_character,
    base_url = S7::class_character,
    rate_limit_per_minute = S7::class_numeric,
    last_request_time = S7::class_any
  )
)

#' Create Alpha Vantage Provider
#'
#' Factory function to create AlphaVantageProvider objects
#'
#' @param api_key Alpha Vantage API key (optional, will use ALPHAVANTAGE_API_KEY environment variable if not provided)
#' @param rate_limit_per_minute Rate limit (default 5 for free tier)
#' @return AlphaVantageProvider object
#' @export
create_alpha_vantage_provider <- function(
  api_key = NULL,
  rate_limit_per_minute = 5
) {
  # Get API key from environment if not provided
  if (is.null(api_key)) {
    api_key <- get_api_token("alpha_vantage", required = TRUE)
  } else {
    # Validate provided API key
    if (is.na(api_key) || trimws(api_key) == "") {
      stop(
        "Alpha Vantage API key cannot be empty. Please provide a valid API key or set the ALPHAVANTAGE_API_KEY environment variable."
      )
    }
    api_key <- trimws(api_key)
  }

  AlphaVantageProvider(
    name = "alpha_vantage",
    config = list(api_key = api_key),
    enabled = TRUE,
    priority = 2,
    rate_limit = rate_limit_per_minute,
    api_key = api_key,
    base_url = "https://www.alphavantage.co/query",
    rate_limit_per_minute = rate_limit_per_minute,
    last_request_time = Sys.time() - 60 # Initialize to allow immediate request
  )
}

# S7 method for fetching OHLCV data from Alpha Vantage
S7::method(fetch_ohlcv, AlphaVantageProvider) <- function(
  provider,
  symbol,
  start_date,
  end_date,
  outputsize = "full",
  ...
) {
  # Rate limiting
  time_since_last <- as.numeric(difftime(
    Sys.time(),
    provider@last_request_time,
    units = "secs"
  ))
  min_interval <- 60 / provider@rate_limit_per_minute

  if (time_since_last < min_interval) {
    wait_time <- min_interval - time_since_last
    message(paste("Rate limiting: waiting", round(wait_time, 1), "seconds"))
    Sys.sleep(wait_time)
  }

  # Update last request time
  provider@last_request_time <- Sys.time()

  # Build API URL
  url <- paste0(
    provider@base_url,
    "?function=TIME_SERIES_DAILY",
    "&symbol=",
    symbol,
    "&outputsize=",
    outputsize,
    "&apikey=",
    provider@api_key
  )

  tryCatch(
    {
      # Make API request
      response <- httr::GET(url)

      if (response$status_code != 200) {
        stop(
          "Alpha Vantage API request failed with status: ",
          response$status_code
        )
      }

      # Parse JSON response
      content <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content)

      # Check for API errors
      if ("Error Message" %in% names(data)) {
        stop("Alpha Vantage API error: ", data[["Error Message"]])
      }

      if ("Note" %in% names(data)) {
        warning("Alpha Vantage API note: ", data$Note)
      }

      # Extract time series data - check for multiple possible keys
      possible_keys <- c(
        "Time Series (Daily)",
        "Time Series (Daily Adjusted)",
        "Daily Time Series",
        "Daily Adjusted Time Series"
      )

      time_series_key <- NULL
      for (key in possible_keys) {
        if (key %in% names(data)) {
          time_series_key <- key
          break
        }
      }

      if (is.null(time_series_key)) {
        # Debug: show available keys
        available_keys <- names(data)
        stop(paste(
          "Unexpected Alpha Vantage response format. Available keys:",
          paste(available_keys, collapse = ", ")
        ))
      }

      time_series <- data[[time_series_key]]

      # Convert to data.table
      dates <- as.Date(names(time_series))

      # Extract OHLCV data with error handling
      extract_field <- function(field_name) {
        tryCatch(
          {
            values <- sapply(time_series, function(x) {
              if (is.list(x) && field_name %in% names(x)) {
                as.numeric(x[[field_name]])
              } else {
                NA_real_
              }
            })
            as.numeric(values)
          },
          error = function(e) {
            rep(NA_real_, length(time_series))
          }
        )
      }

      dt <- data.table::data.table(
        date = dates,
        open = extract_field("1. open"),
        high = extract_field("2. high"),
        low = extract_field("3. low"),
        close = extract_field("4. close"),
        adjusted = extract_field("5. adjusted close"),
        volume = extract_field("6. volume"),
        symbol = symbol
      )

      # Filter by date range
      if (!missing(start_date)) {
        dt <- dt[date >= as.Date(start_date)]
      }
      if (!missing(end_date)) {
        dt <- dt[date <= as.Date(end_date)]
      }

      # Sort by date
      data.table::setorder(dt, date)

      # Add efficient data transformations
      dt <- add_ohlcv_transformations(dt)

      return(dt)
    },
    error = function(e) {
      stop("Alpha Vantage data fetch failed: ", e$message)
    }
  )
}
