#' Data Provider Base Class
#'
#' S7 class for data provider interface
#'
#' @param name Character name of the provider
#' @param config List of configuration settings
#' @param enabled Logical indicating if provider is enabled
#' @param priority Numeric priority for failover ordering
#' @param rate_limit Numeric rate limit for requests
#' @importFrom S7 new_class class_character class_list class_logical class_numeric class_any
#' @importFrom data.table := as.data.table setnames setkey setorder
#' @export
DataProvider <- S7::new_class(
  "DataProvider",
  properties = list(
    name = S7::class_character,
    config = S7::class_list,
    enabled = S7::class_logical,
    priority = S7::class_numeric,
    rate_limit = S7::class_numeric
  )
)

#' Provider Manager Class
#'
#' Manages multiple data providers with failover
#'
#' @param providers List of data provider objects
#' @param failover_strategy Character strategy for failover
#' @param circuit_breaker Circuit breaker configuration
#' @export
ProviderManager <- S7::new_class(
  "ProviderManager",
  properties = list(
    providers = S7::class_list,
    failover_strategy = S7::class_character,
    circuit_breaker = S7::class_any
  )
)

#' Data Validation Results Class
#'
#' Contains validation results for fetched data
#'
#' @param is_valid Logical indicating if data is valid
#' @param quality_score Numeric quality score (0-1)
#' @param anomalies List of detected anomalies
#' @param warnings Character vector of warnings
#' @param errors Character vector of errors
#' @export
DataValidation <- S7::new_class(
  "DataValidation",
  properties = list(
    is_valid = S7::class_logical,
    quality_score = S7::class_numeric,
    anomalies = S7::class_list,
    warnings = S7::class_character,
    errors = S7::class_character
  )
)

# S7 method for fetching OHLCV data using provider manager with failover
S7::method(fetch_ohlcv, ProviderManager) <- function(
  provider,
  symbol,
  start_date,
  end_date,
  ...
) {
  # Try providers in priority order
  for (sub_provider in provider@providers) {
    if (!sub_provider@enabled) {
      next
    }

    tryCatch(
      {
        # Use quantmod for MVP
        data <- quantmod::getSymbols(
          symbol,
          from = start_date,
          to = end_date,
          auto.assign = FALSE,
          ...
        )

        # Convert to data.table for performance
        dt <- data.table::as.data.table(data, keep.rownames = "date")
        dt[, "date" := as.Date(date)]
        dt[, "symbol" := symbol]

        # Standardize column names for OHLCV data
        dt <- standardize_ohlcv_columns(dt)

        # Add efficient data transformations
        dt <- add_ohlcv_transformations(dt)

        return(dt)
      },
      error = function(e) {
        warning(paste("Provider", sub_provider@name, "failed:", e$message))
        NULL
      }
    )
  }

  stop("All providers failed for symbol: ", symbol)
}

#' Validate Data Quality
#'
#' Validates fetched financial data
#'
#' @param data Data to validate
#' @param symbol Symbol name
#' @param validation_rules List of validation rules
#' @return DataValidation object
#' @export
validate_data <- function(data, symbol, validation_rules = list()) {
  errors <- character(0)
  warnings <- character(0)
  anomalies <- list()

  # Basic validation checks
  if (nrow(data) == 0) {
    errors <- c(errors, "No data returned")
  }

  if (any(is.na(data))) {
    warnings <- c(warnings, "Data contains NA values")
  }

  # Calculate quality score (simple implementation)
  quality_score <- max(0, 1 - (length(warnings) * 0.1) - (length(errors) * 0.6))

  DataValidation(
    is_valid = length(errors) == 0,
    quality_score = quality_score,
    anomalies = anomalies,
    warnings = warnings,
    errors = errors
  )
}

#' Detect Data Anomalies
#'
#' Detects anomalies in financial data
#'
#' @param data Financial data
#' @param method Detection method
#' @return List of detected anomalies
#' @export
detect_anomalies <- function(data, method = "statistical") {
  # Simple statistical anomaly detection for MVP
  anomalies <- list()

  if ("close" %in% names(data) && nrow(data) > 1) {
    # Detect extreme price movements
    returns <- diff(log(data$close))

    # Use a more robust threshold for small datasets
    if (length(returns) >= 2) {
      threshold <- 0.3 # Fixed 30% threshold for log returns (roughly 35% price change)
      extreme_moves <- which(abs(returns) > threshold)

      if (length(extreme_moves) > 0) {
        anomalies$extreme_moves <- extreme_moves
      }
    }
  }

  return(anomalies)
}

#' Cache Data
#'
#' Simple file-based caching for MVP
#'
#' @param data Data to cache
#' @param symbol Symbol name
#' @param cache_config Cache configuration
#' @return Logical indicating success
#' @export
cache_data <- function(data, symbol, cache_config = list()) {
  cache_dir <- cache_config$dir %||% "cache"

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_file <- file.path(cache_dir, paste0(symbol, "_", Sys.Date(), ".rds"))

  tryCatch(
    {
      saveRDS(data, cache_file)
      TRUE
    },
    error = function(e) {
      warning("Failed to cache data: ", e$message)
      FALSE
    }
  )
}


#' Standardize OHLCV Column Names
#'
#' Standardizes column names from different data providers
#'
#' @param dt data.table with OHLCV data
#' @return data.table with standardized column names
#' @importFrom data.table setnames
#' @export
standardize_ohlcv_columns <- function(dt) {
  # Common column name mappings from different providers
  col_mappings <- list(
    # Yahoo Finance / quantmod format
    c(".*\\.Open", "open"),
    c(".*\\.High", "high"),
    c(".*\\.Low", "low"),
    c(".*\\.Close", "close"),
    c(".*\\.Volume", "volume"),
    c(".*\\.Adjusted", "adjusted"),
    # Generic formats
    c("Open", "open"),
    c("High", "high"),
    c("Low", "low"),
    c("Close", "close"),
    c("Volume", "volume"),
    c("Adjusted", "adjusted")
  )

  # Apply column name standardization
  current_names <- names(dt)
  for (mapping in col_mappings) {
    pattern <- mapping[1]
    replacement <- mapping[2]
    matches <- grep(pattern, current_names, ignore.case = TRUE)
    if (length(matches) > 0) {
      data.table::setnames(dt, current_names[matches[1]], replacement)
      current_names <- names(dt)
    }
  }

  return(dt)
}

#' Add OHLCV Data Transformations
#'
#' Adds efficient data transformations using data.table
#'
#' @param dt data.table with OHLCV data
#' @return data.table with additional transformations
#' @importFrom data.table setorder setkey
#' @export
add_ohlcv_transformations <- function(dt) {
  # Ensure data is sorted by date
  data.table::setorder(dt, date)

  # Set key for efficient operations
  data.table::setkey(dt, date, symbol)

  # Add basic transformations using data.table syntax
  dt[,
    `:=`(
      # Price transformations
      typical_price = (high + low + close) / 3,
      price_range = high - low,
      price_change = close - shift(close, 1L),
      price_change_pct = (close - shift(close, 1L)) /
        shift(close, 1L) *
        100,

      # Log transformations for returns
      log_close = log(close),
      log_return = log(close) - shift(log(close), 1L),

      # Volume transformations
      volume_ma_5 = frollmean(volume, 5, na.rm = TRUE),
      volume_ratio = volume / frollmean(volume, 20, na.rm = TRUE),

      # Volatility measures
      true_range = pmax(
        high - low,
        abs(high - shift(close, 1L)),
        abs(low - shift(close, 1L)),
        na.rm = TRUE
      )
    ),
    by = "symbol"
  ]

  return(dt)
}

#' Fast OHLCV Data Processing
#'
#' High-performance OHLCV data processing using data.table
#'
#' @param dt data.table with OHLCV data
#' @param operations List of operations to perform
#' @return Processed data.table
#' @export
process_ohlcv_fast <- function(
  dt,
  operations = c("returns", "ma", "volatility")
) {
  if ("returns" %in% operations) {
    # Fast return calculations
    dt[,
      `:=`(
        return_1d = (close - shift(close, 1L)) /
          shift(close, 1L),
        return_5d = (close - shift(close, 5L)) /
          shift(close, 5L),
        return_20d = (close - shift(close, 20L)) /
          shift(close, 20L)
      ),
      by = "symbol"
    ]
  }

  if ("ma" %in% operations) {
    # Fast moving averages
    dt[,
      `:=`(
        sma_5 = frollmean(close, 5, na.rm = TRUE),
        sma_10 = frollmean(close, 10, na.rm = TRUE),
        sma_20 = frollmean(close, 20, na.rm = TRUE),
        sma_50 = frollmean(close, 50, na.rm = TRUE)
      ),
      by = "symbol"
    ]
  }

  if ("volatility" %in% operations) {
    # Fast volatility calculations
    dt[,
      `:=`(
        volatility_5d = frollapply(log_return, 5, sd, na.rm = TRUE),
        volatility_20d = frollapply(log_return, 20, sd, na.rm = TRUE),
        atr_14 = frollmean(true_range, 14, na.rm = TRUE)
      ),
      by = "symbol"
    ]
  }

  return(dt)
}

#' Efficient Data Aggregation
#'
#' Fast data aggregation using data.table
#'
#' @param dt data.table with OHLCV data
#' @param period Aggregation period ("daily", "weekly", "monthly")
#' @return Aggregated data.table
#' @importFrom data.table week month year
#' @export
aggregate_ohlcv <- function(dt, period = "weekly") {
  if (period == "weekly") {
    dt[, "week_year" := paste(year(date), week(date), sep = "-")]
    agg_dt <- dt[,
      .(
        date = max(date),
        open = first(open),
        high = max(high),
        low = min(low),
        close = last(close),
        volume = sum(volume, na.rm = TRUE)
      ),
      by = .(symbol, week_year)
    ]
    agg_dt[, "week_year" := NULL]
  } else if (period == "monthly") {
    dt[, "month_year" := paste(year(date), month(date), sep = "-")]
    agg_dt <- dt[,
      .(
        date = max(date),
        open = first(open),
        high = max(high),
        low = min(low),
        close = last(close),
        volume = sum(volume, na.rm = TRUE)
      ),
      by = .(symbol, month_year)
    ]
    agg_dt[, "month_year" := NULL]
  } else {
    # Return daily data as-is
    agg_dt <- dt
  }

  return(agg_dt)
}
