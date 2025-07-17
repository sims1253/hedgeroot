#' Advanced Provider Manager with Failover
#'
#' Enhanced provider manager with sophisticated failover strategies
#'
#' @param providers List of data provider objects
#' @param failover_strategy Character strategy for failover
#' @param circuit_breaker Circuit breaker configuration
#' @param circuit_breaker_threshold Numeric failure threshold for circuit breaker
#' @param circuit_breaker_window Numeric time window for circuit breaker reset
#' @param provider_health List of provider health metrics
#' @param retry_config List of retry configuration settings
#' @param cache_config List of cache configuration settings
#' @export
AdvancedProviderManager <- S7::new_class(
  "AdvancedProviderManager",
  properties = list(
    providers = S7::class_list,
    failover_strategy = S7::class_character,
    circuit_breaker = S7::class_any,
    circuit_breaker_threshold = S7::class_numeric,
    circuit_breaker_window = S7::class_numeric,
    provider_health = S7::class_list,
    retry_config = S7::class_list,
    cache_config = S7::class_list
  )
)

#' Create Advanced Provider Manager
#'
#' Factory function to create AdvancedProviderManager with failover
#'
#' @param providers List of data provider objects
#' @param failover_strategy Failover strategy ("priority", "round_robin", "health_based")
#' @param circuit_breaker_threshold Number of failures before circuit breaker trips
#' @param circuit_breaker_window Time window for circuit breaker (minutes)
#' @param retry_config Retry configuration
#' @param cache_config Cache configuration
#' @return AdvancedProviderManager object
#' @export
create_advanced_provider_manager <- function(
  providers = list(),
  failover_strategy = "priority",
  circuit_breaker_threshold = 3,
  circuit_breaker_window = 15,
  retry_config = list(max_retries = 2, backoff_factor = 2),
  cache_config = list(enabled = TRUE, ttl_minutes = 60)
) {
  # Initialize provider health tracking
  provider_health <- lapply(providers, function(p) {
    list(
      name = p@name,
      failures = 0,
      last_failure = NULL,
      circuit_open = FALSE,
      last_success = Sys.time(),
      response_times = numeric(0)
    )
  })
  names(provider_health) <- sapply(providers, function(p) p@name)

  AdvancedProviderManager(
    providers = providers,
    failover_strategy = failover_strategy,
    circuit_breaker = NULL,
    circuit_breaker_threshold = circuit_breaker_threshold,
    circuit_breaker_window = circuit_breaker_window,
    provider_health = provider_health,
    retry_config = retry_config,
    cache_config = cache_config
  )
}

#' Fetch OHLCV with Advanced Failover
#'
#' @param provider AdvancedProviderManager object
#' @param symbol Character symbol to fetch
#' @param start_date Start date for data
#' @param end_date End date for data
#' @param ... Additional arguments
#' @return data.table with OHLCV data
#' @export
fetch_ohlcv.AdvancedProviderManager <- function(
  provider,
  symbol,
  start_date,
  end_date,
  ...
) {
  # Check cache first
  if (provider@cache_config$enabled) {
    cached_data <- get_cached_data(
      provider,
      symbol,
      start_date,
      end_date
    )
    if (!is.null(cached_data)) {
      message("Returning cached data for ", symbol)
      return(cached_data)
    }
  }

  # Get provider order based on strategy
  provider_order <- get_provider_order(provider)

  last_error <- NULL

  for (provider_name in provider_order) {
    sub_provider <- get_provider_by_name(provider, provider_name)

    if (is.null(sub_provider) || !sub_provider@enabled) {
      next
    }

    # Check circuit breaker
    if (is_circuit_open(provider, provider_name)) {
      message("Circuit breaker open for provider: ", provider_name)
      next
    }

    # Attempt to fetch data with retries
    result <- fetch_with_retry(
      provider,
      sub_provider,
      symbol,
      start_date,
      end_date,
      ...
    )

    if (!is.null(result)) {
      # Success - update health metrics
      update_provider_health(provider, provider_name, success = TRUE)

      # Cache the result
      if (provider@cache_config$enabled) {
        cache_data(result, symbol, provider@cache_config)
      }

      return(result)
    } else {
      # Failure - update health metrics
      update_provider_health(provider, provider_name, success = FALSE)
    }
  }

  stop(
    "All providers failed for symbol: ",
    symbol,
    if (!is.null(last_error)) paste(". Last error:", last_error$message) else ""
  )
}

#' Get Provider Order Based on Strategy
#'
#' @param provider_manager AdvancedProviderManager object
#' @return Character vector of provider names in order
get_provider_order <- function(provider_manager) {
  switch(
    provider_manager@failover_strategy,
    "priority" = {
      # Sort by priority (lower number = higher priority)
      providers_sorted <- provider_manager@providers[order(sapply(
        provider_manager@providers,
        function(p) p@priority
      ))]
      sapply(providers_sorted, function(p) p@name)
    },
    "round_robin" = {
      # Simple round robin (could be enhanced with state tracking)
      sapply(provider_manager@providers, function(p) p@name)
    },
    "health_based" = {
      # Sort by health score (success rate, response time)
      health_scores <- sapply(
        names(provider_manager@provider_health),
        function(name) {
          health <- provider_manager@provider_health[[name]]
          if (health$circuit_open) {
            return(-1)
          }

          # Calculate success rate over recent period
          success_rate <- max(0, 1 - (health$failures / 10)) # Simple metric

          # Factor in response time (lower is better)
          avg_response_time <- if (length(health$response_times) > 0) {
            mean(tail(health$response_times, 5))
          } else {
            1.0 # Default
          }

          success_rate / (1 + avg_response_time)
        }
      )

      names(sort(health_scores, decreasing = TRUE))
    },
    sapply(provider_manager@providers, function(p) p@name) # Default to original order
  )
}

#' Get Provider by Name
#'
#' @param provider_manager AdvancedProviderManager object
#' @param provider_name Name of provider to retrieve
#' @return Provider object or NULL
get_provider_by_name <- function(provider_manager, provider_name) {
  for (provider in provider_manager@providers) {
    if (provider@name == provider_name) {
      return(provider)
    }
  }
  return(NULL)
}

#' Check if Circuit Breaker is Open
#'
#' @param provider_manager AdvancedProviderManager object
#' @param provider_name Name of provider to check
#' @return Logical indicating if circuit is open
is_circuit_open <- function(provider_manager, provider_name) {
  health <- provider_manager@provider_health[[provider_name]]
  if (is.null(health)) {
    return(FALSE)
  }

  # Check if circuit breaker should be reset
  if (health$circuit_open && !is.null(health$last_failure)) {
    time_since_failure <- as.numeric(difftime(
      Sys.time(),
      health$last_failure,
      units = "mins"
    ))
    if (time_since_failure > provider_manager@circuit_breaker_window) {
      # Reset circuit breaker
      provider_manager@provider_health[[provider_name]]$circuit_open <- FALSE
      provider_manager@provider_health[[provider_name]]$failures <- 0
      message("Circuit breaker reset for provider: ", provider_name)
      return(FALSE)
    }
  }

  return(health$circuit_open)
}

#' Fetch Data with Retry Logic
#'
#' @param provider_manager AdvancedProviderManager object
#' @param provider Provider object
#' @param symbol Symbol to fetch
#' @param start_date Start date
#' @param end_date End date
#' @param ... Additional arguments
#' @return Data or NULL on failure
fetch_with_retry <- function(
  provider_manager,
  provider,
  symbol,
  start_date,
  end_date,
  ...
) {
  max_retries <- provider_manager@retry_config$max_retries %||% 2
  backoff_factor <- provider_manager@retry_config$backoff_factor %||% 2

  for (attempt in 1:(max_retries + 1)) {
    start_time <- Sys.time()

    tryCatch(
      {
        result <- fetch_ohlcv(provider, symbol, start_date, end_date, ...)

        # Record response time
        response_time <- as.numeric(difftime(
          Sys.time(),
          start_time,
          units = "secs"
        ))
        record_response_time(provider_manager, provider@name, response_time)

        return(result)
      },
      error = function(e) {
        message(
          "Provider ",
          provider@name,
          " failed (attempt ",
          attempt,
          "): ",
          e$message
        )

        if (attempt <= max_retries) {
          wait_time <- backoff_factor^(attempt - 1)
          message("Retrying in ", wait_time, " seconds...")
          Sys.sleep(wait_time)
        }

        NULL
      }
    )
  }

  return(NULL)
}

#' Update Provider Health Metrics
#'
#' @param provider_manager AdvancedProviderManager object
#' @param provider_name Name of provider
#' @param success Logical indicating success or failure
update_provider_health <- function(provider_manager, provider_name, success) {
  health <- provider_manager@provider_health[[provider_name]]
  if (is.null(health)) {
    return(invisible(provider_manager))
  }

  if (success) {
    health$last_success <- Sys.time()
    health$failures <- max(0, health$failures - 1) # Decay failures on success
  } else {
    health$failures <- health$failures + 1
    health$last_failure <- Sys.time()

    # Trip circuit breaker if threshold exceeded
    if (health$failures >= provider_manager@circuit_breaker_threshold) {
      health$circuit_open <- TRUE
      message("Circuit breaker tripped for provider: ", provider_name)
    }
  }

  provider_manager@provider_health[[provider_name]] <- health
  return(provider_manager)
}

#' Record Response Time
#'
#' @param provider_manager AdvancedProviderManager object
#' @param provider_name Name of provider
#' @param response_time Response time in seconds
record_response_time <- function(
  provider_manager,
  provider_name,
  response_time
) {
  health <- provider_manager@provider_health[[provider_name]]
  if (is.null(health)) {
    return()
  }

  # Keep only last 10 response times
  health$response_times <- c(health$response_times, response_time)
  if (length(health$response_times) > 10) {
    health$response_times <- tail(health$response_times, 10)
  }

  provider_manager@provider_health[[provider_name]] <- health
  return(provider_manager)
}

#' Get Cached Data
#'
#' @param provider_manager AdvancedProviderManager object
#' @param symbol Symbol to fetch
#' @param start_date Start date
#' @param end_date End date
#' @return Cached data or NULL
get_cached_data <- function(provider_manager, symbol, start_date, end_date) {
  cache_dir <- provider_manager@cache_config$dir %||% "cache"
  ttl_minutes <- provider_manager@cache_config$ttl_minutes %||% 60

  if (!dir.exists(cache_dir)) {
    return(NULL)
  }

  # Create cache key
  cache_key <- paste(symbol, start_date, end_date, sep = "_")
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))

  if (!file.exists(cache_file)) {
    return(NULL)
  }

  # Check if cache is still valid
  file_age_minutes <- as.numeric(difftime(
    Sys.time(),
    file.mtime(cache_file),
    units = "mins"
  ))
  if (file_age_minutes > ttl_minutes) {
    # Cache expired
    file.remove(cache_file)
    return(NULL)
  }

  tryCatch(
    {
      readRDS(cache_file)
    },
    error = function(e) {
      warning("Failed to read cache file: ", e$message)
      NULL
    }
  )
}
