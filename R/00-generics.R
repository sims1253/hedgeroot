#' Generic Functions for hedgeroot
#'
#' This file contains all S7 generic function definitions
#' to ensure they are available before methods are defined.

#' Fetch OHLCV Data Generic Function
#'
#' S7 generic function for fetching OHLCV data from different providers.
#' The specific arguments (symbol, start_date, end_date, etc.) are passed 
#' through ... and documented in the individual method implementations.
#'
#' @param provider Data provider object
#' @param ... Arguments passed to provider-specific methods (typically symbol, start_date, end_date)
#' @return data.table with OHLCV data
#' @export
fetch_ohlcv <- S7::new_generic("fetch_ohlcv", "provider")
