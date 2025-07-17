#' Generic Functions for hedgeroot
#'
#' This file contains all S7 generic function definitions
#' to ensure they are available before methods are defined.

#' Fetch OHLCV Data Generic Function
#'
#' S7 generic function for fetching OHLCV data from different providers
#'
#' @param provider Data provider object
#' @param symbol Character symbol to fetch
#' @param start_date Start date for data
#' @param end_date End date for data
#' @param ... Additional arguments passed to provider-specific methods
#' @return data.table with OHLCV data
#' @export
fetch_ohlcv <- S7::new_generic("fetch_ohlcv", "provider")
