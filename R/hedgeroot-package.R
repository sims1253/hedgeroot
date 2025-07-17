#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data .env
#' @importFrom data.table := shift frollmean frollapply first last
#' @importFrom stats sd
#' @importFrom utils tail
## usethis namespace: end
NULL

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Global variables for rlang/purrr functions (standalone)
utils::globalVariables(c(
  "as_function", "global_env", "is_logical", "is_true", "inject", "set_names"
))

#' Fetch OHLCV Data
#'
#' Generic function to fetch OHLCV data from providers
#'
#' @param provider Provider object
#' @param symbol Character symbol to fetch
#' @param start_date Start date for data
#' @param end_date End date for data
#' @param ... Additional arguments
#' @return data.table with OHLCV data
#' @export
fetch_ohlcv <- function(provider, symbol, start_date, end_date, ...) {
  UseMethod("fetch_ohlcv")
}
