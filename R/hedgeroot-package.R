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

# Global variables for data.table NSE
utils::globalVariables(c(
  # Column names used in data.table NSE contexts
  "date",
  "symbol",
  "open",
  "high",
  "low",
  "close",
  "volume",
  "typical_price",
  "price_range",
  "price_change",
  "price_change_pct",
  "log_close",
  "log_return",
  "volume_ma_5",
  "volume_ratio",
  "true_range",
  "return_1d",
  "return_5d",
  "return_20d",
  "sma_5",
  "sma_10",
  "sma_20",
  "sma_50",
  "volatility_5d",
  "volatility_20d",
  "atr_14",
  "week_year",
  "month_year",
  # data.table functions
  "."
))
