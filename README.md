
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hedgeroot

<!-- badges: start -->

[![License](https://img.shields.io/badge/License-AGPL--3-blue.svg)](LICENSE)
[![R-CMD-check](https://github.com/sims1253/hedgeroot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sims1253/hedgeroot/actions/workflows/R-CMD-check.yaml)
[![Tests](https://github.com/sims1253/hedgeroot/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/sims1253/hedgeroot/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sims1253/hedgeroot/graph/badge.svg)](https://app.codecov.io/gh/sims1253/hedgeroot)
[![GH-Pages](https://github.com/sims1253/hedgeroot/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/sims1253/hedgeroot/actions/workflows/pkgdown.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

hedgeroot is a data management package for financial data. It handles
data fetching, validation, and storage with support for multiple data
providers.

## Features

- Multi-source data integration (Alpha Vantage, Interactive Brokers)
- Provider failover with circuit breakers and health monitoring
- Built on data.table for performance
- Data validation with quality scoring and anomaly detection
- S7 object system for type safety

## Installation

You can install the development version of hedgeroot from GitHub:

``` r
# Install devtools if you haven't already
install.packages("devtools")

# Install hedgeroot
devtools::install_github("sims1253/hedgeroot")
```

## API Token Setup

To use hedgeroot with real market data, you’ll need to set up API tokens
for your chosen data providers. The package uses environment variables
to securely manage these credentials.

### Alpha Vantage API Key

Alpha Vantage provides free and premium financial data. To get started:

1.  Sign up for a free API key at [Alpha
    Vantage](https://www.alphavantage.co/support/#api-key)
2.  Set the `ALPHAVANTAGE_API_KEY` environment variable using one of the
    methods below

### Free Tier Limitations

**Alpha Vantage Free Tier:**

- 5 API requests per minute
- 25 API requests per day
- Unadjusted daily or intraday time series data only
- No real-time intraday data

The package automatically respects these rate limits. For higher usage,
consider upgrading to Alpha Vantage’s premium tiers.

### Verifying Setup

Test your API token setup:

``` r
library(hedgeroot)

# This will use your environment variable automatically
provider <- create_alpha_vantage_provider()

# Test with a simple data fetch
data <- fetch_ohlcv(provider, "AAPL", Sys.Date() - 30, Sys.Date())
```

### Testing with Real APIs

The package includes integration tests that make real API calls. To
conserve your API quota:

**Default behavior (recommended):**

``` bash
# Tests skip real API calls by default
testthat::test_dir("tests/testthat")
```

**Enable real API testing:**

``` bash
# Set both environment variables to enable real API calls
export ALPHAVANTAGE_API_KEY="your_api_key"
export HEDGEROOT_ENABLE_API_TESTS="true"

# Run tests - will make real API calls (uses ~5 requests)
testthat::test_dir("tests/testthat")
```

The integration tests are optimized to minimize API usage, but will
still consume several requests from your daily quota when enabled.

## Basic Usage

### Simple Data Provider

``` r
library(hedgeroot)

# Create a provider
provider <- create_alpha_vantage_provider(api_key = "your_api_key")

# Fetch OHLCV data
data <- fetch_ohlcv(provider, "AAPL", "2023-01-01", "2023-12-31")
```

### Advanced Provider Management

``` r
# Create multiple providers with failover
providers <- list(
  create_alpha_vantage_provider(api_key = "your_av_key"),
  create_ib_provider(host = "127.0.0.1", port = 7497)
)

# Create advanced manager with circuit breaker
manager <- create_advanced_provider_manager(
  providers = providers,
  failover_strategy = "priority",
  circuit_breaker_threshold = 3,
  circuit_breaker_window = 15
)

# Fetch data with automatic failover
data <- fetch_ohlcv(manager, "AAPL", "2023-01-01", "2023-12-31")
```

### Data Validation

``` r
# Validate fetched data
validation <- validate_data(data)

# Check validation results
if (validation@is_valid) {
  cat("Data quality score:", validation@quality_score, "\n")
} else {
  cat("Validation errors:", validation@errors, "\n")
}
```

## Architecture

### Provider System

hedgeroot uses a modular provider system supporting:

- **Alpha Vantage**: Free and paid tiers for equity and forex data
- **Interactive Brokers**: Professional trading platform integration  
- **Custom Providers**: Extensible architecture for new data sources

### Failover Strategy

- **Priority-based**: Providers tried in order of priority
- **Health-based**: Providers selected based on historical performance
- **Circuit Breaker**: Failing providers temporarily disabled
- **Automatic Recovery**: Providers re-enabled after cooldown period

## Configuration

### Environment Variables

``` bash
# Alpha Vantage API key
export ALPHAVANTAGE_API_KEY="your_api_key"

# IB Gateway settings
export IB_HOST="127.0.0.1"
export IB_PORT="7497"
export IB_CLIENT_ID="1"
```

## Contributing

1.  Fork the repository
2.  Create a feature branch
3.  Make your changes with tests
4.  Ensure all tests pass with `devtools::check()`
5.  Submit a pull request

## License

This project is licensed under the AGPL-3 License. See the LICENSE file
for details.
