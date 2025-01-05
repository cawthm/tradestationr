# tradestationr

A high-performance R interface to the TradeStation API, focusing on market data retrieval and storage. This package is designed with speed and efficiency in mind, utilizing {data.table} for in-memory operations and SQLite for persistent storage.

## Features

- **Fast Data Operations**: Uses {data.table} for blazing fast in-memory data manipulation
- **Persistent Storage**: SQLite-based storage for efficient data persistence and querying
- **Comprehensive API Coverage**:
  - OAuth2 authentication with token caching
  - Historical market data retrieval
  - Real-time streaming market data
  - Options chain data
- **Robust Error Handling**: Graceful handling of API rate limits and connection issues
- **Performance Monitoring**: Built-in tools to measure and optimize performance

## Installation

```r
# Install from GitHub
remotes::install_github("yourusername/tradestationr")
```

## Prerequisites

1. TradeStation Developer Account
2. API Client ID and Secret
3. R 4.0.0 or later

## Authentication

Before using the package, set up your TradeStation API credentials:

```r
# In your .Renviron file:
TS_CLIENT_ID="your_client_id"
TS_CLIENT_SECRET="your_client_secret"
```

Or set them in your script (not recommended for production):

```r
Sys.setenv(
  TS_CLIENT_ID = "your_client_id",
  TS_CLIENT_SECRET = "your_client_secret"
)
```

## Example Usage

Here's a complete example showing authentication, static data retrieval, and streaming:

```r
library(tradestationr)
library(data.table)

# Initialize client
ts <- TradeStation$new()  # Uses environment variables by default
ts$authenticate()         # Will cache tokens to .tokens file

# Get market data handler
md <- ts$market_data()

# Get historical bars
aapl_hist <- md$get_bars(
  symbol = "AAPL",
  interval = "5min",
  start_date = Sys.Date() - 1
)

# Get option chain data
expirations <- md$get_option_expirations("AAPL")
strikes <- md$get_option_strikes("AAPL", expirations$Expiration[1])

# Stream real-time quotes
quotes <- data.table()
md$stream_quotes("AAPL", function(quote) {
  quotes <- rbindlist(list(quotes, quote), fill = TRUE)
  print(quote)
})
```

For more examples, see the `examples/` directory.

## Performance Tips

1. Use data.table operations instead of base R
2. Batch API requests when possible
3. Index frequently queried columns in SQLite
4. Use appropriate data types (especially for timestamps)
5. Stream data directly to storage for large datasets

## Token Management

The package handles OAuth2 token management automatically:

- Tokens are cached to disk (default: `.tokens` file)
- Access tokens are automatically refreshed when expired
- Refresh tokens are persisted for long-term use
- Token state is managed securely and thread-safely

## Contributing

Contributions are welcome! Please read the contributing guidelines before submitting pull requests.

## License

MIT

## Acknowledgments

- TradeStation API Team
- {data.table} developers
- R community 