library(tradestationr)
library(data.table)

# Initialize client with environment variables
ts <- TradeStation$new(
  client_id = Sys.getenv("TS_CLIENT_ID"),
  client_secret = Sys.getenv("TS_CLIENT_SECRET"),
  token_path = ".tokens"
)

# Authenticate (will use cached tokens if available)
ts$authenticate()

# Get market data handler
md <- ts$market_data()

# Example 1: Get static historical data for AAPL
cat("\nFetching historical data for AAPL...\n")
aapl_hist <- md$get_bars(
  symbol = "AAPL",
  interval = "5min",
  start_date = Sys.Date() - 1,
  end_date = Sys.Date()
)
print(aapl_hist)

# Example 2: Get option chain data
cat("\nFetching AAPL option expirations...\n")
aapl_expirations <- md$get_option_expirations("AAPL")
print(aapl_expirations)

if (nrow(aapl_expirations) > 0) {
  # Get strikes for first expiration
  cat("\nFetching option strikes for first expiration...\n")
  first_expiry <- aapl_expirations$Expiration[1]
  strikes <- md$get_option_strikes("AAPL", first_expiry)
  print(strikes)
}

# Example 3: Stream real-time quotes
cat("\nStarting quote stream for AAPL...\n")
cat("Press Esc to stop streaming\n")

# Create in-memory quote storage
quotes <- data.table()

md$stream_quotes("AAPL", function(quote) {
  # Add timestamp
  quote[, timestamp := Sys.time()]
  
  # Append to in-memory storage
  quotes <- rbindlist(list(quotes, quote), fill = TRUE)
  
  # Print latest quote
  cat(sprintf("\rLast: %s @ %s", 
              quote$Last, 
              format(quote$timestamp, "%H:%M:%S")))
})

# After stopping stream, show summary
cat("\n\nQuote summary:\n")
print(quotes[, .(
  min_price = min(Last),
  max_price = max(Last),
  n_quotes = .N
), by = Symbol]) 