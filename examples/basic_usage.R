library(tradestationr)  # When installed via remotes::install_github
# For development, use:
library(R6)
library(checkmate)
library(httr2)
library(logger)
library(data.table)
library(jsonlite)
library(RSQLite)   # Adding this as it's used in other parts

source("R/auth.R")
source("R/market_data.R")
source("R/tradestation.R")

# Initialize client with path to .secrets directory
ts <- TradeStation$new(secrets_dir = ".secrets")

# Authenticate (will use cached tokens if available)
ts$authenticate()

# Check authentication status
if (!ts$is_authenticated()) {
  stop("Authentication failed")
}

# Get historical bar data
bars <- ts$market_data()$get_bars(
  symbol = "AAPL",
  interval = "1min",
  start_date = as.Date("2024-01-23"),  # Use explicit dates
  end_date = as.Date("2024-01-26")     # Use a single day in the past
)

# Display first few bars
cat("\nHistorical bars for AAPL:\n")
print(head(bars), topn = 6)

# Get symbol details
details <- ts$market_data()$get_symbol_details("AAPL")
cat("\nSymbol details for AAPL:\n")
print(details, nrows = 1)

# Example of streaming quotes (runs until interrupted)
cat("\nStreaming real-time quotes for AAPL (press Ctrl+C to stop):\n")
cat("♥ = heartbeat, $ = quote\n\n")

# Keep track of quotes received
quote_count <- 0
start_time <- Sys.time()

# Create a data.table to store quotes
quotes <- data.table::data.table()

tryCatch({
  ts$market_data()$stream_quotes(
    symbols = "AAPL",
    callback = function(quote) {
      quote_count <<- quote_count + 1
      
      # Print a summary of the quote
      cat(sprintf("\rQuotes: %d | Last: %s | Bid: %s | Ask: %s | Volume: %s | Time: %s",
                  quote_count,
                  quote$Last,
                  quote$Bid,
                  quote$Ask,
                  quote$Volume,
                  format(as.POSIXct(quote$TradeTime), "%H:%M:%S")))
      flush.console()
    }
  )
}, error = function(e) {
  cat(sprintf("\nStreaming stopped: %s\n", e$message))
}, finally = {
  cat(sprintf("\nTotal quotes received: %d\n", quote_count))
}) 