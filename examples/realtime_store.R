library(R6)
library(checkmate)
library(httr2)
library(logger)
library(data.table)
library(jsonlite)

source("R/auth.R")
source("R/market_data.R")
source("R/tradestation.R")

# Initialize store
quote_store <- data.table(
  symbol = character(),
  timestamp = as.POSIXct(character()),
  last = numeric(),
  bid = numeric(),
  ask = numeric(),
  bid_size = integer(),
  ask_size = integer(),
  volume = integer(),
  type = character()
)
setkey(quote_store, symbol, timestamp)

# Initialize stats
stats <- list(
  start_time = Sys.time(),
  updates = 0,
  quotes = 0,
  heartbeats = 0
)

# Initialize client and authenticate
ts <- TradeStation$new(secrets_dir = ".secrets")
ts$authenticate()

if (!ts$is_authenticated()) {
  stop("Authentication failed")
}

# Stream real-time quotes
cat("\nStarting real-time quote stream for AAPL...\n")
cat("Stats will be printed every 5 seconds\n\n")

# Create a function to print stats
print_stats <- function() {
  runtime <- difftime(Sys.time(), stats$start_time, units = "secs")
  cat("\nStream Statistics:")
  cat("\nRuntime:", format(runtime, digits=2), "seconds")
  cat("\nTotal Updates:", stats$updates)
  cat("\nQuotes:", stats$quotes)
  cat("\nHeartbeats:", stats$heartbeats)
  cat("\nUpdates/sec:", format(stats$updates/as.numeric(runtime), digits=2))
  
  if (nrow(quote_store) > 0) {
    latest <- quote_store[type == "quote"][.N]
    cat("\n\nLatest Quote:")
    cat("\nSymbol:", latest$symbol)
    cat("\nTime:", format(latest$timestamp, "%H:%M:%S"))
    cat("\nLast:", latest$last)
    cat("\nBid:", latest$bid)
    cat("\nAsk:", latest$ask)
    cat("\nVolume:", latest$volume)
  }
  cat("\n")
}

# Set up timer to print stats every 5 seconds
last_stats <- Sys.time()

# Start streaming
ts$market_data()$stream_quotes(
  symbols = "AAPL",
  callback = function(dt) {
    # Update store
    quote_store <- rbindlist(list(quote_store, dt))
    setkey(quote_store, symbol, timestamp)
    
    # Update stats
    stats$updates <<- stats$updates + 1
    if (dt$type[1] == "quote") {
      stats$quotes <<- stats$quotes + 1
    } else {
      stats$heartbeats <<- stats$heartbeats + 1
    }
    
    # Print stats every 5 seconds
    now <- Sys.time()
    if (difftime(now, last_stats, units="secs") >= 5) {
      print_stats()
      last_stats <<- now
    }
  }
) 