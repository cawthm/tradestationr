library(R6)
library(checkmate)
library(httr2)
library(logger)
library(data.table)
library(jsonlite)

source("R/auth.R")
source("R/market_data.R")
source("R/tradestation.R")

# Configuration
symbols <- c("AAPL", "SPY", "QQQ")
output_file <- "market_data.csv"

# Initialize client with secrets directory path
ts <- TradeStation$new(secrets_dir = ".secrets")
ts$authenticate()

# Create empty output file with headers
dt <- data.table(
  symbol = character(),
  received_time = as.POSIXct(character()),
  trade_time = as.POSIXct(character()),
  server_time = as.POSIXct(character()),
  last = numeric(),
  bid = numeric(),
  ask = numeric(),
  bid_size = integer(),
  ask_size = integer(),
  volume = integer(),
  type = character()
)
fwrite(dt, output_file)

# Start streaming
cat("Starting stream for symbols:", paste(symbols, collapse=", "), "\n")
cat("Writing to:", output_file, "\n")
cat("Press Ctrl+C to stop\n\n")

# Stream quotes using the MarketData class
ts$market_data()$stream_quotes(
  symbols = symbols,
  callback = function(dt) {
    # Append to CSV file
    fwrite(dt, output_file, append = TRUE)
    
    # Print status based on type
    if (dt$type == "heartbeat") {
      cat("♥")  # Visual heartbeat indicator
    } else {
      cat(sprintf("\nQuote: %s @ %s | Bid: %s | Ask: %s | Volume: %s\n",
                  dt$symbol, dt$last, dt$bid, dt$ask, dt$volume))
    }
    flush.console()
  }
) 