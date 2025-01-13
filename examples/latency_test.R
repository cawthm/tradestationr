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
output_file <- "latency_stats.csv"

# Initialize stats tracking
stats <- data.table(
  timestamp = as.POSIXct(character()),
  symbol = character(),
  event = character(),
  latency_ms = numeric()
)
fwrite(stats, output_file)

# Function to record timing event
record_event <- function(symbol, event, start_time) {
  latency <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
  new_stat <- data.table(
    timestamp = Sys.time(),
    symbol = symbol,
    event = event,
    latency_ms = latency
  )
  fwrite(new_stat, output_file, append = TRUE)
  return(Sys.time())  # Return current time for next measurement
}

# Initialize client
ts <- TradeStation$new(secrets_dir = ".secrets")
ts$authenticate()

# Print diagnostic info
cat("Starting latency test\n")
cat("Symbols:", paste(symbols, collapse=", "), "\n")
cat("Stats file:", output_file, "\n")
cat("System info:\n")
cat("- R version:", R.version.string, "\n")
cat("- Platform:", Sys.info()["sysname"], Sys.info()["release"], "\n")
cat("- CPU cores:", parallel::detectCores(), "\n")
cat("\nPress Ctrl+C to stop\n\n")

# Stream quotes with detailed timing
ts$market_data()$stream_quotes(
  symbols = symbols,
  callback = function(dt) {
    if (dt$type == "quote") {
      # Record timing at each stage
      t0 <- dt$received_time  # Initial receipt time from MarketData class
      
      # Record CSV write timing
      t1 <- Sys.time()
      fwrite(dt, "market_data.csv", append = TRUE)
      t2 <- Sys.time()
      
      # Calculate latencies
      network_latency <- as.numeric(difftime(dt$received_time, dt$server_time, units = "secs")) * 1000
      processing_latency <- as.numeric(difftime(t1, dt$received_time, units = "secs")) * 1000
      write_latency <- as.numeric(difftime(t2, t1, units = "secs")) * 1000
      total_latency <- network_latency + processing_latency + write_latency
      
      # Record all latencies
      new_stats <- data.table(
        timestamp = Sys.time(),
        symbol = dt$symbol,
        event = c("network", "processing", "write", "total"),
        latency_ms = c(network_latency, processing_latency, write_latency, total_latency)
      )
      fwrite(new_stats, output_file, append = TRUE)
      
      # Print real-time stats
      cat(sprintf("\rSymbol: %-4s | Network: %4.1fms | Processing: %4.1fms | Write: %4.1fms | Total: %4.1fms",
                  dt$symbol, network_latency, processing_latency, write_latency, total_latency))
      flush.console()
    }
  }
) 