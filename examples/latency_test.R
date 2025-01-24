library(R6)
library(checkmate)
library(httr2)
library(logger)
library(data.table)
library(jsonlite)
library(microbenchmark)  # For precise timing
library(RSQLite)         # For SQLite support

source("R/auth.R")
source("R/market_data.R")
source("R/tradestation.R")

# Configuration
symbols <- c("AAPL", "SPY", "QQQ", "TSLA", "GS", "MSTR", "NVDA", "AMZN", "NFLX", "META", "COIN", "GOOG")
db_file <- "market_data.sqlite"
detailed_log <- "latency_detailed.log"

# Initialize SQLite database
cat("Initializing SQLite database...\n")
con <- dbConnect(SQLite(), db_file)

# Create tables if they don't exist
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS latency_stats (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp DATETIME,
    symbol TEXT,
    event TEXT,
    latency_ms REAL,
    request_id TEXT,
    stage TEXT,
    msg_size INTEGER,
    exchange_time DATETIME,
    broker_time DATETIME,
    our_time DATETIME
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS market_data (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp DATETIME,
    symbol TEXT,
    last_price REAL,
    bid_price REAL,
    ask_price REAL,
    volume INTEGER,
    trade_time DATETIME,
    msg_type TEXT
  )
")

# Create indices for faster querying
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_latency_timestamp ON latency_stats(timestamp)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_latency_symbol ON latency_stats(symbol)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_market_timestamp ON market_data(timestamp)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_market_symbol ON market_data(symbol)")

# Initialize detailed logging
log_conn <- file(detailed_log, "w")
writeLines(paste("Latency Test Started:", Sys.time()), log_conn)

# Enhanced timing function with nanosecond precision
get_precise_time <- function() {
  unclass(Sys.time()) * 1e9
}

# Define NULL coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Function to record detailed timing event
record_event <- function(symbol, event, start_time, request_id = NULL, stage = NULL, details = NULL) {
  current_time <- get_precise_time()
  latency <- (current_time - start_time) / 1e6  # Convert to milliseconds
  
  # Prepare data for SQLite
  data <- c(
    format(as.POSIXct(current_time/1e9, origin="1970-01-01"), "%Y-%m-%d %H:%M:%S"),
    symbol,
    event,
    latency,
    request_id %||% NA_character_,
    stage %||% NA_character_,
    if (!is.null(details$msg_size)) details$msg_size else NA_integer_,
    if (!is.null(details$exchange_time)) details$exchange_time else NA_character_,
    if (!is.null(details$broker_time)) details$broker_time else NA_character_,
    if (!is.null(details$our_time)) details$our_time else NA_character_
  )
  
  # Insert into SQLite using unnamed parameters
  dbExecute(con,
    "INSERT INTO latency_stats 
     (timestamp, symbol, event, latency_ms, request_id, stage, msg_size, 
      exchange_time, broker_time, our_time)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = data
  )
  
  # Log detailed information
  if (!is.null(details)) {
    writeLines(sprintf("[%s] %s - %s: %s", 
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S.%OS3"),
                      request_id,
                      event,
                      toJSON(details)),
              log_conn)
  }
  
  return(current_time)
}

# Initialize client with enhanced debugging
ts <- TradeStation$new(secrets_dir = ".secrets")

# Track authentication timing
auth_start <- get_precise_time()
ts$authenticate()
record_event("SYSTEM", "authentication", auth_start, "AUTH001", "complete")

if (!ts$is_authenticated()) {
  stop("Authentication failed")
}

# Print diagnostic info
cat("Starting enhanced latency test\n")
cat("Symbols:", paste(symbols, collapse=", "), "\n")
cat("Database:", db_file, "\n")
cat("Detailed log:", detailed_log, "\n")
cat("System info:\n")
cat("- R version:", R.version.string, "\n")
cat("- Platform:", Sys.info()["sysname"], Sys.info()["release"], "\n")
cat("- CPU cores:", parallel::detectCores(), "\n")
cat("- Memory available:", format(memory.size(max=TRUE), units="auto"), "MB\n")
cat("\nPress Ctrl+C to stop\n\n")

# Enhanced quote streaming with detailed timing
msg_counter <- 0
ts$market_data()$stream_quotes(
  symbols = symbols,
  callback = function(dt) {
    msg_counter <<- msg_counter + 1
    request_id <- sprintf("REQ%06d", msg_counter)
    
    if (dt$type == "quote") {
      # Record initial receipt time with nanosecond precision
      t0 <- get_precise_time()
      
      # Parse and process timing
      t1 <- get_precise_time()
      
      # Record to SQLite using unnamed parameters
      dbExecute(con,
        "INSERT INTO market_data 
         (timestamp, symbol, last_price, bid_price, ask_price, volume, trade_time, msg_type)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = c(
          format(dt$received_time, "%Y-%m-%d %H:%M:%S"),
          dt$symbol,
          dt$last,
          dt$bid,
          dt$ask,
          dt$volume,
          format(dt$trade_time, "%Y-%m-%d %H:%M:%S"),
          dt$type
        )
      )
      t2 <- get_precise_time()
      
      # Calculate detailed latencies
      exchange_time <- as.POSIXct(dt$TradeTime, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
      exchange_ns <- as.numeric(exchange_time) * 1e9
      
      # Latency calculations
      wire_latency <- (t0 - unclass(dt$server_time) * 1e9) / 1e6  # Broker to us
      broker_latency <- (unclass(dt$server_time) * 1e9 - exchange_ns) / 1e6  # Exchange to broker
      total_latency <- (t0 - exchange_ns) / 1e6  # Total exchange to us
      parse_latency <- (t1 - t0) / 1e6
      write_latency <- (t2 - t1) / 1e6
      
      # Record all stages
      stages <- c("wire", "parse", "write", "total", "broker")
      latencies <- c(wire_latency, parse_latency, write_latency, total_latency, broker_latency)
      
      for (i in seq_along(stages)) {
        record_event(
          symbol = dt$symbol,
          event = stages[i],
          start_time = t0,
          request_id = request_id,
          stage = stages[i],
          details = list(
            latency = latencies[i],
            msg_size = nchar(toJSON(dt)),
            exchange_time = format(exchange_time, "%Y-%m-%d %H:%M:%OS3"),
            broker_time = format(as.POSIXct(dt$server_time, origin="1970-01-01"), "%Y-%m-%d %H:%M:%OS3"),
            our_time = format(as.POSIXct(t0/1e9, origin="1970-01-01"), "%Y-%m-%d %H:%M:%OS3")
          )
        )
      }
      
      # Print real-time stats with enhanced metrics
      cat(sprintf("\r[%s] %s | Ex→Br: %4.1fms | Br→Us: %4.1fms | Total: %4.1fms | Size: %d bytes",
                  format(Sys.time(), "%H:%M:%S"),
                  sprintf("%-4s", dt$symbol),
                  broker_latency,
                  wire_latency,
                  total_latency,
                  nchar(toJSON(dt))))
      flush.console()
    } else if (dt$type == "heartbeat") {
      record_event(
        symbol = "SYSTEM",
        event = "heartbeat",
        start_time = get_precise_time(),
        request_id = request_id,
        stage = "complete",
        details = list(
          timestamp = format(dt$server_time, "%Y-%m-%d %H:%M:%OS3")
        )
      )
    }
  }
)

# Cleanup
close(log_conn)
dbDisconnect(con) 
