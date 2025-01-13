#' @title Market Data Methods
#' @description Methods for retrieving historical and streaming market data
#' @importFrom data.table data.table as.data.table rbindlist
#' @importFrom jsonlite fromJSON parse_json
#' @importFrom logger log_info log_error
#' @importFrom checkmate assert_string assert_choice assert_function assert_class

#' @export
MarketData <- R6::R6Class(
  "MarketData",
  
  public = list(
    #' @description Initialize market data handler
    #' @param parent Parent TradeStation instance
    initialize = function(parent) {
      assert_class(parent, "TradeStation")
      private$.parent <- parent
      invisible(self)
    },
    
    #' @description Get historical bars for a symbol
    #' @param symbol The trading symbol
    #' @param interval Bar interval (1min, 5min, 15min, 1hour, 1day)
    #' @param start_date Start date (YYYY-MM-DD)
    #' @param end_date End date (YYYY-MM-DD)
    #' @return data.table of historical bars
    get_bars = function(symbol, interval = "1min", 
                       start_date = Sys.Date() - 1,
                       end_date = Sys.Date()) {
      assert_string(symbol)
      assert_choice(interval, c("1min", "5min", "15min", "1hour", "1day"))
      
      # Convert interval to numeric minutes
      interval_map <- c(
        "1min" = 1,
        "5min" = 5,
        "15min" = 15,
        "1hour" = 60,
        "1day" = 1440
      )
      interval_minutes <- interval_map[interval]
      
      path <- sprintf("/marketdata/barcharts/%s", symbol)
      
      # Format dates as ISO 8601 with time component
      start_datetime <- format(as.POSIXct(start_date), "%Y-%m-%dT00:00:00Z")
      
      # If end_date is today, use current time instead of end of day
      if (as.Date(end_date) == Sys.Date()) {
        end_datetime <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
      } else {
        end_datetime <- format(as.POSIXct(end_date), "%Y-%m-%dT23:59:59Z")
      }
      
      resp <- private$.request(
        path = path,
        query = list(
          interval = interval_minutes,
          unit = "Minute",
          firstdate = start_datetime,
          lastdate = end_datetime
        )
      )
      
      data <- fromJSON(rawToChar(resp$body))
      private$.process_bar_data(data$Bars)
    },
    
    #' @description Stream real-time bars
    #' @param symbol The trading symbol
    #' @param interval Bar interval (1min, 5min)
    #' @param callback Function to handle incoming bars
    stream_bars = function(symbol, interval = "1min", callback) {
      assert_string(symbol)
      assert_choice(interval, c("1min", "5min"))
      assert_function(callback)
      
      # Convert interval to numeric minutes
      interval_map <- c("1min" = 1, "5min" = 5)
      interval_minutes <- interval_map[interval]
      
      path <- sprintf("/marketdata/stream/barcharts/%s", symbol)
      
      private$.request(
        path = path,
        query = list(interval = interval_minutes),
        stream = TRUE,
        callback = function(chunk) {
          private$.process_stream_chunk(chunk, callback)
        }
      )
    },
    
    #' @description Get symbol details
    #' @param symbol The trading symbol
    #' @return data.table of symbol details
    get_symbol_details = function(symbol) {
      assert_string(symbol)
      
      path <- sprintf("/marketdata/symbols/%s", symbol)
      resp <- private$.request(path = path)
      as.data.table(fromJSON(rawToChar(resp$body)))
    },
    
    #' @description Get option expirations for a symbol
    #' @param symbol The underlying symbol
    #' @return data.table of option expirations
    get_option_expirations = function(symbol) {
      assert_string(symbol)
      
      path <- sprintf("/marketdata/options/%s/expirations", symbol)
      resp <- private$.request(path = path)
      as.data.table(fromJSON(rawToChar(resp$body)))
    },
    
    #' @description Get option strikes for a symbol and expiration
    #' @param symbol The underlying symbol
    #' @param expiration The expiration date
    #' @return data.table of option strikes
    get_option_strikes = function(symbol, expiration) {
      assert_string(symbol)
      assert_string(expiration)
      
      path <- sprintf("/marketdata/options/%s/strikes", symbol)
      resp <- private$.request(
        path = path,
        query = list(expiration = expiration)
      )
      as.data.table(fromJSON(rawToChar(resp$body)))
    },
    
    #' @description Get option spread types
    #' @return data.table of available option spread types
    get_option_spread_types = function() {
      path <- "/marketdata/options/spreads/types"
      resp <- private$.request(path = path)
      as.data.table(fromJSON(rawToChar(resp$body)))
    },
    
    #' @description Calculate option risk/reward metrics
    #' @param symbol The option symbol
    #' @param price The price to analyze
    #' @return data.table of risk/reward metrics
    get_option_risk_reward = function(symbol, price) {
      assert_string(symbol)
      assert_number(price)
      
      path <- sprintf("/marketdata/options/%s/risk-reward", symbol)
      resp <- private$.request(
        path = path,
        method = "POST",
        body = list(price = price)
      )
      as.data.table(fromJSON(rawToChar(resp$body)))
    },
    
    #' @description Stream option chain data
    #' @param symbol The underlying symbol
    #' @param strikes Number of strikes (default 10)
    #' @param callback Function to handle incoming option chain data
    stream_option_chain = function(symbol, strikes = 10, callback) {
      assert_string(symbol)
      assert_function(callback)
      
      path <- sprintf("/marketdata/stream/options/%s/chain", symbol)
      
      private$.request(
        path = path,
        query = list(strikes = strikes),
        stream = TRUE,
        callback = function(chunk) {
          private$.process_stream_chunk(chunk, callback)
        }
      )
    },
    
    #' @description Stream option quotes
    #' @param symbols Vector of option symbols
    #' @param callback Function to handle incoming option quotes
    stream_option_quotes = function(symbols, callback) {
      assert_character(symbols)
      assert_function(callback)
      
      symbols_str <- paste(symbols, collapse = ",")
      path <- sprintf("/marketdata/stream/options/quotes/%s", symbols_str)
      
      private$.request(
        path = path,
        stream = TRUE,
        callback = function(chunk) {
          private$.process_stream_chunk(chunk, callback)
        }
      )
    },
    
    #' @description Get quote snapshots for symbols
    #' @param symbols Vector of trading symbols
    #' @return data.table of quote snapshots
    get_quote_snapshots = function(symbols) {
      assert_character(symbols)
      
      symbols_str <- paste(symbols, collapse = ",")
      path <- sprintf("/marketdata/quotes/%s", symbols_str)
      
      resp <- private$.request(path = path)
      as.data.table(fromJSON(rawToChar(resp$body))$Quotes)
    },
    
    #' @description Stream real-time quotes
    #' @param symbols Vector of trading symbols
    #' @param callback Function to handle incoming quotes. The callback receives a data.table with standardized columns:
    #' \itemize{
    #'   \item symbol: Character - Trading symbol
    #'   \item received_time: POSIXct - When we received the update (local time)
    #'   \item trade_time: POSIXct - Time of the last trade (UTC)
    #'   \item server_time: POSIXct - Time from TradeStation server (UTC, if available)
    #'   \item last: Numeric - Last trade price
    #'   \item bid: Numeric - Best bid price
    #'   \item ask: Numeric - Best ask price
    #'   \item bid_size: Integer - Size at best bid
    #'   \item ask_size: Integer - Size at best ask
    #'   \item volume: Integer - Trading volume
    #'   \item type: Character - Update type ("quote", "heartbeat", etc)
    #' }
    stream_quotes = function(symbols, callback) {
      assert_character(symbols)
      assert_function(callback)
      
      symbols_str <- paste(symbols, collapse = ",")
      path <- sprintf("/marketdata/stream/quotes/%s", symbols_str)
      
      cat(sprintf("\nInitiating quote stream for symbols: %s\n", symbols_str))
      
      # Initialize buffer for incomplete data
      buffer <- ""
      
      # Stream handler that processes JSON objects delimited by newlines
      private$.request(
        path = path,
        stream = TRUE,
        callback = function(chunk) {
          # Handle both raw and character data
          data <- if (is.raw(chunk)) {
            rawToChar(chunk)
          } else if (is.character(chunk)) {
            chunk
          } else {
            cat("Unexpected data type:", class(chunk), "\n")
            return(TRUE)
          }
          
          # Add new data to buffer
          if (!is.null(data) && nchar(data) > 0) {
            buffer <<- paste0(buffer, data)
            
            # Split buffer on newlines
            lines <- strsplit(buffer, "\n")[[1]]
            
            # Process all complete lines except the last one (which might be incomplete)
            if (length(lines) > 1) {
              for (i in 1:(length(lines)-1)) {
                line <- lines[i]
                if (nchar(line) > 0) {  # Skip empty lines
                  tryCatch({
                    # Parse JSON
                    update <- fromJSON(line)
                    
                    # Get current time immediately
                    received_time <- Sys.time()
                    
                    # Create standardized data.table
                    if (!is.null(update$Heartbeat)) {
                      dt <- data.table(
                        symbol = NA_character_,
                        received_time = received_time,
                        trade_time = as.POSIXct(NA),
                        server_time = as.POSIXct(update$Timestamp, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                        last = NA_real_,
                        bid = NA_real_,
                        ask = NA_real_,
                        bid_size = NA_integer_,
                        ask_size = NA_integer_,
                        volume = NA_integer_,
                        type = "heartbeat"
                      )
                    } else {
                      # Handle partial updates
                      dt <- data.table(
                        symbol = update$Symbol,
                        received_time = received_time,
                        trade_time = if (!is.null(update$TradeTime)) 
                          as.POSIXct(update$TradeTime, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC") 
                          else as.POSIXct(NA),
                        server_time = if (!is.null(update$ServerTime))  # TradeStation might not provide this
                          as.POSIXct(update$ServerTime, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
                          else as.POSIXct(NA),
                        last = if (!is.null(update$Last)) as.numeric(update$Last) else NA_real_,
                        bid = if (!is.null(update$Bid)) as.numeric(update$Bid) else NA_real_,
                        ask = if (!is.null(update$Ask)) as.numeric(update$Ask) else NA_real_,
                        bid_size = if (!is.null(update$BidSize)) as.integer(update$BidSize) else NA_integer_,
                        ask_size = if (!is.null(update$AskSize)) as.integer(update$AskSize) else NA_integer_,
                        volume = if (!is.null(update$Volume)) as.integer(update$Volume) else NA_integer_,
                        type = "quote"
                      )
                    }
                    
                    # Pass standardized data.table to callback
                    callback(dt)
                  }, error = function(e) {
                    cat("Error processing JSON:", e$message, "\n")
                    cat("Line:", line, "\n")
                  })
                }
              }
              # Keep the last (potentially incomplete) line in the buffer
              buffer <<- lines[length(lines)]
            }
          }
          
          TRUE  # Continue streaming
        }
      )
    }
  ),
  
  private = list(
    .parent = NULL,
    
    # Convert bar data to data.table
    .process_bar_data = function(data) {
      dt <- as.data.table(data)
      if (nrow(dt) > 0) {
        # Convert timestamp to POSIXct
        if ("TimeStamp" %in% names(dt)) {
          dt[, TimeStamp := as.POSIXct(TimeStamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")]
          setkey(dt, TimeStamp)
        } else if ("DT" %in% names(dt)) {
          dt[, TimeStamp := as.POSIXct(DT, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")]
          setkey(dt, TimeStamp)
        }
      }
      dt
    },
    
    # Process streaming data chunks
    .process_stream_chunk = function(chunk, callback) {
      tryCatch({
        # Handle partial JSON chunks
        data <- parse_json(chunk)
        if (!is.null(data)) {
          dt <- as.data.table(data)
          callback(dt)
        }
      }, error = function(e) {
        log_error(sprintf("Error processing stream chunk: %s", e$message))
      })
    },
    
    # Helper to access parent's request method
    .request = function(...) {
      private$.parent$request(...)
    }
  )
) 