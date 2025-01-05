#' @title Market Data Methods
#' @description Methods for retrieving historical and streaming market data
#' @importFrom data.table data.table as.data.table rbindlist
#' @importFrom jsonlite fromJSON parse_json
#' @importFrom logger log_info log_error
#' @importFrom checkmate assert_string assert_choice assert_function assert_character

#' @export
MarketData <- R6::R6Class(
  "MarketData",
  
  private = list(
    .parent = NULL,
    
    # Convert bar data to data.table
    .process_bar_data = function(data) {
      dt <- as.data.table(data)
      if (nrow(dt) > 0) {
        # Convert timestamp to POSIXct
        dt[, Timestamp := as.POSIXct(Timestamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")]
        setkey(dt, Timestamp)
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
    }
  ),
  
  public = list(
    #' @description Initialize market data handler
    #' @param parent Parent TradeStation instance
    initialize = function(parent) {
      private$.parent <- parent
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
      
      path <- sprintf("/marketdata/barcharts/%s", symbol)
      
      resp <- private$.parent$.__request(
        path = path,
        query = list(
          interval = interval,
          startDate = format(as.Date(start_date), "%Y-%m-%d"),
          endDate = format(as.Date(end_date), "%Y-%m-%d")
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
      
      path <- sprintf("/marketdata/stream/barcharts/%s", symbol)
      
      private$.parent$.__request(
        path = path,
        query = list(interval = interval),
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
      resp <- private$.parent$.__request(path = path)
      as.data.table(fromJSON(rawToChar(resp$body)))
    },
    
    #' @description Get option expirations for a symbol
    #' @param symbol The underlying symbol
    #' @return data.table of option expirations
    get_option_expirations = function(symbol) {
      assert_string(symbol)
      
      path <- sprintf("/marketdata/options/%s/expirations", symbol)
      resp <- private$.parent$.__request(path = path)
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
      resp <- private$.parent$.__request(
        path = path,
        query = list(expiration = expiration)
      )
      as.data.table(fromJSON(rawToChar(resp$body)))
    },
    
    #' @description Get option spread types
    #' @return data.table of available option spread types
    get_option_spread_types = function() {
      path <- "/marketdata/options/spreads/types"
      resp <- private$.parent$.__request(path = path)
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
      resp <- private$.parent$.__request(
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
      
      private$.parent$.__request(
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
      
      private$.parent$.__request(
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
      
      resp <- private$.parent$.__request(path = path)
      as.data.table(fromJSON(rawToChar(resp$body))$Quotes)
    },
    
    #' @description Stream real-time quotes
    #' @param symbols Vector of trading symbols
    #' @param callback Function to handle incoming quotes
    stream_quotes = function(symbols, callback) {
      assert_character(symbols)
      assert_function(callback)
      
      symbols_str <- paste(symbols, collapse = ",")
      path <- sprintf("/marketdata/stream/quotes/%s", symbols_str)
      
      private$.parent$.__request(
        path = path,
        stream = TRUE,
        callback = function(chunk) {
          private$.process_stream_chunk(chunk, callback)
        }
      )
    }
  )
) 