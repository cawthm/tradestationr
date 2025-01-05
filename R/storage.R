#' @title Storage Handler
#' @description SQLite-based persistent storage for market data
#' @importFrom RSQLite dbConnect dbDisconnect dbWriteTable dbReadTable SQLite
#' @importFrom data.table as.data.table setkey
#' @importFrom logger log_info log_error

#' @export
Storage <- R6::R6Class(
  "Storage",
  
  private = list(
    .conn = NULL,
    .db_path = NULL,
    
    # Initialize database tables
    .init_tables = function() {
      # Bars table
      if (!dbExistsTable(private$.conn, "bars")) {
        sql <- "
        CREATE TABLE bars (
          symbol TEXT NOT NULL,
          timestamp DATETIME NOT NULL,
          open REAL NOT NULL,
          high REAL NOT NULL,
          low REAL NOT NULL,
          close REAL NOT NULL,
          volume INTEGER NOT NULL,
          interval TEXT NOT NULL,
          PRIMARY KEY (symbol, timestamp, interval)
        )"
        dbExecute(private$.conn, sql)
        dbExecute(private$.conn, "CREATE INDEX idx_bars_timestamp ON bars(timestamp)")
      }
      
      # Options table
      if (!dbExistsTable(private$.conn, "options")) {
        sql <- "
        CREATE TABLE options (
          underlying TEXT NOT NULL,
          symbol TEXT NOT NULL,
          strike REAL NOT NULL,
          expiration DATE NOT NULL,
          type TEXT NOT NULL,
          bid REAL,
          ask REAL,
          last REAL,
          volume INTEGER,
          timestamp DATETIME NOT NULL,
          PRIMARY KEY (symbol, timestamp)
        )"
        dbExecute(private$.conn, sql)
        dbExecute(private$.conn, "CREATE INDEX idx_options_underlying ON options(underlying)")
        dbExecute(private$.conn, "CREATE INDEX idx_options_expiration ON options(expiration)")
      }
    }
  ),
  
  public = list(
    #' @description Initialize storage handler
    #' @param db_path Path to SQLite database file
    initialize = function(db_path = "market_data.sqlite") {
      private$.db_path <- db_path
      private$.conn <- dbConnect(SQLite(), db_path)
      private$.init_tables()
      log_info(sprintf("Storage initialized with database: %s", db_path))
    },
    
    #' @description Store historical bars
    #' @param symbol Trading symbol
    #' @param bars data.table of bar data
    #' @param interval Bar interval
    store_bars = function(symbol, bars, interval) {
      assert_string(symbol)
      assert_data_table(bars)
      assert_choice(interval, c("1min", "5min", "15min", "1hour", "1day"))
      
      bars[, `:=`(
        symbol = symbol,
        interval = interval
      )]
      
      tryCatch({
        dbWriteTable(
          private$.conn, 
          "bars", 
          as.data.frame(bars), 
          append = TRUE,
          row.names = FALSE
        )
        log_info(sprintf("Stored %d bars for %s", nrow(bars), symbol))
      }, error = function(e) {
        log_error(sprintf("Failed to store bars: %s", e$message))
      })
    },
    
    #' @description Retrieve historical bars
    #' @param symbol Trading symbol
    #' @param interval Bar interval
    #' @param start_date Start date
    #' @param end_date End date
    #' @return data.table of historical bars
    get_bars = function(symbol, interval, start_date, end_date) {
      sql <- sprintf("
        SELECT * FROM bars 
        WHERE symbol = '%s'
        AND interval = '%s'
        AND timestamp BETWEEN '%s' AND '%s'
        ORDER BY timestamp",
        symbol, interval,
        format(as.POSIXct(start_date), "%Y-%m-%d %H:%M:%S"),
        format(as.POSIXct(end_date), "%Y-%m-%d %H:%M:%S")
      )
      
      dt <- as.data.table(dbGetQuery(private$.conn, sql))
      if (nrow(dt) > 0) {
        setkey(dt, timestamp)
      }
      dt
    },
    
    #' @description Store option chain data
    #' @param underlying Underlying symbol
    #' @param options data.table of option data
    store_options = function(underlying, options) {
      assert_string(underlying)
      assert_data_table(options)
      
      options[, underlying := underlying]
      
      tryCatch({
        dbWriteTable(
          private$.conn,
          "options",
          as.data.frame(options),
          append = TRUE,
          row.names = FALSE
        )
        log_info(sprintf("Stored %d options for %s", nrow(options), underlying))
      }, error = function(e) {
        log_error(sprintf("Failed to store options: %s", e$message))
      })
    },
    
    #' @description Clean up and close database connection
    finalize = function() {
      if (!is.null(private$.conn)) {
        dbDisconnect(private$.conn)
        log_info("Storage connection closed")
      }
    }
  )
) 