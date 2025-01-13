library(R6)
library(checkmate)
library(httr2)
library(logger)
library(data.table)
library(jsonlite)

source("R/auth.R")
source("R/market_data.R")
source("R/tradestation.R")

# Initialize and authenticate
ts <- TradeStation$new(secrets_dir = ".secrets")
ts$authenticate()

# Test market data functionality
if (ts$is_authenticated()) {
  # Get historical data for AAPL
  bars <- ts$market_data()$get_bars("AAPL", interval = "1min", 
                                  start_date = Sys.Date() - 1,
                                  end_date = Sys.Date())
  print(head(bars))
  
  # Get symbol details
  details <- ts$market_data()$get_symbol_details("AAPL")
  print(details)
} 