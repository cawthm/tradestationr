library(R6)
library(checkmate)
library(httr2)
library(logger)
library(data.table)
library(jsonlite)

source("R/auth.R")
source("R/market_data.R")
source("R/tradestation.R")

ts <- TradeStation$new(secrets_dir = ".secrets")
ts$authenticate()

# Access token through the token manager
token_manager <- ts$private$.token_manager
cat(token_manager$get_access_token()) 