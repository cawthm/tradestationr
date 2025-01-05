#' @title High Performance R Interface to TradeStation API
#' @description A comprehensive R package for interacting with the TradeStation API,
#'   focusing on high-performance market data retrieval and storage. The package
#'   uses data.table for fast in-memory operations and SQLite for persistent
#'   storage.
#'
#' @section Features:
#' \itemize{
#'   \item OAuth2 authentication with token caching
#'   \item Historical market data retrieval
#'   \item Real-time streaming market data
#'   \item Options chain data
#'   \item High-performance data storage with SQLite
#'   \item Efficient data manipulation with data.table
#' }
#'
#' @section Classes:
#' \describe{
#'   \item{TradeStation}{Main client class for API interaction}
#'   \item{MarketData}{Market data retrieval and streaming}
#'   \item{Storage}{Persistent data storage with SQLite}
#' }
#'
#' @docType package
#' @name tradestationr
#' @import data.table
#' @import httr2
#' @import RSQLite
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON parse_json
#' @importFrom logger log_info log_error
#' @importFrom checkmate assert_string assert_choice assert_function assert_data_table
NULL

# Global variables used in data.table operations
utils::globalVariables(c(
  "Timestamp",
  "underlying",
  "."
)) 