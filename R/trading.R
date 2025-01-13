#' Trading class for order execution
#' @description Handles order placement, modification, and cancellation
Trading <- R6Class(
  "Trading",
  
  public = list(
    #' @description Initialize Trading instance
    #' @param parent Parent TradeStation instance
    initialize = function(parent) {
      private$.parent <- parent
      checkmate::assert_class(private$.parent, "TradeStation")
    },
    
    #' @description Place a new order
    #' @param symbol Trading symbol
    #' @param quantity Number of shares/contracts
    #' @param order_type Type of order (Market, Limit, etc)
    #' @param side Buy, Sell, SellShort, or BuyToCover
    #' @param duration Good Till Cancel (GTC) or Day
    #' @param limit_price Limit price for limit orders
    #' @param stop_price Stop price for stop orders
    place_order = function(symbol, quantity, order_type = "Market", 
                          side = "Buy", duration = "Day",
                          limit_price = NULL, stop_price = NULL) {
      checkmate::assert_string(symbol)
      checkmate::assert_number(quantity, lower = 1)
      checkmate::assert_choice(order_type, c("Market", "Limit", "Stop", "StopLimit"))
      checkmate::assert_choice(side, c("Buy", "Sell", "SellShort", "BuyToCover"))
      checkmate::assert_choice(duration, c("Day", "GTC"))
      
      # Build order payload
      payload <- list(
        AccountID = private$.parent$get_account_id(),
        Symbol = symbol,
        Quantity = quantity,
        OrderType = order_type,
        TradeAction = side,
        TimeInForce = duration,
        Route = "Intelligent"  # Default routing
      )
      
      # Add price fields if needed
      if (order_type %in% c("Limit", "StopLimit")) {
        checkmate::assert_number(limit_price, lower = 0)
        payload$LimitPrice = limit_price
      }
      if (order_type %in% c("Stop", "StopLimit")) {
        checkmate::assert_number(stop_price, lower = 0)
        payload$StopPrice = stop_price
      }
      
      # Submit order
      resp <- private$.request(
        path = "/orders",
        method = "POST",
        body = payload
      )
      
      # Parse and return response
      fromJSON(rawToChar(resp$body))
    },
    
    #' @description Cancel an existing order
    #' @param order_id ID of order to cancel
    cancel_order = function(order_id) {
      checkmate::assert_string(order_id)
      
      resp <- private$.request(
        path = sprintf("/orders/%s", order_id),
        method = "DELETE"
      )
      
      fromJSON(rawToChar(resp$body))
    },
    
    #' @description Replace/modify an existing order
    #' @param order_id ID of order to modify
    #' @param quantity New quantity
    #' @param limit_price New limit price
    #' @param stop_price New stop price
    replace_order = function(order_id, quantity = NULL, 
                           limit_price = NULL, stop_price = NULL) {
      checkmate::assert_string(order_id)
      if (!is.null(quantity)) checkmate::assert_number(quantity, lower = 1)
      if (!is.null(limit_price)) checkmate::assert_number(limit_price, lower = 0)
      if (!is.null(stop_price)) checkmate::assert_number(stop_price, lower = 0)
      
      # Build modification payload
      payload <- list()
      if (!is.null(quantity)) payload$Quantity = quantity
      if (!is.null(limit_price)) payload$LimitPrice = limit_price
      if (!is.null(stop_price)) payload$StopPrice = stop_price
      
      resp <- private$.request(
        path = sprintf("/orders/%s", order_id),
        method = "PUT",
        body = payload
      )
      
      fromJSON(rawToChar(resp$body))
    },
    
    #' @description Get orders for account
    #' @param status Filter by order status (OPEN, EXECUTED, CANCELED, REJECTED, EXPIRED)
    #' @param since Optional date filter (YYYY-MM-DD)
    #' @return data.table of orders with standardized columns
    get_orders = function(status = NULL, since = NULL) {
      query <- list()
      
      if (!is.null(status)) {
        checkmate::assert_choice(status, 
          c("OPEN", "EXECUTED", "CANCELED", "REJECTED", "EXPIRED"))
        query$status <- status
      }
      
      if (!is.null(since)) {
        checkmate::assert_date(since)
        query$since <- format(since, "%Y-%m-%d")
      }
      
      resp <- private$.request(
        path = "/orders",
        query = query
      )
      
      # Parse response into data.table
      orders <- as.data.table(fromJSON(rawToChar(resp$body)))
      
      # Convert timestamps if present
      if ("Timestamp" %in% names(orders)) {
        orders[, Timestamp := as.POSIXct(Timestamp, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")]
      }
      
      orders
    }
  ),
  
  private = list(
    .parent = NULL,
    
    # Helper for API requests
    .request = function(path, method = "GET", query = NULL, body = NULL) {
      private$.parent$request(path, method, query, body)
    }
  )
) 