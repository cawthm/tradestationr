library(R6)
library(checkmate)
library(httr2)
library(logger)
library(data.table)
library(jsonlite)

source("R/auth.R")
source("R/market_data.R")
source("R/trading.R")
source("R/tradestation.R")

# Initialize client
ts <- TradeStation$new(secrets_dir = ".secrets")
ts$authenticate()

# Helper function to display order details
print_order <- function(order, description) {
  cat("\n", description, "\n")
  cat("Order ID:", order$OrderID, "\n")
  cat("Status:", order$Status, "\n")
  cat("Message:", order$Message, "\n")
  cat("Timestamp:", order$Timestamp, "\n")
  return(order$OrderID)  # Return order ID for tracking
}

# Track orders in a data.table
orders <- data.table(
  order_id = character(),
  symbol = character(),
  side = character(),
  quantity = integer(),
  timestamp = as.POSIXct(character())
)

# Example 1: Buy to Open (Long Position)
symbol <- "SPY"
quantity <- 100

cat("\n=== Example 1: Buy to Open ===\n")
buy_order <- ts$trading()$place_order(
  symbol = symbol,
  quantity = quantity,
  order_type = "Market",
  side = "Buy",
  duration = "Day"
)
buy_id <- print_order(buy_order, "Buy to Open Order")

# Store order details
orders <- rbindlist(list(orders, data.table(
  order_id = buy_id,
  symbol = symbol,
  side = "Buy",
  quantity = quantity,
  timestamp = Sys.time()
)))

# Wait briefly to check order status
Sys.sleep(2)
status <- ts$trading()$get_orders(status = "OPEN")
cat("\nOpen Orders after Buy:\n")
print(status)

# Example 2: Sell to Close (Exit Long Position)
cat("\n=== Example 2: Sell to Close ===\n")
sell_order <- ts$trading()$place_order(
  symbol = symbol,
  quantity = quantity,
  order_type = "Market",
  side = "Sell",
  duration = "Day"
)
sell_id <- print_order(sell_order, "Sell to Close Order")

# Store order details
orders <- rbindlist(list(orders, data.table(
  order_id = sell_id,
  symbol = symbol,
  side = "Sell",
  quantity = quantity,
  timestamp = Sys.time()
)))

# Example 3: Sell Short to Open (Short Position)
cat("\n=== Example 3: Sell Short to Open ===\n")
short_order <- ts$trading()$place_order(
  symbol = symbol,
  quantity = quantity,
  order_type = "Market",
  side = "SellShort",
  duration = "Day"
)
short_id <- print_order(short_order, "Sell Short Order")

# Store order details
orders <- rbindlist(list(orders, data.table(
  order_id = short_id,
  symbol = symbol,
  side = "SellShort",
  quantity = quantity,
  timestamp = Sys.time()
)))

# Example 4: Buy to Cover (Exit Short Position)
cat("\n=== Example 4: Buy to Cover ===\n")
cover_order <- ts$trading()$place_order(
  symbol = symbol,
  quantity = quantity,
  order_type = "Market",
  side = "BuyToCover",
  duration = "Day"
)
cover_id <- print_order(cover_order, "Buy to Cover Order")

# Store order details
orders <- rbindlist(list(orders, data.table(
  order_id = cover_id,
  symbol = symbol,
  side = "BuyToCover",
  quantity = quantity,
  timestamp = Sys.time()
)))

# Display all tracked orders
cat("\n=== Order History ===\n")
print(orders)

# Get final status of all orders
cat("\n=== Final Order Status ===\n")
final_status <- ts$trading()$get_orders(since = Sys.Date())
print(final_status) 