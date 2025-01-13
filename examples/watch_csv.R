library(data.table)

# Configuration
input_file <- "market_data.csv"
refresh_interval <- 0.5  # seconds
display_width <- 80

# Initialize
last_size <- 0
last_read <- 0
last_display <- Sys.time()
display_interval <- 1  # seconds

# Function to format numbers consistently
format_price <- function(x) {
  if (is.na(x)) return(sprintf("%8s", "N/A"))
  sprintf("%8.2f", x)
}

format_volume <- function(x) {
  if (is.na(x)) return(sprintf("%10s", "N/A"))
  sprintf("%10d", x)
}

# Function to create a compact quote display
display_quotes <- function(quotes) {
  cat("\033[2J\033[H")  # Clear screen and move cursor to top
  cat(sprintf("Market Data Watch - %s\n", format(Sys.time(), "%H:%M:%S")))
  cat(paste(rep("-", display_width), collapse=""), "\n")
  cat(sprintf("%-4s %8s %8s %8s %10s %19s\n", 
              "SYM", "BID", "LAST", "ASK", "VOLUME", "TRADE TIME"))
  cat(paste(rep("-", display_width), collapse=""), "\n")
  
  for (i in 1:nrow(quotes)) {
    quote <- quotes[i]
    cat(sprintf("%-4s %s %s %s %s %s\n",
                quote$symbol,
                format_price(quote$bid),
                format_price(quote$last),
                format_price(quote$ask),
                format_volume(quote$volume),
                format(quote$trade_time, "%H:%M:%S")))
  }
  cat(paste(rep("-", display_width), collapse=""), "\n")
}

cat("Watching file:", input_file, "\n")
cat("Press Ctrl+C to stop\n\n")

while (TRUE) {
  if (!file.exists(input_file)) {
    cat("Waiting for file to be created...\n")
    Sys.sleep(refresh_interval)
    next
  }
  
  # Check if file has new data
  current_size <- file.info(input_file)$size
  if (current_size > last_size) {
    # Read new data
    dt <- fread(input_file)
    new_rows <- nrow(dt) - last_read
    
    if (new_rows > 0) {
      # Get latest quotes by symbol
      latest_quotes <- dt[type == "quote"][, .SD[.N], by=symbol]
      
      # Update display if enough time has passed
      now <- Sys.time()
      if (difftime(now, last_display, units="secs") >= display_interval && nrow(latest_quotes) > 0) {
        display_quotes(latest_quotes)
        last_display <- now
      }
      
      # Update counters
      last_size <- current_size
      last_read <- nrow(dt)
    }
  }
  
  # Wait before next check
  Sys.sleep(refresh_interval)
} 