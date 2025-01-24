library(data.table)
library(RSQLite)
library(ggplot2)
library(scales)

# Configuration
db_file <- "market_data.sqlite"
update_interval <- 1  # seconds
window_size <- 60    # seconds to show in rolling window

# Initialize connection
con <- dbConnect(SQLite(), db_file)

# Function to create rolling window plot
create_plot <- function(latency_data) {
  if (nrow(latency_data) == 0) return(NULL)
  
  ggplot(latency_data, aes(x = timestamp, y = latency_ms, color = stage)) +
    geom_line() +
    facet_wrap(~symbol, scales = "free_y") +
    labs(title = sprintf("Latency Over Last %d Seconds", window_size),
         subtitle = format(Sys.time(), "%H:%M:%S"),
         x = "Time",
         y = "Latency (ms)",
         color = "Stage") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10)) +
    scale_x_datetime(labels = date_format("%H:%M:%S"))
}

# Function to print statistics
print_stats <- function(latency_data) {
  if (nrow(latency_data) == 0) {
    cat("No data in current window\n")
    return()
  }
  
  stats <- latency_data[, .(
    count = .N,
    mean = mean(latency_ms),
    median = median(latency_ms),
    p95 = quantile(latency_ms, 0.95),
    max = max(latency_ms)
  ), by = .(symbol, stage)]
  
  cat("\033[2J\033[H")  # Clear screen
  cat("Real-time Latency Statistics (Last", window_size, "seconds)\n")
  cat("Updated at:", format(Sys.time(), "%H:%M:%S"), "\n\n")
  
  for (sym in unique(stats$symbol)) {
    cat("\nSymbol:", sym, "\n")
    sym_stats <- stats[symbol == sym]
    print(sym_stats[order(stage)], digits = 2)
  }
}

# Main monitoring loop
cat("Starting real-time latency monitor...\n")
cat("Press Ctrl+C to stop\n\n")

while (TRUE) {
  # Get data from last window_size seconds
  current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  window_start <- format(Sys.time() - window_size, "%Y-%m-%d %H:%M:%S")
  
  # Query latest data
  latency_data <- dbGetQuery(con, sprintf("
    SELECT timestamp, symbol, stage, latency_ms
    FROM latency_stats
    WHERE timestamp BETWEEN '%s' AND '%s'
    ORDER BY timestamp", window_start, current_time))
  
  # Convert to data.table and format timestamp
  latency_dt <- as.data.table(latency_data)
  if (nrow(latency_dt) > 0) {
    latency_dt[, timestamp := as.POSIXct(timestamp)]
  }
  
  # Print statistics
  print_stats(latency_dt)
  
  # Optional: Save plot
  p <- create_plot(latency_dt)
  if (!is.null(p)) {
    ggsave("latency_monitor.png", p, width = 12, height = 8)
  }
  
  # Get current message rates
  msg_rates <- dbGetQuery(con, sprintf("
    SELECT 
      symbol,
      COUNT(*) as msg_count,
      ROUND(CAST(COUNT(*) AS FLOAT) / %d, 2) as msgs_per_sec
    FROM market_data
    WHERE timestamp BETWEEN '%s' AND '%s'
    GROUP BY symbol", 
    window_size, window_start, current_time))
  
  cat("\nMessage Rates:\n")
  print(msg_rates, row.names = FALSE)
  
  # Sleep until next update
  Sys.sleep(update_interval)
} 