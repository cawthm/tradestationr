library(data.table)
library(RSQLite)
library(ggplot2)

# Connect to existing database
db_file <- "market_data.sqlite"
con <- dbConnect(SQLite(), db_file)

# Get database summary
cat("Database Summary:\n")
cat("================\n")

# Get table sizes
tables <- dbGetQuery(con, "
  SELECT name, 
         (SELECT COUNT(*) FROM latency_stats) as latency_count,
         (SELECT COUNT(*) FROM market_data) as market_data_count
  FROM sqlite_master 
  WHERE type='table' 
  LIMIT 1
")

cat("\nTotal records:\n")
cat("- Latency measurements:", tables$latency_count, "\n")
cat("- Market data points:", tables$market_data_count, "\n")

# Get time range
time_range <- dbGetQuery(con, "
  SELECT 
    MIN(timestamp) as start_time,
    MAX(timestamp) as end_time,
    (strftime('%s', MAX(timestamp)) - strftime('%s', MIN(timestamp))) as duration_secs
  FROM market_data
")

cat("\nTime range:\n")
cat("- Start:", time_range$start_time, "\n")
cat("- End:", time_range$end_time, "\n")
cat("- Duration:", round(time_range$duration_secs/60, 2), "minutes\n")

# Get message counts by symbol
symbol_stats <- dbGetQuery(con, "
  SELECT 
    symbol,
    COUNT(*) as message_count,
    ROUND(COUNT(*) * 1.0 / (strftime('%s', MAX(timestamp)) - strftime('%s', MIN(timestamp))), 2) as avg_msgs_per_sec
  FROM market_data
  GROUP BY symbol
")

cat("\nMessages by symbol:\n")
print(symbol_stats, row.names = FALSE)

# Get latency statistics
latency_stats <- dbGetQuery(con, "
  SELECT 
    symbol,
    stage,
    COUNT(*) as measurements,
    ROUND(AVG(latency_ms), 2) as avg_latency,
    ROUND(MIN(latency_ms), 2) as min_latency,
    ROUND(MAX(latency_ms), 2) as max_latency
  FROM latency_stats
  GROUP BY symbol, stage
  ORDER BY symbol, stage
")

cat("\nLatency statistics (milliseconds):\n")
print(latency_stats, row.names = FALSE)

# Create a summary plot
latency_data <- dbGetQuery(con, "
  SELECT timestamp, symbol, stage, latency_ms
  FROM latency_stats
  ORDER BY timestamp
")

if (nrow(latency_data) > 0) {
  dt <- as.data.table(latency_data)
  dt[, timestamp := as.POSIXct(timestamp)]
  
  p <- ggplot(dt, aes(x = timestamp, y = latency_ms, color = stage)) +
    geom_line(alpha = 0.5) +
    facet_wrap(~symbol, scales = "free_y") +
    labs(title = "Historical Latency Analysis",
         x = "Time",
         y = "Latency (ms)",
         color = "Stage") +
    theme_minimal()
  
  ggsave("historical_latency.png", p, width = 12, height = 8)
  cat("\nCreated historical latency plot: historical_latency.png\n")
}

# Cleanup
dbDisconnect(con)
cat("\nAnalysis complete!\n") 