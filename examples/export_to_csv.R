library(data.table)
library(RSQLite)

# Connect to database
db_file <- "market_data.sqlite"
con <- dbConnect(SQLite(), db_file)

# Export market data
market_data <- dbGetQuery(con, "SELECT * FROM market_data ORDER BY timestamp")
fwrite(market_data, "market_data_export.csv")
cat("Exported", nrow(market_data), "market data records to market_data_export.csv\n")

# Export latency stats
latency_stats <- dbGetQuery(con, "SELECT * FROM latency_stats ORDER BY timestamp")
fwrite(latency_stats, "latency_stats_export.csv")
cat("Exported", nrow(latency_stats), "latency records to latency_stats_export.csv\n")

# Cleanup
dbDisconnect(con)
cat("\nExport complete!\n") 