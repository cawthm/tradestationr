library(data.table)
library(ggplot2)
library(scales)
library(lubridate)

# Configuration
latency_file <- "latency_stats.csv"
output_dir <- "latency_analysis"
dir.create(output_dir, showWarnings = FALSE)

# Load and prepare data
cat("Loading data from", latency_file, "...\n")
dt <- fread(latency_file)

# Convert timestamp to POSIXct if not already
if (!inherits(dt$timestamp, "POSIXct")) {
  dt[, timestamp := as.POSIXct(timestamp)]
}

# Basic statistics by stage and symbol
stats <- dt[!is.na(latency_ms), .(
  count = .N,
  mean = mean(latency_ms),
  median = median(latency_ms),
  sd = sd(latency_ms),
  min = min(latency_ms),
  max = max(latency_ms),
  p95 = quantile(latency_ms, 0.95),
  p99 = quantile(latency_ms, 0.99)
), by = .(symbol, stage)]

# Save statistics
fwrite(stats, file.path(output_dir, "latency_statistics.csv"))

# Print summary
cat("\nLatency Statistics (milliseconds):\n")
print(stats[order(symbol, stage)], digits = 2)

# Create time series plot
p1 <- ggplot(dt[!is.na(latency_ms)], aes(x = timestamp, y = latency_ms, color = stage)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~symbol, scales = "free_y") +
  labs(title = "Latency Over Time by Symbol and Stage",
       x = "Time",
       y = "Latency (ms)",
       color = "Stage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_datetime(labels = date_format("%H:%M:%S"))

ggsave(file.path(output_dir, "latency_timeseries.png"), p1, width = 12, height = 8)

# Create boxplot
p2 <- ggplot(dt[!is.na(latency_ms)], aes(x = stage, y = latency_ms, fill = symbol)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(title = "Latency Distribution by Stage and Symbol",
       x = "Stage",
       y = "Latency (ms)",
       fill = "Symbol") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)

ggsave(file.path(output_dir, "latency_distribution.png"), p2, width = 12, height = 8)

# Calculate rolling statistics
roll_stats <- dt[!is.na(latency_ms), .(
  rolling_mean = frollmean(latency_ms, 10),
  rolling_median = frollmean(latency_ms, 10),
  rolling_sd = frollsd(latency_ms, 10)
), by = .(symbol, stage, timestamp)]

# Create rolling statistics plot
p3 <- ggplot(roll_stats, aes(x = timestamp, y = rolling_mean, color = stage)) +
  geom_line() +
  geom_ribbon(aes(ymin = rolling_mean - rolling_sd, 
                  ymax = rolling_mean + rolling_sd,
                  fill = stage), 
              alpha = 0.2, color = NA) +
  facet_wrap(~symbol, scales = "free_y") +
  labs(title = "Rolling Average Latency (10-sample window)",
       x = "Time",
       y = "Latency (ms)",
       color = "Stage",
       fill = "Stage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_datetime(labels = date_format("%H:%M:%S"))

ggsave(file.path(output_dir, "latency_rolling.png"), p3, width = 12, height = 8)

# Analyze potential correlations
if (dt[, .N] > 1) {
  # Correlation between message size and latency
  size_corr <- dt[!is.na(latency_ms), .(
    correlation = cor(latency_ms, msg_size, use = "complete.obs")
  ), by = .(symbol, stage)]
  
  cat("\nCorrelation between message size and latency:\n")
  print(size_corr)
  
  # Create correlation plot
  p4 <- ggplot(dt[!is.na(latency_ms)], aes(x = msg_size, y = latency_ms, color = stage)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~symbol, scales = "free") +
    labs(title = "Latency vs Message Size",
         x = "Message Size (bytes)",
         y = "Latency (ms)",
         color = "Stage") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "latency_correlation.png"), p4, width = 12, height = 8)
}

# Generate summary report
report <- file.path(output_dir, "latency_report.txt")
cat("Latency Analysis Report\n",
    "=====================\n",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
    file = report)

# Add overall statistics
cat("\nOverall Statistics:\n",
    "Total samples:", dt[, .N], "\n",
    "Time period:", format(min(dt$timestamp)), "to", format(max(dt$timestamp)), "\n",
    "Symbols analyzed:", paste(unique(dt$symbol), collapse = ", "), "\n",
    file = report, append = TRUE)

# Add stage-wise statistics
cat("\nStage-wise Statistics (ms):\n", file = report, append = TRUE)
capture.output(print(stats[order(symbol, stage)], digits = 2), 
              file = report, append = TRUE)

# Add correlation analysis
if (exists("size_corr")) {
  cat("\nMessage Size Correlation Analysis:\n", file = report, append = TRUE)
  capture.output(print(size_corr, digits = 3), 
                file = report, append = TRUE)
}

cat("\nAnalysis complete! Results saved in", output_dir, "\n")
cat("- Summary report:", report, "\n")
cat("- Statistics:", file.path(output_dir, "latency_statistics.csv"), "\n")
cat("- Visualizations: *.png files in", output_dir, "\n") 