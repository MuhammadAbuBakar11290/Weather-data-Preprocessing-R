EDA
# Load the required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(stringr)
library(zoo)  # For handling NA values

# Load the processed data
processed_data <- read_csv("C:/Users/Muhammad Abu Bakkar/Downloads/Filtered_France_Data.csv")

# Display the first few rows of the dataset
print(head(processed_data))

# Convert date_time to POSIXct for proper time handling
processed_data$date_time <- as.POSIXct(processed_data$date_time, format="%d.%m.%Y.%H.%M")

# Univariate Analysis
# Define columns of interest for analysis
columns_of_interest <- c("TSK", "PSFC", "X.U10.", "X.V10.", "X.Q2.", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS", "Windspeed")

# Summary Statistics
summary_stats <- processed_data %>%
  select(all_of(columns_of_interest)) %>%
  summary()

print(summary_stats)

# Create histograms for each variable
for (column in columns_of_interest) {
  p <- ggplot(processed_data, aes_string(x = column)) +
    geom_histogram(binwidth = ifelse(column == "Windspeed", 1, 10), fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram of", column), x = column, y = "Frequency") +
    theme_minimal()
  
  # Save the histogram plot
  ggsave(filename = paste0("C:/Users/Muhammad Abu Bakkar/Downloads/", column, "_histogram.png"), plot = p)
}

# Create boxplots for each variable
for (column in columns_of_interest) {
  p <- ggplot(processed_data, aes_string(y = column)) +
    geom_boxplot(fill = "lightblue", color = "blue") +
    labs(title = paste("Boxplot of", column), y = column) +
    theme_minimal()
  
  # Save the boxplot
  ggsave(filename = paste0("C:/Users/Muhammad Abu Bakkar/Downloads/", column, "_boxplot.png"), plot = p)
}

# Plot time series for each variable
for (column in columns_of_interest) {
  p <- ggplot(processed_data, aes(x = date_time, y = get(column))) +
    geom_line(color = "blue") +
    labs(title = paste("Time Series of", column), x = "Date and Time", y = column) +
    theme_minimal()
  
  # Save the time series plot
  ggsave(filename = paste0("C:/Users/Muhammad Abu Bakkar/Downloads/", column, "_timeseries.png"), plot = p)
}

# Save summary statistics to a CSV file
write_csv(as.data.frame(summary_stats), "C:/Users/Muhammad Abu Bakkar/Downloads/Summary_Statistics.csv")

# Print a message indicating that the EDA results have been saved
cat("Univariate analysis results and visualizations have been saved to the specified directory.\n")
