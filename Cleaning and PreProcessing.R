Data cleaning and preporocessing
# Load the required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(stringr)
library(zoo)  # Load the zoo package

# Function to retrieve Bolton's geographic coordinates
retrieve_bolton_coords <- function() {
  bolton_latitude <- 53.5789
  bolton_longitude <- -2.4292
  return(list(lat = bolton_latitude, long = bolton_longitude))
}


# Load the weather data from the specified file path without headers
weather_dataset <- read_csv("C:/Users/Muhammad Abu Bakkar/Downloads/WRFdata_May2023.csv", col_names = FALSE)

# Set the column names using the second row of the dataset
colnames(weather_dataset) <- as.character(weather_dataset[2, ])

# Remove the second row as it has been used for column names
weather_dataset <- weather_dataset[-2, ]

# Display the first few rows of the dataset to verify
print(head(weather_dataset))

# Retrieve Bolton's geographic coordinates
bolton_coordinates <- retrieve_bolton_coords()

# Define the latitude and longitude boundaries for filtering based on France's coordinates
latitude_min <- 41.303
latitude_max <- 51.124
longitude_min <- -5.142
longitude_max <- 9.561

# Ensure column names are unique and valid in R
colnames(weather_dataset) <- make.names(names(weather_dataset), unique = TRUE)

# Filter the dataset to include only rows within the geographic boundaries of France
france_data <- weather_dataset %>%
  filter(!(XLAT == boltan_lat& XLONG == bolton_long)) %>% 
  filter(XLAT >= latitude_min & XLAT <= latitude_max) %>%
  filter(XLONG >= longitude_min & XLONG <= longitude_max)

# Ensure that at least 350 rows are randomly sampled from the filtered dataset
france_data <- france_data %>%
  sample_n(min(n(), 350))

# Remove the latitude and longitude columns from the dataset
data_without_coordinates <- france_data %>%
  select(-XLAT, -XLONG)

# Function to replace NA values using interpolations
fill_na_with_mean <- function(x) {
  na_fill <- na.approx(x, maxgap = Inf, rule = 2)  # Use na.approx from the zoo package
  return(na_fill)
}

# Apply the NA filling function to all columns in the dataset
data_filled <- data_without_coordinates %>%
  mutate(across(everything(), ~ fill_na_with_mean(.)))

# Display the first few rows of the dataset with NA values filled
print(head(data_filled))

# Define the starting date and time for the data processing
start_datetime <- as.POSIXct("2018-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S")

# Initialize an empty dataframe to store processed data
processed_data <- data.frame()

# List of columns to keep (excluding geographic coordinates)
base_columns <- c("TSK", "PSFC", "X.U10.", "X.V10.", 
                  "X.Q2.", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS")

# Create a sequence for time increments, every 3 hours up to 120 intervals
time_increments <- seq(0, 120 * 3 * 3600, by = 3 * 3600)  # Every 3 hours up to 120 intervals

# Process the filtered data with the alternative method
for (time_increment in time_increments) {
  # Create a copy of the base columns for the current time increment
  temp_df <- data_filled[, base_columns]
  
  if (time_increment > 0) {
    # Create suffix for the column names based on the current time increment
    suffix <- paste0(".", time_increment / (3 * 3600))
    columns_with_suffix <- paste0(base_columns, suffix)
    temp_df[, ] <- data_filled[, columns_with_suffix]
  }
  
  # Compute the datetime for the current time increment
  current_datetime <- start_datetime + time_increment
  
  # Format the datetime for inclusion in the dataframe
  temp_df$date_time <- format(current_datetime, "%d.%m.%Y.%H.%M")
  
  # Combine the processed data with the main dataframe
  processed_data <- bind_rows(processed_data, temp_df)
}

# Calculate the wind speed and add it to the processed data
processed_data <- processed_data %>%
  mutate(Windspeed = sqrt(`X.U10.`^2 + `X.V10.`^2)) %>%
  mutate(Windspeed = round(Windspeed, 2))  # Round wind speed to 2 decimal places

# Display the first few rows of the processed data
print(head(processed_data))

# Save the filtered and processed data to a new CSV file
write_csv(processed_data, "C:/Users/Muhammad Abu Bakkar/Downloads/Filtered_France_Data.csv")

# Print a message indicating that the data has been saved
cat("Filtered and processed data has been saved to 'C:/Users/Muhammad Abu Bakkar/Downloads/Filtered_France_Data.csv'.\n")

