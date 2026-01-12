library(readr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(arrow)
library(readxl)
library(writexl)

# Function to fetch, filter, and process weather data
fetch_process_july_weather_data <- function(county_ids, base_url) {
  all_weather_data <- NULL
  
  for(county_id in county_ids) {
    # Construct the URL for the current county
    url <- paste0(base_url, county_id, ".csv")
    
    # Read the CSV file from the URL
    weather_data <- read_csv(url)
    
    # Convert date_time column to POSIXct, filter for July, and extract hour
    weather_data$date_time <- as.POSIXct(weather_data$date_time, format="%Y-%m-%d %H:%M:%S")
    weather_data <- weather_data[format(weather_data$date_time, "%m") == "07", ]
    weather_data$hour <- hour(weather_data$date_time)
    
    # Add a column for county ID if it doesn't already exist
    if(!"county_id" %in% names(weather_data)) {
      weather_data$county_id <- county_id
    }
    
    # Concatenate the data into one dataframe
    if(is.null(all_weather_data)) {
      all_weather_data <- weather_data
    } else {
      all_weather_data <- bind_rows(all_weather_data, weather_data)
    }
  }
  
  # Group by county ID and hour, and summarize each column
  grouped_avg_data <- all_weather_data %>%
    group_by(county_id, hour) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))
  
  return(grouped_avg_data)
}

# Example usage
initial_static_housing <- read_parquet("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/static_house_info.parquet") 
county_ids <- unique(initial_static_housing$in.county)
base_url <- "https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/weather/2023-weather-data/"

# Fetch, process, and get hourly averages for July weather data by county
grouped_weather_data <- fetch_process_july_weather_data(county_ids, base_url)
grouped_weather_data

weather_county<- "weather_county_agg.xlsx"
write.csv(grouped_weather_data,weather_county)
write_xlsx(grouped_weather_data,weather_county)
