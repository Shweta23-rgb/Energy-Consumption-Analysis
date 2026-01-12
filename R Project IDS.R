library(tidyverse)
library (arrow)
static_housing <- read_parquet("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/static_house_info.parquet") 
energy_data <- read_parquet("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/2023-houseData/102063.parquet")
weather_data <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/weather/2023-weather-data/G4500010.csv")
meta_data <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/data_dictionary.csv")


colnames(static_housing)
colnames(energy_data)
colnames (weather_data)

library(writexl)
statichousing<- "statichousing.xlsx"
write_xlsx(static_housing,path = statichousing)

energydata <- "energydata.xlsx"
write_xlsx(energy_data, energydata)

weatherdata <- "weatherdata.xlsx"
write_xlsx(weather_data,weatherdata)

meta1<-"metadata.xlsx"
write_xlsx(meta_data,meta1)
dim(meta_data)

#Energy Data - Keerthi Krishna 
summary(energy_data)
str(energy_data)
view(energy_data$time)

# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# Assuming energy_data is already read and available
# Ensure that 'time' is in a date-time format
energy_data$time <- as.POSIXct(energy_data$time, format = "%Y-%m-%d %H:%M:%S")

# Extract week from the date
energy_data$week <- format(energy_data$time, "%Y-%U")

# Select columns for plotting and aggregate by week
selected_columns <- c("out.electricity.ceiling_fan.energy_consumption",
                      "out.electricity.clothes_dryer.energy_consumption",
                      "out.electricity.cooling.energy_consumption",
                      "out.electricity.lighting_interior.energy_consumption",
                      "out.electricity.refrigerator.energy_consumption")

energy_data_weekly <- energy_data %>%
  select(week, all_of(selected_columns)) %>%
  group_by(week) %>%
  summarise_all(sum)

# Melting the data for ggplot
data_melted <- energy_data_weekly %>%
  gather(key = "Appliance", value = "Energy_Consumption", -week)

# Plotting the data
ggplot(data_melted, aes(x = week, y = Energy_Consumption, color = Appliance)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Weekly Energy Consumption for Various Appliances",
       x = "Week",
       y = "Total Energy Consumption") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability


# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# Read the data
energy_data <- read_excel("path_to_your_file/energydata.xlsx")

# Ensure that 'time' is in a date-time format
energy_data$time <- as.POSIXct(energy_data$time)

# Extract the week number from the date
energy_data$week_number <- isoweek(energy_data$time)

# Create a formatted week label
energy_data$week_label <- paste("Week", energy_data$week_number)

# Select columns for plotting and aggregate by week
selected_columns <- c("out.electricity.ceiling_fan.energy_consumption",
                      "out.electricity.clothes_dryer.energy_consumption",
                      "out.electricity.cooling.energy_consumption",
                      "out.electricity.lighting_interior.energy_consumption",
                      "out.electricity.refrigerator.energy_consumption")

energy_data_weekly <- energy_data %>%
  select(week_label, all_of(selected_columns)) %>%
  group_by(week_label) %>%
  summarise_all(sum)

# Melting the data for ggplot
data_melted <- energy_data_weekly %>%
  gather(key = "Appliance", value = "Energy_Consumption", -week_label)

# Plotting the data
ggplot(data_melted, aes(x = week_label, y = Energy_Consumption, color = Appliance)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Weekly Energy Consumption for Various Appliances",
       x = "Week",
       y = "Total Energy Consumption") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability


# Load necessary libraries
library(readxl)
library(ggplot2)

# Read the data
data <- read_excel("path_to_your_file/energydata.xlsx")
data <- energy_data
# Select columns for plotting
selected_columns <- c("out.electricity.ceiling_fan.energy_consumption",
                      "out.electricity.clothes_dryer.energy_consumption",
                      "out.electricity.cooling.energy_consumption",
                      "out.electricity.lighting_interior.energy_consumption",
                      "out.electricity.refrigerator.energy_consumption")

# Melting the data for ggplot
library(tidyr)
data_melted <- data %>%
  select(time, all_of(selected_columns)) %>%
  gather(key = "Appliance", value = "Energy_Consumption", -time)

# Plotting the data
ggplot(data_melted, aes(x = time, y = Energy_Consumption, color = Appliance)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series of Energy Consumption for Various Appliances",
       x = "Time",
       y = "Energy Consumption")


# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# Ensure that 'time' is in a date-time format
energy_data$time <- as.POSIXct(energy_data$time)

# Extract week from the date
energy_data$week <- format(energy_data$time, "%Y-%U")

# Select columns for plotting and aggregate by week
selected_columns <- c("out.electricity.ceiling_fan.energy_consumption",
                      "out.electricity.clothes_dryer.energy_consumption",
                      "out.electricity.cooling.energy_consumption",
                      "out.electricity.lighting_interior.energy_consumption",
                      "out.electricity.refrigerator.energy_consumption")

energy_data_weekly <- energy_data %>%
  select(week, all_of(selected_columns)) %>%
  group_by(week) %>%
  summarise_all(sum)

# Melting the data for ggplot
data_melted <- energy_data_weekly %>%
  gather(key = "Appliance", value = "Energy_Consumption", -week)

# Plotting the data with modified legend position
ggplot(data_melted, aes(x = week, y = Energy_Consumption, color = Appliance)) +
  geom_line(aes(group = Appliance), size = 1.5) +  # Make lines thicker
  theme_minimal() +
  labs(title = "Weekly Energy Consumption for Various Appliances",
       x = "Week",
       y = "Total Energy Consumption") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = c(0.20, 0.85),  # Position the legend inside the plot
        legend.background = element_rect(fill = "white", colour = "black"),  # Add a box around the legend
        legend.text = element_text(size = 6),  # Reduce the size of the text in the legend
        legend.key.size = unit(1.0, "lines"))  # Reduce the size of the legend box

# Assuming you have already loaded the energy_data dataframe
# If not, load it using readxl
# energy_data <- read_excel("path_to_your_file/energydata.xlsx")

library(dplyr)

# Sum of all columns
# Assuming you have already loaded the energy_data dataframe
# If not, load it using readxl

# Identifying numerical columns (assuming numeric and integer types are relevant)
numerical_columns <- sapply(energy_data, is.numeric)

# Compute sum for each numerical column
column_sums <- colSums(energy_data[, numerical_columns], na.rm = TRUE)

# Display the results
print("Sum of each numerical column:")
print(column_sums)


# Check for null values in each column
null_values_count <- sapply(energy_data, function(x) sum(is.na(x)))

# Display the results
print("Sum of each column:")
print(column_sums)

print("Null values in each column:")
print(null_values_count)

#BarChart for each column

# Calculate the sums for numerical columns
# Assuming you have already loaded the energy_data dataframe
# If not, load it using readxl
# energy_data <- read_excel("path_to_your_file/energydata.xlsx")

library(ggplot2)
library(dplyr)

# Calculate the sums for numerical columns
numerical_columns <- sapply(energy_data, is.numeric)
column_sums <- colSums(energy_data[, numerical_columns], na.rm = TRUE)

# Create a dataframe for plotting
sums_df <- data.frame(Column = names(column_sums), Sum = column_sums)

# Plotting the bar chart with customized y-axis
ggplot(sums_df, aes(x = reorder(Column, Sum), y = Sum)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 5000),  # Set y-axis limits
                     breaks = seq(0, 50000, by = 1000),  # Set y-axis breaks
                     labels = scales::comma) +  # Format the labels with commas for readability
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Sum of Each Column in energy_data", x = "Column", y = "Sum")


#JOINING WEATHER AND TIME DATASETS
library(readxl)
library(dplyr)
library(lubridate)

# Load the datasets
energy_data <- read_excel("/mnt/data/energydata.xlsx")
weather_data <- read_excel("/mnt/data/weatherdata.xlsx")

# Convert the time columns to the same format if necessary
energy_data$time <- as.POSIXct(energy_data$time, tz = "UTC")
weather_data$date_time <- as.POSIXct(weather_data$date_time, tz = "UTC")

# If the time granularity is different, adjust it. For example, if you need to match by hour:
energy_data$time <- round_date(energy_data$time, "hour")
weather_data$date_time <- round_date(weather_data$date_time, "hour")

# Join the datasets
# Use inner_join, left_join, right_join, or full_join as per your requirement
joined_data <- left_join(energy_data, weather_data, by = c("time" = "date_time"))

# View the first few rows of the joined dataset
head(joined_data)
dim(joined_data)

file_numbers <- as.lstatic_housing$bldg_id



base_url <- "https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/2023-houseData/100.parquet"
file_numbers <- as.list(static_housing$bldg_id)
read_and_print_links <- function(base_url, file_numbers) {
  # Initialize an empty dataframe to store the results
  appended_df <- data.frame()
  
  # Loop through file_numbers and read each file
  for (file_number in file_numbers) {
    # Construct the URL by replacing  100 with the current file_number
    file_url <- sub(100, file_number, base_url, fixed = TRUE)
    
    # Read the data file from the URL
    # Adjust the file reading method based on your actual file format
    current_df <- arrow::read_parquet(file_url)  # Replace with the appropriate function
    
    # Filter rows based on the condition
    # Assuming 'time' is a column in the data frame and is in POSIXct format
    current_df <- current_df[format(current_df$time, "%Y-%m") == "2018-07", ]
    current_df$building_ID <- file_number
    # Append the current dataframe to the result dataframe
    appended_df <- rbind(appended_df, current_df)
  }
  
  # Return the final appended dataframe
  return(appended_df)
}

dim(appended_df)
base_url <- "https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/2023-houseData/100.parquet"
file_numbers <- static_housing$bldg_id

read_and_print_links(base_url, file_numbers)

```




#
energy_usage_data <- list()

for (building_id in unique(static_housing$building_id)) {
  file_url <- paste0("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/2023-houseData/", building_id, ".parquet")
  energy_data <- tryCatch(
    {
      arrow::read_parquet(file_url)
    },
    error = function(e) NULL
  )
  if (!is.null(energy_data)) {
    energy_usage_data[[building_id]] <- energy_data
  }
}

energy_usage_data <- do.call(rbind, energy_usage_data)
energy_usage_data

static_housing$bldg_id


read_and_print_links <- function(base_url, file_numbers) {
  appended_df <- data.frame()
  
  for (file_number in file_numbers) {
    file_url <- sub(100, file_number, base_url, fixed = TRUE)
    current_df <- arrow::read_parquet(file_url)
    current_df <- current_df[format(current_df$time, "%Y-%m") == "2018-07", ]
    current_df$building_ID <- file_number
    appended_df <- rbind(appended_df, current_df)
  }
  
  return(appended_df)
}

base_url <- "https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/2023-houseData/100.parquet"
file_numbers <- static_housing$bldg_id
results_df <- read_and_print_links(base_url, file_numbers)

