# Load necessary libraries
library(sf)
library(ggplot2)

energy_data <- Subset_V4 %>%
  group_by(in.county) %>%
  summarize(total_energy = sum(Final_Energy_KWH, na.rm = TRUE))
energy_data

# Load the South Carolina county shapefile
sc_counties <- st_read("/Users/USER/Downloads/1950_Census_Data_for_NEH_Workshop_WFL1/1950_Census_Data_for_NEH_Workshop_WFL1.shp")
# Merge energy_data with the shapefile based on county names
merged1_data <- merge(sc_counties, energy_data, by.x = "GISJOIN", by.y = "in.county", all.x = TRUE)
merged1_data <- data.frame(merged1_data)
colnames(merged1_data)

head(merged1_data,1)# Create the heatmap
ggplot(merged1_data, aes(fill = total_energy)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Energy Consumption Heatmap by County in South Carolina") +
  theme_minimal()
