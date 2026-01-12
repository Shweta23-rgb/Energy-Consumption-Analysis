library (arrow)
library(tidyverse)
library(dplyr)
library(caret)

merge_proj_data <- read_parquet("/Users/USER/Desktop/IDS Files/output_file.parquet")

#Checking for Null Counts
null_counts <- sapply(merge_proj_data, function(x) sum(is.na(x)))
sum(null_counts)

# List of columns to sum as provided
columns_to_sum <- c(
  "out.electricity.ceiling_fan.energy_consumption",
  "out.electricity.clothes_dryer.energy_consumption",
  "out.electricity.clothes_washer.energy_consumption",
  "out.electricity.cooling_fans_pumps.energy_consumption",
  "out.electricity.cooling.energy_consumption",
  "out.electricity.dishwasher.energy_consumption",
  "out.electricity.freezer.energy_consumption",
  "out.electricity.heating_fans_pumps.energy_consumption",
  "out.electricity.heating_hp_bkup.energy_consumption",
  "out.electricity.heating.energy_consumption",
  "out.electricity.hot_tub_heater.energy_consumption",
  "out.electricity.hot_tub_pump.energy_consumption",
  "out.electricity.hot_water.energy_consumption",
  "out.electricity.lighting_exterior.energy_consumption",
  "out.electricity.lighting_garage.energy_consumption",
  "out.electricity.lighting_interior.energy_consumption",
  "out.electricity.mech_vent.energy_consumption",
  "out.electricity.plug_loads.energy_consumption",
  "out.electricity.pool_heater.energy_consumption",
  "out.electricity.pool_pump.energy_consumption",
  "out.electricity.pv.energy_consumption",
  "out.electricity.range_oven.energy_consumption",
  "out.electricity.refrigerator.energy_consumption",
  "out.electricity.well_pump.energy_consumption",
  "out.fuel_oil.heating_hp_bkup.energy_consumption",
  "out.fuel_oil.heating.energy_consumption",
  "out.fuel_oil.hot_water.energy_consumption",
  "out.natural_gas.clothes_dryer.energy_consumption",
  "out.natural_gas.fireplace.energy_consumption",
  "out.natural_gas.grill.energy_consumption",
  "out.natural_gas.heating_hp_bkup.energy_consumption",
  "out.natural_gas.heating.energy_consumption",
  "out.natural_gas.hot_tub_heater.energy_consumption",
  "out.natural_gas.hot_water.energy_consumption",
  "out.natural_gas.lighting.energy_consumption",
  "out.natural_gas.pool_heater.energy_consumption",
  "out.natural_gas.range_oven.energy_consumption",
  "out.propane.clothes_dryer.energy_consumption",
  "out.propane.heating_hp_bkup.energy_consumption",
  "out.propane.heating.energy_consumption",
  "out.propane.hot_water.energy_consumption",
  "out.propane.range_oven.energy_consumption"
)

# Assuming 'data' is your dataframe that contains all the columns
# Convert all columns to numeric in case they are not already
merge_proj_data[columns_to_sum] <- lapply(merge_proj_data[columns_to_sum], as.numeric)

# Use rowSums to sum across the selected columns for each row
# na.rm=TRUE ensures that NA values are treated as zeros
merge_proj_data$total_energy_consumption <- rowSums(merge_proj_data[, columns_to_sum], na.rm = TRUE)


library(e1071)
library(caret)

colnames(merge_proj_data)
# Predictor variables

# Assuming 'merge_proj_data' is your dataframe

# List of predictor variables
cols_1 <- c("hour","Dry Bulb Temperature [°C]" ,"Relative Humidity [%]", "Wind Speed [m/s]","Wind Direction [Deg]" ,
            "Global Horizontal Radiation [W/m2]","Direct Normal Radiation [W/m2]","Diffuse Horizontal Radiation [W/m2]",   
  'in.sqft', 'in.bedrooms', 'county', 'in.building_america_climate_zone', 'in.ceiling_fan', 
  'in.cooling_setpoint', 'in.cooling_setpoint_has_offset', 'in.cooling_setpoint_offset_magnitude', 
  'in.cooling_setpoint_offset_period', 'in.ducts', 'in.geometry_foundation_type', 
  'in.geometry_wall_type', 'in.has_pv', 'in.heating_fuel', 'in.hot_water_fixtures', 
  'in.hvac_cooling_partial_space_conditioning', 'in.hvac_cooling_type', 'in.hvac_heating_type', 
  'in.hvac_heating_type_and_fuel', 'in.insulation_ceiling', 'in.insulation_wall', 
  'in.lighting', 'in.misc_extra_refrigerator', 'in.misc_freezer', 'in.misc_pool_pump', 
  'in.occupants', 'in.pv_system_size', 'in.refrigerator', 'in.roof_material', 
  'in.usage_level', 'in.vacancy_status', 'in.water_heater_efficiency', 'in.water_heater_fuel'
)

# Exclude the first two columns ('in.sqft' and 'in.bedrooms') from conversion
cols_to_convert <- cols_1[-c(1, 10)]

# Convert the specified columns to factors
merge_proj_data[cols_to_convert] <- lapply(merge_proj_data[cols_to_convert], as.factor)

# Verify the changes and the columns in cols_1
str(merge_proj_data[cols_1])
# Convert in.sqft to numeric
merge_proj_data$in.sqft <- as.numeric(merge_proj_data$in.sqft)


# Splitting the data into training and testing sets
set.seed(123)  # Setting seed for reproducibility
splitIndex <- createDataPartition(merge_proj_data$total_energy_consumption, p = 0.6, list = FALSE)
train_data <- merge_proj_data[splitIndex, ]
test_data <- merge_proj_data[-splitIndex, ]

# Separate predictors and target variable for training and testing sets
train_X <- train_data[1:100,cols_1]
train_Y <- train_data$total_energy_consumption[1:100]
test_X <- test_data[1:100, cols_1]
test_Y <- test_data$total_energy_consumption[1:100]

# Fit SVM model using e1071
# Assuming 'merge_proj_data' is your dataframe

# Fit the SVM regression model
lm_model <- lm(total_energy_consumption ~ hour + `Dry Bulb Temperature [°C]` + `Relative Humidity [%]` + in.sqft +
                 `Wind Speed [m/s]` + `Wind Direction [Deg]` + `Global Horizontal Radiation [W/m2]` +
                 `Direct Normal Radiation [W/m2]` + `Diffuse Horizontal Radiation [W/m2]` + in.bedrooms + `in.building_america_climate_zone` + `in.ceiling_fan` + 
                 `in.cooling_setpoint` + `in.cooling_setpoint_has_offset` + `in.cooling_setpoint_offset_magnitude` + 
                 `in.cooling_setpoint_offset_period` + `in.ducts` + `in.geometry_foundation_type` + 
                 `in.geometry_wall_type` + `in.has_pv` + `in.heating_fuel` + `in.hot_water_fixtures` + 
                 `in.hvac_cooling_partial_space_conditioning` + `in.hvac_cooling_type` + `in.hvac_heating_type` + 
                 `in.hvac_heating_type_and_fuel` + `in.insulation_ceiling` + `in.insulation_wall` + 
                 `in.lighting` + `in.misc_extra_refrigerator` + `in.misc_freezer` + `in.misc_pool_pump` + 
                 `in.occupants` + `in.pv_system_size` + `in.refrigerator` + `in.roof_material` + 
                 `in.usage_level` + `in.vacancy_status` + `in.water_heater_efficiency` + `in.water_heater_fuel`, 
               data = train_data)



# View the model summary
print(lm_model)

# View the model summary

# Predictions on the test data
predictions <- predict(svm_model, test_X)

# Assessing the model's performance
confusionMatrix(predictions, test_Y)

