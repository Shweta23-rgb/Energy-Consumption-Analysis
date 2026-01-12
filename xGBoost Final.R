

cols_4<-c('hour','in.county','Dry Bulb Temperature [°C]','Relative Humidity [%]','Wind Speed [m/s]',
          'Wind Direction [Deg]','Direct Normal Radiation [W/m2]','Diffuse Horizontal Radiation [W/m2]',
          'Global Horizontal Radiation [W/m2]', 'in.sqft',
          'in.bedrooms',
          'in.building_america_climate_zone',
          'in.ceiling_fan',
          'in.clothes_dryer',
          'in.clothes_washer',
          'in.cooling_setpoint',
          'in.cooling_setpoint_has_offset',
          'in.cooling_setpoint_offset_magnitude',
          'in.dishwasher',
          'in.ducts',
          'in.geometry_foundation_type',
          'in.geometry_wall_type',
          'in.has_pv',
          'in.heating_fuel',
          'in.hot_water_fixtures',
          'in.hvac_cooling_partial_space_conditioning',
          'in.hvac_cooling_type',
          'in.hvac_heating_type',
          'in.hvac_heating_type_and_fuel',
          'in.infiltration',
          'in.insulation_ceiling',
          'in.insulation_wall',
          'in.lighting',
          'in.misc_extra_refrigerator',
          'in.misc_freezer',
          'in.misc_pool_pump',
          'in.occupants',
          'in.pv_system_size',
          'in.refrigerator',
          'in.roof_material',
          'in.usage_level',
          'in.vacancy_status',
          'in.water_heater_efficiency',
          'in.water_heater_fuel',
          'Final_Energy_KWH'
          
)

Subset_V4<-Merged_Final[,cols_4]

non_numeric_cols <- sapply(Subset_V4, function(x) !is.numeric(x))
Subset_V4[non_numeric_cols] <- lapply(Subset_V4[non_numeric_cols], as.factor)

#xGBoost Model
set.seed(123)

# Split data into training and test sets (e.g., 80% training, 20% test)
train_indices <- sample(1:nrow(Subset_V4), size = 0.7 * nrow(Subset_V4))
train_data <- Subset_V4[train_indices, ]
test_data <- Subset_V4[-train_indices, ]


library (xgboost)

# Convert training data to DMatrix format
dtrain <- xgb.DMatrix(data = data.matrix(train_data[, -which(names(train_data) == "Final_Energy_KWH")]), 
                      label = train_data$Final_Energy_KWH)

params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 8,
  subsample = 0.5,
  colsample_bytree = 0.5
)

nrounds <- 3000  # Number of boosting rounds. Adjust based on your dataset and needs

xgb_model <- xgboost(params = params, data = dtrain, nrounds = nrounds)

summary(xgb_model)


# Assuming you have a trained XGBoost model 'xgb_model' and a test set 'test_data'

# Predict on the test set
dtest <- xgb.DMatrix(data = data.matrix(test_data[, -which(names(test_data) == "Final_Energy_KWH")]))
predictions <- predict(xgb_model, dtest)

# Compute RMSE
rmse <- sqrt(mean((predictions - test_data$Final_Energy_KWH)^2))
print(paste("RMSE:", rmse))

# Compute R-squared
SST <- sum((test_data$Final_Energy_KWH - mean(test_data$Final_Energy_KWH))^2)
SSR <- sum((predictions - test_data$Final_Energy_KWH)^2)
r_squared <- 1 - SSR/SST
print(paste("R-squared:", r_squared))


feature_names <- colnames(train_data[, -which(names(train_data) == "Final_Energy_KWH")])

# Obtain feature importance
importance_matrix <- xgb.importance(feature_names = feature_names, model = xgb_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix)
