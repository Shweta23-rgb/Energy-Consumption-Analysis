library(caret)
set.seed(123)  # Setting seed for reproducibility
splitIndex <- createDataPartition(Subset_V1$Final_Energy_KWH, p = 0.6, list = FALSE)
train_data <- Subset_V1[splitIndex, ]
test_data <- Subset_V1[-splitIndex, ]

# Separate predictors and target variable for training and testing sets
train_X <- train_data[cols_1]
train_Y <- train_data$Final_Energy_KWH
test_X <- test_data[, cols_1]
test_Y <- test_data$Final_Energy_KWH

model_lm <- lm( Final_Energy_KWH~ ., data = train_data)
summary(model_lm)


#Model 2
library(caret)
set.seed(123)  # Setting seed for reproducibility
splitIndex <- createDataPartition(Subset_V2$Final_Energy_KWH, p = 0.6, list = FALSE)
train_data1 <- Subset_V2[splitIndex, ]
test_data1 <- Subset_V2[-splitIndex, ]

# Separate predictors and target variable for training and testing sets
train_X1 <- train_data1[,cols_2]
train_Y1<- train_data1$Final_Energy_KWH
test_X1 <- test_data1[, cols_2]
test_Y1 <- test_data1$Final_Energy_KWH

library(kernlab)

model_svm <- ksvm(Final_Energy_KWH~ ., data = train_data1)
summary(model_svm)






##
My best accuracy variables : cols_3<-c(
  'Dry Bulb Temperature [°C]',
  'Relative Humidity [%]',
  'Global Horizontal Radiation [W/m2]',
  'in.sqft',
  'in.bedrooms',
  'in.building_america_climate_zone',
  'in.ceiling_fan',
  'in.cooling_setpoint',
  'in.cooling_setpoint_has_offset',
  'in.cooling_setpoint_offset_magnitude',
  #-----------
  'in.clothes_dryer',
  'in.clothes_washer',
  'in.insulation_slab',
  'Wind Speed [m/s]',
  #-------------------
  'in.ducts',
  'in.geometry_foundation_type',
  'in.geometry_wall_type',
  'in.has_pv',
  'in.heating_fuel',
  'in.hot_water_fixtures',
  'in.hvac_cooling_partial_space_conditioning',
  'in.hvac_cooling_type',
  'in.hvac_heating_type',
  #'in.hvac_heating_type_and_fuel',
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

Subset_V3<-Merged_Final[,cols_2]




```{r}
str(Subset_V3)
non_numeric_cols <- sapply(Subset_V3, function(x) !is.numeric(x))
Subset_V3[non_numeric_cols] <- lapply(Subset_V3[non_numeric_cols], as.factor)
str(Subset_V3)



# Example assuming 'energy_consumption' is the target variable
model_lm_3 <- lm( Final_Energy_KWH~ ., data = Subset_V3)
summary(model_lm_3)



```{r}

cols_4<-c('hour','county','Dry Bulb Temperature [°C]','Relative Humidity [%]','Wind Speed [m/s]',
          'Wind Direction [Deg]','Direct Normal Radiation [W/m2]','Diffuse Horizontal Radiation [W/m2]',
          'Global Horizontal Radiation [W/m2]', 'in.sqft',
          'in.bedrooms',
          'in.building_america_climate_zone',
          'in.ceiling_fan',
          'in.cooling_setpoint',
          'in.cooling_setpoint_has_offset',
          'in.cooling_setpoint_offset_magnitude',
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
          'in.water_heater_fuel')

Subset_V4<-Merged_Final[,cols_4]


non_numeric_cols <- sapply(Subset_V4, function(x) !is.numeric(x))
Subset_V4[non_numeric_cols] <- lapply(Subset_V4[non_numeric_cols], as.factor)

str(Subset_V4)

library(arrow)
model_data <- "Subset_V4.parquet"
write_parquet(Subset_V4,model_data)

colnames(Subset_V4)