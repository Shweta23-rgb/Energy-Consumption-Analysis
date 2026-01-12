library(arrow)
Merged_Final <- read_parquet("/Users/USER/Downloads/Aggregate_Final_Dataset (1).parquet")
cols_4<-c('hour','in.county','Dry Bulb Temperature [°C]','Relative Humidity [%]','Wind Speed [m/s]',
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
          'in.water_heater_fuel',
          'Final_Energy_KWH')

Subset_V4<-Merged_Final[,cols_4]

non_numeric_cols <- sapply(Subset_V4, function(x) !is.numeric(x))
Subset_V4[non_numeric_cols] <- lapply(Subset_V4[non_numeric_cols], as.factor)

str(Subset_V4)

library(arrow)
model_data <- "/Users/USER/Desktop/IDS Files/Subset_V4.parquet"
write_parquet(Subset_V4,model_data)