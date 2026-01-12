library(arrow)
library(tidyverse)
Subset_V4<-read_parquet("/Users/USER/Desktop/IDS Files/Subset_V4.parquet")
colnames(Subset_V4)
# Assuming Subset_V2 is your dataframe
library(e1071)
# Set seed for reproducibility
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

nrounds <- 1000  # Number of boosting rounds. Adjust based on your dataset and needs

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

unique(Test_Optimied_Variables$in.water_heater_efficiency)
unique()
#Checking for Variables
Test_Optimied_Variables <-test_data
unique(Test_Optimied_Variables$in.pv_system_size)

#Test_Optimied_Variables$in.insulation_wall<-"Brick, 12-in, 3-wythe, R-7"
#Test_Optimied_Variables$in.hvac_cooling_partial_space_conditioning<-"40% Conditioned"
#Test_Optimied_Variables$in.usage_level<-"Low"
#Test_Optimied_Variables$in.hot_water_fixtures<-"100% Usage"
#Test_Optimied_Variables$in.has_pv <- "Yes"
#Test_Optimied_Variables$in.hvac_cooling_type <- "Central AC"
#Test_Optimied_Variables$in.water_heater_fuel <- "Natural Gas"
#Test_Optimied_Variables$in.water_heater_efficiency <-"Electric Heat Pump, 80g gal"
#Test_Optimied_Variables$in.pv_system_size <- "None"
Test_Optimied_Variables$in.pv_system_size[Test_Optimied_Variables$in.pv_system_size == "None"] <- "5.0 kWDC"


dtest2 <- xgb.DMatrix(data = data.matrix(Test_Optimied_Variables[, -which(names(test_data) == "Final_Energy_KWH")]))

predictions1 <- predict(xgb_model, dtest2)
#actual vs predicted reduced due to upgrades
df_new = data.frame(predictions1,test_data$Final_Energy_KWH)
summary(predictions1)
summary(test_data$Final_Energy_KWH)


# Calculate average based on category
averages <- Merged_Final %>%
  group_by(upgrade.clothes_dryer) %>%
  summarise(mean_value = mean(Final_Energy_KWH, na.rm = TRUE))

# Display table with averages
averages_table <- as.data.frame(table(Merged_Final$upgrade.clothes_dryer
))
colnames(averages_table) <- c("Category", "Frequency")
averages_table$Mean_Value <- averages$mean_value

print(averages_table)
