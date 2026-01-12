library(arrow)
library(tidyverse)
Subset_V4<-read_parquet("/Users/USER/Desktop/IDS Files/Subset_V4.parquet")

# Assuming Subset_V2 is your dataframe
library(e1071)
# Set seed for reproducibility
set.seed(123)

# Split data into training and test sets (e.g., 60% training, 40% test)
train_indices <- sample(1:nrow(Subset_V4), size = 0.6 * nrow(Subset_V4))
train_data <- Subset_V4[train_indices, ]
test_data <- Subset_V4[-train_indices, ]


library (xgboost)

# Convert training data to DMatrix format
dtrain <- xgb.DMatrix(data = data.matrix(train_data[, -which(names(train_data) == "Final_Energy_KWH")]), 
                      label = train_data$Final_Energy_KWH)

params2 <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 8,
  subsample = 0.5,
  colsample_bytree = 0.5
)

nrounds2 <- 1000  # Number of boosting rounds. Adjust based on your dataset and needs

xgb_model2 <- xgboost(params = params2, data = dtrain, nrounds = nrounds2)

# Assuming you have a trained XGBoost model 'xgb_model' and a test set 'test_data'

# Predict on the test set
dtest2 <- xgb.DMatrix(data = data.matrix(test_data[, -which(names(test_data) == "Final_Energy_KWH")]))
predictions2 <- predict(xgb_model2, dtest2)


(predictions2)
# Compute RMSE
rmse2 <- sqrt(mean((predictions2 - test_data$Final_Energy_KWH)^2))
print(paste("RMSE:", rmse2))

# Compute R-squared
SST2 <- sum((test_data$Final_Energy_KWH - mean(test_data$Final_Energy_KWH))^2)
SSR2 <- sum((predictions2 - test_data$Final_Energy_KWH)^2)
r_squared2<- 1 - SSR2/SST2
print(paste("R-squared:", r_squared2))


# Ensure that the feature names match those used in the model
# You might need to adjust this to match the exact features used
feature_names <- colnames(train_data[, -which(names(train_data) == "Final_Energy_KWH")])

# Obtain feature importance
importance_matrix <- xgb.importance(feature_names = feature_names, model = xgb_model2)
print(importance_matrix)


importance_matrix <- xgb.importance(model = xgb_model2)
print(importance_matrix)
# Visualize feature importance
xgb.plot.importance(importance_matrix)
