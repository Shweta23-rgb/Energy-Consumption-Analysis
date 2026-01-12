library(gbm)
library(caret)

# Create the training matrix
train_matrix <- data.matrix(train_data[, -which(names(train_data) == "Final_Energy_KWH")])

# Define the parameters for the Gradient Boosting model
gbm_params <- list(
  distribution = "gaussian",
  n.trees = nrounds,
  interaction.depth = 8,
  shrinkage = 0.1,
  bag.fraction = 0.5
)

# Train the Gradient Boosting model
gbm_model <- gbm(
  formula = Final_Energy_KWH ~ .,
  data = train_data,
  distribution = gbm_params$distribution,
  n.trees = gbm_params$n.trees,
  interaction.depth = gbm_params$interaction.depth,
  shrinkage = gbm_params$shrinkage,
  bag.fraction = gbm_params$bag.fraction,
  verbose = FALSE
)


# Predict on the test set
test_matrix <- data.matrix(test_data[, -which(names(test_data) == "Final_Energy_KWH")])
predictions_gbm <- predict(gbm_model, newdata = test_data, n.trees = gbm_params$n.trees)

# Compute RMSE
rmse_gbm <- sqrt(mean((predictions_gbm - test_data$Final_Energy_KWH)^2))
print(paste("RMSE (GBM):", rmse_gbm))

# Compute R-squared
SST_gbm <- sum((test_data$Final_Energy_KWH - mean(test_data$Final_Energy_KWH))^2)
SSR_gbm <- sum((predictions_gbm - test_data$Final_Energy_KWH)^2)
r_squared_gbm <- 1 - SSR_gbm/SST_gbm
print(paste("R-squared (GBM):", r_squared_gbm))

importance_summary <- summary(gbm_model)
importance_summary

