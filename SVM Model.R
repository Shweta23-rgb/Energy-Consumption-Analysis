library(arrow)
library(tidyverse)
Subset_V4<-read_parquet("/Users/USER/Desktop/IDS Files/Subset_V4.parquet")

# Assuming Subset_V2 is your dataframe
library(e1071)
# Set seed for reproducibility
set.seed(123)

# Split data into training and test sets (e.g., 80% training, 20% test)
train_indices <- sample(1:nrow(Subset_V4), size = 0.7 * nrow(Subset_V4))
train_data <- Subset_V4[train_indices, ]
test_data <- Subset_V4[-train_indices, ]

svm_model <- svm(Final_Energy_KWH ~ ., data = train_data)

# Summary of the Random Forest model
print(svm_model)