library(arrow)
Merged_Final <- read_parquet("/Users/USER/Desktop/IDS Files/Aggregate_Final_Dataset.parquet")
# Calculate average based on category
averages <- Merged_Final %>%
  group_by(in.occupants) %>%
  summarise(mean_value = mean(Final_Energy_KWH, na.rm = TRUE))

# Display table with averages
averages_table <- as.data.frame(table(Merged_Final$in.occupants))
colnames(averages_table) <- c("Category", "Frequency")
averages_table$Mean_Value <- averages$mean_value

print(averages_table)

unique(Merged_Final$in.occupants)


colnames(Test_Optimied_Variables)