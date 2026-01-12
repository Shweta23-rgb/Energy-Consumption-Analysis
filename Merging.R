library(tidyverse)
library(arrow)
library(readxl)
aggregate_hourly_cdw <- read_xlsx("/Users/USER/Downloads/aggregate_hourly_cdw (1).xlsx")
merged_house_Static_energy <- read_xlsx("/Users/USER/Downloads/merged_house_Static_energy (1).xlsx")
Final_Dataset<- merge(aggregate_hourly_cdw,merged_house_Static_energy , by = c("hour","in.county"), all = TRUE)


parquet_out <- "output_file.parquet"
write_csv_arrow(Final_Dataset,parquet_out)




