library(readxl)
temp_EDA <- read_xlsx("/Users/USER/Desktop/IDS Files/aggregate_hourly_cdw.xlsx")
colnames(temp_EDA)

library(caret)
lm_temp_humidity <- lm(`Relative Humidity [%]` ~ `Dry Bulb Temperature [°C]`, data = temp_EDA)
summary(lm_temp_humidity)

lm_temp_WindSpeed <- lm(`Relative Humidity [%]` ~ `Wind Speed [m/s]`, data = temp_EDA)
summary(lm_temp_WindSpeed)

lm_temp_WindDir <- lm(`Relative Humidity [%]` ~ `Wind Direction [Deg]`, data = temp_EDA)
summary(lm_temp_WindDir)

lm_temp_Global_Horizonal <- lm(`Relative Humidity [%]` ~ `Global Horizontal Radiation [W/m2]`, data = temp_EDA)
summary(lm_temp_Global_Horizonal)

lm_temp_Diffuse_Horizontal <- lm(`Relative Humidity [%]` ~ `Diffuse Horizontal Radiation [W/m2]`, data = temp_EDA)
summary(lm_temp_Diffuse_Horizontal)

lm_temp_Direct_Normal <- lm(`Relative Humidity [%]` ~ `Direct Normal Radiation [W/m2]`, data = temp_EDA)
summary(lm_temp_Direct_Normal)