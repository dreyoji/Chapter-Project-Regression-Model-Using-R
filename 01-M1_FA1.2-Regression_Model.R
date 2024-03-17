# Purpose: Multiple Regression Model in R
# Authors: Bernardo, Ryoji
#          Reonal, Paul Miguel
#          Santos, Aron Nick S.
# Topics: Data Science / Regression Model
# Packages: readr, dplyr, ggplot2, car
# Data: bank-full.csv
# =========================================================================================================

library(readr)   # For reading data
library(dplyr)   # For data manipulation
library(ggplot2) # For data visualization

# =========================================================================================================

# Load the marketing dataset
data <- read.csv("bank-full.csv", sep = ";", header = TRUE)

# Display the structure and summary of the dataset
str(data)
summary(data)

# Select specific columns from the dataset
selected_data <- data %>% select(duration, previous, age, job, balance)

# Standardize selected columns for better comparison
selected_data[c("duration", "previous", "age", "balance")] <- scale(selected_data[c("duration", "previous", "age", "balance")])

# Display the summary of the standardized data
summary(selected_data)

# Convert the 'job' column to a factor
selected_data$job <- as.factor(selected_data$job)

# =========================================================================================================

# Build a multiple regression model with all predictors and display the summary
model <- lm(balance ~ age + previous + duration, data = selected_data)
summary(model)

# Build a second model with only age and duration as predictors and display the summary
model2 <- lm(balance ~ age + duration, data = selected_data)
summary(model2)

# Create a scatter plot with regression line
ggplot(selected_data, aes(x = age, y = balance, color = job, group = job)) +
  geom_point() +
  geom_hline(yintercept = mean(selected_data$balance)) +
  geom_smooth(method = 'lm')
