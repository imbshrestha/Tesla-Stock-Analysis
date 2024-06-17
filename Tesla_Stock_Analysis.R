# Set working directory (optional if already set in RStudio)
setwd("/Users/bimalshrestha/Downloads/")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Read the Tesla dataset
tesla_data <- read.csv("Tesla Dataset.csv")

# Convert Date column to Date type
tesla_data <- tesla_data %>%
  mutate(Date = ymd(Date))

# Display the first few rows of the dataset
print(head(tesla_data))

# Summary statistics of the dataset
print(summary(tesla_data))

# Plot the closing prices over time
ggplot(tesla_data, aes(x = Date, y = Close)) +
  geom_line() +
  labs(title = "Tesla Stock Closing Prices Over Time",
       x = "Date",
       y = "Closing Price")

# Calculate and plot the daily returns
tesla_data <- tesla_data %>%
  mutate(Return = (Close - lag(Close)) / lag(Close))

ggplot(tesla_data, aes(x = Date, y = Return)) +
  geom_line() +
  labs(title = "Tesla Stock Daily Returns Over Time",
       x = "Date",
       y = "Daily Return")
  # Read the data from the CSV file
Tesla_Stock_Analysis <- read.csv("Tesla Dataset.csv")

# Convert the 'Date' column to a Date object
Tesla_Stock_Analysis$Date <- as.Date(Tesla_Stock_Analysis$Date, format="%Y-%m-%d")

# Plot the closing price over time
plot(Tesla_Stock_Analysis$Date, Tesla_Stock_Analysis$Close, type="l", xlab="Date", ylab="Closing Price", main="Tesla Stock Closing Price Over Time")
     
# Assuming Tesla_Stock_Analysis contains the historical data
# Convert 'Date' column to Date object if not already done
Tesla_Stock_Analysis$Date <- as.Date(Tesla_Stock_Analysis$Date, format="%Y-%m-%d")

# Load the forecast package
library(forecast)
# Convert the 'Date' column to a Date object
Tesla_Stock_Analysis$Date <- as.Date(Tesla_Stock_Analysis$Date, format="%Y-%m-%d")

# Create a time series object
ts_data <- ts(Tesla_Stock_Analysis$Close, frequency = 1)

# Fit an ARIMA model
fit <- auto.arima(ts_data)

# Forecast future closing prices
future_forecast <- forecast(fit, h = 20)  # Forecasting the next 10 periods

# Plot the forecast
plot(future_forecast, xlab = "Date", ylab = "Closing Price", main = "Forecasted Tesla Stock Closing Prices")
