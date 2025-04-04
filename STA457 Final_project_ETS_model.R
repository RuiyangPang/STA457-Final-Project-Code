install.packages("tidyverse")
install.packages("lubridate")
install.packages("forecast")
install.packages("tseries")

library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)

# Load datasets
cocoa_data <- read.csv("Daily Prices_ICCO.csv", stringsAsFactors = FALSE)
cocoa <- cocoa_data
#clean the data the reform the data structure
cocoa_data$Date <- as.Date(cocoa_data$Date, format='%d/%m/%Y')
cocoa_data$Price <- as.numeric(gsub(",", "", cocoa_data$ICCO.daily.price..US..tonne.))
cocoa_data <- cocoa_data %>% select(Date, Price) %>% arrange(Date)
#climate data
climate_data <- read.csv("Ghana_data.csv", stringsAsFactors = FALSE) %>%
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>%
  arrange(DATE)

# Split data into training and testing sets
size <- round(0.95 * nrow(cocoa_data))
train_data <- cocoa_data[1:(nrow(cocoa_data) - 5), ]
test_data <- cocoa_data[(nrow(cocoa_data)- 4): nrow(cocoa_data), ]

#Convert the data into time series data format

start_year <- as.numeric(format(cocoa_data$Date[1], "%Y"))
start_day  <- as.numeric(format(cocoa_data$Date[1], "%j"))

price_ts <- ts(
  data      = cocoa_data$Price,
  start     = c(start_year, start_day),
  frequency = 365
)

####################ETS model##################
cocoa_ets = ets(train_data$Price, model = "MMN")
summary(cocoa_ets)

# plot the decomposition
plot(cocoa_ets)

cocoa_pred = predict(cocoa_ets, nrow(test_data))

library(tibble)

new_test_data <- tibble(
  Date = test_data$Date,
  Price = cocoa_pred$mean )


new_data <- cocoa_data
new_data$Price[(nrow(train_data) + 1):nrow(cocoa_data)] <- cocoa_pred$mean
 
ggplot() +
  geom_line(data = test_data, aes(x = Date, y = Price), color = "black") +
  geom_line(data = new_test_data, aes(x = Date, y = Price), color = "red") +
  labs(title = "ETS Forecast vs Actual Cocoa Prices (Test Set)", y = "Price", x = "Date") +
  theme_minimal()


ggplot(forecast_df_merged, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual Price"), size = 1) +
  geom_line(aes(y = ARIMA_Forecast, color = "ARIMA Forecast"), size = 1) +
  geom_line(aes(y = GARCH_Forecast, color = "GARCH Forecast"), size = 1) +
  geom_line(aes(y = ETS_Forecast, color = "ETS Forecast"), size = 1) +
  labs(title = "ETS Forecast vs Actual Cocoa Prices (Test Set)",
       x = "Date",
       y = "Price",
       color = "Legend") +
  theme_minimal()

