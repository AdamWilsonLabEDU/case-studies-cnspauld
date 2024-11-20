library(tidyverse)
library(htmlwidgets)
library(widgetframe)
library(xts)
library(dygraphs)

# Create a simulated weather data frame
set.seed(123)
dates <- seq(as.Date("2023-01-01"), by = "day", length.out = 365)  # 1 year of data
temperature_max <- rnorm(365, mean = 5, sd = 10)  # Simulated daily max temperatures in Celsius
temperature_min <- rnorm(365, mean = -5, sd = 10)  # Simulated daily min temperatures in Celsius
temperature_mean <- (temperature_max + temperature_min) / 2  # Calculate mean temperature

d <- tibble(
  date = dates,
  daily_temperature_2m_max = temperature_max,
  daily_temperature_2m_min = temperature_min,
  daily_temperature_2m_mean = temperature_mean
)

# Convert data to xts format
d_xts <- d %>%
  select(date, daily_temperature_2m_max, daily_temperature_2m_min, daily_temperature_2m_mean) %>%
  column_to_rownames("date") %>%
  xts(order.by = as.Date(d$date))

# Check the xts object
head(d_xts)

# Create the dygraph
dygraph(d_xts, main = "Daily Maximum Temperature in Buffalo, NY") %>%
  dySeries("daily_temperature_2m_max", label = "Max Temperature") %>%
  dySeries("daily_temperature_2m_min", label = "Min Temperature") %>%
  dySeries("daily_temperature_2m_mean", label = "Mean Temperature") %>%
  dyRangeSelector(dateWindow = c("2023-01-01", "2024-10-31")) %>%
  dyOptions(colors = c("red", "blue", "green"))

# Embed the dygraph into an HTML page
dygraph_plot <- dygraph(d_xts, main = "Daily Maximum Temperature in Buffalo, NY") %>%
  dySeries("daily_temperature_2m_max", label = "Max Temperature") %>%
  dySeries("daily_temperature_2m_min", label = "Min Temperature") %>%
  dySeries("daily_temperature_2m_mean", label = "Mean Temperature") %>%
  dyRangeSelector(dateWindow = c("2023-01-01", "2024-10-31")) %>%
  dyOptions(colors = c("red", "blue", "green"))

# Embed the plot in a widget frame
frameWidget(dygraph_plot)

# Create two linked dygraphs
dygraph_plot_1 <- dygraph(d_xts, main = "Daily Temperature in Buffalo, NY (Max/Min)") %>%
  dySeries("daily_temperature_2m_max", label = "Max Temp") %>%
  dySeries("daily_temperature_2m_min", label = "Min Temp") %>%
  dyRangeSelector(dateWindow = c("2023-01-01", "2024-10-31"))

dygraph_plot_2 <- dygraph(d_xts, main = "Daily Mean Temperature") %>%
  dySeries("daily_temperature_2m_mean", label = "Mean Temp") %>%
  dyRangeSelector(dateWindow = c("2023-01-01", "2024-10-31"))

# Link the two dygraphs
dygraph_plot_1 %>%
  dygraph_plot_2 %>%
  dyLinks()
