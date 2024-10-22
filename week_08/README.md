# Install and load necessary packages if you don't have them
#install.packages("readr")  # for reading the file
#install.packages("ggplot2")  # for plotting

library(readr)
library(ggplot2)

# Read the data directly from the NOAA FTP server
url <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_annmean_mlo.txt"
co2_data <- read_table(url, col_names = c("Year", "Mean", "Interpolated"), skip = 57)

head(co2_data)

# Plot the time series of CO₂ annual mean values
ggplot(co2_data, aes(x = Year, y = Mean)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Annual Mean CO₂ Concentration at Mauna Loa",
       x = "Year", y = "CO₂ (ppm)") +
  theme_minimal()

quarto::render("casestudy8draft.qmd",output_format = "all")
