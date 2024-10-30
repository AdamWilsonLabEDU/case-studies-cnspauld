library(sf)
library(tidyverse)
library(ggmap)
library(spData)
data(world)
data(us_states)
library(dplyr)
library(lubridate)

# Download a csv from noaa with storm track information
dataurl="https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/csv/ibtracs.NA.list.v04r01.csv"

storm_data <- read_csv(dataurl)

# Extract year, filter, replace -999.0 values with NA, and add decade column
storm_data <- storm_data %>%
  mutate(year = year(ISO_TIME)) %>%
  filter(year >= 1950) %>%
  mutate_if(is.numeric, function(x) ifelse(x == -999.0, NA, x)) %>%
  mutate(decade = (floor(year / 10) * 10))

storm_data <- st_as_sf(storm_data, coords = c("LON", "LAT"), crs = 4326)
region <- st_bbox(storm_data)

# Plot the storm data with a panel for each decade
ggplot() +
  geom_sf(data = world, fill = "lightgrey") +  # Replace `world` with a world shapefile or sf object
  stat_bin2d(data = storm_data, aes(x = st_coordinates(storm_data)[,1], y = st_coordinates(storm_data)[,2]), bins = 100) +
  scale_fill_distiller(palette = "YlOrRd", trans = "log", direction = -1, breaks = c(1, 10, 100, 1000)) +
  facet_wrap(~decade) +
  coord_sf(xlim = region[c(1, 3)], ylim = region[c(2, 4)]) +
  theme_minimal() +
  labs(title = "Storm Tracks by Decade", fill = "Storm Density")

states <- states(cb = TRUE, year = 2021) %>%
  st_transform(st_crs(storm_data)) %>%
  select(state = NAME)  # Renames 'NAME' to 'state'

# Spatial join to link storms with states, group, count unique storms, and sort by storm count
storm_states <- st_join(storm_data, states, join = st_intersects, left = FALSE) %>%
  group_by(state) %>%
  summarize(storms = length(unique(NAME))) %>%
  arrange(desc(storms)) %>%
  slice(1:5)

# Display the top 5 states with the most storms
print(storm_states)
 