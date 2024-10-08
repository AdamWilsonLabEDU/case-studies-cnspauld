library(terra)
library(spData)
library(tidyverse)
library(sf)

library(ncdf4)
download.file("https://crudata.uea.ac.uk/cru/data/temperature/absolute.nc","crudata.nc", method="curl")

# read in the data using the rast() function from the terra package
tmean=rast("crudata.nc")

plot(tmean)

plot(max(tmean))

max_tm <- max(tmean)

data(world)
world_vector <- vect(world)

max_temp <- terra::extract(max_tm, world_vector, fun = max, na.rm = T, small = T)

world_clim <-
  bind_cols(world, max_temp) 

ggplot(world_clim) +
  geom_sf(aes(fill = max_temp[,2])) +
  scale_fill_viridis_c(name="Maximum\nTemperature (C)") +
  theme(legend.position = "bottom")

hottest_continents <-
  world_clim %>%
  group_by(continent, name_long) %>% 
  summarize(temperature = max(max)) %>%
  slice_max(temperature, n = 1) %>%
  arrange(desc(temperature)) %>%
  st_set_geometry(NULL) 