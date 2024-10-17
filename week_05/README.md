#Case Study 5

library(spData)
library(sf)
library(tidyverse)
library(units) #this one is optional, but can help with unit conversions

#load 'world' data from spData package
data("world")  
# load 'states' boundaries from spData package
data("us_states")
# plot(world[1])  #plot if desired
# plot(us_states[1]) #plot if desired

albers="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

Canada <- world %>%
  filter(name_long == "Canada") %>%
  st_transform(albers) %>%
  st_buffer(dist = 10000)

us_transform <- st_transform(us_states, albers) %>%
  filter(NAME == "New York") 

border <- st_intersection(Canada, us_transform)

ggplot(border) +
  geom_sf(data = us_transform, fill = "grey") +
  geom_sf(fill = "red") +
  labs(title = "New York Land within 10km of Canada",
       subtitle = "Area = 3495.19km^2",
       caption = "Source: spData package")
