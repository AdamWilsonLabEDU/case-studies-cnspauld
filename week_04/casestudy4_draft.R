#Case Study 4

#load data
library(tidyverse)
library(nycflights13)

#to find the farthest airport graphically
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

#arranged distance within flight table
arrange(.data=flights, by_group = distance)

#used left join to combine flights to airports, as it's a one to many relationship
flights_joined <- left_join(flights, airports, by = c("dest" = "faa"),
                            copy = FALSE, suffix = c(".flights", ".airports"))

#filtered through joined tables to find max distance
flights_joined %>% filter(distance == max(distance)) %>% select(name) %>% distinct()

#renamed max distance to farthest airport, and used distinct() to isolate the one name
farthest_airport <- flights_joined %>% filter(distance == max(distance)) %>% select(name) %>% distinct()

#Honolulu International Airport (HNL)
