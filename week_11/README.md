# Required Libraries
library(tidycensus)
library(sf)
library(dplyr)
library(foreach)
library(mapview)

# Step 1: Define the race variables (from the census)
race_vars <- c(
  "Total Population" = "P1_001N",
  "White alone" = "P1_003N",
  "Black or African American alone" = "P1_004N",
  "American Indian and Alaska Native alone" = "P1_005N",
  "Asian alone" = "P1_006N",
  "Native Hawaiian and Other Pacific Islander alone" = "P1_007N",
  "Some Other Race alone" = "P1_008N",
  "Two or More Races" = "P1_009N"
)

# Step 2: Download block-level census data for Erie County, NY (Buffalo)
options(tigris_use_cache = TRUE)  # Cache shapefiles for speed
erie <- get_decennial(
  geography = "block", 
  variables = race_vars, 
  year = 2020,
  state = "NY", 
  county = "Erie County", 
  geometry = TRUE, 
  sumfile = "pl", 
  cache_table = TRUE
)

# Step 3: Crop the spatial data to a specific bounding box
bbox <- st_sfc(
  st_polygon(list(matrix(c(-78.9, 42.888, 
                           -78.85, 42.888, 
                           -78.85, 42.92, 
                           -78.9, 42.92, 
                           -78.9, 42.888), 
                         ncol = 2, byrow = TRUE))),
  crs = st_crs(erie)  # Ensure CRS matches the data
)

# Crop the data to the bounding box
erie_cropped <- st_intersection(erie, bbox)

# Step 4: Loop through each racial group and generate random points
erie_cropped$variable <- as.factor(erie_cropped$variable)  # Convert racial variable to factor

# Initialize an empty list to store the results
results_list <- list()

# Loop over each racial group
foreach(race = levels(erie_cropped$variable), .combine = rbind) %do% {
  # Filter the data for the current race group
  race_data <- erie_cropped %>% filter(variable == race)
  
  # Generate random points for each census block in the current race group
  random_points <- st_sample(race_data, size = nrow(race_data), type = "random")
  
  # Convert random points to sf object
  random_points_sf <- st_as_sf(random_points)
  
  # Add a column for the racial group
  random_points_sf <- random_points_sf %>% mutate(variable = race)
  
  # Append to the results list
  results_list[[race]] <- random_points_sf
}

# Step 5: Combine the results into a single sf object
final_sf <- do.call(rbind, results_list)

# Step 6: Visualize the results using mapview (interactive map)
mapview(final_sf, zcol = "variable", cex = 3)  # You can adjust cex (point size)