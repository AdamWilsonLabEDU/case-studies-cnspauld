library(terra)
library(rasterVis)
library(ggmap)
library(tidyverse)
library(knitr)
library(sf)
# New Packages
library(ncdf4) # to import data from netcdf format

# Create afolder to hold the downloaded data
dir.create("data",showWarnings = F) #create a folder to hold the data

lulc_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MCD12Q1.051_aid0001.nc?raw=true"
lst_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MOD11A2.006_aid0001.nc?raw=true"

# download them
download.file(lulc_url,destfile="data/MCD12Q1.051_aid0001.nc", mode="wb")
download.file(lst_url,destfile="data/MOD11A2.006_aid0001.nc", mode="wb")

lulc=rast("data/MCD12Q1.051_aid0001.nc",subds="Land_Cover_Type_1")
lst=rast("data/MOD11A2.006_aid0001.nc",subds="LST_Day_1km")

plot(lulc)

lulc=lulc[[13]]
plot(lulc)

Land_Cover_Type_1 = c(
  Water = 0, 
  `Evergreen Needleleaf forest` = 1, 
  `Evergreen Broadleaf forest` = 2,
  `Deciduous Needleleaf forest` = 3, 
  `Deciduous Broadleaf forest` = 4,
  `Mixed forest` = 5, 
  `Closed shrublands` = 6,
  `Open shrublands` = 7,
  `Woody savannas` = 8, 
  Savannas = 9,
  Grasslands = 10,
  `Permanent wetlands` = 11, 
  Croplands = 12,
  `Urban & built-up` = 13,
  `Cropland/Natural vegetation mosaic` = 14, 
  `Snow & ice` = 15,
  `Barren/Sparsely vegetated` = 16, 
  Unclassified = 254,
  NoDataFill = 255)

lcd=data.frame(
  ID=Land_Cover_Type_1,
  landcover=names(Land_Cover_Type_1),
  col=c("#000080","#008000","#00FF00", "#99CC00","#99FF99", "#339966", "#993366", "#FFCC99", 
        "#CCFFCC", "#FFCC00", "#FF9900", "#006699", "#FFFF00", "#FF0000", "#999966", "#FFFFFF", 
        "#808080", "#000000", "#000000"),
  stringsAsFactors = F)
# colors from https://lpdaac.usgs.gov/about/news_archive/modisterra_land_cover_types_yearly_l3_global_005deg_cmg_mod12c1
kable(head(lcd))

# convert to raster (easy)
lulc=as.factor(lulc)

# update the RAT with a left join
#levels(lulc)=left_join(levels(lulc)[[1]],lcd)[-1,]
#activeCat(lulc)=1

# plot it
gplot(lulc)+
  geom_raster(aes(fill=as.factor(value)))+
  scale_fill_manual(values=setNames(lcd$col,lcd$ID),
                    labels=lcd$landcover,
                    breaks=lcd$ID,
                    name="Landcover Type")+
  coord_equal()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(ncol=1,byrow=TRUE))

plot(lst[[1:12]])

scoff(lst)=cbind(0.02,-273.15)
plot(lst[[1:10]])

#Part 1
lw <- data.frame(x = -78.791547, y = 43.007211) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

raster_crs <- st_crs(lst) # get the CRS of the raster
lw_proj <- st_transform(lw, crs = raster_crs)

lst_values <- extract(lst, lw_proj, buffer = 1000, fun = mean, na.rm = TRUE)

lst_values_transposed <- t(lst_values)[-1]

dates <- time(lst)

df <- data.frame(date = dates, lst_value = as.vector(lst_values_transposed))

ggplot(df, aes(x = date, y = lst_value)) +
  geom_point() +  # raw data points
  geom_smooth(span = 0.03, n = 100, method = "loess") +  # smooth curve
  labs(x = "Date", y = "LST Value", title = "LST over Time at Specified Location") +
  theme_minimal()

#Part 2

lst_month <- tapp(lst, index = "month", fun = mean, na.rm = TRUE)

names(lst_month) <- month.name[as.numeric(str_replace(names(lst_month), "m_", ""))]

gplot(lst_month) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient() +
  coord_equal()

monthly_mean <- global(lst_month, fun = mean, na.rm = TRUE)

#Part 3

lulc2 <- resample(lulc, lst_month, method= "near")

lcds1=cbind.data.frame(
  values(lst_month),
  ID=values(lulc2[[1]]))%>%
  na.omit()

lcds1_tidy <- lcds1 %>%
  gather(key='month',value='value',-Land_Cover_Type_1_13)

lcds1_tidy <- lcds1_tidy %>%
  mutate(ID = as.numeric(-Land_Cover_Type_1_13),  # Convert Land Cover ID to numeric
    month = factor(month, levels = month.name, ordered = TRUE))
    
lcds1_tidy_joined <- lcds1_tidy %>%
  left_join(lcd, by = "ID")

ggplot(lcds1_tidy_joined, aes(x = month, y = value, color = Land_Cover_Type_1_13, group = Land_Cover_Type_1_13)) +
  geom_line() +  # Line plot to show trends over months
  geom_point() +  # Points for each data point
  labs(
    title = "Monthly Variability in LST between Land Cover Types",
    x = "Month",
    y = "LST Value",
    color = "Land Cover Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )    
 