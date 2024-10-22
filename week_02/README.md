#load tidyverse and ggplot libraries 
library("ggplot2")
library("tidyverse")

#define the link to the data 
dataurl="https://data.giss.nasa.gov/tmp/gistemp/STATIONS_v4/tmp_USW00014733_14_0_1/station.csv"

#the next line tells the NASA site to create the temporary file
httr::GET("https://data.giss.nasa.gov/cgi-bin/gistemp/stdata_show_v4.cgi?id=USW00014733&ds=14&dt=1")

# the next lines download the data
temp=read_csv(dataurl, 
              na="999.90", # tell R that 999.90 means missing in this dataset
              skip=1, # we will use our own column names as below so we'll skip the first row
              col_names = c("YEAR","JAN","FEB","MAR", # define column names 
                            "APR","MAY","JUN","JUL",  
                            "AUG","SEP","OCT","NOV",  
                            "DEC","DJF","MAM","JJA",  
                            "SON","metANN"))
# renaming is necessary because they used dashes ("-")
# in the column names and R doesn't like that.

View(temp)

#graph the annual mean temp in June, July, and August (JJA)

ggplot(temp, aes(x = YEAR, y = JJA)) +
  geom_path() + #connects observations in the order in which they appear in the data
  geom_smooth() + #adds a smooth line
  ylab("Mean Summer Temperatures (C)") + 
  ggtitle("Mean Summer Temperatures in Buffalo, NY", "Summer includes June, July, and August Data from the Global
          Historical Climate Network
          Blue line is a LOESS smooth") #added comma between title and subheadings
#based off of the data, summers are hotter now than they were in the 1920s 