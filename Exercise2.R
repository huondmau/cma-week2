library("readr")
library("sf")
library(dplyr)
library(ggplot2)

wildschwein_BE <- read_delim("Datasets/wildschwein_BE_2056.csv", ",")

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056)

str(wildschwein_BE)

## TASK 2: Getting an overview -------------------------------------------------

difftime_secs <- function(later, now){
  as.numeric(difftime(later, now, units = "secs"))
}

## compute time differences between observations
wildschwein_BE <-wildschwein_BE %>%
  group_by(TierID) %>% 
  mutate(timelag = difftime_secs(lead(DatetimeUTC), DatetimeUTC))

#Now inspect your data in more detail. Try to answer the following questions:
  
  #How many individuals were tracked?

wildschwein_BE %>% 
  group_by(TierName)

## ANSWER: 3 individuals were tracked


  #For how long were the individual tracked?

wildschwein_BE %>% 
  group_by(TierName) %>% 
  summarise(TimeTracked = difftime_secs(max(DatetimeUTC), min(DatetimeUTC)))

# 1 Rosa  20275167 secs
# 2 Ruth  22607072 secs
# 3 Sabi  29253602 secs

# Are there gaps?

#ANSWER: quite some gaps. Sabi was tracked 1.44 times longer than Rosa
   
   
  #Were all individuals tracked concurrently or sequentially?

p1 <- wildschwein_BE %>%
  ggplot(aes(timelag, DatetimeUTC,  col = TierName)) +
  geom_point() +
  theme_minimal()
p1

# ANSWER: either way. Sabi was tracked first (sequentially) while the other two's were tracked concurrently at a later point.


# What is the temporal sampling interval between the locations?

summary(wildschwein_BE$timelag)

 # ANSWER: The mean temporal sampling interval is 1408 seconds.

# TASK: 3 Distance between locations -------------------------------------------

# Similar to how we calculated the timelag between subsequent locations, we can calculate the distance like so:
later <- lag(wildschwein_BE$geometry)
now <- wildschwein_BE$geometry

st_distance(later, now, by_element = TRUE)  # by_element must be set to TRUE

# wrap the output in as.numeric().
distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

# Use this function to create a new column named steplength with the distance between locations.

## compute time differences between observations
wildschwein_BE <- wildschwein_BE %>%
  group_by(TierID) %>% 
  mutate(steplength = distance_by_element(lead(geometry), geometry))

# Task 4: Deriving distance & speed --------------------------------------------

#  let’s calculate the Euclidean distance between subsequent locations using the function st_distance() with the option by_element = TRUE

# Calculate Euclidean distance between subsequent locations
wildschwein_BE$steplength <- st_distance(wildschwein_BE$geometry, lag(wildschwein_BE$geometry, default = NA), by_element = TRUE)

# Calculate speed based on steplength and timelag
wildschwein_BE$speed <- wildschwein_BE$steplength / wildschwein_BE$timelag

# Print the first few rows to check the results
head(wildschwein_BE)

# Task 5: Plausibility Check ---------------------------------------------------

# use slice() to extract just a couple of rows

wildschwein_sample <- wildschwein_BE |>
  filter(TierName == "Sabi") |> 
  head(100)

# visualise the sample using tmap with the *view” mode:

library(tmap)
tmap_mode("view")

tm_shape(wildschwein_sample) + 
  tm_dots()

# casting our points to lines to see the sequence of these samples

wildschwein_sample_line <- wildschwein_sample |> 
  # dissolve to a MULTIPOINT:
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING")

tmap_options(basemaps = "OpenStreetMap")

tm_shape(wildschwein_sample_line) +
  tm_lines() +
  tm_shape(wildschwein_sample) + 
  tm_dots()

# EXERCISE B--------------------------------------------------------------------

# import data

caro <- read_delim("Datasets/caro60.csv", ",") |>
  st_as_sf(coords = c("E","N"), crs = 2056) |> 
  select(DatetimeUTC)

caro

# Calculate timelag using a sampling window of 120 seconds
caro <- caro %>%
  mutate(timelag = difftime_secs(lead(DatetimeUTC, default = last(DatetimeUTC)),
                                 lag(DatetimeUTC, default = first(DatetimeUTC))))

# calculate steplength
caro <- caro %>% 
  mutate(steplength = st_distance(lead(geometry), lag(geometry, default = first(geometry)), by_element = TRUE))

# calculate speed
caro <- caro %>% 
  mutate(speed = steplength / timelag)

caro

# Task 2: Calculate speed at scale 2--------------------------------------------

# w = 240s -> using an offset of 2.

# Calculate timelag using a sampling window of 240 seconds
caro <- caro %>%
  mutate(timelag2 = difftime_secs(lead(DatetimeUTC, n = 2, default = last(DatetimeUTC)),
                            lag(DatetimeUTC, n = 2, default = first(DatetimeUTC))))
# calculate steplength
caro <- caro %>% 
  mutate(steplength2 = st_distance(lead(geometry, n = 2), lag(geometry, n = 2, default = first(geometry)), by_element = TRUE))

# calculate speed
caro <- caro %>% 
  mutate(speed2 = steplength2 / timelag2)

# show results
caro |> 
  st_drop_geometry() |> 
  select(timelag2, steplength2, speed2) |> 
  head()

# Task 3: Calculate speed at scale 3 -------------------------------------------

# w = 380s -> using an offset of 4.

# Calculate timelag using a sampling window of 480 seconds
caro <- caro %>%
  mutate(timelag3 = difftime_secs(lead(DatetimeUTC, n = 4, default = last(DatetimeUTC)),
                                  lag(DatetimeUTC, n = 4, default = first(DatetimeUTC))))
# calculate steplength
caro <- caro %>% 
  mutate(steplength3 = st_distance(lead(geometry, n = 4), 
                                   lag(geometry, n = 4, default = first(geometry)), by_element = TRUE))

# calculate speed
caro <- caro %>% 
  mutate(speed3 = steplength3 / timelag3)

# show results
caro |> 
  st_drop_geometry() |> 
  select(timelag3, steplength3, speed3) |> 
  head()

# Task 4: Compare speed across scales ------------------------------------------

# process the data

caro <- caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)

library(units)

ggplot(caro, aes(y = speed)) + 
  # we remove outliers to increase legibility, analogue
  # Laube and Purves (2011)
  geom_boxplot(outliers = FALSE)

library(tidyr)

# before pivoting, let's simplify our data.frame
caro2 <- caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)

caro_long <- caro2 |> 
  pivot_longer(c(speed, speed2, speed3))

ggplot(caro_long, aes(name, value)) +
  # we remove outliers to increase legibility, analogue
  # Laube and Purves (2011)
  geom_boxplot(outliers = FALSE)

# Exercise C: import own movement data------------------------------------------

#Acquire data and save it to a subfolder of your current R Project named data

# rm(list = ls())
library(tidyverse)
library(here)
library(XML)
library(lubridate)
library(ggmap)
library(geosphere)
options(digits.secs = 3)
options(scipen = 999)

# parse GPX file
path_tmp <- paste0("Data/Fahrt_am_Morgen.gpx")
parsed <- htmlTreeParse(file = path_tmp, useInternalNodes = TRUE)

# get values via via the respective xpath
coords <- xpathSApply(parsed, path = "//trkpt", xmlAttrs)
elev   <- xpathSApply(parsed, path = "//trkpt/ele", xmlValue)
ts_chr <- xpathSApply(parsed, path = "//trkpt/time", xmlValue)

# combine into df 
dat_df <- data.frame(
  ts_POSIXct = ymd_hms(ts_chr, tz = "EST"),
  lat = as.numeric(coords["lat",]), 
  lon = as.numeric(coords["lon",]), 
  elev = as.numeric(elev)
)
head(dat_df)

# Plot the data using ggplot
ggplot(dat_df, aes(x = lon, y = lat)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Spatial Data Plot")



