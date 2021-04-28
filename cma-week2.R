## Load the necessary libraries ################################################

library(tidyverse)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## Import the downloaded csv ##################################################

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",") # adjust path

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

## Calculate the timelag between GPS fixes and store it as integer in "timelag" ####

wildschwein_BE$timelag <- as.integer(
  difftime(lead(wildschwein_BE$DatetimeUTC), wildschwein_BE$DatetimeUTC, units = "secs"))

## Data inspection ##############################################################
unique(wildschwein_BE$TierID)   # 3 individuals were tracked
unique(wildschwein_BE$TierName)  # the 3 individuals are named Sabi, Rosa and Ruth

# How Long: Sabi between 09-14 and 07- 15, Ruth between 11-14 and 09-15, Rosa between 11-14 and 07-15
ggplot(wildschwein_BE, aes(x=DatetimeUTC, y = TierName)) +
  geom_point()+
  scale_x_datetime(breaks = "1 month", minor_breaks = "1 month")+
  theme_bw()

# Most timelags around 900s, some around 3600s and 10800s, various larger dime differences occurred
wildschwein_BE$timelag_round <- round(wildschwein_BE$timelag/100) *100

#count timelags per 100s-cetegory
wildschwein_BE_group <- wildschwein_BE %>%
  group_by(timelag_round) %>%
  summarise(n = n())

#visuazlize original data as histogram (LOG-SCALE!)
wildschwein_BE %>%
  filter(timelag>=0, timelag < 15000)%>%
  ggplot(aes(timelag)) +
  geom_histogram(binwidth = 50) +
  scale_y_log10() +
  theme_bw()

#visuazlize original data as histogram
wildschwein_BE %>%
  filter(timelag>=0, timelag < 15000)%>%
  ggplot(aes(timelag)) +
  geom_histogram(binwidth = 20) +
  theme_bw()

# When did the larger timelags occur?
wildschwein_BE %>%
  filter(timelag >= 0) %>%
  ggplot(aes(DatetimeUTC, timelag, col = TierName)) +
  geom_point() +
  geom_line () +
  theme_bw()

wildschwein_BE %>%
  filter(timelag >= 0, timelag < 25000) %>%
  ggplot(aes(DatetimeUTC, timelag, col = TierName)) +
  geom_point() +
  geom_line () +
  theme_bw()
# Plot shosw a large number of "long" timelags for Ruth from May 2015
