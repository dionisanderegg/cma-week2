## Load the necessary libraries ################################################

library(tidyverse)    # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## Import the downloaded csv ##################################################

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",")

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

## Calculate the timelag between GPS fixes and store it as integer in "timelag" ####

wildschwein_BE <- group_by(wildschwein_BE, TierID)

wildschwein_BE <- mutate(wildschwein_BE, timelag = as.integer(
  difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")))

wildschwein_BE <- filter(wildschwein_BE, timelag >= 0)

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

#count timelags per 100s-category
wildschwein_BE_group <- wildschwein_BE %>%
  st_drop_geometry() %>%
  group_by(timelag_round) %>%
  summarise(n = n())

# Show n per timmelag-category in DF with descending n
View(arrange(wildschwein_BE_group, desc(n)))

#visuazlize original data as histogram (LOG-SCALE!)
wildschwein_BE %>%
  filter(timelag < 15000)%>%
  ggplot(aes(timelag)) +
  geom_histogram(binwidth = 50) +
  scale_y_log10() +
  theme_bw()

#visuazlize original data as histogram
wildschwein_BE %>%
  filter(timelag < 15000)%>%
  ggplot(aes(timelag)) +
  geom_histogram(binwidth = 20) +
  theme_bw()

# When did the larger timelags occur?
wildschwein_BE %>%
  ggplot(aes(DatetimeUTC, timelag, col = TierName)) +
  geom_point() +
  geom_line () +
  theme_bw()

# Filter for timelags < 25000s
wildschwein_BE %>%
  filter(timelag < 25000) %>%
  ggplot(aes(DatetimeUTC, timelag, col = TierName)) +
  geom_point() +
  geom_line () +
  theme_bw()
# Plot shosw a large number of "long" timelags for Ruth from May 2015

## Task 2 ########################################################################
# Calculate the euclidian distance and store it in "steplength"
wildschwein_BE$steplength <- sqrt(
  (wildschwein_BE$E - lead(wildschwein_BE$E))^2 + (wildschwein_BE$N - lead(wildschwein_BE$N))^2
  )

# calculate the animals speed by distance and timelag and store it in speed_ms => meters per second
wildschwein_BE$speed_ms <- wildschwein_BE$steplength / wildschwein_BE$timelag

## Task 3 #########################################################################
# Read data and convert to sf
caro60 <- read_delim("caro60.csv", delim = ",")
caro60 <- st_as_sf(caro60, coords = c("E", "N"), crs = 2056, remove = FALSE)

# Subset Date for trajectories of 3, 6, 9 minutes
seq3 <- seq(1,200, by = 3)
seq6 <- seq(1,200, by = 6)
seq9 <- seq(1,200, by = 9)

caro3 <- slice(caro60, seq3)
caro6 <- slice(caro60, seq6)
caro9 <- slice(caro60, seq9)

# Add minutes per trajectory
caro60$Trajectory <- "1 minute"
caro3$Trajectory <- "3 minutes"
caro6$Trajectory <- "6 minutes"
caro9$Trajectory <- "9 minutes"

caro60$timelag <- as.integer(difftime(lead(caro60$DatetimeUTC), caro60$DatetimeUTC, units = "secs"))
caro3$timelag <- as.integer(difftime(lead(caro3$DatetimeUTC), caro3$DatetimeUTC, units = "secs"))
caro6$timelag <- as.integer(difftime(lead(caro6$DatetimeUTC), caro6$DatetimeUTC, units = "secs"))
caro9$timelag <- as.integer(difftime(lead(caro9$DatetimeUTC), caro9$DatetimeUTC, units = "secs"))

caro60$steplength <- sqrt((caro60$E - lead(caro60$E))^2 + (caro60$N - lead(caro60$N))^2)
caro3$steplength <- sqrt((caro3$E - lead(caro3$E))^2 + (caro3$N - lead(caro3$N))^2)
caro6$steplength <- sqrt((caro6$E - lead(caro6$E))^2 + (caro6$N - lead(caro6$N))^2)
caro9$steplength <- sqrt((caro9$E - lead(caro9$E))^2 + (caro9$N - lead(caro9$N))^2)

caro60$speed_ms <- caro60$steplength / caro60$timelag
caro3$speed_ms <- caro3$steplength / caro3$timelag
caro6$speed_ms <- caro6$steplength / caro6$timelag
caro9$speed_ms <- caro9$steplength / caro9$timelag

# Bind rows for graphical analysis
caro <- bind_rows(caro60, caro3, caro6, caro9)

# Show all trajectories => Difficult to read
ggplot(caro, aes (E, N, col = Trajectory)) +
  geom_path() +
  geom_point() +
  theme_bw()

# subset trajectories:
# 1 min vs 3 min
caro %>%
  filter(Trajectory == "1 minute" | Trajectory == "3 minutes") %>%
  ggplot(aes(E,N, col = Trajectory, alpha = Trajectory)) +
  geom_path() +
  geom_point() +
  scale_alpha_manual(values = c (0.33,1)) +
  theme_bw()

# 1 min vs 6 min
caro %>%
  filter(Trajectory == "1 minute" | Trajectory == "6 minutes") %>%
  ggplot(aes(E,N, col = Trajectory, alpha = Trajectory)) +
  geom_path() +
  geom_point() +
  scale_alpha_manual(values = c (0.33,1)) +
  theme_bw()

# 1 min vs 9 min
caro %>%
  filter(Trajectory == "1 minute" | Trajectory == "9 minutes") %>%
  ggplot(aes(E,N, col = Trajectory, alpha = Trajectory)) +
  geom_path() +
  geom_point() +
  scale_alpha_manual(values = c (0.33,1)) +
  theme_bw()

# Speed, depending on the Trajectory timescale => The higer the scale, the lower the maximum speeds!
ggplot(caro, aes(DatetimeUTC, speed_ms, col = Trajectory)) +
  geom_line() +
  geom_point()

## Task 4 ###################################################################################
# Smooth parameters with a moving window funcion
library(zoo)

example <- rnorm(10)
example1 <- rollmean(example, k = 3, fill = NA, allign = "left")
example2 <- rollmean(example, k = 4, fill = NA, allign = "left")
example3 <- rollmean(example, k = 5, fill = NA, allign = "left")

# Apply on speed of caro

caro60$speed_ms_smooth03 <- rollmean(caro60$speed_ms, k = 3, fill = NA, allign = "left")
caro60$speed_ms_smooth04 <- rollmean(caro60$speed_ms, k = 4, fill = NA, allign = "left")
caro60$speed_ms_smooth06 <- rollmean(caro60$speed_ms, k = 6, fill = NA, allign = "left")
caro60$speed_ms_smooth10 <- rollmean(caro60$speed_ms, k = 10, fill = NA, allign = "left")
caro60$speed_ms_smooth15 <- rollmean(caro60$speed_ms, k = 15, fill = NA, allign = "left")
caro60$speed_ms_smooth25 <- rollmean(caro60$speed_ms, k = 25, fill = NA, allign = "left")

caro60_speeds <- gather(caro60, k, speed_ms, speed_ms : speed_ms_smooth25)

ggplot(caro60_speeds, aes(DatetimeUTC, speed_ms, col = k)) +
  geom_line() +
  theme_bw() +
  labs(title = "influence of window sizes on speed", x = "\nDatetimeUTC [hh:mm]", y ="speed in m/s\n" )