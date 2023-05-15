# Historical temperature analyses of recent records (2012 to 2022) to obtain standard deviations 
# of hourly temperaturesin weekly intervals, in the Waterloo and Toronto Regions (Southern Ontario)

# Enabling packages
library(weathercan)
library(dplyr)
library(lubridate)
library(tidyr)

# Obtaining data within 100km from Waterloo
near_stations <- stations_search(coords = c(43.466667, -80.516670), 
                             interval = "hour", dist = 100,ends_earliest=2000) 
near_stations<-as.data.frame(near_stations)

# Downloading data
download.data<-weather_dl(station_ids = near_stations$station_id,
                          start = "2012-01-01", end = "2022-12-31", 
                          interval="hour") 
download.data<-as.data.frame(download.data)

# Obtaining standard deviations for weekly periods
weekly_data <- download.data %>%
  mutate(year = year(time), month = month(time), week = week(time)) %>%
  unite_("date", c("year", "month", "week"), sep ="-") %>%
  group_by(date,station_name) %>%
  summarise(temp = sd(temp, na.rm = TRUE))

# Separating date information into different columns
weekly_data <- weekly_data %>% 
  separate(date, c('year', 'month', 'week'), sep = '[-]')

# Excluding missing data
weekly_data <- subset(weekly_data,!temp=="NA")

# Visualize distributions of standard deviations
hist(weekly_data$temp,prob=T,breaks=7)

# Using temperature data from over 12 thousand whether stations, we conclude that 
# over 20% of the standard deviations of hourly temperatures in Ontario is between 2-4C.
# We used a value of 2.5C across the autocorrelation experiment because:
# 1) it is a commonly occurring standard deviation of temperatures for the region across the year
# 2) Climate change-related studies predict a decrease in standard deviation within the next
# years for Canada

# "Variance is decreasing most rapidly in the high northern latitudes, especially 
# in Canada and Russia" (Duffy, Gouhier, & Ganguly, 2022; Hansen et al = below)
# Hansen, J., Sato, M. & Ruedy, R. Perception of climate change. Proc. Natl Acad. Sci. USA 109, E2415-E2423 (2012).
# "In the northern latitudes, variance and autocorrelation exhibit opposite temporal 
# trends. The decreasing variance may be attributed to a decrease in high-frequency 
# variability and more rapid warming of the lower than the upper quantiles of the 
# temperature distribution. Studies of reanalysis data and observations have also 
# implicated decreasing cold-season sub-seasonal variability and rapidly warming 
# cold days in decreasing temperature variability in mid to high northern latitude.
# Duffy, K., Gouhier, T.C. & Ganguly, A.R. Climate-mediated shifts in temperature 
# fluctuations promote extinction risk. Nat. Clim. Chang. 12, 1037-1044 (2022)."
# https://doi.org/10.1038/s41558-022-01490-7

write.csv(download.data,"C:/Users/user/Desktop/hourly_data.csv", row.names=FALSE)
