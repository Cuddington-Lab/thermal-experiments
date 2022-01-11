# Generating synthetic temperature sequences with CoSMoS package
# Source: https://cran.r-project.org/web/packages/CoSMoS/vignettes/vignette.html

# Goal: to obtain synthetic temperature sequences with the same 
# distribution and an autocorrelation structure as the real data 
# (2006 temperatures measured by uWaterloo Weather Station)


# Libraries
library(CoSMoS)
library(ggplot2)
library(lubridate)

# Import clean historical temperatures from uWaterloo Weather Station for 2006
# 2006 is the year to be used as a control (no night warming)
historical <- read.csv("https://raw.githubusercontent.com//Cuddington-Lab/thermal-experiments/main/historical.csv",
                     header=TRUE, stringsAsFactors = TRUE)

# Format date as POSIXct
historical$date <- as.POSIXct(historical$date,format="%Y-%m-%d %H:%M:%S")

# Transform temperature to Kelvin to avoid negative values (some of the models
# to be used do not handle negative numbers)
historical$value <- historical$value + 273.15

# Visualize time series 
quickTSPlot(historical$value)
 
# Format dataset so that it is in both dataframe and datatable formats
# I was not sure how to do that using a code, so I used a dataframe within 
# package CoSMos to achieve this format which is required for the model to run
data("precip")
historical2 <- rbind(precip, historical)
N <- 79633
historical2 <- tail(historical2, -N)

# Fit a distribution and an autocorrelation structure to the data

# I tried to fit a normal distribution but it is not working, here is the error:
# Error in numericDeriv(form[[3L]], names(ind), env, ifelse(internalPars <  : 
# Missing value or an infinity produced when evaluating the model

# I used ggamma model and it has a good fit for the month of June
hist_model <- analyzeTS(TS = historical2, season = "month", 
                         dist = "ggamma", acsID = "paretoII", lag.max = 12)

# Check the goodness-of-fit for the distribution (method = "dist"), 
# autocorrelation (method = "acs"), and basic statistics (method = "stat")
reportTS(aTS = hist_model, method = "dist") + theme_light()
reportTS(aTS = hist_model, method = "acs") + theme_light()
reportTS(aTS = hist_model, method = "stat")

# It seems like the autocorrelation model is not fitting very well in June 
# (to be checked; other models to be tested here)

# Simulate temperatures based on the model
# To obtain additional temperature profiles, add longer periods of time to the code:
# from = as.POSIXct(x = "1978-12-01 00:00:00"),
# to = as.POSIXct(x = "2008-12-01 00:00:00")
sim_temp <- simulateTS(aTS = hist_model)

# Change temperatures back to Celsius
historical2$value <- historical$value - 273.15
sim_temp$value <- sim_temp$value - 273.15

# Visualize observed and simulated temperatures
dta <- historical2
dta[, id := "observed"]
sim_temp[, id := "simulated"]
dta <- rbind(dta, sim_temp)
ggplot(data = dta) + geom_line(mapping = aes(x = date, y = value)) + 
  facet_wrap(facets = ~id, ncol = 1) + theme_light()

# Visualize observed and simulated temperatures for June 1 to 5
june.obs <- subset(historical2, (date>="2006-06-01 00:00:01" &
                             date<="2006-06-05 23:59:00"))
june.sim <- subset(sim_temp, (date>="2006-06-01 00:00:01" &
                                   date<="2006-06-05 23:59:00"))
ggplot(NULL, aes(date, value)) + 
    geom_line(data = june.obs, color="green") +
    geom_line(data = june.sim, color="red")


## Create all the treatments based on the temperature regimes obtained above
## For this first trial, I used the real temperatures to create the regimes,
## but all subsequential trials could be based on synthetic temperature sequences

# Format data as dataframe and add a column with the hours of the day
june.obs <- as.data.frame(june.obs)
june.obs$hour <- hour(june.obs$date)

# Temperature regimes were adapted from: Higashi, Barton, & Oliver (2019).  
# Warmer nights offer no respite for a defensive mutualism. J Anim Ecol 89, 1895-1905.
# This article also considers daytime as having 14 hours and nighttime as having 10 hours, so I am using these lenghts as my reference

# Create control treatment
june.obs.control <- june.obs
june.obs.control$id <- gsub("observed", "control", june.obs.control$id)

# Create all-day warming treatment (mean daily increase of 2.5C from control)
june.obs.allday <- june.obs
june.obs.allday$value <- ifelse(june.obs.allday$hour >= 6 & june.obs.allday$hour <= 19,
                               june.obs.allday$value+2.5, june.obs.allday$value+2.5)
june.obs.allday$id <- gsub("observed", "allday", june.obs.allday$id)

# Create night warming treatment (mean daily increase of 0.5C from control)
june.obs.night <- june.obs
june.obs.night$value <- ifelse(june.obs.night$hour >= 6 & june.obs.night$hour <= 19,
                               june.obs.night$value+0, june.obs.night$value+1.2)
june.obs.night$id <- gsub("observed", "night", june.obs.night$id)

# Summary stats
merge <- rbind(june.obs.control, june.obs.allday, june.obs.night)
tapply(merge$value, merge$id,
       function(x) format(summary(x), scientific = FALSE))

# Visualize all treatments
colnames(merge)[3] <- "treatment"
ggplot(data=merge, aes(x=date, y=value, col=treatment)) + geom_line()
