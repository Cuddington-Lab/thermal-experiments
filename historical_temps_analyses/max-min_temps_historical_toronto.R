# HISTORICAL TEMPERATURE ANALYSIS: TORONTO AIRPORT DATA FROM 1950 TO 2012
# QUESTION: HAS THE AMPLITUDE BETWEEN MAXIMUM AND MINIMUM DAILY SUMMER TEMPERATURES CHANGED ACROSS YEARS?
# A REDUCTION IN THE AMPLITUDE (MAX - MIN) INDICATES THAT MIN TEMPERATURES ARE DISPROPORTIONALLY INCREASING IN RELATION TO MAX TEMPERATURES

# SUMMARY OF PROCEDURES:
# a GAM applied to each year
# (actual temperature - residual) to detrend the temperature data (tends to increase as days pass by during the summer)
# detrended data: GAM to understand pattern for max-min (temperature amplitude) across years

# SUMMARY OF RESULTS: GAM indicates a reduction in the amplitude or daily temperature range, especially after the 90s

#relevant reading:
#https://noamross.github.io/gams-in-r-course/chapter2
#https://fromthebottomoftheheap.net/blog/
#https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/
#https://www.frontiersin.org/articles/10.3389/fevo.2018.00149/full

#download temperature data (saved on May 11, 2022 from:
#https://climate.weather.gc.ca/historical_data/search_historic_data_e.html)
hist <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/historical_temps_analyses/historical_temps_toronto.csv", header=TRUE, stringsAsFactors = TRUE)
hist$LOCAL_DATE <- as.Date(hist$LOCAL_DATE, format = "%m/%d/%Y")
hist <- hist[!is.na(hist$MEAN_TEMPERATURE),]
hist$JULIAN <- as.numeric(format(hist$LOCAL_DATE, "%j"))

#to avoid disproportional effects caused by extreme minimum temperatures, we subset and use the summer months only
histsummer <- subset(hist, LOCAL_MONTH>=5 & LOCAL_MONTH<=7)
plot(histsummer[1:90,13])
#(may,june,july) = months selected for analysis
#non-linear detrend was chosen: GAM (linear models did not capture the shape of the data for most years)

#separate each of the historical years in a list
year_list <- split(histsummer, f = histsummer$LOCAL_YEAR)

#loop to run a GAM to detrend summer temperatures within each year
all_residmax <- list()
all_residmin <- list()
all_pmax <- list()
all_pmin <- list()
all_results <- list()
pmax <- data.frame(year=integer(),edf=double(),Ref.df=double(),F=double(),pvalue=double())
pmin <- data.frame(year=integer(),edf=double(),Ref.df=double(),F=double(),pvalue=double())

count = 1
for (i in year_list) {
  
  #fit a gam  mod (max or min ~ julian day)
  library(mgcv)
  gammax <- gam(i[, 15] ~ s(i[, 37],bs="tp",k=30), method="REML")
  gammin <- gam(i[, 13] ~ s(i[, 37],bs="tp",k=30), method="REML")
  
  #save detrended data for each year
  resultmax <- residuals(gammax)
  resultmin <- residuals(gammin)
  all_residmax[[count]] = as.data.frame(resultmax)
  all_residmin[[count]] = as.data.frame(resultmin)
  
  result <- summary(gammax)$s.table
  all_results[[count]] = as.data.frame(result)
  pmax[count,1] = i[1,"LOCAL_YEAR"]
  pmax[count,2] = all_results[[count]][["edf"]][1]
  pmax[count,3] = all_results[[count]][["Ref.df"]][1]
  pmax[count,4] = all_results[[count]][["F"]][1]
  pmax[count,5] = all_results[[count]][["p-value"]][1]
  
  result <- summary(gammin)$s.table
  all_results[[count]] = as.data.frame(result)
  pmin[count,1] = i[1,"LOCAL_YEAR"]
  pmin[count,2] = all_results[[count]][["edf"]][1]
  pmin[count,3] = all_results[[count]][["Ref.df"]][1]
  pmin[count,4] = all_results[[count]][["F"]][1]
  pmin[count,5] = all_results[[count]][["p-value"]][1]
  
  gam.check(gammax)
  gam.check(gammin)
  
  count=count+1
}


# p-values are all significant (a horizontal line can't go through the response
# variable confidence interval) across all years
# residuals are well distributed (no hidden trends which have not been caught by the model)
# distribution does not deviate a lot from normal

sum(pmax$edf > 2)
sum(pmin$edf > 2)
# complexity of ~80% models suggests that a non-linear model is appropriate in most cases
# (models are wiggly; most edf values are greater than 1, even 2)
# out of 63 years, 47 (max temp) and 53 (min temp) have edf values greater than 2
summary(gammax)
summary(gammin)

#edf in GAM models:
#This is a value between 0 and infinity and is a sort of mathematical transformation of λ. 
#The higher the edf, the more non-linear is the smoothing spline (Zuur, 2009)
#https://link.springer.com/content/pdf/10.1007/978-0-387-87458-6.pdf

#"The effective degrees of freedom (edf) estimated from generalized additive models were used as a proxy for the degree of non-linearity in stressor-response 
#relationships. (a) An edf of 1 is equivalent to a linear relationship, (b) an edf > 1 and ≤ 2 is a weakly non-linear relationship, and (c) an edf > 2 indicates 
#a highly non-linear relationship" (Zuur et al. 2009).

plot(gammax)
plot(gammin)

#create a new dataframe containing detrended temperature data and respective time periods
detrendedmax <- unlist(all_residmax)
detrendedmin <- unlist(all_residmin)
detrendedmax <- as.data.frame(detrendedmax)
detrendedmin <- as.data.frame(detrendedmin)

detrended <- as.data.frame(cbind(LOCAL_YEAR=histsummer$LOCAL_YEAR,
                                 JULIAN=histsummer$JULIAN,RESMAX=detrendedmax$detrendedmax,
                                 RESMIN=detrendedmin$detrendedmin,MIN=histsummer$MIN_TEMPERATURE,
                                 MAX=histsummer$MAX_TEMPERATURE))
detrended$LOCAL_DATE <- histsummer$LOCAL_DATE

#detrended temperatures are derived from subtracting the trend from the data
#https://kevinkotze.github.io/ts-5-tut/
detrended$MAXDET <- detrended$MAX - detrended$RESMAX
detrended$MINDET <- detrended$MIN - detrended$RESMIN

#run a gam to find if the max-min amplitude has changed throughout the years
library(mgcv)

gam.model <- gam(I(MAXDET-MINDET) ~ s(LOCAL_YEAR,bs="tp",k=30)+s(JULIAN,bs="tp",k=30), 
                 data=detrended, method="REML")

#The plots generated by mgcv's plot() function are partial effect plots. 
#That is, they show the component effect of each of the smooth or linear terms 
#in the model, which add up to the overall prediction.
#https://noamross.github.io/gams-in-r-course/chapter2
plot(gam.model, pages=1, shade = TRUE, shift = coef(gam.model)[1])
par(mfrow = c(1, 4))
gam.check(gam.model)
summary(gam.model)

#plot actual and predicted data
detrended$pred.gam = predict(gam.model)
library(ggplot2)
ggplot(detrended, aes(LOCAL_DATE, (MAXDET-MINDET))) +
  ylab("Maximum - minimum temperature (?C)") +
  xlab("Time (summer days)") +
  geom_point() +
  geom_line(aes(y = pred.gam), size = 1, col = "blue")
