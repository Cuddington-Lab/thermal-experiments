# HISTORICAL TEMPERATURE ANALYSIS: TORONTO AIRPORT DATA FROM 1950 TO 2012
# QUESTION: HAS THE AMPLITUDE BETWEEN MAXIMUM AND MINIMUM DAILY SUMMER TEMPERATURES CHANGED ACROSS YEARS?
# A REDUCTION IN THE AMPLITUDE (MAX - MIN) INDICATES THAT MIN TEMPERATURES ARE DISPROPORTIONALLY INCREASING IN RELATION TO MAX TEMPERATURES

# SUMMARY OF PROCEDURES:
# a linear model applied to each year
# (actual temperature - residual) to detrend the temperature data (tends to increase as days pass by during the summer)
# detrended data: GAM to understand pattern for max-min (temperature amplitude) across years

# SUMMARY OF RESULTS: GAM indicates a reduction in the amplitude or daily temperature range, especially after the 90s; 
# however, several r-squared values within the yearly linear regressions may be too low (share this with Kim; try other possibilities instead of linear regression)

#keep reading:
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
##(may,june,july)
##(or use a non-linear detrend / fit a parabola / or a gam

#separate each of the historical years in a list
year_list <- split(histsummer, f = histsummer$LOCAL_YEAR)
#check fits here

#loop to run a linear model to detrend summer temperatures within each year
all_resultsmax <- list()
all_resultsmin <- list()
all_rsqmax <- list()
all_rsqmin <- list()
all_confintmax <- list()
all_confintmin <- list()
all_predmax <- list()
all_predmin <- list()

count = 1
for (i in year_list) {
  
#fit a linear mod (max or min ~ julian day)
linearmax <- lm(i[, 15] ~ i[, 37])
linearmin <- lm(i[, 13] ~ i[, 37])

#save detrended data for each year
resultmax <- residuals(linearmax)
resultmin <- residuals(linearmin)
all_resultsmax[[count]] = as.data.frame(resultmax)
all_resultsmin[[count]] = as.data.frame(resultmin)

rsqmax <- summary(linearmax)$r.squared
rsqmin <- summary(linearmin)$r.squared
all_rsqmax[[count]] = as.data.frame(rsqmax)
all_rsqmin[[count]] = as.data.frame(rsqmin)

confintmax <- confint(linearmax)
confintmin <- confint(linearmin)
all_confintmax[[count]] = as.data.frame(confintmax)
all_confintmin[[count]] = as.data.frame(confintmin)

predmax <- predict(linearmax)
predmin <- predict(linearmin)
all_predmax[[count]] = as.data.frame(predmax)
all_predmin[[count]] = as.data.frame(predmin)

count=count+1
}

# all_confintmax and min: this needs to be checked yearly to help determine the quality of linear models
#To determine what range of values a coefficient might take
#In terms of interpreting the CI for the purposes of null-hypothesis 
#significance testing, you look to see whether the expected null value is 
#within the CI (for slopes, this expected null value is often 
#[but doesn't have to be] 0); if it is not, you can reject the null hypothesis 
#at the corresponding level of α (e.g., α = .05 for a 95% CI)--
#your 95% CI for ROC_DSRS_5 does not contain 0, for example, 
#so we could reject the null for this slope. 
#https://stats.stackexchange.com/questions/197466/interpret-confidence-interval-upper-and-lower-in-linear-regression


#r-squared
rsqmax1 <- unlist(all_rsqmax)
rsqmax1 <- as.data.frame(rsqmax1)

#create a new dataframe containing detrended temperature data and respective time periods
detrendedmax <- unlist(all_resultsmax)
detrendedmin <- unlist(all_resultsmin)
detrendedmax <- as.data.frame(detrendedmax)
detrendedmin <- as.data.frame(detrendedmin)

predmax <- unlist(all_predmax)
predmin <- unlist(all_predmin)
predmax <- as.data.frame(predmax)
predmin <- as.data.frame(predmin)

detrended <- as.data.frame(cbind(LOCAL_YEAR=histsummer$LOCAL_YEAR,
                           JULIAN=histsummer$JULIAN,RESMAX=detrendedmax$detrendedmax,
                           RESMIN=detrendedmin$detrendedmin,MIN=histsummer$MIN_TEMPERATURE,
                           MAX=histsummer$MAX_TEMPERATURE,PREDMAX=predmax$predmax,
                           PREDMIN=predmin$predmin))
detrended$LOCAL_DATE <- histsummer$LOCAL_DATE

#plotting linear fits
library(ggpubr)
linearplot <- ggplot(subset(detrended, LOCAL_YEAR<=1955), aes(JULIAN, MIN)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point()+
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label")

# create faceted panel
linearplot + facet_grid(. ~ LOCAL_YEAR)

#checking residuals (done for minimum temperatures; need to do for maximum)
#points should be approximately equally distributed along graph area (this shows that
#there is no hidden non-linear trend in the data)
PLOT <- ggplot(subset(detrended, LOCAL_YEAR<=1955), aes(PREDMIN, RESMIN)) +
  geom_point() +
  ggtitle("Linear regression: checking distribution of residuals across graph area") +
  xlab("Predicted") + ylab("Residuals") +
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18))

# create faceted panel
PLOT + facet_grid(. ~ LOCAL_YEAR)


#detrended temperatures are derived from subtracting the trend from the data
#https://kevinkotze.github.io/ts-5-tut/
detrended$MAXDET <- detrended$MAX - detrended$RESMAX
detrended$MINDET <- detrended$MIN - detrended$RESMIN

#run a gam to find if the max-min amplitude has changed throughout the years
library(mgcv)
# "Generalised additive models (GAMs) are statistical models that can be used to 
# estimate trends as smooth functions of time
# simultaneous confidence intervals and the first derivatives of the trend are 
# used to properly account for model uncertainty and identify periods of change"

# "the shape of the fitted trend will be estimated from the data itself"
# (https://www.frontiersin.org/articles/10.3389/fevo.2018.00149/full)

# basis functions: set of functions that, when together, provide the best fit to the data;
# such functions arise from the explansion of a covariate (this is similar to 4 basis
# functions making up a cubic polynomial)

# types of basis functions: among them we have splines (there are several);
# the one I am using: thin plate regression splines (TPRS):
# adds simplicity by reducing number of basis functions when compared to other techniques
# this is achieved by summarizing the data the same way as PC does (selects the
# basis functions which explain most of the variance)

# GAM: smooth functions which are penalized (the number of basis functions to be used
# defines a given complexity of the model, or the extent to which the model
# is wiggly = ondulado; and subsequently parameters of model are calculated based on 
# maximizing a penalized log-likelihood (this is the measure of the fit of the model)
# penalty matrix: how much the wiggliness of one function affects the wiggliness of other
# https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/

# "default wiggliness penalty used in GAMs is on the second derivative of the spline, 
# which measures the rate of change of the slope, or the curvature"
# "The aim of automatic smoothness selection is to find an optimal value of ?? that 
# balances the fit of the model with model complexity to avoid overfitting"
# (this is what we do when we select REML smoothness)
# https://www.frontiersin.org/articles/10.3389/fevo.2018.00149/full

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

#STILL NEED TO READ / UNDERSTAND THIS; THIS CODE STRING DOES NOT WORK?
#edf(gam.model)

#plot actual and predicted data
detrended$pred.gam = predict(gam.model)
library(ggplot2)
ggplot(detrended, aes(LOCAL_DATE, (MAXDET-MINDET))) +
ylab("Maximum - minimum temperature (?C)") +
xlab("Time (summer days)") +
geom_point() +
geom_line(aes(y = pred.gam), size = 1, col = "blue")
