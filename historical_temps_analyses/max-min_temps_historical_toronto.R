hist <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/historical_temps_analyses/historical_temps_toronto.csv", header=TRUE, stringsAsFactors = TRUE)
hist$LOCAL_DATE <- as.Date(hist$LOCAL_DATE, format = "%m/%d/%Y")
hist <- hist[!is.na(hist$MEAN_TEMPERATURE),]
hist$JULIAN <- as.numeric(format(hist$LOCAL_DATE, "%j"))

#to avoid disproportional effects caused by extreme minimum temperatures, 
#we subset and use the summer months only
histsummer <- subset(hist, LOCAL_MONTH>=6 & LOCAL_MONTH<=8)

#create list to separate each year
year_list <- split(histsummer, f = histsummer$LOCAL_YEAR)
all_results <- list()

#loop to run linear model to detrend summer temperatures in each year
count = 1
for (i in year_list) {
  
  #fit a linear model
  linear <- lm((i[, 15]-i[, 13]) ~ i[, 37])
  
  #save detrended data for each year
  result <- residuals(linear) + mean(i[, 15]-i[, 13])
  all_results[[count]] = as.data.frame(result)
  count=count+1
  
}

#create dataframe based on detrended data
MAXMIN=unlist(all_results)
MAXMIN <- as.data.frame(MAXMIN)
MAXMIN$LOCAL_YEAR <- histsummer$LOCAL_YEAR
MAXMIN$JULIAN <- histsummer$JULIAN

#run gam model to check if max-min amplitude has reduced across years
library(mgcv)
gam.model <- gam(MAXMIN ~ s(JULIAN,bs="tp")+s(LOCAL_YEAR,bs="tp",k=30), data=MAXMIN, method="REML")
par(mfrow = c(2, 2))
plot(gam.model)
summary(gam.model)
gam.check(gam.model)


