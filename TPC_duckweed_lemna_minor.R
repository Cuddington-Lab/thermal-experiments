library(rTPC)
library(nls.multstart)

### L. minor
tpcLM <- read.csv("https://raw.githubusercontent.com//Cuddington-Lab/thermal-experiments/main/full_experimental_data.csv",
                  header=TRUE)

### Remove failed experiments
tpcLM <- tpcLM[!(tpcLM$Technical_issue=="y"),]

### Select constant temperatures only
tpcLM <- tpcLM[tpcLM$Thermal_regime == "constant",]

### Add columns containing sum of fronds
tpcLM[,7:12] <- sapply(tpcLM[,7:12],as.numeric)
tpcLM$Total_frond <- tpcLM$LM_Rep1 + 
  tpcLM$LM_Rep2 + tpcLM$LM_Rep3

### Create new column including RGR and replace negative RGR values with zero
tpcLM$rgr <- (log(tpcLM$Total_frond) - log(12)) / 5
tpcLM$rgr <- pmax(tpcLM$r,0)

### Create performance dataset for each species; remove single observations and NA data
table(tpcLM$Mean_Temp,tpcLM$rgr)
tpcLM <- tpcLM[!(tpcLM$Mean_Temp==27 | tpcLM$Mean_Temp==30),]
tpcLM <- tpcLM[complete.cases(tpcLM$Total_frond),]

plot(tpcLM$Mean_Temp,tpcLM$rgr)
#remove rgr of zero in higher temperatures (probable measurement error)
tpcLM <- tpcLM[which(!(tpcLM$Mean_Temp==15
                       & tpcLM$rgr==0)), ]
tpcLM <- tpcLM[which(!(tpcLM$Mean_Temp==19
                       & tpcLM$rgr==0)), ]
tpcLM <- tpcLM[which(!(tpcLM$Mean_Temp==23
                       & tpcLM$rgr==0)), ]

tpcLM <- data.frame(temp=tpcLM$Mean_Temp,rgr=tpcLM$rgr)

# choose model
mod = "ratkowsky_1983"

### Ratkowsky (Ratkowsky et al., 1983)
#r_t  = (a*(T- T_{min})*(1-exp(b*(T-T{max}))))^2
#where T is the temperature (°C); and a, b, Tmin, and Tmax (minimum and maximum temperatures) are fitted constants

# get start values
start_vals <- get_start_vals(tpcLM$temp, tpcLM$rgr, model_name = mod)

# get limits
low_lims <- get_lower_lims(tpcLM$temp, tpcLM$rgr, model_name = mod)
upper_lims <- get_upper_lims(tpcLM$temp, tpcLM$rgr, model_name = mod)

# fit model
fit <- nls_multstart(rgr~ratkowsky_1983(temp = temp, tmin, tmax, a, b),
                     data = tpcLM,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')
fit

# predict new data
new_data <- data.frame(temp = seq(min(tpcLM$temp), max(tpcLM$temp), 0.5))
new_data$.fitted <- predict(fit, newdata = new_data)

# plot data and model fit
ggplot(tpcLM, aes(temp, rgr)) +
  geom_point() +
  xlim(0, 40) +
  ylim(0, 0.3) +
  geom_line(aes(temp,.fitted), new_data, col = 'blue',size=2) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Relative growth rate (frond count)',
       title = 'L. minor')

#References
#Ratkowsky, D.A., Lowry, R.K., McMeekin, T.A., Stokes, A.N., & Chandler, R.E. 
#(1983). Model for bacterial culture growth rate throughout the entire 
#biokinetic temperature range. J. Bacteriol. 154, 1222–1226.