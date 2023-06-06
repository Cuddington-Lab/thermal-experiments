library(rTPC)
library(nls.multstart)

### L. minor
datin1 <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/metafile_autocorrelation_2022-2023.csv",
                   header=TRUE, stringsAsFactors = TRUE,fileEncoding="UTF-8-BOM")
datin2 <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/metafile_autocorrelation_2021-2022.csv",
                   header=TRUE, stringsAsFactors = TRUE,fileEncoding="UTF-8-BOM")
datin <- rbind(datin1,datin2)

datin$Frond_count <- datin$Frond_count_1 + datin$Frond_count_2 + datin$Frond_count_3

datin <- subset(datin, Treatment == "constant")

# remove failed experiment
datin <- subset(datin, !Experiment_Number == 83)
datin <- subset(datin, !Species == "LP")

tpc <- data.frame(temp=datin$Mean_temperature,frond=datin$Frond_count)

# choose model
mod = "ratkowsky_1983"

# get start vals
start_vals <- get_start_vals(tpc[, 1], tpc[, 2], model_name = mod)

# get limits
low_lims <- get_lower_lims(tpc[, 1], tpc[, 2], model_name = mod)
upper_lims <- get_upper_lims(tpc[, 1], tpc[, 2], model_name = mod)

# fit model
fit <- nls_multstart(frond~ratkowsky_1983(temp = temp, tmin, tmax, a, b),
                       data = tpc,
                       iter = 500,
                       start_lower = start_vals - 10,
                       start_upper = start_vals + 10,
                       lower = low_lims,
                       upper = upper_lims,
                       supp_errors = 'Y')

new_data <- data.frame(temp = seq(min(tpc$temp), max(tpc$temp), 0.5))
new_data$.fitted <- predict(fit, newdata = new_data)

plot(jitter(tpc$temp, 3),tpc$frond, pch = 19, 
     cex=1.3,cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4,
     xlab="Temperature",ylab="Reproduction (# of individuals)")
lines(new_data$temp,new_data$.fitted,lwd=3)

#References
#Ratkowsky, D.A., Lowry, R.K., McMeekin, T.A., Stokes, A.N., & Chandler, R.E. 
#(1983). Model for bacterial culture growth rate throughout the entire 
#biokinetic temperature range. J. Bacteriol. 154, 1222-1226.