#Power analysis for autocorrelation experiments - duckweed
#based on file named simpleanovathermal.Rmd

#Download the data from github repo and check import
datin <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/expdata_metafile_FEB_20_22.csv",
                  header=TRUE, stringsAsFactors = TRUE)
str(datin)

#Exclude NAs and samples with standard deviations too different from set value of 2.5
#(code for duckweeds, needs to be adapted for aphids)
datin <- subset(datin, Obs_sd < 2.7 & Obs_sd > 2.2 & Gaps != "y")

#Create new treatment label and check
table(datin$Autocorrelation, datin$cat_1_4)
levels(datin$cat_1_4) = c("","m<0","", "m>0" )
datin$label<-paste0(datin$Autocorrelation, datin$cat_1_4)
table(datin$label)

#Create new column including sum of fronds (sumFro) 
datin$sumFro=datin$Duckweed_Rep1+datin$Duckweed_Rep2+datin$Duckweed_Rep3
datin <- subset(datin, sumFro != "NA")
table(datin$Mean_Temp, datin$label)

#Select mean temp
dat27 <- subset(datin, Mean_Temp == 27)

#Perform power analysis based on preliminary data
#Source: https://med.und.edu/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module.pdf
library(pwr)

#Subset data to include only one 0.95 autocorrelation group and control group
#Include here max amount of preliminary samples from table view; same number of samples for each group
datpower <- rbind(dat27[ sample(which (dat27$label == "0") ,7), ],
              dat27[ sample(which (dat27$label == "0.95m<0") ,7), ],
              dat27[ sample(which (dat27$label == "0.95m>0") ,7), ])

datpower$RGR <- (log(datpower$sumFro) - log(12)) / 5

#BoxPlot 
tr=boxplot(RGR~label, data=datpower,main=expression(paste("Duckweed growth rates in autocorrelated temperature regimes: mean temperature 27",degree,"C")),outline=FALSE,
           xlab="Autocorrelated treatment", ylab="Relative growth rate",
           names = levels(as.factor(datpower$label)))
stripchart(RGR~label, data=datpower, 
           vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE) 


anova=aov(RGR~label, data=datpower)
summary(anova)

#Obtain effect size
treatmean <- mean(dat27[dat27$label == "0.95m<0", "sumFro"])
controlmean <- mean(dat27[dat27$label == "0", "sumFro"])
treatsd <- sd(dat27[dat27$label == "0.95m<0", "sumFro"])
controlsd <- sd(dat27[dat27$label == "0", "sumFro"])

effsize <- (treatmean-controlmean)/(sqrt((controlsd^2)+(treatsd^2))/2)

#Perform power test to obtain estimated "n" in each group based on effect size
pwr.t.test(d=effsize, sig.level=0.05, power=0.80, type="two.sample", alternative="two.sided")

l1=lm(RGR~label, data=datpower)
summary(l1)
library(kableExtra)
kable_classic(kable(summary(l1)$coefficients,caption="Anova results for mean 27&deg;C treatments"))



hist(datpower[datpower$label == "0", "RGR"], breaks = 7,xlab="Relative growth rate")
hist(datpower[datpower$label == "0.95m<0", "RGR"], breaks = 7,xlab="Relative growth rate",xlim=c(0.12,0.24))
hist(datpower[datpower$label == "0.95m>0", "RGR"], breaks = 7,xlab="Relative growth rate")

mean(datpower[datpower$label == "0.95m<0", "RGR"])
mean(datpower[datpower$label == "0", "RGR"])
