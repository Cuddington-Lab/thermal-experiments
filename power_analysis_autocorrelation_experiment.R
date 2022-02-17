#Power analysis for autocorrelation experiments - duckweed
#based on file named simpleanovathermal.Rmd

#Download the data from github repo and check import
datin <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/metafile_to_be_updated_FEB_15_new.csv",
                  header=TRUE, stringsAsFactors = TRUE)
str(datin)

#Exclude NAs and samples with standard deviations too different from set value of 2.5
datin <- subset(datin, Duckweed_Rep1 != "NA" & Obs_sd < 2.7 & Obs_sd > 2.2)

#Create new treatment label and check
table(datin$Autocorrelation, datin$Cat)
levels(datin$Cat) = c("","m<0","", "m>0" )
datin$label<-paste0(datin$Autocorrelation, datin$Cat)
table(datin$label)

#Create new column including sum of fronds (sumFro) 
datin$sumFro=datin$Duckweed_Rep1+datin$Duckweed_Rep2+datin$Duckweed_Rep3
datin <- subset(datin, sumFro != "NA")
table(datin$Mean_Temp, datin$label)

#Select mean temp of 27C
dat27 <- subset(datin, Mean_Temp == 27)

#Plot 
tr=boxplot(sumFro~label, data=dat27,main=expression(paste("mean temperature 27",degree,"C")),outline=FALSE,
           xlab="autocorr treatment", ylab="summed fronds",
           names = levels(as.factor(dat27$label)))
stripchart(sumFro~label, data=dat27, 
           vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE) 

#Stats: To determine if there is a significant relationship 
#between frond count (dependent variable) and 
#different autocorrelated temperature regimes (independent variable)
l1=lm(sumFro~label, data=dat27)
summary(l1)

#Format results
#Adding a table of results for Jorren's benefit
library(kableExtra)
kable_classic(kable(summary(l1)$coefficients,caption="Anova results for mean 27&deg;C treatments"))

#Perform power analysis based on preliminary data
#Source: https://med.und.edu/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module.pdf

library(pwr)

#Get ajusted R^2 from linear model using preliminary data and calculate f2
summary <- summary(l1)
f2 <- sqrt(-summary$adj.r.squared)

#Conduct power test (u = variables-1)
power <- pwr.f2.test(u = 2, f2 = f2, sig.level = 0.05, power = 0.8)

#Calculate total sample number: n=v+u+1
total_n <- power$v + power$u + 1

#Calculate sample per treatment
n_per_treatment <- total_n / 3