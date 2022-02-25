#Power analysis for autocorrelation experiments - duckweed
#based on file named simpleanovathermal.Rmd

#Download the data from github repo and check import
datin <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/expdata_metafile_FEB_20_22.csv",
                  header=TRUE, stringsAsFactors = TRUE)
str(datin)

#Exclude NAs and samples with standard deviations too different from set value of 2.5
datin <- subset(datin, Obs_sd < 2.7 & Obs_sd > 2.2 & Gaps!="y")

#Create new treatment label and check
table(datin$Autocorrelation, datin$Cat)
levels(datin$Cat) = c("","m<0","", "m>0" )
datin$label<-paste0(datin$Autocorrelation, datin$Cat)
table(datin$label)

#Create new column including sum of fronds (sumFro) 
datin$sumFro=datin$Offspring_Plant1+datin$Offspring_Plant2+datin$Offspring_Plant3
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


#Subset data to include only one 0.95 autocorrelation group and control group
#Obtain same number of samples for each group
datpower <- rbind(dat27[ sample(which (dat27$label == "0") ,13), ],
                  dat27[ sample(which (dat27$label == "0.95m>0") ,11), ])
#Perform anova
anova <- aov(sumFro~label, data=datpower)
summary(anova)
#Obtain effect size
library(effectsize)
#https://cran.r-project.org/web/packages/effectsize/vignettes/anovaES.html
eta_squared(anova, partial = FALSE)

pwr.anova.test(k = 2, f = 0.00919, sig.level = 0.05, power = 0.80)