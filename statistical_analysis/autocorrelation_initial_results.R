#Statistical analyses for autocorrelation experiments - duckweeds and aphids
#based on file named simpleanovathermal.Rmd

#Download the data from github repo and check import
datin <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/expdata_metafile_FEB_20_22.csv",
                  header=TRUE, stringsAsFactors = TRUE)
str(datin)

#Exclude NAs and samples with standard deviations too different from set value of 2.5
datin <- subset(datin, Gaps != "y" & (Obs_sd >= 2.2 & Obs_sd <= 2.8))

#Create new treatment label (cat_1_4: if investigating effect of initial 
#sequence slope; cat_1: whole sequence)
table(datin$Autocorrelation, datin$cat_1_4)
levels(datin$cat_1_4) = c("","m<0","", "m>0" )
datin$label<-paste0(datin$Autocorrelation, datin$cat_1_4)
table(datin$label)

#Create new column including sum of fronds (sumFro) 
#duckweed:
datin$sumFro=datin$Duckweed_Rep1+datin$Duckweed_Rep2+datin$Duckweed_Rep2
#aphids: 
#datin$sumFro=datin$Offspring_Plant1+datin$Offspring_Plant2+datin$Offspring_Plant3

#Exclude missing data
datin <- subset(datin, sumFro != "NA")
table(datin$Mean_Temp, datin$label)

#Select mean temp
dat27 <- subset(datin, Mean_Temp == 27)

#Select experiments in which all treatments occurred in the same week
datunique <- dat27[!duplicated(dat27[c("Experiment_Number","cat_1_4")]),]
datunique <- datunique[unsplit(table(datunique$Experiment_Number), datunique$Experiment_Number) >= 3, ]

#If investigating whole sequence (cat_1), remove one of the 2 autocorrelated regimes
#datunique <- subset(datunique, label != "0.95m>0")

#BoxPlot 
tr=boxplot(sumFro~label, data=datunique,main=expression(paste("Autocorrelated temperature regimes: mean temperature 27",degree,"C")),outline=FALSE,
           xlab="Autocorrelated treatment", ylab="Summed offspring",
           names = levels(as.factor(datunique$label)))
stripchart(sumFro~label, data=datunique, 
           vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE) 

#Anova (Are average performances significantly different across groups?)
anova=aov(sumFro~label, data=datunique)
summary(anova)

#Equality of variances (Is distribution of performances significantly different across groups?) 
library(car)
leveneTest(sumFro ~ label, data = datunique)
#http://www.sthda.com/english/wiki/compare-multiple-sample-variances-in-r

#Histograms
hist(datunique[datunique$label == "0", "sumFro"], xlab="Reproduction output",xlim=c(0,40),ylim=c(0,5))
hist(datunique[datunique$label == "0.95m<0", "sumFro"], xlab="Reproduction output",xlim=c(0,40),ylim=c(0,5))
hist(datunique[datunique$label == "0.95m>0", "sumFro"], xlab="Reproduction output",xlim=c(0,40),ylim=c(0,5))
