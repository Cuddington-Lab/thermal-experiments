#Power analysis for autocorrelation experiments - duckweed
#based on file named simpleanovathermal.Rmd

#Download the data from github repo and check import
datin <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/expdata_metafile_FEB_20_22.csv",
                  header=TRUE, stringsAsFactors = TRUE)
str(datin)

#Exclude NAs and samples with standard deviations too different from set value of 2.5
#(code for duckweeds, needs to be adapted for aphids)
datin <- subset(datin, Duckweed_Rep1 != "NA" & Obs_sd < 2.7 & Obs_sd > 2.2 & Gaps != "y")

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

#Perform power analysis based on preliminary data
#Source: https://med.und.edu/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module.pdf
library(pwr)

#Subset data to include only one 0.95 autocorrelation group and control group
#Obtain same number of samples for each group
datpower <- rbind(dat27[ sample(which (dat27$label == "0") ,6), ],
              dat27[ sample(which (dat27$label == "0.95m<0") ,6), ])

#BoxPlot 
tr=boxplot(sumFro~label, data=datpower,main=expression(paste("slope based on whole sequence - duckweed: mean temperature 27",degree,"C")),outline=FALSE,
           xlab="autocorr treatment", ylab="summed fronds",
           names = levels(as.factor(datpower$label)))
stripchart(sumFro~label, data=datpower, 
           vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE) 

#Perform anova
anova <- aov(sumFro~label, data=datpower)
summary(anova)

#Obtain effect size
library(effectsize)
#https://cran.r-project.org/web/packages/effectsize/vignettes/anovaES.html
effectsize <- eta_squared(anova, partial = FALSE)
summary(effectsize)

#Perform power test to obtain estimated "n" in each group based on effect size
pwr.anova.test(k = 2, f = effectsize$Eta2, sig.level = 0.05, power = 0.80)
