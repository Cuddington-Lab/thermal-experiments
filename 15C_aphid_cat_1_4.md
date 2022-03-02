---
title: "15 C power analysis-aphids-autocorrelation-1_4_cat"
author: "nbutool"
date: "01/03/2022"
output: html_document
---

The purpose of this document is to display the results of the Power analysis for each mean temperature (15, 19, 23, 27) for the aphid data. For each temperature there will be 4 results total as follows.

1. Anova summary comparing 0 to 0.95 m<0, categorization based on whole sequence
2. Anova summary comparing 0 to 0.95 m>0, categorization based on whole sequence
3. Anova summary comparing 0 to 0.95 m<0, categorization based on first 1/4th of sequence
4. Anova summary comparing 0 to 0.95 m>0, categorization based on first 1/4th of
sequence

In this document we'll focus on cat_1_4.

##No. of acceptable samples per temperature
```{r Determining # of acceptable samples for cat_1, echo=TRUE}
#Power analysis for autocorrelation experiments - duckweed
#based on file named simpleanovathermal.Rmd

#Download the data from github repo and check import
datin <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/expdata_metafile_FEB_20_22.csv",
                  header=TRUE, stringsAsFactors = TRUE)
str(datin)

#Exclude NAs and samples with standard deviations too different from set value of 2.5
#(code for duckweeds, needs to be adapted for aphids)
datin <- subset(datin, Offspring_Plant1 != "NA" & Obs_sd < 2.7 & Obs_sd > 2.2 & Gaps != "y")

#Create new treatment label and check
table(datin$Autocorrelation, datin$cat_1_4)
levels(datin$cat_1_4) = c("","m<0","", "m>0" )
datin$label<-paste0(datin$Autocorrelation, datin$cat_1_4)
table(datin$label)

#Create new column including sum of fronds (sumFro) 
datin$sumFro=datin$Offspring_Plant1+datin$Offspring_Plant2+datin$Offspring_Plant3
datin <- subset(datin, sumFro != "NA")
table(datin$Mean_Temp, datin$label)

```


# 15 deg C

```{r Selecting temperature, echo=TRUE}

#Select mean temp of 15C
dat27 <- subset(datin, Mean_Temp == 15)

#Perform power analysis based on preliminary data
#Source: https://med.und.edu/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module.pdf
library(pwr)

```

## Categorization for 1/4th of whole sequence

checking 0 to 0.95m<0

```{r echo=TRUE}
#Subset data to include only one 0.95 autocorrelation group and control group
#Obtain same number of samples for each group
datpower <- rbind(dat27[ sample(which (dat27$label == "0") ,5), ],
                  dat27[ sample(which (dat27$label == "0.95m<0") ,5), ])

```

Producing boxplot

```{r echo=TRUE}
#BoxPlot 
tr=boxplot(sumFro~label, data=datpower,main=expression(paste("slope based on whole sequence - aphid: mean temperature 15",degree,"C")),outline=FALSE,
           xlab="autocorr treatment", ylab="summed nymphs",
           names = levels(as.factor(datpower$label)))
stripchart(sumFro~label, data=datpower, 
           vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE) 
```

power analysis results

```{r echo=TRUE}
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
```

checking 0 to 0.95m>0

```{r include=FALSE}
#Subset data to include only one 0.95 autocorrelation group and control group
#Obtain same number of samples for each group
datpower <- rbind(dat27[ sample(which (dat27$label == "0") ,6), ],
                  dat27[ sample(which (dat27$label == "0.95m>0") ,6), ])
```

producing boxplot
```{r echo=FALSE}
#BoxPlot 
tr=boxplot(sumFro~label, data=datpower,main=expression(paste("slope based on whole sequence - aphid: mean temperature 15",degree,"C")),outline=FALSE,
           xlab="autocorr treatment", ylab="summed nymphs",
           names = levels(as.factor(datpower$label)))
stripchart(sumFro~label, data=datpower, 
           vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE) 
```

power analysis results

```{r echo=FALSE}
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
```

Note the code is not shown for the second iteration comparing 0 to 0.95 m>0 since it's the same as the other code, just the sample size is changed. 



