# Statistical analysis for the autocorrelation experiment
# Debora
# Dec 2, 2022

# Analysis for a single species 
# Comparing strong autocorrelation x random temperature variation

# Download metafile
datin <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/metafile_autocorrelation_2022-2023.csv",
                  header=TRUE, stringsAsFactors = TRUE,fileEncoding="UTF-8-BOM")

# Exclude missing data
datin <- subset(datin, Completed_experiment_number != "NA")

# Obtain sum of fronds within each replicate (total fronds for 3 petri dishes)
datin$Frond_count <- datin$Frond_count_1 + datin$Frond_count_2 + datin$Frond_count_3

# Exclude constant temperature regimes and "0.95rev" temperature regimes
datin <- subset(datin, Treatment != "constant")

library(tidyverse)
datin <- datin %>%
  filter(str_detect(Profile_name, "rev", negate = TRUE))

# Exclude failed experiments 
# i.e., samples with standard deviations too different from set value of 2.5
datin <- subset(datin, Obs_sd >= 2.1 & Obs_sd <= 2.9)

# Select a single species (LP, Field_LM)
datin2 <- subset(datin, Species == "LP")

# Select average temperature
datin2 <- subset(datin2, Mean_temperature == 10)

# t-test
# Calculate difference in frond counts within the same temperature distribution
# (comparing 2 temperature regimes created in the same week)
# https://dzchilds.github.io/stats-for-bio/paired-sample-t-test.html
datin2_diffs <- 
  datin2 %>%
  group_by(Experiment_Number) %>%
  summarise(Difference = diff(Frond_count))

t.test(datin2_diffs$Difference)

# Anova (Are average performances significantly different across groups?)
library(report)
anova=aov(Frond_count~Treatment, data=datin2)
summary(anova)
report(anova)

# BoxPlot 
datin2$Treatment <- droplevels(datin2$Treatment)

boxplot(Frond_count~Treatment, data=datin2,main=expression(paste("Autocorrelated temperature regimes: mean temperature 10",degree,"C")),outline=FALSE,
           xlab="Autocorrelated treatment", ylab="Summed offspring",
           names = levels(as.factor(datin2$Treatment)))
stripchart(Frond_count~Treatment, data=datin2, 
           vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE) 

# Comparing an invasive and a native species

# Select average temperature
frondcount <- subset(datin, Mean_temperature == 10)

# Define comparison groups
frondcount <- aggregate(Frond_count ~ Species+Treatment+Incubator+Experiment_Number, data = datin, FUN = sum)

# To change the endpoint from frond count to relative growth rate
# datin$RGRf <- (log(datin$Frond_count) - log(12)) / 5
# frondcount2 <- aggregate(RGRf ~ Species+Treatment+Incubator+Experiment_Number, data = datin, FUN = sum)

# BoxPlot 
library(ggplot2)
ggplot(frondcount, aes(x=Treatment, y=Frond_count, fill=Species)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Barplot (mean and SE for summed offspring)
datunique.mean <- aggregate(Frond_count ~ Treatment + Species, data = frondcount, mean)
datunique.sd <- aggregate(Frond_count ~ Treatment + Species, data = frondcount, sd)
datunique <- data.frame(cbind(Treatment=as.character(datunique.mean$Treatment),
                              Species=as.character(datunique.mean$Species),
                              Mean=datunique.mean$Frond_count,
                              sd=datunique.sd$Frond_count))

datunique$Mean <- as.numeric(datunique$Mean) 
datunique$sd <- as.numeric(datunique$sd)

ggplot(datunique, aes(x=Treatment, y=Mean, fill=Species)) +
    geom_bar(stat = "identity",
           position = position_dodge()) +
  geom_errorbar(aes(x=Treatment, ymin=Mean-sd, ymax=Mean+sd), width=0.4, 
                colour="black", alpha=0.9, size=1.3, stat = "identity",
                position=position_dodge(.9)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                     panel.background = element_blank(), axis.line = element_line(colour = "black"))

#mixed linear model with random effect of week on the intercept
library(lme4)
frondcount$Treatment <- as.factor(frondcount$Treatment)
summary(frondcount)

mxlin=lmer(Frond_count~Treatment*Species + (1|Experiment_Number), data=frondcount)

summary(mxlin)
report(mxlin)

# Comparing preparation techniques (generation homogenization)
datin <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/gen_homog_constant.csv",
                  header=TRUE, stringsAsFactors = TRUE,fileEncoding="UTF-8-BOM")

datin <- subset(datin, Species == "Lab_LM")

datin <- subset(datin, Mean_temperature == 20)

datin$Frond_count <- datin$Frond_count_1 + datin$Frond_count_2 + datin$Frond_count_3

library(plyr)
datin2 <- ddply(datin,.(Prep_technique),function(x) x[sample(nrow(x),4),])

boxplot(Frond_count~Prep_technique, data=datin2,main=expression(paste("Comparing preparation techniques: mean temperature 20",degree,"C")),outline=FALSE,
        xlab="Pre-experiment technique", ylab="Summed offspring",
        names = c("Generation homogeneization", "No preparation"))
stripchart(Frond_count~Prep_technique, data=datin2, 
           vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE) 

#Anova (Are average performances significantly different across groups?)
anova=aov(Frond_count~Prep_technique, data=datin2)
summary(anova)
report(anova)

#Barplot
datunique.mean <- aggregate(Frond_count ~ Prep_technique, data = datin2, mean)
datunique.sd <- aggregate(Frond_count ~ Prep_technique, data = datin2, sd)
datunique <- data.frame(cbind(Prep_technique=as.character(datunique.mean$Prep_technique),
                              Mean=datunique.mean$Frond_count,
                              sd=datunique.sd$Frond_count))

datunique$Mean <- as.numeric(datunique$Mean) 
datunique$sd <- as.numeric(datunique$sd)

library(ggplot2)
ggplot(datunique, aes(x=Prep_technique, y=Mean, fill=Prep_technique)) +
  geom_bar(stat = "identity",
           position = position_dodge()) +
  geom_errorbar(aes(x=Prep_technique, ymin=Mean-sd, ymax=Mean+sd), width=0.4, 
                colour="black", alpha=0.9, size=1.3, stat = "identity",
                position=position_dodge(.9)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                     panel.background = element_blank(), axis.line = element_line(colour = "black"))
