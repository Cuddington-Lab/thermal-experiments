### Anova for the pea-aphid experiment at 27 C - autocorrelated temperatures

# Goal: to determine if there is a significant relationship between aphid 
# offspring (dependent variable) and different autocorrelated temperature regimes
# (independent variable)

library(ggplot2)
library(tidyverse)

# Load metafile data from Google Docs (expdata_metafile_01062021.csv)
# and attribute columns as factors or numeric, as appropriate
aphid.offspring <- read.csv("C:/Users/user/Desktop/uWaterloo/experiments/expdata_metafile_01062021.csv",
header = TRUE, colClasses = c("factor", "factor","factor","factor","factor",
                                "factor","factor","numeric","numeric","numeric",
                                "numeric","numeric","numeric","factor"))
summary(aphid.offspring)

# Create new data frame including only mean temp of 27C
anova.27C <- aphid.offspring %>% filter(Mean_Temp == 27)

# Concatenate columns Autocorrelation and Cat to enable autocorrelation regime
#labelling (new column name: Autocorr_label)
anova.27C$Autocorr_label<-paste(anova.27C$Autocorrelation,anova.27C$Cat)

# Rename autocorrelation regimes
anova.27C$Autocorr_label[anova.27C$Autocorr_label == "0.95 N"] <- "a=0.9,m<0"
anova.27C$Autocorr_label[anova.27C$Autocorr_label == "0.6 N/A"] <- "a=0.6"
anova.27C$Autocorr_label[anova.27C$Autocorr_label == "0 N/A"] <- "a=0"
anova.27C$Autocorr_label[anova.27C$Autocorr_label == "0.95 P"] <- "a=0.9,m>0"

# Create new column including sum of offspring (Total_Offspring) 
anova.27C$Total_Offspring <- anova.27C$Offspring_Plant1 + 
  anova.27C$Offspring_Plant2 + anova.27C$Offspring_Plant3

# Run an analysis of variance
res.aov <- aov(Total_Offspring ~ Autocorr_label, data = anova.27C)
summary(res.aov)

# Boxplots
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
boxplots <- ggplot(aes(y = Total_Offspring, x = factor(Autocorr_label)), 
                   data = anova.27C)
boxplots <- boxplots + stat_summary(fun.data = min.mean.sd.max, 
                                    geom = "boxplot") + 
  geom_jitter(position=position_jitter(width=.2), size=3) + 
  ggtitle("Mean temperature 27°C") + 
  xlab("Autocorrelation") + ylab("Total_Offspring")
boxplots

# Linear model (0 autocorrelation vs. other groups)
lm.27C <- lm(Total_Offspring ~ Autocorr_label, data = anova.27C)
summary(lm.27C)