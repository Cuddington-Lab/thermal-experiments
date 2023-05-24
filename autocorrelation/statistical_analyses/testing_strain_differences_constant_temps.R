# During the analyses conducted for the poster presentation at CSEE 2023, the following groups were created:

# a- Field L. minor + Lab L. minor = L. minor (2 strains were put together)

# For this case, a comparison between the 2 strains across 2 linear regions of the thermal performance curve
# did not show significant differences between the reproductive performance (under constant conditions)
# Thus, the 2 strains were grouped for further analyses (using fluctuating temperatures).
# The main problem of this is that the lab strain may be more insensitive to temperature fluctuations
# than the field strain. That is the reason why we may have not seen a different distribution of
# performance in low temperatures when comparing random variation and strong autocorrelation
# (as we used the lab strain in these temperatures).
# However, we did see a difference in the mean performance when comparing random variation and a strong
# autocorrelation in low to optimal temperatures (conflicting results, as the lab species is indeed 
# responding to these different treatments).


## 1) comparing Field_LM x Lab_LM in constant temperatures below 27C
## 2) comparing Field_LM x Lab_LM in constant temperatures above 27C
## (linear portions of the thermal performance curves)
## because the best models do not show a significant difference between these 2
## strains, it is justified grouping them together when testing responses to autocorrelation

## showing the procedure to compare strains below 27C (the same was done for above 27C)

## Download dataset
datin1 <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/metafile_autocorrelation_2022-2023.csv",header=TRUE, stringsAsFactors = TRUE,fileEncoding="UTF-8-BOM")
datin2 <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/metafile_autocorrelation_2021-2022.csv",header=TRUE, stringsAsFactors = TRUE,fileEncoding="UTF-8-BOM")
datin <- rbind(datin1,datin2)

# Data validation: excluding experiments outside thresholds established in my methods section
# (for standard deviation and autocorrelation of observed temperatures)
datin <- datin[!(datin$Treatment == 0 & (datin$Obs_sd <= 2.1 | datin$Obs_sd >= 2.9))
               &!(datin$Treatment == 0.95 & (datin$Obs_sd <= 2.1 | datin$Obs_sd >= 2.9)),]

datin <- datin[!(datin$Treatment == 0 & (datin$Obs_ac <= -0.2 | datin$Obs_ac >= 0.2))
               &!(datin$Treatment == 0.95 & (datin$Obs_ac <= 0.92 | datin$Obs_ac >= 0.98)),]

# Excluding failed experiments
datin <- datin[!datin$Experiment_Number == 83,]
datin <- datin[!(datin$Experiment_Number == 99 & datin$Incubator ==3),]
datin <- datin[!(datin$Experiment_Number == 99 & datin$Incubator ==4),]

# Select temperatures, treatments, and strains
datin <- subset(datin, !Mean_temperature > 27)
datin <- subset(datin, !Species == "LP")
datin <- subset(datin, Treatment == "constant")

# Sum samples within each replicate
datin$Frond_count <- datin$Frond_count_1 + datin$Frond_count_2 + datin$Frond_count_3

# Check data
table(datin$Species,datin$Mean_temperature)

# Create candidate models
library(lme4)
lm.fit.spF = lm(Frond_count ~ Species*Mean_temperature,  data=datin)
experimF <- lmer(Frond_count ~ Species*Mean_temperature + (1|Experiment_Number),  data=datin)
incubF <- lmer(Frond_count ~ Species*Mean_temperature + (1|Incubator),  data=datin)
inc_expF <- lmer(Frond_count ~ Species*Mean_temperature + (1|Incubator)+(1|Experiment_Number),  data=datin)
exp.incF <- lmer(Frond_count ~ Species*Mean_temperature + (1|Experiment_Number/Incubator),  data=datin)

# Model assessment
Cand.modsF <- list("no random effects" = lm.fit.spF,
                   "experiment number" = experimF,
                   "incubator" = incubF, 
                   "incubator and experiment number" = inc_expF,
                   "incubator nested in experiment number" = exp.incF)

library(performance)
compareF <- compare_performance(Cand.modsF)
knitr::kable(compareF[,c(1,2,3,4)],"simple")

# Display results for best model
library(sjPlot)
tab_model(experimF, digits = 3)

# Plot data and model
ggplot(data=datin, aes(x=Mean_temperature, y=Frond_count, col = Species)) +
  geom_jitter(aes(), size=4,width = 1.2, height = 1.2) +
  geom_smooth(method=lm,se=F,size=2) +
  scale_color_manual(values = c("Field_LM" = "grey37",
                                "Lab_LM" = "black")) +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     axis.text=element_text(size=15),
                     axis.title=element_text(size=15),
                     legend.text=element_text(size=13))


# Check model assumptions

#Assumption 1 - Linearity
plot(resid(lm.fit.spF),datin$Frond_count)
#Looks pretty random so we probably haven't violated this assumption 
#(see Fox 2008 for a discussion about the utility of significance vs visual testing; 
#essentially they argue that significance tests don't offer you additional information 
#that your naked eye can't already detect)

#Assumption 2 Homogeneity of Variance
#Fligner-Killeen's test of homogeneity of variance, one per independent variabel in each favoured model ..:
#Less sensitive (and thus more reliable) for outliers than Levene's test
#Can also handle continious independent variables (such as our variable 'Incomeln')
#Anything above p=0.05 is ok...
#https://rpubs.com/loveb/mm
fligner.test(residuals(experimF) ~ datin$Experiment_Number)
fligner.test(residuals(experimF) ~ datin$Mean_temperature)

#Assumption 3: The residuals of the model are normally distributed
qqnorm(residuals(experimF), pch = 1, frame = FALSE)
qqline(residuals(experimF), col = "steelblue", lwd = 2)
#There is some deviation from from the expected normal line towards the tails, 
#but overall the line looks straight and therefore pretty normal and suggests that the 
#assumption is not violated. See http://data.library.virginia.edu/diagnostic-plots/ 
#and https://www.r-bloggers.com/model-validation-interpreting-residual-plots/ for 
#more information regarding these visual tests


# b- No generation separation L. minor + 2nd generation first daughters L. minor = L. minor
# A comparison between the 2 pre-experimental techniques at 20C showed that the performance
# of 2nd generation daughters is better than the group not submitted to separation, as expected
# (results are statistically significant following an Anova)
# For our grouped analyses, only the 37C treatments included this generation separation
# The main problem with this is that, when comparing the 27C groups (no separation) with the 37C groups
# (submitted to separation), the difference between their performances should be greater than
# what we actually found (the 27C group should have an even better performance than it did, and
# results from 2 recent rounds at 27C including generation separation show that.
# Thus, our main results would not change (they could have been even stronger).
