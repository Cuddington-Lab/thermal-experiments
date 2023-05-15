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

datin <- subset(datin, Mean_temperature == 27)

datin$Frond_count <- datin$Frond_count_1 + datin$Frond_count_2 + datin$Frond_count_3

datin <- subset(datin, !Species == "LP")
datin <- subset(datin, !Treatment == "constant")

# Create new treatment label (depending on slope of regression for observed temperature series)
table(datin$Treatment, datin$cat_1)
levels(datin$cat_1) = c("","N","P","N/A")
datin$label<-paste0(datin$Treatment, datin$cat_1)
table(datin$label)

# Re-label groups to simplify visualization of results
library(stringr)
datin$label <- str_replace(datin$label, "0N/A", "0")
datin$label <- str_replace(datin$label, "0.95N", "hot-cold")
datin$label <- str_replace(datin$label, "0.95P", "cold-hot")

boxplot(datin$Frond_count, plot=FALSE)$out
datin.w <- subset(datin, !Frond_count==111)

library(lme4)
lm.fit.spF = lm(Frond_count ~ label*Mean_temperature,  data=datin.w)
experimF <- lmer(Frond_count ~ label*Mean_temperature + (1|Experiment_Number),  data=datin.w)
incubF <- lmer(Frond_count ~ label*Mean_temperature + (1|Incubator),  data=datin.w)
inc_expF <- lmer(Frond_count ~ label*Mean_temperature + (1|Incubator)+(1|Experiment_Number),  data=datin.w)
exp.incF <- lmer(Frond_count ~ label*Mean_temperature + (1|Experiment_Number/Incubator),  data=datin.w)

#model assessment
##set up named list
Cand.modsF <- list("no random effects" = lm.fit.spF,
                   "experiment number" = experimF,
                   "incubator" = incubF, 
                   "incubator and experiment number" = inc_expF,
                   "incubator nested in experiment number" = exp.incF)

library(performance)
compareF <- compare_performance(Cand.modsF)
knitr::kable(compareF[,c(1,2,3,4)],"simple")

library(sjPlot)
tab_model(exp.incF, digits = 3)

ggplot(data=datin.w, aes(x=Mean_temperature, y=Frond_count, col = label)) +
  geom_jitter(aes(), size=4,width = 1.2, height = 1.2) +
  geom_smooth(method=lm,se=F,size=2) +
  scale_color_manual(values = c("0" = "grey37",
                                "hot-cold" = "#D55E00", "cold-hot" = "#0072B2")) +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     axis.text=element_text(size=15),
                     axis.title=element_text(size=15),
                     legend.text=element_text(size=13))

library(emmeans)
f <- emmeans(exp.incF, list(pairwise ~ label*Mean_temperature), lmer.df="satterthwaite")
f1 <- as.data.frame(f$`pairwise differences of label, Mean_temperature`)
knitr::kable(f1,"simple",digits=4)

#https://ademos.people.uic.edu/Chapter18.html
#assumptions of linear models and linear mixed models: 
#linearity (in parameters), homoscedasticity (equal variance), 
#normal distribution of residuals, 
#normal distribution of random effects (relevant for linear mixed models only), 
#and independence (no clustering unaccounted for)

#Assumption 1 - Linearity
plot(resid(exp.incF),datin.w$Frond_count)
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
fligner.test(residuals(exp.incF) ~ datin.w$Experiment_Number)
fligner.test(residuals(exp.incF) ~ datin.w$Mean_temperature)

#Assumption 3: The residuals of the model are normally distributed
require("lattice")
qqmath(exp.incF, id=0.05)
#There is some deviation from from the expected normal line towards the tails, 
#but overall the line looks straight and therefore pretty normal and suggests that the 
#assumption is not violated. See http://data.library.virginia.edu/diagnostic-plots/ 
#and https://www.r-bloggers.com/model-validation-interpreting-residual-plots/ for 
#more information regarding these visual tests