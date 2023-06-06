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

# Excluding experiments for 27C which included maternal effects removal (so that all replicates 
# within this average temperature have the same preparation method; this is because the performance
# is much higher when removing maternal effects around optimal conditions; this did not make much of 
# a difference around 10-15C)
# 10C, 15C and 27C: no maternal effects removal (2021-2022 dataset)
# 10C and 37C: included maternal effects removal (2022-2023 dataset)
datin <- datin[!(datin$Experiment_Number == 99),]
datin <- datin[!(datin$Experiment_Number == 100),]
datin <- datin[!(datin$Experiment_Number == 103),]
datin <- datin[!(datin$Experiment_Number == 104),]

datin$Frond_count <- datin$Frond_count_1 + datin$Frond_count_2 + datin$Frond_count_3

datin <- subset(datin, !Species == "LP")
datin <- subset(datin, !Treatment == "constant")

# Create new treatment Treatment (depending on slope of regression for observed temperature series)
table(datin$Treatment, datin$cat_1)
levels(datin$cat_1) = c("","N","P","N/A")
datin$label<-paste0(datin$Treatment, datin$cat_1)
table(datin$label)

colnames(datin)[6] <- "Treatment1"
colnames(datin)[33] <- "Treatment"

# Relabel groups to simplify visualization of results
library(stringr)
datin$Treatment <- str_replace(datin$Treatment, "0N/A", "0")
datin$Treatment <- str_replace(datin$Treatment, "0.95N", "hot-cold")
datin$Treatment <- str_replace(datin$Treatment, "0.95P", "cold-hot")
datin$Treatment <- str_replace(datin$Treatment, "0", " no autocorrelation")
datin$Mean_temperature <- str_replace(datin$Mean_temperature, "15", "10")
datin$Mean_temperature <- str_replace(datin$Mean_temperature, "10", "10-15")

datin$Mean_temperature <- as.factor(datin$Mean_temperature)
datin$Experiment_Number <- as.factor(datin$Experiment_Number)
datin$Treatment <- as.factor(datin$Treatment)
datin$Incubator <- as.factor(datin$Incubator)

# I am not removing outliers as there's no reason to do so (my model fitted well and I did not find
# obvious measurement errors)
#boxplot(datin$Frond_count, plot=FALSE)$out
#datin <- subset(datin, !Frond_count==111)

# I am not removing zero values for frond counts because there are only 2 out of 10 observations in the
# hot-cold group affected by this (differently than the species comparison model, which has more
# mortality when I include LP)

#good tutorial for mixed models: https://ourcodingclub.github.io/tutorials/mixed-models/
library(lme4)
lm.fit.spF = lm(Frond_count ~ Treatment*Mean_temperature,  data=datin)
experimF <- lmer(Frond_count ~ Treatment*Mean_temperature + (1|Experiment_Number),  data=datin)
incubF <- lmer(Frond_count ~ Treatment*Mean_temperature + (1|Incubator),  data=datin)
inc_expF <- lmer(Frond_count ~ Treatment*Mean_temperature + (1|Incubator)+(1|Experiment_Number),  data=datin)
exp.incF <- lmer(Frond_count ~ Treatment*Mean_temperature + (1|Experiment_Number/Incubator),  data=datin)

#model assessment
##set up named list
Cand.modsF <- list("no random effects" = lm.fit.spF,
                   "experiment number" = experimF,
                   "incubator" = incubF, 
                   "incubator and experiment number" = inc_expF)#,
                   #"incubator nested in experiment number" = exp.incF)

library(performance)
compareF <- compare_performance(Cand.modsF)
knitr::kable(compareF[,c(1,2,3,4)],"simple")

library(sjPlot)
tab_model(experimF, digits = 3)
#this paper is the reference for marginal and conditional R2:
#https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.2041-210x.2012.00261.x
#good explanation https://jamanetwork.com/journals/jamaneurology/fullarticle/2722840
#marginal and conditional r2: https://stats.stackexchange.com/questions/277371/are-r2-for-glmm-useful-for-modelers-but-not-necessarily-for-readers
library(report)
report(experimF)

my_title <- expression(paste(italic("L. minor"), " reproductive performance across temperatures"))

library(ggplot2)
ggplot(data=datin, aes(x=Mean_temperature, y=Frond_count, col = Treatment, group = Treatment)) +
  geom_jitter(aes(shape=Treatment, color=Treatment, size=Treatment), size=4,width = 0.3, height = 0.3) +
  scale_shape_manual(values = c(" no autocorrelation" = 15,
                                "hot-cold" = 19, "cold-hot" = 17)) +
  geom_smooth(method=lm,se=F,size=2) +
  scale_color_manual(values = c(" no autocorrelation" = "grey37",
                                "hot-cold" = "#D55E00", "cold-hot" = "#0072B2")) +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     axis.text=element_text(size=15),
                     axis.title=element_text(size=15),
                     legend.title=element_text(size=15),
                     plot.title = element_text(size=17),
                     legend.text=element_text(size=13)) +
                     ggtitle(my_title) +
  labs(y = "Frond count", x = "Mean temperature (°C)")

library(emmeans)
f <- emmeans(experimF, list(pairwise ~ Treatment*Mean_temperature), lmer.df="satterthwaite")
f1 <- as.data.frame(f$`pairwise differences of Treatment, Mean_temperature`)
f1 <- subset(f1,p.value<0.05)
knitr::kable(f1,"simple",digits=4)


### Checking model assumptions
#https://ademos.people.uic.edu/Chapter18.html
#assumptions of linear models and linear mixed models: 
#linearity (in parameters), homoscedasticity (equal variance), 
#normal distribution of residuals, 
#normal distribution of random effects (relevant for linear mixed models only), 
#and independence (no clustering unaccounted for)

#Assumption 1 - Linearity
#The residuals should be approximately normally distributed. The Shapiro-Wilk test 
#can be used to check the normal distribution of residuals. 
#Null hypothesis: data is drawn from a normal distribution.
shapiro.test(resid(experimF))
#As the p-value is non-significant (p > 0.05), we fail to reject the null hypothesis 
# and conclude that data is drawn from a normal distribution

#Assumption 2 - Homogeneity of Variance (homoscedasticity)
#The variance should be similar for all groups. Bartlett's test can be used to check the
#homogeneity of variances. Null hypothesis: samples from populations have equal variances
#https://www.r-bloggers.com/2021/10/analysis-of-covariance-ancova-using-r/
bartlett.test(residuals(experimF) ~ datin$Treatment)
library(car)
leveneTest(residuals(experimF) ~ datin$Treatment)
#As the p-value is non-significant (p > 0.05), we fail to reject the null hypothesis 
# and conclude that treatments have equal variances

#Assumption 3: The residuals of the model are normally distributed
require("lattice")
qqmath(experimF, id=0.05)
plot(experimF)
#There is some deviation from from the expected normal line towards the tails, 
#but overall the line looks straight and therefore pretty normal and suggests that the 
#assumption is not violated. See http://data.library.virginia.edu/diagnostic-plots/ 
#and https://www.r-bloggers.com/model-validation-interpreting-residual-plots/ for 
#more information regarding these visual tests

# Checking if ancova could have been done instead of linear mixed model
ggplot(datin, aes(as.numeric(Experiment_Number), Frond_count, colour = Treatment)) + geom_point(size = 3) + 
  geom_smooth(method = "lm", aes(fill = Treatment), alpha = 0.1) + theme(legend.position="top")
#my random factor "Experiment number"cannot be a covariate in an Ancova analysis because the
#treatments do not have the same slope
#https://www.r-bloggers.com/2021/10/analysis-of-covariance-ancova-using-r/
#additionally, ancovas do not accept unbalanced number of replicates in each group (which is my case)

# partial eta is not recommended for mixed models
#a <- parameters::model_parameters(experimF, effects = "fixed", ci_method = "satterthwaite")
#t_to_eta2(0.9668, df_error = 67.38)

#https://yuzar-blog.netlify.app/posts/2022-08-01-sjplot/
summary(experimF)
library(sjPlot)
set_theme(base = theme_light())
estimates <- plot_model(experimF, show.values = TRUE, value.offset = 0.3,
           title = "Change in frond count as compared to control",
           rm.terms = c("Mean_temperature10-15", "Mean_temperature37", "Treatmentcold-hot",
                        "Treatmenthot-cold"),vline.color = "red") + 
  theme(text = element_text(size = 20))

# vline
estimates$layers[[1]]$aes_params$size <- 4
# labels
estimates$layers[[4]]$aes_params$size <- 8

estimates

#This coefficient = -11.41 represents the mean decrease in frond count for every 
#additional frond count in the zero group (control). If your zero group frond count increases by 1 frond, 
#the average frond count in hot-cold decreases by 11.41 fronds.

#this shows that the change in frond counts as compared to the zero autocorrelation group 
#(Intercept, not shown) with 95% confidence intervals and significance stars which indicate 
#the changes in frond count which are significant
#the vertical 0 indicates no effect (position 0 for most linear models)

#plot_model(experimF, type = "int")


#https://stats.stackexchange.com/questions/358417/continuous-independent-variable-with-three-levels
#https://cran.r-project.org/web/packages/interactions/vignettes/categorical.html
library(interactions)
cat_plot(experimF, pred = Mean_temperature, modx = Treatment,plot.points = TRUE,interval = FALSE,
         point.shape = TRUE, pred.point.size = 10, 
         point.size = 7, jitter = 1.3, geom = "line",vary.lty = TRUE,
         line.thickness = 2,
         colors = c("no autocorrelation" = "grey37","cold-hot" = "#0072B2","hot-cold" = "#D55E00")) + 
  theme_bw(base_size=34) +
  theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
  ggtitle(my_title) +
  labs(y = "Frond count", x = "Mean temperature (°C)") +
  geom_point(size=2)