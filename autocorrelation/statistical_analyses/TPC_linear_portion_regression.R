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

datin <- subset(datin, Mean_temperature <= 30)

datin$Frond_count <- datin$Frond_count_1 + datin$Frond_count_2 + datin$Frond_count_3

datin <- subset(datin, !Species == "LP")

# Create new treatment label (depending on slope of regression for observed temperature series)
table(datin$Treatment, datin$cat_1)
levels(datin$cat_1) = c("","N","P","N/A")
datin$label<-paste0(datin$Treatment, datin$cat_1)
table(datin$label)

table(datin$label, datin$Mean_temperature)

# Re-label groups to simplify visualization of results
library(stringr)
datin$label <- str_replace(datin$label, "0N/A", "0")
datin$label <- str_replace(datin$label, "0.95N", "hot-cold")
datin$label <- str_replace(datin$label, "0.95P", "cold-hot")
datin$label <- str_replace(datin$label, "constantN/A", "constant")

set.seed(10035)
datin.w <- rbind(datin[sample(which(datin$Treatment == "constant" & datin$Mean_temperature == 7),6),],
                 datin[sample(which(datin$Treatment == "constant" & datin$Mean_temperature == 11),5),],
                 datin[sample(which(datin$label == "0" & datin$Mean_temperature == 10),6),],
                 datin[sample(which(datin$label == "hot-cold" & datin$Mean_temperature == 10),6),],
                 datin[sample(which(datin$label == "cold-hot" & datin$Mean_temperature == 10),6),],
                 datin[sample(which(datin$Treatment == "constant" & datin$Mean_temperature == 20),6),],
                 datin[sample(which(datin$Treatment == "0" & datin$Mean_temperature == 15),6),],
                 datin[sample(which(datin$label == "hot-cold" & datin$Mean_temperature == 15),6),],
                 datin[sample(which(datin$label == "cold-hot" & datin$Mean_temperature == 15),6),],
                 datin[sample(which(datin$Treatment == "constant" & datin$Mean_temperature == 30),6),],
                 datin[sample(which(datin$Treatment == "0" & datin$Mean_temperature == 27),6),],
                 datin[sample(which(datin$label == "hot-cold" & datin$Mean_temperature == 27),6),],
                 datin[sample(which(datin$label == "cold-hot" & datin$Mean_temperature == 27),6),]
)


boxplot(datin.w$Frond_count, plot=FALSE)$out
outliers <- boxplot(datin.w$Frond_count, plot=FALSE)$out
datin.w2 <- datin.w
datin.w2 <- datin.w2[-which(datin.w2$Frond_count %in% outliers),]

lmFrond = lm(Frond_count~Treatment*Mean_temperature, data = datin.w2)
summary(lmFrond)
library(report)
report(lmFrond)

ggplot(data=datin.w2, aes(x=Mean_temperature, y=Frond_count, group = 1, col = label)) +
  geom_jitter(aes(), size=4,width = 1.2, height = 1.2) +
  geom_smooth(method=lm,se=T,col="#009E73") +
  scale_color_manual(values = c("constant" = "black", "0" = "grey37",
                                "hot-cold" = "#D55E00", "cold-hot" = "#0072B2")) +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     axis.text=element_text(size=15),
                     axis.title=element_text(size=15),
                     legend.text=element_text(size=13))

plot(lmFrond$residuals, pch = 16, col = "red")
#should look random
