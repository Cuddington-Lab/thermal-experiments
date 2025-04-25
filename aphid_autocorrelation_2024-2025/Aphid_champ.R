#Aphid champ reorganization 

#Uploading data 
aphiddataset <- read.table("Aphid_Data_PairONLY_withCAT2.csv", sep=",", header=T)


#Looking at number of trials of different auto treatment and different mean temp 
table(aphiddataset$Treatment,aphiddataset$Mean_temperature)




# Exclude experiments outside thresholds established in methods
aphiddataset <- aphiddataset[aphiddataset$Obs_sd >= 2.1 & aphiddataset$Obs_sd <= 2.9, ]
aphiddataset <- aphiddataset[!(aphiddataset$Treatment == 0 & (aphiddataset$Obs_ac < 0 | aphiddataset$Obs_ac >= 0.29)) &
                 !(aphiddataset$Treatment == 0.95 & (aphiddataset$Obs_ac <= 0.78 | aphiddataset$Obs_ac >= 0.99)), ]

# Exclude failed experiments due to contamination
aphiddataset <- subset(aphiddataset, !Errors == "y" | is.na(Errors))

# Label hot-cold and cold-hot treatments
colnames(aphiddataset)[6] <- "Label"




# Relabel groups to simplify visualization
library(stringr)
aphiddataset$Label <- str_replace(aphiddataset$Label, "0.95", "strong autocorrelation")
aphiddataset$Label <- str_replace(aphiddataset$Label, "0", "no autocorrelation")

aphiddataset$Treatment <- aphiddataset$cat_1
aphiddataset$Treatment <- str_replace(aphiddataset$Treatment, "N", "hot-cold")
aphiddataset$Treatment <- str_replace(aphiddataset$Treatment, "P", "cold-hot")
aphiddataset$Treatment[aphiddataset$Treatment == "" | is.na(aphiddataset$Treatment)] <- "no autocorrelation"
 
# Extract experiment run IDs
aphiddataset$Exp_run <- str_sub(aphiddataset$Profile_name,-2,-1)
aphiddataset$Exp_run <- gsub('_','',aphiddataset$Exp_run)
aphiddataset$Exp_run <- as.numeric(aphiddataset$Exp_run)
## Subset Data

table(aphiddataset$Treatment,aphiddataset$Mean_temperature)

# We subset the data to retain only those experiment runs that have at least 3  replicates within each `Mean_Temperature` level
aphiddataset <- aphiddataset[
  ave(seq_along(aphiddataset$Exp_run),
      interaction(aphiddataset$Exp_run, aphiddataset$Mean_temperature),
      FUN = length) >= 3,
]
table(aphiddataset$Treatment,aphiddataset$Mean_temperature)



aphiddataset$Total_Aphids <- aphiddataset$Aphid_count_1 +  aphiddataset$Aphid_count_2 +  aphiddataset$Aphid_count_3

aphiddataset15 <- subset(aphiddataset, aphiddataset$Mean_temperature==15)






# Calculate the mean and standard error for each treatment
treatment_means <- tapply(aphiddataset15$Total_Aphids, aphiddataset15$Treatment, mean, na.rm = TRUE)
treatment_sds <- tapply(aphiddataset15$Total_Aphids, aphiddataset15$Treatment, sd, na.rm = TRUE)
treatment_n <- tapply(aphiddataset15$Total_Aphids, aphiddataset15$Treatment, function(x) length(x[!is.na(x)]))
# Calculate standard error
treatment_se <- treatment_sds / sqrt(treatment_n)
# Create a data frame for plotting
plot_data <- data.frame(
  Treatment = names(treatment_means),
  Mean_Aphids = treatment_means,
  SE = treatment_se
)
# Create the base R bar plot with error bars
bp <- barplot(
  plot_data$Mean_Aphids,
  names.arg = plot_data$Treatment,
  col = "skyblue",
  ylim = c(0, max(plot_data$Mean_Aphids + plot_data$SE) * 1.1),
  xlab = "Treatment",
  ylab = "Mean Total Aphids",
  main = "Total Aphids by Treatment"
)
# Add error bars in the middle of the bars
arrows(
  x0 = bp,  # x-position matches the bar centers
  y0 = plot_data$Mean_Aphids - plot_data$SE,
  x1 = bp,  # x-position matches the bar centers
  y1 = plot_data$Mean_Aphids + plot_data$SE,
  angle = 90,
  code = 3,
  length = 0.1
)







