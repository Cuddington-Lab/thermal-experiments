#Uploading data 
yvanaphiddataset <- read.table("Yvan_Aphid_Data2.csv", sep=",", header=T)

#Looking at number of trials of different auto treatment and different mean temp 
table(yvanaphiddataset$Treatment,yvanaphiddataset$Mean_temperature)


# Exclude failed experiments due to contamination
yvanaphiddataset <- subset(yvanaphiddataset, !Errors == "y" | is.na(Errors))


#Looking at number of trials of different auto treatment and different mean temp 
table(yvanaphiddataset$Treatment,yvanaphiddataset$Mean_temperature)

yvanaphiddataset$Total_Aphids <- yvanaphiddataset$Aphid_count_1 +  yvanaphiddataset$Aphid_count_2 +  yvanaphiddataset$Aphid_count_3

yvanaphiddataset15 <- subset(yvanaphiddataset, yvanaphiddataset$Mean_temperature==15)

# Calculate the mean and standard error for each treatment
treatment_means <- tapply(yvanaphiddataset15$Total_Aphids, yvanaphiddataset15$Treatment, mean, na.rm = TRUE)
treatment_sds <- tapply(yvanaphiddataset15$Total_Aphids, yvanaphiddataset15$Treatment, sd, na.rm = TRUE)
treatment_n <- tapply(yvanaphiddataset15$Total_Aphids, yvanaphiddataset15$Treatment, function(x) length(x[!is.na(x)]))
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



Yvanmodel <- aov(as.numeric(Total_Aphids)~as.factor(Treatment), data = combined)
summary(Yvanmodel)



