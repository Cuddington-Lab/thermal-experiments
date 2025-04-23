library(rTPC)
library(nls.multstart)

### Datasets
datin1 <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/dataset_autocorrelation_2022-2023.csv",
                   header=TRUE, stringsAsFactors = TRUE,fileEncoding="UTF-8-BOM")
datin2 <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/dataset_autocorrelation_2021-2022.csv",
                   header=TRUE, stringsAsFactors = TRUE,fileEncoding="UTF-8-BOM")
datin <- rbind(datin1,datin2)

### Total reproductive number for each replicate (summing 3 dishes)
datin$Frond_count <- datin$Frond_count_1 + datin$Frond_count_2 + datin$Frond_count_3

### Selecting constant temperature experiments
datin <- subset(datin, Treatment == "constant")

### Removing failed experiments
datin <- subset(datin, (!Errors == "y")|is.na(Errors))
### Removing failed experiments
datin <- subset(datin, !Species=="LP")
### Plotting observations
colors <- c("#999999", "#E69F00")
plot(jitter(datin$Mean_temperature, 3),datin$Frond_count, pch = 19, col = colors[factor(datin$Species)])
legend(6, 68, legend=c("Field_LM", "Lab_LM"),
       col=colors, pch=16, cex=0.8)

# Excluding obvious error
datin <- datin[!(datin$Mean_temperature == 20 & datin$Frond_count > 50), ]

### Modelling thermal performance via a Ratkowsky thermal performance curve

tpc <- data.frame(temp=datin$Mean_temperature,frond=datin$Frond_count, Species=datin$Species)

mylist <- list()
mylist[[1]] <- subset(tpc,Species=="Field_LM")
mylist[[2]] <- subset(tpc,Species=="Lab_LM")



# choose model
mod = "ratkowsky_1983"

new_data <- data.frame(temp = seq(min(datin$Mean_temperature), max(datin$Mean_temperature), 0.5))
all_results <- list()
count = 1
for (i in mylist){
  # get start vals
  start_vals <- get_start_vals(i[, 1], i[, 2], model_name = mod)
  # get limits
  low_lims <- get_lower_lims(i[, 1], i[, 2], model_name = mod)
  upper_lims <- get_upper_lims(i[, 1], i[, 2], model_name = mod)
  # fit model
  fit <- nls_multstart(frond~ratkowsky_1983(temp = temp, tmin, tmax, a, b),
                       data = i[, 1:2],
                       iter = 500,
                       start_lower = start_vals - 10,
                       start_upper = start_vals + 10,
                       lower = low_lims,
                       upper = upper_lims,
                       supp_errors = 'Y')
  
  result <- predict(fit, newdata = new_data)
  
  all_results[[count]] = as.data.frame(result)
  
  count=count+1
}  

df <- as.data.frame(do.call(cbind, all_results))
colnames(df)  <- c("tpc_fieldLM","tpc_labLM")
df$temp <- new_data$temp

plot(jitter(datin$Mean_temperature, 3),datin$Frond_count, pch = 19, 
     cex=1.3,cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4, 
     col = colors[factor(datin$Species)],xlab="Temperature",ylab="Reproduction (# of individuals)")
legend(6, 55, legend=c("L. minor (field)","L. minor (lab)"), text.font=c(3,3),
       col=colors, pch=16, cex=1.1)
lines(df$temp,df$tpc_fieldLM,col="#999999",lwd=3)
lines(df$temp,df$tpc_labLM,col="orange",lwd=3)


#References
#Ratkowsky, D.A., Lowry, R.K., McMeekin, T.A., Stokes, A.N., & Chandler, R.E. 
#(1983). Model for bacterial culture growth rate throughout the entire 
#biokinetic temperature range. J. Bacteriol. 154, 1222-1226.


### Testing for any differences between species within each temperature: 2 linear models were run, 1 for
### the increasing portion of the thermal performance curve (temperatures <30) 
### and another one for the decreasing portion (temperatures >30)

### Model for the increasing portion of the thermal performance curve
tpc_model <- datin
  
tpc_model <- within(tpc_model, Species <- relevel(Species, ref = "Lab_LM"))

table(tpc_model$Mean_temperature, tpc_model$Species)

tpc_model$Mean_temperature <- as.character(tpc_model$Mean_temperature)
tpc_model$Mean_temperature[tpc_model$Mean_temperature %in% c("11", "15")] <- "11-15"
tpc_model$Mean_temperature[tpc_model$Mean_temperature %in% c("20", "23", "25", "26")] <- "20-26"
tpc_model$Mean_temperature[tpc_model$Mean_temperature %in% c("30", "31")] <- "30-31"
tpc_model$Mean_temperature[tpc_model$Mean_temperature %in% c("39", "40")] <- "39-40"

table(tpc_model$Mean_temperature, tpc_model$Species)



set.seed(1234567)
# Find the minimum count per temperature group for each species
species_table <- table(tpc_model$Mean_temperature, tpc_model$Species)
min_counts <- apply(species_table, 1, function(x) min(x[x > 0]))

# Initialize an empty data frame to store the subsampled data
tpc_model_subsampled <- data.frame()

# Loop over each temperature group
for (temp_group in rownames(species_table)) {
  # Loop over each species
  for (species in colnames(species_table)) {
    # Subset the original data for the current temperature and species
    temp_species_data <- tpc_model[tpc_model$Mean_temperature == temp_group & tpc_model$Species == species, ]
    
    # Only sample if there are observations (greater than 0)
    if (nrow(temp_species_data) > 0) {
      # Randomly sample the rows to the minimum count for this group
      sampled_data <- temp_species_data[sample(1:nrow(temp_species_data), min(min_counts[temp_group], nrow(temp_species_data))), ]
      
      # Add the sampled data to the result dataframe
      tpc_model_subsampled <- rbind(tpc_model_subsampled, sampled_data)
    }
  }
}

# Verify the new counts after subsampling
table(tpc_model_subsampled$Mean_temperature, tpc_model_subsampled$Species)

tpc_model_subsampled$Mean_temperature <- as.factor(tpc_model_subsampled$Mean_temperature)

# Running candidate models including different combinations of random effects
library(lme4)

simple <- glm(Frond_count ~ Mean_temperature*Species,  data=tpc_model_subsampled, family="quasipoisson")

options(scipen = 999)
library(emmeans)
phi <- sum(residuals(simple, type = "pearson")^2)/df.residual(simple)  

pairwise_adj <- emmeans(simple, pairwise ~ Species | Mean_temperature, type = "response", vcov. = vcov(simple)*phi)
#https://stackoverflow.com/questions/78357281/post-hoc-comparisons-of-quasi-family-glmer-models-with-emmeans
pairwise_adj