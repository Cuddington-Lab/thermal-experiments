library(rTPC)
library(nls.multstart)

### L. minor
datin1 <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/metafile_autocorrelation_2022-2023.csv",
                   header=TRUE, stringsAsFactors = TRUE,fileEncoding="UTF-8-BOM")
datin2 <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/metafile_autocorrelation_2021-2022.csv",
                   header=TRUE, stringsAsFactors = TRUE,fileEncoding="UTF-8-BOM")
datin <- rbind(datin1,datin2)

datin$Frond_count <- datin$Frond_count_1 + datin$Frond_count_2 + datin$Frond_count_3

datin <- subset(datin, Treatment == "constant")

# remove failed experiment
datin <- subset(datin, !Experiment_Number == 83)

table(datin$Species,datin$Mean_temperature)

#set.seed(20233)
#datin <- rbind(datin[sample(which(datin$Mean_temperature == 7),6),])

colors <- c("#999999", "black", "#E69F00")

plot(jitter(datin$Mean_temperature, 3),datin$Frond_count, pch = 19, col = colors[factor(datin$Species)])
legend(6, 68, legend=c("Field_LM", "Lab_LM", "LP"),
       col=colors, pch=16, cex=0.8)

tpc <- data.frame(temp=datin$Mean_temperature,frond=datin$Frond_count, Species=datin$Species)

mylist <- list()
mylist[[1]] <- subset(tpc,Species=="Field_LM")
mylist[[2]] <- subset(tpc,Species=="Lab_LM")
mylist[[3]] <- subset(tpc,Species=="LP")
  
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
colnames(df)  <- c("tpc_fieldLM","tpc_labLM","tpc_LP")
df$temp <- new_data$temp

plot(jitter(datin$Mean_temperature, 3),datin$Frond_count, pch = 19, 
     cex=1.3,cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4, 
     col = colors[factor(datin$Species)])
legend(6, 68, legend=c("L. minor (field)","L. minor (lab)","L. punctata"), text.font=c(3,3),
       col=colors, pch=16, cex=1.1)
lines(df$temp,df$tpc_fieldLM,col="#999999",lwd=3)
lines(df$temp,df$tpc_LP,col="#E69F00",lwd=3)
lines(df$temp,df$tpc_labLM,col="black",lwd=3)

datin1 <- subset(datin,!Species=="Lab_LM")
plot(jitter(datin1$Mean_temperature, 3),datin1$Frond_count, pch = 19, 
     cex=1.3,cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4, 
     col = c("#999999","#E69F00"),xlab="Temperature",ylab="Reproduction (# of individuals)")
legend(4.8, 70, legend=c("L. minor","L. punctata"), text.font=c(3,3),
       col=c("#999999","#E69F00"), pch=16, cex=1.1)
lines(df$temp,df$tpc_fieldLM,col="#999999",lwd=3)
lines(df$temp,df$tpc_LP,col="#E69F00",lwd=3)

#References
#Ratkowsky, D.A., Lowry, R.K., McMeekin, T.A., Stokes, A.N., & Chandler, R.E. 
#(1983). Model for bacterial culture growth rate throughout the entire 
#biokinetic temperature range. J. Bacteriol. 154, 1222-1226.