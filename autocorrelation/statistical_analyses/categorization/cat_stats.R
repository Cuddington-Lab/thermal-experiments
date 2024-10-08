library('readxl')
options(scipen=999)
#find the files
setwd("C:/Users/user/OneDrive - University of Waterloo/Desktop/Autocorrelation experiment/thermal_log")
files <- list.files(pattern = "_")

#set up empty dataframe
lng=length(files)
cat_stats=setNames(data.frame(matrix(ncol = 11, nrow = lng)), c("fname", "slp", "r2", "p","cat", "program_mean",
                                                            "obs_mean", "program_sd", "obs_sd", "program_ac",
                                                            "obs_ac"))

#run a regression on each file, and categorize as positive or negative slope; add summary stats
for (i in 1:length(files)){
  
  #read in file
  my_data <- as.data.frame(read_excel(files[i], skip=10, 
                                      col_names=FALSE))
  colnames(my_data)=c("Date", "Time", "Obs", "Program")
  
  #convert missing data recorded as zeros to NAs
  my_data$Obs=ifelse(my_data$Obs==0, NA,my_data$Obs)
  
  #convert Date and Time to DateTime format
  my_data$DateTime=as.POSIXct(paste(my_data$Date, my_data$Time), 
                              format="%Y-%m-%d %H:%M")
  if (is.na(my_data$DateTime[1])){
    my_data$DateTime=as.POSIXct(paste(my_data$Date, my_data$Time), 
                                format="%d/%m/%Y %H:%M")
  }
  
  #regress data
  my_data=my_data[-c(1:61), ]
  l4=length(my_data$Obs)
  my_data$sequence=1:l4
  lreg=lm(my_data$Obs[1:l4]~my_data$sequence)
  summary(lreg)
  plot(my_data$Obs[1:l4]~my_data$sequence)
  abline(lreg, col="red")
  
  #record slope and categorize as positive or negative
  cat_stats$fname[i]=files[i]
  cat_stats$slp[i]=lreg$coef[2]
  cat_stats$r2[i]=summary(lreg)$r.squared
  cat_stats$p[i]=summary(lreg)$coefficients[2,4]  

  cat_stats$cat[i]=ifelse(lreg$coef[2]>0, "P", "N")

  #subset data to initial lenght of 120 time steps
  index=seq(from=1, to=nrow(my_data), by=60)
  
  subset <- data.frame(sub_time=my_data$Time[index],sub_obs_temp=my_data$Obs[index],
                       sub_set_temp=my_data$Program[index])
  
  #add summary stats
  cat_stats$program_mean[i]=mean(subset$sub_set_temp, na.rm=TRUE)
  cat_stats$obs_mean[i]=mean(subset$sub_obs_temp, na.rm=TRUE)
  
  cat_stats$program_sd[i]=sd(subset$sub_set_temp, na.rm=TRUE)
  cat_stats$obs_sd[i]=sd(subset$sub_obs_temp, na.rm=TRUE)
  
  cat_stats$program_ac[i]=acf(subset$sub_set_temp,na.action=na.pass)$acf[2]
  cat_stats$obs_ac[i]=acf(subset$sub_obs_temp,na.action=na.pass)$acf[2]
}

#write file with results
write.csv(cat_stats, file="p_value_thermal_regimes_strong_autocorrelation.csv")
