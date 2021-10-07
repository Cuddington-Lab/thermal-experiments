library('readxl')

#find the files
setwd("~/kim/thermal/")
files <- list.files(pattern = "_095")

#set up empty dataframe
lng=length(files)
cat095=setNames(data.frame(matrix(ncol = 3, nrow = lng)), c("fname", "slp", "cat"))

#run a regression on each file, and categorize as postive or negative slope
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

#regress first 1/4 of data
l4=length(my_data$Obs)/4
lreg=lm(my_data$Obs[1:l4]~my_data$DateTime[1:l4])
summary(lreg)
plot(my_data$Obs[1:l4]~my_data$DateTime[1:l4])
abline(lreg, col="red")

#record slope and categorize as positive or negative
cat095$fname[i]=files[i]
cat095$slp[i]=lreg$coef[2]
cat095$cat[i]=ifelse(lreg$coef[2]>0, "P", "N")
}

#write file with results
write.csv(cat095, file="cat095_PorN.csv")