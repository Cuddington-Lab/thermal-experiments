#reading in data and checking
aphidat=read.csv("C:\\Users\\kcudding\\Downloads\\aphidtherm.csv")
head(aphidat)
str(aphidat)

#calc summed offspring
aphidat$sumaphids=rowSums(aphidat[,7:9])

#subsetted for just mean = 15
a15=aphidat[aphidat$Mean_Temp==15,]
a15$Profile_name=factor(a15$Profile_name)

#counting number of reps of profiles at 15C
table(a15$Profile_name)

#counts number of reps for each mean and autocorr
table(aphidat$Mean_Temp, aphidat$Autocorrelation)

#plot 27C 
#subset to 27
a27=aphidat[aphidat$Mean_Temp==27,]
a27$Profile_name=factor(a27$Profile_name)

#plot the subsetted data

plot(sumaphids~Autocorrelation, col=a27$Experiment_Start, pch=16,data=a27)



