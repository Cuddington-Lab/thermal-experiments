#Night warming experiment: generating thermal sequences
#version 2: trying to make smoother daily cycles based on reference data shape

#download clean temperature data (June 1 to 10, 1998-2020, uWaterloo Weather Station)
refjune <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/historical_temperatures_uwaterloo_june_1_10.csv", header=TRUE, stringsAsFactors = TRUE)

#remove very hot/very cold years
refjune<-subset(refjune, !(year==1998 | year==1999 | year== 2005
                           | year== 2009| year== 2020))

#create average day based on mean hourly temperatures
refjune <- aggregate(list(control=refjune$Temperature), 
                     by = list(time=refjune$hour), mean)

#create daytime warming (dw) by adding an increment to each hourly temperature, 
#and culminating in a maximum temperature increase of 5C 

#from minimum to maximum temperatures
refminmax <- subset(refjune,time>=5 & time<=15)
refminmax$incr <- c(seq(0, 5, by = 5/(length(refminmax$time)-1))) #because we want to increase the max to 5C
refminmax$dw <- refminmax$control+refminmax$incr

#from maximum to minimum temperatures
refmaxmin <- subset(refjune,time<=5 | time>=15)
refmaxmin$time2 <- c(c(10:15),c(1:9))
refmaxmin <- refmaxmin[order(refmaxmin$time2),]
refmaxmin$incr <- c(seq(5, 0, by = -(5/(length(refmaxmin$time2)-1)))) #because we want to increase the max to 5C
refmaxmin$dw <- refmaxmin$control+refmaxmin$incr

#bind sequences above
refmaxmin <- subset(refmaxmin, select = -c(time2) )
thermalseq <- rbind(refminmax,refmaxmin)
thermalseq <- thermalseq[order(thermalseq$time),]
thermalseq <- thermalseq[!duplicated(thermalseq$time), ]
rownames(thermalseq) <- NULL

#create whole day warming sequence
thermalseq$ww <- thermalseq$control+2.5

#create night warming sequence by fitting a linear model to portions of the sequence 
lmnw <- data.frame(time=c(1,13),temp=c(thermalseq$control[thermalseq$time==17],
                                       min(thermalseq$control)+5),
                   time1=c(1,11),temp1=c(min(thermalseq$control)+5,max(thermalseq$control)))

#from 5pm to 5am
fit <- lm(cbind(temp) ~ time, data = lmnw)
lm3 <- data.frame(time=c(1:13))
lm3$temp <- predict(fit,lm3)
lm3$time <- c(c(17:23),c(0:5))

#from 5am to 3pm
fit <- lm(cbind(temp1) ~ time1, data = lmnw)
lm4 <- data.frame(time1=c(1:11))
lm4$temp <- predict(fit,lm4)
lm4$time1 <- c(5:15)
colnames(lm4)[1] <- "time"

#put sequences together and add 4pm (same value as control; so that maximum temperatures
#are not greater than control; this also makes the night warming curve smoother close to
#the peak)
nw <- rbind(lm3,lm4,c(16,thermalseq$control[thermalseq$time==16]))

#check difference between mean temp in control x night warming treatment (should be 2.5C)
mean(nw$temp)-mean(thermalseq$control)

#add small value to each temperature different than min/max to approach a 2.5C difference
#between control and night warming treatment
nw$temp <- ifelse((nw$temp < max(nw$temp) & nw$temp > min(nw$temp)),
                  nw$temp+(1.2/22),nw$temp)

#reorder, remove duplicates, add night warming to thermalseq dataframe
nw <- nw[order(nw$time),]
nw <- nw[!duplicated(nw$time), ]
rownames(nw) <- NULL
thermalseq$nw <- nw$temp

#create a 10-day sequence
rows= c(1:nrow(thermalseq))
times = 11
thermalseq <- thermalseq[rep(rows, times),]
thermalseq <- thermalseq[-(1:12),]
thermalseq <- thermalseq[-(242:252),]

#plot treatments
par(mfrow=c(1,1))
plot(thermalseq[,2],type="l",col="black",ylim=c(10,30),
     main="Fig. 2: Night warming experiment treatments",
     xlab="Time (hours)", ylab="Temperature (°C)")
lines(thermalseq[,4],type="l",col="green")
lines(thermalseq[,5],type="l",col="red")
lines(thermalseq[,6],type="l",col="red")


plot(thermalseq[,2],type="l",col="black",ylim=c(10,30),
     main="Fig. 2: Night warming experiment treatments",
     xlab="Time (hours)", ylab="Temperature (°C)")
lines(thermalseq[,4],type="l",col="orange")
lines(thermalseq[,5],type="l",col="green")
lines(thermalseq[,6],type="l",col="red")
legend(1, 30, legend=c("control", "day warming", 
                       "whole day warming","night warming"),
       col=c("black", "orange", "green","red"), lty=1, cex=0.6,bty = "n")

#check stats
library(pastecs)
stats <- stat.desc(thermalseq[,c(2,4:6)])
round(stats, 1)

