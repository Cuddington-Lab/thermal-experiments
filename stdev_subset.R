datin <- read.csv("C:/Users/user/Desktop/testsd00new.csv",header=TRUE)
index=seq(from=1, to=7208, by=60)
subset <- data.frame(sub_time=datin$time[index],sub_temp=datin$actual[index],
                     sub_set=datin$set[index])
sd(subset$sub_temp)
