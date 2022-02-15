library('readxl')

setwd("C:/Users/naazb/thermal-experiments/files_for_stat_ana")

datin <- read.xls("e58inc5_27_000_5.xls",header=TRUE)
index=seq(from=1, to=7208, by=60)
subset <- data.frame(sub_time=datin$time[index],sub_temp=datin$actual[index],
                     sub_set=datin$set[index])
#calculating stdev of actual temp data

sd(subset$sub_temp)

#calculating mean of actual temp data

mean(subset$sub_temp)

# calculating the autocorrelation of of actual temp data


acf(subset$sub_temp)$acf[2]