library("readxl")

rm(list=ls())
dirst="~/Downloads/thermal/thermalcode/"
setwd(dirst)
filenames <- list.files(dirst, pattern="*.xls", full.names=FALSE)

smd=data.frame(lfile=NA,interval=NA, npts=NA, nobs=NA, meanp=NA, meano=NA, varp=NA, varo=NA,maxp=NA, maxo=NA, minp=NA, mino=NA, acfp=NA,acfo=NA)

smd <- data.frame(matrix(NA, ncol = 10, nrow = length(filenames))) 
colnames(smd)<-c("lfile","interval", "npts", "nobs", "meanp", "meano", "varp", "varo","acfp","acfo")
fnme=(rep(NA,length(filenames)))
start=as.POSIXct(rep(NA,length(filenames)))

for (i in 1:length(filenames)) {
df1 <- read_excel(filenames[i], sheet = 1, skip=10)
df1[,3]=ifelse(df1[,3]==0, NA, df1[,3])
df1$TS <- as.POSIXct(paste(df1[,1], df1[,2]), "%d/%m/%Y %H:%M", tz = "")

fnme[i]=as.character(filenames[i])
start[i]=as.POSIXct(df1$TS[1])

smd$interval[i]=df1$TS[nrow(df1)]-df1$TS[1]
smd$nobs[i]=sum(!is.na(df1[,3]))
smd$npts[i]=sum(!is.na(df1[,4]))
smd$meanp[i]=mean(df1[,4], na.rm=TRUE)
smd$meano[i]=mean(df1[,3], na.rm=TRUE)
smd$varp[i]=var(df1[,4], na.rm=TRUE)
smd$varo[i]=var(df1[,3], na.rm=TRUE)
smd$maxp[i]=max(df1[,4], na.rm=TRUE)
smd$maxo[i]=max(df1[,3],na.rm=TRUE)
smd$minp[i]=min(df1[,4],na.rm=TRUE)
smd$mino[i]=min(df1[,3],na.rm=TRUE)

#smd$acfp[i]=mean(df1[,4], na.rm=TRUE)
#smd$acfo[i]=mean(df1[,3], na.rm=TRUE)

}

smd$lfile=fnme
smd$start=start


plot(df1$TS,df1[,3], type="l")
lines(df1$TS,df1[,4], col="red")
summary(df1)
#acf(df1[,3], na.action=na.pass)