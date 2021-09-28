library("readxl")

rm(list=ls())
dirst="C:\\Users\\kcudding\\thermallog\\"
setwd(dirst)
filenames <- list.files(dirst, pattern="*.xls", full.names=FALSE)

smd=data.frame(lfile=NA,interval=NA, npts=NA, nobs=NA, meanp=NA, meano=NA, varp=NA, varo=NA,maxp=NA, maxo=NA, minp=NA, mino=NA, acfp=NA,acfo=NA)

smd <- data.frame(matrix(NA, ncol = 10, nrow = length(filenames))) 
colnames(smd)<-c("lfile","interval", "npts", "nobs", "meanp", "meano", "varp", "varo","acfp","acfo")
fnme=(rep(NA,length(filenames)))
start=as.POSIXct(rep(NA,length(filenames)))

for (i in 1:length(filenames)) {
df1 <- as.data.frame(read_excel(filenames[i], sheet = 1, skip=10,col_names =FALSE))
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
f1=gsub(".*long(.*?) *xls.*", "\\1", fnme)

smd$AC=as.numeric(substr(f1, 4, 7))
smd$start=start
smd #giant summary file

#write.csv(smd, "giantsummary.csv")

dfile=fnme
dAC=smd$AC
dmean=smd$meanp-smd$meano
dvr=smd$varp-smd$varo
dmx=smd$maxp-smd$maxo
dmn=smd$minp-smd$mino
dinc=data.frame(dfile,dinterval=smd$interval,dmean, dvr,dmx,dmn, dAC)
dinc #differences between program and output
dinc$rdac=as.factor(round(dinc$dAC,1))

#write.csv(smd, "diffsprogobs.csv")

#big differences in variance for white noise
aggregate(d[, 3:4], list(dinc$rdac), mean)


#get plot and stats on individual sequence

cfile="e20inc1simplelong27_0_7_N.xls"
df <- as.data.frame(read_excel(cfile, sheet = 1, skip=10, col_names =FALSE))
df$TS <- as.POSIXct(paste(df[,1], df[,2]), "%d/%m/%Y %H:%M", tz = "")
df[,3]=ifelse(df[,3]==0, NA, df[,3])

plot(df$TS,df[,3], type="l", xlab="time", ylab="temp")
lines(df$TS,df[,4], col="red")
legend("topleft", c("programmed", "observed"), col=c(2,1), lty=1, bty="n")
summary(df)
#acf(df[,3], na.action=na.pass)

filenamesp <- list.files(dirst, pattern="*.\\.pro", full.names=FALSE)
df2 <- as.data.frame(read_excel(filenamesp[1], sheet = 1, skip=17,col_names =FALSE))


