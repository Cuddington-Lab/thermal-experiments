#select random seed: only implement once! For reproducibility of the simulated numbers. 
a=as.numeric(Sys.time())
set.seed(a)

#create mimicry function: only need to run once. Orders the temperatures of each temperature sequence to mimic the order
#of the 0.99 autocorrelation, keeping the random variation the same. 
#(reference paper: https://link.springer.com/article/10.1007/BF01200792)
mimicry<- function(x,y){
  # x, y: 2 real vectors of length T
  # z: real vector of length T in which the elements of x
  # occur in the same rank order as the elements of y
  # 10 September 1995
  # modified for R, Cuddington 2018
  xs=sort(x,index.return=TRUE)
  ys=sort(y, index.return=TRUE)
  zs=sort(ys$ix,index.return=TRUE)
  z=xs$x[zs$ix]
}

#set parameters: mean, upper threshold, lower threshold, sd and similulation tolerance limit(so it doesn't run forever 
#searching for the perfect solution)
# the higher the tolerance, the greater the chance for the code to find a solution within the provided boundaries.
mtemp=27 #mean
sdev=2.5 #std dev
tol=100
ut=31 #upper threshold
lt=10 #lower threshold
leg=120 #length


#divides the temperature sequence into quartiles
#checks if sd and range are reasonable, this is just a warning, if the inequality is true it means the code will run 
#longer, but will still produce valid results.
asd=(ut-lt)/4
if (asd>1.25*sdev||asd<.75*sdev) {
  print(c("Too big a difference between standard deviation and range, estimated range sd=",asd))
}

#generate random numbers
x=rnorm(leg, m=mtemp, sd=sdev)

#check distribution, mean and standard deviation, creates histogram
hist(x)
text(20,22, paste("mean=",round(mean(x),1)))
text(20,20, paste("stdev=",round(sd(x),1)))
icount=0;

#correct any numbers outside upper and lower threshold (until tolerance limit is reached)
while(((any(x>ut)||any(x<lt)))&&icount<tol) {
  icount=icount+1
  
  for (i in 1:length(x)){
    if(x[i]>ut) x[i]=rnorm(1, m=mtemp, sd=sdev)
    if(x[i]<lt) x[i]=rnorm(1, m=mtemp, sd=sdev)
  }
}

#if extreme values after the correction to the tolerance limit persists, they are removed and replaced with the values
#of the upper and lower threshold values
#truncate values outside of thresholds if tolerance exceeded 
if (icount>=tol){
  print(c("exceeded loop tolerance, truncating",icount,tol))
  x[x>ut]=ut
  x[x<lt]=lt
}

#check distribution, mean and standard deviation, to ensure the distribution is still normal.
hist(x)
text(20,20, paste("mean=",round(mean(x),1)))
text(20,22, paste("stdev=",round(sd(x),1)))
 
#standardize mean and variance (x is the temperatures produced by a normal distribution with the given mean temp and stdev
# this code redistributes the error produced by the simulation after the truncation to get a distribution as close as possible
# to the summary stats (i.e. mean, stdev, var). Therefore, noise is created based on x. 
meanxn=mean(x); varxn=sd(x);
noise=(((x-meanxn)/(varxn))*sdev)+mtemp;
#noise[noise<lt]=lt
#noise[noise>ut]=ut
hist(noise)
text(20,20, paste("mean=",round(mean(noise),3)))
text(20,22, paste("stdev=",round(sd(noise),3)))

#generate sequences with desired degree of autocorrelation (using Ar(1) process) within given
#error tolerance
#(a white noise sequence and an autocorrelated signal are created)
#(the white noise is then rearranged so that it has the same order as the autocorrelated signal) 
lbet=c(0,0.6,0.99) 
tol2=0.01;

gs=vector('double',leg)
tseq=matrix(NaN, leg,length(lbet)+1)
acz=vector('double', length(lbet))


for (j in 1:length(lbet)) {#loop1
acoef=lbet[j];


ac=-99
while ((ac<(acoef-tol2))||(ac>(acoef+tol2))) {#loop2
gs[1]=rnorm(1,0,1);

crand=rnorm(leg, 0,1);
for (cnts in 2:leg) {#loop2
gs[cnts] =acoef*gs[cnts-1]+crand[cnts]*sqrt(sdev)*sqrt(1.-acoef^2);
} #loop3 through desired sequence length

ac=acf(gs,1)$acf[2]
ac

} #loop2 until generate sequence with desired autocorr within tolerance

z=mimicry(noise,gs)
acz[j]=acf(z,1)$acf[2]
tseq[,j]=round(z,1)

} #loop1 through autocorr values


tseq[,4]=rev(tseq[,3])

#check out temp sequences
plot(tseq[,2],type="l")
lines(tseq[,3], type="l", col="red")
#lines(tseq[,1], type="l", col="blue")

#save as dataframe
temps=data.frame(tseq)
colnames(temps)=paste("auto_",round(acz,2), sep="")
colnames(temps)[4]=paste("auto_",round(acz[3],2),"rev", sep="")

# all metrics should be identical
apply(temps, 2,max)
apply(temps, 2,min)
apply(temps, 2,mean)
apply(temps, 2,sd)


#write file with series (each column a different autocorrelation, and series with identical elements)
tempfilename=paste("C:/Users/kcudding/thermallog/temps_mean_",round(mtemp,0),"sd_",round(sdev*10,0),".csv", sep="")
write.csv(temps*10,tempfilename, row.names=FALSE)

