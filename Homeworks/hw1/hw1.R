hw1Data=read.table(file.choose(), header=T, sep="") #load "survival Liver Transplant Recipients.dat"
attach(hw1Data)
time1=time/365
library(survival)
Srv=Surv(time1, status)~ 1
hw1data.surv=survfit(Srv)
plot(hw1data.surv, xlab="Time in Years", ylab="Survival Liver Transplant Recipients", main="Survival of liver transplant Recipients vs. Time in Years")

##########################Life table code############################

library(KMsurv)
library(nlme)
tall=data.frame(time1, status)
fail=gsummary(tall, sum, groups=time1)
total=gsummary(tall, length, groups=time1)
rm(time1)
ltab.data<-cbind(fail[,1:2], total[,2])
detach(hw1data)
attach(ltab.data)
lt=(length(time1))
time1[lt+1]=NA
nevent=status
nlost=`total[, 2]`-status
mytable<-lifetab(time1, length(nevent), nlost, nevent)
mytable