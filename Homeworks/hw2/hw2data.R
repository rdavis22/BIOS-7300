library(easypackages)
libraries("survival", "km.ci", "muhaz", "fmsb")
library(KMsurv)
library(OIsurv)
hw2data<-read.table(file="/Users/Rick/Documents/Tulane/MPH/BIOS 7300/Homeworks/hw2/Survival times of patients with gastric cancer.txt", header=T, sep="")
attach(hw2data)

#####Survival curve plotting#####
##Nelson-Aalen Estimates (s1)##
a<- survfit(coxph(Surv(time, status)~1), type="aalen", conf.type = "log-log") #Nelson-Aalen estimates
h.aalen<-(-log(a$surv))
aalen.est<-cbind(time=a$time, d=a$n.event, n=a$n.risk,h.aalen, s1=a$surv)

##Kaplan-Meier Estimates (s2)##
b<-survfit(Surv(time, status)~1, conf.type="log-log")
km.est<-cbind(time=b$time, s2=b$surv)

##Merging the 2 and plotting##
all<-merge(data.frame(aalen.est), data.frame(km.est), by="time")
plot(all$time, all$s1, type="s", xlab="Survival Time (Weeks)", 
    ylab="Survival Probability", col=2)
points(all$time, all$s1, pch=1, col=2)
lines(all$time, all$s2, type="s", col=4)
points(all$time, all$s2, pch=3, col=4)
legend(150, 0.9, c("Nelson-Aalen", "Kaplan-Meier"), pch=c(1, 3), col=c(2,4))

##Confidence Bands##
#library(km.ci)
#hw2data1<-hw2data
#cba <- survfit(coxph(Surv(time, status)~1), hw2data1)
#cbb <- survfit(Surv(time, status)~1, hw2data1)
#KMcb<-km.ci(cbb, conf.level = 0.95, method="hall-wellner")

##Epanechnikov Smoothed Hazard Function##
#<-density(b$surv, bw=20, adjust=1, kernel="epanechnikov") #KM smoothed hazard function
SmthdHzrd<-muhaz(time, status, bw.grid=5, bw.method="global",  kern="epanechnikov")
plot(SmthdHzrd, main="H(t) of Gastric cancer survival using Epanechnikov")
#SmthDHzrd<-density(a$surv, bw=20, adjust=1, kernel="epanechnikov") #NA smoothed hazard function

##Descriptive statistics for Survival Data##
s2 <- b$surv
meds2 <- median(s2) #median of KM survival estimate
LL95s2 <-b$lower[s2==median(s2)]
LL95s2 <- LL95s2[1] #lower bound of 95% CI for KM survival estimate
UL95s2 <-b$upper[s2==median(s2)]
UL95s2 <- UL95s2[1] #upper bound of 95% CI for KM survival estimate
CI95s2 <- c(LL95s2, UL95s2)
Qrtls1st_3rd <- quantile(s2, c(0.25, 0.75)) #1st and 3rd quartiles for KM survival estimates
LL95s2Q1 <- b$lower[s2==Qrtls1st_3rd[1]] #lower limit of 95%CI for 1st qrtl of KM surv estimates
UL95s2Q3 <- b$upper[s2==Qrtls1st_3rd[1]] # upper limit of 95%CI for 1st qrtl of KM surv estimates
LL95s2Q3 <- b$lower[s2==Qrtls1st_3rd[2]]
UL95s2Q3 <- b$upper[s2==Qrtls1st_3rd[2]]
semiIQRs2 <- SIQR(s2) #semi interquartile range for KM estimates