library("survival", "km.ci")
hw2data<-read.table(file="/Users/Rick/Documents/Tulane/MPH/BIOS 7300/Homeworks/hw2/Survival times of patients with gastric cancer.txt", header=T, sep="")
attach(hw2data)

#####Survival curve plotting#####
##Nelson-Aalen Estimates (s1)##
a<- survfit(coxph(Surv(time, status)~1), type="aalen") #Nelson-Aalen estimates
h.aalen<-(-log(a$surv))
aalen.est<-cbind(time=a$time, d=a$n.event, n=a$n.risk,h.aalen, s1=a$surv)

##Kaplan-Meier Estimates (s2)##
b<-survfit(Surv(time, status)~1)
km.est<-cbind(time=b$time, s2=b$surv)

##Merging the 2 and plotting##
all<-merge(data.frame(aalen.est), data.frame(km.est), by="time")
plot(all$time, all$s1, type="s", xlab="Survival Time (Weeks)", 
    ylab="Survival Probability", col=2)
points(all$time, all$s1, pch=1, col=2)
lines(all$time, all$s2, type="s", col=4)
points(all$time, all$s2, pch=3, col=4)
legend(150, 0.9, c("Nelson-Aalen", "Kaplan-Meier"), pch=c(1, 3), col=c(2,4))

