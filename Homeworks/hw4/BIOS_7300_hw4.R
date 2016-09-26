###BIOS 7300 hw4####
#Load libraries and data (Time to death waiting for liver transplant)
library(survival)
library(KMsurv)
#hw4data<-read.table(file=file.choose(), header=T, sep="")
attach(hw4data)

#Q3####
agecat<- cut(age, c(0, min(age), 40, 60, max(age)))
#Part a-interaction model#
model_1<-coxph(formula = Surv(time, status)~agecat*gender, data=hw4data, method="breslow")
#print(model_1)

#Part b-additive model#
model_2<-coxph(formula = Surv(time, status)~agecat+gender, data=hw4data, method="breslow")
#print(model_2)

#Part c-