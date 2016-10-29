###BIOS 7300 hw4####
#Load libraries and data (Time to death waiting for liver transplant)
library(survival)
library(KMsurv)
hw4data <- read.table(file.choose(), header=T, sep="")
attach(hw4data)

#Q3####
agecat<- cut(age, c(0, 50, 60, max(age)))
num_age<- as.numeric(agecat)
age_levels<- cut(num_age, c(0, 1, 2, 3))
#Part a-interaction model of Cox proportional hazards#
model_1<-coxph(formula = Surv(time, status)~agecat*gender, data=hw4data, method="breslow")
chk_model_1<-cox.zph(model_1) #check assumption of proportional hazards
#summary(model_1)

#Part b-additive model#
model_2<-coxph(Surv(time, status)~agecat+gender, data=hw4data, method="breslow")
#The overall model
#library(aod)
#l=matrix(c(1,0,0,1),2,byrow=T)
#wald.test(b = coef(model_2), Sigma = vcov(model_2), L=l)
#summar(model_2)

#Part d-variate model#
model_3<-coxph(Surv(time, status)~num_age+gender, data=hw4data, method="breslow")