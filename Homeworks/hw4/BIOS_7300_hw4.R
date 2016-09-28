###BIOS 7300 hw4####
#Load libraries and data (Time to death waiting for liver transplant)
library(survival)
library(KMsurv)
attach(hw4data)

#Q3####
agecat<- cut(age, c(0, 40, 60, max(age)))
num_age<- as.numeric(agecat)
age_levels<- cut(num_age, c(0, 1, 2, 3))
#Part a-interaction model#
model_1<-coxph(formula = Surv(time, status)~age_levels*gender, data=hw4data)
chk_model_1<-cox.zph(model_1) #check assumption of proportional hazards
#print(model_1)

#Part b-additive model#
model_2<-coxph(Surv(time, status)~age_levels+gender, data=hw4data, method="breslow")
#print(model_2)

#Part d-variate model#
model_3<-coxph(Surv(time, status)~num_age+gender, data=hw4data, method="breslow")