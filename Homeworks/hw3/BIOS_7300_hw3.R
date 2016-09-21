# ###BIOS 7300 Hw 3###
library(survival)
library(KMsurv)
hw3data<-read.table(file=file.choose(), header=T, sep="")
#convert "hw3data" columns into class 'numeric'
 for (i in 1:length(hw3data)) {
   hw3data[i]<- as.numeric(hw3data[,i]) #will throw Nas warning error if you use hwdata[i]
 }
 attach(hw3data)
# ###Log Rank Test####
LogRank <- survdiff(Surv((time), status) ~ treatment)
#print(LogRank)
#
# ###Wilcoxon Test####
# Peto_Peto_modWilcox <- survdiff(Surv((time), status) ~ treatment, data=hw3data, rho=1)
# print(Peto_Peto_modWilcox)
library(survMisc)
wilcx <- survfit(Surv(time, status) ~ treatment, data = hw3data)
a<-comp(ten(wilcx))$tests$lrtests
# print(a)

###Part 3: Survival Comparison for 3 or greater groups####
# library(survival)
# library(KMsurv)
# library(survMisc)
part34data<-read.table(file=file.choose(), header=T, sep="")
attach(part34data)
agecat<- cut(age, c(min(age), 50, 60, max(age)))
age.surv <- survfit( Surv(time, status)~ strata(agecat), conf.type="log-log")
#
# #Log Rank test for 3 or greater groups
lrt_3 <- survdiff(Surv(time, status) ~ agecat, rho=0)
#
#Wilcoxon test for 3 or greater groups
wilcx_3 <- survfit(Surv(time, status) ~ agecat, data = part34data)
a_3<-comp(ten(wilcx_3))$tests$lrtests
#print(a_3)

#Cochran Armitage Trend for 3 or greater groups
# trend_3 <- coxph(Surv(time, status) ~ agecat, data=part34data)
# zz <- c(1,2) #number of strata computed by the Cox fit
# test.num <- zz %*% coef(trend_3) #Trend test statistic
# test.var <- zz %*% trend_3$var %*% zz #Variance of test
# lr_trend<-test.num/sqrt(test.var) #standard normal distribution [i.e.N~[0,1] ]
# print(lr_trend)

###Part 4: Differences in Survival by age and Gender####
#Log Rank test for gender stratified by age
lr_by_gender <- survdiff(Surv(time, status)~ gender+strata(agecat), data=part34data)
#Log Rank test for age stratified by gender
lr_by_age <- survdiff(Surv(time, status)~ agecat+strata(gender), data=part34data)