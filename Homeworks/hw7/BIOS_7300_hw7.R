####BIOS 7300 hw 7####
####Problem 1####
#rm(list=ls())
if(!require(sas7bdat))
  install.packages("sas7bdat")
if(!require(dplyr))
  install.packages("dplyr")
if(!require(tibble))
  install.packages("tibble")
if(!require(survival))
  install.packages("survival")
if(!require(KMsurv))
  install.packages("KMsurv")
if(!require(ggplot2))
  install.packages("ggplot2")
#read in data from Sas file
#hw7data<-read.sas7bdat(file = file.choose(), debug=FALSE)
hw7tibble<-as_tibble(hw7data)
attach(hw7tibble)

##Q1##
#create coxfit model for Q1
result.model.coxph<-coxph(Surv(lenfol, fstat)~bmi+gender+age+hr+
                            diasbp+chf)
#get martingale, deviance, and schoenfeld residuals
rr.model.mart<-residuals(result.model.coxph, type=c("martingale"))
rr.model.dev<-residuals(result.model.coxph, type=c("deviance"))
rr.model.sch<-residuals(result.model.coxph, type=c("schoenfeld"))
detach(hw7tibble)

####Problem 3####
##Part A##
# hw7tibble_bmiless <- filter(hw7tibble, bmi<=25)
# hw7tibble_bmigreat <- filter(hw7tibble, bmi>25)

#Create survival curve of bmi <= 25
attach(hw7tibble_bmiless)
#turn bmi into a logical variable based on bmi. Remember, we are plotting the survival...
#curves based on a categorical bmi level, not the continuouS predictor of "bmi"
bmi_less <- (bmi<=25)
result.surv.bmiless <- survfit(Surv(lenfol, fstat)~bmi_less)
time.bmiless <- result.surv.bmiless$time
surv.bmiless <- result.surv.bmiless$surv
cloglog.bmiless <- log(-log(surv.bmiless))
logtime.bmiless <- log(time.bmiless)
detach(hw7tibble_bmiless)

#Create survival curve of bmi >25
attach(hw7tibble_bmigreat)
bmi_great <- (bmi>25)
result.surv.bmigreat<-survfit(Surv(lenfol, fstat)~bmi_great)
time.bmigreat <- result.surv.bmigreat$time
surv.bmigreat <- result.surv.bmigreat$surv
cloglog.bmigreat <- log(-log(surv.bmigreat))
logtime.bmigreat <- log(time.bmigreat)
detach(hw7tibble_bmigreat)

#plot the two curves
# plot(cloglog.bmiless~logtime.bmiless, type="s", col="blue", lwd=2)
# lines(cloglog.bmigreat~logtime.bmigreat, type="s", col="red", lwd=2)
# legend("bottomright", legend=c("BMI <=25", "BMI > 25"), col=c("blue", "red"), lwd=2, cex=0.75)
p<-ggplot()+
  #add the 2 plots
  geom_step(aes(x=logtime.bmiless, y=cloglog.bmiless, colour="darkblue"))+
  geom_step(aes(x=logtime.bmigreat, y=cloglog.bmigreat, colour="red"))+
  scale_color_manual(labels=c("bmi>25", "bmi<=25"), values=c("red", "darkblue"))+
  labs(title="Checking Proportional Hazards Assumptipon", x="Log time",
       y="Complementary log-log Hazard", color="Bmi levels")
  
  
##Parts B and C##
#get the error "Error in x[ord, ] : subscript out of bounds" when I try to run model with...
#the "result.model.coxph" object created earlier. Solutions?
result.model.coxzph<-cox.zph(coxph(Surv(lenfol, fstat)~bmi+gender+age+hr+
                                      diasbp+chf), transform = "log", global=T)
print(result.model.coxzph)