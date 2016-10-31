####BIOS 7300 hw 7####
#rm(list=ls())
if(!require(sas7bdat))
  install.packages("sas7bdat")
if(!require(dplyr))
  install.packages("dplyr")
if(!require(tibble))
  install.packages("tibble")
if(!require(survival))
  install.packages("survival")
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
attach(rr.model.sch)
