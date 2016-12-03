if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(survival))
  install.packages("survival")

####Data processing####
prostate.data<-read_table(file=file.choose(), col_names=T) %>%
  na.omit()
#turn "treatment" predictor into class "factor"
prostate.data<-mutate_each(prostate.data, funs(factor), treatment)
#relevel to have "2" == "0" in the contrast matrix and "1"=="1" in the matrix
treatment<-relevel(prostate.data$treatment, "2", "1")

####Q1: Weibull AFT####
weibhw9.model<-survreg(Surv(time, status)~age+treatment, dist="weibull",
                       data=prostate.data)

####Q2: Log-Logistic AFT
logloghw9.model<-survreg(Surv(time, status)~age+treatment, dist="loglogistic",
                       data=prostate.data)