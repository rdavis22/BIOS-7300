####BIOS 7300 Final Exam####
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(sas7bdat))
  install.packages("sas7bdat")
if(!require(survival))
  install.packages("survival")
if(!require(flexsurv))
  install.packages("flexsurv")
if(!require(km.ci))
  install.packages("km.ci")

####Data entry####
#final.data<-read.sas7bdat(file=file.choose(), debug = F)
final.tibble<-as_tibble(final.data)
attach(final.tibble)

####Q1####
#a) number of subjects, events, and censoring
num_subj<-length(status)
num_events<-sum(status)
num_censor<-num_subj-num_events

#b) kaplan-meier estimates and 95% CI for two treatment groups
#initial overal survival curve
q1.km<-survfit(Surv(time, status)~1, conf.type="log-log", 
                     data=final.tibble)

#subset data to get KM estimate for each group
q1_trt0.km<-survfit(Surv(time, status)~1, conf.type="log-log", 
                data=subset(final.tibble, treat==0))
q1_trt1.km<-survfit(Surv(time, status)~1, conf.type="log-log", 
                     data=subset(final.tibble, treat==1))

#hall-wellner pointwise confidence bands
q1.hw <- km.ci(q1.km, method="hall-wellner")
q1_trt0.hw <- km.ci(q1_trt0.km, method="hall-wellner")
q1_trt1.hw <- km.ci(q1_trt1.km, method="hall-wellner")

#e) LogRank test for survival curves
lr_test<-survdiff(Surv(time, status)~treat)

####Q2####
employment<-relevel(employment, "other")
q2.coxph<-coxph(Surv(time, status)~treat+employment)

####Q3: Model selection####
#*Do forward and backward selection at some point

####Q4: Weibull model####
##part D: Comparing difference across employment groups, holding treatment constant
q4.weib<-flexsurvreg(Surv(time, status)~treat+employment,
                 dist="exponential")