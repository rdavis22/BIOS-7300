if(!require(tidyverse))
  install.packages(("tidyverse"))
if(!require(survival))
  install.packages(("survival"))
if(!require(KMsurv))
  install.packages(("KMsurv"))

#read in hw8 data
#hw8.data<-read_table(file=file.choose(), col_names = T)

####Hw 8, Q1####
##Q1 Part a) graphical examination to see if exponential distribution fits##
#get the kaplan-meier estimate
km.hw8<-survfit(Surv(time, status)~1, data=hw8.data)
#get the km survival function estimates
survEst.hw8<-km.hw8$surv
#get the km time
survTime.hw8<-km.hw8$time
#get the log-log survival estimate
logLogSurvEst.hw8<-log(-log(survEst.hw8))
#get the log time of the km
logSurvTime.hw8<-log(survTime.hw8)

#plot the data
#create data frame with the log(time) and loglog transform of the KM...
#estimate of the data
hw8q1plot.data<-data.frame(survTime.hw8, survEst.hw8, logSurvTime.hw8,
                       logLogSurvEst.hw8)
p<-ggplot(data=hw8q1plot.data, aes(x=logSurvTime.hw8, y=logLogSurvEst.hw8))+
  #add the points
  geom_point()+
  #add the 95% CI
  geom_smooth(method=lm, se=T)+
  #add the title and center it in middle of the plot; add axis labels too
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Checking fit for exponential function",
       x="log(time)", y="loglog Survival Estimate")


##Q1 Part b) fit the survival distribution
#get the survival 
fit_exp<-survreg(Surv(time, status)~1,
                 data=hw8.data, dist="exponential")

##Q1 part c) estimating the median survival time
#get sigma(ti)
sigma_t<-sum(hw8.data$time)

#get number of failures
r=sum(hw8.data$status)

#get lambda_hat
lambda_hat<-r/sigma_t

#get median survival time
t_50<-1/lambda_hat*log(2)

#get SE and 95% CI for t_50
se_t_50<-t_50/sqrt(r)
CI_95_t_50<-c(t_50-1.96*se_t_50, t_50+1.96*se_t_50)

####HW8, Q2####
##Q2, Part A##
#include only the Placebo Group
hw8q2.data<-subset(hw8.data, Treatment==1)
#get the kaplan-meier estimate
km.hw8q2<-survfit(Surv(time, status)~1, data=hw8q2.data)
#get the km survival function estimates
survEst.hw8q2<-km.hw8q2$surv
#get the km time
survTime.hw8q2<-km.hw8q2$time
#get the log-log survival estimate
logLogSurvEst.hw8q2<-log(-log(survEst.hw8q2))
#get the log time of the km
logSurvTime.hw8q2<-log(survTime.hw8q2)

#plot the data for Q2
#create data frame with the log(time) and loglog transform of the KM...
#estimate of the data
hw8q2plot.data<-data.frame(survTime.hw8q2, survEst.hw8q2, logSurvTime.hw8q2,
                       logLogSurvEst.hw8q2)
p.q2<-ggplot(data=hw8q2plot.data, aes(x=logSurvTime.hw8q2, y=logLogSurvEst.hw8q2))+
  #add the points
  geom_point()+
  #add the 95% CI
  geom_smooth(method=lm, se=T)+
  #add the title and center it in middle of the plot; add axis labels too
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Checking fit for Weibull distribution for Placebo Only",
       x="log(time)", y="loglog Survival Estimate")

##Q2, Part B; Fit data to weibull distribution##
#get intercept and shape
fit_weib.q2<-survreg(Surv(time, status)~1,
                 data=hw8q2.data, dist="weibull")
#get Weibull shape and weibull scale
fit_weib_2.q2<-flexsurvreg(Surv(time,status)~1,
                           data=hw8q2.data, dist="weibull")