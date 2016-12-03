DATA prostate;
  INFILE "c:\Users\Rick\Documents\Tulane\MPH\BIOS 7300\Homeworks\hw9\hw9.dat";
  INPUT patient treatment time status age shb size index;
/*delete any missing values*/
  if cmiss(of _all_) then delete;
  if treatment=1 then treatment=1; else treatment=0;
RUN;

/*****Q1-Fitting AFT to a Weibull distribution*****/
proc lifereg data=prostate;
class treatment;
model time*status(0)=age treatment
/distribution=weibull;
run;

/***Q2: Fitting the Log-logistic AFT model***/
proc lifereg data=prostate;
class treatment;
model time*status(0)=age treatment
/distribution=llogistic;
run;

/***Q3: power analysis***/
/*Q3 A: All subject followed until death*/
proc power;
twosamplesurvival test=logrank
hazardratio = .666666667
refsurvexphazard = 0.346574
followuptime = 5
totaltime = 5
ntotal = .
power = .9;
run;

/*Q3 B: 5 year study with first 2 years as recruitment study (equal # each month)*/
proc power;
twosamplesurvival test=logrank
hazardratio = .666666667
refsurvexphazard = 0.346574
/* !Note, the follow-up time is 3 years when you have a 5 yr study with first two years...
as recruitment*/
followuptime = 3
totaltime = 5
ntotal = .
power = .9;
run;

/*Q3 C: same design as part "B" but with 10% loss of follow-up*/
proc power;
twosamplesurvival test=logrank
hazardratio = .666666667
refsurvexphazard = 0.346574
followuptime = 3
totaltime = 5
ntotal = .
/*loss of follow-up=-ln(.9)*/
glossexphazards =.105361|.105361
power = .9;
run;
