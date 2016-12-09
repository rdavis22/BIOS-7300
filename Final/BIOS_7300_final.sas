DATA smoke;
set 'c:\Users\Rick\Documents\Tulane\MPH\BIOS 7300\Final\smoke.sas7bdat';
/*set indicator functions of employment for problem 2*/
employment_ft=(employment="ft");
employment_pt=(employment="pt");
run;

/*****Q1*****/
/*a-e)*/
PROC LIFETEST DATA=smoke method=km plots=(s d h) conftype=loglog
confband=all outsurv=ci;
TIME time*status(0);
strata treat;
RUN;

/*****Q2*****/
/**Parts A-C, E**/
proc phreg data=smoke plot(overlay)=s plot(overlay)=cumhaz;
class treat(ref=first) employment(ref="other");
model time*status(0)=treat employment;
/*model time*status(0)=treat employment1 employment2;*/
baseline out=a survival=s lower=lcl upper=ucl CUMHAZ=cmh;
/*contrast for global null that Beta=0 (ref=first; employment="other")*/
/*1st line: is treat =0*/
contrast "Global Null hypothesis Beta=0" treat 1,
/*2nd line: is employment "pt" =0*/
employment 0 1,
/*3rd line: is employment "ft" =0*/
employment 1/estimate;
run;

/**Part D**/
/*Get the survival function for treat=1, employment="ft"*/
proc phreg data=smoke plot(overlay)=s plot(overlay)=cumhaz;
/*make treat=1 and employment="ft" the reference levels*/
class treat(ref=last) employment(ref="ft");
model time*status(0)=treat employment;
/*baseline survival function is now the function of interest*/
baseline out=a survival=s lower=lcl upper=ucl CUMHAZ=cmh;
run;

/*****Q3*****/
/**part a: forward selection**/
proc phreg data=smoke;
class treat gender race employment;
model time*status(0)=treat age gender race employment
treat|age treat|gender treat|race treat|employment
/selection=forward start=0 slentry=0.15
slstay=0.10 details;
run;

/**part b: backward selection**/
proc phreg data=smoke;
class treat gender race employment;
model time*status(0)=treat age gender race employment 
treat|age treat|gender treat|race treat|employment
/selection=backward slentry=0.20
slstay=0.15 details;
run;

/*****Q4*****/
/*Parts A-D*/
proc lifereg data=smoke;
/*make treat=0 and employment="other" the reference levels*/
class treat;
/*employment1=(employment="ft"), employment2=(employment="pt")*/
model time*status(0)=treat employment_ft employment_pt
/distribution=weibull;
probplot;
run;

/*Part E*/
proc phreg data=smoke;
/*make treat=0 and employment="other" the reference levels*/
class treat;
/*employment1=(employment="ft"), employment2=(employment="pt")*/
model time*status(0)=treat employment_ft employment_pt;
run;

/*****Q5*****/
proc lifereg data=smoke;
/*make treat=0 and employment="other" the reference levels*/
class treat;
/*employment1=(employment="ft"), employment2=(employment="pt")*/
model time*status(0)=treat employment_ft employment_pt
/*note that we are fitting a 'log-logistic' distribution*/
/distribution=llogistic;
probplot;
run;

/*****Q6*****/
/*Q6 A: All subject followed until death*/
proc power;
twosamplesurvival test=logrank
hazardratio = .666666667
refsurvexphazard = 0.346574
followuptime = 5
totaltime = 5
ntotal = .
power = .9;
run;

/*Q6 B: 5 year study with first 2 years as recruitment study (equal # each month)*/
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

/*Q6 C: same design as part "B" but with 10% loss of follow-up*/
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
