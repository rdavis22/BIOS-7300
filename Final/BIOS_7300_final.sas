DATA smoke;
set 'c:\Users\Rick\Documents\Tulane\MPH\BIOS 7300\Final\smoke.sas7bdat';
/*set indicator functions of employment for problem 2*/
employment1=(employment="ft");
employment2=(employment="pt");
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
contrast "Global Null hypothese Beta=0" treat 0 1 employment 0 1 1/estimate;
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
