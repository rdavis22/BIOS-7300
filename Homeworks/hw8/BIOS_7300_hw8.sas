DATA hw8;
  INFILE "c:\Users\Rick\Documents\Tulane\MPH\BIOS 7300\Homeworks\hw8\hw8.dat";
  INPUT Patient Treatment time status;
/*delete any missing values*/
  if cmiss(of _all_) then delete;
RUN;
/*****Q1*****/
/*Q1 parts a-b*/
/*read in hw 8 data and see if the model fits the exponential distribution*/
/*also fit the data to the exponential distribution*/
proc lifereg data=Hw8;
model time*status(0)=/distribution=exponential;
probplot;
run;
/*****Q2*****/
/***Q2 part a***/
/*Use only the data from the placebo group*/
Data hw8_2;
set hw8;
/*Delete the non-placebo group*/
if Treatment ^= 1 then Delete;
run;

proc lifereg data=hw8_2;
model time*status(0)=/distribution=weibull;
probplot;
run;

/*****Q3*****/
/**Q3, parts A-C: Get the Weibull hazards model**/
proc lifereg data=hw8;
/*put the "Treatment variable into the class statement to make categorical*/
class Treatment;
model time*status(0)=Treatment/distribution=exponential;
probplot;
run;
proc lifereg data=hw8;
class Treatment;
model time*status(0)=Treatment/distribution=weibull;
probplot;
run;
