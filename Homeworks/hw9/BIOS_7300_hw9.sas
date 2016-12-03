DATA prostate;
  INFILE "c:\Users\Rick\Documents\Tulane\MPH\BIOS 7300\Homeworks\hw9\hw9.dat";
  INPUT patient treatment time status age shb size index;
/*delete any missing values*/
  if cmiss(of _all_) then delete;
  if treatment=1 then treatment=1; else treatment=0
RUN;

/*Q1-Fitting AFT to a Weibull distribution*/
proc lifereg data=prostate;
model time*status(0)=age treatment
/distribution=weibull;
run;
