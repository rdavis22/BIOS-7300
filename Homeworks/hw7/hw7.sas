/*Set the path in sas by accessing the folder where "Whas500" is stored and assign a dummy
variable "hw7" for the unix path that follows*/
LIBNAME hw7 'C:\Users\Rick\Documents\Tulane\MPH\BIOS 7300\Homeworks\hw7';
/*create the variable "Hw7data" for the .sas7bdata to be read*/
DATA hw7.Hw7data;
/*Set the input for the data statement above as the "Whas500.sas7bdat" file*/
SET hw7.Whas500;
RUN;



/******Question 1*****/
proc phreg data=hw7.Hw7data;
/*lenfol=time variable and fstat(0) is our censoring term with 0 as censoring*/
model lenfol*fstat(0)= bmi gender age hr diasbp chf;
/*output my residuals to 'Work' folder in variable "a"*/
output out = a LOGSURV=LOGSURV RESDEV=RESDEV
RESMART =RESMART
/*Output first 2 Schoenfeld residuals: "bmi" and "gender"*/
RESSCH=RESSCH1 RESSCH2 RESSCH3 RESSCH4 RESSCH5 RESSCH6
WTRESSCH=WRESSCH1 WRESSCH2 WRESSCH3 WRESSCH4 WRESSCH5 WRESSCH6
RESSCO= RESSCO1 RESSCO2 RESSCO3 RESSCO4 RESSCO5 RESSCO6 xbeta=xbeta/method = ch;
run;
/*Q1 Part A*/
/*need to specify a new "output" otherwise, SAS will overwrite "a" every time
and we want to keep "a" around for later parts of the problem*/
proc sort data=a out=b;
/*by RESMART;*/
/*by descending RESMART;*/
/*by RESDEV;*/
/*by descending RESDEV;*/
/*by WRESSCH1;*/
/*by descending WRESSCH1;*/
/*by WRESSCH2;*/
/*by descending WRESSCH2;*/
/*by WRESSCH3;*/
/*by descending WRESSCH3;*/
/*by WRESSCH4;*/
/*by descending WRESSCH4;*/
/*by WRESSCH5;*/
/*by descending WRESSCH5;*/
/*by WRESSCH6;*/
by descending WRESSCH6;
run;
/*Q1 Part B*/
data a; set a; r=-logsurv; run;
proc lifetest data=a plot=logsurv;
time r*fstat(0);
survival out=c;
run;
/*Q1 Part C*/
/*rank the survival time variable "lenfol"*/
proc rank data=a out=d TIES=mean;
var lenfol; ranks timeRank; run;
/*proc print data=d; run;*/
/*Plot the Martingale residual vs. the rank of "lenfol"*/
title "Martingale residuals vs. timeRank";
proc sgplot data=d;
yaxis grid; refline 0 / axis=y;
scatter y=RESMART x=timeRank;
run;
/*Plot Deviance residuals vs. rank order of "lenfol"*/
title "Deviance residuals vs. timeRank";
proc sgplot data=d;
yaxis grid; refline 0 / axis=y;
scatter y=RESDEV x=timeRank;
run;
/*Q1 Part D*/
/*Martingale residuals vs. risk score*/
/******Question 1*****/
proc phreg data=hw7.Hw7data;
/*lenfol=time variable and fstat(0) is our censoring term with 0 as censoring*/
model lenfol*fstat(0)= gender age hr diasbp chf;
/*output my residuals to 'Work' folder in variable "a"*/
output out = e LOGSURV=LOGSURV RESDEV=RESDEV
RESMART =RESMART
/*Output first 2 Schoenfeld residuals: "bmi" and "gender"*/
RESSCH=RESSCH1 RESSCH2
WTRESSCH=WRESSCH1 WRESSCH2
RESSCO= RESSCO1 RESSCO2 xbeta=xbeta/method = ch;
run;
/*proc loess data=d;*/
/*model RESMART=xbeta;*/
/*run;*/
/*Martingale residuals vs. bmi*/
proc loess data=e;
model RESMART=bmi;
run;
/*proc loess data=d;*/
/*model RESMART=bmi^2;*/
/*run;*/




/******Question 2*****/
proc phreg data=hw7.Hw7data;
model lenfol*fstat(0)= bmi gender age hr diasbp chf;
output out=q2 ld=ld lmax=lmax/method =ch;
run;
/*Q2 Part 1*/
proc sort data=q2 out=q2_1;
/*by descending ld;*/
by descending lmax;
run;
/*Q2 Part 2*/
proc phreg data=hw7.Hw7data;
model lenfol*fstat(0)= bmi gender age hr diasbp chf;
output out=q2b logsurv=logsurv lmax=lmax ld=ld
dfbeta=dbmi/method =ch;
run;
proc sort data=q2b out=q2_2;
by descending dbmi;
/*by dbmi;*/
run;
/*Q2 Part 3*/
data q2;
   modify q2;
   if id=115 then remove;
run;
proc print data=q2;
   title 'Modified data with largest ';
run;
proc phreg data=q2;
model lenfol*fstat(0)=bmi gender age hr diasbp chf;
run;
