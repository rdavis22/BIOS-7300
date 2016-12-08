data mydata;
Infile "C:\Users\rdavis22\Documents\Survival of liver transplant recipients.dat";
Input patient age gender disease time status cof;
run;
/*proc phreg;*/
/*class gender disease cof ;*/
/*model time*status(0)=gender|cof disease|cof age|cof;*/
/*contrast "test 1" gender 1 /estimate ;*/
/*contrast "test 2" disease*cof 0 0 0 0 1, disease*cof 0 0 0 0 0 1,*/
/*disease*cof 0 0 0 0 0 0 1,disease*cof 0 0 0 0 0 0 0 1 /estimate ;*/
/*run;*/
/**********This is the code for Questions 2 and 3 on hw 5_6/****************/
proc phreg;
class gender disease cof ;
model time*status(0)=gender|cof disease|cof age|cof;
contrast "test 1" gender 1 /estimate ;
contrast "test 2" disease*cof 0 0 0 0 1, disease*cof 0 0 0 0 0 1,
disease*cof 0 0 0 0 0 0 1,disease*cof 0 0 0 0 0 0 0 1 /estimate ;
contrast "3a) gender=2 age=50 disease=1 cof=2 vs. gender=1 age=50 disease=2 cof=4" gender 1
disease -1 1 cof 0 0 -1/estimate;
contrast "3b) gender=2 age=50 d=3 c=4 vs. gender=2 age=55 d=2 c=4" disease 0 1 age 5/estimate;
run;
/******This is the code for Question 3, part 3*****/
proc phreg;
class gender disease cof /param=effect;
model time*status(0)=gender|cof age|cof;
contrast 'lrt gender|cof age|cof'
gender*cof 1,
gender*cof 0 1,
gender*cof 0 0 1,
gender*cof 0 0 0 1,
age*cof 1,
age*cof 0 1,
age*cof 0 0 1,
age*cof 0 0 0 1;
run;



/**********************This is the code for Question 4 on hw5_6********************/
proc phreg data=mydata;
model time*status(0)=age gender
/selection=stepwise start=3 slentry=0.20
slstay=0.10 details;
run;

/***************This is the code for Question 5 on hw5_6*****************/
/***part A***/ 
proc phreg data=mydata;
model time*status(0)= gender|cof disease|cof age|cof
/selection=backward slentry=0.15 slstay=0.10 details;
run;

/***part B***/
proc phreg data=mydata;
model time*status(0)=gender|cof disease|cof age|cof
/selection=stepwise start=0 slentry=0.15
slstay=0.10 details;
run;

/*************************This is the code for Question 6 of the HW**********************/
/*Part A*/
proc lifetest data=mydata method=km plots=(s h d);
time time*status(0);
run;
proc phreg data=mydata plot(overlay)=s plot(overlay)=cumhaz;
class disease(ref=last)cof(ref=last);
model time*status(0)= disease cof disease|cof;
contrast "disease 1 cof 0 vs. disease 3 cof 4" cof -1 disease -1 disease*cof -1/estimate;
baseline out=a survival=s lower=lcl upper=ucl CUMHAZ=cmh;
run;

/*Part B*/
proc phreg data=mydata plot(overlay)=s plot(overlay)=cumhaz;
class disease(ref=first) cof(ref=first);
model time*status(0)= disease cof disease|cof;
contrast "disease 1 cof 0 vs. disease 3 cof 4" cof 0 0 0 1 disease 0 1
disease*cof 0 0 0 0 0 0 0 1/estimate;
baseline out=a survival=s lower=lcl upper=ucl CUMHAZ=cmh;
run;
