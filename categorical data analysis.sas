DATA ncbirths;
infile 'C:\Users\mylau\Desktop\661\ncbirths.txt' dlm='09'x firstobs=2;
length mature $11 premie $9 marital $11
     lowbirthweight $11 gender $11 habit $11 whitemom $11;
INPUT fage mage mature$ weeks premie$ visits marital$ gained weight lowbirthweight$
gender$ habit$ whitemom$;
if premie= 'full term' then premie=0;
else premie=1;if premie= 0 then n_premie=0;
else if premie= 1 then n_premie=1;if lowbirthweight= 'not low' then lowbirthweight=0;
else if lowbirthweight='low' then lowbirthweight=1;if lowbirthweight= 0 then n_lbw=0;
else if lowbirthweight=1 then n_lbw=1;p_or_lbw = n_premie + n_lbw;
run;data ncbirths;
   set ncbirths;
   array char_vars{*} $ _character_;
   do i = 1 to dim(char_vars);
      if char_vars{i} = 'NA' then
      char_vars{i}=' ';
   end;
   drop i;
run;* There are premie babies that are not LBW ;
data premie_not_LBW;
set ncbirths;
where premie ne lowbirthweight;
run;* EDA;
PROC SGPlot DATA=ncbirths;
	TITLE "Premie or LBW: Marital Status";
	vbar marital/ group=p_or_lbw groupdisplay=cluster;
RUN;PROC SGPlot DATA=ncbirths;
	TITLE "Premie or LBW: Infant's Gender";
	vbar gender/ group=p_or_lbw groupdisplay=cluster;
RUN;PROC SGPlot DATA=ncbirths;
	TITLE "Premie or LBW: Mom's Race";
	vbar whitemom/ group=p_or_lbw groupdisplay=cluster;
RUN;PROC SGPlot DATA=ncbirths;
	TITLE "Premie or LBW: Smoking Habit";
	vbar habit/ group=p_or_lbw groupdisplay=cluster;
RUN;PROC SGPlot DATA=ncbirths;
	TITLE "Premie or LBW: Mother's Age";
	vbox mage/ group=p_or_lbw groupdisplay=cluster;
RUN;PROC SGPlot DATA=ncbirths;
	TITLE "Premie or LBW: Number of Hospital Visits";
	vbox visits/ group=p_or_lbw groupdisplay=cluster;
RUN;* Cumulative Logit ;* Main Effects;* Simplest model with highest Chi-Sq score under best subset selection;
title "Cumulative logit: Best Model";
proc logistic data=ncbirths;
class p_or_lbw marital whitemom/param=ref;
model p_or_lbw = mage visits marital whitemom
/link=clogit aggregate scale=none lackfit;
output out=prb predprobs=I;
run;title "Main Effects Best Subset Selection";
proc logistic data=ncbirths;
class p_or_lbw marital habit whitemom gender/param=ref;
model p_or_lbw = mage visits marital gender habit whitemom
/link=clogit aggregate scale=none selection= score best=5 lackfit;
output out=prb predprobs=I;
run;title "Main Effects Stepwise Selection";
proc logistic data=ncbirths;
class p_or_lbw marital habit whitemom gender/param=ref;
model p_or_lbw = mage visits marital gender habit whitemom
/link=clogit aggregate scale=none selection= stepwise lackfit;
output out=prb predprobs=I;
run;* Interaction effects - Do not seem to improve AIC;
title "Interaction effects: cumulative logit";
proc logistic data=ncbirths;
class p_or_lbw marital habit whitemom gender/param=ref;
effect V = collection(marital habit whitemom gender);
effect W = collection(mage visits);
model p_or_lbw = V | W
/link=clogit aggregate scale=none selection=backward lackfit;
output out=prb predprobs=I;
run;title "Specific interaction effects: cumulative logit";
proc logistic data=ncbirths;
class p_or_lbw marital habit whitemom gender/param=ref;
model p_or_lbw = mage visits marital gender habit whitemom
marital*visits mage*visits marital*mage
/link=clogit aggregate scale=none selection=score lackfit;
output out=prb predprobs=I;
run;
