
/* ************************************************************************* * 
*   Creator: Tobias Eibinger and Sachintha Fernando
*   Project: Zero Fare
*   TRAFFIC COUNTS ESTIMATION
*   Date: 2026-04-05
** ************************************************************************* */ 




*-------------------------------------------------------
* all days - no covariates
*-------------------------------------------------------

clear
*all days
use "$final/full-traffic-count-data-alldays.dta"



******************************************************

*treatment
gen treatment = 0
replace treatment = 1 if nuts2_id=="LU00" & year>2019
replace treatment = 0 if nuts2_id=="LU00" & year==2020 & inlist(month, 1,2)

*drop covid periods
drop if year == 2020 & month>2
drop if year == 2021


*generate variables
gen lvolume = log(carsvolume)


* without covariates
set seed 130423
sdid lvolume nuts2_id mdate treatment, vce(placebo) graph g1on method(sdid) reps(200)





* standard DiD
gen treated_post = 0
replace treated_post = 1 if year >=2020
replace treated_post = 0 if year==2020 & inlist(month, 1, 2)

gen treated = 0
replace treated = 1 if substr(nuts2_id, 1, 2) == "LU"

reghdfe lvolume i.treated_post##i.treated, absorb(nuts2_id mdate) vce(cluster nuts2_id)

drop treated


******************************************************
* Event Study
******************************************************

gen yadj = lvolume

egen m=min(year) if treatment==1, by(nuts2_id) //indicator for the year of adoption

egen mm=mean(m), by(nuts2_id)
keep if mm==2022 | mm==. //keep only one time of adoption

*sdid with yadj
sdid yadj nuts2_id mdate treatment, vce(noinference) graph g1on 

*calculate equation 9 in Clarke et al. (2024)
matrix lambda = e(lambda)[1..14,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..14,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..14,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..38,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)

*check differences in yearly point estimates and baseline
matlist e(difference)
di meanpre_o


set seed 130423
*bootstrap w placebo inference
local b = 1
local B = 200 // num of reps
while `b' <= `B' {
preserve

*drop treated unit
drop if nuts2_id=="LU00"

*reset ids
drop id
egen id=group(nuts2_id)

*get max id
qui sum(id)
scalar max_id = r(max)
display max_id

*randomly pick placebo units
local placebo_unit = runiformint(1,max_id)

*assign placebo treatments
drop treatment
gen treatment = 0
replace treatment = 1 if (id==`placebo_unit' & year>2019)
replace treatment = 0 if (id==`placebo_unit' & year==2020 & inlist(month, 1,2))
gen treated = (id==`placebo_unit')

*run sdid
qui sdid yadj nuts2_id mdate treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..14,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco_b = e(series)[1..14,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..14,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(38,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 values
matrix d`b' = e(difference)[1..38,2] - meanpre_b // (define 1..X as number of periods)

*update loop
local ++b

restore
}


keep time d
keep if time!=.

local B = 200
forvalues b = 1/`B' {
svmat d`b'
}

egen rsd = rowsd(d11 - d2001) //calculate standard deviation of this difference (specifiy d11-dXX1, where XX is num. of reps)
gen LCI = d + invnormal(0.05)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.95)*rsd //upper bounds on bootstrap CIs

*generate plot
tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Monthly") xtitle("") ytitle("traffic volume - cars") xline(14, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))



******************************************************
* save according to weekday configuration
******************************************************

* all days
save "$final/es_traffic_placebo-nocovs-alldays.dta", replace

******************************************************





*-------------------------------------------------------
* weekdays - no covariates
*-------------------------------------------------------

clear
*weekday
use "$final/full-traffic-count-data-weekday.dta"


******************************************************

*treatment
gen treatment = 0
replace treatment = 1 if nuts2_id=="LU00" & year>2019
replace treatment = 0 if nuts2_id=="LU00" & year==2020 & inlist(month, 1,2)

*drop covid periods
drop if year == 2020 & month>2
drop if year == 2021


*generate variables
gen lvolume = log(carsvolume)


* without covariates
set seed 130423
sdid lvolume nuts2_id mdate treatment, vce(placebo) graph g1on method(sdid) reps(200)





* standard DiD
gen treated_post = 0
replace treated_post = 1 if year >=2020
replace treated_post = 0 if year==2020 & inlist(month, 1, 2)

gen treated = 0
replace treated = 1 if substr(nuts2_id, 1, 2) == "LU"

reghdfe lvolume i.treated_post##i.treated, absorb(nuts2_id mdate) vce(cluster nuts2_id)

drop treated


******************************************************
* Event Study
******************************************************

gen yadj = lvolume

egen m=min(year) if treatment==1, by(nuts2_id) //indicator for the year of adoption

egen mm=mean(m), by(nuts2_id)
keep if mm==2022 | mm==. //keep only one time of adoption

*sdid with yadj
sdid yadj nuts2_id mdate treatment, vce(noinference) graph g1on 

*calculate equation 9 in Clarke et al. (2024)
matrix lambda = e(lambda)[1..14,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..14,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..14,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..38,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)

*check differences in yearly point estimates and baseline
matlist e(difference)
di meanpre_o


set seed 130423
*bootstrap w placebo inference
local b = 1
local B = 200 // num of reps
while `b' <= `B' {
preserve

*drop treated unit
drop if nuts2_id=="LU00"

*reset ids
drop id
egen id=group(nuts2_id)

*get max id
qui sum(id)
scalar max_id = r(max)
display max_id

*randomly pick placebo units
local placebo_unit = runiformint(1,max_id)

*assign placebo treatments
drop treatment
gen treatment = 0
replace treatment = 1 if (id==`placebo_unit' & year>2019)
replace treatment = 0 if (id==`placebo_unit' & year==2020 & inlist(month, 1,2))
gen treated = (id==`placebo_unit')

*run sdid
qui sdid yadj nuts2_id mdate treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..14,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco_b = e(series)[1..14,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..14,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(38,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 values
matrix d`b' = e(difference)[1..38,2] - meanpre_b // (define 1..X as number of periods)

*update loop
local ++b

restore
}


keep time d
keep if time!=.

local B = 200
forvalues b = 1/`B' {
svmat d`b'
}

egen rsd = rowsd(d11 - d2001) //calculate standard deviation of this difference (specifiy d11-dXX1, where XX is num. of reps)
gen LCI = d + invnormal(0.05)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.95)*rsd //upper bounds on bootstrap CIs

*generate plot
tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Monthly") xtitle("") ytitle("traffic volume - cars") xline(14, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))



******************************************************
* save according to weekday configuration
******************************************************

* weekdays
save "$final/es_traffic_placebo-nocovs-weekday.dta", replace


******************************************************



*-------------------------------------------------------
* weekends - no covariates
*-------------------------------------------------------

clear
*weekend
use "$final/full-traffic-count-data-weekend.dta"


******************************************************

*treatment
gen treatment = 0
replace treatment = 1 if nuts2_id=="LU00" & year>2019
replace treatment = 0 if nuts2_id=="LU00" & year==2020 & inlist(month, 1,2)

*drop covid periods
drop if year == 2020 & month>2
drop if year == 2021


*generate variables
gen lvolume = log(carsvolume)


* without covariates
set seed 130423
sdid lvolume nuts2_id mdate treatment, vce(placebo) graph g1on method(sdid) reps(200)





* standard DiD
gen treated_post = 0
replace treated_post = 1 if year >=2020
replace treated_post = 0 if year==2020 & inlist(month, 1, 2)

gen treated = 0
replace treated = 1 if substr(nuts2_id, 1, 2) == "LU"

reghdfe lvolume i.treated_post##i.treated, absorb(nuts2_id mdate) vce(cluster nuts2_id)

drop treated


******************************************************
* Event Study
******************************************************

gen yadj = lvolume

egen m=min(year) if treatment==1, by(nuts2_id) //indicator for the year of adoption

egen mm=mean(m), by(nuts2_id)
keep if mm==2022 | mm==. //keep only one time of adoption

*sdid with yadj
sdid yadj nuts2_id mdate treatment, vce(noinference) graph g1on 

*calculate equation 9 in Clarke et al. (2024)
matrix lambda = e(lambda)[1..14,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..14,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..14,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..38,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)

*check differences in yearly point estimates and baseline
matlist e(difference)
di meanpre_o


set seed 130423
*bootstrap w placebo inference
local b = 1
local B = 200 // num of reps
while `b' <= `B' {
preserve

*drop treated unit
drop if nuts2_id=="LU00"

*reset ids
drop id
egen id=group(nuts2_id)

*get max id
qui sum(id)
scalar max_id = r(max)
display max_id

*randomly pick placebo units
local placebo_unit = runiformint(1,max_id)

*assign placebo treatments
drop treatment
gen treatment = 0
replace treatment = 1 if (id==`placebo_unit' & year>2019)
replace treatment = 0 if (id==`placebo_unit' & year==2020 & inlist(month, 1,2))
gen treated = (id==`placebo_unit')

*run sdid
qui sdid yadj nuts2_id mdate treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..14,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco_b = e(series)[1..14,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..14,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(38,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 values
matrix d`b' = e(difference)[1..38,2] - meanpre_b // (define 1..X as number of periods)

*update loop
local ++b

restore
}


keep time d
keep if time!=.

local B = 200
forvalues b = 1/`B' {
svmat d`b'
}

egen rsd = rowsd(d11 - d2001) //calculate standard deviation of this difference (specifiy d11-dXX1, where XX is num. of reps)
gen LCI = d + invnormal(0.05)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.95)*rsd //upper bounds on bootstrap CIs

*generate plot
tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Monthly") xtitle("") ytitle("traffic volume - cars") xline(14, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))



******************************************************
* save according to weekday configuration
******************************************************

* weekend
save "$final/es_traffic_placebo-nocovs-weekend.dta", replace

******************************************************








******************************************************
* With Covariates 
******************************************************





*-------------------------------------------------------
* all days - with covariates
*-------------------------------------------------------


clear

*all days
use "$final/full-traffic-count-data-alldays.dta"

******************************************************

xtset id mdate


*treatment
gen treatment = 0
replace treatment = 1 if nuts2_id=="LU00" & year>2019
replace treatment = 0 if nuts2_id=="LU00" & year==2020 & inlist(month, 1,2)

*drop covid periods

drop if year == 2020 & month>2
drop if year == 2021


*generate variables
gen lvolume = log(carsvolume)
gen lmean_prec = log(mean_prec)


* with covariates 
set seed 130423
sdid lvolume nuts2_id mdate treatment, covariates(lreal_diesel lreal_super mean_prec, projected) vce(placebo) graph g1on reps(200) method(sdid)
matlist e(beta)


******************************************************
* Event Study
******************************************************



*manually adjust
reghdfe lvolume lreal_diesel lreal_super mean_prec if nuts2_id!="LU00", absorb(nuts2_id mdate) vce(cluster id)

*Storing betas
gen bprec = _b[mean_prec]
gen bdiesel = _b[lreal_diesel]
gen bsuper = _b[lreal_super]

*gen yadj
gen yadj = lvolume - bprec*mean_prec - bdiesel*lreal_diesel - bsuper*lreal_super


egen m=min(year) if treatment==1, by(nuts2_id) //indicator for the year of adoption

egen mm=mean(m), by(nuts2_id)
keep if mm==2022 | mm==. //keep only one time of adoption

*sdid with yadj
sdid yadj nuts2_id mdate treatment, vce(noinference) graph g1on 

*calculate equation 9 in Clarke et al. (2024)
matrix lambda = e(lambda)[1..14,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..14,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..14,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..38,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)

*check differences in yearly point estimates and baseline
matlist e(difference)
di meanpre_o



set seed 130423
*bootstrap w placebo inference
local b = 1
local B = 200 // num of reps
while `b' <= `B' {
preserve


*drop treated unit
drop if nuts2_id=="LU00"

*reset ids
drop id
egen id=group(nuts2_id)

*get max id
qui sum(id)
scalar max_id = r(max)
display max_id


*randomly pick placebo units
local placebo_unit = runiformint(1, `=max_id')

*assign placebo treatments
drop treatment
gen treatment = 0
replace treatment = 1 if (id==`placebo_unit' & year>2019)
replace treatment = 0 if (id==`placebo_unit' & year==2020 & inlist(month, 1,2))
gen treated = (id==`placebo_unit')


*manually adjust
reghdfe lvolume lreal_diesel lreal_super mean_prec if treated==0, absorb(nuts2_id mdate) vce(cluster id)

drop yadj
drop bprec
drop bdiesel
drop bsuper

*Storing betas
gen bprec = _b[mean_prec]
gen bdiesel = _b[lreal_diesel]
gen bsuper = _b[lreal_super]

*gen yadj
gen yadj = lvolume - bprec*mean_prec - bdiesel*lreal_diesel - bsuper*lreal_super



*run sdid
qui sdid yadj nuts2_id mdate treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..14,1] //save lambda weight (define 1..4 as number of pre-treatment periods)
matrix yco_b = e(series)[1..14,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..14,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(38,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 values
matrix d`b' = e(difference)[1..38,2] - meanpre_b // (define 1..X as number of periods)

*update loop
local ++b

restore
}


keep time d
keep if time!=.

local B = 200
forvalues b = 1/`B' {
svmat d`b'
}

egen rsd = rowsd(d11 - d2001) //calculate standard deviation of this difference (specifiy d11-dXX1, where XX is num. of reps)
gen LCI = d + invnormal(0.05)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.95)*rsd //upper bounds on bootstrap CIs

*generate plot
tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Monthly") xtitle("") ytitle("traffic volume - cars") xline(14, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))


******************************************************
* save according to weekday configuration
******************************************************

* all days
save "$final/es_traffic_placebo-covs-alldays.dta", replace

******************************************************




*-------------------------------------------------------
* weekdays - with covariates
*-------------------------------------------------------


clear

*weekday
use "$final/full-traffic-count-data-weekday.dta"

******************************************************

xtset id mdate


*treatment
gen treatment = 0
replace treatment = 1 if nuts2_id=="LU00" & year>2019
replace treatment = 0 if nuts2_id=="LU00" & year==2020 & inlist(month, 1,2)

*drop covid periods

drop if year == 2020 & month>2
drop if year == 2021


*generate variables
gen lvolume = log(carsvolume)
gen lmean_prec = log(mean_prec)


* with covariates 
set seed 130423
sdid lvolume nuts2_id mdate treatment, covariates(lreal_diesel lreal_super mean_prec, projected) vce(placebo) graph g1on reps(200) method(sdid)
matlist e(beta)


******************************************************
* Event Study
******************************************************



*manually adjust
reghdfe lvolume lreal_diesel lreal_super mean_prec if nuts2_id!="LU00", absorb(nuts2_id mdate) vce(cluster id)

*Storing betas
gen bprec = _b[mean_prec]
gen bdiesel = _b[lreal_diesel]
gen bsuper = _b[lreal_super]

*gen yadj
gen yadj = lvolume - bprec*mean_prec - bdiesel*lreal_diesel - bsuper*lreal_super


egen m=min(year) if treatment==1, by(nuts2_id) //indicator for the year of adoption

egen mm=mean(m), by(nuts2_id)
keep if mm==2022 | mm==. //keep only one time of adoption

*sdid with yadj
sdid yadj nuts2_id mdate treatment, vce(noinference) graph g1on 

*calculate equation 9 in Clarke et al. (2024)
matrix lambda = e(lambda)[1..14,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..14,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..14,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..38,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)

*check differences in yearly point estimates and baseline
matlist e(difference)
di meanpre_o



set seed 130423
*bootstrap w placebo inference
local b = 1
local B = 200 // num of reps
while `b' <= `B' {
preserve


*drop treated unit
drop if nuts2_id=="LU00"

*reset ids
drop id
egen id=group(nuts2_id)

*get max id
qui sum(id)
scalar max_id = r(max)
display max_id


*randomly pick placebo units
local placebo_unit = runiformint(1, `=max_id')

*assign placebo treatments
drop treatment
gen treatment = 0
replace treatment = 1 if (id==`placebo_unit' & year>2019)
replace treatment = 0 if (id==`placebo_unit' & year==2020 & inlist(month, 1,2))
gen treated = (id==`placebo_unit')


*manually adjust
reghdfe lvolume lreal_diesel lreal_super mean_prec if treated==0, absorb(nuts2_id mdate) vce(cluster id)

drop yadj
drop bprec
drop bdiesel
drop bsuper

*Storing betas
gen bprec = _b[mean_prec]
gen bdiesel = _b[lreal_diesel]
gen bsuper = _b[lreal_super]

*gen yadj
gen yadj = lvolume - bprec*mean_prec - bdiesel*lreal_diesel - bsuper*lreal_super



*run sdid
qui sdid yadj nuts2_id mdate treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..14,1] //save lambda weight (define 1..4 as number of pre-treatment periods)
matrix yco_b = e(series)[1..14,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..14,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(38,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 values
matrix d`b' = e(difference)[1..38,2] - meanpre_b // (define 1..X as number of periods)

*update loop
local ++b

restore
}


keep time d
keep if time!=.

local B = 200
forvalues b = 1/`B' {
svmat d`b'
}

egen rsd = rowsd(d11 - d2001) //calculate standard deviation of this difference (specifiy d11-dXX1, where XX is num. of reps)
gen LCI = d + invnormal(0.05)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.95)*rsd //upper bounds on bootstrap CIs

*generate plot
tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Monthly") xtitle("") ytitle("traffic volume - cars") xline(14, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))


******************************************************
* save according to weekday configuration
******************************************************

* weekdays
save "$final/es_traffic_placebo-covs-weekday.dta", replace

******************************************************






*-------------------------------------------------------
* weekend - with covariates
*-------------------------------------------------------


clear
*weekend
use "$final/full-traffic-count-data-weekend.dta"

******************************************************

xtset id mdate


*treatment
gen treatment = 0
replace treatment = 1 if nuts2_id=="LU00" & year>2019
replace treatment = 0 if nuts2_id=="LU00" & year==2020 & inlist(month, 1,2)

*drop covid periods

drop if year == 2020 & month>2
drop if year == 2021


*generate variables
gen lvolume = log(carsvolume)
gen lmean_prec = log(mean_prec)


* with covariates 
set seed 130423
sdid lvolume nuts2_id mdate treatment, covariates(lreal_diesel lreal_super mean_prec, projected) vce(placebo) graph g1on reps(200) method(sdid)
matlist e(beta)


******************************************************
* Event Study
******************************************************



*manually adjust
reghdfe lvolume lreal_diesel lreal_super mean_prec if nuts2_id!="LU00", absorb(nuts2_id mdate) vce(cluster id)

*Storing betas
gen bprec = _b[mean_prec]
gen bdiesel = _b[lreal_diesel]
gen bsuper = _b[lreal_super]

*gen yadj
gen yadj = lvolume - bprec*mean_prec - bdiesel*lreal_diesel - bsuper*lreal_super


egen m=min(year) if treatment==1, by(nuts2_id) //indicator for the year of adoption

egen mm=mean(m), by(nuts2_id)
keep if mm==2022 | mm==. //keep only one time of adoption

*sdid with yadj
sdid yadj nuts2_id mdate treatment, vce(noinference) graph g1on 

*calculate equation 9 in Clarke et al. (2024)
matrix lambda = e(lambda)[1..14,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..14,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..14,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..38,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)

*check differences in yearly point estimates and baseline
matlist e(difference)
di meanpre_o



set seed 130423
*bootstrap w placebo inference
local b = 1
local B = 200 // num of reps
while `b' <= `B' {
preserve


*drop treated unit
drop if nuts2_id=="LU00"

*reset ids
drop id
egen id=group(nuts2_id)

*get max id
qui sum(id)
scalar max_id = r(max)
display max_id


*randomly pick placebo units
local placebo_unit = runiformint(1, `=max_id')

*assign placebo treatments
drop treatment
gen treatment = 0
replace treatment = 1 if (id==`placebo_unit' & year>2019)
replace treatment = 0 if (id==`placebo_unit' & year==2020 & inlist(month, 1,2))
gen treated = (id==`placebo_unit')


*manually adjust
reghdfe lvolume lreal_diesel lreal_super mean_prec if treated==0, absorb(nuts2_id mdate) vce(cluster id)

drop yadj
drop bprec
drop bdiesel
drop bsuper

*Storing betas
gen bprec = _b[mean_prec]
gen bdiesel = _b[lreal_diesel]
gen bsuper = _b[lreal_super]

*gen yadj
gen yadj = lvolume - bprec*mean_prec - bdiesel*lreal_diesel - bsuper*lreal_super



*run sdid
qui sdid yadj nuts2_id mdate treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..14,1] //save lambda weight (define 1..4 as number of pre-treatment periods)
matrix yco_b = e(series)[1..14,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..14,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(38,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 values
matrix d`b' = e(difference)[1..38,2] - meanpre_b // (define 1..X as number of periods)

*update loop
local ++b

restore
}


keep time d
keep if time!=.

local B = 200
forvalues b = 1/`B' {
svmat d`b'
}

egen rsd = rowsd(d11 - d2001) //calculate standard deviation of this difference (specifiy d11-dXX1, where XX is num. of reps)
gen LCI = d + invnormal(0.05)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.95)*rsd //upper bounds on bootstrap CIs

*generate plot
tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Monthly") xtitle("") ytitle("traffic volume - cars") xline(14, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))


******************************************************
* save according to weekday configuration
******************************************************

* weekend
save "$final/es_traffic_placebo-covs-weekend.dta", replace

******************************************************

