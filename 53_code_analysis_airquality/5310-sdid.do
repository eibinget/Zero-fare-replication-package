/* ************************************************************************* * 
*   Creator: Tobias Eibinger and Sachintha Fernando
*   Project: Zero Fare
*   Sdid analysis for airquality
*   Date: 2026-04-05
** ************************************************************************* */ 



****************************************************************************
* 1. SDID - with all stations Monthly mean
********************************************************************************
clear
use "$final/no2_nuts2_with_precip.dta"

encode nuts_id, gen(id)


gen treatment = 0
replace treatment = 1 if nuts_id=="LU00" & year> 2019
gen mdate =mofd(date)

gen ln_no2_avg=log(monthly_mean_no2)

*start from March 2018 (because one station in LU doesn't have data for 2018 jan and feb)
drop if mdate < 698
tab month if year==2018

*Dropping already treated NUTS2 units

* Cascais in Portugal 
drop if inlist(nuts_id, "PT17")

* French Regions - Calacaise + Dunkirk
drop if inlist(nuts_id, "FRE1")

*Nuts 2 ring around Luxembourg
drop if inlist(nuts_id, "BE34", "BE33", "DEB2", "DEC0", "FRF3")

drop if inlist(nuts_id, "PL84", "PL51", "PL22", "PL41", "PL63", "PL52")
drop if inlist(nuts_id, "PL71", "PL91", "PL92")
drop if inlist(nuts_id, "PL62","PL81", "PL21" )

* Additional drops - extension to PT
drop if inlist(nuts_id, "EL30", "FRG0", "FRF1", "FR10")

* Dropping Austria because of Climate ticket

drop if inlist(nuts_id, "AT12", "AT22", "AT21", "AT31", "AT32", "AT33", "AT34")

* Dropping Madiera
drop if inlist(nuts_id, "PT30") // Madiera

* Dropping Canary islands
drop if inlist(nuts_id, "ES70") // Canary Islands


drop if year==2020 & month > 2
drop if year==2021

codebook nuts_id

*No covariates 
*******************************************************************************
drop if missing(mean_prec) 

codebook nuts_id

sdid ln_no2_avg nuts_id mdate treatment, vce(placebo) graph g1on reps(500) seed(130423)


*event study
*-------------------------------------
egen m=min(mdate) if treatment==1, by(nuts_id) //indicator for the year of adoption

egen mm=mean(m), by(nuts_id)
keep if mm==720 | mm==. //keep only one time of adoption

sdid ln_no2_avg nuts_id mdate treatment, vce(noinference) graph g1on

matrix lambda = e(lambda)[1..22,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..22,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..22,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..48,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)
matlist e(difference)
di meanpre_o

*drop treated unit
drop if nuts_id=="LU00"

*reset ids
drop id
egen id=group(nuts_id)

*get max id
qui sum(id)
scalar max_id = r(max)
display max_id

set seed 130423
*bootstrap w placebo inference
local b = 1
local B = 500 // num of reps
while `b' <= `B' {
preserve

*randomly pick placebo units
local placebo_unit = runiformint(1,max_id)

*assign placebo treatments
drop treatment
gen treatment = (mdate>719 & id==`placebo_unit')

*run sdid
qui sdid ln_no2_avg nuts_id mdate treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..22,1] //save lambda weight (define 1..4 as number of pre-treatment periods)
matrix yco_b = e(series)[1..22,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..22,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(48,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 values
matrix d`b' = e(difference)[1..48,2] - meanpre_b // (define 1..X as number of periods)

*update loop
local ++b

restore
}

preserve
keep time d
keep if time!=.

local B = 500
forvalues b = 1/`B' {
svmat d`b'
}

egen rsd = rowsd(d11 - d5001) //calculate standard deviation of this difference (specifiy d11-dXX1, where XX is num. of reps)
gen LCI = d + invnormal(0.025)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.975)*rsd //upper bounds on bootstrap CIs
*generate plot
tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-monthly") xtitle("") ytitle("no2") xlab(698(1)767, angle(45)) xline(720, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))

save "$final/es_no_covariates_all_no2.dta", replace
restore

* With covariates 
*************************************************************
clear
use "$final/no2_nuts2_with_precip.dta"

gen treatment = 0
replace treatment = 1 if nuts_id=="LU00" & year> 2019
gen mdate =mofd(date)

gen ln_no2_avg=log(monthly_mean_no2)

encode nuts_id, gen(id)
xtset id mdate

*start from March 2018 (because one station in LU doesn't have data for 2018 jan and feb)
drop if mdate < 698
tab month if year==2018

*Dropping already treated NUTS2 units

* Cascais in Portugal 
drop if inlist(nuts_id, "PT17")

* French Regions - Calacaise + Dunkirk
drop if inlist(nuts_id, "FRE1")

*Nuts 2 ring around Luxembourg
drop if inlist(nuts_id, "BE34", "BE33", "DEB2", "DEC0", "FRF3")

drop if inlist(nuts_id, "PL84", "PL51", "PL22", "PL41", "PL63", "PL52")
drop if inlist(nuts_id, "PL71", "PL91", "PL92")
drop if inlist(nuts_id, "PL62","PL81", "PL21" )

* Additional drops - extension to PT
drop if inlist(nuts_id, "EL30", "FRGO", "FRF1", "FR10")

* Dropping Austria because of Climate ticket

drop if inlist(nuts_id, "AT12", "AT22", "AT21", "AT31", "AT32", "AT33", "AT34")

* Dropping Madiera

drop if inlist(nuts_id, "PT30") // Madiera

* Dropping Canary islands
drop if inlist(nuts_id, "ES70") // Canary Islands

drop if year==2020 & month > 2
drop if year==2021

*------------------------------------
drop if missing(mean_prec)

reghdfe ln_no2_avg mean_prec lreal_diesel lreal_super if nuts_id!="LU00", absorb(nuts_id mdate) vce(cluster nuts_id)

*Storing betas
gen bprec = _b[mean_prec]
gen bdiesel = _b[lreal_diesel]
gen bsuper =_b[lreal_super]

*gen yadj
gen yadj = ln_no2_avg - bprec*mean_prec  - bdiesel*lreal_diesel - bsuper*lreal_super

sdid yadj nuts_id mdate treatment,  vce(placebo) graph g1on reps(500) seed(130423)


*start ES
*-------------------------------------
egen m=min(mdate) if treatment==1, by(nuts_id) //indicator for the year of adoption

egen mm=mean(m), by(nuts_id)
keep if mm==720 | mm==. //keep only one time of adoption

*sdid with yadj
sdid yadj nuts_id mdate treatment, vce(noinference) graph g1on 

matrix lambda = e(lambda)[1..22,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..22,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..22,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..48,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)


*drop treated unit
drop if nuts_id=="LU00"

*reset ids
drop id
egen id=group(nuts_id)

*get max id
qui sum(id)
scalar max_id = r(max)
display max_id

*bootstrap w placebo inference
set seed 130423
local b = 1
local B = 500 // num of reps
while `b' <= `B' {
preserve

*randomly pick placebo units
local placebo_unit = runiformint(1,max_id)

*assign placebo treatments
drop treatment
gen treatment = (mdate>719 & id==`placebo_unit')
gen treated = (id==`placebo_unit')

*run sdid
reghdfe ln_no2_avg mean_prec lreal_diesel lreal_super if id!=`placebo_unit', absorb(nuts_id mdate) vce(cluster nuts_id)

*drop yadj
drop yadj


*drop old betas
drop bprec
drop bdiesel
drop bsuper

*Storing beta
gen bprec = _b[mean_prec]
gen bdiesel = _b[lreal_diesel]
gen bsuper =_b[lreal_super]

*gen yadj
gen yadj = ln_no2_avg - bprec*mean_prec  - bdiesel*lreal_diesel - bsuper*lreal_super



*sdid with yadj
qui sdid yadj nuts_id mdate treatment, vce(noinference) graph 



*collect quantities from above
matrix lambda_b = e(lambda)[1..22,1] //save lambda weight (define 1..4 as number of pre-treatment periods)
matrix yco_b = e(series)[1..22,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..22,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(48,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 values
matrix d`b' = e(difference)[1..48,2] - meanpre_b // (define 1..X as number of periods)

*update loop
local ++b

restore
}

preserve
keep time d
keep if time!=.

local B = 500
forvalues b = 1/`B' {
svmat d`b'
}

egen rsd = rowsd(d11-d5001) //calculate standard deviation of this difference (specifiy d11-dXX1, where XX is num. of reps)
gen LCI = d + invnormal(0.025)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.975)*rsd //upper bounds on bootstrap CIs
*generate plot
tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-monthly") xtitle("") ytitle("no2") xlab(698(1)767, angle(45)) xline(720, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))

save "$final/es_with_covariates_all_no2.dta", replace
restore




*******************************************************************************
* 2. SDID - with only urban stations regions 
********************************************************************************
clear
use  "$final/no2_nuts2_with_precip_urban.dta"

gen treatment = 0
replace treatment = 1 if nuts_id=="LU00" & year> 2019
gen mdate =mofd(date)

gen ln_no2_avg=log(monthly_mean_no2)

*start from March 2018 (because one station in LU doesn't have data for 2018 jan and feb)
drop if mdate < 698
tab month if year==2018

*Dropping already treated NUTS2 units

* Cascais in Portugal 
drop if inlist(nuts_id, "PT17")

* French Regions - Calacaise + Dunkirk
drop if inlist(nuts_id, "FRE1")

*Nuts 2 ring around Luxembourg
drop if inlist(nuts_id, "BE34", "BE33", "DEB2", "DEC0", "FRF3")

drop if inlist(nuts_id, "PL84", "PL51", "PL22", "PL41", "PL63", "PL52")
drop if inlist(nuts_id, "PL71", "PL91", "PL92")
drop if inlist(nuts_id, "PL62","PL81", "PL21" )

* Additional drops - extension to PT
drop if inlist(nuts_id, "EL30", "FRGO", "FRF1", "FR10")

* Dropping Austria because of Climate ticket

drop if inlist(nuts_id, "AT12", "AT22", "AT21", "AT31", "AT32", "AT33", "AT34")

* Dropping Madiera

drop if inlist(nuts_id, "PT30") // Madiera

* Dropping Canary islands
drop if inlist(nuts_id, "ES70") // Canary Islands


* Year restriction 
drop if year==2020 & month > 2
drop if year==2021


* 2.1 No covariates 
********************************************************************************
*sdid monthly_mean_no2 nuts_id mdate treatment,  vce(placebo) graph g1on reps(50) seed(130423)
 drop if missing(mean_prec)                      

sdid ln_no2_avg nuts_id mdate treatment, vce(placebo) graph g1on reps(500) seed(130423)

*-----------------------------------------------------------------------
*event study
*-------------------------------------
egen m=min(mdate) if treatment==1, by(nuts_id) //indicator for the year of adoption

egen mm=mean(m), by(nuts_id)
keep if mm==720 | mm==. //keep only one time of adoption

sdid ln_no2_avg nuts_id mdate treatment, vce(noinference) graph g1on 

matrix lambda = e(lambda)[1..22,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..22,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..22,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..48,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)


*drop treated unit
drop if nuts_id=="LU00"

*reset ids
*drop id
egen id=group(nuts_id)

*get max id
qui sum(id)
scalar max_id = r(max)
display max_id

set seed 130423
*bootstrap w placebo inference
local b = 1
local B = 500 // num of reps
while `b' <= `B' {
preserve

*randomly pick placebo units
local placebo_unit = runiformint(1,max_id)

*assign placebo treatments
drop treatment
gen treatment = (mdate>719 & id==`placebo_unit')

*run sdid
qui sdid ln_no2_avg nuts_id mdate treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..22,1] //save lambda weight (define 1..4 as number of pre-treatment periods)
matrix yco_b = e(series)[1..22,2] //control baseline (define 1..X as number of pre-treatment periods)do "C:\Users\anbyz\AppData\Local\Temp\STD20cc_000015.tmp"

matrix ytr_b = e(series)[1..22,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(48,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 values
matrix d`b' = e(difference)[1..48,2] - meanpre_b // (define 1..X as number of periods)

*update loop
local ++b

restore
}

preserve
keep time d
keep if time!=.

local B = 500
forvalues b = 1/`B' {
svmat d`b'
}

egen rsd = rowsd(d11-d5001) //calculate standard deviation of this difference (specifiy d11-dXX1, where XX is num. of reps)
gen LCI = d + invnormal(0.025)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.975)*rsd //upper bounds on bootstrap CIs
*generate plot
tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-monthly") xtitle("") ytitle("no2") xlab(698(1)767, angle(45)) xline(720, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))

save "$final/es_no_covariates_urban_no2.dta", replace
restore


*With covariates
**************************
clear
use  "$final/no2_nuts2_with_precip_urban.dta"


gen treatment = 0
replace treatment = 1 if nuts_id=="LU00" & year> 2019
gen mdate =mofd(date)

gen ln_no2_avg=log(monthly_mean_no2)

encode nuts_id, gen(id)
xtset id mdate

*start from March 2018 (because one station in LU doesn't have data for 2018 jan and feb)
drop if mdate < 698
tab month if year==2018

*Dropping already treated NUTS2 units

* Cascais in Portugal 
drop if inlist(nuts_id, "PT17")

* French Regions - Calacaise + Dunkirk
drop if inlist(nuts_id, "FRE1")

*Nuts 2 ring around Luxembourg
drop if inlist(nuts_id, "BE34", "BE33", "DEB2", "DEC0", "FRF3")

drop if inlist(nuts_id, "PL84", "PL51", "PL22", "PL41", "PL63", "PL52")
drop if inlist(nuts_id, "PL71", "PL91", "PL92")
drop if inlist(nuts_id, "PL62","PL81", "PL21" )

* Additional drops - extension to PT
drop if inlist(nuts_id, "EL30", "FRGO", "FRF1", "FR10")

* Dropping Austria because of Climate ticket

drop if inlist(nuts_id, "AT12", "AT22", "AT21", "AT31", "AT32", "AT33", "AT34")

* Dropping Madiera

drop if inlist(nuts_id, "PT30") // Madiera

* Dropping Canary islands
drop if inlist(nuts_id, "ES70") // Canary Islands

* Year restriction 
drop if year==2020 & month > 2
drop if year==2021

*------------------------------------
drop if missing(mean_prec)


*ES-----------------------------------
*manually adjust
reghdfe ln_no2_avg mean_prec lreal_diesel lreal_super if nuts_id!="LU00", absorb(nuts_id mdate) vce(cluster nuts_id)

*Storing betas
gen bprec = _b[mean_prec]
gen bdiesel = _b[lreal_diesel]
gen bsuper =_b[lreal_super]

*gen yadj
gen yadj = ln_no2_avg - bprec*mean_prec  - bdiesel*lreal_diesel - bsuper*lreal_super

sdid yadj nuts_id mdate treatment,  vce(placebo) graph g1on reps(500) seed(130423)

*start ES
*-------------------------------------
egen m=min(mdate) if treatment==1, by(nuts_id) //indicator for the year of adoption

egen mm=mean(m), by(nuts_id)
keep if mm==720 | mm==. //keep only one time of adoption

*sdid with yadj
*sdid yadj nuts_id mdate treatment, vce(noinference) graph g1on graph_export(groups, .pdf)

matrix lambda = e(lambda)[1..22,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..22,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..22,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..48,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)


*drop treated unit
drop if nuts_id=="LU00"

*reset ids
drop id
egen id=group(nuts_id)

*get max id
qui sum(id)
scalar max_id = r(max)
display max_id

set seed 130423
*bootstrap w placebo inference
local b = 1
local B = 500 // num of reps
while `b' <= `B' {
preserve

*randomly pick placebo units
local placebo_unit = runiformint(1,max_id)

*assign placebo treatments
drop treatment
gen treatment = (mdate>719 & id==`placebo_unit')
gen treated = (id==`placebo_unit')

*run sdid
reghdfe ln_no2_avg mean_prec lreal_diesel lreal_super if id!=`placebo_unit', absorb(nuts_id mdate) vce(cluster nuts_id)

*drop yadj
drop yadj


*drop old betas
drop bprec
drop bdiesel
drop bsuper

*Storing beta
gen bprec = _b[mean_prec]
gen bdiesel = _b[lreal_diesel]
gen bsuper =_b[lreal_super]

*gen yadj
gen yadj = ln_no2_avg - bprec*mean_prec  - bdiesel*lreal_diesel - bsuper*lreal_super



*sdid with yadj
qui sdid yadj nuts_id mdate treatment, vce(noinference) graph 



*collect quantities from above
matrix lambda_b = e(lambda)[1..22,1] //save lambda weight (define 1..4 as number of pre-treatment periods)
matrix yco_b = e(series)[1..22,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..22,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(48,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 values
matrix d`b' = e(difference)[1..48,2] - meanpre_b // (define 1..X as number of periods)

*update loop
local ++b

restore
}

preserve
keep time d
keep if time!=.

local B = 500
forvalues b = 1/`B' {
svmat d`b'
}

egen rsd = rowsd(d11-d5001) //calculate standard deviation of this difference (specifiy d11-dXX1, where XX is num. of reps)
gen LCI = d + invnormal(0.025)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.975)*rsd //upper bounds on bootstrap CIs
*generate plot
tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-monthly") xtitle("") ytitle("no2") xlab(698(1)767, angle(45)) xline(720, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))


save "$final/es_with_covariates_urban_no2.dta", replace
restore







