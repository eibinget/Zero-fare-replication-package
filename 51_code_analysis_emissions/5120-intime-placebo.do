/* ************************************************************************* * 
*   Creator: Tobias Eibinger and Sachintha Fernando
*   Project: Zero Fare
*   In time placebo. Intervention set to 2018, 2019 is the post treatment year
*   Date: 2026-04-05
** ************************************************************************* */ 

*Percapita CO2 
**********************************************
clear
use "$final/sdid-dataset-urban", replace

drop if year > 2019
drop treatment 

gen treatment=0
replace treatment = 1 if year ==2019 & NUTS2code=="LU00"

save "$final/dataset-urban_intime_placebo", replace

reghdfe lco2cap lgdp lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75  intact if ccode!="LU", absorb(id year) vce(cluster id)

*Storing betas
gen bgdp = _b[lgdp]
gen bload = _b[lload]
gen bdiesel = _b[ldiesel_real]
gen bsuper = _b[lsuper_real]
gen bei = _b[ei]
gen busual = _b[lusual]
gen bdrep = _b[lrel_diesel]
gen bsome = _b[lsome]
gen bcb_inflow75 = _b[cb_inflow75]
gen bintact = _b[intact]

*gen yadj
gen yadj = lco2cap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome  -  bdrep*lrel_diesel 

*start ES
*-------------------------------------

* run ES
replace treatment=1 if year>2018 & ccode=="LU"
egen m=min(year) if treatment==1, by(NUTS2code) //indicator for the year of adoption

egen mm=mean(m), by(NUTS2code)
keep if mm==2019 | mm==. //keep only one time of adoption


sdid yadj NUTS2code year treatment, vce(placebo) graph g1on reps(500) seed(130423)

*calculate equation 9 in Clarke et al. (2024)
matrix lambda = e(lambda)[1..3,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..3,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..3,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..4,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)

*check differences in yearly point estimates and baseline
matlist e(difference)
di meanpre_o

*drop treated unit
drop if ccode=="LU"

*reset ids
drop id
egen id=group(NUTS2code)

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
* Reloading data
use "$final/dataset-urban_intime_placebo.dta", clear

*Prepare for SDID
drop if ccode=="LU"
drop id
egen id=group(NUTS2code)


*randomly pick placebo units
local placebo_unit = runiformint(1,max_id)
*assign placebo treatments
drop treatment
gen treatment = (year>2018 & id==`placebo_unit')
gen treated = (id==`placebo_unit')

*run twfe and exclude placebo unit
reghdfe lco2cap lgdp lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact  if id!=`placebo_unit', absorb(id year) vce(cluster id)



*Storing betas
gen bgdp = _b[lgdp]
gen bload = _b[lload]
gen bdiesel = _b[ldiesel_real]
gen bsuper = _b[lsuper_real]
gen bei = _b[ei]
gen busual = _b[lusual]
gen bsome = _b[lsome]
gen bdrep = _b[lrel_diesel]
gen bcb_inflow75 = _b[cb_inflow75]
gen bintact = _b[intact]

*gen yadj
gen yadj = lco2cap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome  - bdrep*lrel_diesel 

*Update treatment indicators after filtering
replace treatment=1 if year>2018 & id==`placebo_unit'
egen m=min(year) if treatment==1, by(NUTS2code)
egen mm=mean(m), by(NUTS2code)
keep if mm==2019 | mm==.

*run sdid
qui sdid yadj NUTS2code year treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..3,1] //save lambda weight (define 1..4 as number of pre-treatment periods)
matrix yco_b = e(series)[1..3,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..3,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(4,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 in Clarke et al. (2024) values
matrix d`b' = e(difference)[1..4,2] - meanpre_b // (define 1..X as number of periods)

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

tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Annual") xtitle("") ytitle("lco2cap") xlab(2016(1)2019, angle(45)) xline(2019, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))

save "$final/intimeplacebo_es_co2.dta", replace
restore



************************************************************************************
*NOX
*----------------------------------------------

clear
use "$final/dataset-urban_intime_placebo", replace

reghdfe lnoxcap lgdp lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75   intact if ccode!="LU", absorb(id year) vce(cluster id)


*Storing betas
gen bgdp = _b[lgdp]
gen bload = _b[lload]
gen bdiesel = _b[ldiesel_real]
gen bsuper = _b[lsuper_real]
gen bei = _b[ei]
gen busual = _b[lusual]
gen bdrep = _b[lrel_diesel]
gen bsome = _b[lsome]
gen bcb_inflow75 = _b[cb_inflow75]
gen bintact = _b[intact]

*gen yadj
gen yadj = lnoxcap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome  -  bdrep*lrel_diesel 



*start ES
*-------------------------------------

* run ES
replace treatment=1 if year>2018 & ccode=="LU"
egen m=min(year) if treatment==1, by(NUTS2code) //indicator for the year of adoption

egen mm=mean(m), by(NUTS2code)
keep if mm==2019 | mm==. //keep only one time of adoption

sdid yadj NUTS2code year treatment, vce(placebo) graph g1on reps(500) seed(130423)


*calculate equation 9 in Clarke et al. (2024)
matrix lambda = e(lambda)[1..3,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..3,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..3,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..4,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)

*check differences in yearly point estimates and baseline
matlist e(difference)
di meanpre_o

*drop treated unit
drop if ccode=="LU"

*reset ids
drop id
egen id=group(NUTS2code)

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
* Reloading data
use "$final/dataset-urban_intime_placebo.dta", clear

*Prepare for SDID
drop if ccode=="LU"
drop id
egen id=group(NUTS2code)


*randomly pick placebo units
local placebo_unit = runiformint(1,max_id)
*assign placebo treatments
drop treatment
gen treatment = (year>2018 & id==`placebo_unit')
gen treated = (id==`placebo_unit')

*run twfe and exclude placebo unit
reghdfe lnoxcap lgdp lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact  if id!=`placebo_unit', absorb(id year) vce(cluster id)

*Storing betas
gen bgdp = _b[lgdp]
gen bload = _b[lload]
gen bdiesel = _b[ldiesel_real]
gen bsuper = _b[lsuper_real]
gen bei = _b[ei]
gen busual = _b[lusual]
gen bsome = _b[lsome]
gen bdrep = _b[lrel_diesel]
gen bcb_inflow75 = _b[cb_inflow75]
gen bintact = _b[intact]

*gen yadj
gen yadj = lnoxcap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome  - bdrep*lrel_diesel 

*Update treatment indicators after filtering
replace treatment=1 if year>2018 & id==`placebo_unit'
egen m=min(year) if treatment==1, by(NUTS2code)
egen mm=mean(m), by(NUTS2code)
keep if mm==2019 | mm==.

*run sdid
qui sdid yadj NUTS2code year treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..3,1] //save lambda weight (define 1..4 as number of pre-treatment periods)
matrix yco_b = e(series)[1..3,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..3,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(4,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 in Clarke et al. (2024) values
matrix d`b' = e(difference)[1..4,2] - meanpre_b // (define 1..X as number of periods)

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

tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Annual") xtitle("") ytitle("lnoxcap") xlab(2016(1)2019, angle(45)) xline(2019, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))

save "$final/intimeplacebo_es_nox.dta", replace
restore



************************************************************************************
*GHG
*----------------------------------------------
clear
use "$final/dataset-urban_intime_placebo", replace

reghdfe lghgcap lgdp lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75   intact if ccode!="LU", absorb(id year) vce(cluster id)

*Storing betas
gen bgdp = _b[lgdp]
gen bload = _b[lload]
gen bdiesel = _b[ldiesel_real]
gen bsuper = _b[lsuper_real]
gen bei = _b[ei]
gen busual = _b[lusual]
gen bdrep = _b[lrel_diesel]
gen bsome = _b[lsome]
gen bcb_inflow75 = _b[cb_inflow75]
gen bintact = _b[intact]

*gen yadj
gen yadj = lghgcap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome  -  bdrep*lrel_diesel 

*start ES
*-------------------------------------

* run ES
replace treatment=1 if year>2018 & ccode=="LU"
egen m=min(year) if treatment==1, by(NUTS2code) //indicator for the year of adoption

egen mm=mean(m), by(NUTS2code)
keep if mm==2019 | mm==. //keep only one time of adoption

sdid yadj NUTS2code year treatment, vce(placebo) graph g1on reps(500) seed(130423)

*calculate equation 9 in Clarke et al. (2024)
matrix lambda = e(lambda)[1..3,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..3,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..3,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..4,1..2] // Store Ytr-Yco (define 1..X as number of periods)
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)

*check differences in yearly point estimates and baseline
matlist e(difference)
di meanpre_o

*drop treated unit
drop if ccode=="LU"

*reset ids
drop id
egen id=group(NUTS2code)

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
* Reloading data
use "$final/dataset-urban_intime_placebo.dta", clear

*Prepare for SDID
drop if ccode=="LU"
drop id
egen id=group(NUTS2code)


*randomly pick placebo units
local placebo_unit = runiformint(1,max_id)
*assign placebo treatments
drop treatment
gen treatment = (year>2018 & id==`placebo_unit')
gen treated = (id==`placebo_unit')

*run twfe and exclude placebo unit
reghdfe lghgcap lgdp lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact  if id!=`placebo_unit', absorb(id year) vce(cluster id)

*Storing betas
gen bgdp = _b[lgdp]
gen bload = _b[lload]
gen bdiesel = _b[ldiesel_real]
gen bsuper = _b[lsuper_real]
gen bei = _b[ei]
gen busual = _b[lusual]
gen bsome = _b[lsome]
gen bdrep = _b[lrel_diesel]
gen bcb_inflow75 = _b[cb_inflow75]
gen bintact = _b[intact]

*gen yadj
gen yadj = lghgcap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome  - bdrep*lrel_diesel 

*Update treatment indicators after filtering
replace treatment=1 if year>2018 & id==`placebo_unit'
egen m=min(year) if treatment==1, by(NUTS2code)
egen mm=mean(m), by(NUTS2code)
keep if mm==2019 | mm==.

*run sdid
qui sdid yadj NUTS2code year treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..3,1] //save lambda weight (define 1..4 as number of pre-treatment periods)
matrix yco_b = e(series)[1..3,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..3,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(4,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 in Clarke et al. (2024) values
matrix d`b' = e(difference)[1..4,2] - meanpre_b // (define 1..X as number of periods)

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

tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Annual") xtitle("") ytitle("lghgcap") xlab(2016(1)2019, angle(45)) xline(2019, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))

save "$final/intimeplacebo_es_ghg.dta", replace
restore




*-----------------------------------------------------------------------------------
* Creating a dataset to create the event study graphs for the in time placebo
**CO2
use "$final/intimeplacebo_es_co2.dta", clear
rename d d1
rename LCI LCI_1
rename UCI UCI_1

keep time UCI_1 LCI_1 d1

preserve 
**Nox
use "$final/intimeplacebo_es_nox.dta", clear
rename d d2
rename LCI LCI_2
rename UCI UCI_2

keep time UCI_2 LCI_2 d2

save "$final/temp.dta", replace
restore
merge 1:1 time using "$final/temp.dta"
drop _merge

preserve 
**GHG
**# Bookmark #1
use "$final/intimeplacebo_es_ghg.dta", clear
rename d d3
rename LCI LCI_3
rename UCI UCI_3

keep time UCI_3 LCI_3 d3

save "$final/temp.dta", replace
restore
merge 1:1 time using "$final/temp.dta"
drop _merge


preserve 
keep d1 LCI_1 UCI_1 d2 LCI_2 UCI_2 d3 LCI_3 UCI_3
save "$final/intimeplacebo_es_att_and_CI.dta", replace
restore

erase "$final/intimeplacebo_es_ghg.dta"
erase "$final/intimeplacebo_es_co2.dta"
erase "$final/intimeplacebo_es_nox.dta"
erase "$final/temp.dta"


