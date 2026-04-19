
/* ************************************************************************* * 
*   Creator: Tobias Eibinger and Sachintha Fernando
*   Project: Zero Fare
*  Description: Running the analysis at NUTS2level w placebo inference - GHG
*   Date: 2026-04-05
** ************************************************************************* */

*Percapita GHG Emissions - Main Specification
********************************************************************************
clear
use "$final/sdid-dataset-urban"

*manually adjust
reghdfe lghgcap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact if ccode!="LU", absorb(id year) vce(cluster id)

*Storing betas
gen bcases = _b[acases]
gen bgdp = _b[lgdp]
gen bload = _b[lload]
gen bdiesel = _b[ldiesel_real]
gen bsuper = _b[lsuper_real]
gen bei = _b[ei]
gen busual = _b[lusual]
gen bdrep = _b[lrel_diesel]
gen bsome = _b[lsome]
gen bidx = _b[stringIndex]
gen bcb_inflow75 = _b[cb_inflow75]
gen bintact = _b[intact]


*gen yadj
gen yadj = lghgcap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome - bidx*stringIndex - bcases*acases - bdrep*lrel_diesel 

drop if year==2020

*-------------------------
* Results - ATT 
*-------------------------
sdid yadj NUTS2code year treatment, vce(placebo) graph g1on reps(500) seed(130423)


*-------------------------
* Results - Event study
*-------------------------

*start ES
*-------------------------------------

* run ES
replace treatment=1 if year>2019 & ccode=="LU"
egen m=min(year) if treatment==1, by(NUTS2code) //indicator for the year of adoption

egen mm=mean(m), by(NUTS2code)
keep if mm==2021 | mm==. //keep only one time of adoption

sdid yadj NUTS2code year treatment, vce(noinference) graph g1on 



*calculate equation 9 in Clarke et al. (2024)
matrix lambda = e(lambda)[1..4,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..4,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..4,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..6,1..2] // Store Ytr-Yco (define 1..X as number of periods)
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
use "$final/sdid-dataset-urban", clear

*Prepare for SDID
drop if ccode=="LU"
drop id
egen id=group(NUTS2code)


*randomly pick placebo units
local placebo_unit = runiformint(1,max_id)
*assign placebo treatments
drop treatment
gen treatment = (year>2019 & id==`placebo_unit')
gen treated = (id==`placebo_unit')

*run twfe and exclude placebo unit
reghdfe lghgcap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact  if id!=`placebo_unit', absorb(id year) vce(cluster id)

*Storing betas
gen bcases = _b[acases]
gen bgdp = _b[lgdp]
gen bload = _b[lload]
gen bdiesel = _b[ldiesel_real]
gen bsuper = _b[lsuper_real]
gen bei = _b[ei]
gen busual = _b[lusual]
gen bidx = _b[stringIndex]
gen bsome = _b[lsome]

gen bdrep = _b[lrel_diesel]
gen bcb_inflow75 = _b[cb_inflow75]
gen bintact = _b[intact]

*gen yadj
gen yadj = lghgcap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome - bidx*stringIndex - bcases*acases - bdrep*lrel_diesel 

*drop 2020
drop if year==2020


*Update treatment indicators after filtering
replace treatment=1 if year>2019 & id==`placebo_unit'
egen m=min(year) if treatment==1, by(NUTS2code)
egen mm=mean(m), by(NUTS2code)
keep if mm==2021 | mm==.

*run sdid
qui sdid yadj NUTS2code year treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..4,1] //save lambda weight (define 1..4 as number of pre-treatment periods)
matrix yco_b = e(series)[1..4,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..4,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(6,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 values
matrix d`b' = e(difference)[1..6,2] - meanpre_b // (define 1..X as number of periods)

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

tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Annual") xtitle("") ytitle("lghgcap") xlab(2016(1)2023, angle(45)) xline(2019, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))

save "$final/es_ghg_urban.dta", replace
restore



********************************************************************************
*Percapita GHG Emissions -Robustness check 
*Only dropping NUTS2 regions that are fully rural 
********************************************************************************
clear
use "$final/sdid-dataset-rural.dta"

*manually adjust

reghdfe lghgcap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75   intact if ccode!="LU", absorb(id year) vce(cluster id)



*Storing betas
gen bcases = _b[acases]
gen bgdp = _b[lgdp]
gen bload = _b[lload]
gen bdiesel = _b[ldiesel_real]
gen bsuper = _b[lsuper_real]
gen bei = _b[ei]
gen busual = _b[lusual]
gen bdrep = _b[lrel_diesel]
gen bsome = _b[lsome]
gen bidx = _b[stringIndex]
gen bcb_inflow75 = _b[cb_inflow75]
gen bintact = _b[intact]

*gen yadj
gen yadj = lghgcap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome - bidx*stringIndex - bcases*acases - bdrep*lrel_diesel 

drop if year==2020

*-------------------------
* Results - ATT 
*-------------------------
sdid yadj NUTS2code year treatment, vce(placebo) graph g1on reps(500) seed(130423)


*-------------------------
* Results - Event study
*-------------------------

*start ES
*-------------------------------------
* run ES
replace treatment=1 if year>2019 & ccode=="LU"
egen m=min(year) if treatment==1, by(NUTS2code) //indicator for the year of adoption

egen mm=mean(m), by(NUTS2code)
keep if mm==2021 | mm==. //keep only one time of adoption

sdid yadj NUTS2code year treatment, vce(noinference) graph g1on 


*calculate equation 9 in Clarke et al. (2024)
matrix lambda = e(lambda)[1..4,1] //save lambda weight (define 1..X as number of pre-treatment periods)
matrix yco = e(series)[1..4,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr = e(series)[1..4,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..6,1..2] // Store Ytr-Yco (define 1..X as number of periods)
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
use "$final/sdid-dataset-rural.dta", clear

*Prepare for SDID
drop if ccode=="LU"
drop id
egen id=group(NUTS2code)


*randomly pick placebo units
local placebo_unit = runiformint(1,max_id)
*assign placebo treatments
drop treatment
gen treatment = (year>2019 & id==`placebo_unit')
gen treated = (id==`placebo_unit')

*run twfe and exclude placebo unit
reghdfe lghgcap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact  if id!=`placebo_unit', absorb(id year) vce(cluster id)


*Storing betas
gen bcases = _b[acases]
gen bgdp = _b[lgdp]
gen bload = _b[lload]
gen bdiesel = _b[ldiesel_real]
gen bsuper = _b[lsuper_real]
gen bei = _b[ei]
gen busual = _b[lusual]
gen bidx = _b[stringIndex]
gen bsome = _b[lsome]

gen bdrep = _b[lrel_diesel]
gen bcb_inflow75 = _b[cb_inflow75]
gen bintact = _b[intact]

*gen yadj
gen yadj = lghgcap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome - bidx*stringIndex - bcases*acases - bdrep*lrel_diesel 

*drop 2020
drop if year==2020


*Update treatment indicators after filtering
replace treatment=1 if year>2019 & id==`placebo_unit'
egen m=min(year) if treatment==1, by(NUTS2code)
egen mm=mean(m), by(NUTS2code)
keep if mm==2021 | mm==.

*run sdid
qui sdid yadj NUTS2code year treatment, vce(noinference) graph

*collect quantities from above
matrix lambda_b = e(lambda)[1..4,1] //save lambda weight (define 1..4 as number of pre-treatment periods)
matrix yco_b = e(series)[1..4,2] //control baseline (define 1..X as number of pre-treatment periods)
matrix ytr_b = e(series)[1..4,3] //treated baseline (define 1..X as number of pre-treatment periods)
matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(6,1,aux_b[1,1]) // (define 1..X as number of periods)

*collect equation 9 values
matrix d`b' = e(difference)[1..6,2] - meanpre_b // (define 1..X as number of periods)

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

tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Annual") xtitle("") ytitle("lghgcap") xlab(2016(1)2023, angle(45)) xline(2019, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))

save "$final/es_ghg_rural", replace
restore

























