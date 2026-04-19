
/* ************************************************************************* * 
*   Creator: Tobias Eibinger and Sachintha Fernando
*   Project: Zero Fare
*  Description: Running the analysis at NUTS2level w placebo inference - CO2
*   Date: 2026-04-05
** ************************************************************************* */


*Percapita C02 Emissions - Main Specification
********************************************************************************
clear
use "$final/NUTS2-Final-Panel.dta", clear

egen id=group(NUTS2code)
xtset id year

drop if year < 2016

*balanced panel 
drop if missing(lco2cap, acases, lusual, lsome, ei, lgdp, ldiesel_real, lsuper_real, lload, stringIndex)

sort id year
egen obs_count = count(year), by(id)
tabulate obs_count

keep if obs_count==7


*dropping nuts2 regions that do not include a predominantly urban nuts3 region
drop if inlist(NUTS2code, "AT11", "AT21", "AT22", "AT31", "AT32", "BE31", "BE34", "BE35", "BG31")
drop if inlist(NUTS2code, "BG32", "BG33", "BG34", "BG42", "CH05", "CY00", "CZ03", "CZ04", "CZ05")
drop if inlist(NUTS2code, "CZ06", "CZ07", "CZ08", "DE13", "DE14", "DE22", "DE23", "DE24", "DE40")
drop if inlist(NUTS2code, "DE72", "DE73", "DE80", "DE91", "DE93", "DEB1", "DEB2", "DED4", "DEE0")
drop if inlist(NUTS2code, "DEG0", "DK02", "DK03", "DK04", "DK05", "EL41", "EL42", "EL43", "EL51")
drop if inlist(NUTS2code, "EL53", "EL54", "EL61", "EL62", "EL63", "EL64", "EL65", "ES11", "ES12")
drop if inlist(NUTS2code, "ES13", "ES22", "ES23", "ES41", "ES42", "ES43", "ES64", "FI19", "FI1C")
drop if inlist(NUTS2code, "FI1D", "FI20", "FRB0", "FRC1", "FRC2", "FRD1", "FRD2", "FRE2", "FRF1")
drop if inlist(NUTS2code, "FRF2", "FRF3", "FRH0", "FRI2", "FRI3", "FRJ1", "FRK1", "FRM0", "FRY1")
drop if inlist(NUTS2code, "FRY2", "FRY3", "FRY5", "HR02", "HR03", "HR06", "HU12", "HU21", "HU22")
drop if inlist(NUTS2code, "HU23", "HU31", "HU32", "HU33", "IE04", "IE05", "ITC2", "ITF1", "ITF2")
drop if inlist(NUTS2code, "ITF5", "ITF6", "ITH1", "ITH2", "ITI2", "ITI3", "LT02", "NL11")
drop if inlist(NUTS2code, "NL12", "NL13", "NL34", "NO02", "NO06", "NO07", "NO09", "NO0A", "NO0B")
drop if inlist(NUTS2code, "PL42", "PL43", "PL52", "PL62", "PL72", "PL81", "PL82", "PL84", "PL92")
drop if inlist(NUTS2code, "PT15", "PT16", "PT18", "PT20", "RO11", "RO12", "RO21", "RO22", "RO31")
drop if inlist(NUTS2code, "RO41", "RO42", "SE12", "SE21", "SE22", "SE31", "SE32", "SE33", "SI03")
drop if inlist(NUTS2code, "SI04", "SK02", "SK03", "SK04")



*dropping nuts2 regions that only include predominantly rural nuts3 regions
drop if inlist(NUTS2code, "AT11", "BE34", "CZ03", "DE22", "DK05", "EL62", "EL64", "FI1D", "FI20")
drop if inlist(NUTS2code, "FRI2", "FRI3", "FRM0", "IE04", "IE05", "ITF2", "ITH1", "NO02", "NO07")
drop if inlist(NUTS2code, "NO0B", "PL82", "PT18", "SI03")


drop if year==2023 // Edgar 2023 data still estimates


save "$final/sdid-dataset-urban", replace

*manually adjust
reghdfe lco2cap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact if ccode!="LU", absorb(id year) vce(cluster id)

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
gen yadj = lco2cap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome - bidx*stringIndex - bcases*acases - bdrep*lrel_diesel 

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


*Extract time weights
matlist e(lambda)

*Extract control regions
egen id1=group(NUTS2code)

matlist e(omega)


preserve 
clear
matrix omega=e(omega)
svmat omega
rename omega2 id1
save "$final/omega1.dta", replace
restore

merge m:1 id1 using "$final/omega1.dta"

preserve

sort omega1 id1
quietly by omega1 id1:  gen dup = cond(_N==1,0,_n)

keep if dup==1
gsort -omega1

keep NUTS2code id1 omega1
save "$final/unit_weights_co2.dta", replace

restore 
*------------------------
*/

*calculate equation 9 in Clarke et al. (2024).
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
use "$final/sdid-dataset-urban.dta", clear

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
reghdfe lco2cap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact  if id!=`placebo_unit', absorb(id year) vce(cluster id)

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
gen yadj = lco2cap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome - bidx*stringIndex - bcases*acases - bdrep*lrel_diesel 

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

tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Annual") xtitle("") ytitle("lco2cap") xlab(2016(1)2023, angle(45)) xline(2019, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))

save "$final/es_co2_urban.dta", replace
restore


********************************************************************************
*Percapita C02 Emissions -Robustness check 
*Only dropping NUTS2 regions that are fully rural 
********************************************************************************
clear
use "$final/NUTS2-Final-Panel.dta", clear


egen id=group(NUTS2code)
xtset id year
drop if year < 2016

drop if missing(lco2cap, acases, lusual, lsome, ei, lgdp, ldiesel_real, lsuper_real, lload, stringIndex)

sort id year
egen obs_count = count(year), by(id)
tabulate obs_count
keep if obs_count==7


*dropping nuts2 regions that only include predominantly rural nuts3 regions
drop if inlist(NUTS2code, "AT11", "BE34", "CZ03", "DE22", "DK05", "EL62", "EL64", "FI1D", "FI20")
drop if inlist(NUTS2code, "FRI2", "FRI3", "FRM0", "IE04", "IE05", "ITF2", "ITH1", "NO02", "NO07")
drop if inlist(NUTS2code, "NO0B", "PL82", "PT18", "SI03")

drop if year==2023 

save "$final/sdid-dataset-rural.dta", replace

*manually adjust

reghdfe lco2cap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75   intact if ccode!="LU", absorb(id year) vce(cluster id)



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
gen yadj = lco2cap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome - bidx*stringIndex - bcases*acases - bdrep*lrel_diesel 

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


*calculate equation 9 in Clarke et al. (2024).
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
reghdfe lco2cap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact  if id!=`placebo_unit', absorb(id year) vce(cluster id)


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
gen yadj = lco2cap - bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual - bcb_inflow75*cb_inflow75 - bintact*intact - bsome*lsome - bidx*stringIndex - bcases*acases - bdrep*lrel_diesel 

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

tw rarea UCI LCI time, color(gray%30) || scatter d time, color(black) m(d) title("NUTS2-Annual") xtitle("") ytitle("lco2cap") xlab(2016(1)2023, angle(45)) xline(2019, lc(black) lp(solid)) yline(0, lc(black) lp(shortdash)) scheme(sj) legend(off) plotregion(fcolor(white)) graphregion(fcolor(white))

save "$final/es_co2_rural", replace
restore

