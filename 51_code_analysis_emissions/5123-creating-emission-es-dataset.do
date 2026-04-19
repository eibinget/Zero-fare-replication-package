/* ************************************************************************* * 
*   Creator: Tobias Eibinger and Sachintha Fernando
*   Project: Zero Fare
*   Creating a dataset to emission event studies
*   Date: 2026-04-05
** ************************************************************************* */ 
*Outcome 1: Percapita CO2
use "$final/es_co2_urban.dta", clear
rename d d1
rename LCI LCI_1
rename UCI UCI_1

keep time UCI_1 LCI_1 d1
drop if missing(time)

preserve 
*Outcome 2: NOX
use "$final/es_nox_urban.dta", clear
rename d d2
rename LCI LCI_2
rename UCI UCI_2

keep time UCI_2 LCI_2 d2
drop if missing(time)

save "$final/temp.dta", replace


restore
merge 1:1 time using "$final/temp.dta"
drop _merge

preserve 
*Outcome 3: GHG
use "$final/es_ghg_urban.dta", clear
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
save "$final/att_and_CI_placebo_urban.dta", replace
restore

*---------------------------------------------
* Rural
*----------------------------------------------

*Outcome 1: CO2
use "$final/es_co2_rural.dta", clear
rename d d1
rename LCI LCI_1
rename UCI UCI_1

keep time UCI_1 LCI_1 d1
drop if missing(time)

preserve 
*Outcome 2: NOX
use "$final/es_nox_rural.dta", clear
rename d d2
rename LCI LCI_2
rename UCI UCI_2

keep time UCI_2 LCI_2 d2
drop if missing(time)

save "$final/temp.dta", replace
restore
merge 1:1 time using "$final/temp.dta"
drop _merge

preserve 
*Outcome 3: GHG
use "$final/es_ghg_rural.dta", clear
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
save "$final/att_and_CI_placebo_rural.dta", replace
restore

erase "$final/temp.dta"

