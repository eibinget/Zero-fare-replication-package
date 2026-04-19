/* ************************************************************************* * 
*   Creator: Tobias Eibinger and Sachintha Fernando
*   Project: Zero Fare
*   Creating the dataset to plot the air quality ES
*   Date: 2026-04-05
** ************************************************************************* */ 

*Spec 1 - All- no covariates

use "$final/es_no_covariates_all_no2.dta", clear
rename d d1
rename LCI LCI_1
rename UCI UCI_1

keep time UCI_1 LCI_1 d1
drop if time==.

preserve 

*Spec 2 - ALl with covariates

use "$final/es_with_covariates_all_no2.dta", clear
rename d d2
rename LCI LCI_2
rename UCI UCI_2

keep time UCI_2 LCI_2 d2
drop if time==.

save "$final/temp.dta", replace
restore
merge 1:1 time using "$final/temp.dta"
drop _merge

preserve 
*Spec 3 - Urban no covariates
use "$final/es_no_covariates_urban_no2.dta", clear
rename d d3
rename LCI LCI_3
rename UCI UCI_3

keep time UCI_3 LCI_3 d3
drop if time==.

save "$final/temp.dta", replace
restore
merge 1:1 time using "$final/temp.dta"
drop _merge


preserve 
*Spec 4 - Urban with covariates
use "$final/es_with_covariates_urban_no2.dta", replace
rename d d4
rename LCI LCI_4
rename UCI UCI_4

keep time UCI_4 LCI_4 d4
drop if time==.

save "$final/temp.dta", replace
restore
merge 1:1 time using "$final/temp.dta"
drop _merge

save "$final/NO2_att_and_CI_placebo.dta", replace

erase "$final/temp.dta"

