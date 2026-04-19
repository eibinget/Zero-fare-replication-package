

/* ************************************************************************* * 
*   Creator: Tobias Eibinger and Sachintha Fernando
*   Project: Zero Fare
*   SAVE DATA FOR EVENT STUDY GRAPH IN R
*   Date: 2026-04-05
** ************************************************************************* */ 


**Method 1
* no covariates
* all weekdays
use "$final/es_traffic_placebo-nocovs-alldays.dta", clear
rename d d1
rename LCI LCI_1
rename UCI UCI_1

keep time UCI_1 LCI_1 d1
drop if time==.

preserve
*Method 2
* no covariates
* Mo-Fr
use "$final/es_traffic_placebo-nocovs-weekday.dta", clear
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
*Method 3
* no covariates
* Sa-Su
use "$final/es_traffic_placebo-nocovs-weekend.dta", clear
rename d d3
rename LCI LCI_3
rename UCI UCI_3

keep time UCI_3 LCI_3 d3
drop if time==.

save "$final/temp.dta", replace
restore
merge 1:1 time using "$final/temp.dta"
drop _merge

*/
preserve 
*Method 4
* with covariates
* all weekdays
use "$final/es_traffic_placebo-covs-alldays.dta", replace
rename d d4
rename LCI LCI_4
rename UCI UCI_4

keep time UCI_4 LCI_4 d4
drop if time==.

save "$final/temp.dta", replace
restore

merge 1:1 time using "$final/temp.dta"
drop _merge

*
preserve 
*Method 5
* with covariates
* Mo-Fr
use "$final/es_traffic_placebo-covs-weekday.dta", replace
rename d d5
rename LCI LCI_5
rename UCI UCI_5

keep time UCI_5 LCI_5 d5
drop if time==.

save "$final/temp.dta", replace
restore
merge 1:1 time using "$final/temp.dta"
drop _merge

preserve 
*Method 6
* with covariates
* Sa-Su
use "$final/es_traffic_placebo-covs-weekend.dta", replace
rename d d6
rename LCI LCI_6
rename UCI UCI_6

keep time UCI_6 LCI_6 d6
drop if time==.


save "$final/temp.dta", replace
restore


merge 1:1 time using "$final/temp.dta"
drop _merge
*/

erase "$final/temp.dta"


format time %tm
gen year  = yofd(dofm(time))
gen month = month(dofm(time))


save "$final/traffic_att_and_CI_placebo.dta", replace



