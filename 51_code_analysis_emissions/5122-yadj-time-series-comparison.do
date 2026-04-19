/* ************************************************************************* * 
*   Creator: Tobias Eibinger and Sachintha Fernando
*   Project: Zero Fare
*   Creating a dataset to plot the differently weighted pre-trends
*   Date: 2026-04-05
** ************************************************************************* */ 

clear
use "$final/sdid-dataset-urban", replace

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



*******************************************
preserve
keep year NUTS2code lco2cap yadj
drop if NUTS2code=="LU00"
collapse (mean) yadj, by(year)
rename yadj con_avg
save "$final/simple_average_all_controls.dta", replace
restore
*********************************************
preserve 
keep year NUTS2code lco2cap yadj
drop if NUTS2code=="LU00"
merge m:1 NUTS2code using "$final/unit_weights_co2.dta"
keep if _merge==3
collapse (mean) yadj, by(year)
rename yadj w_con_avg
tw line w_con_avg year
save "$final/simple_average_all_weighted.dta", replace
restore
********************
preserve
keep year NUTS2code lco2cap yadj
drop if NUTS2code=="LU00"
merge m:1 NUTS2code using "$final/unit_weights_co2.dta"
keep if _merge==3
drop _merge id1
gen yadj_weighted = yadj*omega1
collapse (sum) yadj_weighted, by(year)
tw line yadj_weighted year
save "$final/weighted_average.dta", replace
restore
********************************
preserve 
keep year NUTS2code lco2cap yadj
keep if NUTS2code=="LU00"
keep year lco2cap yadj 
rename yadj lu_yadj
tw line lu_yadj year
save "$final/lux_timetrend.dta", replace
restore 
*****************************

clear 
use "$final/simple_average_all_controls.dta"
merge 1:1 year using "$final/simple_average_all_weighted.dta"
drop _merge
merge 1:1 year using "$final/weighted_average.dta"
drop _merge 
merge 1:1 year using "$final/lux_timetrend.dta"
drop _merge


tw line con_avg year || line w_con_avg year || line yadj_weighted year || line lu_yadj year

save "$final/yadj_timetrends.dta", replace

erase "$final/simple_average_all_controls.dta"
erase "$final/simple_average_all_weighted.dta"
erase "$final/weighted_average.dta"
erase "$final/lux_timetrend.dta"




