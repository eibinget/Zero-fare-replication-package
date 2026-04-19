/* ************************************************************************* * 
*   Creator: Tobias Eibinger and Sachintha Fernando
*   Project: Zero Fare
*   Leave one out analysis and dataset for plotting
*   Date: 2026-04-05
** ************************************************************************* */


clear
use "$final/sdid-dataset-urban", replace


reghdfe lco2cap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel   cb_inflow75   intact if ccode!="LU", absorb(id year) vce(cluster id)


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

gen yadj = lco2cap -  bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual   - bcb_inflow75*cb_inflow75   - bintact*intact- bsome*lsome - bidx*stringIndex - bcases*acases - bdrep*lrel_diesel

sa "$final/sdid-leaeve-one-out-loop.dta", replace



*-------------------------------------------------------
* Regions
*-------------------------------------------------------

egen id2 = group(NUTS2code) //dropping every region once
** ************************************************************************* */

quietly summarize id2
scalar max_id = r(max)-1
display "Max ID value is: " max_id


local B = max_id
local b = 1 
matrix Results = J(`B', 2, .)


while `b' <= `B' {
	
    preserve
   
	use "$final/sdid-leaeve-one-out-loop.dta", clear

** ************************************************************************* */

	egen id2 = group(NUTS2code) // region
** ************************************************************************* */

    *Pick one placebo unit depending on counter
    local placebo_unit `b'
    
    *Drop the placebo unit if ccode != "LU"
    drop if id2 == `placebo_unit' & ccode != "LU"
    
    *run twfe and exclude placebo unit
reghdfe lco2cap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel   cb_inflow75   intact  if ccode!="LU", absorb(id year) vce(cluster id)


drop yadj
drop bcases
drop bgdp
drop bload
drop bdiesel
drop bsuper
drop bei
drop busual
drop bdrep
drop bsome
drop bidx
drop bcb_inflow75
drop bintact

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

gen yadj = lco2cap -  bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual   - bcb_inflow75*cb_inflow75   - bintact*intact- bsome*lsome - bidx*stringIndex - bcases*acases - bdrep*lrel_diesel 

drop if year==2020

*Update treatment indicators after filtering
replace treatment=1 if year>2019 & ccode=="LU"
egen m=min(year) if treatment==1, by(NUTS2code)
egen mm=mean(m), by(NUTS2code)
keep if mm==2021 | mm==.

*run sdid
qui sdid yadj NUTS2code year treatment, vce(noinference) graph

    *Store
    local current_att = e(ATT)
    
    matrix Results[`b', 1] = `placebo_unit'
    matrix Results[`b', 2] = `current_att'
    
    restore
   
    local b = `b' + 1
}

svmat Results, name(col)
rename c1 placebo_unit
rename c2 att


list placebo_unit att in 1/50
summarize att

*kernel density plot
kdensity att, title("Density of Placebo ATTs")

preserve 

** ************************************************************************* */

save "$final/leave-one-out-att.dta", replace       // region
** ************************************************************************* */

restore






*-------------------------------------------------------
* Countries
*-------------------------------------------------------

clear
use "$final/sdid-dataset-urban", replace

*-------------------------------------------------------

egen id2 = group(ccode)   // dropping every country once
** ************************************************************************* */

quietly summarize id2
scalar max_id = r(max)-1
display "Max ID value is: " max_id


local B = max_id
local b = 1 
matrix Results = J(`B', 2, .)


while `b' <= `B' {
	
    preserve
   
	use "$final/sdid-leaeve-one-out-loop.dta", clear

** ************************************************************************* */
	egen id2 = group(ccode)    // country
** ************************************************************************* */

    *Pick one placebo unit depending on counter
    local placebo_unit `b'
    
    *Drop the placebo unit if ccode != "LU"
    drop if id2 == `placebo_unit' & ccode != "LU"
    
    *run twfe and exclude placebo unit
reghdfe lco2cap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel   cb_inflow75   intact  if ccode!="LU", absorb(id year) vce(cluster id)


drop yadj
drop bcases
drop bgdp
drop bload
drop bdiesel
drop bsuper
drop bei
drop busual
drop bdrep
drop bsome
drop bidx
drop bcb_inflow75
drop bintact

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

gen yadj = lco2cap -  bdiesel*ldiesel_real - bsuper*lsuper_real - bei*ei - bgdp*lgdp - bload*lload - busual*lusual   - bcb_inflow75*cb_inflow75   - bintact*intact- bsome*lsome - bidx*stringIndex - bcases*acases - bdrep*lrel_diesel 

drop if year==2020

*Update treatment indicators after filtering
replace treatment=1 if year>2019 & ccode=="LU"
egen m=min(year) if treatment==1, by(NUTS2code)
egen mm=mean(m), by(NUTS2code)
keep if mm==2021 | mm==.

*run sdid
qui sdid yadj NUTS2code year treatment, vce(noinference) graph

    *Store
    local current_att = e(ATT)
    
    matrix Results[`b', 1] = `placebo_unit'
    matrix Results[`b', 2] = `current_att'
    
    restore
   
    local b = `b' + 1
}

svmat Results, name(col)
rename c1 placebo_unit
rename c2 att


list placebo_unit att in 1/50
summarize att

*kernel density plot
kdensity att, title("Density of Placebo ATTs")

preserve 

** ************************************************************************* */

save "$final/leave-one-out-country.dta", replace  // country
** ************************************************************************* */

restore

