
/* ************************************************************************* * 
*   Creator: Tobias Eibinger and Sachintha Fernando
*   Project: Zero Fare
*  Description: Running the analysis at NUTS2level w placebo inference - CO2
*   Date: 2026-04-05
** ************************************************************************* */


*Percapita C02 Emissions - Main Specification
********************************************************************************
clear
use "$final/sdid-dataset-urban"


eststo co2: reghdfe lco2cap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact if ccode!="LU", absorb(id year) vce(cluster id)

eststo nox: reghdfe lnoxcap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact if ccode!="LU", absorb(id year) vce(cluster id)

eststo ghg: reghdfe lghgcap lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact if ccode!="LU", absorb(id year) vce(cluster id)


* Export to LaTeX
esttab co2 nox ghg using "$results/table_results.tex", replace ///
    b(4) se(4) ///                        // 4 decimal places
    star(* 0.10 ** 0.05 *** 0.01) ///    // significance stars
    label ///                             // use variable labels if defined
    booktabs ///                          // \toprule, \midrule etc.
    title("TWFE regression results") ///
    mtitles("CO\$_2\$" "NO\$_x\$" "GHG") ///
    mgroups("(1) CO\$_2\$" "(2) NO\$_x\$" "(3) GHG", ///
        pattern(1 1 1) prefix(\multicolumn{2}{c}{) suffix(}) ///
        span erepeat(\cmidrule(lrel_diesel){@span})) ///
    collabels("Coef." "SE") ///
    keep(lgdp acases stringIndex lsome lusual ei ldiesel_real lsuper_real lload lrel_diesel cb_inflow75 intact) ///
    varlabels(lgdp "log(gdp)" ///
              acases "asinh(cases)" ///
              stringIndex "stringIndex" ///
              lsome "log(some\_wfh)" ///
              lusual "log(usual\_wfh)" ///
              ei "ei" ///
              ldiesel_real "log(diesel)" ///
              lsuper_real "log(petrol)" ///
              lload "log(freight)" ///
              lrel_diesel "log(rel\_diesel)" ///
              cb_inflow75 "cb\_inflow75" ///
              intact "cb\_inflow75:log(rel\_diesel)") ///
    stats(N_clust N, fmt(%9.0f) ///
        labels("N" "Obs")) ///
    nobaselevels nogaps nocons ///
    substitute(\_ _)             // handle underscores in LaTeX


