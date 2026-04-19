/* ************************************************************************* * 
*   Creator: Tobias Eibinger and Sachintha Fernando
*   Project: Zero Fare
*   Master file for emission analysis
*   Date: 2026-04-05
** ************************************************************************* */ 

* GLOBALS
global PATH         "`c(pwd)'"

global raw 	    	"${PATH}/10_data_raw"
global final 	    "${PATH}/30_data_analysis"
global analysisD    "${PATH}/50_code_analysis_descriptives"
global analysisE    "${PATH}/51_code_analysis_emissions"
global analysisT    "${PATH}/52_code_analysis_traffic"
global analysisA    "${PATH}/53_code_analysis_airquality"
global results  	"${PATH}/60_results"



* Emissions
*---------------------------------------------------------
do $analysisE/5110-sdid-C02.do
do $analysisE/5111-sdid-GHG.do
do $analysisE/5112-sdid-NOX.do
do $analysisE/5120-intime-placebo.do
do $analysisE/5121-othersectors.do
do $analysisE/5122-yadj-time-series-comparison.do
do $analysisE/5123-creating-emission-es-dataset.do
do $analysisE/5124-creating-leave-one-out-dataset.do
do $analysisE/Table-D.1.do

*Traffic
*---------------------------------------------------------
do $analysisT/5210-traffic_vol_es.do
do $analysisT/5211-traffic-nuts2-percap-data-for-es.do

*Air-quality
*---------------------------------------------------------
do $analysisA/5310-sdid.do
do $analysisA/5311-es-for-plotting.do



