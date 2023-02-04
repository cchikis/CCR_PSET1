
/******************************************************************************

This dofile computes upper bound for wages
******************************************************************************/
clear all 
global wdir ".../Stata_"
global data "$wdir/data"
global in "$wdir/in"
global out "$wdir/out"

* Import data 

version 14

import delimited using  "$data/CBP/CBP2019.CB1900CBP-Data", rowrange(2)  varnames(1)    clear


keep if substr(name,1,3)=="ZIP" 
set more off

split name, p(" ")
	keep if name3=="(Chicago,"
	keep name name2 naics2017 empszes empszes_label estab payann payqtr1 emp 
		ren (name name2 estab emp payqtr1 payann) ///
			(desc ZIP Estabs Employees First_Qtr_Payroll_1K Annual_Payroll_1K)
	
	order ZIP, after(desc)
    destring ZIP, g(ZipCode)

	* select only aggregate information (potentially look later at establishment sizes)
    
	keep if naics2017=="00" 
	
    replace Employees = "9.5" if Employees == "a"
	replace Employees = "59.5" if Employees == "b"
	replace Employees = "174.5" if Employees == "c"
	replace Employees = "374.5" if Employees == "e"
	replace Employees = "749.5" if Employees == "f"
	replace Employees = "1749.5" if Employees == "g"
	replace Employees = "3749.5" if Employees == "h"
	replace Employees = "7499.5" if Employees == "i"
	replace Employees = "17499.5" if Employees == "j"
	replace Employees = "37499.5" if Employees == "k"
	replace Employees = "74999.5" if Employees == "l"
	replace Employees = "100000" if Employees == "m"
	drop if First_Qtr_Payroll_1K == "D" | First_Qtr_Payroll_1K == "S" | Annual_Payroll_1K == "D" |    Annual_Payroll_1K == "S"
	keep if empszes_label =="All establishments" 
	destring First_Qtr_Payroll_1K  Annual_Payroll_1K Employees, replace
    joinby ZipCode using "$data/crosswalks/ZIP_TRACT_122021", _merge(_merge)
	drop _merge
	*ren home_ID ID
	*merge m:1 ID using "Stata\Tract Classification.dta"
	*ren ID home_ID
	gen bus_ratio1 = bus_ratio
	* Drop census tracts classified as having missing data and reweight according to
	* remaining percentages.
	* replace BUS_RATIO1 = 0 if Tract_Condition == 2
	egen NEW_RATIO = pc(bus_ratio1), by(ZIP) prop
	
	gen Annual_Payroll = Annual_Payroll_1K * 1000
	foreach var of varlist Employees Annual_Payroll {
		gen `var'_share = `var' * NEW_RATIO
	}
	collapse (sum) Employees_share Annual_Payroll_share, by(tract)
	ren (Employees_share Annual_Payroll_share) (Employees Annual_Payroll)
	format Annual_Payroll %15.0f
	gen Wj = Annual_Payroll / Employees
	gen Wj_monthly = Wj / 12
	egen maxWj_monthly_`i' = max(Wj_monthly)
	* sort home_ID
	tabulate maxWj_monthly_`i'
	display "Copy down above wage for year `i'"
	
	* upper bound for wages 
	