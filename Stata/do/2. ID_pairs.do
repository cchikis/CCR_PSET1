
/******************************************************************************

This do file creates a matrix work ID * home ID 

******************************************************************************/

global wdir ".../Stata_"
global data "$wdir/data"
global in "$wdir/in"
global out "$wdir/out"

cd "$data/US_Gazetteer" 

set more off

* Import data 

use  "$in/2022_Gaz_tracts_national_Cook.dta",  replace
rename tract home_tract
merge 1:1 home_tract using "$in/LTDB_Cookcounty"
keep if _merge==3 
drop _merge 
ren home_tract work_tract
merge 1:m work_tract using "$in/list_work_tracts.dta", keepusing(work_tract)
keep if _merge==3 

drop _merge  

keep home_ID work_ID
gen filler = 1

* Reshape to create full set of work-home pairs and merge in latitude/longitudes.

quietly reshape wide filler, i(work_ID) j(home_ID)
quietly reshape long
drop filler
merge m:1 work_ID using "$in/2022_Gaz_tracts_national_Cook.dta", keepusing(intptlat intptlong tract)
ren (intptlat intptlong) (work_lat work_long)
drop if _merge != 3
drop _merge
merge m:1 home_ID using "$in/2022_Gaz_tracts_national_Cook.dta", keepusing(intptlat intptlong)
ren (intptlat intptlong) (home_lat home_long)
drop if _merge != 3
drop _merge

* Create unique work-home observation ID.

sort work_ID home_ID
gen n = _n
gen groupid = 1 if n <= 100000
replace groupid = 2 if n > 100000 & n <= 200000
replace groupid = 3 if n > 200000 & n <= 300000
replace groupid = 4 if n > 300000 & n <= 400000
replace groupid = 5 if n > 400000 & n <= 500000
replace groupid = 6 if n > 500000 & n <= 600000
replace groupid = 7 if n > 600000 & n <= 700000
replace groupid = 8 if n > 700000 & n <= 800000
replace groupid = 9 if n > 800000 & n <= 900000
replace groupid = 10 if n > 900000 & n <= 1000000
replace groupid = 11 if n > 1000000 & n <= 1100000
replace groupid = 12 if n > 1100000 & n <= 1200000
replace groupid = 13 if n > 1200000 & n <= 1300000
replace groupid = 14 if n > 1300000 & n <= 1400000

save "$out/IDMatrix_cook.dta", replace
