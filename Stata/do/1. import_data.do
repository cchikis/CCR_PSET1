* Some Stata Setup

/******************************************************************************

This do file imports and cleans datasets


******************************************************************************/
global wdir ".../Stata_"
global data "$wdir/data"
global in "$wdir/in"
global out "$wdir/out"
global temp "$wdir/temp"

*****************
* Gazetteer
*****************
import delimited $data/US_Gazetteer/2022_Gaz_tracts_national.txt, clear
rename geoid home_tract

format home_tract %15.0gc 

tostring home_tract, gen(TRACT)  
keep if usps=="IL" 

save "$in/2022_Gaz_tracts_national.dta", replace 


*****************
* LTDB cook county
*****************
use "$data/LTDB/LTDB_Orig_2010_fullcount.dta", clear
keep if county == "Cook County" & state =="IL" 
keep tract tractid pop10 nhwht10 nhblk10 vac10 own10 ohu10 hu10 rent10 
ren tractid home_tract
format home_tract %15.0gc
gen home_ID=_n
save "$in/LTDB_Cookcounty.dta", replace

*****************
* Merge them
*****************
use "$in/2022_Gaz_tracts_national.dta",  clear
merge 1:1 home_tract using "$in/LTDB_Cookcounty"
keep if _merge ==3 
drop _merge

gen work_ID = home_ID
drop tract 

ren home_tract tract

save "$in/2022_Gaz_tracts_national_Cook.dta",  replace

*****************
* Tracts Chicago
*****************

import delimited using "$data/Chicago_Data_Portal/CensusTractsTIGER2010", clear 
format geoid10 %15.0gc
ren geoid10 tract
keep tract tractce10 commarea 
gen home_ID=_n
save "$in/tract_IDs", replace

*****************
* Merge them
*****************
use "$data/US_Gazetteer/2022_Gaz_tracts_national.dta",  clear
ren home_tract tract
merge 1:1 tract using "$in/tract_IDs"
keep if _merge ==3 
drop _merge

gen work_ID = home_ID

save "$in/2022_Gaz_tracts_national_Chicago.dta",  replace



*****************
* Crosswalk
*****************

/*
use "$data/crosswalks/ZIP_TRACT_122021.xlsx",  clear
destring zip, gen(ZipCode)
save "$data/in/ZIP_TRACT_122021",  replace
*/

import excel using "$data/crosswalks/ZIP_TRACT_122019.xlsx", firstrow clear
rename *, lower
destring zip, gen(ZipCode)
destring tract, replace
format tract %15.0gc

merge m:1 tract using "$in/2022_Gaz_tracts_national_Cook.dta", keepusing(home_ID work_ID)
keep if _merge==3
drop _merge


save "$in/ZIP_TRACT_122019_Cook",  replace

