
/******************************************************************************

This dofiles merges data at the zoning and at the census tract level with 
data from LODES

******************************************************************************/


global wdir ".../Stata_"
global data "$wdir/data"
global in "$wdir/in"
global out "$wdir/out"
global Fig "$wdir/Figures"
global temp "$wdir/temp"

* Save here the shapefiles/new

cd "$wdir/shapefiles" 

*********************
* Census tract data 
*********************

clear
spshape2dta geo_export_5a3b95b3-fb84-4297-ae9a-a927db040871, replace

*********************
* Zoning data 
*********************

clear
spshape2dta geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690, replace 

version 14
clear 

**************************
* Zoning and census areas  
**************************

use "geo_export_5a3b95b3-fb84-4297-ae9a-a927db040871_shp", replace
fieldarea _X _Y, gen(census_area) id(_ID) unit(sqft)
save "$in/census_tract_area", replace


use "geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp", clear
fieldarea _X _Y, gen(zoning_area) id(_ID) unit(sqft)
save "$in/zoning_area", replace


use "geo_export_5a3b95b3-fb84-4297-ae9a-a927db040871_shp", clear
keep _ID _X _Y 
*drop if _X==. 
save "census_area_test.dta", replace

**************************
* Analysis  
**************************

* create dta containing area of each field

use "geo_export_5a3b95b3-fb84-4297-ae9a-a927db040871.dta", clear 
merge m:1 _ID using "$in/census_tract_area"
drop _merge
save "geo_export_5a3b95b3-fb84-4297-ae9a-a927db040871_area.dta", replace 

* create dta containing area of each field

use "geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690.dta", clear

* Define zoning categories 

cap drop zone_cat
gen zone_cat = 1 if substr(zone_class,1,1)=="B" | substr(zone_class,1,1)=="C" | substr(zone_class,1,1)=="M" | substr(zone_class,1,2)=="DX"| substr(zone_class,1,2)=="DM"| substr(zone_class,1,2)=="DS"| substr(zone_class,1,3)=="PMD"
replace zone_cat=2 if substr(zone_class,1,1)=="R" | substr(zone_class,1,2)=="DR"
replace zone_cat=3 if zone_cat==.
label define l_zone 1 "business" 2 "residential" 3 "others" 
label values zone_cat l_zone 

merge m:1 _ID using "$in/zoning_area"
ren _ID zoning_ID

******** Figure 1: geographic distribution of zoning areas 

spmap zone_cat using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Spectral)  clm(u) ndsize(vvvthin) ocolor(none none none none)
graph export "$Fig/zoning_areas.pdf", replace 

* use geoinpoly to assign to census tracts 

geoinpoly _CY _CX using "census_area_test.dta"
drop if missing(_ID)
merge m:1 _ID using "geo_export_5a3b95b3-fb84-4297-ae9a-a927db040871_area.dta", assert(2 3) keep(3) nogen
gen tract_ID=statefp10 + countyfp10 + tractce10
destring tract_ID, replace

ren tract_ID home_tract
drop _merge
format home_tract %15.0gc

merge m:1 home_tract using "$in/LTDB_Cookcounty"
* ren home_ID ID
drop _merge

******** Figure 2: geographic distribution of zoning areas, grouped by census 

spmap zone_cat using geo_export_5a3b95b3-fb84-4297-ae9a-a927db040871_shp if zoning_ID !=1, id(zoning_ID) fcolor(Spectral) ndsize(vvvthin) clm(u)
graph export "$Fig/zoning_areas_census.pdf", replace 

* keep relevant data 

keep zoning_ID zoning_area census_area _CX _CY shape_area shape_len zone_class zone_type zone_cat _ID geoid10 home_tract census_area  

save "$in/zoning_census_merge", replace

**********************
* LODES data 
**********************

use  "$in/zoning_census_merge", clear
cap drop _merge

* merge with RAC
merge m:1 home_tract using "$out/LODES_rac_tot_jobs"
cap drop _merge

* merge with WAC 
cap drop work_tract
ren home_tract work_tract
merge m:1 work_tract using "$out/LODES_wac_tot_jobs"
keep if _merge==3
rename work_tract home_tract

*********** Figure 3 and A1: geographic distribution high income  

format share_highwage_rac %12.2f
spmap share_highwage_rac  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Share") legend(size(big))
graph export "$Fig/share_high_wage_residents.png", replace


format share_highwage_wac %12.2f
spmap share_highwage_wac  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Share")  legend(size(big))
graph export "$Fig/share_high_wage_workers.png", replace

foreach var of varlist tot_resid_high_wage tot_resid_low_wage tot_job_high_wage tot_job_low_wage {
	winsor2 `var', cuts(1 99)  

}
format tot_resid_high_wage_w %12.0f
spmap tot_resid_high_wage_w  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Number jobs")  legend(size(big))
graph export "$Fig/number_high_wage_residents.png", replace


format tot_resid_low_wage_w %12.0f
spmap tot_resid_low_wage_w  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(0.0000001pt) ocolor(none none none none none) legtitle("Number jobs") legend(size(big))
graph export "$Fig/number_low_wage_residents.png", replace

format tot_job_high_wage_w %12.0f
spmap tot_job_high_wage_w  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Number jobs") legend(size(big))
graph export "$Fig/number_high_wage_workers.png", replace


format tot_job_low_wage_w %12.0f
spmap tot_resid_low_wage_w  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(0.0000001pt) ocolor(none none none none none) legtitle("Number jobs") legend(size(big))
graph export "$Fig/number_low_wage_workers.png", replace



*********** Figure 4 and A2: geographic distribution education

format share_skilled_rac %12.2f
spmap share_skilled_rac  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Share") legend(size(big))
graph export "$Fig/share_skilled_residents.png", replace

format share_skilled_wac %12.2f
spmap share_skilled_wac  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Share") legend(size(big))
graph export "$Fig/share_skilled_workers.png", replace


format share_unskilled_rac %12.2f
spmap share_unskilled_rac  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds2)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Share") legend(size(big))
graph export "$Fig/share_unskilled_residents.png", replace

format share_unskilled_wac %12.2f
spmap share_unskilled_wac  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Share") legend(size(big))
graph export "$Fig/share_unskilled_workers.png", replace


foreach var of varlist tot_resid_high tot_job_high tot_resid_low tot_job_low {
	winsor2 `var', cuts(1 99) 

}

format tot_resid_high %12.0f
spmap tot_resid_high using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Number jobs") legend(size(big))
graph export "$Fig/number_skilled_residents.png", replace


format tot_job_high %12.0f
spmap tot_job_high using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Number jobs") legend(size(big))
graph export "$Fig/number_skilled_workers.png", replace

format tot_resid_low %12.0f
spmap tot_resid_low  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Number jobs") legend(size(big))
graph export "$Fig/number_unskilled_residents.png", replace


format tot_job_low %12.0f
spmap tot_job_low using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(0.0000001pt) ocolor(none none none none none) legtitle("Number jobs") legend(size(big))
graph export "$Fig/number_unskilled_workers.png", replace

save "$in/lodes_zoning_merged", replace

*********** Figure 5 and A3: geographic distribution race 

format share_white_rac %12.2f
spmap share_white_rac  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Share") legend(size(big))
graph export "$Fig/share_white_residents.png", replace

format share_black_rac %12.2f
spmap share_black_rac  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Share")  legend(size(big))
graph export "$Fig/share_black_residents.png", replace



format share_white_wac %12.2f
spmap share_white_wac  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Share") legend(size(big))
graph export "$Fig/share_white_workers.png", replace

format share_black_wac %12.2f
spmap share_black_wac  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Share") legend(size(big))
graph export "$Fig/share_black_workers.png", replace


foreach var of varlist tot_resid_white tot_job_white tot_resid_black tot_job_black {
	winsor2 `var', cuts(1 99) 

}

format tot_resid_white_w %12.0f
spmap tot_resid_white_w using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Number jobs") legend(size(big))
graph export "$Fig/number_white_residents.png", replace


format tot_resid_black_w %12.0f
spmap tot_resid_black_w  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(0.0000001pt) ocolor(none none none none none) legtitle("Number jobs") legend(size(big))
graph export "$Fig/number_black_residents.png", replace

format tot_job_white_w %12.0f
spmap tot_job_white_w  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(vvvthin) ocolor(none none none none none) legtitle("Number jobs") legend(size(big))
graph export "$Fig/number_white_workers.png", replace


format tot_job_black_w %12.0f
spmap tot_job_black_w  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)   cln(5) clm( q) ndsize(0.0000001pt) ocolor(none none none none none) legtitle("Number jobs") legend(size(big))
graph export "$Fig/number_black_workers.png", replace


******************************
* Merge with opportunity areas
******************************

import delimited using "$data/Chicago_Data_Portal/opportunity_zones_true.csv", clear 
ren v2 tract 
format tract %15.0gc
keep tract 

save "$in/opportunity_areas", replace


* Import estimated tau

import delimited using "$wdir/R_to_csv/eq_wages.csv", clear 

ren w_geoid tract 

format tract %15.0gc


keep tract tau_i_baseline 
duplicates drop

save "$in/tract_tau_R", replace 

* Import estimated productivity 

import delimited using "$wdir/R_to_csv/W_GEOID_order.csv", clear 
gen n=_n
ren v1 tract 
format tract %15.0gc
save "$in/order_W_geoid", replace 

import delimited using "$wdir/R_to_csv/A_i_vec.csv", clear 
gen n=_n 
ren v1 Aj
merge 1:1 n using "$in/order_W_geoid"

sum Aj
gen Aj_norm= (Aj-27723.27 )/(10118.94)

drop _merge
save "$in/A_i", replace 


* Merge with estimated tau 

use "$in/lodes_zoning_merged", replace
drop if home_tract==.
rename home_tract tract 
drop _merge 
merge m:1 tract using "$in/tract_tau_R"
drop _merge 

gen wedge=1+tau_i_baseline


************ Figure 6: geographic breakdown of wedge

format wedge %12.2f
spmap wedge  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)  cln(6) clm(q) ndsize(vvvthin) ocolor(none none none none none none none none none none none) legtitle("wedge")  legend(size(big))
graph export "$Fig/tau.png", replace

************ Figure 7: geographic breakdown of wedge

merge m:1 tract using "$in/A_i"

replace Aj_norm=Aj_norm 
format Aj_norm %12.2f
spmap Aj_norm  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)  cln(6) clm(q) ndsize(vvvthin) ocolor(none none none none none none none none none none none) legtitle("Aj")   legend(size(big))
graph export "$Fig/A_i.png", replace

* save coordinates of opportunity areas 
drop _merge
merge m:1 tract using "$in/opportunity_areas"
gen opp_area=(_merge==3) 

preserve 
keep if opp_area==1
keep _CX _CY zoning_ID
ren zoning_ID ID
save "$in/coord_opp_areas", replace
restore 

******************************
* Maps with Opportunity Zones
******************************

use geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp, clear
ren _ID ID 
merge m:1 ID using "$in/coord_opp_areas"
keep if _merge ==3
ren ID _ID 
save zoning_opp_areas, replace


****** Figure 8-9: Geographic breakdown of wedges and productivity in Opportunity Zones  

use "$in/lodes_zoning_merged", replace
drop if home_tract==.
rename home_tract tract 
drop _merge 
merge m:1 tract using "$in/tract_tau_R"
drop _merge 

gen wedge=1+tau_i_baseline

merge m:1 tract using "$in/A_i"

replace Aj_norm=Aj_norm 

drop _merge
merge m:1 _CX _CY using "$in/coord_opp_areas"

replace wedge = . if _merge!=3 
format wedge %12.2f
spmap wedge  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)  cln(6) clm(custom) clbreaks(0.12 0.46 0.54 0.62 0.70 0.87 2.36)  ndsize(vvvthin) ocolor(none none none none none none none none none none none) polygon(data(zoning_opp_areas) osize(vvvthin) ocolor(gray)) legtitle("wedge")  legend(size(big))
graph export "$Fig/tau_opp_area.png", replace

replace Aj_norm = . if _merge!=3 
format Aj_norm %12.2f
spmap Aj_norm  using geo_export_988f7b08-33a7-4b15-9c84-25978f8c7690_shp if zoning_ID !=1, id(zoning_ID) fcolor(Reds)  cln(6) clm(custom) clbreaks(-2.51 -0.60 -0.15 0.11 0.48 0.93 6.12) ndsize(vvvthin) ocolor(none none none none none none none none none none none) polygon(data(zoning_opp_areas) osize(vvvthin) ocolor(gray)) legtitle("A_j")  legend(size(big))
graph export "$Fig/Aj_opp_area.png", replace


