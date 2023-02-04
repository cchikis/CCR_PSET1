

/******************************************************************************

This do file cleans data from LODES 


******************************************************************************/

global wdir ".../Stata_"
global data "$wdir/data"
global in "$wdir/in"
global out "$wdir/out"
global temp "$wdir/temp"

cd "/Users/nicolorizzotti/Desktop/chicago/spatial_economics/pset/data" 

import delimited "$data/LODES/il_od_main_JT01_2019.csv", stringcols(1 2) clear
 save "$in/LODES_2019.dta", replace

 import delimited "$data/LODES/il_rac_S000_JT01_2019.csv", stringcols(1 2) clear
 save "$in/LODES_rac_2019.dta", replace
 
 import delimited "$data/LODES/il_wac_S000_JT01_2019.csv", stringcols(1 2) clear
 save "$in/LODES_wac_2019.dta", replace
 
 
	**********************
	* RAC
	**********************
	
	use "$in/LODES_rac_2019.dta", clear
	gen home_tract = substr(h_geocode, 1, 11)
	
	egen tot_resid_low = rowtotal(cd01-cd02) 
	egen tot_resid_high = rowtotal(cd03-cd04)
	
	ren c00 tot_resid 
	
	egen tot_resid_low_wage=rowtotal(ce01-ce02)
	gen tot_resid_high_wage=ce03
	
	/*
	egen tot_resid_ind1= rowtotal(cns01-cns08)
    egen tot_resid_ind2= rowtotal(cns09-cns19)
    egen tot_resid_ind2_PA= rowtotal(cns09-cns20)
	*/
	egen tot_resid_old=rowtotal(ca02-ca03)
	ren ca01 tot_resid_young
	
	ren cr01 tot_resid_white
	ren cr02 tot_resid_black

	keep home_tract tot_resid*
    destring tot_resid, replace

	collapse (sum) tot_resid*, by(home_tract)
	destring home_tract, replace 
	format home_tract %15.0gc
	
    gen share_skilled_rac = tot_resid_high/(tot_resid_high+tot_resid_low)
    gen share_unskilled_rac = tot_resid_low/(tot_resid_high+tot_resid_low)
    gen share_highwage_rac = tot_resid_high_wage/(tot_resid)
    gen share_lowwage_rac = tot_resid_low_wage/(tot_resid)
    gen share_white_rac = tot_resid_white/(tot_resid)
    gen share_black_rac = tot_resid_black/(tot_resid)
		
	save "$out/LODES_rac_tot_jobs", replace 
	

	**********************
	* WAC
	**********************
	use "$in/LODES_wac_2019.dta", clear
	gen work_tract = substr(w_geocode, 1, 11)
	
	egen tot_job_low = rowtotal(cd01-cd02) 
	egen tot_job_high = rowtotal(cd03-cd04)
    
	ren c00 tot_job
	
	egen tot_job_low_wage=rowtotal(ce01-ce02)
	gen tot_job_high_wage=ce03
	
	/*
	egen tot_job_ind1= rowtotal(cns01-cns08)
    egen tot_job_ind2= rowtotal(cns09-cns19)
    egen tot_job_ind2_PA= rowtotal(cns09-cns20)
    */ 

	egen tot_job_old=rowtotal(ca02-ca03)
	ren ca01 tot_job_young
	
	ren cr01 tot_job_white
	ren cr02 tot_job_black

	keep work_tract tot_job*
    destring tot_job, replace

	collapse (sum) tot_job*, by(work_tract)
	destring work_tract, replace 
	format work_tract %15.0gc
	
	gen share_skilled_wac = tot_job_high/(tot_job_high+tot_job_low)
    gen share_unskilled_wac = tot_job_low/(tot_job_high+tot_job_low)
    gen share_highwage_wac = tot_job_high_wage/(tot_job)
    gen share_lowwage_wac = tot_job_low_wage/(tot_job)
    gen share_white_wac = tot_job_white/(tot_job)
    gen share_black_wac = tot_job_black/(tot_job)
	
	save "$out/LODES_wac_tot_jobs", replace 
	

    **********************
	* OD
	**********************
    use "$in/LODES_2019.dta", clear
	gen work_tract = substr(w_geocode, 1, 11)
	gen home_tract = substr(h_geocode, 1, 11)

	* Aggregate block level data to census tract level
	collapse (sum) s000 se01 se02 se03 si01 si02 si03 sa01 sa02 sa03, by(work_tract home_tract)
	
	
	destring work_tract, g(tract)
	format tract %15.0gc
	merge m:1 tract using "$in/2022_Gaz_tracts_national_Cook.dta", keepusing(home_ID)
	keep if _merge ==3
	cap drop work_tract
	ren (tract home_ID) (work_tract work_ID)
	drop _merge
	destring home_tract, g(tract)
	format tract %15.0gc
	merge m:1 tract using "$in/2022_Gaz_tracts_national_Cook.dta", keepusing(home_ID)
    keep if _merge ==3
    cap drop home_tract
	ren tract home_tract
	drop if s000 == .
	drop _merge
	drop if home_ID == . | work_ID == .
	save "$temp/LODES_2019_edited.dta", replace
	
	**********************
	* Commuting flows
	**********************
	use "$temp/LODES_2019_edited.dta", clear 
	
	merge m:1 work_tract using "$out/LODES_wac_tot_jobs"
	keep if _merge==3
	gen share_high = tot_job_high/(tot_job_high+tot_job_low)
	gen flow_high=s00*share_high
	gen flow_low=s00*(1-share_high)
	
	drop _merge
    merge m:1 home_tract using "$out/LODES_rac_tot_jobs"
	keep if _merge==3 
	gen share_high_v2 = tot_resid_high/(tot_resid_high+tot_resid_low)
	gen flow_high_v2=s00*share_high_v2
	gen flow_low_v2=s00*(1-share_high_v2)
	
	drop _merge
	save "$out/LODES_2019_flows.dta", replace 
	
    *************************
	* Commuting flows by educ
	*************************
	
	use "$out/LODES_2019_flows.dta", clear
	
    preserve 
	keep work_tract
	duplicates drop 
	save "$in/list_work_tracts.dta", replace
	restore
	
	
	merge 1:1 work_ID home_ID using "$out/IDMatrix_Cook.dta"
	
	keep work_ID home_ID s000 flow_high flow_low 
	order work_ID home_ID s000 
	replace s000 = 0 if s000 == .
	replace flow_high = 0 if flow_high == .
	replace flow_low = 0 if flow_low == .

	ren s000 flows 
 
	sort work_ID home_ID
	save "$in/Flows2019.dta", replace
	
	use "$in/Flows2019.dta", clear

	foreach var of varlist flows flow_high flow_low {
    preserve 
	keep work_ID home_ID `var'
	quietly reshape wide `var', i(work_ID) j(home_ID) // Create full matrix.
     
	export excel using "$wdir/Stata_to_Excel/`var'2019.xlsx", firstrow(variables) replace
	restore
	}
	
/*
	*********************
	* Regression theta
	*********************
	
	use "$in/Flows2019.dta", clear 
	bysort home_ID: egen total_resid=total(flows)
	gen pij=flows/total_resid
	merge 1:1 home_ID work_ID using "$in/Straight-Line Distance.dta"
	bysort home_ID: gen pii= pij if home_ID==work_ID 
	bysort home_ID: egen pii_y= max(pii) 

	gen ln_y = ln(pij/pii_y)
	
	cap drop ln_x
	gen ln_x = -ln(dist_km)
	reghdfe ln_y ln_x if ln_y!=0, noconstant absorb(home_ID work_ID)
	
	
	*********************
	* Workplace Wages 
	*********************
	
	use "$temp/LODES_2019_edited.dta", clear
	collapse (sum) s000 se01 se02 se03, by(work_ID)
	//Need to use the upper wage bound obtained for each year-see upper bounds obtained for each year above

		gen Wj = ((((se01 * ((0 + 1250) / 2)) + (se02 * ((1251 + 3333) / 2)) + (se03 * ((3334 + 9869.655) / 2))) * 12) / (se01 + se02 + se03)) // With upper bound
 
	ren work_ID home_ID
	merge 1:1 home_ID using "$in/2022_Gaz_tracts_national_Cook.dta" // To create full vector.
	keep home_ID Wj
	order home_ID Wj
	replace Wj = 0 if Wj == .
	ren home_ID ID
	sort ID
    save "$out/Wages.dta", replace
    export excel using "Stata_to_Excel/Wj.xlsx", firstrow(variables) replace
	
	*/
	
	