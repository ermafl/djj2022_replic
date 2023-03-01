*********************************************************************************
*	 Project: Breakthrough (BT)
*
*	 Purpose: Tables for the Breakthrough RCT paper
*********************************************************************************


clear all

set more off
cap log close
pause on

	* code parts: If 0, part will not run. If 1, part will execute code. 
	/* IMPORTANT NOTE: If running c0 to produce paper stats, please run all the code parts 
	as some locals are used accross sections */
	local c0 = 1 // run code to produce text file (paper_stats) with stats cited in paper
	local c1 = 0 // setting globals for controls
	local c2 = 1 // paper tables (1.1-1.13)
	local c3 = 0 // appendix tables
	local c4 = 0 // additional slides tables
	
* local that defines latex table setting	
#delimit ;
local frame "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
\begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{colno}{>{\centering\arraybackslash}m{2.3cm}}}
\toprule";
#delimit cr	

** defining blank column program
cap program drop blank_col
program define blank_col, eclass
	matrix blank_coef = e(b) * 0
	matrix blank_sd = e(V) * 0
	ereturn repost b = blank_coef
	ereturn repost V = blank_sd
	ereturn scalar N = 0
end

** text file for all stats listed in paper

if `c0'==1 {
	cap file close paperstat
	file open paperstat using "$mainpaper/paper_stats.txt", write replace
}

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
* Setting Locals for Indices
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

local gender_index2 gender
local gender_index2_educ educ
local gender_index2_emp emp
local gender_index2_sub sub
local gender_index2_fert fert

local aspiration_index2 aspiration

local behavior_index2 behavior_common
local behavior_index2_g behavior_girl
local behavior_index2_b behavior_boy

local behavior_index2_oppsex behavior_oppsex
local behavior_index2_hhchores behavior_hhchores
local behavior_index2_relatives behavior_relatives
local behavior_index2_decision_g behavior_decision_girl
local behavior_index2_mobility_g mobility

local petition_index2 petition
local scholar_index2 scholar

local discrimination_index2 discrimination	
local esteem_index2_girl esteem_girl
local educ_attain_index2_g educ_attain
local mar_fert_asp_index2 mar_fert_asp
local harassed_index2_g harassed 


*************************************************************************
*************************************************************************	
*		Part 1: Setting globals for controls (external .do file)
*************************************************************************
*************************************************************************

if `c1'==1 {
	do "$do/sup_controls"
}


*************************************************************************
*************************************************************************	
*		Part 2: Paper tables (table 1.1-1.11)
*************************************************************************
*************************************************************************

if `c2'==1 {
	
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
*		TABLE 1.1: Descriptive statistics (school and students)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

use "$baseline_sch", clear

lab var Coed "School is co-ed"
lab var distance_hq "Distance to district headquarters (km)"
lab var fulltime_teacher "Number of full-time teachers"
lab var q11_male_teachr "Number of male teachers"
lab var q11_female_teachr "Number of female teachers"

gen q11_tot_teachr=q11_male_teachr+q11_female_teachr
lab var q11_tot_teachr "Number of teachers"
gen q15_tot=q15_6th_tot+q15_7th_tot+q15_8th_tot+q15_9th_tot+q15_10th_tot
lab var q15_tot "Total enrolment"

gen q15_tot_urban=q15_tot if urban==1  
gen q15_tot_rural=q15_tot if urban==0

gen tot_6th_male=q15_6th_male+q15_7th_male
gen tot_6th_female=q15_6th_female+q15_7th_female	
gen tot_8th_male=q15_8th_male+q15_9th_male+q15_10th_male
gen tot_8th_female=q15_8th_female+q15_9th_female+q15_10th_female

lab var tot_6th_male "Males in grades 6 and 7"
lab var tot_6th_female "Females in grades 6 and 7"
lab var tot_8th_male "Males in grades 8, 9, and 10"
lab var tot_8th_female "Females in grades 8, 9, and 10"

count if treat!=. & School_ID!=2711 & School_ID!=2704 // baseline sample counts
local total: di %7.0fc `r(N)'

count if treat==1 & School_ID!=2711 & School_ID!=2704
local treat: di %7.0fc `r(N)'

count if treat==0 & School_ID!=2711 & School_ID!=2704  
local control: di %7.0fc `r(N)'


** paper stats
* all schools

local stat = `total'+1 // 1 missing from baseline, but covered in both endlines
local stat: di %7.0fc `stat'
cap file write paperstat "Intro pg2, total schools, `stat'" _n

* co-ed schools 
count if inlist(coed_status, 1, 4)
local stat: di %7.0fc `r(N)'
cap file write paperstat "3.Study Design - 3.1 Experimental Design pg9, co-ed schools, `stat'" _n

* girls only schools 
count if inlist(coed_status, 3)
local stat: di %7.0fc `r(N)'
cap file write paperstat "3.Study Design - 3.1 Experimental Design pg9, girls only schools, `stat'" _n

* boys only schools 
count if inlist(coed_status, 2)
local stat: di %7.0fc `r(N)'
cap file write paperstat "3.Study Design - 3.1 Experimental Design pg9, boys only schools, `stat'" _n

* treated 
count if inlist(treat, 1)
local stat: di %7.0fc `r(N)'
cap file write paperstat "3.Study Design - 3.1 Experimental Design pg9, treated schools, `stat'" _n

*control
count if inlist(treat, 0)
local stat: di %7.0fc `r(N)'
cap file write paperstat "3.Study Design - 3.1 Experimental Design pg9, control schools, `stat'" _n


cap file close sumstat
file open sumstat using "$tables/el2_tab_descriptive_school_students.tex", write replace


file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\extracolsep{3pt}}p{9cm}*{4}{>{\centering\arraybackslash}m{2cm}}@{}}   \toprule" _n      // table header
file write sumstat " Variable & Treatment & Control & Standardized diff    \\     " _n         
file write sumstat "\midrule" _n 
file write sumstat "Number of schools & `treat' &`control' &    \\    \addlinespace[3pt]   " _n 

local varlist urban Coed tot_6th_male tot_6th_female

foreach var in `varlist' {

	local varlab: variable label `var'   // we use variable label from child vars in this loop, but makes an exception for illiterate which only applies to parents
	
	sum `var' if School_ID!=2711 & School_ID!=2704
	local totalmean: di %7.3f `r(mean)'	
	local totalsd: di %7.3f `r(sd)'
	
	sum `var' if treat==1 & School_ID!=2711 & School_ID!=2704
	local treatmean: di %7.3f `r(mean)'	
	local treatsd: di %7.3f `r(sd)'
	
	sum `var' if treat==0 & School_ID!=2711 & School_ID!=2704
	local controlmean: di %7.3f `r(mean)'	
	local controlsd: di %7.3f `r(sd)'
	
	gen stand_diff = (`treatmean'-`controlmean')/ `totalsd'
	sum stand_diff
	local diff: di %7.3f `r(mean)'
	
	
	foreach x in treatsd controlsd diff {   // to remove leading blank spaces from numbers, and round the number to 3 decimal spaces
	local `x'=trim(string(``x'',"%7.3f"))
}

// we write out tex table lines showing means followed by sd in brackets. We make exceptions for the vars that only apply to girls/boys, or mothers/fathers.

file write sumstat "`varlab'&  `treatmean' & `controlmean'  & `diff'     \\     " _n  
file write sumstat "    & [`treatsd']  & [`controlsd']  &    \\   \addlinespace[3pt]   " _n  

drop stand_diff
}

use "$finaldata", clear

label var B_mom_age "Mother's age"
label var B_dad_age "Father's age"
label var B_m_illiterate "Mother is illiterate"
label var B_f_illiterate "Father is illiterate"
label var B_m_secondary "Mother finished grade 8"
label var B_f_secondary "Father finished grade 8"
label var B_Sgirl "Female"
label var B_Sgrade6 "Enrolled in grade 6"
label var B_Sgrade7 "Enrolled in grade 7"	

//making below loop work
foreach var in B_Sgirl B_Sgrade6 B_Ssocial_scale_int_imp B_Ssocial_scale_belowm B_Ssocial_scale  {
	gen `var'_flag = mi(`var')
}

gen all_flag = 0 // 0 if all variables in the list missing
foreach var in Sage Sgirl Shindu Sgrade6 Scaste_sc mom_age dad_age m_illiterate m_fulltime Sflush_toilet Sgender_index2 Saspiration_index2 Sbehavior_index2 ///
Ssocial_scale Ssocial_scale_int_imp {     
	replace all_flag = 1 if B_`var'_flag==0 
}

count if B_treat!=. & Sschool_id!=2711 & child_id!=3205037 & all_flag==1     // baseline sample counts
local total: di %7.0fc `r(N)'

* all students (including the school missing at baseline)
count if B_treat!=. & child_id!=3205037 & all_flag==1     // all children
local stat: di %7.0fc `r(N)'
cap file write paperstat "Intro pg2, total students, `stat'" _n

count if B_treat!=.  & Sschool_id!=2711 & child_id!=3205037 & all_flag==1     // all children at baseline
local stat: di %7.0fc `r(N)'
cap file write paperstat "3.Study Design - 3.2 Enrollment of baseline participants pg9, students covered in baseline, `stat'" _n

count if B_treat==0 & Sschool_id!=2711 & child_id!=3205037 & all_flag==1 
local control: di %7.0fc `r(N)'

count if B_treat==1 & Sschool_id!=2711 & child_id!=3205037 & all_flag==1 
local treat: di %7.0fc `r(N)'

file write sumstat "\midrule" _n
file write sumstat "Number of students &`treat' &`control' &   \\    \addlinespace[3pt]   " _n 

gen B_Sgender_index2_girl = B_Sgender_index2 if B_Sgirl == 1
gen B_Sgender_index2_boys = B_Sgender_index2 if B_Sgirl == 0
gen B_Sbehavior_index2_girl = B_Sbehavior_index2 if B_Sgirl == 1
gen B_Sbehavior_index2_boys = B_Sbehavior_index2 if B_Sgirl == 0

*** high social desirability scale
summ B_Ssocial_scale, detail
gen B_Shighsd_std=1 if B_Ssocial_scale>`r(p50)'
replace B_Shighsd_std=0 if B_Ssocial_scale<=`r(p50)' //EDIT: Originally ">=" on this line was ">", so 3,075 obs that == median were missing. 
la var B_Shighsd_std "High social desirability score"
gen B_Shighsd_std_flag = mi(B_Shighsd_std)

foreach var in B_Sgender_index2_girl B_Sgender_index2_boys B_Sbehavior_index2_girl B_Sbehavior_index2_boys {
	local base = substr("`var'", 1, strlen("`var'")-5)
	local gender = substr("`var'", strlen("`var'")-4, strlen("`var'"))
	gen `var'_flag = `base'_flag
	if "`gender'" == "_girl" {
		local gender "(girls)"
	}
	else if "`gender'" == "_boys" {
		local gender "(boys)"
	}
	local base_lab: var la `base'
	local new_lab "\hspace{3mm} `base_lab' `gender'"
	la var `var' "`new_lab'"
}

foreach var in Sage Sgirl Shindu Sgrade6 Scaste_sc mom_age dad_age m_illiterate m_fulltime Sflush_toilet Sgender_index2 ///
Saspiration_index2 Sbehavior_index2 Ssocial_scale Shighsd_std {     
	
	if inlist("`var'", "Sgender_index2", "Saspiration_index2", "Sbehavior_index2") local varlab : variable label E_`var' // labels for baseline and endline indices are not the same, choosing endline because it simply says 'Gender Attitude Index'
	else local varlab: variable label B_`var'
	
	sum B_`var' if Sschool_id!=2711 & child_id!=3205037 & B_`var'_flag==0
	local totalmean = `r(mean)'	
	local totalsd = `r(sd)'
	
	sum B_`var' if B_treat==1 & Sschool_id!=2711 & child_id!=3205037 & B_`var'_flag==0
	local treat = `r(mean)'
	local treatmean: di %7.3f `r(mean)'	
	local treatsd: di %7.3f `r(sd)'
	
	sum B_`var' if B_treat==0 & Sschool_id!=2711 & child_id!=3205037 & B_`var'_flag==0
	local control = `r(mean)'
	local controlmean: di %7.3f `r(mean)'	
	local controlsd: di %7.3f `r(sd)'
	
	gen stand_diff = (`treat'-`control')/ `totalsd'
	sum stand_diff
	local diff: di %7.3f `r(mean)'


	foreach x in treatsd controlsd diff {   // to remove leading blank spaces from numbers, and round the number to 3 decimal spaces
	local `x'=trim(string(``x'',"%7.3f"))		
}

// we write out tex table lines showing means followed by sd in brackets.

if abs(`controlmean') < .0001 {
	local controlmean "0.000"
}
if abs(`treatmean') < .0001 {
	local treatmean "0.000"
}

file write sumstat "`varlab'&   `treatmean' & `controlmean'  & `diff'        \\     " _n  
file write sumstat "  & [`treatsd']  & [`controlsd']  &    \\   \addlinespace[3pt]   " _n  

drop stand_diff

}

** paper stats

su B_Pgender_index2 if Sschool_id!=2711 & child_id!=3205037
local stat: di %7.0fc `r(N)'
cap file write paperstat "3.Study Design - 3.2 Enrollment of baseline participants pg10, Num. parents with complete BL surveys, `stat'" _n

su B_Pgender_index2 if Sschool_id!=2711 & child_id!=3205037 & B_Pgender==1
local stat: di %7.0fc `r(N)'
cap file write paperstat "Data Appendix A.1 Sample selection and tracking, Num. fathers with complete BL surveys, `stat'" _n

su B_Pgender_index2 if Sschool_id!=2711 & child_id!=3205037 & B_Pgender==2
local stat: di %7.0fc `r(N)'
cap file write paperstat "Data Appendix A.1 Sample selection and tracking, Num. mothers with complete BL surveys, `stat'" _n


su B_Sage if Sschool_id!=2711 & child_id!=3205037 & B_Sage_flag==0
local stat: di %7.3fc `r(mean)'
cap file write paperstat "3.Study Design - 3.2 Enrollment of baseline participants pg10, mean baseline student age, `stat'" _n

su B_mom_age if Sschool_id!=2711 & child_id!=3205037 & B_mom_age_flag==0 
local stat: di %7.3fc `r(mean)'
cap file write paperstat "3.Study Design pg10, mean baseline mother age, `stat'" _n

su B_dad_age if Sschool_id!=2711 & child_id!=3205037 & B_dad_age_flag==0
local stat: di %7.3fc `r(mean)'
cap file write paperstat "3.Study Design pg10, mean baseline father age, `stat'" _n

su B_m_fulltime if Sschool_id!=2711 & child_id!=3205037 & B_m_fulltime_flag==0
local stat = 100*`r(mean)'
local stat: di %7.3fc `stat'
cap file write paperstat "3.Study Design pg10, % mothers work full time, `stat'" _n


file write sumstat "\midrule" _n


local baseline_var B_Sage B_Sgirl B_Shindu B_Sgrade6 B_Scaste_sc B_mom_age B_dad_age B_m_illiterate B_m_fulltime B_Sflush_toilet B_Sgender_index2 ///
B_Saspiration_index2 B_Sbehavior_index2 B_Ssocial_scale B_Ssocial_scale_belowm B_Ssocial_scale_int_imp


local baseline_var_flag B_Sage_flag B_Shindu_flag B_Scaste_sc_flag B_Scaste_st_flag B_mom_age_flag B_dad_age_flag B_m_illiterate_flag B_m_fulltime_flag ///
B_Sflush_toilet_flag B_Sgender_index2_flag B_Sbehavior_index2_flag B_Ssocial_scale_flag B_Ssocial_scale_belowm_flag B_Ssocial_scale_int_imp_flag


eststo clear
eststo: reg B_treat `baseline_var' `baseline_var_flag' if Sschool_id!=2711 & child_id!=3205037 , cluster(Sschool_id)

local fstat: di %7.3fc `e(F)'

di "fstat is :`fstat'"

file write sumstat "\multicolumn{4}{l}{Notes: F-stat for joint significance of above baseline student variables is `fstat'.} " _n
file write sumstat "\end{tabular}" _n 
file close sumstat

drop all_flag

*** endline 2 age: 
su E2_Sstudent_age_y if attrition==0
local stat: di %7.3fc `r(mean)'
cap file write paperstat "Intro pg2, mean age at EL2, `stat'" _n

******************
*** slides table - baseline indices
******************

cap file close sumstat
file open sumstat using "$slides/el2_tab_bal_bl.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\extracolsep{3pt}}{l}*{4}{>{\centering\arraybackslash}m{2cm}}@{}}   \toprule" _n      // table header
file write sumstat "  & Treatment & Control & Standardized Diff    \\     " _n         
file write sumstat "\midrule" _n 


foreach var in Sgender_index2 Saspiration_index2 Sbehavior_index2 {     

	if inlist("`var'", "Sgender_index2", "Saspiration_index2", "Sbehavior_index2") local varlab : variable label E_`var' // labels for baseline and endline indices are not the same, choosing endline because it simply says 'Gender Attitude Index'
	else local varlab: variable label B_`var'

	sum B_`var' if Sschool_id!=2711 & child_id!=3205037
	local totalmean = `r(mean)'	
	local totalsd = `r(sd)'

	sum B_`var' if B_treat==1 & Sschool_id!=2711 & child_id!=3205037
	local treat = `r(mean)'
	local treatmean: di %7.3f `r(mean)'	
	local treatsd: di %7.3f `r(sd)'

	sum B_`var' if B_treat==0 & Sschool_id!=2711 & child_id!=3205037
	local control = `r(mean)'	
	local controlsd: di %7.3f `r(sd)'
	local controlmean: di %7.3f abs(`r(mean)')	

	gen stand_diff = (`treat'-`control')/ `totalsd'
	sum stand_diff
	local diff: di %7.3f `r(mean)'


	foreach x in treatsd controlsd diff {   // to remove leading blank spaces from numbers, and round the number to 3 decimal spaces
	local `x'=trim(string(``x'',"%7.3f"))
}


// we write out tex table lines showing means followed by sd in brackets.


file write sumstat "`varlab'&   `treatmean' & `controlmean'  & `diff'        \\     " _n  
file write sumstat "  & [`treatsd']  & [`controlsd']  &    \\   \addlinespace[3pt]   " _n  

drop stand_diff

}

file write sumstat "\bottomrule" _n           // table footer
file write sumstat "\end{tabular}" _n 
file close sumstat

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*  TABLE 1.2&1.4: EL1 Primary Outcomes (combined and by gender)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

eststo clear	
local header
local header_g
local header_b	
local n_col = 0
local n_col_g = 0
local n_col_b = 0

putexcel set "$ad_hoc/gender_interaction_terms.xlsx", replace
putexcel A1 = ("Table 1.2 (Behavior index)")

log using "$ad_hoc/tab1.4_el1_primary.smcl", replace
qui log off 

foreach index in gender_index2 gender_index_factor aspiration_index2 aspiration_index_factor behavior_index2 behavior_index_factor {		

	local varlab: variable label E_S`index'
	local header "`header' & `varlab'" // column headers with variables labels 
	local ++n_col // number of columns

	*** girls columns
	local header_g "`header_g' & `varlab'" // girls column headers 
	local ++n_col_g 	

	if strpos("`index'", "aspiration")>0 {
		eststo bs_`index' : reg E_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el_``index''_flag} if B_Sgirl==1 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) // only for girls
		su E_S`index' if B_treat==0 & !mi(E_Steam_id) & B_Sgirl==1 & attrition_el==0
		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean)
		if abs(r(mean)) < .00005 estadd scalar ctrlmean_g=abs(r(mean)) 
		else estadd scalar ctrlmean_g=r(mean) 
		estadd local basic "Yes"

		*** girls regression 
		eststo gbs_`index' : reg E_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el_``index''_flag} if B_Sgirl==1 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) // only for girls
		su E_S`index' if B_treat==0 & !mi(E_Steam_id) & B_Sgirl==1 & attrition_el==0
		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean)
		if abs(r(mean)) < .00005 estadd scalar ctrlmean_g=abs(r(mean)) 
		else estadd scalar ctrlmean_g=r(mean) 
		estadd local basic_g "Yes"
		local stat: di %7.4fc _b[B_treat]
		cap file write paperstat "4.2Short-run results pg16, EL1 girls' aspirations SD, `stat'   " _n 

	}

	else {

		*** p value calculation
		local controls B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el_``index''_flag}
		local gX `controls'
		local i = 2
		foreach var of varlist `controls'{
			local varname gX`var'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_Sgirl * `var'
			local gX `gX' gX`var'
			local varlab: var lab `var'
			if "`varlab'" == "" local varlab `var'
			putexcel A`i' = ("FemaleX`varlab'")
			local ++i 
		}

		*** logging all girl interaction terms included in p-value regression
		qui log on 
		di "Column: `index'"
		reg E_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} gX*  if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
		qui log off 
		local `index'_p: di %7.3f 2*ttail(e(df_r), abs(_b[gXB_treat]/_se[gXB_treat]))
		drop gX*

		*** boys columns
		local header_b "`header_b' & `varlab'" // boys column headers 
		local ++n_col_b 

		eststo bs_`index' : reg E_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
		su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean)
		estadd local basic "Yes"


		*** girls regression
		eststo gbs_`index' : reg E_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el_``index''_flag}  if B_Sgirl == 1 & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
		su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 1
		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean) 
		estadd local basic "Yes"
		estadd local p_val ``index'_p'

		*** boys regression
		eststo bbs_`index' : reg E_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el_``index''_flag}  if B_Sgirl == 0 & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
		su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 0
		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean) 
		estadd local basic "Yes"

		if "`index'"=="gender_index2" | "`index'"=="	"  {

			reg E_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
			local stat: di %7.4fc _b[B_treat]
			local el1_att_coef = _b[B_treat] // paper stat used in benchmark table (so need to run both main and appendix tables together)
			cap file write paperstat "Abstract, EL1 gender attitudes SD, `stat'   " _n 
			cap file write paperstat "Intro pg3, EL1 gender attitudes SD, `stat'   " _n 
			cap file write paperstat "4.2Short-run results pg14, EL1 gender attitudes SD, `stat'   " _n 
			
			local t = _b[B_treat]/_se[B_treat]
			local p = 2*ttail(e(df_r),abs(`t'))
			
			local stat: di %7.5fc `p'			
			cap file write paperstat "4.2Short-run results pg14, EL1 gender attitudes p-value, `stat'   " _n 
		}

		else if "`index'"=="behavior_index2" | "`index'"=="behavior_index_factor" {
			reg E_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
			local stat: di %7.4fc _b[B_treat]
			cap file write paperstat "4.2Short-run results pg16, EL1 self-reported behavior SD, `stat'   " _n 

		}
		
	}

	estadd local cluster = `e(N_clust)'		
	//estadd local basic "Yes"	
}

***combined results***

local frame1 = subinstr("`frame'", "2.3cm", "3.3cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col_g'", .)

#delimit ;

esttab bs_* using "$tables/el1_primary_outcomes_basic_combined.tex", 
b(3) se(3) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment nostar
prehead(" `prehead'" "`header' \\")
keep(B_treat) order(B_treat)
stats(ctrlmean basic N,   
labels("Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %20s %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}");

esttab bs_* using "$tables/el1_primary_outcomes_basic_combined_star.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment
prehead(" `prehead'" "`header' \\")
keep(B_treat) order(B_treat)
stats(ctrlmean basic N,   
labels("Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %20s %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}");

#delimit cr 

***panel results***

local frame1 = subinstr("`frame'", "2.3cm", "2.5cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "4", .)
local header_c " & Girls & Boys & Girls & Boys " // new style for table
local multi_c " & \multicolumn{2}{c}{Gender attitudes index} & \multicolumn{2}{c}{Self-reported behavior index} \\ 	\cmidrule(lr){2-3} \cmidrule(lr){4-5}"

#delimit ;

esttab gbs_gender_index2 bbs_gender_index2 gbs_behavior_index2 bbs_behavior_index2
using "$tables/el1_primary_outcomes_basic.tex", 
b(3) se(3) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment nostar
prehead(" `prehead'" "`multi_c'" "`header_c' \\")
keep(B_treat) order(B_treat)
stats(ctrlmean basic N,   
labels("Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %7.3fc %20s %7.0fc))
postfoot(" p-value: Girls=Boys & \multicolumn{2}{c}{`gender_index2_p'} & \multicolumn{2}{c}{`behavior_index2_p'} \\ " "\bottomrule" "\end{tabular}" "}");

esttab gbs_gender_index2 bbs_gender_index2 gbs_behavior_index2 bbs_behavior_index2
using "$tables/el1_primary_outcomes_basic_star.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment
prehead(" `prehead'" "`multi_c'" "`header_c' \\")
keep(B_treat) order(B_treat)
stats(ctrlmean basic N,   
labels("Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %7.3fc %20s %7.0fc))
postfoot(" p-value: Girls=Boys & \multicolumn{2}{c}{`gender_index2_p'} & \multicolumn{2}{c}{`behavior_index2_p'} \\ " "\bottomrule" "\end{tabular}" "}");

#delimit cr 

log close
}
/*

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	TABLE 1.3: Robustness check for social desirability bias (Endline 1) 	
*	APPENDIX TABLE 1.19: Robustness check for social desirability bias (Endline 1) (gender panels)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

gen lowsd_std =  B_Ssocial_scale_belowm
la var lowsd_std "Low social desirability score"

gen treat_lowsd_std = B_treat*B_Ssocial_scale_belowm
la var treat_lowsd_std "Treated $\times$ Low social desirability score"

*** high social desirability scale
summ B_Ssocial_scale, detail
gen highsd_std=1 if B_Ssocial_scale>`r(p50)'
replace highsd_std=0 if B_Ssocial_scale<=`r(p50)' //EDIT: Originally ">=" on this line was ">", so 3,075 obs that == median were missing. 
la var highsd_std "High social desirability score"
gen treat_highsd_std = B_treat*highsd_std
la var treat_highsd_std "Treated $\times$ High social desirability score"

*** high social desirability scale, opposite 
summ B_Ssocial_scale, detail
gen highsd_std_1 = 1 - lowsd_std
la var highsd_std_1 "High social desirability score"
gen treat_highsd_std_1 = B_treat*highsd_std_1
la var treat_highsd_std_1 "Treated $\times$ High social desirability score"

putexcel B1 = ("Table 1.3 (Behavior index)")

foreach sds in lowsd_std highsd_std highsd_std_1 {

	if "`sds'" == "highsd_std" {
		log using "$ad_hoc/tab1.3_el1_mainsds.smcl", replace
		qui log off
	}

	local abb = substr("`sds'", 1, 1)

	eststo clear	
	local header	
	local n_col = 0
	local header_g
	local header_b	
	local n_col_g = 0
	local n_col_b = 0

	foreach index in gender_index2 aspiration_index2 behavior_index2  {

		local varlab: variable label E_S`index'
		local header "`header' & `varlab'" // column headers with variables labels  
		local ++n_col // number of columns

		*** girls
		local ++n_col_g 
		local header_g "`header_g' & `varlab'" // girls column headers


		if "`index'"=="aspiration_index2" {

			***combined regression
			eststo `abb'bs_`index' : reg E_S`index' B_treat `sds' treat_`sds' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el_``index''_flag} if B_Sgirl==1 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) // only for girls
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0  & B_Sgirl==1 & !mi(`sds')
			estadd scalar ctrlmean=abs(r(mean))
			estadd local cluster = `e(N_clust)'		
			estadd local basic "Yes"
			test B_treat+treat_`sds'==0
			estadd scalar pval=`r(p)' 


			*** girls regression 
			eststo `abb'gbs_`index' : reg E_S`index' B_treat `sds' treat_`sds' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el_``index''_flag} if B_Sgirl==1 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) // only for girls
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & B_Sgirl==1 & attrition_el==0
			if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
			else estadd scalar ctrlmean=r(mean)
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_g=abs(r(mean)) 
			else estadd scalar ctrlmean_g=r(mean) 
			estadd local basic_g "Yes"
			test B_treat+treat_`sds'==0
			estadd scalar pval_g=`r(p)' 

		}

		else {

			*** boys column counts
			local header_b "`header_b' & `varlab'"
			local ++n_col_b	

			*** combined regression
			eststo `abb'bs_`index' : reg E_S`index' B_treat `sds' treat_`sds' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0 & !mi(`sds')
			estadd scalar ctrlmean=abs(r(mean))
			estadd local cluster = `e(N_clust)'		
			estadd local basic "Yes"
			test B_treat+treat_`sds'==0
			estadd scalar pval=`r(p)'

			*** girls regression
			eststo `abb'gbs_`index' : reg E_S`index' B_treat `sds' treat_`sds' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 1, cluster(Sschool_id)
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0 & !mi(`sds') & B_Sgirl == 1
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_g=abs(r(mean)) 
			else estadd scalar ctrlmean_g=r(mean) 
			estadd local basic_g "Yes"
			test B_treat+treat_`sds'==0
			estadd scalar pval_g=`r(p)'
			
			*** boys regression
			eststo `abb'bbs_`index' : reg E_S`index' B_treat `sds' treat_`sds' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 0, cluster(Sschool_id)
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0 & !mi(`sds') & B_Sgirl == 0
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_b=abs(r(mean)) 
			else estadd scalar ctrlmean_b=r(mean) 
			estadd local basic_b "Yes"
			test B_treat+treat_`sds'==0
			estadd scalar pval_b=`r(p)'

			*** p value calculation
			//interacted regression
			local controls B_treat `sds' treat_`sds' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el_``index''_flag} 
			local gX `controls'
			local i = 2
			foreach var of varlist `controls'{
				local varname gX`var'
				local varname = substr("`varname'", 1, 30)
				gen `varname' = B_Sgirl * `var'
				local gX `gX' gX`var'
				local varlab: var la `var'
				if "`varlab'" == "" local varlab `var'
				putexcel B`i' = ("FemaleX`varlab'")
				local ++i
			}

			*** logging all girl interaction terms included in p-value regression
			if "`sds'" == "highsd_std" {
				qui log on
				di "Column: `index'"
				reg E_S`index' B_treat `sds' treat_`sds' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} gX* if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) 
				qui log off
			}

			reg E_S`index' B_treat `sds' treat_`sds' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} gX* if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) 
			local `abb'`index'_p1: di %7.3f 2*ttail(e(df_r), abs(_b[gXB_treat]/_se[gXB_treat]))
			local `abb'`index'_p2: di %7.3f 2*ttail(e(df_r), abs(_b[gX`sds']/_se[gX`sds']))
			local `abb'`index'_p3: di %7.3f 2*ttail(e(df_r), abs(_b[gXtreat_`sds']/_se[gXtreat_`sds']))
			drop gX*
		}

	}

	
	***combined results***
	if "`abb'" == "l" {
		local abb2 "Low"
	}
	else if "`abb'" == "h" {
		local abb2 "High"
	}

	local frame1 = subinstr("`frame'", "2.3cm", "3.3cm", .) // column spacing	
	local prehead = subinstr("`frame1'", "colno", "`n_col'", .)

	#delimit ;

	esttab `abb'bs_* using "$tables/el1_primary_outcomes_`sds'_combined.tex", 
	b(3) se(3) nonotes nomtitles number brackets 
	replace gaps label booktabs style(tex) fragment nostar
	prehead(" `prehead'" "`header' \\")
	keep(B_treat `sds' treat_`sds') order(B_treat `sds' treat_`sds')
	stats(pval ctrlmean basic N,   
	labels("p-value: Treated + Treated $\times$ `abb2' SD = 0" "Control group mean" "Basic controls" "Number of students") 
	fmt(%7.3fc %7.3fc %20s %7.0fc))
	postfoot("\bottomrule" "\end{tabular}" "}"); 

	esttab `abb'bs_* using "$tables/el1_primary_outcomes_`sds'_combined_star.tex", 
	b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
	replace gaps label booktabs style(tex) fragment 
	prehead(" `prehead'" "`header' \\")
	keep(B_treat `sds' treat_`sds') order(B_treat `sds' treat_`sds')
	stats(pval ctrlmean basic N,   
	labels("p-value: Treated + Treated $\times$ `abb2' SD = 0" "Control group mean" "Basic controls" "Number of students") 
	fmt(%7.3fc %7.3fc %20s %7.0fc))
	postfoot("\bottomrule" "\end{tabular}" "}"); 

	local prehead = subinstr("`frame'", "colno", "`n_col'", .);

	esttab `abb'bs_* using "$slides/el1_primary_outcomes_`sds'_combined.tex", 
	b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
	replace gaps label booktabs style(tex) fragment 
	prehead(" `prehead'" "`header' \\")
	keep(B_treat `sds' treat_`sds') order(B_treat `sds' treat_`sds')
	stats(N,   
	labels("Number of students") 
	fmt(%7.0fc))
	postfoot("\bottomrule" "\end{tabular}" "}"); 

	#delimit cr

	local prehead = subinstr("`frame'", "colno", "`n_col'", .)

	***panel results***

	local frame1 = subinstr("`frame'", "2.3cm", "3.3cm", .) // column spacing	
	local prehead = subinstr("`frame1'", "colno", "`n_col_g'", .)
	
	#delimit ;
	
	esttab `abb'gbs_* using "$tables/el1_primary_outcomes_`sds'.tex", 
	b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
	replace gaps label booktabs style(tex) fragment
	prehead("\begin{flushleft} Panel A: Girls \end{flushleft} \vspace{3mm} `prehead'" "`header_g' \\")
	keep(B_treat `sds' treat_`sds') order(B_treat `sds' treat_`sds')
	stats(pval_g ctrlmean_g basic_g N,   
	labels("p-val: Treated + Treated $\times$ `abb2' SD = 0" "Control group mean" "Basic controls" "Number of students") 
	fmt(%7.3fc %7.3fc %20s %7.0fc))
	postfoot("\bottomrule" "\end{tabular}" "}"); 

	#delimit cr 

	local frame1 = subinstr("`frame'", "2.3cm", "4.85cm", .) // column spacing	
	local prehead = subinstr("`frame1'", "colno", "`n_col_b'", .)

	#delimit ; 

	esttab `abb'bbs_* using "$tables/el1_primary_outcomes_`sds'.tex", 
	b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
	append gaps label booktabs style(tex) fragment
	prehead(" \vspace{1cm} \begin{flushleft} Panel B: Boys \end{flushleft} `prehead'" "`header_b' \\")
	keep(B_treat `sds' treat_`sds') order(B_treat `sds' treat_`sds')
	stats(pval_b ctrlmean_b basic_b N,   
	labels("p-val: Treated + Treated $\times$ `abb2' SD = 0" "Control group mean" "Basic controls" "Number of students") 
	fmt(%7.3fc %7.3fc %20s %7.0fc))
	postfoot("\bottomrule" "\end{tabular}" "}"); 

	#delimit cr

}

log close


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	Table 1.5: Heterogeneity by parent attitudes (endline 1)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

rename treat_girl treat_Sgirl
la var treat_Sgirl "Treated $\times$ Female"	

local treatrow B_treat B_Sgirl treat_girl
local treatrowdi B_treat treat_girl

*Parent BL attitudes
gen treat_Pgender_index2 = B_treat * B_Pgender_index2
la var treat_Pgender_index2 "Treated $\times$ Baseline parent attitudes"	 

local file_Pgender_index2 "patt_cont"

local lab_Pgender_index2 "BL parent attitudes"

foreach type in Pgender_index2 {

*** gender and parent BL attitudes
eststo clear	
local header	
local n_col = 0
local treatrowdi B_treat treat_Sgirl treat_`type' // vars to display in table

***New table loop (with panels)

local header_g
local header_b	
local n_col_g = 0
local n_col_b = 0

local treatrowdi_g B_treat treat_Pgender_index2
local treatrowdi_b B_treat treat_Pgender_index2

log using "$ad_hoc/tab1.5_el1_parentatt.smcl", replace
qui log off 

foreach var in Pgender_index2 {
	
	local i=0
	foreach index in gender_index2 aspiration_index2 behavior_index2  {

		local treatrow B_treat B_`var' treat_`var' // vars to use in regression
		local controls B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el_``index''_flag} district_gender_* gender_grade_* 
		local pX
		foreach var1 of varlist `controls'{
			local varname pX`var1'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_`var' * `var1'
			local treatrow `treatrow' `varname'
		}

		local varlab: variable label E_S`index'
		local header "`header' & `varlab'" // column headers with variables labels  
		local ++n_col // number of columns

		*** girls
		local ++n_col_g 
		local header_g "`header_g' & `varlab'" // girls column headers


		if "`index'"=="aspiration_index2" {

			*** girls regression  
			eststo gbs_`index' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==1 , cluster(Sschool_id)
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==1
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_g=abs(r(mean)) 
			else estadd scalar ctrlmean_g=r(mean)
			estadd local cluster_g = `e(N_clust)'		
			estadd local basic_g "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_g=`r(p)'

			*** combined regression
			qui log on 
			eststo cbs_`index' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
			qui log off
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_c=abs(r(mean)) 
			else estadd scalar ctrlmean_c=r(mean)
			estadd local cluster_c = `e(N_clust)'		
			estadd local basic_c "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_c=`r(p)'

		}

		else {

			*** boys column counts
			local header_b "`header_b' & `varlab'"
			local ++n_col_b	

			*** girls regression
			eststo gbs_`index' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 1, cluster(Sschool_id)
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==1
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_g=abs(r(mean)) 
			else estadd scalar ctrlmean_g=r(mean)
			estadd local cluster_g = `e(N_clust)'		
			estadd local basic_g "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_g=`r(p)'
			
			*** boys regression
			eststo bbs_`index' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 0, cluster(Sschool_id)
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==0
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_b=abs(r(mean)) 
			else estadd scalar ctrlmean_b=r(mean)  
			estadd local cluster_b = `e(N_clust)'		
			estadd local basic_b "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_b=`r(p)'

			*** combined regression
			qui log on 
			eststo cbs_`index' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
			qui log off 
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0 
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_c=abs(r(mean)) 
			else estadd scalar ctrlmean_c=r(mean)  
			estadd local cluster_c = `e(N_clust)'		
			estadd local basic_c "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_c=`r(p)'

			*** p value calculation
			//interacted regression
			local controls `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el_``index''_flag} 
			foreach var1 of varlist `controls'{
				local varname gX`var1'
				local varname = substr("`varname'", 1, 30)
				gen `varname' = B_Sgirl * `var1'
			}
			reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} gX*  if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
			local `index'_p1: di %7.3f 2*ttail(e(df_r), abs(_b[gXB_treat]/_se[gXB_treat]))
			local `index'_p2: di %7.3f 2*ttail(e(df_r), abs(_b[gXtreat_Pgender_index2]/_se[gXtreat_Pgender_index2]))
			drop gX*
		}

		drop pX*
	}
}

***combined results***
local frame1 = subinstr("`frame'", "2.3cm", "2.3cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col_g'", .)

#delimit ;

esttab cbs_* using "$tables/el1_heterog_gen_`file_Pgender_index2'_combined.tex", 
b(3) se(3) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment nostar
prehead("`prehead'" "`header_g' \\")
keep(`treatrowdi_g') order(`treatrowdi')
stats(ctrlmean_c basic_c N,   
labels("Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %20s %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

esttab cbs_* using "$tables/el1_heterog_gen_`file_Pgender_index2'_combined_star.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment
prehead("`prehead'" "`header_g' \\")
keep(`treatrowdi_g') order(`treatrowdi')
stats(ctrlmean_c basic_c N,   
labels("Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %20s %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr 

***panel results***

local frame1 = subinstr("`frame'", "2.3cm", "2.3cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col_g'", .)

#delimit ;

esttab gbs_* using "$tables/el1_heterog_gen_`file_Pgender_index2'.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment
prehead("\begin{flushleft} Panel A: Girls \end{flushleft} \vspace{3mm} `prehead'" "`header_g' \\")
keep(`treatrowdi_g') order(`treatrowdi')
stats(Pgender_index2_pval_g ctrlmean_g basic_g N,   
labels("p-val: Treated + Treated $\times$ Baseline parent attitudes = 0" "Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %7.3fc %20s %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr 

local frame1 = subinstr("`frame'", "2.3cm", "3.45cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col_b'", .)

#delimit ; 

esttab bbs_* using "$tables/el1_heterog_gen_`file_Pgender_index2'.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
append gaps label booktabs style(tex) fragment
prehead("\begin{flushleft} Panel B: Boys \end{flushleft} `prehead'" "`header_b' \\")
keep(`treatrowdi_b') order(`treatrowdi')
stats(Pgender_index2_pval_b ctrlmean_b basic_b N,   
labels("p-val: Treated + Treated $\times$ Baseline parent attitudes = 0" "Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %7.3fc %20s %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr

local frame1 = subinstr("`frame'", "2.3cm", "3.45cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col_b'", .)

file open sumstat using "$tables/el1_heterog_gen_`file_Pgender_index2'.tex", write append 
file write sumstat "\begin{flushleft} Panel C: P-vals, Boys vs. Girls \end{flushleft} `prehead' `header_b' \\"
local follow_head
foreach i of numlist 1/2 {
	local follow_head "`follow_head' & \multicolumn{1}{c}{(`i')}"
}

file write sumstat "`follow_head' \\ \midrule "
file write sumstat "Treated & `gender_index2_p1' & `behavior_index2_p1' \\ "
file write sumstat "Treated X Baseline parent attitudes & `gender_index2_p2' & `behavior_index2_p2' \\ "
file write sumstat "\bottomrule \end{tabular} }"
file close sumstat	

}

log close 

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
* TABLE 1.6: Social Norms (Endline 1) (also Table 1.12, Social Norms (Endline 2))
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

local vars E_Sallow_work_s E_Scommunity_allow_work_s E_Scommunity_work_s E_Sallow_college_s E_Scommunity_allow_college_s E_Scommunity_college_s
putexcel C1 = ("Table 1.6&1.12 (1.12 column 6)")

// saving paperstats
foreach var in `vars' {

	reg `var' B_treat district_gender_* gender_grade_* if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)

	if "`var'"=="E_Sallow_work_s"  {
		local norm1 =  100*_b[B_treat]
		local stat =  100*_b[B_treat]
		local stat: di %7.4fc `stat'
		//cap file write paperstat "4.2Short-run results pg18, EL1 secondary outcomes: attitude toward female employment, `stat'pp" _n	
	}

	else if "`var'"=="E_Scommunity_allow_work_s"  {
		local stat =  100*_b[B_treat]
		local stat: di %7.4fc `stat'
		//cap file write paperstat "4.2Short-run results pg18, EL1 secondary outcomes: perception that community has progressive attitude toward female employment, `stat'pp" _n	
	}

	else if "`var'"=="E_Scommunity_work_s"  {
		local norm3 =  100*_b[B_treat]	
		local stat = `norm1' - `norm3'
		local stat: di %7.4fc `stat'
		//cap file write paperstat "4.2Short-run results pg18, EL1 secondary outcomes: thinks women should be allowed to work, but community will oppose them, `stat'pp" _n	
	}

}

*** ADDING NEW PAPER STAT: DROPPED IAT ***
count if B_iat_duplicate == 2
local drop_duplicate = r(N)
count if B_iat_dropiat == 1
local drop_toofast = r(N)
count if B_iat_merge == 1 
local total_IAT = r(N)
local total_drop = `drop_toofast' + `drop_duplicate'
local stat: di %7.4fc `total_drop'/`total_IAT'
local stat = `stat' * 100 
cap file write paperstat "4.2Short-run results pg20, EL1 secondary outcomes: percent of baseline IAT responses that were invalidated,  `stat'%		" _n

eststo clear	
local header	
local nos
local n_col = 0

local b "Treated"
local se
local n "Number of students"
local m "Control group mean"

foreach time in el1 el2 {

	if "`time'" == "el1" {
		log using "$ad_hoc/tab1.6_el1_socialnorms.smcl", replace
		qui log off 
	}
	else if "`time'" == "el2" {
		log using "$ad_hoc/tab1.12_el2_socialnorms.smcl", replace
		qui log off 
	}

	if "`time'" == "el1" {
		local stub 
	}
	else if "`time'" == "el2" {
		local stub 2
	}

	local vars E`stub'_Sallow_work_s E`stub'_Scommunity_allow_work_s E`stub'_Scommunity_work_s E`stub'_Sallow_college_s E`stub'_Scommunity_allow_college_s E`stub'_Scommunity_college_s

	*** girls

	use "$finaldata", clear
	keep if B_Sgirl == 1

	foreach var in `vars'  {

		local varlab: variable label `var'

		if "`var'"=="E_Scommunity_allow_college_s" {
			local varlab = "community thinks women should be allowed to study in college even if it is far away"
		}

		if "`var'"=="E_Scommunity_college_s" {
			local varlab = "women should be allowed to study in college and thinks community will not oppose them"
		}                               


		local header "`header' & `varlab'" // column headers with variables labels 

		local ++n_col // number of columns
		local i = `n_col'

		local nos "`nos' & (`n_col')"		
		
		reg `var' B_treat district_? B_Sgrade6 if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)

		* storing coefs		
		local b`i': di %7.3fc _b[B_treat]
		local se`i'= trim(string(_se[B_treat],"%7.3f"))
		local t`i' = _b[B_treat]/_se[B_treat]
		local p`i' = 2*ttail(e(df_r),abs(`t`i''))
		local n`i': di %7.0f e(N)
		
		if `se`i''>0 local se`i' = "[`se`i'']"
		else  local se`i' = "`se`i''"
		
		if  `p`i'' < 0.1 &  `p`i'' >= 0.05  local b`i' `b`i''\sym{*}
		else if  `p`i'' < 0.05 &  `p`i'' >= 0.01  local b`i' `b`i''\sym{**}
		else if  `p`i'' < 0.01  local b`i' `b`i''\sym{***}

		* Control group mean
		su `var' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
		local m`i': di %7.3fc r(mean)
		
		local b "`b' & `b`i''"
		local se "`se' & `se`i''"
		local m "`m' & `m`i''"
		local n "`n' & `n`i''"

	}

	local allcol = `n_col'+1

	*** no star		
	local b_nostar = subinstr("`b'", "*", "", .)
	cap file close sumstat
	file open sumstat using "$tables/`time'_social_norms.tex", write replace

	file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{`n_col'}{>{\centering\arraybackslash}m{3.2cm}}@{}}   \toprule \\" _n      // table header
	file write sumstat 	"& \multicolumn{3}{c}{Social norms towards work} & \multicolumn{3}{c}{Social norms towards education} \\ 	\cmidrule(lr){2-4} \cmidrule(lr){5-7}" _n
	file write sumstat "& \multicolumn{3}{c}{\textit{Student agrees that...}} & \multicolumn{3}{c}{\textit{Student agrees that...}} \\" _n
	file write sumstat " `header'   \\     " _n      
	file write sumstat " `nos' \\    " _n    
	file write sumstat "\midrule \addlinespace[10pt] " _n 
	file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel A: Girls}}   \\  \addlinespace[3pt]   " _n         

	file write sumstat "`b_nostar'  \\    " _n 
	file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
	file write sumstat "`m'  \\    " _n 
	file write sumstat "`n' \\ \addlinespace[1cm]     " _n 

	file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel B: Boys}}   \\  \addlinespace[3pt]   " _n  
	file close sumstat 

	*** star 

	cap file close sumstat
	file open sumstat using "$tables/`time'_social_norms_star.tex", write replace

	file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{`n_col'}{>{\centering\arraybackslash}m{3.2cm}}@{}}   \toprule \\" _n      // table header
	file write sumstat 	"& \multicolumn{3}{c}{Social norms towards work} & \multicolumn{3}{c}{Social norms towards education} \\ 	\cmidrule(lr){2-4} \cmidrule(lr){5-7}" _n
	file write sumstat "& \multicolumn{3}{c}{\textit{Student agrees that...}} & \multicolumn{3}{c}{\textit{Student agrees that...}} \\" _n
	file write sumstat " `header'   \\     " _n      
	file write sumstat " `nos' \\    " _n    
	file write sumstat "\midrule \addlinespace[10pt] " _n 
	file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel A: Girls}}   \\  \addlinespace[3pt]   " _n         

	file write sumstat "`b'  \\    " _n 
	file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
	file write sumstat "`m'  \\    " _n 
	file write sumstat "`n' \\ \addlinespace[1cm]     " _n 

	file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel B: Boys}}   \\  \addlinespace[3pt]   " _n      
	file close sumstat

	*** boys

	use "$finaldata", clear
	keep if B_Sgirl == 0

	local b "Treated"
	local se
	local n "Number of students"
	local m "Control group mean"

	local i=0
	foreach var in `vars'   {

		local ++i
		local nos "`nos' & (`n_col')"

		reg `var' B_treat district_? B_Sgrade6 if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)

		* storing coefs		
		local b`i': di %7.3fc _b[B_treat]
		local se`i'= trim(string(_se[B_treat],"%7.3f"))
		local t`i' = _b[B_treat]/_se[B_treat]
		local p`i' = 2*ttail(e(df_r),abs(`t`i''))
		local n`i': di %7.0f e(N)

		if `se`i''>0 local se`i' = "[`se`i'']"
		else  local se`i' = "`se`i''"
		
		if  `p`i'' < 0.1 &  `p`i'' >= 0.05  local b`i' `b`i''\sym{*}
		else if  `p`i'' < 0.05 &  `p`i'' >= 0.01  local b`i' `b`i''\sym{**}
		else if  `p`i'' < 0.01  local b`i' `b`i''\sym{***}
		
		* Control group mean
		su `var' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
		local m`i': di %7.3fc r(mean)
		
		local b "`b' & `b`i''"
		local se "`se' & `se`i''"
		local m "`m' & `m`i''"
		local n "`n' & `n`i''"


	}	

	*** no star
	cap file close sumstat
	file open sumstat using "$tables/`time'_social_norms.tex", write append
	local b_nostar = subinstr("`b'", "*", "", .)
	file write sumstat "`b_nostar'  \\    " _n 
	file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
	file write sumstat "`m'  \\    " _n 
	file write sumstat "`n' \\ \addlinespace[1cm]     " _n 
	file close sumstat 

	*** star 
	cap file close sumstat
	file open sumstat using "$tables/`time'_social_norms_star.tex", write append
	file write sumstat "`b'  \\    " _n 
	file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
	file write sumstat "`m'  \\    " _n 
	file write sumstat "`n' \\ \addlinespace[1cm]     " _n 
	file close sumstat

	*** p values 
	local b " "
	local i=0

	use "$finaldata", clear

	foreach var in `vars' {
		local ++i
		local nos "`nos' & (`n_col')"

		local controls B_treat
		local gX `controls'
		local j = 2 
		foreach var1 of varlist `controls'{
			local varname gX`var1'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_Sgirl * `var1'
			local gX `gX' gX`var1'
			local varlab: var la `var1'
			if "`varlab'" == "" local varlab `var1'
			putexcel C`j' = ("FemaleX`varlab'")
			local ++j
		}

		*** logging all girl interaction terms included in p-value regression
		qui log on 
		di "Column: `var'"
		reg `var' B_treat district_gender_* gender_grade_* gX* if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
		qui log off

		local b`i': di %7.3f 2*ttail(e(df_r), abs(_b[gXB_treat]/_se[gXB_treat]))
		drop gX*

		local b "`b' & `b`i''"

	}

	*no star
	cap file close sumstat
	file open sumstat using "$tables/`time'_social_norms.tex", write append
	file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel C: Girls=Boys p-value}}   \\  \addlinespace[3pt]   " _n  
	file write sumstat "`b'  \\    " _n 
	file write sumstat "\bottomrule" _n              
	file write sumstat "\end{tabular}" _n
	file close sumstat	

	*star
	cap file close sumstat
	file open sumstat using "$tables/`time'_social_norms_star.tex", write append
	file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel C: P-vals, Boys vs. Girls}}   \\  \addlinespace[3pt]   " _n  
	file write sumstat "`b'  \\    " _n 
	file write sumstat "\bottomrule" _n              
	file write sumstat "\end{tabular}" _n
	file close sumstat	

	//resetting after we get to the end
	local header	
	local nos
	local n_col = 0

	local b "Treated"
	local se
	local n "Number of students"
	local m "Control group mean"

	log close 
}


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
*	TABLE 1.7 : Endline 1 Secondary Outcomes
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

local header	
local nos
local n_col = 0

local b "Treated"
local se
local n "Number of students"
local m "Control group mean"

putexcel D1 = ("Table 1.7 (col 4)")

local vars esteem_index2_girl discrimination_index2 goodbad_neg occupation_neg
log using "$ad_hoc/tab1.7_el1_secondary.smcl", replace
qui log off

*** girls

use "$finaldata", clear
keep if B_Sgirl == 1

foreach index in `vars'  {

	local ++n_col // number of columns
	local i = `n_col'
	local nos "`nos' & (`n_col')"	

	if strpos("`index'", "discrimination")>0 {
		local varlab: variable label E_S`index'    
		reg E_S`index' B_treat district_? B_Sgrade6 ${el_``index''_flag} if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 1, cluster(Sschool_id) 
		* Control group mean
		su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
		local m`i': di %7.3fc r(mean)
	}

	else if strpos("`index'", "neg")>0 {
		local varlab: variable label E_D_measure_`index'
		reg E_D_measure_`index' B_treat B_D_measure_`index' B_D_measure_`index'_flag /* B_D_measure_`index'_m */ district_? B_Sgrade6 if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 1, cluster(Sschool_id) 
		* Control group mean
		su E_D_measure_`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
		local m`i': di %7.3fc r(mean)
	}
	
	else if strpos("`index'", "esteem")>0 {
		local varlab: variable label E_S`index'
		reg E_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el_``index''_flag} if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==1, cluster(Sschool_id)
		* Control group mean
		su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
		local m`i': di %7.3fc r(mean)
	}

	local header "`header' & `varlab'" // column headers with variables labels 	

	* storing coefs		
	local b`i': di %7.3fc _b[B_treat]
	local se`i'= trim(string(_se[B_treat],"%7.3f"))
	local t`i' = _b[B_treat]/_se[B_treat]
	local p`i' = 2*ttail(e(df_r),abs(`t`i''))
	local n`i': di %7.0f e(N)
	
	if `se`i''>0 local se`i' = "[`se`i'']"
	else  local se`i' = "`se`i''"
	
	if  `p`i'' < 0.1 &  `p`i'' >= 0.05  local b`i' `b`i''\sym{*}
	else if  `p`i'' < 0.05 &  `p`i'' >= 0.01  local b`i' `b`i''\sym{**}
	else if  `p`i'' < 0.01  local b`i' `b`i''\sym{***}
	
	local b "`b' & `b`i''"
	local se "`se' & `se`i''"
	local m "`m' & `m`i''"
	local n "`n' & `n`i''"

}

local allcol = `n_col'+1		

*** no p-val panel
local b_nostar = subinstr("`b'", "*", "", .)
cap file close sumstat
file open sumstat using "$tables/el1_secondary_outcomes.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{`n_col'}{>{\centering\arraybackslash}m{3.2cm}}@{}}   \toprule \\" _n      // table header
file write sumstat " `header'   \\     " _n      
file write sumstat " `nos' \\    " _n    
file write sumstat "\midrule \addlinespace[10pt] " _n 
file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel A: Girls}}   \\  \addlinespace[3pt]   " _n         

file write sumstat "`b_nostar'  \\    " _n 
file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
file write sumstat "`m'  \\    " _n 
file write sumstat "`n' \\ \addlinespace[1cm]     " _n 

file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel B: Boys}}   \\  \addlinespace[3pt]   " _n      

cap file close sumstat

*** p-val panel included
cap file close sumstat
file open sumstat using "$tables/el1_secondary_outcomes_pvals.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{`n_col'}{>{\centering\arraybackslash}m{3.2cm}}@{}}   \toprule \\" _n      // table header
file write sumstat " `header'   \\     " _n      
file write sumstat " `nos' \\    " _n    
file write sumstat "\midrule \addlinespace[10pt] " _n 
file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel A: Girls}}   \\  \addlinespace[3pt]   " _n         

file write sumstat "`b'  \\    " _n 
file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
file write sumstat "`m'  \\    " _n 
file write sumstat "`n' \\ \addlinespace[1cm]     " _n 

file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel B: Boys}}   \\  \addlinespace[3pt]   " _n   

*** slide version
cap file close sumstat
file open sumstat using "$slides/el1_secondary_outcomes.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{`n_col'}{>{\centering\arraybackslash}m{3.2cm}}@{}}   \toprule \\" _n      // table header
file write sumstat " `header'   \\     " _n      
file write sumstat " `nos' \\    " _n    
file write sumstat "\midrule \addlinespace[5pt] " _n 
file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel A: Girls}}   \\  \addlinespace[3pt]   " _n         

file write sumstat "`b'  \\    " _n 
file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
file write sumstat "`m'  \\    " _n 
file write sumstat "`n' \\ \addlinespace[3pt]     " _n 

file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel B: Boys}}   \\  \addlinespace[3pt]   " _n         

*** boys

use "$finaldata", clear
keep if B_Sgirl == 0
local vars esteem_index2_girl discrimination_index2 goodbad_neg occupation_neg

local b "Treated"
local se
local n "Number of students"
local m "Control group mean"

local i=0
foreach index in `vars'   {

	local ++n_col // number of columns
	local i = `n_col'
	local nos "`nos' & (`n_col')"	

	if "`index'" == "esteem_index2_girl" {
		local b`i' "n/a"
		local se`i' "n/a"
		local m`i' "n/a"
		local n`i' "n/a"
	}

	else {
		if strpos("`index'", "discrimination")>0 {
			local varlab: variable label E_S`index'    
			reg E_S`index' B_treat district_? B_Sgrade6 ${el_``index''_flag} if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 0, cluster(Sschool_id) 
			* Control group mean
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
			local m`i': di %7.3fc r(mean)
		}

		else if strpos("`index'", "neg")>0 {
			local varlab: variable label E_D_measure_`index'
			reg E_D_measure_`index' B_treat B_D_measure_`index' B_D_measure_`index'_flag /* B_D_measure_`index'_m */ district_? B_Sgrade6 if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 0, cluster(Sschool_id) // only for girls
			* Control group mean
			su E_D_measure_`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
			local m`i': di %7.3fc r(mean)
		}

		local header "`header' & `varlab'" // column headers with variables labels 	

		* storing coefs		
		local b`i': di %7.3fc _b[B_treat]
		local se`i'= trim(string(_se[B_treat],"%7.3f"))
		local t`i' = _b[B_treat]/_se[B_treat]
		local p`i' = 2*ttail(e(df_r),abs(`t`i''))
		local n`i': di %7.0f e(N)
		
		if `se`i''>0 local se`i' = "[`se`i'']"
		else  local se`i' = "`se`i''"
		
		if  `p`i'' < 0.1 &  `p`i'' >= 0.05  local b`i' `b`i''\sym{*}
		else if  `p`i'' < 0.05 &  `p`i'' >= 0.01  local b`i' `b`i''\sym{**}
		else if  `p`i'' < 0.01  local b`i' `b`i''\sym{***}
	}
	
	local b "`b' & `b`i''"
	local se "`se' & `se`i''"
	local m "`m' & `m`i''"
	local n "`n' & `n`i''"


}	


*** no pval ***
local b_nostar = subinstr("`b'", "*", "", .)
cap file close sumstat
file open sumstat using "$tables/el1_secondary_outcomes.tex", write append

file write sumstat "`b_nostar'  \\    " _n 
file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
file write sumstat "`m'  \\    " _n 
file write sumstat "`n' \\    " _n 

file write sumstat "\bottomrule" _n              
file write sumstat "\end{tabular}" _n

*** slide version
cap file close sumstat
file open sumstat using "$slides/el1_secondary_outcomes.tex", write append

file write sumstat "`b'  \\    " _n 
file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
file write sumstat "`m'  \\    " _n 
file write sumstat "`n' \\   " _n 

file write sumstat "\bottomrule" _n              
file write sumstat "\end{tabular}" _n

*** with p values
cap file close sumstat
file open sumstat using "$tables/el1_secondary_outcomes_pvals.tex", write append

file write sumstat "`b'  \\    " _n 
file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
file write sumstat "`m'  \\    " _n 
file write sumstat "`n' \\ \addlinespace[1cm]     " _n 

file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel C: P-vals, Boys vs. Girls}}   \\  \addlinespace[3pt]   " _n   
local b "Treated"
local i=0

use "$finaldata", clear
local vars esteem_index2_girl discrimination_index2 goodbad_neg occupation_neg

foreach index in `vars' {
	local ++i
	local nos "`nos' & (`n_col')"

	if "`index'" == "esteem_index2_girl" {
		local b`i' "n/a"
	}

	else {

		if strpos("`index'", "discrimination")>0 {
			local controls B_treat ${el_``index''_flag}
			local gX 
			local j = 2 
			foreach var1 of varlist `controls'{
				local varname gX`var1'
				local varname = substr("`varname'", 1, 30)
				gen `varname' = B_Sgirl * `var1'
				local gX `gX' gX`var1'
				local varlab: var la `var1'
				if "`varlab'" == "" local varlab `var1'
				putexcel D`j' = ("FemaleX`varlab'")
				local ++j
			}   
			qui log on 
			di "Column: `index'"
			reg E_S`index' B_treat district_gender_* gender_grade_* ${el_``index''_flag} gX* if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) 
			qui log off 
			local b`i': di %7.3f 2*ttail(e(df_r), abs(_b[gXB_treat]/_se[gXB_treat]))
			drop gX*
		}

		else if strpos("`index'", "neg")>0 {
			local controls B_treat B_D_measure_`index' B_D_measure_`index'_flag /* B_D_measure_`index'_m */  
			local gX 
			local j = 2 
			foreach var1 of varlist `controls'{
				local varname gX`var1'
				local varname = substr("`varname'", 1, 30)
				gen `varname' = B_Sgirl * `var1'
				local gX `gX' gX`var1'
				local varlab: var la `var1'
				if "`varlab'" == "" local varlab `var1'
				putexcel D`j' = ("FemaleX`varlab'")
				local ++j
			}   
			qui log on 
			di "Column: `index'"
			reg E_D_measure_`index' B_treat B_D_measure_`index' B_D_measure_`index'_flag /* B_D_measure_`index'_m */ district_gender_* gender_grade_* gX* if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) 
			qui log off 
			local b`i': di %7.3f 2*ttail(e(df_r), abs(_b[gXB_treat]/_se[gXB_treat]))
			drop gX*
		}

	}

	local b "`b' & `b`i''"
}

file write sumstat "`b'  \\    " _n 
file write sumstat "\bottomrule" _n              
file write sumstat "\end{tabular}" _n
file close sumstat	

//copy "$tables/el1_secondary_outcomes.tex" "$slides/el1_secondary_outcomes.tex", replace

log close 
*/

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
* TABLE 1.8 & 1.10: EL2 Primary Outcomes (combined and by gender)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

putexcel E1 = ("Table 1.10 (Behavior index)")

eststo clear	
local header
local header_g
local header_b	
local n_col = 0
local n_col_g = 0
local n_col_b = 0

log using "$ad_hoc/tab1.10_el2_primary.smcl", replace 
qui log off 

foreach index in gender_index2 gender_index_factor aspiration_index2 aspiration_index_factor behavior_index2  behavior_index_factor {

	local varlab: variable label E2_S`index'
	local header "`header' & `varlab'" // column headers with variables labels 
	local ++n_col // number of columns

	*** girls columns
	local header_g "`header_g' & `varlab'" // girls column headers 
	local ++n_col_g 	

	if "`index'"=="petition_index2" | "`index'"=="scholar_index2"  {

		***p-value calculation
		local controls B_treat
		foreach var of varlist `controls'{
			local varname gX`var'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_Sgirl * `var'
		}
		qui log on 
		di "Column: `index'"
		reg E2_S`index' B_treat district_gender_* gender_grade_* gX* if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
		qui log off 
		local `index'_p: di %7.3f 2*ttail(e(df_r), abs(_b[gXB_treat]/_se[gXB_treat]))
		drop gX*

		*** combined regression
		eststo bs_`index' : reg E2_S`index' B_treat district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id) // no baseline index or flags
		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
		estadd scalar ctrlmean=r(mean)
		estadd local cluster = `e(N_clust)'		
		estadd local basic "Yes"

		if "`index'"=="petition_index2"{
			local stat = 100*`r(mean)'

			local el2_pet_mean = `r(mean)'
			
			local stat: di %7.4fc `stat'			
			cap file write paperstat "4.3Medium-run results pg19, EL2 control group % signed petition, `stat'%   " _n 	

			reg E2_S`index' B_treat district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id) // no baseline index or flags
			local t = _b[B_treat]/_se[B_treat]
			local p = 2*ttail(e(df_r),abs(`t'))
			local stat: di %7.5fc `p'			
			cap file write paperstat "Medium-run results, EL2 signed petition pval, `stat'   " _n 
		}

		else if "`index'"=="scholar_index2"{

			reg E2_S`index' B_treat district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1, cluster(Sschool_id) // no baseline index or flags
			local pp = 100*_b[B_treat]
			local stat = 100*_b[B_treat]

			local stat: di %7.4fc `stat'			
			cap file write paperstat "4.3Medium-run results pg21, EL2 scholarship applied pp increase, `stat'pp   " _n 
			
			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl == 1
			local schmean = 100*`r(mean)'
			local stat =  100*(`pp'/`schmean')
			local stat: di %7.4fc `stat'			
			cap file write paperstat "4.3Medium-run results pg21, EL2 scholarship applied %, `stat'%   " _n 

			reg E2_S`index' B_treat district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1, cluster(Sschool_id) // no baseline index or flags
			local t = _b[B_treat]/_se[B_treat]
			local p = 2*ttail(e(df_r),abs(`t'))
			local stat: di %7.5fc `p'			
			cap file write paperstat "4.3Medium-run results pg21, EL2 applied to scholarship pval, `stat'   " _n 
		}

		*** girls regression
		eststo gbs_`index' : reg E2_S`index' B_treat district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1, cluster(Sschool_id) // no baseline index or flags
		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl == 1
		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean) 
		estadd local basic "Yes"
		estadd local p_val ``index'_p'
		
		if "`index'"=="petition_index2"{

			*** boys columns
			local header_b "`header_b' & `varlab'" // boys column headers 
			local ++n_col_b

			*** boys regression
			eststo bbs_`index' : reg E2_S`index' B_treat district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==0, cluster(Sschool_id) // no baseline index or flags
			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl == 0
			if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
			else estadd scalar ctrlmean=r(mean) 
			estadd local basic "Yes" 

			
		}

	}


	else if "`index'"=="aspiration_index2" | "`index'"=="aspiration_index_factor"{

		*** combined regression
		eststo bs_`index' : reg E2_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el2_``index''_flag}  if B_Sgirl==1 & !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) // only for girls
		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & B_Sgirl==1 & attrition==0
		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean)
		estadd local cluster = `e(N_clust)'		
		estadd local basic "Yes"

		*** girls regression
		eststo gbs_`index' : reg E2_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el2_``index''_flag}  if B_Sgirl==1 & !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) // only for girls
		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl == 1
		if abs(r(mean)) < .00005 estadd scalar ctrlmean_g=abs(r(mean)) 
		else estadd scalar ctrlmean_g=r(mean) 
		estadd local basic_g "Yes"
	}

	else {

		***p-value calculation
		local controls B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el2_``index''_flag} 
		local gX
		local i = 2
		foreach var of varlist `controls'{
			local varname gX`var'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_Sgirl * `var'
			local varlab: var la `var'
			if "`varlab'" == "" local varlab `var'
			local gX `gX' gX`var'
			putexcel E`i' = ("FemaleX`varlab'")
			local ++i 
		}

		qui log on 
		di "Column: `index'"
		reg E2_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} gX*  if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
		qui log off 
		local `index'_p: di %7.3f 2*ttail(e(df_r), abs(_b[gXB_treat]/_se[gXB_treat]))
		drop gX*

		*** boys columns
		local header_b "`header_b' & `varlab'" // boys column headers 
		local ++n_col_b 

		*** combined regression
		eststo bs_`index' : reg E2_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
		estadd scalar ctrlmean=r(mean) 
		estadd local cluster = `e(N_clust)'		
		estadd local basic "Yes"

		if "`index'"=="gender_index2" | "`index'"=="gender_index_factor" {
			local el2_att = _b[B_treat]
			local stat: di %7.4fc _b[B_treat]
			cap file write paperstat "4.3Medium-run results pg20, EL2 gender attitudes SD, `stat'   " _n 

			local stat: di %7.4fc `r(mean)'
			cap file write paperstat "4.3Medium-run results pg20, EL2 gender attitudes control mean, `stat'   " _n 

		}

		else if "`index'"=="behavior_index2"  | "`index'"=="behavior_index_factor"{
			local stat: di %7.4fc _b[B_treat]
			cap file write paperstat "4.3Medium-run results pg21, EL2 behavior attitudes SD, `stat'   " _n 
		}

		*** girls regression
		eststo gbs_`index' : reg E2_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1, cluster(Sschool_id)
		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl == 1
		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean) 
		estadd local basic "Yes"
		estadd local p_val ``index'_p'
		
		*** boys regression
		eststo bbs_`index' : reg E2_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==0, cluster(Sschool_id)
		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl == 0
		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean) 
		estadd local basic "Yes" 
		
	}
}


***combined results***

local prehead = subinstr("`frame'", "colno", "`n_col'", .)

#delimit ;

esttab bs_* using "$tables/el2_primary_outcomes_basic_combined.tex", 
b(3) se(3) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment nostar
prehead(" `prehead'" "`header' \\")
keep(B_treat) order(B_treat)
stats(ctrlmean basic N,   
labels("Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %20s  %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}");

esttab bs_* using "$tables/el2_primary_outcomes_basic_combined_star.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "`header' \\")
keep(B_treat) order(B_treat)
stats(ctrlmean basic N,   
labels("Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %20s  %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr
log close 


/*
***panel results***

local frame1 = subinstr("`frame'", "2.3cm", "2.5cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "6", .)
local header_c " & Girls & Boys & Girls & Boys & Girls & Boys " // new style for table
local multi_c " & \multicolumn{2}{c}{Gender attitudes index} & \multicolumn{2}{c}{Self-reported behavior index} & \multicolumn{2}{c}{Signed petition} \\ 	\cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7}"

#delimit ;

esttab gbs_gender_index2 bbs_gender_index2 gbs_behavior_index2 bbs_behavior_index2 gbs_petition_index2 bbs_petition_index2
using "$tables/el2_primary_outcomes_basic.tex", 
b(3) se(3) nonotes nomtitles number brackets nostar
replace gaps label booktabs style(tex) fragment
prehead(" `prehead'" "`multi_c'" "`header_c' \\")
keep(B_treat) order(B_treat)
stats(ctrlmean basic N,   
labels("Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %7.3fc %20s %7.0fc))
postfoot(" p-value: Girls=Boys & \multicolumn{2}{c}{`gender_index2_p'} & \multicolumn{2}{c}{`behavior_index2_p'} & \multicolumn{2}{c}{`petition_index2_p'} \\ " "\bottomrule" "\end{tabular}" "}");

esttab gbs_gender_index2 bbs_gender_index2 gbs_behavior_index2 bbs_behavior_index2 gbs_petition_index2 bbs_petition_index2
using "$tables/el2_primary_outcomes_basic_star.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment
prehead(" `prehead'" "`multi_c'" "`header_c' \\")
keep(B_treat) order(B_treat)
stats(ctrlmean basic N,   
labels("Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %7.3fc %20s %7.0fc))
postfoot(" p-value: Girls=Boys & \multicolumn{2}{c}{`gender_index2_p'} & \multicolumn{2}{c}{`behavior_index2_p'} & \multicolumn{2}{c}{`petition_index2_p'} \\ " "\bottomrule" "\end{tabular}" "}");

#delimit cr 

** paperstat petition and gender attitudes

local petsd = sqrt(`el2_pet_mean' * (1-`el2_pet_mean'))
di `petsd'
local stat =  (`petsd' * `el2_att')
di `stat'
local stat = (`stat'/`el2_pet_mean')*100
di `stat'
local stat: di %7.4fc `stat'
cap file write paperstat "4.3Medium-run results pg19, EL2 % increase in likelihood of signing petition given effect size of self-reported attitudes, `stat'%   " _n 

*/

/*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	TABLE 1.9 & Appendix TABLE 1.24: EL2 Robustness check for social desirability bias 
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

log using "$ad_hoc/tab1.9_el2_mainsds.smcl", replace 
qui log off

foreach gender in girl boy both {

	use "$finaldata", clear

	if "`gender'" == "girl" {
		keep if B_Sgirl == 1
		local indices gender_index2 aspiration_index2 behavior_index2 scholar_index2 petition_index2
	}
	else if "`gender'" == "boy" {
		keep if B_Sgirl == 0
		local indices gender_index2 behavior_index2 petition_index2
	}
	else if "`gender'" == "both" {
		local indices gender_index2 aspiration_index2 behavior_index2 scholar_index2 petition_index2
	}

	gen lowsd_std =  B_Ssocial_scale_belowm
	la var lowsd_std "Low social desirability score"

	gen treat_lowsd_std = B_treat*B_Ssocial_scale_belowm
	la var treat_lowsd_std "Treated $\times$ Low social desirability score"

	*** high social desirability scale
	summ B_Ssocial_scale, detail
	gen highsd_std=1 if B_Ssocial_scale>`r(p50)'
	replace highsd_std=0 if B_Ssocial_scale<=`r(p50)' //EDIT: Originally ">=" on this line was ">", so 3,075 obs that == median were missing. 
	la var highsd_std "High social desirability score"
	gen treat_highsd_std = B_treat*highsd_std
	la var treat_highsd_std "Treated $\times$ High social desirability score"

	foreach sds in /*lowsd_std*/ highsd_std  {

		eststo clear	
		local header	
		local n_col = 0
		foreach index in `indices' {

			local varlab: variable label E2_S`index'
			local header "`header' & `varlab'" // column headers with variables labels 

			local ++n_col // number of columns

			if "`index'"=="petition_index2" | "`index'"=="scholar_index2"  {
				if "`index'" == "petition_index2" {
					//qui log on
					di "`gender'"
					eststo bs_`index' : reg E2_S`index' B_treat `sds' treat_`sds' district_gender_* gender_grade_* , cluster(Sschool_id) // no baseline index or flags
					test B_treat+treat_`sds'==0
					estadd scalar pval=`r(p)'
					//qui log off
				}
				else {
					eststo bs_`index' : reg E2_S`index' B_treat `sds' treat_`sds' district_gender_* gender_grade_* , cluster(Sschool_id) // no baseline index or flags
					test B_treat+treat_`sds'==0
					estadd scalar pval=`r(p)'
				}
				su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & !mi(B_Ssocial_scale_intbelmed)
				estadd scalar ctrlmean=r(mean) 
			}

			else if "`index'"=="aspiration_index2" {
				eststo bs_`index' : reg E2_S`index' B_treat `sds' treat_`sds' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el2_``index''_flag}  if B_Sgirl==1 & !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) // only for girls
				test B_treat+treat_`sds'==0
				estadd scalar pval=`r(p)'
				su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1 & !mi(`sds')
				if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
				else estadd scalar ctrlmean=r(mean)
			}

			else {
				eststo bs_`index' : reg E2_S`index' B_treat `sds' treat_`sds' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag}  if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id)
				test B_treat+treat_`sds'==0
				estadd scalar pval=`r(p)'
				su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & !mi(`sds')
				if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
				else estadd scalar ctrlmean=r(mean)
			}

			estadd local cluster = `e(N_clust)'		
			estadd local basic "Yes"

			if "`gender'" == "both" {
				if "`index'" == "petition_index2" | "`index'" == "scholar_index2" {
					local controls B_treat `sds' treat_`sds'
				}
				else {
					local controls B_treat `sds' treat_`sds' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el2_``index''_flag}
				}
				local gX 
				foreach var of varlist `controls' {
					local varname gX`var'
					local varname = substr("`varname'", 1, 30)
					gen `varname' = B_Sgirl * `var'
					local gX `gX' gX`var'
				}

				qui log on 
				di "Column: `index'"
				reg E2_S`index' `controls' gX* if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id)
				qui log off 
				local p1_`index': di %7.3f 2*ttail(e(df_r), abs(_b[gXB_treat]/_se[gXB_treat]))
				local p2_`index': di %7.3f 2*ttail(e(df_r), abs(_b[gX`sds']/_se[gX`sds']))
				local p3_`index': di %7.3f 2*ttail(e(df_r), abs(_b[gXtreat_`sds']/_se[gXtreat_`sds']))

				drop gX*
			}
		}

		if "`gender'" == "girl" {

			local prehead = subinstr("`frame'", "colno", "`n_col'", .)

			#delimit ;

			esttab bs_* using "$tables/el2_primary_outcomes_`sds'.tex", 
			b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
			replace gaps label booktabs style(tex) fragment 
			prehead(" \begin{flushleft} Panel A: Girls \end{flushleft} \vspace{3mm} `prehead'" "`header' \\")
			keep(B_treat `sds' treat_`sds') order(B_treat `sds' treat_`sds')
			stats(pval ctrlmean basic N,   
			labels("p-val: Treated + Treated $\times$ High SD = 0" "Control group mean" "Basic controls" "Number of students") 
			fmt(%7.3fc %7.3fc %20s %7.0fc))
			postfoot("\bottomrule" "\end{tabular}" "}"); 

			#delimit cr
		}

		else if "`gender'" == "boy" {

			local frame1 = subinstr("`frame'", "2.3cm", "3.3cm", .) // column spacing	
			local prehead = subinstr("`frame1'", "colno", "`n_col'", .)

			#delimit ;

			esttab bs_* using "$tables/el2_primary_outcomes_`sds'.tex", 
			b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
			append gaps label booktabs style(tex) fragment 
			prehead("\vspace{1cm} \begin{flushleft} Panel B: Boys \end{flushleft} `prehead'" "`header' \\")
			keep(B_treat `sds' treat_`sds') order(B_treat `sds' treat_`sds')
			stats(pval ctrlmean basic N,   
			labels("p-val: Treated + Treated $\times$ High SD = 0" "Control group mean" "Basic controls" "Number of students") 
			fmt(%7.3fc %7.3fc %20s %7.0fc))
			postfoot("\bottomrule" "\end{tabular}" "}"); 

			#delimit cr
		}

		else if "`gender'" == "both" {

			local prehead = subinstr("`frame'", "colno", "`n_col'", .)

			#delimit ;

			esttab bs_* using "$tables/el2_primary_outcomes_`sds'_combined.tex", 
			b(3) se(3) nonotes nomtitles number brackets nostar 
			replace gaps label booktabs style(tex) fragment 
			prehead("`prehead'" "`header' \\")
			keep(B_treat `sds' treat_`sds') order(B_treat `sds' treat_`sds')
			stats(pval ctrlmean basic N,   
			labels("p-val: Treated + Treated $\times$ High SD = 0" "Control group mean" "Basic controls" "Number of students") 
			fmt(%7.3fc %7.3fc %20s %7.0fc))
			postfoot("\bottomrule" "\end{tabular}" "}"); 

			esttab bs_* using "$tables/el2_primary_outcomes_`sds'_combined_star.tex", 
			b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
			replace gaps label booktabs style(tex) fragment 
			prehead("`prehead'" "`header' \\")
			keep(B_treat `sds' treat_`sds') order(B_treat `sds' treat_`sds')
			stats(pval ctrlmean basic N,   
			labels("p-val: Treated + Treated $\times$ High SD = 0" "Control group mean" "Basic controls" "Number of students") 
			fmt(%7.3fc %7.3fc %20s %7.0fc))
			postfoot("\bottomrule" "\end{tabular}" "}"); 

			#delimit cr
		}

	}

}

log close

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	TABLE 1.11: Scholarship on BL vars
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

summ B_Saspiration_index2 if B_Sgirl==1, detail
local median= `r(p50)'
gen B_Saspiration_index2_abm=1 if B_Saspiration_index2>`median' & !mi(B_Saspiration_index2)  & B_Sgirl==1
replace B_Saspiration_index2_abm=0 if B_Saspiration_index2<=`median' & !mi(B_Saspiration_index2)  & B_Sgirl==1


la var B_Saspiration_index2 "BL aspirations index"
la var B_Saspiration_index2_abm "Above-median BL aspirations"
la var B_Sdisc_edu_goals "Has discussed educ goals with parent"

local schvars B_Saspiration_index2 B_Saspiration_index2_abm B_Sdisc_edu_goals 

** imputation and flags

foreach y in `schvars' {
	cap confirm variable `y'_flag
	if !_rc{
		replace `y'=. if `y'_flag
		drop `y'_flag
	}
	qui count if mi(`y')
	if r(N)!=0{
		gen `y'_flag=mi(`y')
		bysort B_Sgirl B_Sdistrict: egen x = mean (`y') 
		qui replace `y'=x if `y'_flag==1
		drop x

	}	
}

* interaction vars
local tschvars
foreach var in `schvars' {
	local l: var lab `var'
	gen treat_`var' = B_treat*`var'
	la var treat_`var'  "Treated $\times$ `l'"		
	local tschvars `tschvars' treat_`var' 
}

eststo clear	
local header ""	
local n_col = 0
foreach var in `schvars' {

	local treatrow B_treat `var' treat_`var'

	local ++n_col // number of columns		

	if `n_col'<=3 {
		eststo bs_`n_col' : reg E2_Sscholar_index2 `treatrow' B_Sgrade6 district_? `var'_flag if B_Sgirl==1 & !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)

		if `n_col'==2 { // above median BL aspirations
		local stat = (_b[B_treat] + _b[treat_B_Saspiration_index2_abm]) / _b[B_treat] 
		local stat: di  %7.4f `stat'
		cap file write paperstat "4.3 Medium-run results - 4.3.1 Effects on primary outcomes, magnitude by which total effect for above-median aspirations higher, `stat'" _n
	}

}
	
	su E2_Sscholar_index2 if B_treat==0 & B_Sgirl==1 & !mi(E2_Steam_id) & attrition==0
	estadd scalar ctrlmean=r(mean) 
	estadd local basic "Yes"

	if "`var'"=="B_Saspiration_index2_abm" {
		test B_treat+treat_B_Saspiration_index2_abm==0
		estadd scalar pval_asp=`r(p)'
	}

	if "`var'"=="B_Sdisc_edu_goals" {
		test B_treat+treat_B_Sdisc_edu_goals==0
		estadd scalar pval_disc=`r(p)'
	}

}

local prehead = subinstr("`frame'", "colno", "`n_col'", .)

#delimit ;

esttab bs_* using "$tables/el2_scholar_blvars_int.tex", 
b(3) se(3) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment nostar
prehead(" `prehead'" "& \multicolumn{3}{c}{Applied to scholarship} \\  \cmidrule(lr){2-4} ")
keep(B_treat treat_B_Saspiration_index2  treat_B_Saspiration_index2_abm treat_B_Sdisc_edu_goals) 
order(B_treat treat_B_Saspiration_index2  treat_B_Saspiration_index2_abm treat_B_Sdisc_edu_goals)
stats(pval_asp pval_disc ctrlmean N,   
labels("p-value: Treated + Treated $\times$ Above-median aspir. = 0" "p-value: Treated + Treated $\times$ Has discussed goals = 0" "Control group mean" "Number of students") 
fmt(%7.3fc %7.3fc  %7.3fc %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

esttab bs_* using "$tables/el2_scholar_blvars_int_star.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "& \multicolumn{3}{c}{Applied to scholarship} \\  \cmidrule(lr){2-4} ")
keep(B_treat treat_B_Saspiration_index2  treat_B_Saspiration_index2_abm treat_B_Sdisc_edu_goals) 
order(B_treat treat_B_Saspiration_index2  treat_B_Saspiration_index2_abm treat_B_Sdisc_edu_goals)
stats(pval_asp pval_disc ctrlmean N,   
labels("p-value: Treated + Treated $\times$ Above-median aspir. = 0" "p-value: Treated + Treated $\times$ Has discussed goals = 0" "Control group mean" "Number of students") 
fmt(%7.3fc %7.3fc  %7.3fc %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr

* paperstats
su B_Sdisc_edu_goals if B_Sgirl==1 & !mi(E2_Steam_id) & attrition==0
local stat = 100* `r(mean)'
local stat: di  %7.4f `stat'
cap file write paperstat "4.3 Medium-run results - 4.3.1 Effects on primary outcomes, BL % girls talked to parents about educ goals , `stat'%" _n

pwcorr  B_Saspiration_index2 B_Sdisc_edu_goals if B_Sgirl==1 & !mi(E2_Steam_id) & attrition==0
local stat: di  %7.4f `r(rho)'
cap file write paperstat "4.3 Medium-run results - 4.3.1 Effects on primary outcomes, Correlation coef. BL aspirations index and BL discussing educ goals with parents , `stat'" _n



* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	TABLE 1.10a: Slide Version of Table 10
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
local n_col = `n_col'
local prehead = subinstr("`frame'", "colno", "`n_col'", .)
#delimit ;

esttab bs_1 bs_2 bs_3 using "$slides/el2_scholar_blvars_int.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "& \multicolumn{3}{c}{Applied to scholarship} \\  \cmidrule(lr){2-4} ")
keep(B_treat treat_B_Saspiration_index2 treat_B_Saspiration_index2_abm treat_B_Sdisc_edu_goals) 
order(B_treat treat_B_Saspiration_index2 treat_B_Saspiration_index2_abm treat_B_Sdisc_edu_goals)
stats(pval_asp pval_disc ctrlmean N,   
labels("Treated + Treated $\times$ Above-median aspir. = 0" "Treated + Treated $\times$ Has discussed goals = 0" "Control group mean" "Number of students") 
fmt(%7.3fc %7.3fc  %7.3fc %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	REMOVED (EL2 Primary Outcomes (heterogeneity by gender))
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
use "$finaldata", clear

rename treat_girl treat_Sgirl
la var treat_Sgirl "Treated $\times$ Female"	

local treatrow B_treat B_Sgirl treat_Sgirl
local treatrowdi B_treat treat_Sgirl

putexcel F1 = ("Table 1.9")
putexcel F2 = ("FemaleXTreated")

eststo clear	
local header	
local n_col = 0
foreach index in gender_index2 behavior_index2 petition_index2  {

	local varlab: variable label E2_S`index'
	local header "`header' & `varlab'" // column headers with variables labels 

	local ++n_col // number of columns

	if "`index'"=="petition_index2" {
		local cntrls district_gender_* gender_grade_* 
		foreach var of varlist `cntrls' {
			local varname gX`var'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_Sgirl * `var'
		}
		eststo bs_`index' : reg E2_S`index' `treatrow' district_gender_* gender_grade_* gX* if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id) // no baseline index or flags
		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
		estadd scalar ctrlmean=r(mean) 
		drop gX*
	}


	else {
		local cntrls B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag}
		local i = 3
		foreach var of varlist `cntrls' {
			local varname gX`var'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_Sgirl * `var'
			local varlab: var la `var'
			if "`varlab'" == "" local varlab `var'
			putexcel F`i' = ("FemaleX`varlab'")
			local ++i
		}
		eststo bs_`index' : reg E2_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} gX* if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
		drop gX*
		if "`index'"=="gender_index2" {
			local stat: di %7.4fc _b[B_treat]
			cap file write paperstat "4.3Medium-run results pg20, EL2 gender attitudes (boys) SD, `stat'   " _n 

			local el2_att_girl = _b[B_treat] + _b[treat_Sgirl]
			local stat = _b[B_treat] + _b[treat_Sgirl]
			local stat: di %7.4fc `stat'
			cap file write paperstat "4.3Medium-run results pg20, EL2 gender attitudes (girls) SD, `stat'   " _n 


			cap local stat = (`el2_att_girl'/`el1_att_girl')*100
			cap local stat: di %7.4fc `stat'
			cap file write paperstat "4.3Medium-run results - 4.3.2 Heterogenous effects by gender pg20, EL2 gender attitudes (girls) as % of short-run effect on girls, `stat'%  " _n 


		}


		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
		estadd scalar ctrlmean=r(mean) 
	}

	estadd local cluster = `e(N_clust)'		
	estadd local basic "Yes"

	test B_treat+treat_Sgirl==0
	estadd scalar pval=`r(p)'


}

local frame1 = subinstr("`frame'", "2.3cm", "3.3cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col'", .)


#delimit ;

esttab bs_* using "$tables/el2_primary_outcomes_hetgen.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment  
prehead(" `prehead'" "`header' \\")
keep(`treatrowdi') order(`treatrowdi')
stats(pval ctrlmean basic N,   
labels("p-value: Treated + Treated $\times$ Female = 0" "Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %7.3fc %20s  %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

esttab bs_* using "$tables/el2_primary_outcomes_hetgen.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "`header' \\")
keep(`treatrowdi') order(`treatrowdi')
stats(pval ctrlmean basic N,   
labels("p-value: Treated + Treated $\times$ Female = 0" "Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %7.3fc %20s  %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	Table 1.13: Endline 2 Secondary Outcomes
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

eststo clear	
local header	
local n_col = 0

* indicator for list randomization regression which needs to be done only at school-grade level
sort Sschool_id B_Sgrade6, stable
by Sschool_id B_Sgrade6: gen uniq_scl_grade = _n==1 

* get max attrtion by school-grade
by Sschool_id B_Sgrade6: egen att_max = min(attrition) // if 1 then should be dropped. 

* filling all school-grade missing boys' harassment values for regression
egen E2_Slr = max(E2_Slr_harass_b_sch_grad), by(Sschool_id B_Sgrade6)
local l : var lab E2_Slr_harass_b_sch_grad
la var E2_Slr "`l'"
order E2_Slr, after(E2_Slr_harass_b_sch_grad)

* generating boys and girls mar_fert_asp_index2
gen E2_Smar_fert_asp_index2_g = E2_Smar_fert_asp_index2 if B_Sgirl == 1
la var E2_Smar_fert_asp_index2_g "Marriage and fertility aspirations (Girls)"
gen E2_Smar_fert_asp_index2_b = E2_Smar_fert_asp_index2 if B_Sgirl == 0
la var E2_Smar_fert_asp_index2_b "Marriage and fertility aspirations (Boys)"

foreach index in esteem_index2_girl educ_attain_index2_g mar_fert_asp_index2_g mar_fert_asp_index2_b harassed_index2_g lr  {		

	local varlab: variable label E2_S`index'
	local header "`header' & `varlab'" // column headers with variables labels 

	local ++n_col // number of columns	

		* has baseline controls
		if strpos("`index'", "esteem")>0 {
			eststo bs_`index' : reg E2_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag}  if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1 , cluster(Sschool_id)

			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1
			estadd scalar ctrlmean=abs(r(mean))
		}
		
		* only at school-grade level
		if strpos("`index'", "lr")>0 {
			eststo bs_`index' : reg E2_S`index' B_treat B_Sgrade6 district_?  if  att_max==0 & uniq_scl_grade==1, cluster(Sschool_id) // only for boys
			su E2_S`index' if B_treat==0 & att_max==0 & uniq_scl_grade==1
			estadd scalar ctrlmean=r(mean)
		}
		
		* no baseline controls
		else  {
			eststo bs_`index' : reg E2_S`index' B_treat district_gender_* gender_grade_* ${el2_``index''_flag} if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)

			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 
			estadd scalar ctrlmean=abs(r(mean))
			
			if "`index'"=="educ_attain_index2_g"  {
				local stat: di %7.4fc _b[B_treat]
				cap file write paperstat "4.3Medium-run results pg21, EL2 secondary outcomes: girls' education SD, `stat'" _n	
			}

			else if "`index'"=="mar_fert_asp_index2"  {
				local stat: di %7.4fc _b[B_treat]
				cap file write paperstat "4.3Medium-run results pg21, EL2 secondary outcomes: girls' marriage and fertility aspirations SD, `stat'" _n	
			}
		}

		estadd local cluster = `e(N_clust)'		
		estadd local basic "Yes"		
		
	}

	local frame1 = subinstr("`frame'", "2.3cm", "2.1cm", .) // column spacing	
	local prehead = subinstr("`frame1'", "colno", "`n_col'", .)

	#delimit ;
	
	esttab bs_* using "$tables/el2_secondary_outcomes.tex", 
	b(3) se(3) nonotes nomtitles number brackets 
	replace gaps label booktabs style(tex) fragment nostar 
	prehead(" `prehead'" "`header' \\")
	keep(B_treat) order(B_treat)
	stats(ctrlmean basic  N,   
	labels("Control group mean" "Basic controls" "Number of observations") 
	fmt(%7.3fc %20s %7.0fc))
	postfoot("\bottomrule" "\end{tabular}" "}"); 

	esttab bs_* using "$tables/el2_secondary_outcomes_star.tex", 
	b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
	replace gaps label booktabs style(tex) fragment 
	prehead(" `prehead'" "`header' \\")
	keep(B_treat) order(B_treat)
	stats(ctrlmean basic  N,   
	labels("Control group mean" "Basic controls" "Number of observations") 
	fmt(%7.3fc %20s %7.0fc))
	postfoot("\bottomrule" "\end{tabular}" "}"); 

	#delimit cr

	drop uniq_scl_grade att_max
	

}

***************************************************************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************************************************************	
*		Part 3: Appendix tables (appendix table 1.1-1.29)
***************************************************************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************************************************************

*NOTE THAT THESE MIGHT BE OUT OF ORDER BECAUSE WE'RE REORGANIZING PAPER

if `c3' == 1 {

** appendix tables - regressions

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  APPENDIX TABLE 1.1:  Descriptive statistics: student characteristics at baseline (by gender)
*  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

label var B_mom_age "Mother's age"
label var B_dad_age "Father's age"
label var B_m_illiterate "Mother is illiterate"
label var B_f_illiterate "Father is illiterate"
label var B_m_secondary "Mother finished grade 8"
label var B_f_secondary "Father finished grade 8"
label var B_Sgirl "Female"
label var B_Sgrade6 "Enrolled in grade 6"
label var B_Sgrade7 "Enrolled in grade 7"	


foreach var in B_Sgirl B_Sgrade6   {
	gen `var'_flag = mi(`var')
}

gen B_Ssocial_scale_flag = mi(B_Ssocial_scale)
gen B_Ssocial_scale_belowm_flag = mi(B_Ssocial_scale_belowm) //making below loop work
gen B_Ssocial_scale_int_imp_flag = mi(B_Ssocial_scale_int_imp) //making below loop work
*** high social desirability scale
summ B_Ssocial_scale, detail
gen B_Shighsd_std=1 if B_Ssocial_scale>`r(p50)'
replace B_Shighsd_std=0 if B_Ssocial_scale<=`r(p50)' //EDIT: Originally ">=" on this line was ">", so 3,075 obs that == median were missing. 
la var B_Shighsd_std "High social desirability score"
gen B_Shighsd_std_flag = mi(B_Shighsd_std)

gen all_flag = 0 // 0 if all variables in the list missing
foreach var in Sage Sgirl Shindu Sgrade6 Scaste_sc mom_age dad_age m_illiterate m_fulltime Sflush_toilet Sgender_index2 Saspiration_index2 Sbehavior_index2 ///
Ssocial_scale Ssocial_scale_int_imp {     
	replace all_flag = 1 if B_`var'_flag==0 
}


file open sumstat using "$tables/alt_el2_tab_descriptive_school_students.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\extracolsep{3pt}}p{4cm}*{6}{>{\centering\arraybackslash}m{2cm}}@{}}   \toprule" _n      // table header
file write sumstat 	"& \multicolumn{3}{c}{Girls} & \multicolumn{3}{c}{Boys} \\ 	\cmidrule(lr){2-4} \cmidrule(lr){5-7} " _n
file write sumstat " Variable & Treat & Control & Standardized diff & Treat & Control & Standardized diff \\ " _n         
file write sumstat "\midrule" _n 
count if B_Sgirl == 1 & Sschool_id!=2711 & child_id!=3205037 & B_treat==1
local girl_t = `r(N)'
count if B_Sgirl == 1 & Sschool_id!=2711 & child_id!=3205037 & B_treat==0
local girl_c = `r(N)'
count if B_Sgirl == 0 & Sschool_id!=2711 & child_id!=3205037 & B_treat==1
local boy_t = `r(N)'
count if B_Sgirl == 0 & Sschool_id!=2711 & child_id!=3205037 & B_treat==0
local boy_c = `r(N)'

file write sumstat "Number of students & `girl_t' & `girl_c' & & `boy_t' & `boy_c' &   \\    \addlinespace[6pt]   " _n 

foreach var in Sage Shindu Sgrade6 Scaste_sc mom_age dad_age m_illiterate m_fulltime Sflush_toilet Sgender_index2 ///
Saspiration_index2 Sbehavior_index2 Ssocial_scale Shighsd_std  {     
	
	if inlist("`var'", "Sgender_index2", "Saspiration_index2", "Sbehavior_index2") local varlab : variable label E_`var' // labels for baseline and endline indices are not the same, choosing endline because it simply says 'Gender Attitude Index'
	else local varlab: variable label B_`var'

	sum B_`var' if B_Sgirl == 1 & Sschool_id!=2711 & child_id!=3205037 & B_`var'_flag==0
	local totalmean_g = `r(mean)'	
	local totalsd_g = `r(sd)'
	
	sum B_`var' if B_Sgirl==1 & Sschool_id!=2711 & child_id!=3205037 & B_`var'_flag==0 & B_treat == 1
	local treatmean_g = `r(mean)'
	local treatmean_g: di %7.3f `r(mean)'	
	local treatsd_g: di %7.3f `r(sd)'

	sum B_`var' if B_Sgirl==1 & Sschool_id!=2711 & child_id!=3205037 & B_`var'_flag==0 & B_treat == 0
	local ctrlmean_g = `r(mean)'
	local ctrlmean_g: di %7.3f `r(mean)'	
	local ctrlsd_g: di %7.3f `r(sd)'

	gen stand_diff_g = (`treatmean_g'-`ctrlmean_g')/ `totalsd_g'
	sum stand_diff_g
	local diff_g: di %7.3f `r(mean)'
	local means treatmean_g ctrlmean_g

	if "`var'" != "Saspiration_index2" {
		sum B_`var' if B_Sgirl==0 & Sschool_id!=2711 & child_id!=3205037 & B_`var'_flag==0
		local totalmean_b = `r(mean)'
		local totalsd_b: di %7.3f `r(sd)'

		sum B_`var' if B_Sgirl==0 & Sschool_id!=2711 & child_id!=3205037 & B_`var'_flag==0 & B_treat == 1
		local treatmean_b = `r(mean)'
		local treatmean_b: di %7.3f `r(mean)'	
		local treatsd_b: di %7.3f `r(sd)'

		sum B_`var' if B_Sgirl==0 & Sschool_id!=2711 & child_id!=3205037 & B_`var'_flag==0 & B_treat == 0
		local ctrlmean_b = `r(mean)'
		local ctrlmean_b: di %7.3f `r(mean)'	
		local ctrlsd_b: di %7.3f `r(sd)'
		
		gen stand_diff_b = (`treatmean_b'-`ctrlmean_b')/ `totalsd_b'
		sum stand_diff_b
		local diff_b: di %7.3f `r(mean)'

		local means treatmean_g ctrlmean_g treatmean_b ctrlmean_b
	}

	else {
		local treatmean_b 
		local ctrlmean_b
		local treatsd_b
		local ctrlsd_b
		local diff_b
	}

	foreach x in girlsd boysd diff {   // to remove leading blank spaces from numbers, and round the number to 3 decimal spaces
	cap local `x'=trim(string(``x'',"%7.3f"))	// cap because for some variables irrelevant	
}

// preventing -0.000
foreach mean in `means' {
	if abs(``mean'') < .0001 {
		local `mean' "0.000"
	}
}

// we write out tex table lines showing means followed by sd in brackets.


file write sumstat " `varlab' & `treatmean_g' & `ctrlmean_g' & `diff_g' & `treatmean_b' & `ctrlmean_b' & `diff_b'  \\     " _n  
if "`var'" != "Saspiration_index2" {
	file write sumstat "  & [`treatsd_g']  & [`ctrlsd_g'] & & [`treatsd_b']  & [`ctrlsd_b'] &    \\   \addlinespace[3pt]   " _n  
}
else {
	file write sumstat "  & [`treatsd_g']  & [`ctrlsd_g']  & & & &     \\   \addlinespace[3pt]   " _n 
}

cap drop stand_diff*	 // cap because for some variables irrelevant
}

file write sumstat "\bottomrule \end{tabular} " _n
file close sumstat

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  APPENDIX TABLE 1.2:  Determinants of Endline attrition and survey location 
*  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

cap log close
log using "$ad_hoc/fully_interacted_regression.smcl", append
qui log off 

foreach gender in girl boy /*both*/{

	if "`gender'" == "girl" {
		use "$finaldata", clear
		keep if B_Sgirl == 1
		local ar replace
		local preprehead "\begin{flushleft} Panel A: Girls \end{flushleft} \vspace{3mm}"
		local indices B_Sgender_index2 B_Saspiration_index2 B_Sbehavior_index2 
		foreach var in `indices'{
			gen treat_`var' = B_treat*`var'
			local varlab: variable label `var'
			lab var treat_`var' "Treated $\times$ `varlab'"			
		}
		
		local treatindices treat_B_Sgender_index2 treat_B_Saspiration_index2 treat_B_Sbehavior_index2 
		local tablerow B_treat treat_B_Sgender_index2 treat_B_Saspiration_index2 treat_B_Sbehavior_index2 B_Sgender_index2 B_Saspiration_index2 B_Sbehavior_index2
		local pf "\clearpage"
	}

	else if "`gender'" == "boy" {
		use "$finaldata", clear
		keep if B_Sgirl == 0
		local ar append 
		local preprehead "\begin{flushleft} Panel B: Boys \end{flushleft}"
		local indices B_Sgender_index2 B_Sbehavior_index2
		foreach var in `indices'{
			gen treat_`var' = B_treat*`var'
			local varlab: variable label `var'
			lab var treat_`var' "Treated $\times$ `varlab'"			
		}
		
		local treatindices treat_B_Sgender_index2 treat_B_Sbehavior_index2 
		local tablerow B_treat treat_B_Sgender_index2 treat_B_Sbehavior_index2 B_Sgender_index2 B_Sbehavior_index2
		local pf 
	}

	else if "`gender'" == "both" {
		use "$finaldata", clear 
		** to account for boys
		replace B_Saspiration_index2 = 0 if B_Sgirl==0	
		local indices B_Sgender_index2 B_Saspiration_index2 B_Sbehavior_index2 B_Sgirl
		foreach var in `indices'{
			gen treat_`var' = B_treat*`var'
			local varlab: variable label `var'
			lab var treat_`var' "Treated $\times$ `varlab'"			
		}
		
		local treatindices treat_B_Sgender_index2 treat_B_Saspiration_index2 treat_B_Sbehavior_index2 treat_B_Sgirl
		local pf
	}

	* survey location
	
	gen E_interview_scl = inlist(E_Splace_of_interview, 1) if !mi(E_Splace_of_interview) & attrition_el==0 // school
	gen E2_interview_scl = 0 if attrition==0
	la var E2_interview_scl "Surveyed in school"
	
	gen E_interview_dir = inlist(E_Splace_of_interview, 1, 2)  if !mi(E_Splace_of_interview) & attrition_el==0   // school and home
	gen E2_interview_dir = inlist(E2_Ssurvey_type, 1) if !mi(E2_Ssurvey_type)  & attrition==0  // student in person
	la var E2_interview_dir "Surveyed student in-person"
	
	* attrition
	rename attrition E2_attrition
	rename attrition_el E_attrition
	
	replace E_attrition=. if Sschool_id==2711 // 2711 is the new school which was surveyed during endline
	replace E2_attrition=. if Sschool_id==2711
	
	la var E2_attrition "Attrited"

	eststo clear	
	local header	
	local n_col = 0


	foreach e in E E2 {

		if "`e'"=="E" {

			foreach var in attrition interview_scl interview_dir  {			

				local varlab: variable label E2_`var'
				local header "`header' & `varlab'" // column headers with variables labels 

				local ++n_col // number of columns

				// for treat=control pval
				qui reg `e'_`var' B_treat district_gender_* gender_grade_*, cluster(Sschool_id)
				local t = _b[B_treat]/_se[B_treat]
				local p = 2*ttail(e(df_r),abs(`t'))


				eststo `e'_`var': reg `e'_`var' B_treat `treatindices' `indices' /*B_Sgender_index2_flag B_Saspiration_index2_flag B_Sbehavior_index2_flag*/ district_gender_* gender_grade_* , cluster(Sschool_id)

				su `e'_`var'  if B_treat==0 
				estadd scalar ctrlmean=r(mean) 	
				
				su `e'_`var'  if B_treat==1 
				estadd scalar treatmean=r(mean) 	

				estadd local basic "Yes"
				estadd scalar pval = `p'

				if "`gender'" == "both"{
					***p-value calculation
					local controls B_treat `treatindices' `indices' /*B_Sgender_index2_flag B_Saspiration_index2_flag B_Sbehavior_index2_flag*/ 
					foreach var1 of varlist `controls'{
						local varname gX`var1'
						local varname = substr("`varname'", 1, 30)
						gen `varname' = B_Sgirl * `var1'
					}

					reg `e'_`var' B_treat `treatindices' `indices' /*B_Sgender_index2_flag B_Saspiration_index2_flag B_Sbehavior_index2_flag*/ district_gender_* gender_grade_* gX*, cluster(Sschool_id)
					local `e'`var'p1: di %7.3f 2*ttail(e(df_r), abs(_b[treat_B_Sgirl]/_se[treat_B_Sgirl]))
					local `e'`var'p2: di %7.3f 2*ttail(e(df_r), abs(_b[gXtreat_B_Sgender_index2]/_se[gXtreat_B_Sgender_index2]))
					local `e'`var'p3: di %7.3f 2*ttail(e(df_r), abs(_b[gXtreat_B_Sbehavior_index2]/_se[gXtreat_B_Sbehavior_index2]))
					local `e'`var'p4: di %7.3f 2*ttail(e(df_r), abs(_b[gXB_Sgender_index2]/_se[gXB_Sgender_index2]))
					local `e'`var'p5: di %7.3f 2*ttail(e(df_r), abs(_b[gXB_Sbehavior_index2]/_se[gXB_Sbehavior_index2]))

					drop gX*

				} 

			}
			
		}
		
		else if "`e'"=="E2" {

			foreach var in attrition interview_dir  {			

				local varlab: variable label E2_`var'
				local header "`header' & `varlab'" // column headers with variables labels 

				local ++n_col // number of columns

				// for treat=control pval
				qui reg `e'_`var' B_treat district_gender_* gender_grade_*, cluster(Sschool_id)
				local t = _b[B_treat]/_se[B_treat]
				local p = 2*ttail(e(df_r),abs(`t'))


				eststo `e'_`var': reg `e'_`var' B_treat `treatindices' `indices' /*B_Sgender_index2_flag B_Saspiration_index2_flag B_Sbehavior_index2_flag*/ district_gender_* gender_grade_*, cluster(Sschool_id)

				su `e'_`var'  if B_treat==0 
				estadd scalar ctrlmean=r(mean) 	
				
				su `e'_`var'  if B_treat==1 
				estadd scalar treatmean=r(mean) 
				
				estadd local basic "Yes"
				estadd scalar pval = `p'

				if "`gender'" == "both"{
					***p-value calculation
					local controls B_treat `treatindices' `indices' /*B_Sgender_index2_flag B_Saspiration_index2_flag B_Sbehavior_index2_flag*/
					local gX
					foreach var1 of varlist `controls'{
						local varname gX`var1'
						local varname = substr("`varname'", 1, 30)
						gen `varname' = B_Sgirl * `var1'
						local gX `gX' gX`var1'
					}

					qui log on 
					di "Appendix Table 1.1: Location/Attrition"
					di "Endline: `e'"
					di "Column: `var'"
					di "`gX'"
					qui log off

					reg `e'_`var' B_treat `treatindices' `indices' /*B_Sgender_index2_flag B_Saspiration_index2_flag B_Sbehavior_index2_flag*/ district_gender_* gender_grade_* gX*, cluster(Sschool_id)
					local `e'`var'p1: di %7.3f 2*ttail(e(df_r), abs(_b[treat_B_Sgirl]/_se[treat_B_Sgirl]))
					local `e'`var'p2: di %7.3f 2*ttail(e(df_r), abs(_b[gXtreat_B_Sgender_index2]/_se[gXtreat_B_Sgender_index2]))
					local `e'`var'p3: di %7.3f 2*ttail(e(df_r), abs(_b[gXtreat_B_Sbehavior_index2]/_se[gXtreat_B_Sbehavior_index2]))
					local `e'`var'p4: di %7.3f 2*ttail(e(df_r), abs(_b[gXB_Sgender_index2]/_se[gXB_Sgender_index2]))
					local `e'`var'p5: di %7.3f 2*ttail(e(df_r), abs(_b[gXB_Sbehavior_index2]/_se[gXB_Sbehavior_index2]))
					drop gX*

				}




			}
			
		}
	}

	if "`gender'" != "both" {

		local prehead = subinstr("`frame'", "colno", "`n_col'", .)

		#delimit ;

		esttab E_* E2_* using "$tables/el2_surveyplace_attrition_bindices.tex", 
		b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
		`ar' longtable gaps label booktabs style(tex) fragment 
		prehead("`preprehead' `prehead'" 	"& \multicolumn{3}{c}{Endline 1} & \multicolumn{2}{c}{Endline 2} \\  \cmidrule(lr){2-4} \cmidrule(lr){5-6}" "`header' \\")
		keep(`tablerow') order(`tablerow')
		stats(ctrlmean treatmean pval basic N,   
		labels("Control group mean" "Treatment group mean" "p-value: Treatment = Control" "Basic controls" "Number of students") 
		fmt(%7.3fc  %7.3fc  %7.3fc %20s %7.0fc))
		postfoot("\bottomrule" "\end{tabular}" "}" "`pf'"); 

		#delimit cr

	}

	else {

		local prehead = subinstr("`frame'", "colno", "`n_col'", .)

		file open sumstat using "$tables/el2_surveyplace_attrition_bindices.tex", write append 
		file write sumstat "\begin{flushleft} Panel C: P-vals, Boys vs. Girls \end{flushleft} `prehead' & \multicolumn{3}{c}{Endline 1} & \multicolumn{2}{c}{Endline 2} \\  \cmidrule(lr){2-4} \cmidrule(lr){5-6} `header' \\"
		local follow_head
		foreach i of numlist 1/5 {
			local follow_head "`follow_head' & \multicolumn{1}{c}{(`i')}"
		}
		file write sumstat "`follow_head' \\ \midrule "
		file write sumstat "Treated & `Eattritionp1' & `Einterview_sclp1' & `Einterview_dirp1' & `E2attritionp1' & `E2interview_dirp1' \\ "
		file write sumstat "Treated $\times$ Gender attitudes index & `Eattritionp2' & `Einterview_sclp2' & `Einterview_dirp2' & `E2attritionp2' & `E2interview_dirp2' \\ "
		file write sumstat "Treated $\times$ Self-reported behavior index & `Eattritionp3' & `Einterview_sclp3' & `Einterview_dirp3' & `E2attritionp3' & `E2interview_dirp3' \\ "
		file write sumstat "Gender attitudes index & `Eattritionp4' & `Einterview_sclp4' & `Einterview_dirp4' & `E2attritionp4' & `E2interview_dirp4' \\ "
		file write sumstat "Self-reported behavior index & `Eattritionp5' & `Einterview_sclp5' & `Einterview_dirp5' & `E2attritionp5' & `E2interview_dirp5' \\ "
		file write sumstat "\bottomrule \end{tabular} }"
		file close sumstat

		eststo clear
	}

}


use "$finaldata", clear

** paper stats
count if attrition_el==0 & Sschool_id!=2711 // el1 respondents out of those surveyed in baseline. 
local stat: di %7.0fc `r(N)'
cap file write paperstat "3.Study Design - 3.Endline data collection pg11, EL1 students (out of those surveyed in BL), `stat'" _n

su attrition_el if Sschool_id!=2711 // EL1 attrition 
local stat = 100*`r(mean)'
local stat: di %7.4f `stat'
cap file write paperstat "3.Study Design - 3.Endline data collection pg11, EL1 attrition rate, `stat'%" _n

count if attrition_el==0  // all el1 respondents
local stat: di %7.0fc `r(N)'
cap file write paperstat "3.Study Design - 3.Endline data collection pg11, EL1 students (all), `stat'" _n

***

count if attrition==0 & Sschool_id!=2711 // el2 respondents out of those surveyed in baseline. 
local stat: di %7.0fc `r(N)'
cap file write paperstat "3.Study Design pg11, EL2 students (out of those surveyed in BL), `stat'" _n

su attrition if Sschool_id!=2711 // EL2 attrition 
local stat = 100*`r(mean)'
local stat: di %7.4f `stat'
cap file write paperstat "3.Study Design pg11, EL2 attrition rate, `stat'%" _n

count if attrition==0  // all el2 respondents
local stat: di %7.0fc `r(N)'
cap file write paperstat "3.Study Design pg11, EL2 students (all), `stat'" _n


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
*	APPENDIX TABLE 1.3: Reasons for Survey Attrition
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

* endline 1	
use "$finaldata", clear

merge 1:1 child_id using "$deid/attrition"
keep if _m==3
drop _m

replace E_final_status = "Refused" if child_id==1503100 // all main variables are missing, refused to asnwer questions. 

* surveyed
gen el1_comp = . 
la var el1_comp "Tracked and surveyed"
gen el1_comp_scl = E_Splace_of_interview==1 & !inlist(E_final_status, "Death", "Mentally Challenged", "No consent", "Refused")
la var el1_comp_scl "In school"
gen el1_comp_home =  E_Splace_of_interview==2 & !inlist(E_final_status, "Death", "Mentally Challenged", "No consent", "Refused")
la var el1_comp_home "At home"
gen el1_comp_phone =  E_Splace_of_interview==3 & !inlist(E_final_status, "Death", "Mentally Challenged", "No consent", "Refused")
la var el1_comp_phone "Over the phone"

* tracked but not surveyed


gen el1_ns = . 
la var el1_ns "Tracked but could not be surveyed"
gen el1_ns_death_dis = inlist(E_final_status, "Death", "Mentally Challenged")
la var el1_ns_death_dis "Student deceased or unwell"
gen el1_ns_ref = inlist(E_final_status, "No consent", "Refused")
la var el1_ns_ref "Student or parent refused assent"

* not tracked
gen el1_nt = . 
la var el1_nt "Not tracked"
gen el1_nt_hh = inlist(E_final_status, "HH not found")
la var el1_nt_hh "Address not trackable"
gen el1_nt_ch = inlist(E_final_status, "Child not found at home")
la var el1_nt_ch "Student not found at home"
gen el1_nt_mov_ch = inlist(E_final_status, "Family in the village but child moved", "Family in village and child moved")
la var el1_nt_mov_ch "Family in village but student moved"
gen el1_nt_mov_hh = inlist(E_final_status, "Family and child moved")
la var el1_nt_mov_hh "Family and student moved"
gen el1_nt_oth = inlist(E_final_status, "Duplicate", "Unclassified")
la var el1_nt_oth "Other"

* - table

cap file close sumstat
file open sumstat using "$tables/el1_attrition_reasons.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\extracolsep{3pt}}{l}*{4}{>{\centering\arraybackslash}m{2cm}}@{}}   \toprule" _n      // table header
file write sumstat "  & Girls & Boys & Total    \\     " _n         
file write sumstat "\midrule" _n 

foreach var of varlist el1_comp* el1_ns* el1_nt* {
	
	local l: var lab `var'
	
	count if `var'==1 & B_Sgirl==1
	local fem : di %7.0fc `r(N)'

	count if `var'==1 & B_Sgirl==0
	local mal  : di %7.0fc `r(N)'	

	count if `var'==1 
	local tot  : di %7.0fc `r(N)'	

	if "`var'"=="el1_comp" | "`var'"=="el1_ns" | "`var'"=="el1_nt" {			
		file write sumstat " `l' &  &   &    \\     " _n   
	}

	else {
		file write sumstat " \hspace{5mm} `l' & `fem' & `mal'  & `tot'   \\     " _n 			
	}

}

file write sumstat "\bottomrule" _n           // table footer
file write sumstat "\end{tabular}" _n 
file close sumstat

*** for slides
cap file close sumstat
file open sumstat using "$slides/el1_attrition_reasons.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\extracolsep{3pt}}{l}*{4}{>{\centering\arraybackslash}m{2cm}}@{}}   \toprule" _n      // table header
file write sumstat "  & Female & Male & Total    \\     " _n         
file write sumstat "\midrule" _n 

foreach var of varlist el1_comp* el1_ns* el1_nt* {
	
	local l: var lab `var'
	
	count if `var'==1 & B_Sgirl==1
	local fem : di %7.0fc `r(N)'

	count if `var'==1 & B_Sgirl==0
	local mal  : di %7.0fc `r(N)'	

	count if `var'==1 
	local tot  : di %7.0fc `r(N)'	

	if "`var'"=="el1_comp" | "`var'"=="el1_ns" | "`var'"=="el1_nt" {			
		file write sumstat " `l' &  &   &    \\     " _n   
	}

	else {
		file write sumstat " \hspace{5mm} `l' & `fem' & `mal'  & `tot'   \\     " _n 			
	}

}

file write sumstat "\bottomrule" _n           // table footer
file write sumstat "\end{tabular}" _n 
file close sumstat

****** paper stats
count if attrition_el==0 // total el1 resp

su el1_comp_scl if attrition_el==0 
local stat = 100*`r(mean)'
local stat: di %7.4fc `stat'
cap file write paperstat "3.Study Design pg11, % of EL1 respondents surveyed at school, `stat'" _n

su el1_comp_home if attrition_el==0 
local stat = 100*`r(mean)'
local stat: di %7.4fc `stat'
cap file write paperstat "3.Study Design pg11, % of EL1 respondents surveyed at home, `stat'" _n

su el1_comp_phone if attrition_el==0 
local stat = 100*`r(mean)'
local stat: di %7.4fc `stat'
cap file write paperstat "3.Study Design pg11, % of EL1 respondents surveyed on phone, `stat'" _n

* endline 2

use "$finaldata", clear

merge 1:1 child_id using "$deid/attrition"

replace E2_final_status = "Not available/tracked" if _m==1
drop _m

* surveyed

gen comp = consent!=0 & !inlist(E2_Sdisability, 5, 6) 


gen el2_comp = . 
la var el2_comp "Tracked and surveyed" 
gen el2_comp_per = E2_Ssurvey_type==1 & comp==1
la var el2_comp_per "In-person"
gen el2_comp_phone =  E2_Ssurvey_type==2 & comp==1
la var el2_comp_phone "On phone"
gen el2_comp_parent =  inlist(E2_Ssurvey_type, 3, 4) & comp==1
la var el2_comp_parent "Parent survey\sym{*}"

* tracked but not surveyed
gen el2_ns = . 
la var el2_ns "Tracked but could not be surveyed"
gen el2_ns_death_dis = (inlist(E2_Sdisability, 5, 6) | E2_Sstudent_available==0) & !mi(E2_Ssurvey_type)
la var el2_ns_death_dis "Student deceased or unwell"
gen el2_ns_ref = consent==0 & !mi(E2_Ssurvey_type) &  E2_Sstudent_available!=0 // excluding no consent due to deceased
la var el2_ns_ref "Student or parent refused assent"

* not tracked
gen el2_nt = mi(E2_Ssurvey_type)
la var el2_nt "Not tracked"

* - table

cap file close sumstat
file open sumstat using "$tables/el2_attrition_reasons.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\extracolsep{3pt}}{l}*{4}{>{\centering\arraybackslash}m{2cm}}@{}}   \toprule" _n      // table header
file write sumstat "  & Girls & Boys & Total    \\     " _n         
file write sumstat "\midrule" _n 

foreach var of varlist el2_comp* el2_ns* el2_nt {
	
	local l: var lab `var'
	
	count if `var'==1 & B_Sgirl==1
	local fem : di %7.0fc `r(N)'

	count if `var'==1 & B_Sgirl==0
	local mal  : di %7.0fc `r(N)'	

	count if `var'==1 
	local tot  : di %7.0fc `r(N)'	

	if "`var'"=="el2_comp" | "`var'"=="el2_ns"  {			
		file write sumstat " `l' &  &    &    \\     " _n   
	}

	else if "`var'"=="el2_nt" {			
		file write sumstat " `l' & `fem' & `mal'  & `tot'    \\     " _n   
	}

	else {
		file write sumstat " \hspace{5mm} `l' & `fem' & `mal'  & `tot'   \\     " _n 			
	}

}

file write sumstat "\bottomrule" _n           // table footer
file write sumstat "\end{tabular}" _n 
file close sumstat

** slides
cap file close sumstat
file open sumstat using "$slides/el2_attrition_reasons.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\extracolsep{3pt}}{l}*{4}{>{\centering\arraybackslash}m{2cm}}@{}}   \toprule" _n      // table header
file write sumstat "  & Female & Male & Total    \\     " _n         
file write sumstat "\midrule" _n 

foreach var of varlist el2_comp* el2_ns* el2_nt {
	
	local l: var lab `var'
	
	count if `var'==1 & B_Sgirl==1
	local fem : di %7.0fc `r(N)'

	count if `var'==1 & B_Sgirl==0
	local mal  : di %7.0fc `r(N)'	

	count if `var'==1 
	local tot  : di %7.0fc `r(N)'	

	if "`var'"=="el2_comp" | "`var'"=="el2_ns"  {			
		file write sumstat " `l' &  &    &    \\     " _n   
	}

	else if "`var'"=="el2_nt" {			
		file write sumstat " `l' & `fem' & `mal'  & `tot'    \\     " _n   
	}

	else {
		file write sumstat " \hspace{5mm} `l' & `fem' & `mal'  & `tot'   \\     " _n 			
	}

}

file write sumstat "\bottomrule" _n           // table footer
file write sumstat "\end{tabular}" _n 
file close sumstat

su el2_comp_phone if attrition==0 
local stat = 100*`r(mean)'
local stat: di %7.4fc `stat'
cap file write paperstat "3.4Primary Outcomes pg13, % of EL2 respondents surveyed on phone, `stat'%" _n

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  APPENDIX TABLE 1.4:  Descriptive stats on school enrollment and program participation
*  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

	* Endline 2
	
	gen E2_in_school= inlist(E2_Sschool_enrolled,1, 2, 3, 4) if !mi(E2_Sschool_enrolled)
	lab var E2_in_school "Current school attendance"
	
	gen E2_same_school=inlist(E2_Sschool_same,1) if !mi(E2_Sschool_same)
	lab var E2_same_school "Same school as at baseline"
	
	gen E2_private_same=inlist(E2_Sschool_enrolled,2) if !mi(E2_Sschool_enrolled)
	lab var E2_private_same "Private school in same village/town as at baseline"
	
	gen E2_govt_diff=inlist(E2_Sschool_enrolled,3) if !mi(E2_Sschool_enrolled)
	lab var E2_govt_diff "Govt school in different village/town than at baseline"
	
	gen E2_private_diff=inlist(E2_Sschool_enrolled,4) if !mi(E2_Sschool_enrolled)
	lab var E2_private_diff "Private school in different village/town than at baseline"
	
	gen E2_college=inlist(E2_Sschool_enrolled,7, 9) if !mi(E2_Sschool_enrolled)
	lab var E2_college "College or vocational course"
	
	gen E2_any_form_scl = inlist(E2_Sschool_enrolled,1, 2, 3, 4, 7, 8) if !mi(E2_Sschool_enrolled)
	la var E2_any_form_scl "Currently in formal schooling/college"

	gen E2_dropout=inlist(E2_Sschool_enrolled,6) if !mi(E2_Sschool_enrolled)
	lab var E2_dropout "Dropped out of school and not pursing any other course"
	
	* Endline 1
	
	gen E_in_school=inlist(E_Sschool_enrolled,1,2,3,4) if !mi(E_Sschool_enrolled)
	lab var E_in_school "School attendance"
	
	gen E_same_school=inlist(E_Sschool_enrolled,1) if !mi(E_Sschool_enrolled)
	lab var E_same_school "Same school"
	
	gen E_private_same=inlist(E_Sschool_enrolled,2) if !mi(E_Sschool_enrolled)
	lab var E_private_same "Private school in same village/town"
	
	gen E_govt_diff=inlist(E_Sschool_enrolled,3) if !mi(E_Sschool_enrolled)
	lab var E_govt_diff "Govt school in different village/town"
	
	gen E_private_diff=inlist(E_Sschool_enrolled,4) if !mi(E_Sschool_enrolled)
	lab var E_private_diff "Private school in different village/town"
	
	gen E_college = .
	
	gen E_any_form_scl = inlist(E_Sschool_enrolled,1, 2, 3, 4) if !mi(E_Sschool_enrolled)
	la var E_any_form_scl "Currently in formal schooling/college"

	gen E_dropout=inlist(E_Sschool_enrolled,6) if !mi(E_Sschool_enrolled)
	lab var E_dropout "Dropped out of school"
	
	replace E_Sattend_session = . if E_Sattend_session_flag!=0
	replace E_Sprogram_aware = . if E_Sprogram_aware_flag !=0
	gen E2_Sattend_session = . 
	gen E2_Sprogram_aware = .
	lab var E2_Sattend_session "Program participation (treatment group only)"
	lab var E2_Sprogram_aware "Aware of program (treatment group only)"



	local scl_vars same_school private_same govt_diff private_diff any_form_scl dropout Sprogram_aware

	cap file close sumstat
	
	file open sumstat using "$tables/el2_tab_outcome_stats.tex", write replace

	file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\extracolsep{2pt}}{l}*{4}{>{\centering\arraybackslash}m{1.5cm}}@{}}  \toprule" _n      
	file write sumstat "& \multicolumn{2}{c}{Endline 1} & \multicolumn{2}{c}{Endline 2} \\  \cmidrule(lr){2-3} \cmidrule(lr){4-5}"
	
	file write sumstat " & Girls & Boys & Girls & Boys \\" _n
	file write sumstat "\midrule" _n 
	
	foreach var in `scl_vars'  {
		
		local label: variable label E2_`var'

			* Endline 1 
			sum E_`var' if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 1, d
			local E_mean_g: di %7.3f r(mean)
			local E_sd: di %7.3f r(sd)
			local E_sd_g= trim(string(`E_sd',"%7.3f"))
			sum E_`var' if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 0, d
			local E_mean_b: di %7.3f r(mean)
			local E_sd: di %7.3f r(sd)
			local E_sd_b= trim(string(`E_sd',"%7.3f"))

			* Endline 2
			sum E2_`var' if !mi(E2_Steam_id) & attrition==0 & B_Sgirl == 1, d
			local E2_mean_g: di %7.3f r(mean)
			local E2_sd: di %7.3f r(sd)
			local E2_sd_g= trim(string(`E2_sd',"%7.3f"))
			sum E2_`var' if !mi(E2_Steam_id) & attrition==0 & B_Sgirl == 0, d
			local E2_mean_b: di %7.3f r(mean)
			local E2_sd: di %7.3f r(sd)
			local E2_sd_b= trim(string(`E2_sd',"%7.3f"))

			* NA for EL2
			if "`var'"=="Sattend_session" | "`var'"=="Sprogram_aware" {
				local E2_mean_g = ""
				local E2_mean_b = ""
				local E2_sd_g = ""
				local E2_sd_b = ""
				
				file write sumstat " `label' & `E_mean_b' &  `E_mean_g' & `E2_mean_b' & `E2_mean_g' \\  " _n
				file write sumstat "  & [`E_sd_b'] & [`E_sd_g'] & `E2_sd_b' & `E2_sd_g' \\  " _n 

			}

			else {
				file write sumstat " `label' & `E_mean_g' &  `E_mean_b' & `E2_mean_g' & `E2_mean_b' \\  " _n
				file write sumstat "  & [`E_sd_g'] & [`E_sd_b'] & [`E2_sd_g'] & [`E2_sd_b'] \\  " _n 
			}

			file write sumstat "\addlinespace"
		}
		
		count if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 1 // no. of obs in endline 2
		local E_obs_g: di %7.0fc `r(N)'
		count if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 0 // no. of obs in endline 2
		local E_obs_b: di %7.0fc `r(N)'

		count if !mi(E2_Steam_id) & attrition==0 & B_Sgirl == 1 // no. of obs in endline 2
		local E2_obs_g: di %7.0fc `r(N)'
		count if !mi(E2_Steam_id) & attrition==0 & B_Sgirl == 0 // no. of obs in endline 2
		local E2_obs_b: di %7.0fc `r(N)'

		file write sumstat "\midrule" _n              

		file write sumstat "Number of observations & `E_obs_g' & `E_obs_b' & `E2_obs_g' & `E2_obs_b'  \\    \addlinespace[3pt]   " _n 

		file write sumstat "\bottomrule" _n              
		file write sumstat "\end{tabular}" _n 
		file close sumstat	

	** paper stats
	sum E_same_school if !mi(E_Steam_id) & attrition_el==0
	local stat = 100*`r(mean)'
	local stat: di %7.4fc `stat'
	cap file write paperstat "3. Study Design - 3.4 Primary Outcomes pg11, % EL1 enrolled in same school as baseline, `stat' " _n

	sum E_dropout if !mi(E_Steam_id) & attrition_el==0
	local stat = 100*`r(mean)'
	local stat: di %7.4fc `stat'
	cap file write paperstat "3. Study Design - 3.4 Primary Outcomes pg11, % EL1 dropped out, `stat' " _n

	sum E_Sprogram_aware if !mi(E_Steam_id) & attrition_el==0
	local stat = 100*`r(mean)'
	local stat: di %7.4fc `stat'
	cap file write paperstat "3. Study Design - 3.4 Primary Outcomes pg11, % EL1 aware of program, `stat' " _n



* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
* APPENDIX TABLE 1.5 : Descriptive stats on attitudes and aspirations
* Summary statistics of all variables in the gender attitudes and aspirations index
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

use "$finaldata", clear

la var B_Saspiration_all_index2 "Aspirations index"

lab var B_Shighest_educ_median "Student's highest desired level of education is above sample median"
la var B_Schild_equal_opps_y "Agree: Men and women should get equal opportunities in all spheres of life"
la var B_Schild_girl_allow_study_y "Agree: Girls should be allowed to study as far as they want"
la var B_Schild_similar_right_y "Agree: Daughters should have a similar right to inherited property as sons"
la var B_Schild_elect_woman_y "Agree: It would be a good idea to elect a woman  as the village Sarpanch"
la var B_Schild_boy_more_opps_n "Disagree: Boys should get more opportunities/resources for education than girls"

local gender B_Schild_woman_role_n B_Schild_man_final_deci_n B_Schild_woman_tol_viol_n B_Schild_wives_less_edu_n ///
B_Schild_boy_more_opps_n B_Schild_equal_opps_y B_Schild_girl_allow_study_y B_Schild_similar_right_y B_Schild_elect_woman_y

local aspiration B_Sdisc_edu_goals B_Shighest_educ_median B_Soccupa_25_white

local behavior B_Scook_clean_comm B_Stalk_opp_gender_comm B_Stake_care_young_sib_comm
la var B_Scook_clean_comm "Boys cook/clean and Girls don't"
la var B_Stalk_opp_gender_comm "Comfortable talking to students of opp. gender"
la var B_Stake_care_young_sib_comm "Boys take care of younger siblings and Girls don't"


count if B_Sgirl!=. & Sschool_id!=2711 & child_id!=3205037    // baseline sample counts
local total: di %7.0fc `r(N)'

count if B_Sgirl==0 & Sschool_id!=2711 & child_id!=3205037
local boys: di %7.0fc `r(N)'

count if B_Sgirl==1 & Sschool_id!=2711 & child_id!=3205037
local girls: di %7.0fc `r(N)'

cap file close sumstat
file open sumstat using "$tables/tab_descriptive_outcome_vars.tex", write replace


file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\extracolsep{3pt}}p{11cm}*{3}{>{\centering\arraybackslash}m{2cm}}@{}}   \toprule" _n      // table header
file write sumstat " Variable & Girls & Boys   \\     " _n         

file write sumstat "\midrule" _n

foreach var in B_Sgender_index2 `gender' {    
	
	local lab1
	local lab2

	local varlab: variable label `var'

	local varlab = subinstr("`varlab'", ".", "", .)

	if "`var'"!="B_Sgender_index2" {

		local l = length("`varlab'")
		local b = ceil(`l'/60)

		if `l' > 90 pause
		if `l' > 50 {
			forv i = 1/`b' {
				local lab`i': piece `i' 60 of "`varlab'", nobreak
			}
			local varlab2 "\hspace{0.5 cm} `lab1'" 
			local varlab3 "\hspace{0.8 cm} `lab2'"
		}
		else {
			local varlab2 "\hspace{0.5 cm} `varlab'"
			local varlab3
		}	
		
	}

	else if "`var'"=="B_Sgender_index2" {
		local varlab2 "\multirow{2}{*}{`varlab'}"
		local varlab3
	}

	qui sum `var' if B_Sgirl==1 & Sschool_id!=2711 & `var'_flag==0
	local girlmean: di %7.3f `r(mean)'	
	local girlsd: di %7.3f `r(sd)'

	qui sum `var' if B_Sgirl==0 & Sschool_id!=2711 & `var'_flag==0
	local boymean: di %7.3f `r(mean)'	
	local boysd: di %7.3f `r(sd)'

	
	
	foreach x in girlsd boysd {   // to remove leading blank spaces from numbers, and round the number to 3 decimal spaces
	local `x'=trim(string(``x'',"%7.3f"))
}


// we write out tex table lines showing means followed by sd in brackets.


file write sumstat "`varlab2'&   `girlmean' & `boymean'       \\     " _n  
file write sumstat " `varlab3' & [`girlsd']  & [`boysd']   \\   \addlinespace[3pt]   " _n  


}

file write sumstat "  \addlinespace[3pt] " _n

foreach var in B_Sbehavior_index2 `behavior' {   
	
	local lab1
	local lab2


	local varlab: variable label `var'
	
	local l = length("`varlab'")
	local b = ceil(`l'/60)

	if "`var'"!="B_Sbehavior_index2" {

		if `l' > 90 pause
		if `l' > 50 {
			forv i = 1/`b' {
				local lab`i': piece `i' 60 of "`varlab'", nobreak
			}
			local varlab2 "\hspace{0.5 cm} `lab1'" 
			local varlab3 "\hspace{0.8 cm} `lab2'"
		}
		else {
			local varlab2 "\hspace{0.5 cm} `varlab'"
			local varlab3
		}	
		
	}

	else if "`var'"=="B_Sbehavior_index2" {
		local varlab2 "\multirow{2}{*}{`varlab'}"
		local varlab3
	}

	sum `var' if B_Sgirl==1 & Sschool_id!=2711 & `var'_flag==0
	local girlmean: di %7.3f `r(mean)'	
	local girlsd: di %7.3f `r(sd)'		

	
	sum `var' if B_Sgirl==0 & Sschool_id!=2711 & `var'_flag==0
	local boymean: di %7.3f `r(mean)'	
	local boysd: di %7.3f `r(sd)'

	
	
	foreach x in girlsd boysd {   // to remove leading blank spaces from numbers, and round the number to 3 decimal spaces
	local `x'=trim(string(``x'',"%7.3f"))
}


// we write out tex table lines showing means followed by sd in brackets.

file write sumstat "`varlab2'&   `girlmean' & `boymean'       \\     " _n  
file write sumstat " `varlab3' & [`girlsd']  & [`boysd']   \\   \addlinespace[3pt]   " _n  


}

file write sumstat "  \addlinespace[3pt] " _n

foreach var in B_Saspiration_all_index2 `aspiration' {   
	
	local lab1
	local lab2


	local varlab: variable label `var'
	
	local l = length("`varlab'")
	local b = ceil(`l'/60)

	if "`var'"!="B_Saspiration_all_index2" {

		if `l' > 90 pause
		if `l' > 50 {
			forv i = 1/`b' {
				local lab`i': piece `i' 60 of "`varlab'", nobreak
			}
			local varlab2 "\hspace{0.5 cm} `lab1'" 
			local varlab3 "\hspace{0.8 cm} `lab2'"
		}
		else {
			local varlab2 "\hspace{0.5 cm} `varlab'"
			local varlab3
		}	
		
	}

	else if "`var'"=="B_Saspiration_all_index2" {
		local varlab2 "\multirow{2}{*}{`varlab'}"
		local varlab3
	}

	sum `var' if B_Sgirl==1 & Sschool_id!=2711 & `var'_flag==0
	local girlmean: di %7.3f `r(mean)'	
	local girlsd: di %7.3f `r(sd)'		

	
	sum `var' if B_Sgirl==0 & Sschool_id!=2711 & `var'_flag==0
	local boymean: di %7.3f `r(mean)'	
	local boysd: di %7.3f `r(sd)'

	
	
	foreach x in girlsd boysd {   // to remove leading blank spaces from numbers, and round the number to 3 decimal spaces
	local `x'=trim(string(``x'',"%7.3f"))
}


// we write out tex table lines showing means followed by sd in brackets.

file write sumstat "`varlab2'&   `girlmean' & `boymean'       \\     " _n  
file write sumstat " `varlab3' & [`girlsd']  & [`boysd']   \\   \addlinespace[3pt]   " _n  


}
file write sumstat "Number of students &`girls' &`boys'   \\    \addlinespace[3pt]   " _n 
file write sumstat "\bottomrule" _n           // table footer
file write sumstat "\end{tabular}" _n 
file close sumstat


	* paper stats
	
	sum B_Schild_woman_role_n if B_Sgirl==0 & Sschool_id!=2711 & B_Schild_woman_role_n_flag==0
	local stat = 100 - (100*`r(mean)')
	local stat: di %7.4fc `stat'
	cap file write paperstat "3.4Primary Outcomes pg12, % boys believe woman's role is homemaker, `stat' " _n

	sum B_Schild_woman_role_n if B_Sgirl==1 & Sschool_id!=2711 & B_Schild_woman_role_n_flag==0
	local stat = 100 - (100*`r(mean)')
	local stat: di %7.4fc `stat'
	cap file write paperstat "3.4Primary Outcomes pg12, % girls believe woman's role is homemaker, `stat' " _n


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
* 	APPENDIX TABLE 1.6: LASSO-selected extended controls for each index
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

/*  
*** loop that was used to find perfectly collinear vars pre-LASSO
local perfect_colinear
foreach var of varlist $cntrls_all_8 {
	local temp_list $cntrls_all_8
	local toremove `var' 
	local abridged_list: list temp_list - toremove 
	foreach var1 of varlist `abridged_list' {
		reg `var' `var1' 
		if e(r2) == 1{
			local perfect_colinear `perfect_colinear' `var' `var1'
		}
	}
}
qui log on 
di "`perfect_colinear'"
log close
*/

cap file close sumstat
file open sumstat using "$tables/el1_el2_matrix_ext_cntrls_8.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\extracolsep{1pt}}p{7.8cm}*{6}{>{\centering\arraybackslash}m{2cm}}@{}}   \toprule" _n      // table header
file write sumstat "  Extended control variable & Gender attitudes index &  Girls' aspirations index & Self-reported behavior index & Applied to scholarship (girls)  & Supported petition     \\   "  _n         
file write sumstat "\midrule" _n 

local vars $cntrls_all_8

foreach var in `vars' {
	
	local line "label & index1 & index2 & index3 & index4 & index5"
	
	* variable label
	local l: var lab `var'	
	if "`l'" == "" {
		local l `var'
	}
	local l = subinstr("`l'", "_", "-", .)
	local line = subinstr("`line'", "label", "`l'", .)
	
	* X if variable selected by LASSO for each resp. index
	if strpos("$ext_gender_all_8", "`var'")>0 local line = subinstr("`line'", "index1", "index1X", .) 
	//if strpos("$ext_aspiration", "`var'")>0 local line = subinstr("`line'", "index2", "index2X", .)
	if strpos("$ext_behavior_common_all_8", "`var'")>0 local line = subinstr("`line'", "index3", "index3X", .)
	//if strpos("$ext_scholar", "`var'")>0 local line = subinstr("`line'", "index4", "index4X", .)
	if strpos("$ext_petition_all_8", "`var'")>0 local line = subinstr("`line'", "index5", "index5X", .)
	
	*Y if variable selected by LASSO in girls subset
	if strpos("$ext_gender_g_all_8", "`var'")>0 local line = subinstr("`line'", "index1", "index1G", .)
	if strpos("$ext_aspiration_g_all_8", "`var'")>0 local line = subinstr("`line'", "index2", "index2G", .)
	if strpos("$ext_behavior_common_g_all_8", "`var'")>0 local line = subinstr("`line'", "index3", "index3G", .)
	if strpos("$ext_scholar_g_all_8", "`var'")>0 local line = subinstr("`line'", "index4", "index4G", .)
	if strpos("$ext_petition_g_all_8", "`var'")>0 local line = subinstr("`line'", "index5", "index5G", .)
	*Z if variable selected by LASSO in boys subset
	if strpos("$ext_gender_b_all_8", "`var'")>0 local line = subinstr("`line'", "index1", "index1B", .)
	if strpos("$ext_aspiration_b_all_8", "`var'")>0 local line = subinstr("`line'", "index2", "index2B", .)
	if strpos("$ext_behavior_common_b_all_8", "`var'")>0 local line = subinstr("`line'", "index3", "index3B", .)
	if strpos("$ext_scholar_b_all_8", "`var'")>0 local line = subinstr("`line'", "index4", "index4B", .)
	if strpos("$ext_petition_b_all_8", "`var'")>0 local line = subinstr("`line'", "index5", "index5B", .)

	forval i = 1/5 {
		local line = subinstr("`line'", "index`i'", "", .)		
	}

	file write sumstat "`line' \\ " _n    	

}


file write sumstat "\bottomrule" _n           // table footer
file write sumstat "\end{tabular}" _n 
file close sumstat

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
*	APPENDIX TABLE 1.10 & 1.23: Primary Outcomes w/ LASSO controls, EL1&El2
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 	

use "$finaldata", clear
cap log close
log using "$ad_hoc/fully_interacted_regression.smcl", append
qui log close 

* quick paperstats artifact
local pre E_S 
local pre2 el1
local gb 
local attr _el 
foreach index in gender_index2 {
	if "`index'"=="gender_index2" {
		reg `pre'`index' B_treat ${ext_``index''`gb'_all_8} if !mi(`pre'team_id) & attrition`attr'==0 , cluster(Sschool_id)
		local stat: di %7.4fc _b[B_treat]
		cap file write paperstat "4.2Short-run results pg15, EL1 gender attitudes SD (w/ LASSO-selected controls), `stat'   " _n 
	}
}

local pre E2_S 
local pre2 el2
local attr
local gb 
foreach index in scholar_index2 petition_index2 {
	if "`index'"=="scholar_index2" {
		local gb _g 
		reg `pre'`index' B_treat ${ext_``index''`gb'_all_8} if !mi(`pre'team_id) & attrition`attr'==0 & B_Sgirl == 1, cluster(Sschool_id)
		local t = _b[B_treat]/_se[B_treat]
		local p = 2*ttail(e(df_r),abs(`t'))
		local stat: di %7.5fc `p'			
		cap file write paperstat "4.3Medium-run results pg21, EL2 applied to scholarship w/ LASSO-selected controls pval, `stat'   " _n 
	}

	else if "`index'"=="petition_index2" {
		local gb 
		reg `pre'`index' B_treat ${ext_``index''`gb'_all_8} if !mi(`pre'team_id) & attrition`attr'==0, cluster(Sschool_id)
		local t = _b[B_treat]/_se[B_treat]
		local p = 2*ttail(e(df_r),abs(`t'))
		local stat: di %7.5fc `p'			
		cap file write paperstat "4.3Medium-run results pg21, EL2 signed petition w/ LASSO-selected controls pval, `stat'   " _n 
	}
}

foreach time in el1 el2 {
	cap log close 
	log using "$ad_hoc/`time'_lasso_check_8.smcl", replace
	qui log off 
	if "`time'" == "el1" {
		local both_list gender_index2 behavior_index2
		local girl_list gender_index2 aspiration_index2 behavior_index2
		local boy_list gender_index2 behavior_index2
		local attr _el
		local pre E_S 
		local pre2 el 
	}
	else if "`time'" == "el2" {
		local both_list gender_index2 behavior_index2 petition_index2
		local girl_list gender_index2 aspiration_index2 behavior_index2 scholar_index2 petition_index2
		local boy_list gender_index2 behavior_index2 petition_index2
		local attr
		local pre E2_S 
		local pre2 el2
	}

	local i=0
	local header " "
	foreach gender in both girl boy {
		use "$finaldata", clear
		local gb 
		*** replace flags as 1 for el flags if missing ***
		foreach var of varlist ${el_gender_flag} ${el_behavior_common_flag} ${el2_aspiration_flag} {
			replace `var' = 1 if mi(`var')
		}
		if "`gender'" == "girl" {
			keep if B_Sgirl == 1
			local gb _g
		}
		else if "`gender'" == "boy" {
			keep if B_Sgirl == 0
			local gb _b
		}
		
		foreach index in ``gender'_list' {

			local ++i
			local varlab: var la `pre'`index' 
			local header "`header' & `varlab'"
			if "`index'"=="aspiration_index2" {
				qui log on 
				reg `pre'`index' B_treat ${ext_``index''`gb'_all_8} if B_Sgirl==1 & !mi(`pre'team_id) & attrition`attr'==0, cluster(Sschool_id)
				qui log off 
			}

			else if "`index'" == "petition_index2" | "`index'"=="scholar_index2" {
				qui log on 
				reg `pre'`index' B_treat ${ext_``index''`gb'_all_8} if !mi(`pre'team_id) & attrition`attr'==0 , cluster(Sschool_id)
				qui log off 
			}

			else {
				qui log on 
				reg `pre'`index' B_treat ${ext_``index''`gb'_all_8} if !mi(`pre'team_id) & attrition`attr'==0 , cluster(Sschool_id)
				qui log off 
			}	

			* storing coefs		
			local b`i': di %7.3fc _b[B_treat]
			local se`i'= trim(string(_se[B_treat],"%7.3f"))
			local t`i' = _b[B_treat]/_se[B_treat]
			local p`i' = 2*ttail(e(df_r),abs(`t`i''))
			local n`i': di %7.0f e(N)

			if `se`i''>0 local se`i' = "[`se`i'']"
			else  local se`i' = "`se`i''"

			if  `p`i'' < 0.1 &  `p`i'' >= 0.05  local b`i' `b`i''\sym{*}
			else if  `p`i'' < 0.05 &  `p`i'' >= 0.01  local b`i' `b`i''\sym{**}
			else if  `p`i'' < 0.01  local b`i' `b`i''\sym{***}

			* Control group mean
			su `pre'`index' if B_treat==0 & !mi(`pre'team_id) & attrition`attr'==0
			if abs(r(mean)) < .00005 local m`i': di %7.3f abs(r(mean))
			else local m`i': di %7.3f r(mean)

		}
	}

	if "`time'" == "el1" {

		cap file close regtab
		file open regtab using "$tables/`time'_primary_outcomes_extended_all_8.tex", write replace

		file write regtab "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\extracolsep{2pt}}{l}*{7}{>{\centering\arraybackslash}m{2.25cm}}@{}}  \toprule" _n      
		file write regtab "& \multicolumn{2}{c}{Both genders} & \multicolumn{3}{c}{Girls} & \multicolumn{2}{c}{Boys} \\  \cmidrule(lr){2-3} \cmidrule(lr){4-6} \cmidrule(lr){7-8}"	
		file write regtab " `header'  \\     " _n     
		file write regtab "  & (1) & (2) & (3) & (4) & (5) & (6) & (7) \\    " _n    
		file write regtab "\midrule" _n 

		file write regtab " Treated & `b1' & `b2' & `b3' & `b4' & `b5' & `b6' & `b7' \\    " _n 
		file write regtab "  & `se1' & `se2' & `se3' & `se4' & `se5' & `se6' & `se7'   \\ \addlinespace[3pt]   " _n 
		file write regtab " Control group mean & `m1' & `m2' & `m3' & `m4' & `m5' & `m6' & `m7' \\    " _n 
		file write regtab " Number of students & `n1' & `n2' & `n3' & `n4' & `n5' & `n6' & `n7' \\    " _n 

		file write regtab "\bottomrule" _n              
		file write regtab "\end{tabular} \\ " _n 
		file close regtab

	}

	else if "`time'" == "el2" {

		cap file close regtab
		file open regtab using "$tables/`time'_primary_outcomes_extended_all_8.tex", write replace

		file write regtab "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\extracolsep{2pt}}{l}*{11}{>{\centering\arraybackslash}m{1.75cm}}@{}}  \toprule" _n      
		file write regtab "& \multicolumn{3}{c}{Both genders} & \multicolumn{5}{c}{Girls} & \multicolumn{3}{c}{Boys} \\  \cmidrule(lr){2-4} \cmidrule(lr){5-9} \cmidrule(lr){10-12}"	
		file write regtab " `header'  \\     " _n     
		file write regtab "  & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10) & (11) \\    " _n    
		file write regtab "\midrule" _n 

		file write regtab " Treated & `b1' & `b2' & `b3' & `b4' & `b5' & `b6' & `b7' & `b8' & `b9' & `b10' & `b11' \\    " _n 
		file write regtab "  & `se1' & `se2' & `se3' & `se4' & `se5' & `se6' & `se7' & `se8' & `se9' & `se10' & `se11'   \\ \addlinespace[3pt]   " _n 
		file write regtab " Control group mean & `m1' & `m2' & `m3' & `m4' & `m5' & `m6' & `m7' & `m8' & `m9' & `m10' & `m11' \\    " _n 
		file write regtab " Number of students & `n1' & `n2' & `n3' & `n4' & `n5' & `n6' & `n7' & `n8' & `n9' & `n10' & `n11' \\    " _n 

		file write regtab "\bottomrule" _n              
		file write regtab "\end{tabular} \\ " _n 
		file close regtab

	}

	log close 
}


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	APPENDIX TABLE 1.9 : Treatment effects on individual gender attitudes 
*			   		   (with Bonferroni correction)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

* matching EL1 and EL2 vars
local E2_Swives_less_edu_n E_Swives_less_edu_n
local E2_Sboy_more_oppo_n E_Sboy_more_oppo_n
local E2_Stown_studies_y E_Stown_studies_y
local E2_Swoman_role_home_n E_Swoman_role_home_n
local E2_Smen_better_suited_n E_Smen_better_suited_n
local E2_Smarriage_more_imp_n E_Smarriage_more_imp_n
local E2_Steacher_suitable_n E_Steacher_suitable_n
local E2_Sallow_work E_Sallow_work_y
local E2_Ssimilar_right_y E_Ssimilar_right_y
local E2_Select_woman_y E_Select_woman_y
local E2_Sman_final_deci_n E_Sman_final_deci_n
local E2_Swoman_viol_n E_Swoman_viol_n
local E2_Scontrol_daughters_n E_Scontrol_daughters_n
local E2_Sstudy_marry E_Sstudy_marry
local E2_Sgirl_marriage_age_19 E_Sgirl_marriage_age_19
local E2_Smarriage_age_diff_m E_Smarriage_age_diff_m
local E2_Sfertility E_Sfertility

#delimit ;
local gender 
E2_Swives_less_edu_n E2_Sboy_more_oppo_n E2_Stown_studies_y 
E2_Swoman_role_home_n E2_Smen_better_suited_n E2_Smarriage_more_imp_n 
E2_Steacher_suitable_n E2_Sallow_work
E2_Ssimilar_right_y	E2_Select_woman_y E2_Sman_final_deci_n E2_Swoman_viol_n 	
E2_Scontrol_daughters_n E2_Sstudy_marry /* E2_Sshy E2_Slaugh */
E2_Sgirl_marriage_age_19 E2_Smarriage_age_diff_m
E2_Sfertility;

#delimit cr


la var E2_Sboy_more_oppo_n "Disagree: Boys should get more opportunities/resources for education"
la var E2_Stown_studies_y "If HH head, would send both children or girl for education"
la var E2_Swoman_role_home_n "Disagree: Woman's most important role is caring for home and children"
la var E2_Scontrol_daughters_n "Disagree: Parents should maintain stricter control over daughters than sons"
la var E2_Sfertility "Disagree: Keep having children if no sons yet but not if no daughters"
la var E2_Smarriage_age_diff_m "Difference between boys and girls age to marry is less than control median"
la var E2_Sallow_work "Agree: Women should be allowed to work outside home"
la var E2_Sstudy_marry "Has gender equal views on getting higher education for better marriage prospects"
la var E2_Sgirl_marriage_age_19 "Sister/female cousins/friends should be married after age 19"

foreach var in `gender' {
	local l: var lab `var'
	local newl = subinstr("`l'", ".", "", . )
	la var `var' "`newl'"	
}


local count: di wordcount("`gender'") // no. of variabls in gender attitudes index. 

cap file close sumstat
file open sumstat using "$tables/el1_el2_genatt_indvars.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{L{12cm}*{4}{>{\raggedright\arraybackslash}m{1.25cm}}@{}}  \toprule" _n      
file write sumstat "& \multicolumn{2}{l}{Endline 1} & \multicolumn{2}{l}{Endline 2} \\  \cmidrule(r){2-3} \cmidrule(r){4-5}"
file write sumstat " & Girls & Boys & Girls & Boys \\" _n
file write sumstat "\midrule" _n 

foreach var in `gender' {

	local l: var lab `var'	

	eststo el1 : reg ``var'' B_treat district_gender_* gender_grade_* B_Sgender_index2 B_Sgender_index2_flag B_Sgender_index2_m if !mi(E_Steam_id) & attrition_el==0 & ``var''_flag==0 & B_Sgirl==0, cluster(Sschool_id)
	local b1: di %7.3fc _b[B_treat]
	local t1 = _b[B_treat]/_se[B_treat]
	local p1 = 2*ttail(e(df_r),abs(`t1'))

	if  `p1' < 0.1/`count' &  `p1' >= 0.05/`count'  local b1 `b1'\sym{*}
	else if  `p1' < 0.05/`count' &  `p1' >= 0.01/`count'  local b1 `b1'\sym{**}
	else if  `p1' < 0.01/`count' local b1 `b1'\sym{***}

	eststo el1 : reg ``var'' B_treat district_gender_* gender_grade_* B_Sgender_index2 B_Sgender_index2_flag B_Sgender_index2_m if !mi(E_Steam_id) & attrition_el==0 & ``var''_flag==0 & B_Sgirl==1, cluster(Sschool_id)
	local b2: di %7.3fc _b[B_treat]
	local t2 = _b[B_treat]/_se[B_treat]
	local p2 = 2*ttail(e(df_r),abs(`t2'))

	if  `p2' < 0.1/`count' &  `p2' >= 0.05/`count'  local b2 `b2'\sym{*}
	else if  `p2' < 0.05/`count' &  `p2' >= 0.01/`count'  local b2 `b2'\sym{**}
	else if  `p2' < 0.01/`count' local b2 `b2'\sym{***}

	eststo el2 : reg `var' B_treat district_gender_* gender_grade_* B_Sgender_index2 B_Sgender_index2_flag B_Sgender_index2_m if !mi(E2_Steam_id) & attrition==0 & `var'_flag==0 & B_Sgirl==0, cluster(Sschool_id)
	
	local b3: di %7.3fc _b[B_treat]
	local t3 = _b[B_treat]/_se[B_treat]
	local p3 = 2*ttail(e(df_r),abs(`t3'))

	if  `p3' < 0.1/`count' &  `p3' >= 0.05/`count'  local b3 `b3'\sym{*}
	else if  `p3' < 0.05/`count' &  `p3' >= 0.01/`count'  local b3 `b3'\sym{**}
	else if  `p3' < 0.01/`count' local b3 `b3'\sym{***}

	eststo el2 : reg `var' B_treat district_gender_* gender_grade_* B_Sgender_index2 B_Sgender_index2_flag B_Sgender_index2_m if !mi(E2_Steam_id) & attrition==0 & `var'_flag==0 & B_Sgirl==1, cluster(Sschool_id)
	
	local b4: di %7.3fc _b[B_treat]
	local t4 = _b[B_treat]/_se[B_treat]
	local p4 = 2*ttail(e(df_r),abs(`t4'))

	if  `p4' < 0.1/`count' &  `p4' >= 0.05/`count'  local b4 `b4'\sym{*}
	else if  `p4' < 0.05/`count' &  `p4' >= 0.01/`count'  local b4 `b4'\sym{**}
	else if  `p4' < 0.01/`count' local b4 `b4'\sym{***}


	file write sumstat " `l' & `b2' & `b1' & `b4' & `b3' \\ \addlinespace[10pt]   " _n 

}

file write sumstat "\bottomrule" _n              
file write sumstat "\end{tabular}" _n 
file close sumstat	

*********** EL2 dowry

gen E2_Sdowry_girl_marriage_n = inlist(E2_Sdowry_girl_marriage, 4, 5) if !mi(E2_Sdowry_girl_marriage)
la var E2_Sdowry_girl_marriage_n "Disagree: Parent's should give dowry for girls' marriage"

su E2_Sdowry_girl_marriage_n if B_treat==1
local stat = 100* `r(mean)'
local stat: di  %7.4f `stat'
cap file write paperstat "4.3Medium-run results - 4.3.1 Effects on primary outcomes, EL2 % don't support for dowry in treatment group, `stat'%" _n

su E2_Sdowry_girl_marriage_n if B_treat==0
local stat = 100* `r(mean)'
local stat: di  %7.4f `stat'
cap file write paperstat "4.3Medium-run results - 4.3.1 Effects on primary outcomes, EL2 % don't support for dowry in control group, `stat'%" _n


reg E2_Sdowry_girl_marriage_n B_treat district_gender_* gender_grade_* B_Sgender_index2 B_Sgender_index2_flag B_Sgender_index2_m, cluster(Sschool_id)
local t = _b[B_treat]/_se[B_treat]
local p = 2*ttail(e(df_r),abs(`t'))

local stat: di %7.5fc `p'			
cap file write paperstat "4.3Medium-run results - 4.3.1 Effects on primary outcomes, EL2 no-support for dowry treated=control pval, `stat'   " _n 



* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
*	APPENDIX TABLE 1.11&1.18: Lee bounds on primary outcomes
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

foreach gender in girl boy both {

	log using "$ad_hoc/lee_bounds_`gender'", replace
	qui log off 

	use "$finaldata", clear
	set more off

	if "`gender'" == "girl" {
		keep if B_Sgirl == 1
		local vars gender_index2 aspiration_index2 behavior_index2
	}

	else if "`gender'" == "boy" {
		keep if B_Sgirl == 0
		local vars gender_index2 behavior_index2
	}

	else if "`gender'" == "both" {
		local vars gender_index2 aspiration_index2 behavior_index2
	}

	gen B_treat_l=B_treat
	gen B_treat_u=B_treat

	lab var B_treat_l "Treated (Lower bound)"
	lab var B_treat_u "Treated (Upper bound)"

	local tablerow B_treat B_treat_l B_treat_u

	local varlab1: var lab B_treat
	local varlab2: var lab B_treat_l
	local varlab3: var lab B_treat_u

	* aspirations index
	summ B_treat if B_treat==1 /*& !mi(E_Steam_id) & attrition_el==0*/ & B_Sgirl==1
	local gt_N = r(N) //count how many treatment observations we have
	summ B_treat if B_treat==0 /*& !mi(E_Steam_id) & attrition_el==0*/ & B_Sgirl==1
	local gc_N = r(N) //count how many control observations we have

	summ B_treat if B_treat==1 /*& !mi(E_Steam_id) & attrition_el==0*/
	local t_N = r(N) //count how many treatment observations we have
	summ B_treat if B_treat==0 /* & !mi(E_Steam_id) & attrition_el==0*/
	local c_N = r(N) //count how many control observations we have

	gen miss = 0		
	local i=0
	set seed 232016
	gen x=runiform() // to break Lee bounds ties

	//local vars gender_index2 aspiration_index2 behavior_index2
	eststo clear
	foreach v in `vars' { // the vars local has the outcome variables of interest

		gen TLO_`v' = 0		// obsns to trim for lower bound
		gen TUP_`v' = 0		// obsns to trim for upper bound
		gen TABS_`v' = 0	// obsns to trim for lower bound in absolute value terms

		* final total obs by treatment and control
		if "`v'"=="aspiration_index2" local ft_N = `gt_N'		
		else if "`v'"!="aspiration_index2" local ft_N = `t_N'		
		
		if "`v'"=="aspiration_index2" local fc_N = `gc_N'		
		else if "`v'"!="aspiration_index2" local fc_N = `c_N'		

		reg E_S`v' B_treat B_S`v' B_S`v'_flag /* B_S`v'_m */ ${el_``v''_flag}  district_gender_* gender_grade_* if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)

		local posneg = sign(_b[B_treat])	// Is treatment effect with untrimmed sample positive or negative?
		replace miss = (E_S`v'== .)		// This is so when sorting data below and using _n, don't have to worry if Stata puts missing values first or last
		
		summ E_S`v' if B_treat==1 & !mi(E_Steam_id) & attrition_el==0
		local t_n = r(N)
		local t_rate = `t_n'/`ft_N'		// retention rate for treatment group

		summ E_S`v' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
		local c_n = r(N)
		local c_rate = `c_n'/`fc_N'		// retention rate for control group

		local trim_rate = abs(`c_rate' - `t_rate')	// % of obsns to trim
		qui log on
		di _n "Endline 1 `v' Trim rate:  `trim_rate'" _n
		qui log off
		
		if `c_rate' > `t_rate' {  // trim from control group
			local trim_n = round(`trim_rate'* `fc_N')	// number of obsns to trim

			cap confirm variable B_S`v'			
			if _rc gsort miss B_treat E_S`v' x						// want to tag observations in either treat or control group that are non-missing and high or low values
			else gsort miss B_treat E_S`v' -B_S`v' x
			by miss B_treat: replace TLO_`v' = 1 if _n <= `trim_n' & miss==0 & B_treat==0 & !mi(E_Steam_id) & attrition_el==0	// trim low values from control group to get lower bound
			cap confirm variable B_S`v'
			if _rc gsort miss B_treat -E_S`v' x
			else gsort miss B_treat -E_S`v' B_S`v' x
			by miss B_treat: replace TUP_`v' = 1 if _n <= `trim_n' & miss==0 & B_treat==0 & !mi(E_Steam_id) & attrition_el==0
		}

		if `c_rate' < `t_rate' {  // trim from treat group
			local trim_n = round(`trim_rate'* `ft_N')	// number of obsns to trim
			di `trim_n'
			cap confirm variable B_S`v'			
			if _rc gsort miss B_treat E_S`v' x						// want to tag observations in either treat or control group that are non-missing and high or low values
			else gsort miss B_treat E_S`v' -B_S`v' x
			by miss B_treat: replace TUP_`v' = 1 if _n <= `trim_n' & miss==0 & B_treat==1 & !mi(E_Steam_id) & attrition_el==0	// trim low values from treat group to get upper bound
			cap confirm variable B_S`v'
			if _rc gsort miss B_treat -E_S`v' x 
			else gsort miss B_treat -E_S`v' B_S`v' x
			by miss B_treat: replace TLO_`v' = 1 if _n <= `trim_n' & miss==0 & B_treat==1 & !mi(E_Steam_id) & attrition_el==0
		}		

		* Lower bound in absolute value terms depends on whether coefficient is positive or negative (unlikely to be exactly 0, but if so, full sample gives lower abs value bound)
		if `posneg'==1 {
			replace TABS_`v' = TLO_`v' 	// if treatment effect is positive, lower bound in absolute value terms is actual lower bound

		}
		if `posneg'==-1 {
			replace TABS_`v' = TUP_`v' // if treatment effect is negative, lower bound in absolute value terms is actual upper bound
		}		

		cap confirm variable B_S`v'		// Check if we have baseline outcome
		di _n "*****   Full sample   *****" _n
		reg E_S`v' B_treat B_S`v' B_S`v'_flag /* B_S`v'_m */ ${el_``v''_flag} district_gender_* gender_grade_* if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) 
		local cluster_`v': di %7.0fc `e(N_clust)'
		local obs_`v': di %7.0fc `e(N)'
		local treat_`v': di %7.3fc _b[B_treat]
		local se_`v': di %7.3fc _se[B_treat]		
		local t_`v' = _b[B_treat]/_se[B_treat]
		scalar p_`v' = 2*ttail(e(df_r),abs(`t_`v''))
		dis p_`v'

		di _n "*****   Lower bound   *****" _n
		reg E_S`v' B_treat_l B_S`v' B_S`v'_flag /* B_S`v'_m */ ${el_``v''_flag} district_gender_* gender_grade_* if TLO_`v'==0 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)

		if "`v'"=="gender_index2" & "`gender'" == "both" {
			local stat: di %7.4fc _b[B_treat_l]
			cap file write paperstat "4.2Short-run results pg15, EL1 gender attitudes SD (lower bound), `stat'   " _n 
		}
		local obs1_`v' : di %7.0fc `e(N)'
		local treat_l_`v': di %7.3fc _b[B_treat_l]
		local se_l_`v': di %7.3fc _se[B_treat_l]
		local t_l_`v' = _b[B_treat_l]/_se[B_treat_l]
		scalar p_l_`v' = 2*ttail(e(df_r),abs(`t_l_`v''))


		di _n "*****   Upper bound   *****" _n
		reg E_S`v' B_treat_u B_S`v' B_S`v'_flag /* B_S`v'_m */ ${el_``v''_flag}  district_gender_* gender_grade_* if TUP_`v'==0 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
		local treat_u_`v': di %7.3fc _b[B_treat_u]
		local se_u_`v': di %7.3fc _se[B_treat_u]
		local t_u_`v' = _b[B_treat_u]/ _se[B_treat_u]
		scalar p_u_`v' = 2*ttail(e(df_r),abs(`t_u_`v''))


		di _n "*****   Bound on magnitude   *****" _n
		reg E_S`v' B_treat B_S`v' B_S`v'_flag  /* B_S`v'_m */ ${el_``v''_flag}  district_gender_* gender_grade_* if TABS_`v'==0 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
		
		
		if p_`v'< 0.01 {
			local treat_`v' `treat_`v''\sym{***}
		}
		else if 0.01 <= p_`v' & p_`v' < 0.05 {
			local treat_`v' `treat_`v''\sym{**}
		}
		else if 0.05 <= p_`v' & p_`v' < 0.1 {
			local treat_`v' `treat_`v''\sym{*}
		}
		else if p_`v'> 0.1 {
			local treat_`v' `treat_`v''
		}
		
		if p_l_`v'< 0.01 {
			local treat_l_`v' `treat_l_`v''\sym{***}
		}
		else if 0.01<= p_l_`v' & p_l_`v' < 0.05 {
			local treat_l_`v' `treat_l_`v''\sym{**}
		}
		else if 0.05<= p_l_`v' & p_l_`v' < 0.1 {
			local treat_l_`v' `treat_l_`v''\sym{*}
		}
		else if p_l_`v'> 0.1 {
			local treat_l_`v' `treat_l_`v''
		}
		
		if p_u_`v'< 0.01 {
			local treat_u_`v' `treat_u_`v''\sym{***}
		}
		else if 0.01<= p_u_`v' & p_u_`v' < 0.05 {
			local treat_u_`v' `treat_u_`v''\sym{**}
		}
		else if 0.05<= p_u_`v' & p_u_`v' < 0.1 {
			local treat_u_`v' `treat_u_`v''\sym{*}
		}
		else if p_u_`v'> 0.1 {
			local treat_u_`v' `treat_u_`v''
		}
	}

	local se se_gender_index2 se_aspiration_index2 se_behavior_index2 ///
	se_l_gender_index2 se_l_aspiration_index2 se_l_behavior_index2 ///
	se_u_gender_index2 se_u_aspiration_index2 se_u_behavior_index2

	foreach x in `se' {
		local `x'=trim(string(``x'',"%7.3f"))
	}


	* header and no. of columns
	eststo clear
	local header
	local n_col =0
	foreach index in gender_index2 aspiration_index2 behavior_index2 scholar_index2 petition_index2  {

		local varlab: variable label E2_S`index'
		local header "`header' & `varlab'" // column headers with variables labels 
		
		local ++n_col // number of columns		
		
	}

	if "`gender'" == "both" {
		** separate table for EL1
		cap file close regtab
		file open regtab using "$tables/el1_primary_leebounds.tex", write replace

		file write regtab "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{3}{>{\centering\arraybackslash}m{2.5cm}}@{}}   \toprule \\" _n      // table header
		file write regtab    " & Gender attitudes index & Girls' aspirations index & Self-reported behavior index   \\     " _n     
		file write regtab "  & (1) & (2) & (3)  \\    " _n    
		file write regtab "\midrule" _n 

		file write regtab "`varlab1' & `treat_gender_index2' & `treat_aspiration_index2'  & `treat_behavior_index2'  \\  " _n
		file write regtab "  & [`se_gender_index2'] & [`se_aspiration_index2'] & [`se_behavior_index2']  \\ \addlinespace[3pt] " _n
		file write regtab "`varlab2' & `treat_l_gender_index2' & `treat_l_aspiration_index2' & `treat_l_behavior_index2' \\  " _n
		file write regtab "  & [`se_l_gender_index2'] & [`se_l_aspiration_index2'] & [`se_l_behavior_index2']  \\ \addlinespace[3pt] " _n
		file write regtab "`varlab3' & `treat_u_gender_index2' & `treat_u_aspiration_index2' & `treat_u_behavior_index2'  \\  " _n
		file write regtab "  & [`se_u_gender_index2'] & [`se_u_aspiration_index2'] & [`se_u_behavior_index2'] \\ \addlinespace[3pt] " _n

		file write regtab "Observations & `obs_gender_index2' & `obs_aspiration_index2' & `obs_behavior_index2'  \\ " _n
		file write regtab "Observations (Lee bounds) & `obs1_gender_index2' & `obs1_aspiration_index2' & `obs1_behavior_index2' \\ " _n

		file write regtab "\bottomrule" _n           // table footer
		file write regtab "\end{tabular}" _n 
		file close regtab

		** separate table for EL1 slides
		cap file close regtab
		file open regtab using "$slides/el1_primary_leebounds.tex", write replace

		file write regtab "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{3}{>{\centering\arraybackslash}m{2.5cm}}@{}}   \toprule \\" _n      // table header
		file write regtab    " & Gender attitudes index & Girls' aspirations index & Self-reported behavior index   \\     " _n     
		file write regtab "  & (1) & (2) & (3)  \\    " _n    
		file write regtab "\midrule" _n 

		file write regtab "`varlab1' & `treat_gender_index2' & `treat_aspiration_index2'  & `treat_behavior_index2'  \\  " _n
		file write regtab "  & [`se_gender_index2'] & [`se_aspiration_index2'] & [`se_behavior_index2']  \\ \addlinespace[3pt] " _n
		file write regtab "`varlab2' & `treat_l_gender_index2' & `treat_l_aspiration_index2' & `treat_l_behavior_index2' \\  " _n
		file write regtab "  & [`se_l_gender_index2'] & [`se_l_aspiration_index2'] & [`se_l_behavior_index2']  \\ \addlinespace[3pt] " _n
		file write regtab "`varlab3' & `treat_u_gender_index2' & `treat_u_aspiration_index2' & `treat_u_behavior_index2'  \\  " _n
		file write regtab "  & [`se_u_gender_index2'] & [`se_u_aspiration_index2'] & [`se_u_behavior_index2'] \\ \addlinespace[3pt] " _n

		file write regtab "\midrule" _n 
		file write regtab "Observations & `obs_gender_index2' & `obs_aspiration_index2' & `obs_behavior_index2'  \\ " _n
		file write regtab "Observations (Lee bounds) & `obs1_gender_index2' & `obs1_aspiration_index2' & `obs1_behavior_index2' \\ " _n

		file write regtab "\bottomrule" _n           // table footer
		file write regtab "\end{tabular}" _n 
		file close regtab
	}

	foreach var in gender_index2 aspiration_index2 behavior_index2 {
		local el1_treat_`var' `treat_`var''
		local el1_se_`var' `se_`var''
		local el1_treat_l_`var' `treat_l_`var''
		local el1_se_l_`var' `se_l_`var''
		local el1_treat_u_`var' `treat_u_`var''
		local el1_se_u_`var' `se_u_`var''
		local el1_obs_`var' `obs_`var''
		local el1_obs1_`var' `obs1_`var''
	}



	** Endline 2 *****

	use "$finaldata", clear

	if "`gender'" == "girl" {
		keep if B_Sgirl == 1
		local vars gender_index2 aspiration_index2 behavior_index2 scholar_index2 petition_index2
	}

	else if "`gender'" == "boy" {
		keep if B_Sgirl == 0
		local vars gender_index2 behavior_index2 petition_index2
	}

	else if "`gender'" == "both" {
		local vars gender_index2 aspiration_index2 behavior_index2 scholar_index2 petition_index2
	}

	gen B_treat_l=B_treat
	gen B_treat_u=B_treat

	lab var B_treat_l "Treated (Lower bound)"
	lab var B_treat_u "Treated (Upper bound)"

	gen miss = 0		
	local i=0
	gen x=runiform() // to brake Lee bounds ties

	//local vars gender_index2 aspiration_index2 behavior_index2 scholar_index2 petition_index2
	eststo clear
	foreach v in `vars' { // the vars local has the outcome variables of interest

		gen TLO_`v' = 0		// obsns to trim for lower bound
		gen TUP_`v' = 0		// obsns to trim for upper bound
		gen TABS_`v' = 0	// obsns to trim for lower bound in absolute value terms

		* final total obs by treatment and control
		if "`v'"=="aspiration_index2" | "`v'"=="scholar_index2"  local ft_N = `gt_N'		
		else local ft_N = `t_N'		
		
		if "`v'"=="aspiration_index2" | "`v'"=="scholar_index2" local fc_N = `gc_N'		
		else local fc_N = `c_N'		
		
		* sign of treatment variable
		if  "`v'"=="petition_index2" | "`v'"=="scholar_index2"  {
			reg E2_S`v' B_treat ${el2_``v''_flag} district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id)
		}
		
		else   {
			reg E2_S`v' B_treat B_S`v'  B_S`v'_flag /* B_S`v'_m */ ${el2_``v''_flag}  district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id)
		}
		
		
		local posneg = sign(_b[B_treat])	// Is treatment effect with untrimmed sample positive or negative?
		replace miss = (E2_S`v'== .)		// This is so when sorting data below and using _n, don't have to worry if Stata puts missing values first or last
		
		summ E2_S`v' if B_treat==1 & !mi(E2_Steam_id) & attrition==0
		local t_n = r(N)
		local t_rate = `t_n'/`ft_N'		// retention rate for treatment group

		summ E2_S`v' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
		local c_n = r(N)
		local c_rate = `c_n'/`fc_N'		// retention rate for control group

		local trim_rate = abs(`c_rate' - `t_rate')	// % of obsns to trim
		qui log on 
		di _n "Endline 2 `v' Trim rate:  `trim_rate'" _n
		qui log off 

		if  "`v'"=="petition_index2" | "`v'"=="scholar_index2"  {

			if `c_rate' > `t_rate' {  // trim from control group
				local trim_n = round(`trim_rate'* `fc_N')	// number of obsns to trim			

				if _rc gsort miss B_treat E2_S`v' x						// want to tag observations in either treat or control group that are non-missing and high or low values
				else gsort miss B_treat E2_S`v' 
				by miss B_treat: replace TLO_`v' = 1 if _n <= `trim_n' & miss==0 & B_treat==0 & !mi(E2_Steam_id) & attrition==0	// trim low values from control group to get lower bound
				if _rc gsort miss B_treat -E2_S`v' x
				else gsort miss B_treat -E2_S`v'
				by miss B_treat: replace TUP_`v' = 1 if _n <= `trim_n' & miss==0 & B_treat==0 & !mi(E2_Steam_id) & attrition==0
			}

			if `c_rate' < `t_rate' {  // trim from treat group
				local trim_n = round(`trim_rate'* `ft_N')	// number of obsns to trim
				di `trim_n'
				if _rc gsort miss B_treat E2_S`v' x						// want to tag observations in either treat or control group that are non-missing and high or low values
				else gsort miss B_treat E2_S`v' 
				by miss B_treat: replace TUP_`v' = 1 if _n <= `trim_n' & miss==0 & B_treat==1 & !mi(E2_Steam_id) & attrition==0	// trim low values from treat group to get upper bound
				if _rc gsort miss B_treat -E2_S`v' x 
				else gsort miss B_treat -E2_S`v' x
				by miss B_treat: replace TLO_`v' = 1 if _n <= `trim_n' & miss==0 & B_treat==1 & !mi(E2_Steam_id) & attrition==0
			}	
		}

		else {
			if `c_rate' > `t_rate' {  // trim from control group
				local trim_n = round(`trim_rate'* `fc_N')	// number of obsns to trim
				cap confirm variable B_S`v'			
				if _rc gsort miss B_treat E2_S`v' x						// want to tag observations in either treat or control group that are non-missing and high or low values
				else gsort miss B_treat E2_S`v' -B_S`v' x
				by miss B_treat: replace TLO_`v' = 1 if _n <= `trim_n' & miss==0 & B_treat==0 & !mi(E2_Steam_id) & attrition==0	// trim low values from control group to get lower bound
				cap confirm variable B_S`v'
				if _rc gsort miss B_treat -E2_S`v' x
				else gsort miss B_treat -E2_S`v' B_S`v' x
				by miss B_treat: replace TUP_`v' = 1 if _n <= `trim_n' & miss==0 & B_treat==0 & !mi(E2_Steam_id) & attrition==0
			}

			if `c_rate' < `t_rate' {  // trim from treat group
				local trim_n = round(`trim_rate'* `ft_N')	// number of obsns to trim
				di `trim_n'
				cap confirm variable B_S`v'			
				if _rc gsort miss B_treat E2_S`v' x						// want to tag observations in either treat or control group that are non-missing and high or low values
				else gsort miss B_treat E2_S`v' -B_S`v' x
				by miss B_treat: replace TUP_`v' = 1 if _n <= `trim_n' & miss==0 & B_treat==1 & !mi(E2_Steam_id) & attrition==0	// trim low values from treat group to get upper bound
				cap confirm variable B_S`v'
				if _rc gsort miss B_treat -E2_S`v' x 
				else gsort miss B_treat -E2_S`v' B_S`v' x
				by miss B_treat: replace TLO_`v' = 1 if _n <= `trim_n' & miss==0 & B_treat==1 & !mi(E2_Steam_id) & attrition==0
			}	
		}


		* Lower bound in absolute value terms depends on whether coefficient is positive or negative (unlikely to be exactly 0, but if so, full sample gives lower abs value bound)
		if `posneg'==1 {
			replace TABS_`v' = TLO_`v' 	// if treatment effect is positive, lower bound in absolute value terms is actual lower bound

		}
		if `posneg'==-1 {
			replace TABS_`v' = TUP_`v' // if treatment effect is negative, lower bound in absolute value terms is actual upper bound
		}		
		
		if  "`v'"=="petition_index2" | "`v'"=="scholar_index2"  {
			di _n "*****   Full sample   *****" _n
			reg E2_S`v' B_treat  district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) 
			local cluster_`v': di %7.0fc `e(N_clust)'
			local obs_`v': di %7.0fc `e(N)'
			local treat_`v': di %7.3fc _b[B_treat]
			local se_`v'= trim(string(_se[B_treat],"%7.3f"))
			local t_`v' = _b[B_treat]/_se[B_treat]
			scalar p_`v' = 2*ttail(e(df_r),abs(`t_`v''))
			dis p_`v'

			di _n "*****   Lower bound   *****" _n
			reg E2_S`v' B_treat_l   district_gender_* gender_grade_* if TLO_`v'==0 & !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id)
			local obs1_`v' : di %7.0fc `e(N)'
			local treat_l_`v': di %7.3fc _b[B_treat_l]
			local se_l_`v'= trim(string(_se[B_treat_l],"%7.3f"))
			local t_l_`v' = _b[B_treat_l]/_se[B_treat_l]
			scalar p_l_`v' = 2*ttail(e(df_r),abs(`t_l_`v''))

			
			di _n "*****   Upper bound   *****" _n
			reg E2_S`v' B_treat_u    district_gender_* gender_grade_* if TUP_`v'==0 & !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id)
			local treat_u_`v': di %7.3fc _b[B_treat_u]
			local se_u_`v'= trim(string(_se[B_treat_u],"%7.3f"))
			local t_u_`v' = _b[B_treat_u]/ _se[B_treat_u]
			scalar p_u_`v' = 2*ttail(e(df_r),abs(`t_u_`v''))


			di _n "*****   Bound on magnitude   *****" _n
			reg E2_S`v' B_treat   district_gender_* gender_grade_* if TABS_`v'==0 & !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id)

		}

		else {
			cap confirm variable B_S`v'		// Check if we have baseline outcome
			di _n "*****   Full sample   *****" _n
			reg E2_S`v' B_treat B_S`v'  B_S`v'_flag  /* B_S`v'_m */ ${el2_``v''_flag} district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) 
			local cluster_`v': di %7.0fc `e(N_clust)'
			local obs_`v': di %7.0fc `e(N)'
			local treat_`v': di %7.3fc _b[B_treat]
			local se_`v'= trim(string(_se[B_treat],"%7.3f"))
			local t_`v' = _b[B_treat]/_se[B_treat]
			scalar p_`v' = 2*ttail(e(df_r),abs(`t_`v''))
			dis p_`v'

			di _n "*****   Lower bound   *****" _n
			reg E2_S`v' B_treat_l B_S`v'  B_S`v'_flag /* B_S`v'_m */ ${el2_``v''_flag} district_gender_* gender_grade_* if TLO_`v'==0 & !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id)
			local obs1_`v' : di %7.0fc `e(N)'
			local treat_l_`v': di %7.3fc _b[B_treat_l]
			local se_l_`v'= trim(string(_se[B_treat_l],"%7.3f"))
			local t_l_`v' = _b[B_treat_l]/_se[B_treat_l]
			scalar p_l_`v' = 2*ttail(e(df_r),abs(`t_l_`v''))			

			
			di _n "*****   Upper bound   *****" _n
			reg E2_S`v' B_treat_u B_S`v'  B_S`v'_flag /* B_S`v'_m */ ${el2_``v''_flag}  district_gender_* gender_grade_* if TUP_`v'==0 & !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id)
			local treat_u_`v': di %7.3fc _b[B_treat_u]
			local se_u_`v'= trim(string(_se[B_treat_u],"%7.3f"))
			local t_u_`v' = _b[B_treat_u]/ _se[B_treat_u]
			scalar p_u_`v' = 2*ttail(e(df_r),abs(`t_u_`v''))


			di _n "*****   Bound on magnitude   *****" _n
			reg E2_S`v' B_treat B_S`v'  B_S`v'_flag /* B_S`v'_m */ ${el2_``v''_flag} district_gender_* gender_grade_* if TABS_`v'==0 & !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id)
		}

		if p_`v'< 0.01 {
			local treat_`v' `treat_`v''\sym{***}
		}
		else if 0.01 <= p_`v' & p_`v' < 0.05 {
			local treat_`v' `treat_`v''\sym{**}
		}
		else if 0.05 <= p_`v' & p_`v' < 0.1 {
			local treat_`v' `treat_`v''\sym{*}
		}
		else if p_`v'> 0.1 {
			local treat_`v' `treat_`v''
		}

		if p_l_`v'< 0.01 {
			local treat_l_`v' `treat_l_`v''\sym{***}
		}
		else if 0.01<= p_l_`v' & p_l_`v' < 0.05 {
			local treat_l_`v' `treat_l_`v''\sym{**}
		}
		else if 0.05<= p_l_`v' & p_l_`v' < 0.1 {
			local treat_l_`v' `treat_l_`v''\sym{*}
		}
		else if p_l_`v'> 0.1 {
			local treat_l_`v' `treat_l_`v''
		}

		if p_u_`v'< 0.01 {
			local treat_u_`v' `treat_u_`v''\sym{***}
		}
		else if 0.01<= p_u_`v' & p_u_`v' < 0.05 {
			local treat_u_`v' `treat_u_`v''\sym{**}
		}
		else if 0.05<= p_u_`v' & p_u_`v' < 0.1 {
			local treat_u_`v' `treat_u_`v''\sym{*}
		}
		else if p_u_`v'> 0.1 {
			local treat_u_`v' `treat_u_`v''
		}
	}


	if "`gender'" == "both" {

		*** combined table
		cap file close regtab
		file open regtab using "$tables/el1_el2_primary_leebounds_combined.tex", write replace
		file write regtab "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\extracolsep{1pt}}{l}*{8}{>{\centering\arraybackslash}m{1.7cm}}@{}}  \toprule" _n      
		file write regtab "& \multicolumn{3}{c}{Endline 1} & \multicolumn{5}{c}{Endline 2} \\  \cmidrule(lr){2-4} \cmidrule(lr){5-9}"	
		file write regtab " & Gender attitudes index & Girls' aspirations index & Self-reported behavior index & Gender attitudes index & Girls' aspirations index & Self-reported behavior index & Applied to scholarship (girls) & Signed petition  \\     " _n     
		file write regtab "  & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\    " _n    
		file write regtab "\midrule" _n 

		file write regtab "`varlab1' & `el1_treat_gender_index2' & `el1_treat_aspiration_index2'  & `el1_treat_behavior_index2' & `treat_gender_index2' & `treat_aspiration_index2'  & `treat_behavior_index2'  & `treat_scholar_index2' & `treat_petition_index2' \\  " _n
		file write regtab "  & [`el1_se_gender_index2'] & [`el1_se_aspiration_index2'] & [`el1_se_behavior_index2'] & [`se_gender_index2'] & [`se_aspiration_index2'] & [`se_behavior_index2']  & [`se_scholar_index2'] & [`se_petition_index2'] \\ \addlinespace[3pt] " _n
		file write regtab "`varlab2' & `el1_treat_l_gender_index2' & `el1_treat_l_aspiration_index2' & `el1_treat_l_behavior_index2' & `treat_l_gender_index2' & `treat_l_aspiration_index2' & `treat_l_behavior_index2'  & `treat_l_scholar_index2' & `treat_l_petition_index2' \\  " _n
		file write regtab "  & [`el1_se_l_gender_index2'] & [`el1_se_l_aspiration_index2'] & [`el1_se_l_behavior_index2']  & [`se_l_gender_index2'] & [`se_l_aspiration_index2'] & [`se_l_behavior_index2']  & [`se_l_scholar_index2'] & [`se_l_petition_index2']  \\ \addlinespace[3pt] " _n
		file write regtab "`varlab3' & `el1_treat_u_gender_index2' & `el1_treat_u_aspiration_index2' & `el1_treat_u_behavior_index2' & `treat_u_gender_index2' & `treat_u_aspiration_index2' & `treat_u_behavior_index2'  & `treat_u_scholar_index2' & `treat_u_petition_index2' \\  " _n
		file write regtab "  & [`el1_se_u_gender_index2'] & [`el1_se_u_aspiration_index2'] & [`el1_se_u_behavior_index2'] & [`se_u_gender_index2'] & [`se_u_aspiration_index2'] & [`se_u_behavior_index2'] & [`se_u_scholar_index2'] & [`se_u_petition_index2']  \\ \addlinespace[3pt] " _n

		file write regtab "Observations & `el1_obs_gender_index2' & `el1_obs_aspiration_index2' & `el1_obs_behavior_index2' & `obs_gender_index2' & `obs_aspiration_index2' & `obs_behavior_index2'  & `obs_scholar_index2' & `obs_petition_index2'  \\ " _n
		file write regtab "Observations (Lee bounds) & `el1_obs1_gender_index2' & `el1_obs1_aspiration_index2' & `el1_obs1_behavior_index2' & `obs1_gender_index2' & `obs1_aspiration_index2' & `obs1_behavior_index2'  & `obs1_scholar_index2' & `obs1_petition_index2'  \\ " _n

		file write regtab "\bottomrule" _n           // table footer
		file write regtab "\end{tabular}" _n 
		file close regtab

		** separate table for EL2
		cap file close regtab
		file open regtab using "$tables/el2_primary_leebounds.tex", write replace

		file write regtab "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{5}{>{\centering\arraybackslash}m{2.5cm}}@{}}   \toprule \\" _n      // table header
		file write regtab    " & Gender attitudes index & Girls' aspirations index & Self-reported behavior index  & Applied to scholarship (girls) & Signed petition  \\     " _n     
		file write regtab "  & (1) & (2) & (3) & (4) & (5)  \\    " _n    
		file write regtab "\midrule" _n 

		file write regtab "`varlab1' & `treat_gender_index2' & `treat_aspiration_index2'  & `treat_behavior_index2'  & `treat_scholar_index2' & `treat_petition_index2' \\  " _n
		file write regtab "  & [`se_gender_index2'] & [`se_aspiration_index2'] & [`se_behavior_index2']  & [`se_scholar_index2'] & [`se_petition_index2'] \\ \addlinespace[3pt] " _n
		file write regtab "`varlab2' & `treat_l_gender_index2' & `treat_l_aspiration_index2' & `treat_l_behavior_index2'  & `treat_l_scholar_index2' & `treat_l_petition_index2' \\  " _n
		file write regtab "  & [`se_l_gender_index2'] & [`se_l_aspiration_index2'] & [`se_l_behavior_index2']  & [`se_l_scholar_index2'] & [`se_l_petition_index2']  \\ \addlinespace[3pt] " _n
		file write regtab "`varlab3' & `treat_u_gender_index2' & `treat_u_aspiration_index2' & `treat_u_behavior_index2'  & `treat_u_scholar_index2' & `treat_u_petition_index2' \\  " _n
		file write regtab "  & [`se_u_gender_index2'] & [`se_u_aspiration_index2'] & [`se_u_behavior_index2'] & [`se_u_scholar_index2'] & [`se_u_petition_index2']  \\ \addlinespace[3pt] " _n

		file write regtab "Observations & `obs_gender_index2' & `obs_aspiration_index2' & `obs_behavior_index2'  & `obs_scholar_index2' & `obs_petition_index2'  \\ " _n
		file write regtab "Observations (Lee bounds) & `obs1_gender_index2' & `obs1_aspiration_index2' & `obs1_behavior_index2'  & `obs1_scholar_index2' & `obs1_petition_index2'  \\ " _n

		file write regtab "\bottomrule" _n           // table footer
		file write regtab "\end{tabular}" _n 
		file close regtab

		** slides
		cap file close regtab
		file open regtab using "$slides/el2_primary_leebounds.tex", write replace

		file write regtab "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{5}{>{\centering\arraybackslash}m{2.5cm}}@{}}   \toprule \\" _n      // table header
		file write regtab    " & Gender attitudes index & Girls' aspirations index & Self-reported behavior index & Applied to scholarship (girls) & Signed petition  \\     " _n     
		file write regtab "  & (1) & (2) & (3) & (4) & (5)  \\    " _n    
		file write regtab "\midrule" _n 

		file write regtab "`varlab1' & `treat_gender_index2' & `treat_aspiration_index2'  & `treat_behavior_index2'  & `treat_scholar_index2' & `treat_petition_index2' \\  " _n
		file write regtab "  & [`se_gender_index2'] & [`se_aspiration_index2'] & [`se_behavior_index2']  & [`se_scholar_index2'] & [`se_petition_index2'] \\ \addlinespace[3pt] " _n
		file write regtab "`varlab2' & `treat_l_gender_index2' & `treat_l_aspiration_index2' & `treat_l_behavior_index2'  & `treat_l_scholar_index2' & `treat_l_petition_index2' \\  " _n
		file write regtab "  & [`se_l_gender_index2'] & [`se_l_aspiration_index2'] & [`se_l_behavior_index2']  & [`se_l_scholar_index2'] & [`se_l_petition_index2'] \\ \addlinespace[3pt] " _n
		file write regtab "`varlab3' & `treat_u_gender_index2' & `treat_u_aspiration_index2' & `treat_u_behavior_index2'  & `treat_u_scholar_index2' & `treat_u_petition_index2' \\  " _n
		file write regtab "  & [`se_u_gender_index2'] & [`se_u_aspiration_index2'] & [`se_u_behavior_index2'] & [`se_u_scholar_index2'] & [`se_u_petition_index2']  \\ \addlinespace[3pt] " _n

		file write regtab "\midrule" _n 
		file write regtab "Observations & `obs_gender_index2' & `obs_aspiration_index2' & `obs_behavior_index2'  & `obs_scholar_index2' & `obs_petition_index2' \\ " _n
		file write regtab "Observations (Lee bounds) & `obs1_gender_index2' & `obs1_aspiration_index2' & `obs1_behavior_index2'  & `obs1_scholar_index2' & `obs1_petition_index2' \\ " _n

		file write regtab "\bottomrule" _n           // table footer
		file write regtab "\end{tabular}" _n 
		file close regtab
	}

	else if "`gender'" == "girl" {
		*** combined table
		cap file close regtab
		file open regtab using "$tables/el1_el2_primary_leebounds.tex", write replace
		file write regtab "\begin{flushleft} Panel A: Girls \end{flushleft}"
		file write regtab "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\extracolsep{1pt}}{l}*{8}{>{\centering\arraybackslash}m{1.7cm}}@{}}  \toprule" _n      
		file write regtab "& \multicolumn{3}{c}{Endline 1} & \multicolumn{5}{c}{Endline 2} \\  \cmidrule(lr){2-4} \cmidrule(lr){5-9}"	
		file write regtab " & Gender attitudes index & Girls' aspirations index & Self-reported behavior index & Gender attitudes index & Girls' aspirations index & Self-reported behavior index & Applied to scholarship (girls) & Signed petition  \\     " _n     
		file write regtab "  & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\    " _n    
		file write regtab "\midrule" _n 

		file write regtab "`varlab1' & `el1_treat_gender_index2' & `el1_treat_aspiration_index2'  & `el1_treat_behavior_index2' & `treat_gender_index2' & `treat_aspiration_index2'  & `treat_behavior_index2'  & `treat_scholar_index2' & `treat_petition_index2' \\  " _n
		file write regtab "  & [`el1_se_gender_index2'] & [`el1_se_aspiration_index2'] & [`el1_se_behavior_index2'] & [`se_gender_index2'] & [`se_aspiration_index2'] & [`se_behavior_index2']  & [`se_scholar_index2'] & [`se_petition_index2'] \\ \addlinespace[3pt] " _n
		file write regtab "`varlab2' & `el1_treat_l_gender_index2' & `el1_treat_l_aspiration_index2' & `el1_treat_l_behavior_index2' & `treat_l_gender_index2' & `treat_l_aspiration_index2' & `treat_l_behavior_index2'  & `treat_l_scholar_index2' & `treat_l_petition_index2' \\  " _n
		file write regtab "  & [`el1_se_l_gender_index2'] & [`el1_se_l_aspiration_index2'] & [`el1_se_l_behavior_index2']  & [`se_l_gender_index2'] & [`se_l_aspiration_index2'] & [`se_l_behavior_index2']  & [`se_l_scholar_index2'] & [`se_l_petition_index2']  \\ \addlinespace[3pt] " _n
		file write regtab "`varlab3' & `el1_treat_u_gender_index2' & `el1_treat_u_aspiration_index2' & `el1_treat_u_behavior_index2' & `treat_u_gender_index2' & `treat_u_aspiration_index2' & `treat_u_behavior_index2'  & `treat_u_scholar_index2' & `treat_u_petition_index2' \\  " _n
		file write regtab "  & [`el1_se_u_gender_index2'] & [`el1_se_u_aspiration_index2'] & [`el1_se_u_behavior_index2'] & [`se_u_gender_index2'] & [`se_u_aspiration_index2'] & [`se_u_behavior_index2'] & [`se_u_scholar_index2'] & [`se_u_petition_index2']  \\ \addlinespace[3pt] " _n

		file write regtab "Observations & `el1_obs_gender_index2' & `el1_obs_aspiration_index2' & `el1_obs_behavior_index2' & `obs_gender_index2' & `obs_aspiration_index2' & `obs_behavior_index2'  & `obs_scholar_index2' & `obs_petition_index2'  \\ " _n
		file write regtab "Observations (Lee bounds) & `el1_obs1_gender_index2' & `el1_obs1_aspiration_index2' & `el1_obs1_behavior_index2' & `obs1_gender_index2' & `obs1_aspiration_index2' & `obs1_behavior_index2'  & `obs1_scholar_index2' & `obs1_petition_index2'  \\ " _n

		file write regtab "\bottomrule" _n           // table footer
		file write regtab "\end{tabular}" _n 
		file close regtab
	}

	else if "`gender'" == "boy" {
		*** combined table
		cap file close regtab
		file open regtab using "$tables/el1_el2_primary_leebounds.tex", write append
		file write regtab " \vspace{1cm} \begin{flushleft} Panel B: Boys \end{flushleft}"
		file write regtab "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\extracolsep{1pt}}{l}*{8}{>{\centering\arraybackslash}m{2.6cm}}@{}}  \toprule" _n      
		file write regtab "& \multicolumn{2}{c}{Endline 1} & \multicolumn{3}{c}{Endline 2} \\  \cmidrule(lr){2-3} \cmidrule(lr){4-6}"	
		file write regtab " & Gender attitudes index & Self-reported behavior index & Gender attitudes index & Self-reported behavior index & Signed petition  \\     " _n     
		file write regtab "  & (1) & (2) & (3) & (4) & (5) \\    " _n    
		file write regtab "\midrule" _n 

		file write regtab "`varlab1' & `el1_treat_gender_index2' & `el1_treat_behavior_index2' & `treat_gender_index2' & `treat_behavior_index2' & `treat_petition_index2' \\  " _n
		file write regtab "  & [`el1_se_gender_index2'] & [`el1_se_behavior_index2'] & [`se_gender_index2'] & [`se_behavior_index2']  & [`se_petition_index2'] \\ \addlinespace[3pt] " _n
		file write regtab "`varlab2' & `el1_treat_l_gender_index2' & `el1_treat_l_behavior_index2' & `treat_l_gender_index2' & `treat_l_behavior_index2'  & `treat_l_petition_index2' \\  " _n
		file write regtab "  & [`el1_se_l_gender_index2'] & [`el1_se_l_behavior_index2']  & [`se_l_gender_index2'] & [`se_l_behavior_index2']  & [`se_l_petition_index2']  \\ \addlinespace[3pt] " _n
		file write regtab "`varlab3' & `el1_treat_u_gender_index2' & `el1_treat_u_behavior_index2' & `treat_u_gender_index2' & `treat_u_behavior_index2'  & `treat_u_petition_index2' \\  " _n
		file write regtab "  & [`el1_se_u_gender_index2'] & [`el1_se_u_behavior_index2'] & [`se_u_gender_index2'] & [`se_u_behavior_index2'] & [`se_u_petition_index2']  \\ \addlinespace[3pt] " _n

		file write regtab "Observations & `el1_obs_gender_index2' & `el1_obs_behavior_index2' & `obs_gender_index2' & `obs_behavior_index2'  & `obs_petition_index2'  \\ " _n
		file write regtab "Observations (Lee bounds) & `el1_obs1_gender_index2' & `el1_obs1_behavior_index2' & `obs1_gender_index2' & `obs1_behavior_index2'  & `obs1_petition_index2'  \\ " _n

		file write regtab "\bottomrule" _n           // table footer
		file write regtab "\end{tabular}" _n 
		file close regtab
	}
log close
}

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
* APPENDIX TABLE 1.11: Heterogeneity of effects by baseline parent attitudes (Endline 1, above median)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

use "$finaldata", clear

rename treat_girl treat_Sgirl
la var treat_Sgirl "Treated $\times$ Female"	

local file_Pgender_index2 "patt"

local lab_Pgender_index2 "BL parent attitudes"

cap log close
log using "$ad_hoc/fully_interacted_regression.smcl", append
qui log off 

local header 
local n_col = 0

use "$finaldata", clear

*Parent BL attitudes
la var treat_Pgender_index2_m "Treated $\times$ Above median baseline parent attitudes"	

local treatrowdi_g B_treat treat_Pgender_index2_m
local treatrowdi_b B_treat treat_Pgender_index2_m

foreach var in Pgender_index2 {
	eststo clear
	local i=0
	foreach index in gender_index2 aspiration_index2 behavior_index2  {

		local treatrow B_treat B_`var'_median treat_`var'_m // vars to use in regression

		local controls B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el_``index''_flag} district_gender_* gender_grade_*
		local pX
		foreach var1 of varlist `controls'{
			local varname pX`var1'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_`var' * `var1'
			local treatrow `treatrow' `varname'
		}

		local varlab: variable label E_S`index'
		local header "`header' & `varlab'" // column headers with variables labels  
		local ++n_col // number of columns\

		if "`index'"=="aspiration_index2" {

			eststo bs_`index' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==1 , cluster(Sschool_id)
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==1
			if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
			else estadd scalar ctrlmean=r(mean) 
			estadd local cluster = `e(N_clust)'		
			estadd local basic "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval=`r(p)'


		}

		else {

			eststo bs_`index' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==1
			if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
			else estadd scalar ctrlmean=r(mean) 
			estadd local cluster = `e(N_clust)'		
			estadd local basic "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval=`r(p)'
		}

		drop pX*
	}
}

***export results***

local frame1 = subinstr("`frame'", "2.3cm", "2.3cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col'", .)

#delimit ;

esttab bs_* using "$tables/el1_heterog_gen_`file_Pgender_index2'.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment
prehead("`prehead'" "`header' \\")
keep(`treatrowdi_g') order(`treatrowdi')
stats(Pgender_index2_pval ctrlmean basic N,   
labels("p-val: Treated + Treated $\times$ Above median attitudes = 0" "Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %7.3fc %20s %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr 

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
* APPENDIX TABLE 1.12: SEE TABLE 1.4
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
* APPENDIX TABLES 1.13, 1.14, 1.27, 1.28
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

use "$finaldata", clear

rename attrition el2_attrition
rename attrition_el el1_attrition

foreach var in E_Stalk_opp_gender_comm E_Ssit_opp_gender_comm E_Scook_clean_comm {
	local corr_var = subinstr("`var'", "_comm", "", .)
	local lab: var la `corr_var'
	la var `var' "`lab'"
}

la var E_Sabsent_sch_hhwork_comm "Student has not missed school due to household responsibilities"
la var E2_Sdisc_work_comm "Disagree: Do you discourage your sister from working outside home?"
la var E2_Sfuture_work "I can talk to my parents about what work I would like to do in the future"
la var E_Ssit_opp_gender_comm "Do you sit next to students of opposite sex in the classroom?"

foreach var in `el1_aspiration_girls_only' `el2_aspiration_girls_only' {
	local lab: var la `var'
	local lab = subinstr("`lab'", ">", "$>$", .)
	la var `var' "`lab'"
}

local el1_behavior_common E_Stalk_opp_gender_comm E_Ssit_opp_gender_comm E_Scook_clean_comm ///
E_Sabsent_sch_hhwork_comm E_Sdiscourage_college_comm E_Sdiscourage_work_comm

local el1_behavior_girls_only E_Shh_shopping_g E_Stake_care_young_sib_g  E_Sfuture_work_g E_Sdecision_past_tenth_g ///
E_Sdecision_work_g E_Sdecision_kindofwork_g E_Sdecision_chores_g E_Salone_friend_g E_Sabsent_sch_g

local el2_behavior_common E2_Stalk_opp_gen_comm E2_Ssit_opp_gen_comm E2_Sfriend_opp_gen_comm E2_Splay_opp_gen_comm ///
E2_Stalk_week_opp_gen_comm E2_Scook_clean_comm E2_Sabsent_sch_hhwork_comm E2_Sdisc_col_comm E2_Sdisc_work_comm 

local el2_behavior_girls_only E2_Sfuture_work E2_Sdec_past_twel_y E2_Sdecision_work_y E2_Sdecision_kindofwork_y E2_Sattendance ///
E2_Salone_friend E2_Salone_market E2_Salone_events E2_Salone_past_week E2_Salone_school_y

local el1_aspiration_girls_only E_Sboard_score_median E_Shighest_educ_median E_Sdiscuss_educ E_Soccupa_25_white E_Scont_educ

local el2_aspiration_girls_only E2_Stwel_score_exp_m E2_Sdiscuss_educ E2_Scont_educ E2_Shighest_educ_m ///
E2_Soccupa_25_y E2_Splan_college E2_Scol_course_want_y E2_Scol_course_want_stem E2_Scont_have_job_y E2_Stot_marry_age E2_Sage_first_child_numb

*** DEFINING LOCALS FOR TOO LONG LABELS
foreach var of varlist `el1_behavior_girls_only' `el1_behavior_common' `el2_behavior_girls_only' `el2_behavior_common' `el1_aspiration_girls_only' `el2_aspiration_girls_only' {
	local `var': var la `var'
}

***endline 1 behavior
local E_Stalk_opp_gender_comm "`E_Stalk_opp_gender_comm'tives?"
local E_Sdiscourage_college_comm "`E_Sdiscourage_college_comm'way?"
//local E_Sdecision_past_tenth_g = subinstr("`E_Sdecision_past_tenth_g'", "*", "", .)
local E_Sdecision_past_tenth_g "`E_Sdecision_past_tenth_g'ade"
//local E_Sdecision_kindofwork_g = subinstr("`E_Sdecision_kindofwork_g'", "*", "", .)
local E_Sdecision_kindofwork_g "`E_Sdecision_kindofwork_g'dies"
//local E_Sdecision_chores_g = subinstr("`E_Sdecision_chores_g'", "*", "", .)
local E_Sdecision_chores_g "`E_Sdecision_chores_g' etc...)"
***endline 1 aspiration
local E_Sboard_score_median "Expected 10th marks $>$ control-gender median"
local E_Shighest_educ_median "Highest level of education you would like to complete $>$ control-gender median"
local E_Sdiscuss_educ "Have you discussed your education goals with your parents or adult relatives?"
local E_Scont_educ "`E_Scont_educ'?"
***endline 2 behavior
local E2_Sabsent_sch_hhwork_comm "`E2_Sabsent_sch_hhwork_comm'onsibilities"
local E2_Sdisc_col_comm "`E2_Sdisc_col_comm'way?"
local E2_Salone_market "`E2_Salone_market'e?"
***endline 2 aspiration
local E2_Stwel_score_exp_m "Expected 12th marks $>$ control-gender median"
local E2_Sdiscuss_educ "`E2_Sdiscuss_educ'es?"
local E2_Scont_educ "`E2_Scont_educ'?"
local E2_Shighest_educ_m "Highest level of education you would like to complete $>$ control-gender median"
local E2_Splan_college "`E2_Splan_college'areer?"
local E2_Scont_have_job_y "`E2_Scont_have_job_y'en I am married and have children"
local E2_Salone_past_week "`E2_Salone_past_week'chase?"
local E2_Sage_first_child_numb "At what age would you like to have your first child?"

gen E2_Stot_marry_age_flag = mi(E2_Stot_marry_age)
gen E2_Sage_first_child_numb_flag = mi(E2_Sage_first_child_numb)

foreach var in `el1_aspiration_girls_only' `el2_aspiration_girls_only' {
	local lab: var la `var'
	local lab = subinstr("`lab'", ">", "$>$", .)
	la var `var' "`lab'"
}

foreach index in behavior aspiration {

	cap log close 
	log using "$ad_hoc/`index'_bonferonni_count", replace
	qui log off 

	foreach time in el1 el2 {

		cap file close sumstat
		file open sumstat using "$tables/`time'_`index'_indvars.tex", write replace

		if "`index'" == "behavior" {

			file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{L{11cm}*{3}{>{\centering\arraybackslash}m{2cm}}@{}} \toprule" _n      
			file write sumstat " & Boys & Girls \\" _n
			file write sumstat "\midrule" _n 

			local count: di wordcount("``time'_`index'_common''") // no. of variabls in gender attitudes index. 
			qui log on 
			di "`time' common count: `count'"
			qui log off

			foreach var in ``time'_`index'_common' {

				local l ``var''

				reg `var' B_treat district_gender_* gender_grade_* B_S`index'_index2 /*B_S`index'_index2_flag*/ /* B_S`index'_index2_m */ if !mi(E_Steam_id) & `time'_attrition==0 & `var'_flag==0 & B_Sgirl==0, cluster(Sschool_id)
				local b1: di %7.3fc _b[B_treat]
				local t1 = _b[B_treat]/_se[B_treat]
				local p1 = 2*ttail(e(df_r),abs(`t1'))

				if  `p1' < 0.1/`count' &  `p1' >= 0.05/`count'  local b1 `b1'\sym{*}
				else if  `p1' < 0.05/`count' &  `p1' >= 0.01/`count'  local b1 `b1'\sym{**}
				else if  `p1' < 0.01/`count' local b1 `b1'\sym{***}

				reg `var' B_treat district_gender_* gender_grade_* B_S`index'_index2 /*B_S`index'_index2_flag*/ /* B_S`index'_index2_m */ if !mi(E_Steam_id) & `time'_attrition==0 & `var'_flag==0 & B_Sgirl==1, cluster(Sschool_id)
				local b2: di %7.3fc _b[B_treat]
				local t2 = _b[B_treat]/_se[B_treat]
				local p2 = 2*ttail(e(df_r),abs(`t2'))

				if  `p2' < 0.1/`count' &  `p2' >= 0.05/`count'  local b2 `b2'\sym{*}
				else if  `p2' < 0.05/`count' &  `p2' >= 0.01/`count'  local b2 `b2'\sym{**}
				else if  `p2' < 0.01/`count' local b2 `b2'\sym{***}

				file write sumstat " `l' & `b1' & `b2' \\ \addlinespace[10pt]   " _n 

			}

		}

		else {
			file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{L{10cm}*{1}{>{\raggedright\arraybackslash}m{2cm}}@{}}  \toprule" _n      
			file write sumstat " & Girls \\" _n
			file write sumstat "\midrule" _n 
		}


		local count1: di wordcount("``time'_`index'_girls_only'")
		local count = `count1'
		qui log on 
		di "`time' girls only count: `count'"
		qui log off 

		foreach var in ``time'_`index'_girls_only' {

			local l ``var''

			reg `var' B_treat district_gender_* gender_grade_* B_S`index'_index2 /*B_S`index'_index2_flag*/ /* B_S`index'_index2_m */ if !mi(E_Steam_id) & `time'_attrition==0 & `var'_flag==0 & B_Sgirl==1, cluster(Sschool_id)
			local b2: di %7.3fc _b[B_treat]
			local t2 = _b[B_treat]/_se[B_treat]
			local p2 = 2*ttail(e(df_r),abs(`t2'))

			if  `p2' < 0.1/`count' &  `p2' >= 0.05/`count'  local b2 `b2'\sym{*}
			else if  `p2' < 0.05/`count' &  `p2' >= 0.01/`count'  local b2 `b2'\sym{**}
			else if  `p2' < 0.01/`count' local b2 `b2'\sym{***}
			file write sumstat " `l' & `b2' \\ \addlinespace[10pt]   " _n 
			

		}

		file write sumstat "\bottomrule" _n              
		file write sumstat "\end{tabular}" _n 
		file close sumstat	
	}

	log close 
}

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	APPENDIX TABLE 1.7: Correlates of primary outcomes in Endline 1 control group
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

lab var B_no_female_sib "No. of female siblings"
lab var B_no_male_sib "No. of male siblings"
lab var B_Sparent_stay "Parents stay with the student"
lab var B_Sgrade6 "Grade 6"
lab var B_Pgender_index2 "Baseline parent gender attitudes index"

**control variables	
local fe_B_Sgirl B_Sgrade6 district_1 district_2 district_3 district_4
local fe_B_Pgender_index2 district_gender_* gender_grade_*

* labels
local varlab1: var lab B_Sgirl
local varlab2: var lab B_Pgender_index2


eststo clear	
local header	
local n_col = 0

local index gender_index2

foreach var in B_Sgirl B_Pgender_index2 {
	
	local ++n_col // number of columns					

	eststo bs_`var' : reg E_S`index' `var' ${el_``index''_flag} `fe_`var'' if !mi(E_Steam_id) & attrition_el==0 & B_treat==0, cluster(Sschool_id) 

	if "`var'"=="B_Sgirl" {
		local stat: di %7.4fc _b[B_Sgirl]
		local coef =  _b[B_Sgirl]
		cap file write paperstat "4.2Short-run results pg15, EL1 gender attitudes SD - diff between girls and boys, `stat'" _n
		
		cap local stat =(`el1_att_coef' / `coef')*100
		cap local stat: di %7.4fc `stat'
		cap file write paperstat "4.2Short-run results pg15, EL1 gender attitudes - treatment effect as % of status quo gender gap in attitudes, `stat'" _n

	}

	if "`var'"=="B_Pgender_index2" {
		local stat: di %7.4fc _b[B_Pgender_index2]
		cap file write paperstat "4.2Short-run results pg15, EL1 gender attitudes on BL parent gender attitudes (SD), `stat'" _n
	}

	su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
	estadd scalar ctrlmean=abs(r(mean))
	estadd local basic "Yes"			
}

local frame1 = subinstr("`frame'", "2.3cm", "3cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col'", .)

#delimit ;

esttab bs_* using "$tables/el1_benchmarking_size.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "& \multicolumn{2}{c}{Gender attitudes index} \\  \cmidrule(lr){2-3} ")
keep(B_Sgirl B_Pgender_index2) order(B_Sgirl B_Pgender_index2)
stats(ctrlmean basic N,   
labels("Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %20s %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
*	APPENDIX TABLE 1.16: Heterogeneity by gender, controlling for heterogeneity by BL attitudes
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 	

*** new approach ***

foreach gender in both_1 both  { //girl boy {

	local i = 1 //p-val counter

	if "`gender'" == "girl" {
		use "$finaldata", clear
		keep if B_Sgirl == 1
		local ar replace
		local preprehead "\begin{flushleft} Panel A: Girls \end{flushleft} \vspace{3mm}"
		local tablerow B_treat B_Sgirl /*treat_Sgirl*/ B_Sgender_index2 treat_Satt
		local tablerowdi B_treat /*treat_Sgirl*/ treat_Satt	
	}

	else if "`gender'" == "boy" {
		use "$finaldata", clear
		keep if B_Sgirl == 0
		local ar append 
		local preprehead "\begin{flushleft} Panel B: Boys \end{flushleft}"
		local tablerow B_treat B_Sgirl /*treat_Sgirl*/ treat_Satt
		local tablerowdi B_treat /*treat_Sgirl*/ treat_Satt	
	}

	else if "`gender'" == "both" {
		use "$finaldata", clear 
		local tablerow B_treat B_Sgirl treat_Sgirl treat_Satt
		local tablerowdi B_treat treat_Sgirl treat_Satt	
		local preprehead "\begin{flushleft} Panel B: With interaction of treatment and baseline outcome \end{flushleft}"
	}

	else if "`gender'" == "both_1" {
		use "$finaldata", clear 
		local tablerow B_treat B_Sgirl treat_Sgirl
		local tablerowdi B_treat treat_Sgirl
		local preprehead "\begin{flushleft} Panel A: Without interaction of treatment and baseline outcome \end{flushleft} \vspace{3mm}"
	}

	gen treat_Sgirl = B_treat*B_Sgirl
	la var treat_Sgirl "Treated $\times$ Female" //leaving space so columns align in 2 panel table 	

	eststo clear	
	local header	
	local n_col = 0

	* endline 1
	foreach index in gender_index2 behavior_index2  {

		cap drop treat_Satt
		gen treat_Satt = B_treat*B_S`index'
		la var treat_Satt "Treated $\times$ Baseline outcome"

		local varlab: variable label E_S`index'
		local header "`header' & `varlab'" // column headers with variables labels 

		local ++n_col // number of columns	

		local controls B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el_``index''_flag}

		foreach var of varlist `controls' {
			local varname gX`var'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_Sgirl * `var'
		}

		eststo E_`index' : reg E_S`index' `tablerow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} gX* if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) 
		drop gX*
		su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0

		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean)


		estadd local cluster = `e(N_clust)'		
		estadd local basic "Yes"			
	}

	* endline 2	
	
	foreach index in gender_index2 behavior_index2 /*petition_index2*/ {

		cap drop treat_Satt
		gen treat_Satt = B_treat*B_S`index'
		la var treat_Satt "Treated $\times$ Baseline outcome"

		local varlab: variable label E2_S`index'
		local header "`header' & `varlab'" // column headers with variables labels 

		local ++n_col // number of columns		

		if "`index'"=="petition_index2" {

			local controls ${el_``index''_flag}
			foreach var of varlist `controls' {
				local varname gX`var'
				local varname = substr("`varname'", 1, 30)
				gen `varname' = B_Sgirl * `var'
			}
			eststo E2_`index' : reg E2_S`index' `tablerow' district_gender_* gender_grade_* gX*  if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) 
			drop gX*
		}

		else {

			local controls B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el_``index''_flag}
			foreach var of varlist `controls' {
				local varname gX`var'
				local varname = substr("`varname'", 1, 30)
				gen `varname' = B_Sgirl * `var'
			}
			eststo E2_`index' : reg E2_S`index' `tablerow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} gX* if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) 
			drop gX*
		}


		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
		estadd scalar ctrlmean=r(mean) 

		estadd local cluster = `e(N_clust)'		
		estadd local basic "Yes"			
	}

	if "`gender'" == "both_1" {

		#delimit ;
		local prehead "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
		\begin{tabular}{p{5cm}*{`n_col'}{>{\centering\arraybackslash}m{2.3cm}}}
		\toprule";
		#delimit cr	

		#delimit ;

		esttab E_* E2_* using "$tables/el1_el2_heterog_gender_att_bl.tex", 
		b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
		replace gaps label booktabs style(tex) fragment 
		prehead(" `preprehead' `prehead'" 	"& \multicolumn{2}{c}{Endline 1} & \multicolumn{2}{c}{Endline 2} \\  \cmidrule(lr){2-3} \cmidrule(lr){4-5}" "`header' \\")
		keep(`tablerowdi') order(`tablerowdi')
		stats(ctrlmean basic N,   
		labels("Control group mean" "Basic controls" "Number of students") 
		fmt(%7.3fc %20s %7.0fc))
		postfoot("\bottomrule" "\end{tabular}" "}" "\vspace{1cm}"); 

		#delimit cr	
	}

	else if "`gender'" == "both" {
		
		#delimit ;
		local prehead "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
		\begin{tabular}{p{5cm}*{`n_col'}{>{\centering\arraybackslash}m{2.3cm}}}
		\toprule";
		#delimit cr	

		#delimit ;

		esttab E_* E2_* using "$tables/el1_el2_heterog_gender_att_bl.tex", 
		b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
		append gaps label booktabs style(tex) fragment 
		prehead(" `preprehead' `prehead'" 	"& \multicolumn{2}{c}{Endline 1} & \multicolumn{2}{c}{Endline 2} \\  \cmidrule(lr){2-3} \cmidrule(lr){4-5}" "`header' \\")
		keep(`tablerowdi') order(`tablerowdi')
		stats(ctrlmean basic N,   
		labels("Control group mean" "Basic controls" "Number of students") 
		fmt(%7.3fc %20s %7.0fc))
		postfoot("\bottomrule" "\end{tabular}" "}"); 

		#delimit cr	

		*** slide version *** 

		#delimit ;

		esttab E_* E2_* using "$slides/el1_el2_heterog_gender_att_bl.tex", 
		b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
		replace gaps label booktabs style(tex) fragment 
		prehead(" `prehead'" 	"& \multicolumn{2}{c}{Endline 1} & \multicolumn{2}{c}{Endline 2} \\  \cmidrule(lr){2-3} \cmidrule(lr){4-5}" "`header' \\")
		keep(`tablerowdi') order(`tablerowdi')
		stats(N, 
		labels("Number of students") 
		fmt(%7.0fc))
		postfoot("\bottomrule" "\end{tabular}" "}"); 
		
		#delimit cr
	}
}

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	APPENDIX TABLE 1.17: Heterogeneity by gender, controlling for heterogeneity by wealth proxies
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

use "$finaldata", clear

cap log close
log using "$ad_hoc/wealth_proxy.smcl", replace
qui log off 


foreach gender in girl boy both {

	if "`gender'" == "girl" {
		use "$finaldata", clear
		local wealth B_f_fulltime B_Shouse_pukka_y B_Sflush_toilet B_Snewspaper_house B_Pown_land
		foreach var in `wealth' {
			gen `var'_treat = `var'*B_treat
			local varlab: variable label `var'
			lab var `var'_treat "Treated $\times$ `varlab'"
		}

		local wealth_treated B_f_fulltime_treat B_Shouse_pukka_y_treat B_Sflush_toilet_treat B_Snewspaper_house_treat B_Pown_land_treat
		local wealth_flag B_f_fulltime_flag B_Shouse_pukka_y_flag B_Sflush_toilet_flag B_Snewspaper_house_flag B_Pown_land_flag
		local treatrow B_treat B_Sgirl `wealth_treated' `wealth' `wealth_flag' 
		local treatrowdi B_treat `wealth_treated' `wealth' 
		keep if B_Sgirl == 1
		local ar replace
		local preprehead "\begin{flushleft} Panel A: Girls \end{flushleft} \vspace{3mm}"
		local pf "\clearpage"
	}

	else if "`gender'" == "boy" {
		use "$finaldata", clear
		local wealth B_f_fulltime B_Shouse_pukka_y B_Sflush_toilet B_Snewspaper_house B_Pown_land
		foreach var in `wealth' {
			gen `var'_treat = `var'*B_treat
			local varlab: variable label `var'
			lab var `var'_treat "Treated $\times$ `varlab'"
		}

		local wealth_treated B_f_fulltime_treat B_Shouse_pukka_y_treat B_Sflush_toilet_treat B_Snewspaper_house_treat B_Pown_land_treat
		local wealth_flag B_f_fulltime_flag B_Shouse_pukka_y_flag B_Sflush_toilet_flag B_Snewspaper_house_flag B_Pown_land_flag

		local treatrow B_treat B_Sgirl `wealth_treated' `wealth' `wealth_flag' 
		local treatrowdi B_treat `wealth_treated' `wealth' 
		keep if B_Sgirl == 0
		local ar append 
		local preprehead "\begin{flushleft} Panel B: Boys \end{flushleft}"
		local pf "\clearpage"
	}

	else if "`gender'" == "both" {
		use "$finaldata", clear
		local wealth B_f_fulltime B_Shouse_pukka_y B_Sflush_toilet B_Snewspaper_house B_Pown_land
		foreach var in `wealth' {
			gen `var'_treat = `var'*B_treat
			local varlab: variable label `var'
			lab var `var'_treat "Treated $\times$ `varlab'"
		}

		gen B_Sgirl_treat = B_Sgirl * B_treat
		la var B_Sgirl_treat "Treated $\times$ Female"

		local wealth_treated B_Sgirl_treat B_f_fulltime_treat B_Shouse_pukka_y_treat B_Sflush_toilet_treat B_Snewspaper_house_treat B_Pown_land_treat
		local wealth_flag B_f_fulltime_flag B_Shouse_pukka_y_flag B_Sflush_toilet_flag B_Snewspaper_house_flag B_Pown_land_flag

		local treatrow B_treat B_Sgirl `wealth_treated' `wealth' `wealth_flag' 
		local treatrowdi B_treat `wealth_treated' `wealth' 
	}

	local header
	eststo clear

	foreach e in E_S E2_S {

		if "`e'"=="E_S" {

			foreach index in gender_index2 behavior_index2  {         

				local varlab: variable label E2_S`index'
				local header "`header' & `varlab'" // column headers with variables labels 

				local ++n_col // number of columns

				eststo `e'`index': reg `e'`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)

				su `e'`index'  if B_treat==0 
				if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
				else estadd scalar ctrlmean=r(mean)  

				su `e'`index'  if B_treat==1 
				estadd scalar treatmean=r(mean)     

				estadd local basic "Yes"
				estadd scalar pval = `p'

				if "`gender'" == "both"{
                    ***p-value calculation
                    local controls B_f_fulltime B_Shouse_pukka_y B_Sflush_toilet B_Snewspaper_house B_Pown_land B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el_``index''_flag}
                    foreach var1 of varlist `controls'{ //generating female*baseline and female*wealth
                    local varname gX`var1'
                    local varname = substr("`varname'", 1, 30)
                    gen `varname' = B_Sgirl * `var1'
                }

                qui log on 
                di "ORIGINAL `e'`index'"
                reg `e'`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
                di "FULLY INTERACTED `e'`index'"
                eststo `e'`index': reg `e'`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} gX* if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
                qui log off
                su `e'`index'  if B_treat==0 
                if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
                else estadd scalar ctrlmean=r(mean)    

                su `e'`index'  if B_treat==1 
                estadd scalar treatmean=r(mean) 

                estadd local basic "Yes"
                estadd scalar pval = `p'
	     		drop gX*

                } 

            }
            
        }
        
        else if "`e'"=="E2_S" {

        	foreach index in gender_index2 behavior_index2  {           

        		local varlab: variable label E2_S`index'
        		local header "`header' & `varlab'" // column headers with variables labels 

        		local ++n_col // number of columns

        		eststo `e'`index': reg `e'`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)

        		su `e'`index'  if B_treat==0 
        		estadd scalar ctrlmean=r(mean)  

        		su `e'`index'  if B_treat==1 
        		estadd scalar treatmean=r(mean) 

        		estadd local basic "Yes"
        		estadd scalar pval = `p'

        		if "`gender'" == "both"{

                    ***p-value calculation
                    local controls B_f_fulltime B_Shouse_pukka_y B_Sflush_toilet B_Snewspaper_house B_Pown_land B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el2_``index''_flag}
                    foreach var1 of varlist `controls'{ //generating female*wealth
                    local varname gX`var1'
                    local varname = substr("`varname'", 1, 30)
                    gen `varname' = B_Sgirl * `var1'
                }

                qui log on 
                di "ORIGINAL `e'`index'"
                reg `e'`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id)
                di "FULLY INTERACTED"
                eststo `e'`index': reg `e'`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} gX* if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
                qui log off 
                su `e'`index'  if B_treat==0 
                if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
                else estadd scalar ctrlmean=r(mean)  

                su `e'`index'  if B_treat==1 
                estadd scalar treatmean=r(mean) 

                estadd local basic "Yes"
                estadd scalar pval = `p'
                drop gX*

                }
            }
            
        }
    }

    if "`gender'" != "both" {

    	local prehead = subinstr("`frame'", "colno", "4", .)

    	#delimit ;

    	esttab E_* E2_* using "$tables/el1_el2_hetgen_wealth.tex", 
    	b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
    	`ar' longtable gaps label booktabs style(tex) fragment 
    	prehead("`preprehead' `prehead'"  "& \multicolumn{2}{c}{Endline 1} & \multicolumn{2}{c}{Endline 2} \\  \cmidrule(lr){2-3} \cmidrule(lr){4-5}" "`header' \\")
    	keep(`treatrowdi') order(`treatrowdi')
    	stats(ctrlmean treatmean basic N,   
    	labels("Control group mean" "Treatment group mean" "Basic controls" "Number of students") 
    	fmt(%7.3fc  %7.3fc  %7.3fc %20s %7.0fc))
    	postfoot("\bottomrule" "\end{tabular}" "}" "`pf'"); 

    	#delimit cr

    }

    else {

    	local prehead = subinstr("`frame'", "colno", "4", .)

    	#delimit ;

    	esttab E_* E2_* using "$tables/el1_el2_hetgen_wealth_combined.tex", 
    	b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
    	replace gaps label booktabs style(tex) fragment 
    	prehead("`prehead'"  "& \multicolumn{2}{c}{Endline 1} & \multicolumn{2}{c}{Endline 2} \\  \cmidrule(lr){2-3} \cmidrule(lr){4-5}" "`header' \\")
    	keep(`treatrowdi') order(`treatrowdi')
    	stats(ctrlmean treatmean basic N,   
    	labels("Control group mean" "Treatment group mean" "Basic controls" "Number of students") 
    	fmt(%7.3fc  %7.3fc  %7.3fc %20s %7.0fc))
    	postfoot("\bottomrule" "\end{tabular}" "}"); 

    	#delimit cr

    	local prehead = subinstr("`frame'", "colno", "4", .)

    	file open sumstat using "$tables/el1_el2_hetgen_wealth.tex", write append 
    	file write sumstat "\begin{flushleft} Panel C: P-vals, Boys vs. Girls \end{flushleft} `prehead' & \multicolumn{2}{c}{Endline 1} & \multicolumn{2}{c}{Endline 2} \\  \cmidrule(lr){2-3} \cmidrule(lr){4-5} `header' \\"
    	local follow_head
    	foreach i of numlist 1/4 {
    		local follow_head "`follow_head' & \multicolumn{1}{c}{(`i')}"
    	}
    	file write sumstat "`follow_head' \\ \midrule "
    	file write sumstat "Treated & `E_Sgender_index2p1' & `E_Sbehavior_index2p1' & `E2_Sgender_index2p1' & `E2_Sbehavior_index2p1' \\ "
    	file write sumstat "Treated $\times$ Father works full-time & `E_Sgender_index2p2' & `E_Sbehavior_index2p2' & `E2_Sgender_index2p2' & `E2_Sbehavior_index2p2' \\ "
    	file write sumstat "Treated $\times$ House is pukka & `E_Sgender_index2p3' & `E_Sbehavior_index2p3' & `E2_Sgender_index2p3' & `E2_Sbehavior_index2p3' \\ "
    	file write sumstat "Treated $\times$ Household gets newspaper daily & `E_Sgender_index2p4' & `E_Sbehavior_index2p4' & `E2_Sgender_index2p4' & `E2_Sbehavior_index2p4' \\ "
    	file write sumstat "Treated $\times$ Household owns some land & `E_Sgender_index2p5' & `E_Sbehavior_index2p5' & `E2_Sgender_index2p5' & `E2_Sbehavior_index2p5' \\ "
    	file write sumstat "\bottomrule \end{tabular} }"
    	file close sumstat

    	eststo clear
    }

}

log close

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
* APPENDIX TABLE 1.8/1.26: Endline 1/Endline 2 Treatment effects on gender attitudes sub-indices (basic controls)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

foreach index in gender_index2_fert {
	if "`index'"=="gender_index2_fert"  {
		eststo bs_`index' : reg E_S`index' B_treat district_gender_* gender_grade_* if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id) // no baseline index

		local stat: di %7.4fc _b[B_treat]
		cap file write paperstat "4.2Short-run results pg17, EL1 fertility attitudes SD, `stat'" _n	

	}
}

foreach time in el1 el2 {

	local vars gender_index2_educ gender_index2_emp gender_index2_sub gender_index2_fert
	eststo clear	
	local header	
	local nos
	local n_col = 0

	local b "Treated"
	local se
	local n "Number of students"
	local m "Control group mean"

	if "`time'" == "el1" {
		local num
		local attr _el
	}

	else if "`time'" == "el2" {
		local num 2
		local attr 
	}

	*Girls 
	use "$finaldata", clear
	keep if B_Sgirl == 1
	la var E_Sgender_index2_sub "Other equal rights for women"

	foreach index in `vars'  {

		local varlab: variable label E_S`index'
		local header "`header' & `varlab'" // column headers with variables labels 

		local ++n_col // number of columns
		local i = `n_col'

		local nos "`nos' & (`n_col')"

		if "`index'"=="gender_index2_fert"  {
			eststo bs_`index' : reg E`num'_S`index' B_treat district_? B_Sgrade6 if !mi(E_Steam_id) & attrition`attr'==0 , cluster(Sschool_id) // no baseline index

		}

		else {
			eststo bs_`index' : reg E`num'_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el`num'_``index''_flag} if !mi(E`num'_Steam_id) & attrition`attr'==0, cluster(Sschool_id)

		}

		* storing coefs		
		local b`i': di %7.3fc _b[B_treat]
		local se`i'= trim(string(_se[B_treat],"%7.3f"))
		local t`i' = _b[B_treat]/_se[B_treat]
		local p`i' = 2*ttail(e(df_r),abs(`t`i''))
		local n`i': di %7.0f e(N)
		
		if `se`i''>0 local se`i' = "[`se`i'']"
		else  local se`i' = "`se`i''"
		
		if  `p`i'' < 0.1 &  `p`i'' >= 0.05  local b`i' `b`i''\sym{*}
		else if  `p`i'' < 0.05 &  `p`i'' >= 0.01  local b`i' `b`i''\sym{**}
		else if  `p`i'' < 0.01  local b`i' `b`i''\sym{***}

		* Control group mean
		su E`num'_S`index' if B_treat==0 & !mi(E`num'_Steam_id) & attrition`attr'==0
		if "`index'"=="gender_index2_fert"  local m`i': di %7.3f r(mean) // not normalized
		else if abs(r(mean)) < .00005 local m`i': di %7.3f abs(r(mean))
		else local m`i': di %7.3f r(mean)
		
		local b "`b' & `b`i''"
		local se "`se' & `se`i''"
		local m "`m' & `m`i''"
		local n "`n' & `n`i''"


	}
	
	local allcol = `n_col'+1	
	
	cap file close sumstat
	file open sumstat using "$tables/`time'_att_subind.tex", write replace

	file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{`n_col'}{>{\centering\arraybackslash}m{2.8cm}}@{}}   \toprule \\" _n      // table header
	file write sumstat " `header'   \\     " _n      
	file write sumstat " `nos' \\    " _n    
	file write sumstat "\midrule \addlinespace[10pt] " _n 
	file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel A: Girls}}   \\  \addlinespace[3pt]   " _n         

	file write sumstat "`b'  \\    " _n 
	file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
	file write sumstat "`m'  \\    " _n 
	file write sumstat "`n' \\    \addlinespace[1cm] " _n 

	file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel B: Boys}}  \\  \addlinespace[3pt]   " _n         

	* Boys
	local b "Treated"
	local se
	local n "Number of students"
	local m "Control group mean"

	use "$finaldata", clear
	keep if B_Sgirl == 0

	local i=0
	foreach index in `vars'   {

		local ++i
		local nos "`nos' & (`n_col')"

		if "`index'"=="gender_index2_fert"  {
			eststo bs_`index' : reg E`num'_S`index' B_treat district_? B_Sgrade6 if !mi(E`num'_Steam_id) & attrition`attr'==0 , cluster(Sschool_id) // no baseline index
		}

		else {
			eststo bs_`index' : reg E`num'_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el`num'_``index''_flag} if !mi(E`num'_Steam_id) & attrition`attr'==0, cluster(Sschool_id)
		}
		

		* storing coefs		
		local b`i': di %7.3fc _b[B_treat]
		local se`i'= trim(string(_se[B_treat],"%7.3f"))
		local t`i' = _b[B_treat]/_se[B_treat]
		local p`i' = 2*ttail(e(df_r),abs(`t`i''))
		local n`i': di %7.0f e(N)

		if `se`i''>0 local se`i' = "[`se`i'']"
		else  local se`i' = "`se`i''"
		
		if  `p`i'' < 0.1 &  `p`i'' >= 0.05  local b`i' `b`i''\sym{*}
		else if  `p`i'' < 0.05 &  `p`i'' >= 0.01  local b`i' `b`i''\sym{**}
		else if  `p`i'' < 0.01  local b`i' `b`i''\sym{***}
		
		* Control group mean
		su E`num'_S`index' if B_treat==0 & !mi(E`num'_Steam_id) & attrition`attr'==0
		if "`index'"=="gender_index2_fert"  local m`i': di %7.3f r(mean) // not normalized
		else local m`i': di %7.3f r(mean)

		
		local b "`b' & `b`i''"
		local se "`se' & `se`i''"
		local m "`m' & `m`i''"
		local n "`n' & `n`i''"


	}	

	file write sumstat "`b'  \\    " _n 
	file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
	file write sumstat "`m'  \\    " _n 
	file write sumstat "`n' \\    " _n 
	
	file write sumstat "\bottomrule" _n              
	file write sumstat "\end{tabular}" _n 
	file close sumstat	
	
	* refreshing locals
	forval i = 1/`n_col' {
		foreach type in b se m n p {
			local `type'`i'		
		}		
	}
}


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
* APPENDIX TABLE 1.12/1.26: Endline 1 and 2 Effects on Behavior sub-indices 
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

foreach time in el1 el2 {

	if "`time'" == "el1" {
		local num
		local attr _el
	}

	else if "`time'" == "el2" {
		local num 2
		local attr 
	}

	local vars behavior_index2_oppsex behavior_index2_hhchores behavior_index2_relatives behavior_index2_dec_g behavior_index2_mobil_g

	eststo clear	
	local header	
	local nos
	local n_col = 0

	local b "Treated"
	local se
	local n "Number of students"
	local m "Control group mean"

	*** girls only
	use "$finaldata", clear
	keep if B_Sgirl == 1

	foreach index in `vars'  {

		local varlab: variable label E_S`index'
		local header "`header' & `varlab'" // column headers with variables labels 

		local ++n_col // number of columns
		local i = `n_col'

		local nos "`nos' & (`n_col')"		
		
		* no baseline variables
		if "`index'"=="behavior_index2_relatives"  {
			reg E`num'_S`index' B_treat district_? B_Sgrade6 ${el`num'_``index''_flag} if !mi(E`num'_Steam_id) & attrition`attr'==0 , cluster(Sschool_id) // no baseline index
		}
		
		else {
			reg E`num'_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el_``index''_flag} if !mi(E`num'_Steam_id) & attrition`attr'==0, cluster(Sschool_id)
		}

		* storing coefs		
		local b`i': di %7.3fc _b[B_treat]
		local se`i'= trim(string(_se[B_treat],"%7.3f"))
		local t`i' = _b[B_treat]/_se[B_treat]
		local p`i' = 2*ttail(e(df_r),abs(`t`i''))
		local n`i': di %7.0f e(N)
		
		if `se`i''>0 local se`i' = "[`se`i'']"
		else  local se`i' = "`se`i''"
		
		if  `p`i'' < 0.1 &  `p`i'' >= 0.05  local b`i' `b`i''\sym{*}
		else if  `p`i'' < 0.05 &  `p`i'' >= 0.01  local b`i' `b`i''\sym{**}
		else if  `p`i'' < 0.01  local b`i' `b`i''\sym{***}

		* Control group mean
		su E`num'_S`index' if B_treat==0 & !mi(E`num'_Steam_id) & attrition`attr'==0
		if "`index'"=="behavior_index2_mobil_g"  local m`i': di %7.3f r(mean) // not normalized
		else if abs(r(mean)) < .00005 local m`i': di %7.3f abs(r(mean))
		else local m`i': di %7.3f r(mean)
		
		local b "`b' & `b`i''"
		local se "`se' & `se`i''"
		local m "`m' & `m`i''"
		local n "`n' & `n`i''"


	}

	local allcol = `n_col'+1					
	
	cap file close sumstat
	file open sumstat using "$tables/`time'_beh_subind.tex", write replace

	file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{`n_col'}{>{\centering\arraybackslash}m{3cm}}@{}}   \toprule \\" _n      // table header
	file write sumstat " `header'   \\     " _n      
	file write sumstat " `nos' \\    " _n    
	file write sumstat "\midrule \addlinespace[10pt] " _n 
	file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel A: Girls}}   \\  \addlinespace[3pt]   " _n         

	file write sumstat "`b'  \\    " _n 
	file write sumstat " `se' \\ \addlinespace[3pt]   " _n 
	file write sumstat "`m'  \\    " _n 
	file write sumstat "`n' \\ \addlinespace[1cm]     " _n 

	file write sumstat " \multicolumn{`allcol'}{l}{\textit{Panel B: Boys}}   \\  \addlinespace[3pt]   " _n         

	* endline 2
	local b "Treated"
	local se
	local n "Number of students"
	local m "Control group mean"

	local i=0

	*** boys only
	use "$finaldata", clear
	keep if B_Sgirl == 0

	local vars behavior_index2_oppsex behavior_index2_hhchores behavior_index2_relatives //behavior_index2_dec_g behavior_index2_mobil_g

	foreach index in `vars'   {

		local ++i
		local nos "`nos' & (`n_col')"

		* no baseline variables
		if "`index'"=="behavior_index2_relatives"  {
			reg E`num'_S`index' B_treat district_? B_Sgrade6 ${el`num'_``index''_flag}  if !mi(E`num'_Steam_id) & attrition`attr'==0, cluster(Sschool_id)
		}
		
		else {
			reg E`num'_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_? B_Sgrade6 ${el`num'_``index''_flag} if !mi(E`num'_Steam_id) & attrition`attr'==0, cluster(Sschool_id)
		}

		* storing coefs		
		local b`i': di %7.3fc _b[B_treat]
		local se`i'= trim(string(_se[B_treat],"%7.3f"))
		local t`i' = _b[B_treat]/_se[B_treat]
		local p`i' = 2*ttail(e(df_r),abs(`t`i''))
		local n`i': di %7.0f e(N)

		if `se`i''>0 local se`i' = "[`se`i'']"
		else  local se`i' = "`se`i''"
		
		if  `p`i'' < 0.1 &  `p`i'' >= 0.05  local b`i' `b`i''\sym{*}
		else if  `p`i'' < 0.05 &  `p`i'' >= 0.01  local b`i' `b`i''\sym{**}
		else if  `p`i'' < 0.01  local b`i' `b`i''\sym{***}
		
		* Control group mean
		su E`num'_S`index' if B_treat==0 & !mi(E`num'_Steam_id) & attrition`attr'==0
		if abs(r(mean)) < .00005 local m`i': di %7.3f abs(r(mean))
		else local m`i': di %7.3f r(mean)
		
		local b "`b' & `b`i''"
		local se "`se' & `se`i''"
		local m "`m' & `m`i''"
		local n "`n' & `n`i''"

	}	

	file write sumstat "`b' & n/a & n/a  \\    " _n 
	file write sumstat " `se' & n/a & n/a \\ \addlinespace[3pt]   " _n 
	file write sumstat "`m' & n/a & n/a  \\    " _n 
	file write sumstat "`n' & n/a & n/a \\    " _n 
	
	file write sumstat "\bottomrule" _n              
	file write sumstat "\end{tabular}" _n 
	file close sumstat	
	
	* refreshing locals
	forval i = 1/`n_col' {
		foreach type in b se m n p {
			local `type'`i'		
		}		
	}
}

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
*	APPENDIX TABLE 1.22: Association between stated and revealed preferences, by social desirability score (control group only)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

**************************************
* Plan to attend college (high sds)
**************************************
use "$finaldata", clear

eststo clear	
local header	
local n_col = 0

local all " "
local treat "& B_treat==1"
local ctrl "& B_treat==0"

*** high social desirability scale
summ B_Ssocial_scale, detail
gen highsd_std=1 if B_Ssocial_scale>`r(p50)'
replace highsd_std=0 if B_Ssocial_scale<=`r(p50)' //EDIT: Originally ">=" on this line was ">", so 3,075 obs that == median were missing. 
la var highsd_std "High social desirability score"
gen treat_highsd_std = B_treat*highsd_std
la var treat_highsd_std "Treated $\times$ High social desirability score"

la var E2_Splan_college "Plans to go to college"
gen plan_sds = E2_Splan_college*highsd_std
la var plan_sds "Plans college $\times$ High SDS"
gen asp_sds = E2_Saspiration_index2*highsd_std
la var E2_Saspiration_index2 "EL2 aspirations index"
la var asp_sds "EL2 aspirations index $\times$ High SDS"


foreach type in all treat ctrl {

	eststo plan_`type': reg E2_Sscholar_index2 E2_Splan_college district_gender_* gender_grade_* if E2_Splan_college_flag!=1 ``type'', cluster(Sschool_id)
	eststo plansds_`type': reg E2_Sscholar_index2 E2_Splan_college highsd_std plan_sds district_gender_* gender_grade_* if E2_Splan_college_flag!=1 ``type'', cluster(Sschool_id)
	
	eststo asp_`type': reg E2_Sscholar_index2 E2_Saspiration_index2 district_gender_* gender_grade_* ${el2_aspiration_flag} if !mi(E2_Sscholar_index2) ``type'', cluster(Sschool_id)
	eststo aspsds_`type': reg E2_Sscholar_index2 E2_Saspiration_index2 highsd_std asp_sds district_gender_* gender_grade_* ${el2_aspiration_flag} if !mi(E2_Sscholar_index2) ``type'', cluster(Sschool_id)

	su E2_Sscholar_index2 if !mi(E2_Steam_id) & E2_Splan_college_flag!=1 ``type''
	estadd scalar ctrlmean=r(mean) : plan_`type'*
	
	su E2_Sscholar_index2 if !mi(E2_Steam_id) & E2_Splan_college_flag!=1 & !mi(B_Ssocial_scale)``type''
	estadd scalar ctrlmean=r(mean) : plansds_`type'*
	
	su E2_Sscholar_index2 if !mi(E2_Steam_id) & !mi(E2_Saspiration_index2) ``type''
	estadd scalar ctrlmean=r(mean) : asp_`type'*
	
	su E2_Sscholar_index2 if !mi(E2_Steam_id) & !mi(E2_Saspiration_index2) & !mi(B_Ssocial_scale) ``type''
	estadd scalar ctrlmean=r(mean) : aspsds_`type'*
	
	estadd local basic "Yes": plan_`type'* plansds_`type'* asp_`type'* aspsds_`type'*
	
	**** correlation
	corr E2_Sscholar_index2 E2_Splan_college if !mi(E2_Steam_id) & E2_Splan_college_flag!=1 ``type''
	
	* to get r
	local r1: di %7.3f r(rho)	
	
	* to get p value
	if (r(rho) != . & r(rho) < 1) {
		local p1=min(2*ttail(r(N)-2,abs(r(rho))*sqrt(r(N)-2)/sqrt(1-r(rho)^2)),1)
	}
	else if (r(rho)>=1 & r(rho) != .) {
		local p1=0
	}
	else if r(rho) == . {
		local p1= .
	}
	
	if  `p1' < 0.1 &  `p1' >= 0.05  local r1 `r1'\sym{*}
	else if  `p1' < 0.05 &  `p1' >= 0.01  local r1 `r1'\sym{**}
	else if  `p1' < 0.01  local r1 `r1'\sym{***}
	
	estadd local corr "`r1'": plan_`type'*
	
	corr E2_Sscholar_index2 E2_Saspiration_index2 if !mi(E2_Steam_id) & !mi(E2_Saspiration_index2) ``type''
	
	* to get r
	local r1: di %7.3f r(rho)	
	
	* to get p value
	if (r(rho) != . & r(rho) < 1) {
		local p1=min(2*ttail(r(N)-2,abs(r(rho))*sqrt(r(N)-2)/sqrt(1-r(rho)^2)),1)
	}
	else if (r(rho)>=1 & r(rho) != .) {
		local p1=0
	}
	else if r(rho) == . {
		local p1= .
	}
	
	if  `p1' < 0.1 &  `p1' >= 0.05  local r1 `r1'\sym{*}
	else if  `p1' < 0.05 &  `p1' >= 0.01  local r1 `r1'\sym{**}
	else if  `p1' < 0.01  local r1 `r1'\sym{***}
	
	estadd local corr "`r1'": asp_`type'*

}

*********************************
* Dowry and petition (high SDS)
*********************************

use "$finaldata", clear
foreach x in E2_Sdowry_girl_marriage {
	
	local varlab: variable label `x'	

	gen `x'_n = inlist(`x',4,5) if !mi(`x')
	replace `x'_n = `x' if mi(`x')
	label var `x'_n "Disagree: `varlab'"
	label values `x'_n yesno
	order `x'_n, after(`x')		

}
gen E2_Sdowry_girl_marriage_n_flag = mi(E2_Sdowry_girl_marriage_n)

*eststo clear	
local header	
local n_col = 0



local all " "
local treat "& B_treat==1"
local ctrl "& B_treat==0"

*** high social desirability scale
summ B_Ssocial_scale, detail
gen highsd_std=1 if B_Ssocial_scale>`r(p50)'
replace highsd_std=0 if B_Ssocial_scale<=`r(p50)' //EDIT: Originally ">=" on this line was ">", so 3,075 obs that == median were missing. 
la var highsd_std "High social desirability score"
gen treat_highsd_std = B_treat*highsd_std
la var treat_highsd_std "Treated $\times$ High social desirability score"

la var E2_Sdowry_girl_marriage_n "Against dowry"
gen dowry_sds = E2_Sdowry_girl_marriage_n*highsd_std
la var dowry_sds "Against dowry $\times$ High SDS"

gen att_sds = E2_Sgender_index2*highsd_std
la var E2_Sgender_index2 "EL2 gender attitudes index"
la var att_sds "EL2 gender attitudes index $\times$ High SDS"

foreach type in all treat ctrl {

	
	eststo dowry_`type': reg E2_Spetition_index2 E2_Sdowry_girl_marriage_n district_gender_* gender_grade_* if E2_Sdowry_girl_marriage_n_flag!=1 ``type'', cluster(Sschool_id)
	eststo dowrysds_`type': reg E2_Spetition_index2 E2_Sdowry_girl_marriage_n highsd_std dowry_sds district_gender_* gender_grade_* if E2_Sdowry_girl_marriage_n_flag!=1 ``type'', cluster(Sschool_id)
	
	eststo att_`type': reg E2_Spetition_index2 E2_Sgender_index2 district_gender_* gender_grade_* ${el2_gender_flag} if !mi(E2_Spetition_index2) ``type'', cluster(Sschool_id)
	eststo attsds_`type': reg E2_Spetition_index2 E2_Sgender_index2 highsd_std att_sds district_gender_* gender_grade_* ${el2_gender_flag} if !mi(E2_Spetition_index2) ``type'', cluster(Sschool_id)

	
	su E2_Spetition_index2 if !mi(E2_Steam_id) & E2_Sdowry_girl_marriage_n_flag!=1 ``type''
	estadd scalar ctrlmean=r(mean) : dowry_`type'*
	
	su E2_Spetition_index2 if !mi(E2_Steam_id) & E2_Sdowry_girl_marriage_n_flag!=1 & !mi(B_Ssocial_scale)``type''
	estadd scalar ctrlmean=r(mean) : dowrysds_`type'*
	
	su E2_Spetition_index2 if !mi(E2_Steam_id) & !mi(E2_Sgender_index2) ``type''
	estadd scalar ctrlmean=r(mean) : att_`type'*
	
	su E2_Spetition_index2 if !mi(E2_Steam_id) & !mi(E2_Sgender_index2) & !mi(B_Ssocial_scale) ``type''
	estadd scalar ctrlmean=r(mean) : attsds_`type'*
	
	estadd local basic "Yes": dowry_`type'* dowrysds_`type'* att_`type'* attsds_`type'*
	
		**** correlation
		corr E2_Spetition_index2 E2_Sdowry_girl_marriage_n if !mi(E2_Steam_id) & E2_Splan_college_flag!=1 ``type''

	* to get r
	local r1: di %7.3f r(rho)	
	
	* to get p value
	if (r(rho) != . & r(rho) < 1) {
		local p1=min(2*ttail(r(N)-2,abs(r(rho))*sqrt(r(N)-2)/sqrt(1-r(rho)^2)),1)
	}
	else if (r(rho)>=1 & r(rho) != .) {
		local p1=0
	}
	else if r(rho) == . {
		local p1= .
	}
	
	if  `p1' < 0.1 &  `p1' >= 0.05  local r1 `r1'\sym{*}
	else if  `p1' < 0.05 &  `p1' >= 0.01  local r1 `r1'\sym{**}
	else if  `p1' < 0.01  local r1 `r1'\sym{***}
	
	estadd local corr "`r1'": dowry_`type'*
	
	corr E2_Spetition_index2 E2_Sgender_index2 if !mi(E2_Steam_id) & !mi(E2_Sgender_index2) ``type''
	
	* to get r
	local r1: di %7.3f r(rho)	
	
	* to get p value
	if (r(rho) != . & r(rho) < 1) {
		local p1=min(2*ttail(r(N)-2,abs(r(rho))*sqrt(r(N)-2)/sqrt(1-r(rho)^2)),1)
	}
	else if (r(rho)>=1 & r(rho) != .) {
		local p1=0
	}
	else if r(rho) == . {
		local p1= .
	}
	
	if  `p1' < 0.1 &  `p1' >= 0.05  local r1 `r1'\sym{*}
	else if  `p1' < 0.05 &  `p1' >= 0.01  local r1 `r1'\sym{**}
	else if  `p1' < 0.01  local r1 `r1'\sym{***}
	
	estadd local corr "`r1'": att_`type'*


}

*** version for appendix table
la var E2_Splan_college "Plans to go to college"
gen plan_sds = E2_Splan_college*highsd_std
la var plan_sds "Plans college $\times$ High SDS"
gen asp_sds = E2_Saspiration_index2*highsd_std
la var E2_Saspiration_index2 "EL2 aspirations index"
la var asp_sds "EL2 aspirations index $\times$ High SDS"

local n_col = 4
local frame1 = subinstr("`frame'", "2.3cm", "2cm", .) // column spacing
local prehead = subinstr("`frame1'", "colno", "`n_col'", .)
local header " & Control & Control & Control & Control "

#delimit ;

esttab plansds_ctrl aspsds_ctrl dowrysds_ctrl attsds_ctrl using "$tables/sch_dowry_sds.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "& \multicolumn{2}{c}{Applied to scholarship} & \multicolumn{2}{c}{Signed petition} \\  \cmidrule(lr){2-3} \cmidrule(lr){4-5}" " ")
keep(highsd_std E2_Splan_college plan_sds E2_Saspiration_index2 asp_sds highsd_std E2_Sdowry_girl_marriage_n dowry_sds E2_Sgender_index2 att_sds) 
order( highsd_std E2_Splan_college plan_sds E2_Saspiration_index2 asp_sds highsd_std E2_Sdowry_girl_marriage_n dowry_sds E2_Sgender_index2 att_sds)
stats(ctrlmean basic N,   
labels("Dep var mean" "Basic controls" "Number of students") 
fmt(%7.3fc  %20s %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
*	APPENDIX TABLE 1.26: Treatment effects on school performance
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

use "$endline_sch", clear

**We divide each of our outcome variables by 100 to get a proportion
local outcomes scert_att_average scert_hindi_average scert_eng_average scert_mat_average scert_sc_average ///
scert_ss_average scert_overall_average board_hin board_eng board_mat board_sct board_sos ///
board_per

foreach var in `outcomes' {
	gen `var'_temp = `var'/100
}

local fe district_1 district_2 district_3	

local header " & Hindi & English & Math & Science & Social Science & All subjects"

* proportion scoring >50
eststo clear	
local n_col = 0
foreach var in scert_hindi_average_temp scert_eng_average_temp scert_mat_average_temp scert_sc_average_temp scert_ss_average_temp scert_overall_average_temp  {

	local ++n_col // number of columns

	eststo `var': reg `var' treat `fe', robust

		* Control group mean and sd
		su `var' if treat==0
		estadd scalar ctrlmean=r(mean) 
		estadd scalar ctrlsd=r(sd) 			
		
	}		
	
	local prehead = subinstr("`frame'", "colno", "`n_col'", .)

	local allcol = `n_col'+1

	#delimit ;

	esttab * using "$tables/school_performance_score.tex", 
	b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
	replace gaps label booktabs style(tex) fragment 
	prehead(" `prehead'" "& \multicolumn{`n_col'}{c}{\emph{Proportion scoring $>$50 in...}} \\  \cmidrule(lr){2-`allcol'}" "`header' \\")
	keep(treat) order(treat)
	stats(ctrlmean ctrlsd N,   
	labels("Control group mean" "Control SD" "Number of schools") 
	fmt(%7.3fc %7.3fc %7.0fc))
	postfoot("\bottomrule" "\end{tabular}" "}"); 

	#delimit cr
	
* proportion passing	
eststo clear	
local n_col = 0
foreach var in 	board_hin_temp board_eng_temp board_mat_temp board_sct_temp board_sos_temp board_per_temp  {

	local ++n_col // number of columns

	eststo `var':  reg `var' treat `fe', robust

		* Control group mean and sd
		su `var' if treat==0
		estadd scalar ctrlmean=r(mean) 
		estadd scalar ctrlsd=r(sd) 			
		
	}		
	
	local prehead = subinstr("`frame'", "colno", "`n_col'", .)

	local allcol = `n_col'+1

	#delimit ;

	esttab * using "$tables/school_performance_pass.tex", 
	b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
	replace gaps label booktabs style(tex) fragment 
	prehead(" `prehead'" 	"& \multicolumn{`n_col'}{c}{\emph{Proportion passing in...}} \\  \cmidrule(lr){2-`allcol'}" "`header' \\")
	keep(treat) order(treat)
	stats(ctrlmean ctrlsd N,   
	labels("Control group mean" "Control SD" "Number of schools") 
	fmt(%7.3fc %7.3fc %7.0fc))
	postfoot("\bottomrule" "\end{tabular}" "}"); 

	#delimit cr


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  APPENDIX TABLE 1.28:  Primary outcomes on treatment and continuous SDS (standardized)
*  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear
gen treat_social_scale = B_treat * B_Ssocial_scale
la var treat_social_scale "Treatment $\times$ Social desirability score"

foreach gender in /*girl boy*/ both {

	if "`gender'" == "girl" {
		use "$finaldata", clear
		keep if B_Sgirl == 1
		gen treat_social_scale = B_treat * B_Ssocial_scale
		la var treat_social_scale "Treatment $\times$ Social desirability score"
		local el1_list gender_index2 aspiration_index2 behavior_index2
		local el2_list gender_index2 aspiration_index2 behavior_index2 scholar_index2 petition_index2
	}
	else if "`gender'" == "boy" {
		use "$finaldata", clear
		gen treat_social_scale = B_treat * B_Ssocial_scale
		la var treat_social_scale "Treatment $\times$ Social desirability score"
		keep if B_Sgirl == 0
		local el1_list gender_index2 behavior_index2
		local el2_list gender_index2 behavior_index2 petition_index2
	}
	else if "`gender'" == "both" {
		use "$finaldata", clear
		gen treat_social_scale = B_treat * B_Ssocial_scale
		la var treat_social_scale "Treatment $\times$ Social desirability score"
		local el1_list gender_index2 aspiration_index2 behavior_index2
		local el2_list gender_index2 aspiration_index2 behavior_index2 scholar_index2 petition_index2
	}

	* endline 1
	local i=0
	foreach index in `el1_list'   {

		local ++i
		if "`index'"=="aspiration_index2" {
			reg E_S`index' B_treat B_Ssocial_scale treat_social_scale B_S`index' /* B_S`index'_flag */  /* B_S`index'_m */ district_gender_* gender_grade_* if B_Sgirl==1 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) // only for girls
		}

		else {
			reg E_S`index' B_treat B_Ssocial_scale treat_social_scale B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
		}	

		* storing coefs
		local j = 0
		foreach ctrl in B_treat B_Ssocial_scale treat_social_scale {
			
			local ++j

			local b_`j'_`i': di %7.3fc _b[`ctrl']
			local se_`j'_`i'= trim(string(_se[`ctrl'],"%7.3f"))
			local t_`j'_`i' = _b[`ctrl']/_se[`ctrl']
			local p_`j'_`i' = 2*ttail(e(df_r),abs(`t_`j'_`i''))
			local n`i': di %7.0f e(N)

			if `se_`j'_`i''>0 local se_`j'_`i' = "[`se_`j'_`i'']"
			else  local se_`j'_`i' = "`se_`j'_`i''"

			if `p_`j'_`i'' < 0.1 &  `p_`j'_`i'' >= 0.05  local b_`j'_`i' `b_`j'_`i''\sym{*}
			else if `p_`j'_`i'' < 0.05 &  `p_`j'_`i'' >= 0.01  local b_`j'_`i' `b_`j'_`i''\sym{**}
			else if `p_`j'_`i'' < 0.01  local b_`j'_`i' `b_`j'_`i''\sym{***}

		}

		* Control group mean
		su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
		if abs(r(mean)) < .00005 local m`i': di %7.3f abs(r(mean))
		else local m`i': di %7.3f r(mean)			 

	}

	foreach index in `el2_list' {

		local ++i
		if "`index'"=="aspiration_index2" {
			reg E2_S`index' B_treat B_Ssocial_scale treat_social_scale B_S`index'  /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag}  if B_Sgirl==1 & !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) // only for girls
		}

		else if "`index'"=="petition_index2" | "`index'"=="scholar_index2"  {
			eststo bs_`index' : reg E2_S`index' B_treat B_Ssocial_scale treat_social_scale district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) // no baseline index or flags
		}

		else {
			reg E2_S`index' B_treat B_Ssocial_scale treat_social_scale B_S`index'  /* B_S`index'_flag */  /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id)
		}	

		* storing coefs
		local j = 0		
		foreach ctrl in B_treat B_Ssocial_scale treat_social_scale {
			
			local ++j

			local b_`j'_`i': di %7.3fc _b[`ctrl']
			local se_`j'_`i'= trim(string(_se[`ctrl'],"%7.3f"))
			local t_`j'_`i' = _b[`ctrl']/_se[`ctrl']
			local p_`j'_`i' = 2*ttail(e(df_r),abs(`t_`j'_`i''))
			local n`i': di %7.0f e(N)

			if `se_`j'_`i''>0 local se_`j'_`i' = "[`se_`j'_`i'']"
			else  local se_`j'_`i' = "`se_`j'_`i''"

			if `p_`j'_`i'' < 0.1 &  `p_`j'_`i'' >= 0.05  local b_`j'_`i' `b_`j'_`i''\sym{*}
			else if `p_`j'_`i'' < 0.05 &  `p_`j'_`i'' >= 0.01  local b_`j'_`i' `b_`j'_`i''\sym{**}
			else if `p_`j'_`i'' < 0.01  local b_`j'_`i' `b_`j'_`i''\sym{***}

		}
		
		* Control group mean
		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
		if abs(r(mean)) < .00005 local m`i': di %7.3f abs(r(mean))
		else local m`i': di %7.3f r(mean)		 

	}

	*** combined table ***
	if "`gender'" == "girl" {
		cap file close regtab
		file open regtab using "$tables/sdsscore_std_primary_outcomes.tex", write replace

		file write regtab "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\extracolsep{2pt}}{l}*{8}{>{\centering\arraybackslash}m{1.75cm}}@{}}  \toprule" _n      
		file write regtab "& \multicolumn{3}{c}{Endline 1} & \multicolumn{5}{c}{Endline 2} \\  \cmidrule(lr){2-4} \cmidrule(lr){5-9}"	
		file write regtab " & Gender attitudes index & Girls' aspirations index & Self-reported behavior index & Gender attitudes index & Girls' aspirations index & Self-reported behavior index & Applied to scholarship (girls) & Signed petition  \\     " _n     
		file write regtab "  & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\    " _n    
		file write regtab "\midrule" _n 

		file write regtab "\textit{Panel A: Girls} & & & & & & & & \\ " _n
		file write regtab " Treated & `b_1_1' & `b_1_2' & `b_1_3' & `b_1_4' & `b_1_5' & `b_1_6' & `b_1_7' & `b_1_8'  \\    " _n 
		file write regtab "  & `se_1_1' & `se_1_2' & `se_1_3' & `se_1_4' & `se_1_5' & `se_1_6' & `se_1_7' & `se_1_8'   \\ \addlinespace[3pt]   " _n 
		file write regtab " Social desirability score & `b_2_1' & `b_2_2' & `b_2_3' & `b_2_4' & `b_2_5' & `b_2_6' & `b_2_7' & `b_2_8'  \\    " _n 
		file write regtab "  & `se_2_1' & `se_2_2' & `se_2_3' & `se_2_4' & `se_2_5' & `se_2_6' & `se_2_7' & `se_2_8'   \\ \addlinespace[3pt]   " _n 
		file write regtab " Treated $\times$ Social desirability score & `b_3_1' & `b_3_2' & `b_3_3' & `b_3_4' & `b_3_5' & `b_3_6' & `b_3_7' & `b_3_8'  \\    " _n 
		file write regtab "  & `se_3_1' & `se_3_2' & `se_3_3' & `se_3_4' & `se_3_5' & `se_3_6' & `se_3_7' & `se_3_8'   \\ \addlinespace[3pt]   " _n 
		file write regtab " Control group mean & `m1' & `m2' & `m3' & `m4' & `m5' & `m6' & `m7' & `m8' \\    " _n 
		file write regtab " Number of students & `n1' & `n2' & `n3' & `n4' & `n5' & `n6' & `n7' & `n8' \\ \addlinespace[6pt]   " _n 

		file close regtab
	}

	else if "`gender'" == "boy"{
		cap file close regtab
		file open regtab using "$tables/sdsscore_std_primary_outcomes.tex", write append

		file write regtab "\textit{Panel B: Boys} & & & & & & & & \\ " _n
		file write regtab " Treated & `b_1_1' & NA & `b_1_2' & `b_1_3' & NA & `b_1_4' & NA & `b_1_5'  \\    " _n 
		file write regtab "  & `se_1_1' & NA & `se_1_2' & `se_1_3' & NA & `se_1_4' & NA & `se_1_5'   \\ \addlinespace[3pt]   " _n 
		file write regtab " Social desirability score & `b_2_1' & NA & `b_2_2' & `b_2_3' & NA & `b_2_4' & NA & `b_2_5' \\    " _n 
		file write regtab "  & `se_2_1' & NA & `se_2_2' & `se_2_3' & NA & `se_2_4' & NA & `se_2_5'  \\ \addlinespace[3pt]   " _n 
		file write regtab " Treated $\times$ Social desirability score & `b_3_1' & NA & `b_3_2' & `b_3_3' & NA & `b_3_4' & NA & `b_3_5'  \\    " _n 
		file write regtab "  & `se_3_1' & NA & `se_3_2' & `se_3_3' & NA & `se_3_4' & NA & `se_3_5'   \\ \addlinespace[3pt]   " _n 
		file write regtab " Control group mean & `m1' & NA & `m2' & `m3' & NA & `m4' & NA & `m5' \\    " _n 
		file write regtab " Number of students & `n1' & NA & `n2' & `n3' & NA & `n4' & NA & `n5' \\ \addlinespace[6pt]   " _n 

		file close regtab

	}

	else if "`gender'" == "both" {
		cap file close regtab
		file open regtab using "$tables/sdsscore_std_primary_outcomes_combined.tex", write replace

		file write regtab "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\extracolsep{2pt}}{l}*{8}{>{\centering\arraybackslash}m{1.75cm}}@{}}  \toprule" _n      
		file write regtab "& \multicolumn{3}{c}{Endline 1} & \multicolumn{5}{c}{Endline 2} \\  \cmidrule(lr){2-4} \cmidrule(lr){5-9}"	
		file write regtab " & Gender attitudes index & Girls' aspirations index & Self-reported behavior index & Gender attitudes index & Girls' aspirations index & Self-reported behavior index & Applied to scholarship (girls) & Signed petition  \\     " _n     
		file write regtab "  & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\    " _n    
		file write regtab "\midrule" _n 

		file write regtab " Treated & `b_1_1' & `b_1_2' & `b_1_3' & `b_1_4' & `b_1_5' & `b_1_6' & `b_1_7' & `b_1_8'  \\    " _n 
		file write regtab "  & `se_1_1' & `se_1_2' & `se_1_3' & `se_1_4' & `se_1_5' & `se_1_6' & `se_1_7' & `se_1_8'   \\ \addlinespace[3pt]   " _n 
		file write regtab " Social desirability score & `b_2_1' & `b_2_2' & `b_2_3' & `b_2_4' & `b_2_5' & `b_2_6' & `b_2_7' & `b_2_8'  \\    " _n 
		file write regtab "  & `se_2_1' & `se_2_2' & `se_2_3' & `se_2_4' & `se_2_5' & `se_2_6' & `se_2_7' & `se_2_8'   \\ \addlinespace[3pt]   " _n 
		file write regtab " Treated $\times$ Social desirability score & `b_3_1' & `b_3_2' & `b_3_3' & `b_3_4' & `b_3_5' & `b_3_6' & `b_3_7' & `b_3_8'  \\    " _n 
		file write regtab "  & `se_3_1' & `se_3_2' & `se_3_3' & `se_3_4' & `se_3_5' & `se_3_6' & `se_3_7' & `se_3_8'   \\ \addlinespace[3pt]   " _n 
		file write regtab " Control group mean & `m1' & `m2' & `m3' & `m4' & `m5' & `m6' & `m7' & `m8' \\    " _n 
		file write regtab " Number of students & `n1' & `n2' & `n3' & `n4' & `n5' & `n6' & `n7' & `n8' \\ \addlinespace[6pt]   " _n 

		file write regtab "\bottomrule" _n              
		file write regtab "\end{tabular} " _n 
		file close regtab
	}
}

use "$finaldata", clear
gen treat_social_scale = B_treat * B_Ssocial_scale
la var treat_social_scale "Treatment $\times$ Social desirability score"

local i = 0
local el1_list gender_index2 behavior_index2
local el2_list gender_index2 behavior_index2 petition_index2
foreach index in `el1_list' {
	local ++i
	local controls B_treat B_S`index'  /* B_S`index'_flag */ /* B_S`index'_m */ ${el_``index''_flag} B_Ssocial_scale treat_social_scale
	foreach var of varlist `controls' {
		local varname gX`var'
		local varname = substr("`varname'", 1, 30)
		gen `varname' = B_Sgirl * `var'
	}
	reg E_S`index' B_treat B_Ssocial_scale treat_social_scale B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} gX* if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
	local p_1_`i': di %7.3f 2*ttail(e(df_r),abs(_b[gXB_treat]/_se[gXB_treat]))	 
	local p_2_`i': di %7.3f 2*ttail(e(df_r),abs(_b[gXB_Ssocial_scale]/_se[gXB_Ssocial_scale]))
	local p_3_`i': di %7.3f 2*ttail(e(df_r),abs(_b[gXtreat_social_scale]/_se[gXtreat_social_scale]))	 	 
	drop gX*
}

foreach index in `el2_list' {
	local ++i
	if "`index'" == "petition_index2"{
		local controls B_treat B_Ssocial_scale treat_social_scale
		foreach var of varlist `controls' {
			local varname gX`var'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_Sgirl * `var'
		}
		reg E2_S`index' B_treat B_Ssocial_scale treat_social_scale district_gender_* gender_grade_* gX* if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id) // no baseline index or flags
		local p_1_`i': di %7.3f 2*ttail(e(df_r),abs(_b[gXB_treat]/_se[gXB_treat]))	 
		local p_2_`i': di %7.3f 2*ttail(e(df_r),abs(_b[gXB_Ssocial_scale]/_se[gXB_Ssocial_scale]))
		local p_3_`i': di %7.3f 2*ttail(e(df_r),abs(_b[gXtreat_social_scale]/_se[gXtreat_social_scale]))	 
		drop gX*
	}

	else {
		local controls B_treat B_Ssocial_scale treat_social_scale B_S`index' /* B_S`index'_flag */  /* B_S`index'_m */ ${el2_``index''_flag}
		foreach var of varlist `controls' {
			local varname gX`var'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_Sgirl * `var'
		}
		reg E2_S`index' B_treat B_S`index'  /* B_S`index'_flag */  /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} gX* if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id) 
		local p_1_`i': di %7.3f 2*ttail(e(df_r),abs(_b[gXB_treat]/_se[gXB_treat]))	 
		local p_2_`i': di %7.3f 2*ttail(e(df_r),abs(_b[gXB_Ssocial_scale]/_se[gXB_Ssocial_scale]))
		local p_3_`i': di %7.3f 2*ttail(e(df_r),abs(_b[gXtreat_social_scale]/_se[gXtreat_social_scale]))		 
		drop gX*	
	} 
}

cap file close regtab
file open regtab using "$tables/sdsscore_std_primary_outcomes.tex", write append

file write regtab "\textit{Panel C: Girls=Boys p-values} & & & & & & & & \\"

file write regtab " Treated & `p_1_1' & NA & `p_1_2' & `p_1_3' & NA & `p_1_4' & NA & `p_1_5'  \\    " _n 
file write regtab " Social desirability score & `p_2_1' & NA & `p_2_2' & `p_2_3' & NA & `p_2_4' & NA & `p_2_5'  \\    " _n 
file write regtab " Treated $\times$ Social desirability score & `p_3_1' & NA & `p_3_2' & `p_3_3' & NA & `p_3_4' & NA & `p_3_5'  \\    " _n 

file write regtab "\bottomrule" _n              
file write regtab "\end{tabular} " _n 
file close regtab


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  APPENDIX TABLE 1.29: Heterogeneity of effects by baseline parent attitudes (Endline 2)
*  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

rename treat_girl treat_Sgirl
la var treat_Sgirl "Treated $\times$ Female"	

local treatrow B_treat B_Sgirl treat_girl
local treatrowdi B_treat treat_girl

*Parent BL attitudes
gen treat_Pgender_index2 = B_treat * B_Pgender_index2
la var treat_Pgender_index2 "Treated $\times$ Baseline parent attitudes"

local file_Pgender_index2 "patt"

local lab_Pgender_index2 "BL parent attitudes"

***New table loop (with panels)

local header_g
local header_b	
local n_col_g = 0
local n_col_b = 0

local treatrowdi_g B_treat treat_Pgender_index2
local treatrowdi_b B_treat treat_Pgender_index2

foreach var in Pgender_index2 {
	
	local i=0
	foreach index in gender_index2 aspiration_index2 behavior_index2 scholar_index2 petition_index2  {

		local treatrow B_treat B_`var' treat_`var' // vars to use in regression
		if "`index'" == "scholar_index2" | "`index'" == "petition_index2" {
			local controls district_gender_* gender_grade_* 
		}
		else {
			local controls B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} 
		}
		local pX
		foreach var1 of varlist `controls'{
			local varname pX`var1'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_`var' * `var1'
			local treatrow `treatrow' `varname'
		}

		local varlab: variable label E2_S`index'
		local header "`header' & `varlab'" // column headers with variables labels  

		*** girls
		local ++n_col_g 
		local header_g "`header_g' & `varlab'" // girls column headers


		if "`index'"=="aspiration_index2" {

			*** girls regression 
			eststo gbs_`index' : reg E2_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==1 , cluster(Sschool_id)
			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_g=abs(r(mean)) 
			else estadd scalar ctrlmean_g=r(mean)
			estadd local cluster_g = `e(N_clust)'		
			estadd local basic_g "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_g=`r(p)'

			*** combined regression 
			eststo cbs_`index' : reg E2_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_c=abs(r(mean)) 
			else estadd scalar ctrlmean_c=r(mean)
			estadd local cluster_c = `e(N_clust)'		
			estadd local basic_c "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_c=`r(p)'

		}

		else if "`index'" == "scholar_index2" {

			*** girls regression 
			eststo gbs_`index' : reg E2_S`index' `treatrow' district_gender_* gender_grade_*  if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==1 , cluster(Sschool_id)
			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_g=abs(r(mean)) 
			else estadd scalar ctrlmean_g=r(mean)
			estadd local cluster_g = `e(N_clust)'		
			estadd local basic_g "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_g=`r(p)'

			*** combined regression 
			eststo cbs_`index' : reg E2_S`index' `treatrow' district_gender_* gender_grade_*  if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_c=abs(r(mean)) 
			else estadd scalar ctrlmean_c=r(mean)
			estadd local cluster_c = `e(N_clust)'		
			estadd local basic_c "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_c=`r(p)'

		}

		else if "`index'" == "petition_index2" {

			*** boys column counts
			local header_b "`header_b' & `varlab'"
			local ++n_col_b	

			*** girls regression
			eststo gbs_`index' : reg E2_S`index' `treatrow' district_gender_* gender_grade_* if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 1, cluster(Sschool_id)
			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_g=abs(r(mean)) 
			else estadd scalar ctrlmean_g=r(mean)
			estadd local cluster_g = `e(N_clust)'		
			estadd local basic_g "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_g=`r(p)'
			
			*** boys regression
			eststo bbs_`index' : reg E2_S`index' `treatrow' district_gender_* gender_grade_*  if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 0, cluster(Sschool_id)
			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl==0
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_b=abs(r(mean)) 
			else estadd scalar ctrlmean_b=r(mean)  
			estadd local cluster_b = `e(N_clust)'		
			estadd local basic_b "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_b=`r(p)'

			*** combined regression
			eststo cbs_`index' : reg E2_S`index' `treatrow' district_gender_* gender_grade_*  if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_c=abs(r(mean)) 
			else estadd scalar ctrlmean_c=r(mean)  
			estadd local cluster_c = `e(N_clust)'		
			estadd local basic_c "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_c=`r(p)'

			*** p value calculation
			//interacted regression
			local controls `treatrow'
			foreach var1 of varlist `controls'{
				local varname gX`var1'
				local varname = substr("`varname'", 1, 30)
				gen `varname' = B_Sgirl * `var1'
			}
			reg E2_S`index' `treatrow' district_gender_* gender_grade_* gX*  if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
			local `index'_p1: di %7.3f 2*ttail(e(df_r), abs(_b[gXB_treat]/_se[gXB_treat]))
			local `index'_p2: di %7.3f 2*ttail(e(df_r), abs(_b[gXtreat_Pgender_index2]/_se[gXtreat_Pgender_index2]))
			drop gX*

		}

		else {

			*** boys column counts
			local header_b "`header_b' & `varlab'"
			local ++n_col_b	

			*** girls regression
			eststo gbs_`index' : reg E2_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 1, cluster(Sschool_id)
			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_g=abs(r(mean)) 
			else estadd scalar ctrlmean_g=r(mean)
			estadd local cluster_g = `e(N_clust)'		
			estadd local basic_g "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_g=`r(p)'
			
			*** boys regression
			eststo bbs_`index' : reg E2_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl == 0, cluster(Sschool_id)
			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0 & B_Sgirl==0
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_b=abs(r(mean)) 
			else estadd scalar ctrlmean_b=r(mean)  
			estadd local cluster_b = `e(N_clust)'		
			estadd local basic_b "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_b=`r(p)'

			*** combined regression
			eststo cbs_`index' : reg E2_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
			su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
			if abs(r(mean)) < .00005 estadd scalar ctrlmean_c=abs(r(mean)) 
			else estadd scalar ctrlmean_c=r(mean)  
			estadd local cluster_c = `e(N_clust)'		
			estadd local basic_c "Yes"
			test B_treat+treat_`var'==0
			estadd scalar Pgender_index2_pval_c=`r(p)'

			*** p value calculation
			//interacted regression
			local controls `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el2_``index''_flag} 
			foreach var1 of varlist `controls'{
				local varname gX`var1'
				local varname = substr("`varname'", 1, 30)
				gen `varname' = B_Sgirl * `var1'
			}
			reg E2_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} gX*  if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
			local `index'_p1: di %7.3f 2*ttail(e(df_r), abs(_b[gXB_treat]/_se[gXB_treat]))
			local `index'_p2: di %7.3f 2*ttail(e(df_r), abs(_b[gXtreat_Pgender_index2]/_se[gXtreat_Pgender_index2]))
			drop gX*
		}
		drop pX*
	}
}

***combined results***
local frame1 = subinstr("`frame'", "2.3cm", "1.9cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col_g'", .)

#delimit ;

esttab cbs_* using "$tables/el2_heterog_gen_`file_Pgender_index2'_combined.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment
prehead("`prehead'" "`header_g' \\")
keep(`treatrowdi_g') order(`treatrowdi')
stats(ctrlmean_c basic_c N,   
labels("Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %7.3fc %20s %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr 

***panel results***

local frame1 = subinstr("`frame'", "2.3cm", "1.9cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col_g'", .)

#delimit ;

esttab gbs_* using "$tables/el2_heterog_gen_`file_Pgender_index2'.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment
prehead("\begin{flushleft} Panel A: Girls \end{flushleft} \vspace{2mm} `prehead'" "`header_g' \\")
keep(`treatrowdi_g') order(`treatrowdi')
stats(Pgender_index2_pval_g ctrlmean_g basic_g N,   
labels("p-val: Treated + Treated $\times$ Above median attitudes = 0" "Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %7.3fc %20s %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr 

local frame1 = subinstr("`frame'", "2.3cm", "3.35cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col_b'", .)

#delimit ; 

esttab bbs_* using "$tables/el2_heterog_gen_`file_Pgender_index2'.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
append gaps label booktabs style(tex) fragment
prehead("\begin{flushleft} Panel B: Boys \end{flushleft} `prehead'" "`header_b' \\")
keep(`treatrowdi_b') order(`treatrowdi')
stats(Pgender_index2_pval_b ctrlmean_b basic_b N,   
labels("p-val: Treated + Treated $\times$ Above median attitudes = 0" "Control group mean" "Basic controls" "Number of students") 
fmt(%7.3fc %7.3fc %20s %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr

local frame1 = subinstr("`frame'", "2.3cm", "3.35cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col_b'", .)

file open sumstat using "$tables/el2_heterog_gen_`file_Pgender_index2'.tex", write append 
file write sumstat "\begin{flushleft} Panel C: P-vals, Boys vs. Girls \end{flushleft} `prehead' `header_b' \\"
local follow_head
foreach i of numlist 1/2 {
	local follow_head "`follow_head' & \multicolumn{1}{c}{(`i')}"
}
file write sumstat "`follow_head' \\ \midrule "
file write sumstat "Treated & `gender_index2_p1' & `behavior_index2_p1' & `petition_index2_p1' \\ "
file write sumstat "Treated X Baseline parent attitudes & `gender_index2_p2' & `behavior_index2_p2' & `petition_index2_p1' \\ "
file write sumstat "\bottomrule \end{tabular} }"
file close sumstat

**** additional paperstat Seema requested for paper
/* commenting because uses encrypted private data; logged in adhoc analysis, and writing hardcoded values to paper stat
cap veracrypt, dismount drive(A)
veracrypt "$main_loc/Data/Endline/rawdata/Raw data", mount drive(A)
import delimited "A:\breakthrough_endline_student_WIDE.csv", clear
keep starttime child_id
gen startdate = date(starttime, "DMYhms")
gen compdate = date("Sept 30, 2016", "MDY")
gen comp = startdate - compdate
duplicates drop child_id, force
merge 1:1 child_id using "$finaldata"
keep if _merge == 3
gen comp_months = month(comp)
log using "$ad_hoc/EL1_survey_date.smcl", replace
sum comp_months, d
log close
*/

cap file write paperstat "3.3Study design pg 11, Distance between Endline 1 survey date and program end (Sept 30 2016), avg = 3.53, min = 2, max = 10" _n


}

putexcel clear

********************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************
*** Part 4: slides table - baseline chars
********************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************

if `c4'==1 {	

***  slide tables

**********************************
*** slides table - baseline chars
**********************************
use "$finaldata", clear

//making below loop work
foreach var in B_Sgirl B_Sgrade6 B_Ssocial_scale_int_imp B_Ssocial_scale_belowm B_Ssocial_scale  {
	gen `var'_flag = mi(`var')
}

cap file close sumstat
file open sumstat using "$slides/el2_tab_bal_chars.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\extracolsep{3pt}}{l}*{4}{>{\centering\arraybackslash}m{2cm}}@{}}   \toprule" _n      // table header
file write sumstat "  & Treatment & Control & Standardized Diff    \\     " _n         
file write sumstat "\midrule" _n 


gen all_flag = 0 // 0 if all variables in the list missing
foreach var in Sage Sgirl Shindu Sgrade6 Scaste_sc  m_illiterate m_fulltime Sflush_toilet  {     
	
	local varlab: variable label B_`var'
	
	sum B_`var' if Sschool_id!=2711 & child_id!=3205037 & B_`var'_flag==0 
	local totalmean = `r(mean)'	
	local totalsd =`r(sd)'
	
	sum B_`var' if B_treat==1 & Sschool_id!=2711 & child_id!=3205037 & B_`var'_flag==0
	local treat = `r(mean)'
	local treatmean: di %7.3f `r(mean)'	
	local treatsd: di %7.3f `r(sd)'
	
	sum B_`var' if B_treat==0 & Sschool_id!=2711 & child_id!=3205037 & B_`var'_flag==0
	local control = `r(mean)'
	local controlmean: di %7.3f `r(mean)'	
	local controlsd: di %7.3f `r(sd)'
	
	gen stand_diff = (`treat'-`control')/ `totalsd'
	sum stand_diff
	local diff: di %7.3f `r(mean)'


	foreach x in treatsd controlsd diff {   // to remove leading blank spaces from numbers, and round the number to 3 decimal spaces
	local `x'=trim(string(``x'',"%7.3f"))
	
	replace all_flag = 1 if B_`var'_flag==0 	
}


// we write out tex table lines showing means followed by sd in brackets.


file write sumstat "`varlab'&   `treatmean' & `controlmean'  & `diff'        \\     " _n  
file write sumstat "  & [`treatsd']  & [`controlsd']  &    \\   \addlinespace[3pt]   " _n  

drop stand_diff

}

file write sumstat "\midrule" _n

count if B_treat!=. & Sschool_id!=2711 & child_id!=3205037 & all_flag==1    // baseline sample counts
local total: di %7.0fc `r(N)'

count if B_treat==0 & Sschool_id!=2711 & child_id!=3205037 & all_flag==1
local control: di %7.0fc `r(N)'

count if B_treat==1 & Sschool_id!=2711 & child_id!=3205037 & all_flag==1
local treat: di %7.0fc `r(N)'

file write sumstat "Number of students &`treat' &`control' &   \\   " _n 

drop all_flag

file write sumstat "\bottomrule" _n           // table footer
file write sumstat "\end{tabular}" _n 
file close sumstat


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*	
*	Persuasion Rate
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
use "$finaldata", clear

local gender  E_Swives_less_edu_n E_Select_woman_y E_Sboy_more_oppo_n E_Stown_studies_y ///
E_Sman_final_deci_n E_Swoman_viol_n E_Scontrol_daughters_n E_Swoman_role_home_n ///
E_Smen_better_suited_n E_Ssimilar_right_y ///
E_Smarriage_more_imp_n E_Steacher_suitable_n E_Sgirl_marriage_age_19 ///
E_Smarriage_age_diff_m E_Sstudy_marry E_Sallow_work_y E_Sfertility

	* removing imputed values to get 1 for gender progressive asnwer and 0 for not
	local gender_ni
	local gender_nm	
	foreach var in `gender'  {	
		gen `var'_ni = `var' if `var'_flag==0 // non-imputed variable
		gen `var'_nm =  `var'_flag==0 // variable is not-missing
		local gender_ni `gender_ni' `var'_ni
		local gender_nm `gender_nm' `var'_nm
	}
	
* 
foreach var in `gender' {
	gen q_`var' = `var'_ni // for reshape
}

* program awareness
gen E_Sprogram_aware_ni = E_Sprogram_aware if E_Sprogram_aware_flag==0

// NOTE : can't do all non-missing	
egen att_sum=rsum2(`gender_ni'), allmiss // only missing if all missing
egen nm_sum = rsum2(`gender_nm'), allmiss // total non-missing variables for observation

gen att_perc = (att_sum/nm_sum)*100
la var att_perc "Gender Progressive Attitudes \%"

cap file close sumstat
file open sumstat using "$tables/el2_tab_persuasion_rate.tex", write replace

file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{2}{>{\centering\arraybackslash}m{2.0cm}}@{}}   \toprule" _n      // table header
file write sumstat "  & Rate   \\     " _n         
file write sumstat "\midrule" _n 

su E_Sprogram_aware_ni
local aware = `r(mean)'*100
local aware: di %7.2f `aware'		
file write sumstat "Awareness of program in treatment group & `aware'\% \\    " _n 

su att_perc if B_treat==0
local ctrl = `r(mean)'
local ctrldi: di %7.2f `ctrl'
file write sumstat "Progressive gender attitudes in control group & `ctrldi'\% \\    " _n 

su att_perc if B_treat==1
local treat = `r(mean)'
local treatdi: di %7.2f `treat'
file write sumstat "Progressive gender attitudes in treatment group & `treatdi'\% \\    " _n 

* persuasion rate
local prate = ((100-`ctrl') - (100-`treat')) *100 / (100-`ctrl')
local prate: di %7.2f `prate'

local stat =((100-`ctrl') - (100-`treat')) *100 / (100-`ctrl')
local stat:	di  %7.4f `stat'
cap file write paperstat "Abstract, persuasion rate EL1, `stat'%" _n
cap file write paperstat "Intro pg3, persuasion rate EL1, `stat'%" _n
cap file write paperstat "4.2Short-run results pg15, persuasion rate EL1, `stat'%" _n

local stat = 100 - `ctrl'
local stat: di  %7.4f `stat'
cap file write paperstat "4.2Short-run results pg15, EL1 control group % with gender-regressive views, `stat'%" _n

local stat = 100 - `treat'
local stat: di  %7.4f `stat'
cap file write paperstat "4.2Short-run results pg15, EL1 treatment group % with gender-regressive views, `stat'%" _n


di `prate'
file write sumstat "Persuasion rate assuming 100\% exposure in treatment group & `prate'\% \\    " _n 

* persuasion rate with program awareness
su att_perc if B_treat==1 & E_Sprogram_aware_ni==1
local treat_a = `r(mean)'
local prate_a = ((100-`ctrl') - (100-`treat_a')) *100 / (100-`ctrl')
local prate_a: di %7.2f `prate_a'

file write sumstat "Persuasion rate using program awareness rate in treatment group & `prate_a'\% \\    " _n 

file write sumstat "\bottomrule" _n              
file write sumstat "\end{tabular}" _n 
file close sumstat	

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
* EL1 Primary Outcomes (for the entire sample) (basic controls)
* EL1 - Not Imputed - No Drop - (behavior old vars) - Controlling for missing flags
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

eststo clear	
local header	
local n_col = 0

foreach index in gender_index2 aspiration_index2 behavior_index2    {		

	local varlab: variable label E_S`index'
	local header "`header' & `varlab'" // column headers with variables labels 

	local ++n_col // number of columns		

	if strpos("`index'", "aspiration")>0 {
		eststo bs_`index' : reg E_S`index' B_treat B_S`index'  /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} if B_Sgirl==1 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) // only for girls

		su E_S`index' if B_treat==0 & !mi(E_Steam_id) & B_Sgirl==1 & attrition_el==0
		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean)
	}

	else {
		eststo bs_`index' : reg E_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)

		su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean)
	}

	estadd local cluster = `e(N_clust)'		
	estadd local basic "Yes"		

}
local frame1 = subinstr("`frame'", "2.3cm", "3.3cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col'", .)

#delimit ;

esttab bs_* using "$slides/el1_primary_outcomes_basic.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "`header' \\")
keep(B_treat) order(B_treat)
stats(N,   
labels("Number of students") 
fmt(%7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	 EL2 Primary Outcomes (Basic Controls)
*	EL2 - Not Imputed - No Drop - (gender EL1 weights) - Controlling for missing flags
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

eststo clear	
local header	
local n_col = 0
foreach index in gender_index2 aspiration_index2 behavior_index2 petition_index2 scholar_index2  {

	local varlab: variable label E2_S`index'
	local header "`header' & `varlab'" // column headers with variables labels 

	local ++n_col // number of columns

	if "`index'"=="petition_index2" | "`index'"=="scholar_index2"  {
		eststo bs_`index' : reg E2_S`index' B_treat district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id) // no baseline index or flags
		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
		estadd scalar ctrlmean=r(mean) 
	}

	else if "`index'"=="aspiration_index2" {
		eststo bs_`index' : reg E2_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} if B_Sgirl==1 & !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) // only for girls

		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & B_Sgirl==1 & attrition==0
		if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
		else estadd scalar ctrlmean=r(mean)
	}

	else {
		eststo bs_`index' : reg E2_S`index' B_treat B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
		estadd scalar ctrlmean=r(mean) 
	}

	estadd local cluster = `e(N_clust)'		
	estadd local basic "Yes"

}

local prehead = subinstr("`frame'", "colno", "`n_col'", .)

#delimit ;

esttab bs_* using "$slides/el2_primary_outcomes_basic.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "`header' \\")
keep(B_treat) order(B_treat)
stats(ctrlmean N,   
labels("Control group mean" "Number of students") 
fmt(%7.3fc %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr

	***

	
	rename treat_girl treat_Sgirl
	la var treat_Sgirl "Treated x Female"	

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	EL2 Primary Outcomes (heterogeneity by gender)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

local treatrow B_treat B_Sgirl treat_Sgirl
local treatrowdi B_treat treat_Sgirl

eststo clear	
local header	
local n_col = 0
foreach index in gender_index2 behavior_index2   {

	local varlab: variable label E_S`index'
	local header "`header' & `varlab'" // column headers with variables labels 

	local ++n_col // number of columns

	eststo bs_`index' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)

	if "`index'"=="behavior_index2" {
		local stat: di %7.4fc _b[treat_Sgirl]
		cap file write paperstat "4.2Short-run results pg17, EL1 behavior treated*female coef, `stat'   " _n 
		local t = _b[treat_Sgirl]/_se[treat_Sgirl]
		local p = 2*ttail(e(df_r),abs(`t'))

		local stat: di %7.5fc `p'			
		cap file write paperstat "4.2Short-run results pg17, EL1 behavior treated*female pval, `stat'   " _n 

	}

	su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
	estadd scalar ctrlmean=r(mean) 

	estadd local cluster = `e(N_clust)'		
	estadd local basic "Yes"

	test B_treat+treat_Sgirl==0
	estadd scalar pval=`r(p)'

}

local frame1 = subinstr("`frame'", "2.3cm", "3.3cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col'", .)


#delimit ;

esttab bs_* using "$slides/el1_primary_outcomes_hetgen.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "`header' \\")
keep(`treatrowdi') order(`treatrowdi')
stats(ctrlmean N,   
labels("Control group mean" "Number of students") 
fmt(%7.3fc %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr



* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	EL2 Primary Outcomes (heterogeneity by gender)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

local treatrow B_treat B_Sgirl treat_Sgirl
local treatrowdi B_treat treat_Sgirl

eststo clear	
local header	
local n_col = 0
foreach index in gender_index2 behavior_index2 petition_index2  {

	local varlab: variable label E2_S`index'
	local header "`header' & `varlab'" // column headers with variables labels 

	local ++n_col // number of columns

	if "`index'"=="petition_index2" {
		eststo bs_`index' : reg E2_S`index' `treatrow' district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id) // no baseline index or flags
		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
		estadd scalar ctrlmean=r(mean) 
	}


	else {
		eststo bs_`index' : reg E2_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el2_``index''_flag} if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)


		su E2_S`index' if B_treat==0 & !mi(E2_Steam_id) & attrition==0
		estadd scalar ctrlmean=r(mean) 
	}

	estadd local cluster = `e(N_clust)'		
	estadd local basic "Yes"

	test B_treat+treat_Sgirl==0
	estadd scalar pval=`r(p)'


}

local frame1 = subinstr("`frame'", "2.3cm", "3.3cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col'", .)


#delimit ;

esttab bs_* using "$slides/el2_primary_outcomes_hetgen.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "`header' \\")
keep(`treatrowdi') order(`treatrowdi')
stats(ctrlmean N,   
labels("Control group mean" "Number of students") 
fmt(%7.3fc %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*	Effects by gender and age (endline 1)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

la var B_Schild_age "Age"

gen female_age = B_Sgirl*B_Schild_age
la var female_age "Female x Age"

local treatrow 	B_Sgirl B_Schild_age female_age 
local treatrowdi B_Sgirl B_Schild_age female_age 

local fe B_Sgrade6 district_1 district_2 district_3 district_4 // for female variable to show

eststo clear	
local header	
local n_col = 0

** endline 1 - control only
foreach index in gender_index2 behavior_index2   {

	local varlab: variable label E_S`index'
	local header "`header' & `varlab'" // column headers with variables labels 

	local ++n_col // number of columns		

	eststo E_`index' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ `fe' ${el_``index''_flag} if !mi(E_Steam_id) & attrition_el==0 & B_treat==0 , cluster(Sschool_id)

}

local frame1 = subinstr("`frame'", "2.3cm", "3.3cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col'", .)

#delimit ;

esttab E_* using "$slides/el1_gender_age_control.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "`header' \\")
keep(`treatrowdi') order(`treatrowdi')
stats(ctrlmean N,   
labels("Control group mean" "Number of students") 
fmt(%7.3fc %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*		Table : Attitude persistence in control group (endline 1)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear


lab var B_Sgender_index2 "Baseline Gender Attitudes Index"
lab var E_Sgender_index2 "Gender Attitudes Index"

eststo clear	
local header	
local n_col = 0

foreach var in E_Sgender_index2 {
	
	local varlab: variable label `var'
	local header "`header' & `varlab'" // column headers with variables labels 

	local ++n_col // number of columns	
	eststo coef: reg `var' B_Sgender_index2 if !mi(E_Steam_id) & attrition_el==0 & B_treat==0	
}

local frame1 = subinstr("`frame'", "2.3cm", "3.3cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col'", .)

#delimit ;

esttab coef using "$slides/el1_att_persist.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "`header' \\")
keep(B_Sgender_index2) order(B_Sgender_index2)
stats(N,   
labels("Observations") 
fmt(%7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*		Table : Gender attitudes as percentage (endline 1)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

local gender_b B_Schild_woman_role_n B_Schild_man_final_deci_n B_Schild_woman_tol_viol_n B_Schild_wives_less_edu_n ///
B_Schild_boy_more_opps_n B_Schild_equal_opps_y B_Schild_girl_allow_study_y B_Schild_similar_right_y B_Schild_elect_woman_y

local gender_e E_Swives_less_edu_n E_Select_woman_y E_Sboy_more_oppo_n E_Stown_studies_y ///
E_Sman_final_deci_n E_Swoman_viol_n E_Scontrol_daughters_n E_Swoman_role_home_n ///
E_Smen_better_suited_n E_Ssimilar_right_y ///
E_Smarriage_more_imp_n E_Steacher_suitable_n E_Sgirl_marriage_age_19 ///
E_Smarriage_age_diff_m E_Sstudy_marry E_Sallow_work_y E_Sfertility

local stat: di wordcount("`gender_e'")

cap file write paperstat "3.4Primary Outcomes pg12, EL1 gender attitudes variable count, `stat'" _n

local aspiration_e E_Sboard_score_median E_Shighest_educ_median E_Sdiscuss_educ E_Soccupa_25_white E_Scont_educ
local stat: di wordcount("`aspiration_e'")
cap file write paperstat "3.4Primary Outcomes pg12, EL1 girls' aspirations variable count, `stat'" _n

local behavior_common E_Stalk_opp_gender_comm E_Ssit_opp_gender_comm E_Scook_clean_comm ///
E_Sabsent_sch_hhwork_comm E_Sdiscourage_college_comm E_Sdiscourage_work_comm

local stat: di wordcount("`behavior_common'")

cap file write paperstat "3.4Primary Outcomes pg12, EL1 common behavior variable count, `stat'" _n



* changing to missing for this analysis 
foreach var in `gender_b' `gender_e' {
	replace `var' = . if `var'_flag==1
}

foreach list in gender_b gender_e {
	egen `list'_nomiss = rownonmiss(``list'')
	egen `list'_sum = rowtotal(``list'')
}

gen E_Sgender_perc = gender_e_sum/gender_e_nomiss if gender_e_nomiss>0
gen B_Sgender_perc = gender_b_sum/gender_b_nomiss if gender_b_nomiss>0
la var E_Sgender_perc "Gender Attitudes (percentage)"
la var B_Sgender_perc "BL Gender Attitudes (percentage)"

bys Sschool_id B_Sgirl: egen B_Sgender_perc_m = mean(B_Sgender_perc)	
label var B_Sgender_perc_m "BL Gender Attitudes (percent) (School-gender Avg)"

eststo clear	
local header	
local n_col = 0

foreach var in E_Sgender_perc {
	
	local varlab: variable label `var'
	local header "`header' & `varlab'" // column headers with variables labels 

	local ++n_col // number of columns	
	eststo coef_b: reg `var' B_Sgender_perc B_Sgender_perc_m district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 & B_treat==0 , cluster(Sschool_id) //edited by jake, removing treatment coefficient

	su B_Sgender_perc if !mi(E_Steam_id) & attrition_el==0 & B_treat == 0
	estadd scalar mean=r(mean) 
	su B_Sgender_perc if !mi(E_Steam_id) & attrition_el==0 & B_treat == 0
	estadd scalar sd=r(sd) 

	su B_Sgender_perc if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==1 & B_treat == 0 
	local mean_g = `r(mean)'
	su B_Sgender_perc if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==0 & B_treat == 0
	local mean_b = `r(mean)'

	local diff = `mean_g' - `mean_b'
	local diff: di %7.3fc `diff'
	estadd local diff "`diff'"

	eststo coef_t: reg `var' B_Sgender_perc B_treat B_Sgender_perc_m district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) //edited by jake, removing treatment coefficient

	su B_Sgender_perc if !mi(E_Steam_id) & attrition_el==0
	estadd scalar mean=r(mean) 
	su B_Sgender_perc if !mi(E_Steam_id) & attrition_el==0
	estadd scalar sd=r(sd) 

	su B_Sgender_perc if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==1
	local mean_g = `r(mean)'
	su B_Sgender_perc if !mi(E_Steam_id) & attrition_el==0 & B_Sgirl==0
	local mean_b = `r(mean)'

	local diff = `mean_g' - `mean_b'
	local diff: di %7.3fc `diff'
	estadd local diff "`diff'"
}

local frame1 = subinstr("`frame'", "2.3cm", "3.3cm", .) // column spacing	
local prehead = subinstr("`frame1'", "colno", "`n_col'", .)

#delimit ;

esttab coef_b using "$slides/el1_att_perc.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "`header' \\")
keep(B_Sgender_perc) order(B_Sgender_perc)
stats(mean sd diff N,   
labels("Baseline analogue mean" "Baseline analogue SD" "Baseline analogue girl-boy difference in means" "Observations") 
fmt(%7.3fc %7.3fc %7.3fc %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr

#delimit ;

esttab coef_t using "$slides/el1_att_perc_t.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "`header' \\")
keep(B_treat) order(B_treat)
stats(mean sd diff N,   
labels("Baseline analogue mean" "Baseline analogue SD" "Baseline analogue girl-boy difference in means" "Observations") 
fmt(%7.3fc %7.3fc %7.3fc %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*		Table : Behavior subindices on treat##female (endline 1)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

	**** creating variables
	
	*Baseline	
	egen B_Scook_clean_y = rowmax(B_Shelp_cook B_Shelp_clean B_Shelp_clean_home B_Shelp_laundry)
	lab var B_Scook_clean_y "Child cooks/cleans/wash clothes"

	egen B_Stake_care_young_sib_y = rowmax(B_Shelp_care_child B_Shelp_care_old B_Shelp_care_child B_Shelp_care_old)
	lab var B_Stake_care_young_sib_y "Child takes care of young sibling/old people"	

	gen B_Sabsent_sch_n=inlist(B_Sabsent_sch,0) if !mi(B_Sabsent_sch)
	lab var B_Sabsent_sch_n "During last week student was not absent from school"

	* Endline
	
	gen E_Sabsent_sch_n=inlist(E_Sabsent_days,0) if !mi(E_Sabsent_days)
	lab var E_Sabsent_sch_n "During last week student was not absent from school"
	
	gen E_Sabsent_sch_hhwork_n=inlist(E_Sabsent_sch_reason_hhwork,0) if !mi(E_Sabsent_sch_reason_hhwork)
	lab var E_Sabsent_sch_hhwork_n "Student has not missed school due to household responsibilities in the last one month"


	local varlist E_Sdiscourage_college E_Sdiscourage_work
	
	foreach x in `varlist' {

		gen `x'_n=inlist(`x',0) if !mi(`x')
		local varlab: variable label `x' 
		label var `x'_n "Disagree: `varlab'"
	}
	
	local E_Stalk_opp_gender_comf B_Stalk_opp_gender_y
	local E_Ssit_opp_gender
	local E_Scook_clean_y B_Scook_clean_y
	local E_Stake_care_young_sib_y B_Stake_care_young_sib_y
	local E_Shh_shopping_y B_Shelp_get_groce
	
	local E_Salone_friend B_Salone_friend
	local E_Sabsent_sch_n B_Sabsent_sch_n
	local E_Sabsent_sch_hhwork_n
	local E_Sdiscourage_work_n
	local E_Sdiscourage_college_n
	

	lab var E_Stalk_opp_gender_comf "Comfortable talking to students of the opposite sex"
	lab var E_Ssit_opp_gender "Do you sit next to students of the opposite sex in class?"
	lab var E_Scook_clean_y "At least once a week: Cook/clean/wash clothes"
	lab var E_Stake_care_young_sib_y "At least once a week: Take care of younger siblings/old persons"
	lab var E_Shh_shopping_y "At least once a week: Went shopping for household provisions/paid bills"

	lab var E_Salone_friend "Allowed to go to school alone or with friends?"
	lab var E_Sabsent_sch_n "During last week, was not absent from school"
	lab var E_Sabsent_sch_hhwork_n "Has not missed school due to household responsibilities in last week"
	lab var E_Sdiscourage_work_n "Disagree: Do you discourage your sister/cousin sister to work outside of home?"
	lab var E_Sdiscourage_college_n "Disagree: Do you discourage your sister/cousin sister from studying in college if it is far"



	local beh1	E_Stalk_opp_gender_comf E_Ssit_opp_gender E_Scook_clean_y E_Stake_care_young_sib_y E_Shh_shopping_y
	local beh2	E_Salone_friend E_Sabsent_sch_n E_Sabsent_sch_hhwork_n E_Sdiscourage_work_n E_Sdiscourage_college_n


	rename treat_girl treat_Sgirl
	la var treat_Sgirl "Treated x Female"	

	local treatrow B_treat B_Sgirl treat_Sgirl
	local treatrowdi B_treat B_Sgirl treat_Sgirl

	local fe B_Sgrade6 district_1 district_2 district_3 district_4 // to allow female var to show

	forval i = 1/2 {

		eststo clear	
		local header	
		local n_col = 0
		foreach var in `beh`i''  {

			if "``var''"!="" { // BL var not missing 
	** gen school-gender mean
	egen ``var''_m = mean(``var''), by(Sschool_id  B_Sgirl)
}

local varlab: variable label `var'
local header "`header' & `varlab'" // column headers with variables labels

local ++n_col // number of columns

if "``var''"!="" {
	eststo : reg `var' `treatrow' ``var'' ``var''_m  `fe' if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) 
}

else {
	eststo : reg `var' `treatrow' `fe' if !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id) 
}

su `var' if !mi(E_Steam_id) &  attrition_el==0 
estadd scalar mean=r(mean) 	

}

local prehead = subinstr("`frame'", "colno", "`n_col'", .)

#delimit ;

esttab * using "$slides/el1_behavior_si`i'.tex", 
b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
replace gaps label booktabs style(tex) fragment 
prehead(" `prehead'" "`header' \\")
keep(`treatrowdi') order(`treatrowdi')
stats(mean  N,   
labels("Dep var mean" "Number of students") 
fmt(%7.3fc %7.0fc))
postfoot("\bottomrule" "\end{tabular}" "}"); 

#delimit cr

}

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*		Table : Behavior subindices and SDS
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear

foreach var in B_Stake_care_young_sib_g B_Stake_care_young_sib_b {
	bys Sschool_id B_Sgirl: egen `var'_m = mean (`var')	
}

summ B_Ssocial_scale, detail
gen highsd_std=1 if B_Ssocial_scale>`r(p50)'
replace highsd_std=0 if B_Ssocial_scale<=`r(p50)' //EDIT: Originally ">=" on this line was ">", so 3,075 obs that == median were missing. 
la var highsd_std "High social desirability score"
gen treat_highsd_std = B_treat*highsd_std
la var treat_highsd_std "Treated $\times$ High social desirability score"


foreach sds in highsd_std  {

	local treatrow B_treat `sds' treat_`sds'

	eststo clear	
	eststo c1: reg E_Sbehavior_index2_hhchores `treatrow' B_Sbehavior_index2_hhchores B_Sbehavior_index2_hhchores_flag B_Sbehavior_index2_hhchores_m  ///
	district_gender_* gender_grade_*  if B_Sgirl==1 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)


	eststo c2: reg E_Sbehavior_index2_hhchores `treatrow' B_Sbehavior_index2_hhchores B_Sbehavior_index2_hhchores_flag B_Sbehavior_index2_hhchores_m  ///
	district_gender_* gender_grade_* if B_Sgirl==0 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)

	eststo c3: reg E_Stake_care_young_sib_g `treatrow' B_Stake_care_young_sib_g B_Stake_care_young_sib_g_m  ///
	district_gender_* gender_grade_*  B_Stake_care_young_sib_g_flag if E_Stake_care_young_sib_g_flag==0 & B_Sgirl==1 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)


	eststo c4: reg E_Stake_care_young_sib_b `treatrow' B_Stake_care_young_sib_b B_Stake_care_young_sib_b_m  ///
	district_gender_* gender_grade_*  B_Stake_care_young_sib_b_flag if E_Stake_care_young_sib_b_flag==0 & B_Sgirl==0 & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)


	local n_col = 4
	local header " & Participation in HH chores sub-index (girls) & Participation in HH chores sub-index (boys) & Girl does not take care of young siblings/old people & Boy takes care of young siblings/old people"
	local prehead = subinstr("`frame'", "colno", "`n_col'", .)


	#delimit ;

	esttab c* using "$slides/el1_beh_si_sds.tex", 
	b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
	replace gaps label booktabs style(tex) fragment 
	prehead(" `prehead'" "`header' \\")
	keep(B_treat `sds' treat_`sds') order(B_treat `sds' treat_`sds')
	stats( N,   
	labels("Number of students") 
	fmt(%7.0fc))
	postfoot("\bottomrule" "\end{tabular}" "}"); 

	#delimit cr

}

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*		Table : EL1 indices on treat##female and treat##SDS
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "$finaldata", clear	

rename treat_girl treat_Sgirl
la var treat_Sgirl "Treated x Female"	

*** high social desirability scale
summ B_Ssocial_scale, detail
gen highsd_std=1 if B_Ssocial_scale>`r(p50)'
replace highsd_std=0 if B_Ssocial_scale<=`r(p50)' //EDIT: Originally ">=" on this line was ">", so 3,075 obs that == median were missing. 
la var highsd_std "High social desirability score"
gen treat_highsd_std = B_treat*highsd_std
la var treat_highsd_std "Treated $\times$ High social desirability score"

local file_highsd_std "sds"

local lab_highsd_std "Social desirability score"
gen B_highsd_std = highsd_std //artifact of below loop

foreach type in highsd_std {

*** gender and student BL attitudes
eststo clear	
local header	
local n_col = 0
local treatrowdi B_treat treat_Sgirl treat_`type' // vars to display in table

foreach var in Sgirl `type' {

	local treatrow B_treat B_`var' treat_`var' // vars to use in regression
	
	local i=0
	foreach index in gender_index2 aspiration_index2 behavior_index2  {

		if !("`var'"=="Sgirl" & "`index'"=="aspiration_index2") {
			local varlab: variable label E_S`index'
			local header "`header' & `varlab'" // column headers with variables labels 
			local ++n_col // number of columns
		}

		local ++i

		local controls B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ ${el_``index''_flag}

		cap drop gX*
		foreach var1 of varlist `controls' {
			local varname gX`var1'
			local varname = substr("`varname'", 1, 30)
			gen `varname' = B_Sgirl * `var1'
		}

		if "`var'"=="`type'" {
			eststo `var'_`i' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} gX* if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
		}

		else if "`var'"=="Sgirl"  {
			eststo `var'_`i' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} gX* if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
		}
		/*
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
			if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
else estadd scalar ctrlmean=r(mean)
				
			test B_treat+treat_`var'==0
			estadd scalar `var'_pval=`r(p)'
			
			*/
		}	
	}

	local frame1 = subinstr("`frame'", "2.3cm", "1.8cm", .) // column spacing	
	local prehead = subinstr("`frame1'", "colno", "`n_col'", .)

	#delimit ;

	esttab Sgirl_1 Sgirl_3 `type'* using "$slides/el1_heterog_gen_`file_`type''.tex", 
	b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
	replace gaps label booktabs style(tex) fragment 
	prehead("`prehead'"  "& \multicolumn{2}{c}{Gender} & \multicolumn{3}{c}{`lab_`type''} \\  \cmidrule(lr){2-3} \cmidrule(lr){4-6}" "`header' \\")
	keep(`treatrowdi') order(`treatrowdi')
	stats(   N,   
	labels(  "Number of students") 
	fmt( %7.0fc))
	postfoot("\bottomrule" "\end{tabular}" "}"); 

	#delimit cr
	
}


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*	
*		Table : EL1 indices on treat##female and treat##self-efficacy
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


use "$finaldata", clear	

rename treat_girl treat_Sgirl
la var treat_Sgirl "Treated x Female"	


rename B_Sefficacy_index2_median B_effic
rename treat_Sefficacy_index2_m treat_effic

la var B_effic "Above median BL self-efficacy attitudes"
lab var treat_effic "Treated x Above median BL self-efficacy attitudes"

local file_effic "effic"

local lab_effic "BL self-efficacy"

foreach type in effic {

*** gender and student BL attitudes
eststo clear	
local header	
local n_col = 0
local treatrowdi B_treat treat_Sgirl B_`type' treat_`type' // vars to display in table

foreach var in Sgirl `type' {

	local treatrow B_treat B_`var' treat_`var' // vars to use in regression
	
	local i=0
	foreach index in gender_index2 aspiration_index2 behavior_index2  {

		if !("`var'"=="Sgirl" & "`index'"=="aspiration_index2") {
			local varlab: variable label E_S`index'
			local header "`header' & `varlab'" // column headers with variables labels 
			local ++n_col // number of columns
		}

		local ++i

		if "`var'"=="`type'" {
			eststo `var'_`i' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag} if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
		}

		else if "`var'"=="Sgirl"  {
			eststo `var'_`i' : reg E_S`index' `treatrow' B_S`index' /* B_S`index'_flag */ /* B_S`index'_m */ district_gender_* gender_grade_* ${el_``index''_flag}  if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
		}
			/*
			su E_S`index' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
			if abs(r(mean)) < .00005 estadd scalar ctrlmean=abs(r(mean)) 
else estadd scalar ctrlmean=r(mean)
				
			test B_treat+treat_`var'==0
			estadd scalar `var'_pval=`r(p)'
			
			*/
		}	
	}

	local frame1 = subinstr("`frame'", "2.3cm", "1.8cm", .) // column spacing	
	local prehead = subinstr("`frame1'", "colno", "`n_col'", .)

	#delimit ;

	esttab Sgirl_1 Sgirl_3 `type'* using "$slides/el1_heterog_gen_`file_`type''.tex", 
	b(3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets 
	replace gaps label booktabs style(tex) fragment 
	prehead("`prehead'"  "& \multicolumn{2}{c}{Gender} & \multicolumn{3}{c}{`lab_`type''} \\  \cmidrule(lr){2-3} \cmidrule(lr){4-6}" "`header' \\")
	keep(`treatrowdi') order(`treatrowdi')
	stats(   N,   
	labels(  "Number of students") 
	fmt( %7.0fc))
	postfoot("\bottomrule" "\end{tabular}" "}"); 

	#delimit cr
	
}


}



cap file close paperstat
