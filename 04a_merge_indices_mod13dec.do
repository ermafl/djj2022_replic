	*********************************************************************************
	*	 Project: Breakthrough (BT)
	*
	*	 Purpose: 1. Merging endline 2, endline, and baseline data
	*					   2. Prepping data for main analysis
	*********************************************************************************

		clear all
		
		set more off
		cap log close
		pause on
		
	********************************************************************************
						*		MERGING DATASETS            *
	********************************************************************************

	*********************************
	*  		 	 BASELINE		    *
	*********************************

		use "$baseline", clear	
		
		**added on 30th August 2018
		drop if Schild_disability==4
		
		rename * B_*
		rename B_Sschool_id Sschool_id 
		rename B_child_id child_id
		

		la def mf 0 "male" 1 "female"
		
		* recoding gender to 0-1 
		gen B_Sgender = B_Sstudent_gender
		recode B_Sgender (1=0)(2=1)
		la var B_Sgender "Baseline student gender"
		la val B_Sgender mf
		order B_Sgender, after(B_Sstudent_gender)
		
		* changing value label names
		foreach var of varlist _all {
			local lab : value label `var'		

			if "`lab'" != "" {
			
				qui label dir 
				if strpos("`r(names)'", "bl_`lab' ")>0 { // if new label already added
					label values `var' bl_`lab' 
					cap label drop `lab'
					}
					
				if strpos("`r(names)'", "bl_`lab' ")==0 {
					label copy `lab' bl_`lab'	 // adding suffix to value label 			
					label values `var' bl_`lab' 			
					}			
				}
			}
			
		sort Sschool_id child_id
		tempfile baseline
		save `baseline', replace

		
	*********************************
	*  		 	 ENDLINE 1	 	    *
	*********************************

		use "$endline", clear

	**Dropping unnecessary observations
		drop if Sstudent_consent==0 | Sstudent_consent==.r
		drop if Sparent_consent==0
		drop if Sstudent_present==0 | Sstudent_present==.r 
		drop if Sdisability==3 | Sdisability==4 | Sdisability==5 | Sdisability==6

		**added on 30th August 2018
		drop if child_id == 3205037 // this child was blind at baseline
		
		rename * E_*
		rename E_Sschool_id Sschool_id 
		rename E_child_id child_id
		
		* changing value label names
		foreach var of varlist _all {
			local lab : value label `var'		

			if "`lab'" != "" {
			
				qui label dir 
				if strpos("`r(names)'", "el_`lab' ")>0 { // if new label already added
					label values `var' el_`lab' 
					cap label drop `lab'
					}
					
				if strpos("`r(names)'", "el_`lab' ")==0 {
					label copy `lab' el_`lab'	 // adding suffix to value label 			
					label values `var' el_`lab' 			
					}			
				}
			}
		
		sort Sschool_id child_id
		tempfile endline
		save `endline', replace
		
	*********************************
	*  		 	 ENDLINE 2 	    *
	*********************************

	use "$endline2_student_final", clear

	la def mf 0 "male" 1 "female"

	* gender
	cap drop E2_Sgender
	gen E2_Sgender = E2_Sgender_updated
	recode E2_Sgender (2=0)
	la var E2_Sgender "Endline 2 student gender" //
	la val E2_Sgender mf
	order E2_Sgender, after(E2_Sgender_updated)

	*********************************
	*    	MERGING DATASET		    *
	*********************************

		**************** merging baseline ****************
		
		merge 1:1 Sschool_id child_id using `baseline', gen(merge_bl)
		
	/*

		Result                           # of obs.
		-----------------------------------------
		not matched                           526
			from master                        44  (merge_bl==1)
			from using                        482  (merge_bl==2)

		matched                            14,327  (merge_bl==3)
		-----------------------------------------

	*/
		
		gen attrition = merge_bl
		recode attrition (1=0) (3=0) (2=1)
		lab var attrition "Sample attrition (endline 2)"
		lab define attrition 1 "Yes" 0 "No"
		lab values attrition attrition
		
		recode E2_Scaste (24=19) // to match baseline

		**************** merging endline 1 ****************
		
		merge 1:1 Sschool_id child_id using `endline', gen(merge_el)
		
		/*

		Result                           # of obs.
		-----------------------------------------
		not matched                           865
			from master                       865  (merge_el==1)
			from using                          0  (merge_el==2)

		matched                            13,988  (merge_el==3)
		-----------------------------------------


		*/
		
		gen attrition_el = merge_el
		recode attrition_el (1=1) (3=0) (2=0)
		lab var attrition_el "Sample attrition (endline 1)"
		
		replace attrition_el = 1 if child_id==1503100 // all main variables are missing, refused to asnwer questions. 

		lab values attrition_el attrition
		
		* making replacements for school not surveyed in baseline
		replace B_Sstudent_gender=2 if Sschool_id==2711
		replace B_Sgender=1 if Sschool_id==2711	
		replace B_Scaste=E_Scaste if Sschool_id==2711
		replace B_Sreligion=E_Sreligion if Sschool_id==2711
		replace B_treat=E_Streatment if Sschool_id==2711
		
		replace B_Sgirl=1 if B_Sstudent_gender==2 & Sschool_id==2711
		replace B_Sdistrict=E_Sdistrict if Sschool_id==2711
		replace B_Sclass=6 if E_Sclass==4 & Sschool_id==2711
		replace B_Sclass=7 if E_Sclass==5 & Sschool_id==2711
		replace B_Sgrade6=1 if E_Sclass==4 & Sschool_id==2711
		replace B_Sgrade6=0 if E_Sclass==5 & Sschool_id==2711
		replace B_Sgrade7=1 if E_Sclass==5 & Sschool_id==2711
		replace B_Sgrade7=0 if E_Sclass==4 & Sschool_id==2711
		
		** type of school enrolled in currently 
		gen E2_Sschool_enrolled = E_Sschool_enrolled if inlist(E2_Sschool_same,1) // if in same school
		replace E2_Sschool_enrolled = 1 if inlist(E2_Sschool_location, 1) & inlist(E2_Sschool_type, 1) /// 
			& inlist(E2_Sstudent_enrolled, 1) // govt, same vil 
		replace E2_Sschool_enrolled = 2 if inlist(E2_Sschool_location, 1) & inlist(E2_Sschool_type, 2) /// 
			& inlist(E2_Sstudent_enrolled, 1) // pvt, same vil 
		replace E2_Sschool_enrolled = 3 if inlist(E2_Sschool_location, 2, 3, 4) & inlist(E2_Sschool_type, 1) /// 
			& inlist(E2_Sstudent_enrolled, 1) // govt, other vil/town
		replace E2_Sschool_enrolled = 4 if inlist(E2_Sschool_location, 2, 3, 4) & inlist(E2_Sschool_type, 2) /// 
			& inlist(E2_Sstudent_enrolled, 1) // pvt, other vil/town
		replace E2_Sschool_enrolled = 5 if inlist(E2_Sstudent_current_status, 1) // open school
		replace E2_Sschool_enrolled = 6 if inlist(E2_Sstudent_current_status, 2) // dropped out of school
		replace E2_Sschool_enrolled = 7 if inlist(E2_Sstudent_current_status, 4) // completed school, enrolled college
		replace E2_Sschool_enrolled = 8 if inlist(E2_Sstudent_current_status, 3) // completed school but not enrolled in college
		replace E2_Sschool_enrolled = 9 if inlist(E2_Sstudent_current_status, 5) // dropped out, pursing vocational course

			la def school_enrolled 1 "Government school in same village" 2 "Private school in same village" 3 "Government school in another village" ///
			4 "Private school in another village" 5 "Open school" 6 "Dropped out of school" 7 "Enrolled in college" ///
			8 "Completed school, not enrolled in college" 9 "Dropped out, pursuing vocational course"

		la val E2_Sschool_enrolled school_enrolled
		la var E2_Sschool_enrolled "Current schooling status (school/college, type, location)"
		

		** checking main variables across 3 phases
		
		local bl "B_treat B_Sdistrict B_Spanipat B_Ssonipat B_Srohtak B_Sjhajjar B_Sgender B_Sreligion B_Scaste"
		local el "E_Streatment E_Sdistrict E_Spanipat E_Ssonipat E_Srohtak E_Sjhajjar E_Sgender E_Sreligion E_Scaste"
		local el2 "E2_Streat E2_Sdistrict E2_Spanipat E2_Ssonipat E2_Srohtak E2_Sjhajjar E2_Sgender E2_Sreligion E2_Scaste"
		

		forval i = 1/9 {
			local x:  word `i' of `bl'
			local y:  word `i' of `el'
			local z:  word `i' of `el2'
			
			qui count if `x'!=`y' & !mi(`x') & !mi(`y')
				di "Mismatches between `x' and `y': `r(N)'"
			qui count if `x'!=`z' & !mi(`x') & !mi(`z')
				di "Mismatches between `x' and `z': `r(N)'"
			qui count if `y'!=`z' & !mi(`y') & !mi(`z')
				di "Mismatches between `y' and `z': `r(N)'"			
			}
			
		* Updated: 29 mismatches between baseline and EL2 gender and 27 mismatches between EL1 and EL2 gender
		
		** updating baseline gender based on EL2 gender_updated
		gen B_Sgender_org = B_Sgirl
			la val B_Sgirl Sgirl
			la var B_Sgender_org "Baseline gender: orignal and uncorrected"
			
		replace B_Sgender=E2_Sgender  if  E2_Sgender!=B_Sgender & !mi(E2_Sgender)	
		replace B_Sgirl = B_Sgender if B_Sgirl!=B_Sgender

		*drop EL1 and EL2 versions of above variables once gender rectified
		drop `el' `el2' B_Sgender
		
		order Sschool_id child_id B_* E_* E2_*
		
		sort Sschool_id child_id
		
		** attrition edit **
		* including disabilities (except physical), not available, no consent as attrition
		gen consent = 1 
		replace consent= 0 if (E2_Sstudent_available==0) | mi(E2_Sparent_consent) // not available for survey (so consent question skipped)
		replace consent = 0 if (E2_Sparent_consent==3) | (E2_Sread_consent==0) | (E2_Sstudent_consent==0)
			order consent, before(attrition)
			
		replace attrition = 1 if inlist(consent, 0) | (inlist(E2_Sdisability, 5, 6)) 

		replace attrition = 1 if inlist(E2_Ssurvey_type, 3, 4) // if parent survey, variables used for indices all missing 

		tempfile base_end
		save `base_end', replace
		
	**********************************************
	*			BASELINE SCHOOL DATA			 *
	**********************************************	
		
		use "$baseline_sch", clear
		
		keep School_ID Coed urban q10_guest_teachr fulltime_teacher pct_male_teacher q12_activity_teachr q13_counselor q18_pta_meet ///
				q22_library q22_toilets q22_electricity q22_avail_computers q22_avail_internet q22_sports_field q22_mid_meal ///
				q22_auditorium q22_avail_edusat q21_week1 q21_week2 q21_week3 q21_week4 q21_week6 q21_week7 distance_hq
		
		gen pct_female_teacher=1-pct_male_teacher
		lab var pct_female_teacher "Percentage of female teachers"
		
		drop if School_ID==2704 // incorrectly surveyed in baseline
		
		rename * B_*
		rename B_Coed B_coed
		rename B_School_ID Sschool_id
		
		
		tempfile school
		save `school', replace
		clear 
		
		use `base_end', clear
		merge m:1 Sschool_id using `school'
			
	/*	
	  Result                           # of obs.
		-----------------------------------------
		not matched                            44
			from master                        44  (_merge==1)
			from using                          0  (_merge==2)

		matched                            14,809  (_merge==3)
		-----------------------------------------

	*/

		drop _merge
		
		sort Sschool_id child_id
		
		save `base_end', replace

	**********************************************
	*		 CENSUS 2011 DATA					 *
	**********************************************	

		use "$baseline_cen", clear
		
		keep school_id village_id Ctot_p Ctot_m Ctot_f Cm_06 Cf_06 Cp_lit Cm_lit Cf_lit Ctot_work_p Ctot_work_m Ctot_work_f  
		replace school_id=2711 if school_id==2704 // different school of the same village so we can use the census data of the same village
		
		gen Cfem_lab_part=Ctot_work_f/Ctot_f //EDIT: Female LFP is female workers/f population, not share of workers that are female 
		lab var Cfem_lab_part "Female Labor Participation"
		
		gen Cmale_lab_part=Ctot_work_m/Ctot_m  
		lab var Cmale_lab_part "Male Labor Participation"

		gen Cfem_lit_rate=Cf_lit/Ctot_f
		lab var Cfem_lit_rate "Female Adult Literacy Rate"
		
		gen Cmale_lit_rate=Cm_lit/Ctot_m
		lab var Cmale_lit_rate "Male Adult Literacy Rate"
		
		gen Csex_ratio = Cm_06/Cf_06
		la var Csex_ratio "Male:Female 0-6 Sex Ratio"

		rename school_id Sschool_id
		sort Sschool_id
		
		tempfile census
		save `census', replace

		use `base_end', clear
		merge m:1 Sschool_id using `census'
		
		
	*	 Result                           # of obs.
	*    -----------------------------------------
	*    not matched                             0
	*    matched                            14,853  (_merge==3)
	*    -----------------------------------------

		drop _merge

		
		
	********************************************************************************
					*		BASELINE PRIMARY OUTCOMES: INDICES            *
	********************************************************************************
		
		// edit: doing both girls and boys variables w.r.t control- girls median
		local g 1
		local b 0
		foreach x in g b {
		
			summ B_Shighest_edu_finance if B_treat==0 & B_Sgirl==1, detail
			local control_median= `r(p50)'
			gen B_Shighest_educ_median_`x'=1 if B_Shighest_edu_finance>`control_median' & !mi(B_Shighest_edu_finance) 
			replace B_Shighest_educ_median_`x'=0 if B_Shighest_edu_finance<=`control_median' & !mi(B_Shighest_edu_finance) 
			la val B_Shighest_educ_median_`x' yesno
			
			label var B_Shighest_educ_median_`x' "Highest level of education you would like to complete > control-gender median"
			}
			
			gen B_Shighest_educ_median = B_Shighest_educ_median_g if B_Sgirl==1
			replace B_Shighest_educ_median = B_Shighest_educ_median_b if B_Sgirl==0
			la var  B_Shighest_educ_median "Highest level of education you would like to complete > control-gender median"
				la val B_Shighest_educ_median yesno
		
		order B_Shighest_educ_median_g B_Shighest_educ_median_b B_Shighest_educ_median, after(B_Shighest_edu_finance)

		gen B_Soccupa_25_white=1 if inlist(B_Schild_occupation_25yy_4,1) | inlist(B_Schild_occupation_25yy_7,1) | inlist(B_Schild_occupation_25yy_8,1)
		replace B_Soccupa_25_white=0 if B_Soccupa_25_white!=1 & !mi(B_Schild_occupation_25yy) & B_Schild_occupation_25yy!=".r"
		lab var B_Soccupa_25_white " Student expects white collar job when he/she is 25 years old"
		order B_Soccupa_25_white, after(B_Schild_occupation_25yy_12)

		lab var B_Sdisc_edu_goals "Student has discussed education goals with parent or adult relative"

		
			**************************************************************
						*		1. Gender Attitudes            *
			**************************************************************

	**** 1.1 Generating flags for missing values and imputing with gender-dist-treat means ****

		local gender B_Schild_woman_role_n B_Schild_man_final_deci_n B_Schild_woman_tol_viol_n B_Schild_wives_less_edu_n ///
			B_Schild_boy_more_opps_n B_Schild_equal_opps_y B_Schild_girl_allow_study_y B_Schild_similar_right_y B_Schild_elect_woman_y

	su `gender'
		local gender_flag			  
		
		foreach y in `gender' {
				cap confirm variable `y'_flag
				if !_rc{
					replace `y'=. if `y'_flag
					drop `y'_flag
				}
				qui count if mi(`y')
				if r(N)!=0{
					gen `y'_flag=mi(`y')
					bys B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
					qui replace `y'=x if `y'_flag==1
					drop x
					local gender_flag `gender_flag' `y'_flag
					
				}	
			}	
		global bl_gender_flag `gender_flag'
			
	** Average Index

		cap drop check
		egen B_Sgender_index=rowtotal(`gender'), mi
		egen check=rownonmiss(`gender')

		tab check
		qui sum check
		replace B_Sgender_index=. if check<`r(max)'   
		replace B_Sgender_index=B_Sgender_index/`r(max)'
		drop check
		label var B_Sgender_index "Student gender index"
		
	** Weighted Index

	//remove
		local gender B_Schild_woman_role_n B_Schild_man_final_deci_n B_Schild_woman_tol_viol_n B_Schild_wives_less_edu_n ///
			B_Schild_boy_more_opps_n B_Schild_equal_opps_y B_Schild_girl_allow_study_y B_Schild_similar_right_y B_Schild_elect_woman_y

		
		local vars
		foreach var in `gender' {
				gen `var'_temp=`var'
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}
		
		* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
		* Set the weight=sum of row entries in the inverted covariance matrix 

			dis "`vars'"
			corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
			matrix Ainv=inv(A)										// inverse of A
			mat list Ainv
			mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
		local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
		egen B_Sgender_index2=rsum2(sum_*), anymiss
			
		cap drop sum_*		
		
		lab var B_Sgender_index "Unweighted Baseline Gender Attitudes index"
		lab var B_Sgender_index2 "Baseline Gender Attitudes Index"
		
	
	// Factor based index:
	
	
	local gender B_Schild_woman_role_n B_Schild_man_final_deci_n B_Schild_woman_tol_viol_n B_Schild_wives_less_edu_n ///
		B_Schild_boy_more_opps_n B_Schild_equal_opps_y B_Schild_girl_allow_study_y B_Schild_similar_right_y B_Schild_elect_woman_y
	qui factor `gender' if B_treat==0 /* & attrition_el==0 & !mi(E_Steam_id) */ , factors(1)
	predict B_Sgender_index_factor
	qui sum B_Sgender_index_factor if B_treat==0 & !mi(E_Steam_id) & B_Sgirl==1 & attrition_el==0
	replace B_Sgender_index_factor = (B_Sgender_index_factor - r(mean) )/r(sd)
//	corr B_Sgender_index_factor B_Sgender_index2 E_Sgender_index // test relationship between factor index and old unweighted and inverse var. weighted indices
	
	
	
	****** 1.3a Weighted Gender Attitudes index (without imputing) ********
	
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		local i=0
			foreach x in `gender' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)   // sum of weights of variables to keep
		
		** weighting the variables
		
		egen B_Sgender_index2_ni=rsum2(sum_*), anymiss
		replace B_Sgender_index2_ni = B_Sgender_index2_ni*(C[1,1]/keep_sum)
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var B_Sgender_index2_ni "BL Gender Attitudes Index"		

************************ 1.A Gender Attitudes: Education ************************

local educ B_Schild_wives_less_edu_n B_Schild_boy_more_opps_n 

	* flags for missing values
	global bl_educ_flag
	foreach var in `educ' {
		global bl_educ_flag `var'_flag
		}

**Average

	cap drop check
	egen B_Sgender_index_educ=rowtotal(`educ'), mi
	egen check=rownonmiss(`educ')

	tab check
	qui sum check
	replace B_Sgender_index_educ=. if check<`r(max)'   
	replace B_Sgender_index_educ=B_Sgender_index_educ/`r(max)'
	drop check
	label var B_Sgender_index_educ "BL Gender Attitudes (Education) index"


**Weighted
	
	local vars
	foreach var in `educ' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			}
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

				
	local i=0
		foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
		}
		
		egen B_Sgender_index2_educ=rsum2(sum_*), anymiss
		
	cap drop sum_*		
		
	lab var B_Sgender_index_educ "Unweighted Baseline Education Attitudes Sub-Index"
	lab var B_Sgender_index2_educ "Baseline Education Attitudes Sub-Index"

	
	local educ B_Schild_wives_less_edu_n B_Schild_boy_more_opps_n 
	corrmat `educ'
	factor `educ', factors(1)
	predict B_Sgender_index_factor_educ
	
	
	****** 1.Aa Weighted Gender Attitudes (education) index (without imputing) ********
		
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		local i=0
			foreach x in `educ' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)   // sum of weights of variables to keep
		
		** weighting the variables
		
		egen B_Sgender_index2_educ_ni=rsum2(sum_*) , anymiss
		replace B_Sgender_index2_educ_ni = B_Sgender_index2_educ_ni*(C[1,1]/keep_sum)
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var B_Sgender_index2_educ_ni "Baseline Education Attitudes Sub-Index"		



************************ 1.B Gender Attitudes: Employment ************************

	gen B_Sgender_index_emp = B_Schild_woman_role_n
	gen B_Sgender_index2_emp = B_Schild_woman_role_n
	
	lab var B_Sgender_index_emp "Unweighted Baseline Employment Attitudes Sub-Index"
	lab var B_Sgender_index2_emp "Baseline Employment Attitudes Sub-Index"
	
	gen B_Sgender_index2_emp_ni = B_Schild_woman_role_n if B_Schild_woman_role_n_flag==0
	lab var B_Sgender_index2_emp_ni "Baseline Employment Attitudes Sub-Index"

************************ 1.C Gender Attitudes: Female Gender Roles ************************

	local sub B_Schild_man_final_deci_n B_Schild_woman_tol_viol_n B_Schild_similar_right_y B_Schild_elect_woman_y

	* flags for missing values
	global bl_sub_flag
	foreach var in `educ' {
		global bl_sub_flag `var'_flag
		}
		
**Average

	cap drop check
	egen B_Sgender_index_sub=rowtotal(`sub'), mi
	egen check=rownonmiss(`sub')

	tab check
	qui sum check
	replace B_Sgender_index_sub=. if check<`r(max)'   
	replace B_Sgender_index_sub=B_Sgender_index_sub/`r(max)'
	drop check
	label var B_Sgender_index_sub "Student gender index (Female gender roles)"


**Weighted
	
	local vars
	foreach var in `sub' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			}
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

				
	local i=0
		foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
		}
		
		egen B_Sgender_index2_sub=rsum2(sum_*), anymiss
		
	cap drop sum_*		

	lab var B_Sgender_index_sub "Unweighted Baseline Attitudes towards Female Gender Roles Sub-Index"
	lab var B_Sgender_index2_sub "Baseline Attitudes towards Female Gender Roles Sub-Index"
	
		****** 1.Ca Weighted Gender Attitudes (female gender roles) index (without imputing) ********
		
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		local i=0
			foreach x in `sub' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  // sum of weights of variables to keep
		
		** weighting the variables
		
		egen B_Sgender_index2_sub_ni=rsum2(sum_*), anymiss
		replace B_Sgender_index2_sub_ni = B_Sgender_index2_sub_ni*(C[1,1]/keep_sum)
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var B_Sgender_index2_sub_ni "Baseline Attitudes toward Female Gender Roles Sub-Index"		

	
		**************************************************************
					*		2. Aspirations Index           *
		**************************************************************
	
**** 2.1 Generating flags for missing values and imputing with gender-dist-treat means ****

	
	local aspiration B_Sdisc_edu_goals B_Shighest_educ_median B_Soccupa_25_white
	
	local aspiration_flag
	
		
	foreach y in `aspiration' {
			cap confirm variable `y'_flag
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y')
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local aspiration_flag `aspiration_flag' `y'_flag
				
			}	
		}
	global bl_aspiration_flag `aspiration_flag'
	
** Average Index
	cap drop check
	egen B_Saspiration_index=rowtotal(`aspiration') if B_Sgirl==1, mi
	egen check=rownonmiss(`aspiration') if B_Sgirl==1

	tab check if B_Sgirl==1
	qui sum check if B_Sgirl==1
	replace B_Saspiration_index=. if check<`r(max)' & B_Sgirl==1  
	replace B_Saspiration_index=B_Saspiration_index/`r(max)' if B_Sgirl==1
	drop check
	label var B_Saspiration_index "Student Aspiration index"

	
** Weighted Index
		
		local vars
		foreach var in `aspiration' {
				gen `var'_temp=`var'
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen B_Saspiration_index2=rsum2(sum_*) if B_Sgirl==1, anymiss
			
	cap drop sum_*			
			
	lab var B_Saspiration_index "Unweighted Baseline Aspirations Index"
	lab var B_Saspiration_index2 "Baseline Aspirations Index"
	
	
// Factor analysis for aspirations:

	local aspiration B_Sdisc_edu_goals B_Shighest_educ_median B_Soccupa_25_white
	
	local aspiration_flag
	factor `aspiration' if B_treat==0, factors(1) //follows authors, though weird to condition on treatment, no?
	predict B_Saspiration_index_factor if B_Sgirl==1  
	sum B_Saspiration_index_factor if B_Sgirl==1 & B_treat==0 & attrition_el==0 & !mi(E_Steam_id)
	replace B_Saspiration_index_factor = (B_Saspiration_index_factor - r(mean))/r(sd) if !missing(B_Saspiration_index_factor) & B_Sgirl==1 //normalise by control arm mean and SD
	

	****** 2.1a Weighted Aspirations index (without imputing) ********
		
		* limit to girls
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `aspiration' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==1 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==1 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==1 // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of weights of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if B_Sgirl==1 // sum of weights of variables to keep
		
		** weighting the variables		
		egen B_Saspiration_index2_ni=rsum2(sum_*) if B_Sgirl==1, anymiss
		replace B_Saspiration_index2_ni = B_Saspiration_index2_ni*(C[1,1]/keep_sum) if B_Sgirl==1
			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var B_Saspiration_index2_ni "Baseline Aspirations Index"		

	
	
	**** Aspirations (boys) ****

	
	local aspiration_boy B_Sdisc_edu_goals B_Shighest_educ_median B_Soccupa_25_white
	
	local aspiration_boy_flag
	
		
	foreach y in `aspiration_boy' {
			local aspiration_boy_flag `aspiration_boy_flag' `y'_flag
		
		}
	global bl_aspiration_boy_flag `aspiration_boy_flag'
	
** Average Index
	cap drop check
	egen B_Saspiration_boy_index=rowtotal(`aspiration_boy') if B_Sgirl==0, mi
	egen check=rownonmiss(`aspiration_boy') if B_Sgirl==0

	tab check if B_Sgirl==0
	qui sum check if B_Sgirl==0
	replace B_Saspiration_boy_index=. if check<`r(max)' & B_Sgirl==0   
	replace B_Saspiration_boy_index=B_Saspiration_boy_index/`r(max)' if B_Sgirl==0
	drop check
	label var B_Saspiration_boy_index "Student Aspiration index (boys)"

	
** Weighted Index
		
		local vars
		foreach var in `aspiration_boy' {
				gen `var'_temp=`var'
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen B_Saspiration_boy_index2=rsum2(sum_*) if B_Sgirl==0, anymiss
			
	cap drop sum_*			
			
	lab var B_Saspiration_boy_index "Unweighted Baseline Aspirations Index (boys)"
	lab var B_Saspiration_boy_index2 "Baseline Aspirations Index (boys)"
	
	****** 2.1a Weighted Aspirations index (boys) (without imputing) ********
		
		* limit to boys
		preserve
		keep if B_Sgirl==0
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `aspiration_boy' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==0  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==0 // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw) if B_Sgirl==0 // sum of weights of variables to keep
		
		** weighting the variables		
		egen B_Saspiration_boy_index2_ni=rsum2(sum_*) if B_Sgirl==0 , anymiss
		replace B_Saspiration_boy_index2_ni = B_Saspiration_boy_index2_ni*(C[1,1]/keep_sum) if B_Sgirl==0
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var B_Saspiration_boy_index2_ni "Baseline Aspirations Index (Boys)"		
	
	** common aspiration index
	
	local aspiration_all B_Sdisc_edu_goals B_Shighest_educ_median B_Soccupa_25_white
	
	local aspiration_all_flag
	
		
	foreach y in `aspiration_all' {
				local aspiration_all_flag `aspiration_all_flag' `y'_flag

		}
		
	global bl_aspiration_all_flag `aspiration_all_flag'

		
			local vars
		foreach var in `aspiration_all' {
				gen `var'_temp=`var'
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}


		preserve
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `aspiration_all' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]   // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp  // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw) // sum of weights of variables to keep
		
		** weighting the variables		
		egen B_Saspiration_all_index2=rsum2(sum_*) , anymiss
		replace B_Saspiration_all_index2 = B_Saspiration_all_index2*(C[1,1]/keep_sum) 
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var B_Saspiration_all_index2 "Baseline Aspirations Index (All)"		


		**************************************************************
					*		3. Behaviors Index           *
		**************************************************************

	
*************************** 3.1 Recoding Variables *****************************

	*a.
	gen B_Steased_n=inlist(B_Steased_y,0) if !mi(B_Steased_y) 
	lab var B_Steased_n "Student has never been teased by student of the opposite sex"

	*b. Household chores questions -- constructing differently for boys and girls
	*Boys
	gen B_Scook_clean_b=inlist(B_Shelp_cook,1) | inlist(B_Shelp_clean,1) | inlist(B_Shelp_clean_home,1) | inlist(B_Shelp_laundry,1) ///
		if (!mi(B_Shelp_cook) | !mi(B_Shelp_clean) | !mi(B_Shelp_clean_home) | !mi(B_Shelp_laundry)) & B_Sgirl==0
	lab var B_Scook_clean_b "Boys cook/clean/wash clothes"
	
	gen B_Stake_care_young_sib_b=inlist(B_Shelp_care_child,1) | inlist(B_Shelp_care_old,1) ///
		if (!mi(B_Shelp_care_child) | !mi(B_Shelp_care_old)) & B_Sgirl==0
	lab var B_Stake_care_young_sib_b "Boys take care of young sibling/old people"

	gen B_Shelp_get_groce_b=inlist(B_Shelp_get_groce,1)  ///
		if !mi(B_Shelp_care_child) & B_Sgirl==0
	lab var B_Shelp_get_groce_b "Boys help with shopping for hh provisions"

	*Girls
	gen B_Scook_clean_g=inlist(B_Shelp_cook,0) & inlist(B_Shelp_clean,0) & inlist(B_Shelp_clean_home,0) & inlist(B_Shelp_laundry,0) ///
		if (!mi(B_Shelp_cook) | !mi(B_Shelp_clean) | !mi(B_Shelp_clean_home) | !mi(B_Shelp_laundry)) & B_Sgirl==1
	lab var B_Scook_clean_g "Girls do not cook/clean/wash clothes"
	
	gen B_Stake_care_young_sib_g=inlist(B_Shelp_care_child,0) & inlist(B_Shelp_care_old,0) ///
		if (!mi(B_Shelp_care_child) | !mi(B_Shelp_care_old)) & B_Sgirl==1
	lab var B_Stake_care_young_sib_g "Girls do not take care of young sibling/old people"

	gen B_Shelp_get_groce_g=inlist(B_Shelp_get_groce,0)  ///
		if !mi(B_Shelp_get_groce) & B_Sgirl==1
	lab var B_Shelp_get_groce_g "Girls do not help with shopping for hh provisions"

	gen B_Sabsent_sch_g=inlist(B_Sabsent_sch,0) if !mi(B_Sabsent_sch) & B_Sgirl==1
	lab var B_Sabsent_sch_g "During last week girl was not absent from school"

	*Interaction with opposite sex
	gen B_Stalk_opp_gender_b = inlist(B_Stalk_opp_gender_y,1) if !mi(B_Stalk_opp_gender_y) & B_Sgirl==0
	gen B_Stalk_opp_gender_g = inlist(B_Stalk_opp_gender_y,1) if !mi(B_Stalk_opp_gender_y) & B_Sgirl==1
	
	*Mobility 
	gen B_Salone_friend_g = inlist(B_Salone_friend,1) if !mi(B_Salone_friend) & B_Sgirl==1
	

*******3.2 Generating flags for missing values and imputing with gender-dist-treat means *******




	local behavior_girl B_Stalk_opp_gender_g B_Salone_friend_g B_Sabsent_sch_g B_Scook_clean_g B_Shelp_get_groce_g B_Stake_care_young_sib_g 
	
	local behavior_boy B_Stalk_opp_gender_b B_Scook_clean_b B_Shelp_get_groce_b B_Stake_care_young_sib_b 
	
	local behavior_girl_flag
	foreach y in `behavior_girl' {
			cap confirm variable `y'_flag
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y') 
			if r(N)!=0{
				gen `y'_flag=mi(`y') if B_Sgirl==1
				bysort B_Sdistrict B_treat: egen x = mean (`y') if B_Sgirl==1
				qui replace `y'=x if `y'_flag==1
				drop x
				local behavior_girl_flag `behavior_girl_flag' `y'_flag
				
			}	
		}	
		
	global bl_behavior_girl_flag `behavior_girl_flag'
			
	local behavior_boy_flag
	foreach y in `behavior_boy' {
			cap confirm variable `y'_flag
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y') 
			if r(N)!=0{
				gen `y'_flag=mi(`y') if B_Sgirl==0
				bysort B_Sdistrict B_treat: egen x = mean (`y') if B_Sgirl==0
				qui replace `y'=x if `y'_flag==1  
				drop x
				local behavior_boy_flag `behavior_boy_flag' `y'_flag
				
			}	
		}		

	global bl_behavior_boy_flag `behavior_boy_flag'

	//Adding factor based index for boys and girls

		local behavior_girl B_Stalk_opp_gender_g B_Salone_friend_g B_Sabsent_sch_g B_Scook_clean_g B_Shelp_get_groce_g B_Stake_care_young_sib_g 
		factor 	`behavior_girl' if B_Sgirl==1 & B_treat==0 & attrition_el==0 & !mi(E_Steam_id)	, factors(1)
		predict B_Sbehavior_index__factor_g
		local behavior_boy B_Stalk_opp_gender_b B_Scook_clean_b B_Shelp_get_groce_b B_Stake_care_young_sib_b 
		factor 	`behavior_boy' if B_Sgirl==0	, factors(1)
		predict B_Sbehavior_index__factor_b

	
	
**Average Girls' Behaviors Index
	local behavior_girl B_Stalk_opp_gender_g B_Salone_friend_g B_Sabsent_sch_g B_Scook_clean_g B_Shelp_get_groce_g B_Stake_care_young_sib_g 
	
	local behavior_boy B_Stalk_opp_gender_b B_Scook_clean_b B_Shelp_get_groce_b B_Stake_care_young_sib_b 
	
	cap drop check
	egen B_Sbehavior_index_g=rowtotal(`behavior_girl') if B_Sgirl==1, mi
	egen check=rownonmiss(`behavior_girl') if B_Sgirl==1

	tab check if B_Sgirl==1
	qui sum check if B_Sgirl==1
	replace B_Sbehavior_index_g=. if check<`r(max)' & B_Sgirl==1   
	replace B_Sbehavior_index_g=B_Sbehavior_index_g/`r(max)' if B_Sgirl==1
	drop check
	label var B_Sbehavior_index_g "Girls' Behavior index"
	
	
	
**Weighted Girls' Behaviors Index
	
		local vars
		foreach var in `behavior_girl' {
				gen `var'_temp=`var'
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen B_Sbehavior_index2_g=rsum2(sum_*) if B_Sgirl==1, anymiss
			
	cap drop sum_*
			
	lab var B_Sbehavior_index_g "Girls' Unweighted Baseline Behavior Index"
	lab var B_Sbehavior_index2_g "Girls' Baseline Behavior Index"
	lab var B_Sbehavior_index__factor_b "Girls' Factor Baseline behavior index"
	****** 2.1a Weighted Girls behavior index (without imputing) ********
		
		* limit to girls
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `behavior_girl' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==1 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==1 // weight of non-missing variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==1 // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if B_Sgirl==1 // sum of weights of variables to keep
		
		** weighting the variables		
		egen B_Sbehavior_index2_g_ni=rsum2(sum_*) if B_Sgirl==1, anymiss
		replace B_Sbehavior_index2_g_ni = B_Sbehavior_index2_g_ni*(C[1,1]/keep_sum) if B_Sgirl==1
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var B_Sbehavior_index2_g_ni "Baseline Girls' Behavior Index"		


**Average Boys' Behaviors Index
	
	cap drop check
	egen B_Sbehavior_index_b=rowtotal(`behavior_boy') if B_Sgirl==0, mi
	egen check=rownonmiss(`behavior_boy') if B_Sgirl==0

	tab check if B_Sgirl==0 
	qui sum check if B_Sgirl==0 
	replace B_Sbehavior_index_b=. if check<`r(max)' & B_Sgirl==0 
	replace B_Sbehavior_index_b=B_Sbehavior_index_b/`r(max)' if B_Sgirl==0
	drop check
	label var B_Sbehavior_index_b "Boys' Behavior index"
	
	
	
**Weighted Boys' Behaviors Index
	
		local vars
		foreach var in `behavior_boy' {
				gen `var'_temp=`var'
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen B_Sbehavior_index2_b=rsum2(sum_*) if B_Sgirl==0, anymiss
			
	cap drop sum_*
		
	lab var B_Sbehavior_index_b "Boys' Unweighted Baseline Behavior Index"
	lab var B_Sbehavior_index2_b "Boys' Baseline Behavior Index"
	

		****** 2.1a Weighted Behavior index (boys) (without imputing) ********
		
		* limit to boys
		preserve
		keep if B_Sgirl==0
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `behavior_boy' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==0  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==0 // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw) if B_Sgirl==0 // sum of weights of variables to keep
		
		** weighting the variables		
		egen B_Sbehavior_index2_b_ni=rsum2(sum_*) if B_Sgirl==0 , anymiss
		replace B_Sbehavior_index2_b_ni = B_Sbehavior_index2_b_ni*(C[1,1]/keep_sum) if B_Sgirl==0
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var B_Sbehavior_index2_b_ni "Baseline Boys' Behavior Index"		


****COMMON BEHAVIOR INDEX FOR BOYS AND GIRLS

	**household chores (opposite direction for boys and girls)

	gen B_Scook_clean_comm=inlist(B_Shelp_cook,1) | inlist(B_Shelp_clean,1) | inlist(B_Shelp_clean_home,1) | inlist(B_Shelp_laundry,1) ///
		if (!mi(B_Shelp_cook) | !mi(B_Shelp_clean) | !mi(B_Shelp_clean_home) | !mi(B_Shelp_laundry)) & B_Sgirl==0
	replace B_Scook_clean_comm=inlist(B_Shelp_cook,0) & inlist(B_Shelp_clean,0) & inlist(B_Shelp_clean_home,0) & inlist(B_Shelp_laundry,0) ///
		if (!mi(B_Shelp_cook) | !mi(B_Shelp_clean) | !mi(B_Shelp_clean_home) | !mi(B_Shelp_laundry)) & B_Sgirl==1
	
	
	gen B_Stake_care_young_sib_comm=inlist(B_Shelp_care_child,1) | inlist(B_Shelp_care_old,1) ///
		if (!mi(B_Shelp_care_child) | !mi(B_Shelp_care_old)) & B_Sgirl==0
	replace B_Stake_care_young_sib_comm=inlist(B_Shelp_care_child,0) & inlist(B_Shelp_care_old,0) ///
		if (!mi(B_Shelp_care_child) | !mi(B_Shelp_care_old)) & B_Sgirl==1

	**interaction with opp sex (same direction for boys and girls)
	gen B_Stalk_opp_gender_comm = inlist(B_Stalk_opp_gender_y,1) if !mi(B_Stalk_opp_gender_y) 

	
	*gen B_Stake_care_young_sib_comm_flag = mi(B_Stake_care_young_sib_comm)
	
	local behavior_common B_Scook_clean_comm B_Stalk_opp_gender_comm B_Stake_care_young_sib_comm

	local behavior_common_flag
	foreach y in `behavior_common' {
			cap confirm variable `y'_flag
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y')
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local behavior_common_flag `behavior_common_flag' `y'_flag
				
			}	
	}
	
	global bl_behavior_common_flag `behavior_common_flag'
	
**average
	local behavior_common B_Scook_clean_comm B_Stalk_opp_gender_comm B_Stake_care_young_sib_comm

	local behavior_common_flag
	egen B_Sbehavior_index=rowtotal(`behavior_common'), mi
	egen check=rownonmiss(`behavior_common')
	tab check
	qui sum check
	replace B_Sbehavior_index=. if check<`r(max)'
	replace B_Sbehavior_index=B_Sbehavior_index/`r(max)'
	drop check
	label var B_Sbehavior_index "Self-reported behavior index"

	local behavior_common B_Scook_clean_comm B_Stalk_opp_gender_comm B_Stake_care_young_sib_comm
	factor `behavior_common' if B_treat==0  /* & attrition_el==0 & !mi(E_Steam_id) */, factors(1)
	predict B_Sbehavior_index_factor
	sum B_Sbehavior_index_factor  if B_treat==0 /* & attrition_el==0 & !mi(E_Steam_id)  */
	replace B_Sbehavior_index_factor   = (B_Sbehavior_index_factor  -r(mean))/r(sd)

	
**weighted

	local vars
	foreach var in `behavior_common' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			}
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

				
	local i=0
		foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
		}
		
	egen B_Sbehavior_index2=rsum2(sum_*), anymiss
		
	*cap drop *_temp
	cap drop sum_*		
		
	lab var B_Sbehavior_index "Unweighted Baseline Behavior Index"
	lab var B_Sbehavior_index2 "Baseline Behavior Index"
	
	
	****** 1.Ca Weighted behavior index (without imputing) ********
		
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		local i=0
			foreach x in `behavior_common' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-missing variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  // sum of weights of variables to keep
		
		** weighting the variables
		
		egen B_Sbehavior_index2_ni=rsum2(sum_*), anymiss
		replace B_Sbehavior_index2_ni = B_Sbehavior_index2_ni*(C[1,1]/keep_sum)
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var B_Sbehavior_index2_ni "Baseline Behavior Index"		


** Behavior sub-indices

**3.A Interaction with the opposite sex (boys and girls)

	gen B_Sbehavior_index_oppsex = B_Stalk_opp_gender_comm
	gen B_Sbehavior_index2_oppsex = B_Stalk_opp_gender_comm
	gen B_Sbehavior_index2_oppsex_ni = B_Stalk_opp_gender_comm
	
	lab var B_Sbehavior_index_oppsex "Unweighted Baseline Interaction with the Opposite Sex Sub-Index"
	lab var B_Sbehavior_index2_oppsex "Baseline Interaction with the Opposite Sex Sub-Index"
	lab var B_Sbehavior_index2_oppsex_ni "Baseline Interaction with the Opposite Sex Sub-Index"

	

**3.B Participation in the household chores (boys and girls)



	gen B_Sbehavior_index_hhchores = B_Scook_clean_comm 
	gen B_Sbehavior_index2_hhchores = B_Scook_clean_comm 
	gen B_Sbehavior_index2_hhchores_ni = B_Scook_clean_comm 

	
	lab var B_Sbehavior_index_hhchores "Unweighted Baseline Participation in HH Chores Sub-Index"
	lab var B_Sbehavior_index2_hhchores "Baseline Participation in HH Chores Sex Sub-Index"
	lab var B_Sbehavior_index2_hhchores_ni "Baseline Participation in HH Chores Sex Sub-Index"



**3.C Supoorting female relatives' ambitions (boys and girls)

	/**Does not exist**/


**3.D Decision_making (only for girls)

	gen B_Sbehavior_index_decision_g = B_Sabsent_sch_g
	gen B_Sbehavior_index2_dec_g = B_Sabsent_sch_g 
	gen B_Sbehavior_index2_dec_g_ni= B_Sabsent_sch_g 

	
	lab var B_Sbehavior_index_decision_g "Girls' Unweighted Baseline Decision-making Sub-Index"
	lab var B_Sbehavior_index2_dec_g "Girls' Baseline Decision-making Sub-Index"
	lab var B_Sbehavior_index2_dec_g_ni "Girls' Baseline Decision-making Sub-Index"
	

**3.E Mobility (only for girls)
	

	gen B_Sbehavior_index_mobility_g = B_Salone_friend_g
	gen B_Sbehavior_index2_mobil_g = B_Salone_friend_g
	gen B_Sbehavior_index2_mobil_g_ni= B_Salone_friend_g
	
	lab var B_Sbehavior_index_mobility_g "Girls' Unweighted Baseline Mobility Sub-Index"
	lab var B_Sbehavior_index2_mobil_g "Girls' Baseline Mobility Sub-Index"
	lab var B_Sbehavior_index2_mobil_g_ni "Girls' Baseline Mobility Sub-Index"

********************************************************************************
	   *		BASELINE SECONDARY OUTCOMES AND CONTROLS: INDICES            *
********************************************************************************


***4. Parent gender index


** Average Parent gender index

/*NOTE: Only 40% of the parents were surveyed at baseline. That means 60% of the data points will be imputed so that we do not lose out on sample size.
		For this reason, individual questions have not been imputed for the gender-treatment-district status, and the entire variable is imputed for the sample 
		average in the end.
*/

	rename B_Pparent_* B_P_*
	
	local parent B_P_woman_role_n B_P_man_final_deci_n B_P_woman_tol_viol_n B_P_wives_less_edu_n ///
		B_P_boy_more_opps_n B_P_equal_opps_y B_P_girl_allow_study_y B_P_similar_right_y B_P_elect_woman_y
	
	su  B_P_woman_role_n B_P_man_final_deci_n B_P_woman_tol_viol_n B_P_wives_less_edu_n ///
		B_P_boy_more_opps_n B_P_equal_opps_y B_P_girl_allow_study_y B_P_similar_right_y B_P_elect_woman_y
	
	egen B_Pgender_index=rowtotal(`parent'), mi
	egen check=rownonmiss(`parent')
	tab check
	qui sum check if B_parent_merge==3
	replace B_Pgender_index=. if check<`r(max)'
	replace B_Pgender_index=B_Pgender_index/`r(max)'
	drop check
	label var B_Pgender_index "Parent gender index"

	
* flags for missing variables
local parent_flag 

foreach var in `parent' {
	gen `var'_flag = mi(`var')
	local parent_flag `parent_flag' `var'_flag
	}

**Weighted Parent gender index

	local vars
	foreach var in `parent' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			}
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		local i=0
			foreach x in `parent' {
			local ++i				
			gen `x'_nmi = `x'_flag==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			}			
			
	* overall sum of weights
	mata : st_matrix("C", colsum(st_matrix("B")))		
			
	* sum of wieghts of non-missing variables 
	egen double keep_sum = rowtotal(*_kw) // sum of weights of variables to keep

	** weighting the variables		
	egen B_Pgender_index2=rsum2(sum_*), anymiss
	replace B_Pgender_index2 = B_Pgender_index2*(C[1,1]/keep_sum)
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 	
		
	lab var B_Pgender_index "Unweighted Baseline Parent Gender Attitudes Index"
	lab var B_Pgender_index2 "Baseline Parent Gender Attitudes Index"

	***imputing values by gender-district-treatment
	
	gen B_Pgender_index2_impute = B_Pgender_index2

	gen B_Pgender_index2_flag=mi(B_Pgender_index2)
	
	bysort B_Sgirl B_Sdistrict: egen x = mean(B_Pgender_index2) 
	qui replace B_Pgender_index2_impute=x if B_Pgender_index2_flag==1
	drop x
	lab var B_Pgender_index2_impute "Baseline Parent Gender Attitudes Index (Imputed)"

***5. Self-esteem index

	local varlist B_Ssatisfy B_Sgood_qlty B_Sable_do_most
	
	foreach x in `varlist' {
		local varlab: variable label `x'
		gen `x'_y=inlist(`x',1,2) if !mi(`x')
		label var `x'_y "Agree: `varlab'"
		label values `x'_y yesno
		order `x'_y, after(`x')
		}
		
		
**Average self-esteem index
	
	local esteem B_Ssatisfy_y B_Sgood_qlty_y B_Sable_do_most_y
	
	local esteem_flag
		
	foreach y in `esteem' {
			cap confirm variable `y'_flag
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y')
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local esteem_flag `esteem_flag' `y'_flag
				
			}	
	}
	
	global bl_esteem_flag `esteem_flag'

	local esteem B_Ssatisfy_y B_Sgood_qlty_y B_Sable_do_most_y

	cap drop check
	egen B_Sesteem_index=rowtotal(`esteem'), mi
	egen check=rownonmiss(`esteem')

	tab check
	qui sum check
	replace B_Sesteem_index=. if check<`r(max)'   
	replace B_Sesteem_index=B_Sesteem_index/`r(max)'
	drop check
	label var B_Sesteem_index "Self-esteem index"
	
	
**Weighted self-esteem index
	
	local vars
	foreach var in `esteem' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen B_Sesteem_index2=rsum2(sum_*), anymiss
			
	cap drop *_temp
	cap drop sum_*			
			
	lab var B_Sesteem_index "Unweighted Baseline Self-esteem Index"
	lab var B_Sesteem_index2 "Baseline Self-esteem Index"



***Boys' self-esteem index

	local esteem B_Ssatisfy_y B_Sgood_qlty_y B_Sable_do_most_y

	cap drop check
	egen B_Sesteem_index_boy=rowtotal(`esteem') if B_Sgirl==0, mi 
	egen check=rownonmiss(`esteem') if B_Sgirl==0

	tab check
	qui sum check
	replace B_Sesteem_index_boy=. if check<`r(max)' & B_Sgirl==0 
	replace B_Sesteem_index_boy=B_Sesteem_index_boy/`r(max)' if B_Sgirl==0
	drop check
	label var B_Sesteem_index_boy "Boys' Self-esteem Index"
	
	
**Weighted boys' self-esteem index
	
	local vars
	foreach var in `esteem' {
			gen `var'_temp=`var' if B_Sgirl==0
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}

	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==0
			}
			
	egen B_Sesteem_index2_boy=rsum2(sum_*), anymiss
			
	cap drop *_temp
	cap drop sum_*
			
	lab var B_Sesteem_index_boy "Unweighted Boys' Baseline Self-esteem Index"
	lab var B_Sesteem_index2_boy "Boys' Baseline Self-esteem Index"


***Girls' self-esteem index

	local esteem B_Ssatisfy_y B_Sgood_qlty_y B_Sable_do_most_y

	cap drop check
	egen B_Sesteem_index_girl=rowtotal(`esteem') if B_Sgirl==1, mi 
	egen check=rownonmiss(`esteem') if B_Sgirl==1

	tab check
	qui sum check
	replace B_Sesteem_index_girl=. if check<`r(max)' & B_Sgirl==1
	replace B_Sesteem_index_girl=B_Sesteem_index_girl/`r(max)' if B_Sgirl==1
	drop check
	label var B_Sesteem_index_girl "Girls' Self-esteem Index"
	
	
**Weighted girls' self-esteem index
	
	local vars
	foreach var in `esteem' {
			gen `var'_temp=`var' if B_Sgirl==1
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}

	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==1
			}
			
	egen B_Sesteem_index2_girl=rsum2(sum_*), anymiss
			
	cap drop sum_*			
			
	lab var B_Sesteem_index_girl "Unweighted Girls' Baseline Self-esteem Index"
	lab var B_Sesteem_index2_girl "Girls' Baseline Self-esteem Index"
	
** weighted - not imputed
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		local i=0
			foreach x in `esteem' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  // sum of weights of variables to keep
		
		** weighting the variables
		
		egen B_Sesteem_index2_girl_ni=rsum2(sum_*), anymiss
		replace B_Sesteem_index2_girl_ni = B_Sesteem_index2_girl_ni*(C[1,1]/keep_sum)
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var B_Sesteem_index2_girl_ni "Girls' Baseline Self-esteem Index"		

****** PARENT DRMOGRAPHICS *******
	gen B_in_sample=1 if B_Pgender_index2_flag==0
	replace B_in_sample=0 if B_Pgender_index2_flag==1
	label var B_in_sample "In intergenerational transmission sample"


	cap drop B_num_father
	gen B_num_father=0
	gen B_f_emp_status=.
	gen B_f_edu_level=.
	gen B_f_marital=.
	gen B_f_age=.


	foreach x of numlist 1/11{
		replace B_num_father=B_num_father+1 if B_Sparent_relation`x'==3 
		replace B_f_emp_status=B_Sparent_emp_status`x' if B_Sparent_relation`x'==3
		replace B_f_edu_level=B_Sparent_edu_level`x' if B_Sparent_relation`x'==3
		replace B_f_marital=B_Sparent_status`x' if B_Sparent_relation`x'==3
		replace B_f_age=B_Sparent_age`x' if B_Sparent_relation`x'==3
	}
	tab B_num_father  

	cap drop B_num_mother
	gen B_num_mother=0
	gen B_m_emp_status=.
	gen B_m_edu_level=.
	gen B_m_marital=.
	gen B_m_age=.
	foreach x of numlist 1/11{
		replace B_num_mother=B_num_mother+1 if B_Sparent_relation`x'==4 
		replace B_m_emp_status=B_Sparent_emp_status`x' if B_Sparent_relation`x'==4 
		replace B_m_edu_level=B_Sparent_edu_level`x' if B_Sparent_relation`x'==4 
		replace B_m_marital=B_Sparent_status`x' if B_Sparent_relation`x'==4 
		replace B_m_age=B_Sparent_age`x' if B_Sparent_relation`x'==4 
	}
	tab B_num_mother 

	label var B_f_emp_status "Father's employment status"
	label var B_f_edu_level "Father's education level"
	label var B_f_marital "Father's marital status"
	label var B_m_emp_status "Mother's employment status"
	label var B_m_edu_level "Mother's education level"
	label var B_m_marital "Mother's marital status"


	label copy bl_Sparent_emp_status1 B_f_emp_status
	label copy bl_Sparent_emp_status1 B_m_emp_status
	label copy bl_Sparent_edu_level1 B_f_edu_level
	label copy bl_Sparent_edu_level1 B_m_edu_level
	label copy bl_Sparent_status1 B_f_marital
	label copy bl_Sparent_status1 B_m_marital
	foreach x in B_f_emp_status B_m_emp_status B_f_edu_level B_f_marital B_m_edu_level B_m_marital{
		label values `x' `x'
	}
	 
	count if (B_Sfather_stay==1 & B_num_father==0) | (B_Smother_stay==1 & B_num_mother==0)


 
	forval x=1/11{ 
		gen B_f_edu_level_`x'=B_f_edu_level==`x' if !mi(B_f_edu_level) & B_f_edu_level!=12
		gen B_m_edu_level_`x'=B_m_edu_level==`x' if !mi(B_m_edu_level) & B_m_edu_level!=12	
	}


	forval x=1/5{
		gen B_f_emp_status_`x'=B_f_emp_status==`x' if !mi(B_f_emp_status)
		gen B_m_emp_status_`x'=B_m_emp_status==`x' if !mi(B_m_emp_status)
	}	

	forval x=1/4{
		gen f_birth_place_`x'=B_Sfather_birth_place==`x' if !mi(B_Sfather_birth_place)
		gen m_birth_place_`x'=B_Smother_birth_place==`x' if !mi(B_Smother_birth_place)
	}	

	gen B_f_nostay=inlist(B_Sfather_stay,2) if !mi(B_Sfather_stay) 
	gen B_m_nostay=inlist(B_Smother_stay,2) if !mi(B_Smother_stay)
	gen B_f_deceased=inlist(B_Sfather_stay,3) if !mi(B_Sfather_stay)
	gen B_m_deceased=inlist(B_Smother_stay,3) if !mi(B_Smother_stay)
	gen B_f_major_dec=B_Smajor_deci==1 if !mi(B_Smajor_deci)
	gen B_m_major_dec=B_Smajor_deci==2 if !mi(B_Smajor_deci)

	gen B_f_illiterate=inlist(B_f_edu_level,1) if !mi(B_f_edu_level) & B_f_edu_level<11
	gen B_m_illiterate=inlist(B_m_edu_level,1) if !mi(B_m_edu_level) & B_m_edu_level<11
	gen B_f_primary=inlist(B_f_edu_level,2,3) if !mi(B_f_edu_level) & B_f_edu_level<11
	gen B_m_primary=inlist(B_m_edu_level,2,3) if !mi(B_m_edu_level) & B_m_edu_level<11
	gen B_f_secondary=inlist(B_f_edu_level,4) if !mi(B_f_edu_level) & B_f_edu_level<11
	gen B_m_secondary=inlist(B_m_edu_level,4) if !mi(B_m_edu_level) & B_m_edu_level<11
	gen B_f_highedu=inlist(B_f_edu_level,5,6,7,8,9,10) if !mi(B_f_edu_level) & B_f_edu_level<11
	gen B_m_highedu=inlist(B_m_edu_level,5,6,7,8,9,10) if !mi(B_m_edu_level) & B_m_edu_level<11
	gen B_f_fulltime=inlist(B_f_emp_status,1) if !mi(B_f_emp_status)
	gen B_m_fulltime=inlist(B_m_emp_status,1) if !mi(B_m_emp_status)
	gen B_f_parttime=inlist(B_f_emp_status,5) if !mi(B_f_emp_status)
	gen B_m_parttime=inlist(B_m_emp_status,5) if !mi(B_m_emp_status)
	gen B_f_not_working=inlist(B_f_emp_status,2,3,4,6,7) if !mi(B_f_emp_status)
	gen B_m_not_working=inlist(B_m_emp_status,2,3,4,6,7) if !mi(B_m_emp_status)



	label var B_m_parttime "Mother works part-time"
	label var B_m_fulltime "Mother works full-time"
	label var B_m_not_working "Mother is not working"
	label var B_m_illiterate "Mother is illiterate"
	label var B_m_primary "Mother is literate or finished primary school"  // unemployment, retired, not capable of working, student
	label var B_m_secondary "Mother finished middle school (Class 8)"
	label var B_m_highedu "Mother finished Class 10 or higher"


	label var B_f_parttime "Father works part-time"
	label var B_f_fulltime "Father works full-time"
	label var B_f_not_working "Father is not working"
	label var B_f_illiterate "Father is illiterate"
	label var B_f_primary "Father is literate or finished primary school"
	label var B_f_secondary "Father finished middle school (Class 8)"
	label var B_f_highedu "Father finished Class 10 or higher"

**extra control variables
	gen B_Sparent_stay=1 if inlist(B_Sfather_stay,1) & inlist(B_Smother_stay,1)
	replace B_Sparent_stay=0 if B_Sparent_stay==. & !mi(B_Sfather_stay) & !mi(B_Smother_stay)
	la var B_Sparent_stay "Lives with father and/or mother"
	
** Own TV

	gen B_own_tv=B_Phh_durables_2
	label var B_own_tv "Own TV"
	
	
** Socio-economic status variables


	*Counting number of female siblings*
	gen B_no_female_sib = 0
	local vars "B_Ssib_gender1 B_Ssib_gender2 B_Ssib_gender3 B_Ssib_gender4 B_Ssib_gender5 B_Ssib_gender6 B_Ssib_gender7 B_Ssib_gender8 B_Ssib_gender9 B_Ssib_gender10 B_Ssib_gender11"
	foreach a of local vars {
		replace B_no_female_sib = B_no_female_sib +1 if `a'==2
		}

	*Counting number of male siblings*
	gen B_no_male_sib = 0
	local vars "B_Ssib_gender1 B_Ssib_gender2 B_Ssib_gender3 B_Ssib_gender4 B_Ssib_gender5 B_Ssib_gender6 B_Ssib_gender7 B_Ssib_gender8 B_Ssib_gender9 B_Ssib_gender10 B_Ssib_gender11"
	foreach a of local vars {
		replace B_no_male_sib = B_no_male_sib +1 if `a'==1
		}
		
	**Cleaning SHHsize
	replace B_Shh_members=. if B_Shh_members>19

	replace B_Scellphone_house=. if B_Scellphone_house==2

** Actual number of kids (max 6)

	gen B_numkids=B_Ssibsize+1    // add 1 for the reference child to numkids (number of siblings)
	label var B_numkids "Number of children"



	gen B_numboys=0
	gen B_numgirls=0
	foreach x of numlist 1/11{
		replace B_numboys=B_numboys+1 if inlist(B_Ssib_relation`x',6) 
		replace B_numboys=B_numboys+1 if inlist(B_Sparent_relation`x',6)   // some siblings were listed in the parent roster (confirmed these shouldn't be parents misentered as siblings)
		replace B_numgirls=B_numgirls+1 if inlist(B_Ssib_relation`x',5) 
		replace B_numgirls=B_numgirls+1 if inlist(B_Sparent_relation`x',5)   
	}

	gen B_numkids2=B_numboys+B_numgirls   // excluding the sample child
	gen B_actual_pct_sons2=(B_numboys)/B_numkids2 
	label var B_actual_pct_sons2 "Percent sons among children"
	drop B_numkids2
	rename B_actual_pct_sons2 B_Sactual_pct_sons2

	replace B_numboys=B_numboys+1 if B_Sgirl==0
	replace B_numgirls=B_numgirls+1 if B_Sgirl==1
	assert B_numkids==B_numboys+B_numgirls if Sschool_id!=2711

	gen B_actual_pct_sons=B_numboys/B_numkids
	label var B_actual_pct_sons "Percent sons among children (incl. sample child)"

	rename B_numkids B_Snumkids

** Respondent is eldest 
	gen B_eldest=(B_Ssibsize==B_Ssibsize_young) if !mi(B_Ssibsize) & !mi(B_Ssibsize_young)
		label var B_eldest "Eldest or only child"



** Additional parent education vars


	label var B_Pprimary "Finished primary"
	label var B_Psecondary "Finished secondary"
	gen B_Pgrade10=inlist(B_Pstudied,6) if B_Pstudied<=10
	label var B_Pgrade10 "Finished Class 10"
	gen B_Pgrade10up=inlist(B_Pstudied,6,7,8,10) if B_Pstudied<=10
	label var B_Pgrade10up "Finished Class 10+"
	gen B_Pgrade12up=inlist(B_Pstudied,7,8,10) if B_Pstudied<=10
	label var B_Pgrade12up "Finished Class 12+"


** sex ratio

	egen B_sch_class=group(Sschool_id B_Sclass)

	gen temp_b=1 if B_Sboy==1
	gen temp_g=1 if B_Sgirl==1

	bys B_sch_class: egen B_num_boys=count(temp_b)
	bys B_sch_class: egen B_num_girls=count(temp_g)
	gen B_sex_ratio=B_num_boys/B_num_girls

	
	
** parent ages

	replace B_P_age=B_Pspouse_age if B_P_age==.d & B_Page_gap==0
	gen B_mom_age=B_P_age if B_Pgender==2
		replace B_mom_age=B_Pspouse_age if B_Pgender==1
		gen B_mom_age_flag=mi(B_mom_age)
	gen B_dad_age=B_P_age if B_Pgender==1
		replace B_dad_age=B_Pspouse_age if B_Pgender==2
		gen B_dad_age_flag=mi(B_dad_age)
	
	// impute missing
	gen gap=B_dad_age-B_mom_age
	sum gap, d
	replace B_mom_age=B_dad_age-`r(p50)' if mi(B_mom_age) & !mi(B_dad_age)
	replace B_dad_age=B_mom_age+`r(p50)' if mi(B_dad_age) & !mi(B_mom_age)
	drop gap

	sum B_mom_age
	replace B_mom_age=floor(r(mean)) if mi(B_mom_age)
	sum B_dad_age
	replace B_dad_age=floor(r(mean)) if mi(B_dad_age)
	
	
** Labels

	label var B_Sown_house "Family owns the house"
	label var B_Shouse_elec "House is connected to electricity"
	label var B_Shouse_kitc "House has a separate kitchen"
	label var B_Stwo_meals_day "In the last 7 days, had 2 meals each day"
	label var B_Scellphone_house "Household owns: cell phone"
	label var B_Stv_house "Household owns: TV"
	label var B_Sradio_house "Household owns: radio"
	label var B_Snewspaper_house "Household gets newspapers daily"
	label var B_Smagazine_house "Household gets magazines"
	label var B_Scomputer_house "Household owns: computers or laptop"

	
** D measure

// higher D-measure indicate implicit preference for boys

	gen B_D_measure_neg=B_D_measure*(-1)
	label var B_D_measure "Implicit preference for boys"
	label var B_D_measure_neg "Implicit preference for girls"
	rename B_iat_boy_good_first B_iat_boygood
	label var B_iat_boygood "Boy/good first"

**Social desirability score

	local social B_Shard_ifnot_enco B_Sfeel_resent B_Slittle_ability B_Srebel_against ///
		B_Socca_advan B_Ssome_forgive B_Sjealous B_Sirritated B_Sdeliberately_hurt
	
	foreach var in `social' {
		local varlab: variable label `var'
		gen `var'_n=1 if `var'==2 //EDIT: was originally `var'==0
		replace `var'_n=0 if `var'==1
		lab var `var'_n "Disagree: `varlab'"
	}
	
	local social B_Sgood_listen B_Sadmit_mistake B_Scourteous B_Sirked
	foreach var in `social' {
		replace `var'=0 if `var'==2
	}
	
	local social B_Shard_ifnot_enco_n B_Sfeel_resent_n B_Slittle_ability_n B_Srebel_against_n ///
		B_Sgood_listen B_Socca_advan_n B_Sadmit_mistake B_Ssome_forgive_n ///
		B_Scourteous B_Sirked B_Sjealous_n B_Sirritated_n B_Sdeliberately_hurt_n
	

	local social_flag
		
	foreach y in `social' {
			cap confirm variable `y'_flag
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y')
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local social_flag `social_flag' `y'_flag
				
			}	
	}
	
	global bl_social_flag `social_flag'

	cap drop check
	egen B_Ssocial_scale=rowtotal(`social'), mi
	egen check=rownonmiss(`social')

	tab check
	qui sum check
	replace B_Ssocial_scale=. if check<`r(max)'   
	drop check
	label var B_Ssocial_scale "Baseline social desirability score"
	gen B_Ssocial_scale_int_imp = B_Ssocial_scale
	la var B_Ssocial_scale_int_imp "Baseline social desirability score (0-13, imputed)"
	

	* SDS on 0-13 scale, not imputed
	local social B_Shard_ifnot_enco_n B_Sfeel_resent_n B_Slittle_ability_n B_Srebel_against_n ///
		B_Sgood_listen B_Socca_advan_n B_Sadmit_mistake B_Ssome_forgive_n ///
		B_Scourteous B_Sirked B_Sjealous_n B_Sirritated_n B_Sdeliberately_hurt_n
	
	local social_bin // social desirability values that are binary (not imputed for missing)
	foreach y in `social' {
		local l: var lab `y'
		gen `y'_bin=`y'
		replace  `y'_bin=. if `y'_flag==1
			la var `y'_bin "`l'"
		local social_bin `social_bin' `y'_bin
		}


	egen B_Ssocial_scale_int=rowtotal(`social_bin'), mi // if al 13 missing, set to missing
	egen check=rownonmiss(`social_bin')

	tab check
	qui sum check
	replace B_Ssocial_scale_int=. if check<`r(max)'   // if even 1 is missing, set to missing
	drop check
	
	summ B_Ssocial_scale_int, detail
	gen B_Ssocial_scale_intbelmed=1 if B_Ssocial_scale_int<`r(p50)' & !mi(B_Ssocial_scale_int)
	replace B_Ssocial_scale_intbelmed=0 if B_Ssocial_scale_int>=`r(p50)' & !mi(B_Ssocial_scale_int)
		la var B_Ssocial_scale_intbelmed "Low BL social desirability"
		

la var B_Ssocial_scale_int "Baseline Social Desirability Score (0-13)"
	
**Self-efficacy index

	local varlist B_Senjoy_learn B_Smake_comm B_Sfull_idea B_Ssocial_prob B_Sparent_help B_Sdefini_opinion
	
	foreach x in `varlist' {
		local varlab: variable label `x'
		gen `x'_y=inlist(`x',1,2) if !mi(`x')
		label var `x'_y "Agree: `varlab'"
		label values `x'_y yesno
		order `x'_y, after(`x')
		
	}
	
		
	**imputing values
	
	local efficacy B_Ssatisfy_y B_Senjoy_learn_y B_Sgood_qlty_y B_Sable_do_most_y B_Smake_comm_y B_Sfull_idea_y B_Ssocial_prob_y B_Sparent_help_y B_Sdefini_opinion_y
	local efficacy_flag
		
	foreach y in `efficacy' {
			cap confirm variable `y'_flag
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y')
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local efficacy_flag `efficacy_flag' `y'_flag
				
			}	
	}

	cap drop check
	egen B_Sefficacy_index=rowtotal(`efficacy'), mi
	egen check=rownonmiss(`efficacy')

	tab check
	qui sum check
	replace B_Sefficacy_index=. if check<`r(max)'   
	replace B_Sefficacy_index=B_Sefficacy_index/`r(max)'
	drop check
	label var B_Sefficacy_index "Self-efficacy index"
	
	
**Weighted self-efficacy index
	
	local vars
	foreach var in `efficacy' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		local i=0
			foreach x in `efficacy' {
			local ++i				
			gen `x'_nmi = `x'_flag==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			}
						
	* overall sum of weights
	mata : st_matrix("C", colsum(st_matrix("B")))		
			
	* sum of wieghts of non-missing variables 
	egen double keep_sum = rowtotal(*_kw) // sum of weights of variables to keep

	** weighting the variables		
	egen B_Sefficacy_index2=rsum2(sum_*), anymiss
	replace B_Sefficacy_index2 = B_Sefficacy_index2*(C[1,1]/keep_sum)
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
	
	* imputing missing
	bysort B_Sgirl B_Sdistrict B_treat: egen x = mean(B_Sefficacy_index2) 
	qui replace B_Sefficacy_index2=x if mi(B_Sefficacy_index2)
	drop x
						
	lab var B_Sefficacy_index "Unweighted Baseline Self-efficacy Index"
	lab var B_Sefficacy_index2 "Baseline Self-efficacy Index"

********************************************************************************
				*		 ENDLINE PRIMARY OUTCOMES: INDICES        *
********************************************************************************
	

	
	cap drop E_Sfertility
	gen E_Sfertility = 0 if E_Stwo_boys==1 & inlist(E_Stwo_girls, 2, 3) // stop after having boys, but not after having girls
	replace E_Sfertility = 1 if !mi(E_Stwo_boys) & !mi(E_Stwo_girls) & E_Sfertility!=0
	replace E_Sfertility = .d if inlist(E_Stwo_girls, .d) | inlist(E_Stwo_boys, .d)
	replace E_Sfertility = .r if inlist(E_Stwo_girls, .r) | inlist(E_Stwo_boys, .r)
	replace E_Sfertility = .s if inlist(E_Stwo_girls, .s) | inlist(E_Stwo_boys, .s)

	lab var E_Sfertility "Student agrees to more children after two girls, but not after two boys"
	
	
	*Making new variables

	*Work outside of home
	gen E_Sallow_work_y = E_Sallow_work	
	local l: variable label E_Sallow_work
	la var E_Sallow_work_y "`l'"

	*Demure bride/groom and laugh
	foreach var in E_Sshy_boy_y E_Sshy_girl_y E_Sboy_laugh_y E_Sgirl_laugh_y {
	bys Sschool_id B_Sgirl: egen `var'_total1=total(`var')

	bys Sschool_id: gen `var'_temp = 1 if !mi(`var')
	bys Sschool_id B_Sgirl: egen `var'_total2=total(`var'_temp)
	gen `var'_perc=(`var'_total1/`var'_total2)*100

	drop `var'_temp `var'_total1 `var'_total2
	}

	gen E_Sshy_boy_girl = E_Sshy_boy_y_perc / E_Sshy_girl_y_perc
	gen E_Slaugh_boy_girl = E_Sboy_laugh_y_perc / E_Sgirl_laugh_y_perc

	lab var E_Sshy_boy_girl "\% of people who said boy should be demure/\% of people who said girl should be demure(School-gender level)"
	lab var E_Slaugh_boy_girl "\% of people who said boy should be demure/\% of people who said girl should be demure(School-gender level)"

	
		* shy demeanor for suitable bride/groom AND girl/boy laughing
	
	foreach var in E_Sshy_boy_y E_Sshy_girl_y {
		local l: variable label `var'
		egen `var'_mean = mean(`var') if !mi(E_Sshy_boy_y) | !mi(E_Sshy_girl_y), by(Sschool_id B_Sgirl)
		la var `var'_mean "`l' (school-gender mean)"
		}
		
	foreach var in E_Sboy_laugh_y E_Sgirl_laugh_y {
		local l: variable label `var'
		egen `var'_mean = mean(`var') if !mi(E_Sboy_laugh_y) | !mi(E_Sgirl_laugh_y), by(Sschool_id B_Sgirl)
		la var `var'_mean "`l' (school-gender mean)"
		}
		
	gen E_Sshy = 1 if E_Sshy_girl_y_mean<=E_Sshy_boy_y_mean & (!mi(E_Sshy_girl_y_mean) & !mi(E_Sshy_boy_y_mean))
	replace E_Sshy = 0 if E_Sshy_girl_y_mean>E_Sshy_boy_y_mean & (!mi(E_Sshy_girl_y_mean) & !mi(E_Sshy_boy_y_mean))
	replace E_Sshy = .d if E_Sshy_boy==.d | E_Sshy_girl==.d
	replace E_Sshy = .r if E_Sshy_boy==.r | E_Sshy_girl==.r

	gen E_Slaugh = 1 if E_Sgirl_laugh_y_mean<=E_Sboy_laugh_y_mean & (!mi(E_Sgirl_laugh_y_mean) & !mi(E_Sboy_laugh_y_mean))
	replace E_Slaugh = 0 if E_Sgirl_laugh_y_mean>E_Sboy_laugh_y_mean & (!mi(E_Sgirl_laugh_y_mean) & !mi(E_Sboy_laugh_y_mean))
	replace E_Slaugh = .d if E_Sboy_laugh==.d | E_Sgirl_laugh==.d
	replace E_Slaugh = .r if E_Sboy_laugh==.r | E_Sgirl_laugh==.r
	
	la var E_Sshy "Shy girl suitable <= Shy boy suitable"
	la var E_Slaugh "Laughing: girl cover mouth <= boy cover mouth"
	order E_Sshy_boy_y_mean E_Sshy_girl_y_mean E_Sshy, after(E_Sshy_girl_n)
	order E_Sboy_laugh_y_mean E_Sgirl_laugh_y_mean E_Slaugh, after(E_Sboy_laugh_n)
	la val E_Sshy E_Slaugh yesno
	
	gen E_Sshy_flag = mi(E_Sshy) if attrition_el==0
	gen E_Slaugh_flag = mi(E_Slaugh) if attrition_el==0
	
	* girl marriage after 19
	replace E_Sgirl_marriage_age_19 = .d if E_Sgirl_marriage_age==.d | E_Sgirl_marriage_age_numb==.d
	replace E_Sgirl_marriage_age_19 = .r if E_Sgirl_marriage_age==.r | E_Sgirl_marriage_age_numb==.r
	replace E_Sgirl_marriage_age_19 = .i if inlist(E_Sgirl_marriage_age, 2, 3, 4, 5) // answers like after educ, work, parents etc. 
	
	replace E_Smarriage_age_diff_m = .d if inlist(E_Sboy_marriage_age, .d) | inlist(E_Sgirl_marriage_age, .d)
	replace E_Smarriage_age_diff_m = .r if inlist(E_Sboy_marriage_age, .r) | inlist(E_Sgirl_marriage_age, .r )
	replace E_Smarriage_age_diff_m = .i if inlist(E_Sboy_marriage_age,2, 3, 4, 5)  | inlist(E_Sgirl_marriage_age, 2, 3, 4, 5) // non-numeric value for either
	
	replace E_Sstudy_marry = .d if inlist(E_Sgirl_study_marry, .d) |  inlist(E_Sboy_study_marry, .d)	
	replace E_Sstudy_marry = .r if inlist(E_Sgirl_study_marry, .r) |  inlist(E_Sboy_study_marry, .r)

	replace E_Sfertility = .d if E_Stwo_girls==.d | E_Stwo_boys==.d
	replace E_Sfertility = .r if E_Stwo_girls==.r | E_Stwo_boys==.r


			**********************************************************
			*					GENDER ATTITUDES		     		 *
			**********************************************************
	
	foreach var in E_Swives_less_edu E_Sboy_more_oppo E_Sman_final_deci E_Swoman_viol ///
	E_Scontrol_daughters E_Swoman_role_home E_Smen_better_suited E_Smarriage_more_imp E_Steacher_suitable {
		gen `var'_n_flag2 = inlist(`var'_n, .d, .r, .i, .f, .e) if attrition==0
		replace  `var'_n_flag2 = 1 if attrition==1 & mi(`var'_n) // attrited - so invalid skip
		}
		
	foreach var in E_Select_woman E_Stown_studies E_Ssimilar_right E_Sallow_work {
		gen `var'_y_flag2 = inlist(`var'_y, .d, .r, .i, .f, .e) if attrition==0
		replace  `var'_y_flag2 = 1 if attrition==1 & mi(`var'_y) // attrited - so invalid skip
		}
	
	foreach var in E_Sshy E_Slaugh E_Sgirl_marriage_age_19 E_Smarriage_age_diff_m E_Sstudy_marry E_Sfertility {
		gen `var'_flag2 = inlist(`var', .d, .r, .i, .f, .e) if attrition==0
		replace  `var'_flag2 = 1 if attrition==1 & mi(`var') // attrited - so invalid skip
		}
	
		****		
	// E_Sshy E_Slaugh not inlcuded in index as they fail the correlation check metnioned in PaP
	local gender E_Swives_less_edu_n E_Select_woman_y E_Sboy_more_oppo_n E_Stown_studies_y ///
		E_Sman_final_deci_n E_Swoman_viol_n E_Scontrol_daughters_n E_Swoman_role_home_n ///
		E_Smen_better_suited_n E_Ssimilar_right_y ///
		E_Smarriage_more_imp_n E_Steacher_suitable_n E_Sgirl_marriage_age_19 ///
		E_Smarriage_age_diff_m E_Sstudy_marry E_Sallow_work_y E_Sfertility
		

		
		* to drop index values later if >50% invalid missing variables in index. 
		global el_gender_flag2	
		foreach y in `gender' {		
				global el_gender_flag2 $el_gender_flag2 `y'_flag2
				}		

	
	local gender_flag
	
		foreach y in `gender' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition_el==0
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local gender_flag `gender_flag' `y'_flag
				
			}	
		}		
	
global el_gender_flag `gender_flag'	

**Average Gender index
	
	cap drop check
	egen E_Sgender_index=rowtotal(`gender'), mi
	egen check=rownonmiss(`gender')

	tab check
	qui sum check
	replace E_Sgender_index=. if check<`r(max)'   
	replace E_Sgender_index=E_Sgender_index/`r(max)'
	drop check
	label var E_Sgender_index "Student gender index"

	// Factor based index:
	
	
		local gender E_Swives_less_edu_n E_Select_woman_y E_Sboy_more_oppo_n E_Stown_studies_y ///
		E_Sman_final_deci_n E_Swoman_viol_n E_Scontrol_daughters_n E_Swoman_role_home_n ///
		E_Smen_better_suited_n E_Ssimilar_right_y ///
		E_Smarriage_more_imp_n E_Steacher_suitable_n E_Sgirl_marriage_age_19 ///
		E_Smarriage_age_diff_m E_Sstudy_marry E_Sallow_work_y E_Sfertility
		
		factor `gender' if B_treat==0 & !mi(E_Steam_id)  & attrition_el==0, factors(1)
		
	predict E_Sgender_index_factor
	sum E_Sgender_index_factor if B_treat==0 & !mi(E_Steam_id)  & attrition_el==0
	replace E_Sgender_index_factor = (E_Sgender_index_factor--r(mean))/r(sd)
	// corr B_Sgender_index_factor E_Sgender_index_factor // test relationship between factor index and old unweighted and inverse var. weighted indices
// 	corr E_Sgender_index E_Sgender_index_factor //very high correl btwn endline 
	

**Weighted gender index
	
	local vars
	foreach var in `gender' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			}
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
	
	local i=0
		foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
	
	}
		

	egen E_Sgender_index2_i=rsum2(sum_*), anymiss
		
	cap drop sum_*		
		
	lab var E_Sgender_index "Unweighted Gender Attitudes Index"
	lab var E_Sgender_index2_i "Gender Attitudes Index"
	lab var E_Sgender_index_factor "Factor Gender Attitudes Index"	
	****** 1.Ca Weighted gender index (without imputing) ********
		
		
		global el1_wts " "
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		local i=0
			foreach x in `gender' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			// 1/13/2020 addition for reweighting EL2
			
			* storing EL1 weights
			local wt: di B[`i',1]
			global el1_wts ${el1_wts} `x'`wt'
			
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		local el1_sum = C[1,1] // storing sum of el1 weights
				
		* sum of weights of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  // sum of weights of variables to keep
		
		** weighting the variables
		
		egen E_Sgender_index2_ni=rsum2(sum_*), anymiss
		replace E_Sgender_index2_ni = E_Sgender_index2_ni*(C[1,1]/keep_sum)
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 			
			
	lab var E_Sgender_index2_ni "Endline Gender Attitudes Index"		

	
**1.A Gender Attitudes: Education``

local educ E_Swives_less_edu_n E_Sboy_more_oppo_n E_Stown_studies_y

* flags for missing values
	global el_educ_flag
	foreach var in `educ' {
		global el_educ_flag $el_educ_flag `var'_flag
		}
		
**Average

	cap drop check
	egen E_Sgender_index_educ=rowtotal(`educ'), mi
	egen check=rownonmiss(`educ')

	tab check
	qui sum check
	replace E_Sgender_index_educ=. if check<`r(max)'   
	replace E_Sgender_index_educ=E_Sgender_index_educ/`r(max)'
	drop check
	label var E_Sgender_index_educ "Student gender index (Education)"


**Weighted
	
	local vars
	foreach var in `educ' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			}
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

				
	local i=0
		foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
		}
		
		egen E_Sgender_index2_educ_i=rsum2(sum_*), anymiss
		
	*cap drop *_temp
	cap drop sum_*
		
	lab var E_Sgender_index_educ "Unweighted Education Attitudes Sub-Index"
	lab var E_Sgender_index2_educ_i "Education Attitudes Sub-Index"

** Weighted (not imputed)
		
		global el1_educ_wts " "
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		local i=0
			foreach x in `educ' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			* storing EL1 weights // 7 Feb addition
			local wt: di B[`i',1]
			global el1_educ_wts ${el1_educ_wts} `x'`wt'
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  // sum of weights of variables to keep
		
		** weighting the variables
		
		egen E_Sgender_index2_educ_ni=rsum2(sum_*), anymiss
		replace E_Sgender_index2_educ_ni = E_Sgender_index2_educ_ni*(C[1,1]/keep_sum)
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 			
			
	lab var E_Sgender_index2_educ_ni "Education attitudes"		



**1.B Gender Attitudes: Employment

local emp E_Swoman_role_home_n E_Smen_better_suited_n E_Smarriage_more_imp_n E_Steacher_suitable_n E_Sallow_work_y


* flags for missing values
	global el_emp_flag
	foreach var in `emp' {
		global el_emp_flag $el_emp_flag `var'_flag
		}
		
**Average

	cap drop check
	egen E_Sgender_index_emp=rowtotal(`emp'), mi
	egen check=rownonmiss(`emp')

	tab check
	qui sum check
	replace E_Sgender_index_emp=. if check<`r(max)'   
	replace E_Sgender_index_emp=E_Sgender_index_emp/`r(max)'
	drop check
	label var E_Sgender_index_emp "Student gender index (Employment)"


**Weighted
	
	local vars
	foreach var in `emp' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			}
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

				
	local i=0
		foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
		}
		
		egen E_Sgender_index2_emp_i=rsum2(sum_*), anymiss
		
	*cap drop *_temp
	cap drop sum_*
		
	lab var E_Sgender_index_emp "Unweighted Employment Attitudes Sub-Index"
	lab var E_Sgender_index2_emp_i "Employment Attitudes Sub-Index"

** Weighted (not imputed)
		
		global el1_emp_wts ""
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		local i=0
			foreach x in `emp' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			* storing EL1 weights // 7 Feb addition
			local wt: di B[`i',1]
			global el1_emp_wts ${el1_emp_wts} `x'`wt'

			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  // sum of weights of variables to keep
		
		** weighting the variables
		
		egen E_Sgender_index2_emp_ni=rsum2(sum_*), anymiss
		replace E_Sgender_index2_emp_ni = E_Sgender_index2_emp_ni*(C[1,1]/keep_sum)
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 			
			
	lab var E_Sgender_index2_emp_ni "Employment attitudes"		



**1.C Gender Attitudes: Female Gender Roles

local sub E_Select_woman_y E_Sman_final_deci_n E_Swoman_viol_n E_Scontrol_daughters_n E_Ssimilar_right_y /// 
		 E_Sgirl_marriage_age_19 E_Smarriage_age_diff_m E_Sstudy_marry

		 
	 * flags for missing values
	global el_sub_flag
	foreach var in `sub' {
		global el_sub_flag $el_sub_flag `var'_flag
		}
		
	**Average

	cap drop check
	egen E_Sgender_index_sub=rowtotal(`sub'), mi
	egen check=rownonmiss(`sub')

	tab check
	qui sum check
	replace E_Sgender_index_sub=. if check<`r(max)'   
	replace E_Sgender_index_sub=E_Sgender_index_sub/`r(max)'
	drop check
	label var E_Sgender_index_sub "Student gender index (Female gender roles)"


**Weighted
	
	local vars
	foreach var in `sub' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			}
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

				
	local i=0
		foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
		}
		
		egen E_Sgender_index2_sub_i=rsum2(sum_*), anymiss
		
	*cap drop *_temp
	cap drop sum_*
	
	lab var E_Sgender_index_sub "Unweighted Attitudes towards Female Gender Roles Sub-Index"
	lab var E_Sgender_index2_sub_i "Attitudes towards Female Gender Roles Sub-Index"

** Weighted (not imputed)
		global el1_sub_wts " "
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		local i=0
			foreach x in `sub' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			* storing EL1 weights // 7 Feb addition
			local wt: di B[`i',1]
			global el1_sub_wts ${el1_sub_wts} `x'`wt'
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  // sum of weights of variables to keep
		
		** weighting the variables
		
		egen E_Sgender_index2_sub_ni=rsum2(sum_*), anymiss
		replace E_Sgender_index2_sub_ni = E_Sgender_index2_sub_ni*(C[1,1]/keep_sum)
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 			
			
	lab var E_Sgender_index2_sub_ni "Attitudes towards female gender roles"		

**1.D Gender Attitudes: Fertility

	gen E_Sgender_index_fert = E_Sfertility if E_Sfertility_flag==0 // not including imputed values for single variable indices 
	gen E_Sgender_index2_fert_i = E_Sfertility if E_Sfertility_flag==0
	gen E_Sgender_index2_fert_ni = E_Sfertility if E_Sfertility_flag==0
	
	lab var E_Sgender_index_fert "Unweighted Fertility Attitudes Sub-Index"
	lab var E_Sgender_index2_fert_i "Progressive Fertility Attitudes (Dummy)"
	lab var E_Sgender_index2_fert_ni "Fertility attitudes"

		
		
			**********************************************************
			*					ASPIRATIONS				     		 *
			**********************************************************


**recoding two variables again - 13.06.2018
	local aspiration E_Sboard_score_median E_Shighest_educ_median E_Sdiscuss_educ E_Soccupa_25_white E_Scont_educ

	cap drop E_Sboard_score_median E_Shighest_educ_median
	
	local g 1
	local b 0
	
	foreach x in g b {
		summ E_Sboard_score if B_treat==0 & B_Sgirl==``x'', detail
		local control_median= `r(p50)'
		gen E_Sboard_score_median_`x'=1 if E_Sboard_score>`control_median' & !mi(E_Sboard_score)  & B_Sgirl==``x''
		replace E_Sboard_score_median_`x'=0 if E_Sboard_score<=`control_median' & !mi(E_Sboard_score)  & B_Sgirl==``x''
		la val E_Sboard_score_median_`x' yesno
		
		label var E_Sboard_score_median_`x' "Expected 10th marks > control-gender median"
		
		}
		
		gen E_Sboard_score_median = E_Sboard_score_median_g if B_Sgirl==1
		replace E_Sboard_score_median = E_Sboard_score_median_b if B_Sgirl==0
		la var  E_Sboard_score_median "Expected 10th marks > control-gender median"
			la val E_Sboard_score_median yesno
		order E_Sboard_score_median_g E_Sboard_score_median_b E_Sboard_score_median, after(E_Sboard_score)

	**** 
	
	gen E_Shighest_educ_order = E_Shighest_educ
	recode E_Shighest_educ_order (10=5)(5=6)(6=7)(7=8)(8=9)(9=10)
	label var E_Shighest_educ_order "Highest level of education you would like to complete, in order"

		foreach x in g b {	
			summ E_Shighest_educ_order if B_treat==0 & B_Sgirl==``x'', detail
			local control_median= `r(p50)'
			gen E_Shighest_educ_median_`x'=1 if E_Shighest_educ_order>`control_median' & !mi(E_Shighest_educ_order) & B_Sgirl==``x''
			replace E_Shighest_educ_median_`x'=0 if E_Shighest_educ_order<=`control_median' & !mi(E_Shighest_educ_order) & B_Sgirl==``x''
			la val E_Shighest_educ_median_`x' yesno
			
			label var E_Shighest_educ_median_`x' "Highest level of education you would like to complete > control-gender median"
			}
			
		gen E_Shighest_educ_median = E_Shighest_educ_median_g if B_Sgirl==1
		replace E_Shighest_educ_median = E_Shighest_educ_median_b if B_Sgirl==0
		la var  E_Shighest_educ_median "Highest level of education you would like to complete > control-gender median"
			la val E_Shighest_educ_median yesno
	
	order E_Shighest_educ_order E_Shighest_educ_median_g E_Shighest_educ_median_b E_Shighest_educ_median, after(E_Shighest_educ)

	** flags for invalid skips
	gen E_Sboard_score_median_flag2 = inlist(E_Sboard_score, .d, .r) if attrition==0
	replace E_Sboard_score_median_flag2= 1 if attrition==1 & mi(E_Sboard_score) 
	
	gen E_Shighest_educ_median_flag2 = inlist(E_Shighest_educ_order, .d, .r) if attrition==0
	replace E_Shighest_educ_median_flag2 = 1 if attrition==1 & mi(E_Shighest_educ_order)
	
	gen E_Soccupa_25_white_flag2 = inlist(E_Soccupa_25, .d, .r) if attrition==0
	replace E_Soccupa_25_white_flag2 = 1 if attrition==1 & mi(E_Soccupa_25)
	
	foreach var in E_Sdiscuss_educ E_Scont_educ {
		gen `var'_flag2 = inlist(`var', .d, .r, .i, .f, .e) if attrition==0
		replace  `var'_flag2 = 1 if attrition==1 & mi(`var') // attrited - so invalid skip
		}
	
	
	
**Average Aspirations Index
	
		
* to drop index values later if >50% invalid missing variables in index. 
global el_aspiration_flag2

foreach y in `aspiration' {		
		global el_aspiration_flag2 $el_aspiration_flag2 `y'_flag2
		}	
		
		
local aspiration_flag
	
		foreach y in `aspiration' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition_el==0
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local aspiration_flag `aspiration_flag' `y'_flag
				
			}	
		}		

	
	global el_aspiration_flag `aspiration_flag'
	
	cap drop check if B_Sgirl==1
	egen E_Saspiration_index=rowtotal(`aspiration') if B_Sgirl==1, mi
	egen check=rownonmiss(`aspiration') if B_Sgirl==1

	tab check if B_Sgirl==1
	qui sum check if B_Sgirl==1
	replace E_Saspiration_index=. if check<`r(max)' & B_Sgirl==1  
	replace E_Saspiration_index=E_Saspiration_index/`r(max)' if B_Sgirl==1
	drop check
	label var E_Saspiration_index "Student Aspiration index"

	
	// E_S aspiration index, by EMF 29 oct 2022
	local aspiration E_Sboard_score_median E_Shighest_educ_median E_Sdiscuss_educ E_Soccupa_25_white E_Scont_educ
		factor `aspiration' if B_Sgirl==1 & B_treat==0 & !mi(E_Steam_id)  & attrition_el==0,  factors(1)
		predict E_Saspiration_index_factor if B_Sgirl==1 & !mi(E_Steam_id)  & attrition_el==0
		sum E_Saspiration_index_factor if B_treat==0  & B_Sgirl==1 & !mi(E_Steam_id) & attrition_el==0
		replace E_Saspiration_index_factor = (E_Saspiration_index_factor-r(mean))/r(sd)
	
**Weighted aspiration index
		
		local vars
		foreach var in `aspiration' {
				gen `var'_temp=`var'
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}

	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E_Saspiration_index2_i=rsum2(sum_*) if B_Sgirl==1, anymiss
			
	*cap drop *_temp
	cap drop sum_*
			
	lab var E_Saspiration_index "Unweighted Aspirations Index"
	lab var E_Saspiration_index2_i "Aspirations Index"
	lab var E_Saspiration_index_factor "Factor Aspirations Index"		
		
	****** 2.1a Weighted Aspirations index (without imputing) ********
		
		* limit to girls
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `aspiration' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==1 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==1 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==1 // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if B_Sgirl==1 // sum of weights of variables to keep
		
		** weighting the variables		
		egen E_Saspiration_index2_ni=rsum2(sum_*) if B_Sgirl==1, anymiss
		replace E_Saspiration_index2_ni = E_Saspiration_index2_ni*(C[1,1]/keep_sum) if B_Sgirl==1
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var E_Saspiration_index2_ni "Endline Aspirations Index"	
	
	****** BOYS Aspirations index ********
	
**Average Aspirations Index
	
	local aspiration_boy E_Sboard_score_median E_Shighest_educ_median E_Sdiscuss_educ E_Soccupa_25_white E_Scont_educ
		
	local aspiration_boy_flag
	
		foreach y in `aspiration_boy' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition_el==0
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local aspiration_boy_flag `aspiration_boy_flag' `y'_flag
				
			}	
		}		

	
	global el_aspiration_boy_flag `aspiration_boy_flag'
	
	cap drop check if B_Sgirl==0
	egen E_Saspiration_boy_index=rowtotal(`aspiration_boy') if B_Sgirl==0, mi
	egen check=rownonmiss(`aspiration_boy') if B_Sgirl==0

	tab check if B_Sgirl==0
	qui sum check if B_Sgirl==0
	replace E_Saspiration_boy_index=. if check<`r(max)' & B_Sgirl==0 
	replace E_Saspiration_boy_index=E_Saspiration_boy_index/`r(max)' if B_Sgirl==0
	drop check
	label var E_Saspiration_boy_index "Unweighted Endline Boys' Aspiration Index"

	
	
**Weighted aspiration index
		
		local vars
		foreach var in `aspiration' {
				gen `var'_temp=`var'
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}

	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E_Saspiration_boy_index2_i=rsum2(sum_*) if B_Sgirl==0, anymiss
			
	cap drop sum_*
			
	lab var E_Saspiration_boy_index "Unweighted Endline Boys' Aspirations Index"
	lab var E_Saspiration_boy_index2_i "Endline Boys' Aspirations Index"
			
		
	****** 2.1a Weighted Aspirations index (without imputing) ********
		
		* limit to boys
		preserve
		keep if B_Sgirl==0
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `aspiration_boy' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==0 // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if B_Sgirl==0 // sum of weights of variables to keep
		
		** weighting the variables		
		egen E_Saspiration_boy_index2_ni=rsum2(sum_*) if B_Sgirl==0, anymiss
		replace E_Saspiration_boy_index2_ni = E_Saspiration_boy_index2_ni*(C[1,1]/keep_sum) if B_Sgirl==0
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var E_Saspiration_index2_ni "Endline Boys' Aspirations Index"	
			
			
***3. Behaviors Index ************************************

	*Mobility
	/* rename E_Sdisco_college  E_Sdiscourage_college 
	rename E_Sdisco_work E_Sdiscourage_work */
	local varlist E_Sdiscourage_college E_Sdiscourage_work
	foreach x in `varlist' {
	gen `x'_g=inlist(`x',0) if !mi(`x') & B_Sgirl==1
	gen `x'_b=inlist(`x',0) if !mi(`x') & B_Sgirl==0
	local varlab: variable label `x'
	label var `x'_g "Disagree: `varlab'"
	label var `x'_b "Disagree: `varlab'"

	}

	gen E_Salone_friend_g=inlist(E_Salone_friend,1) if !mi(E_Salone_friend) & B_Sgirl==1
	lab var E_Salone_friend_g "Are you allowed to go to the school alone or with friends?"

	*Interaction with opposite sex
	
	*Girls
	gen E_Stalk_opp_gender_g=inlist(E_Stalk_opp_gender_comf,1) if !mi(E_Stalk_opp_gender_comf) & B_Sgirl==1
	gen E_Ssit_opp_gender_g=inlist(E_Ssit_opp_gender,1) if !mi(E_Ssit_opp_gender) & B_Sgirl==1
	lab var E_Stalk_opp_gender_g "Student is comfortable talking to students of the opposite sex"
	lab var E_Ssit_opp_gender_g  "Student sister next to students of the opposite sex in the classroom"

	*Boys
	gen E_Stalk_opp_gender_b=inlist(E_Stalk_opp_gender_comf,1) if !mi(E_Stalk_opp_gender_comf) & B_Sgirl==0
	gen E_Ssit_opp_gender_b=inlist(E_Ssit_opp_gender,1) if !mi(E_Ssit_opp_gender) & B_Sgirl==0
	lab var E_Stalk_opp_gender_b "Student is comfortable talking to students of the opposite sex"
	lab var E_Ssit_opp_gender_b  "Student sits next to students of the opposite sex in the classroom"

	*Participation in hh chores
	*Girls
	gen E_Sabsent_sch_hhwork_g=inlist(E_Sabsent_sch_reason_hhwork,0) if !mi(E_Sabsent_sch_reason_hhwork) & B_Sgirl==1
	lab var E_Sabsent_sch_hhwork_g "Girl has not missed school due to household responsibilities in the last one month"
	
	gen E_Scook_clean_g=inlist(E_Scook_clean_y,0) if !mi(E_Scook_clean_y) & B_Sgirl==1
	gen E_Shh_shopping_g=inlist(E_Shh_shopping_y,0) if !mi(E_Shh_shopping_y) & B_Sgirl==1
	gen E_Stake_care_young_sib_g=inlist(E_Stake_care_young_sib_y,0) if !mi(E_Stake_care_young_sib_y) & B_Sgirl==1

	lab var E_Scook_clean_g "Student does not cook/clean/wash clothes"
	lab var E_Stake_care_young_sib_g "Student does not take care of young sibling/old people"
	lab var E_Shh_shopping_g "Student does not help with shopping for hh provisions"

	*Boys
	gen E_Sabsent_sch_hhwork_b=inlist(E_Sabsent_sch_reason_hhwork,1) if !mi(E_Sabsent_sch_reason_hhwork) & B_Sgirl==0
	lab var E_Sabsent_sch_hhwork_b "Boy has missed school due to household responsibilities in the last one month"
	
	gen E_Scook_clean_b=inlist(E_Scook_clean_y,1) if !mi(E_Scook_clean_y) & B_Sgirl==0
	gen E_Shh_shopping_b=inlist(E_Shh_shopping_y,1) if !mi(E_Shh_shopping_y) & B_Sgirl==0
	gen E_Stake_care_young_sib_b=inlist(E_Stake_care_young_sib_y,1) if !mi(E_Stake_care_young_sib_y) & B_Sgirl==0

	lab var E_Scook_clean_b "Boy cooks/cleans/washes clothes"
	lab var E_Stake_care_young_sib_b "Boy takes care of young sibling/old people"
	lab var E_Shh_shopping_b "Boy helps with shopping for hh provisions"



	*Decision-making
	gen E_Sabsent_sch_g=inlist(E_Sabsent_days,0) if !mi(E_Sabsent_days) & B_Sgirl==1
	lab var E_Sabsent_sch_g "During last week student was not absent from school"

	gen E_Sfuture_work_g=inlist(E_Sfuture_work,1) if !mi(E_Sfuture_work) & B_Sgirl==1
	gen E_Sdecision_past_tenth_g=inlist(E_Sdecision_past_tenth_y,1) if !mi(E_Sdecision_past_tenth_y) & B_Sgirl==1
	gen E_Sdecision_work_g=inlist(E_Sdecision_work_y,1) if !mi(E_Sdecision_work_y) & B_Sgirl==1
	gen E_Sdecision_kindofwork_g=inlist(E_Sdecision_kindofwork_y,1) if !mi(E_Sdecision_kindofwork_y) & B_Sgirl==1
	gen E_Sdecision_chores_g=inlist(E_Sdecision_chores_y,1) if !mi(E_Sdecision_chores_y) & B_Sgirl==1
	
	lab var E_Sfuture_work_g "Student is able to talk to parents about what work she would do in the future"
	lab var E_Sdecision_past_tenth_g "Student takes Decision: Whether or not you will continue in school past 10th grade"
	lab var E_Sdecision_work_g "Student takes Decision: If you will work after you finish your studies"
	lab var E_Sdecision_kindofwork_g "Student takes Decision: What type of work you will do after you finish your studies"
	lab var E_Sdecision_chores_g "Student takes Decision: What types of chores you do at home (cooking, cleaning etc)"

	
	* flags for invalid missings
	local g 1
	local b 0
	
	foreach x in g b {
		gen E_Stalk_opp_gender_`x'_flag2 = inlist(E_Scomf_opp_gender_girl, .d, .r, .i, .f, .e) |  inlist(E_Scomf_opp_gender_boy, .d, .r, .i, .f, .e) if attrition==0 & B_Sgirl==``x''
		replace  E_Stalk_opp_gender_`x'_flag2 = 1 if attrition==1 & mi(E_Stalk_opp_gender_`x') & B_Sgirl==``x''

		gen E_Ssit_opp_gender_`x'_flag2 = inlist(E_Ssit_opp_gender_girl, .d, .r, .i, .f, .e) |  inlist(E_Ssit_opp_gender_boy, .d, .r, .i, .f, .e) if attrition==0 & B_Sgirl==``x''
		replace E_Ssit_opp_gender_`x'_flag2 = 1 if attrition==1 & mi(E_Ssit_opp_gender_`x')  & B_Sgirl==``x''
		
		gen E_Sabsent_sch_hhwork_`x'_flag2 = inlist(E_Sabsent_sch_reason_hhwork, .d, .r, .i, .f, .e) if attrition==0 & B_Sgirl==``x''
		replace E_Sabsent_sch_hhwork_`x'_flag2 = 1 if attrition==1 & mi(E_Sabsent_sch_hhwork_`x')  & B_Sgirl==``x''
		
		foreach var in E_Scook_clean E_Sdiscourage_work E_Sdiscourage_college {
			gen `var'_`x'_flag2 = inlist(`var', .d, .r, .i, .f, .e) if attrition==0 & B_Sgirl==``x''
			replace `var'_`x'_flag2 = 1 if attrition==1 & mi(`var'_`x') & B_Sgirl==``x'' 		
			}
		}
	 
	 foreach var in E_Sfuture_work E_Sdecision_past_tenth E_Sdecision_work E_Sdecision_kindofwork E_Salone_friend E_Sabsent_sch {
	 	gen `var'_g_flag2 = inlist(`var', .d, .r, .i, .f, .e) if attrition==0
		replace  `var'_g_flag2 = 1 if attrition==1 & mi(`var') // attrited - so invalid skip
		}

	
	
	**imputing values	
	local behavior_girl E_Stalk_opp_gender_g E_Ssit_opp_gender_g E_Scook_clean_g ///
			E_Sfuture_work_g E_Sdecision_past_tenth_g E_Sdecision_work_g ///
			E_Sdecision_kindofwork_g E_Salone_friend_g E_Sabsent_sch_g E_Sabsent_sch_hhwork_g ///
			E_Sdiscourage_work_g E_Sdiscourage_college_g
	
	local behavior_boy E_Stalk_opp_gender_b E_Ssit_opp_gender_b E_Scook_clean_b ///
			 E_Sabsent_sch_hhwork_b E_Sdiscourage_work_b E_Sdiscourage_college_b
			 
			 
	* to drop index values later if >50% invalid missing variables in index. 
	global el_behavior_girl_flag2

	foreach y in `behavior_girl' {		
			global el_behavior_girl_flag2 $el_behavior_girl_flag2 `y'_flag2
			}	
			
	global el_behavior_boy_flag2

	foreach y in `behavior_boy' {		
			global el_behavior_boy_flag2 $el_behavior_boy_flag2 `y'_flag2
			}	
			
	* * * * * * * * * * * * * * * * * * * * 
		
	local behavior_girl_flag
			
	foreach y in `behavior_girl' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition_el==0 & B_Sgirl==1
				bysort B_Sdistrict B_treat: egen x = mean (`y') if B_Sgirl==1
				qui replace `y'=x if `y'_flag==1
				drop x
				local behavior_girl_flag `behavior_girl_flag' `y'_flag
				
			}	
		}		

	global el_behavior_girl_flag `behavior_girl_flag'
	
	local behavior_boy_flag
			
	foreach y in `behavior_boy' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition_el==0 & B_Sgirl==0
				bysort B_Sdistrict B_treat: egen x = mean (`y') if B_Sgirl==0
				qui replace `y'=x if `y'_flag==1
				drop x
				local behavior_boy_flag `behavior_boy_flag' `y'_flag
				
			}	
		}		

		global el_behavior_boy_flag `behavior_boy_flag'

		
		
		** 10:05
**Average Girls' Behavior Index

	cap drop check
	egen E_Sbehavior_index_g=rowtotal(`behavior_girl') if B_Sgirl==1 & attrition_el==0, mi
	egen check=rownonmiss(`behavior_girl') if B_Sgirl==1 & attrition_el==0

	tab check if B_Sgirl==1
	qui sum check if B_Sgirl==1
	replace E_Sbehavior_index_g=. if check<`r(max)' & B_Sgirl==1 & attrition_el==0   
	replace E_Sbehavior_index_g=E_Sbehavior_index_g/`r(max)' if B_Sgirl==1 & attrition_el==0
	drop check
	label var E_Sbehavior_index_g "Girls' Behavior index"
	
	
	
**Weighted Girls' Behavior Index
		
		
		local vars
		foreach var in `behavior_girl' {
				gen `var'_temp=`var' if B_Sgirl==1 
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==1 & attrition_el==0
			}
			
	egen E_Sbehavior_index2_g_i=rsum2(sum_*) if B_Sgirl==1, anymiss
			
	*cap drop *_temp
	cap drop sum_*
			
	lab var E_Sbehavior_index_g "Girls' Unweighted Behavior Index"
	lab var E_Sbehavior_index2_g_i "Girls' Behavior Index"
	
		****** 2.1a Weighted Girls' Behavior index (without imputing) ********
		
		* limit to girls
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `behavior_girl' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==1 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==1 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==1 // ind weight * variable value; 0 for missing variables
			}			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if B_Sgirl==1 // sum of weights of variables to keep
		
		** weighting the variables		
		egen E_Sbehavior_index2_g_ni=rsum2(sum_*) if B_Sgirl==1, anymiss
		replace E_Sbehavior_index2_g_ni = E_Sbehavior_index2_g_ni*(C[1,1]/keep_sum) if B_Sgirl==1
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum			
			
	lab var E_Sbehavior_index2_g_ni "Endline Girls' Behavior Index"	

**Average Boys' Behavior Index
	
	local behavior_boy E_Stalk_opp_gender_b E_Ssit_opp_gender_b E_Scook_clean_b ///
			 E_Sabsent_sch_hhwork_b E_Sdiscourage_work_b E_Sdiscourage_college_b
	
	factor `behavior_boy'
	
	cap drop check
	egen E_Sbehavior_index_b=rowtotal(`behavior_boy') if B_Sgirl==0 & attrition_el==0, mi
	egen check=rownonmiss(`behavior_boy') if B_Sgirl==0 & attrition_el==0

	tab check if B_Sgirl==0
	qui sum check if B_Sgirl==0
	replace E_Sbehavior_index_b=. if check<`r(max)' &  B_Sgirl==0 & attrition_el==0  
	replace E_Sbehavior_index_b=E_Sbehavior_index_b/`r(max)' if B_Sgirl==0 & attrition_el==0
	drop check
	label var E_Sbehavior_index_b "Boys' Behavior index"
	
	
**Weighted Boys' Behavior Index		
		
		local vars
		foreach var in `behavior_boy' {
				gen `var'_temp=`var' if B_Sgirl==0 
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==0 & attrition_el==0
			}
			
	egen E_Sbehavior_index2_b_i=rsum2(sum_*) if B_Sgirl==0, anymiss
			
	*cap drop *_temp
	cap drop sum_*
			
	lab var E_Sbehavior_index_b "Boys' Unweighted Behavior Index"
	lab var E_Sbehavior_index2_b_i "Boys' Behavior Index"

		****** 2.1a Weighted Boys' Behavior index (without imputing) ********
		
		* limit to girls
		preserve
		keep if B_Sgirl==0
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `behavior_boy' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==0 // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if B_Sgirl==0 // sum of weights of variables to keep
		
		** weighting the variables		
		egen E_Sbehavior_index2_b_ni=rsum2(sum_*) if B_Sgirl==0, anymiss
		replace E_Sbehavior_index2_b_ni = E_Sbehavior_index2_b_ni*(C[1,1]/keep_sum) if B_Sgirl==0
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var E_Sbehavior_index2_b_ni "Endline Boys' Behavior Index"	


****COMMON BEHAVIOR INDEX FOR BOYS AND GIRLS

	**Opposite sex (same direction for both boys and girls)

	gen E_Stalk_opp_gender_comm=inlist(E_Stalk_opp_gender_comf,1) if !mi(E_Stalk_opp_gender_comf)
	gen E_Ssit_opp_gender_comm=inlist(E_Ssit_opp_gender,1) if !mi(E_Ssit_opp_gender)
	
	**Household chores (opposite direction for girls and boys)
	
	gen E_Scook_clean_comm=inlist(E_Scook_clean_y,0) if !mi(E_Scook_clean_y) & B_Sgirl==1
	replace E_Scook_clean_comm=inlist(E_Scook_clean_y,1) if !mi(E_Scook_clean_y) & B_Sgirl==0

	gen E_Sabsent_sch_hhwork_comm=inlist(E_Sabsent_sch_reason_hhwork,0) if !mi(E_Sabsent_sch_reason_hhwork) & B_Sgirl==1
	replace E_Sabsent_sch_hhwork_comm=inlist(E_Sabsent_sch_reason_hhwork,1) if !mi(E_Sabsent_sch_reason_hhwork) & B_Sgirl==0


	**Supporting female relatives' ambitions (same direction for girls and boys)

	local varlist E_Sdiscourage_college E_Sdiscourage_work
	
	
	foreach x in `varlist' {
	
	gen `x'_comm=inlist(`x',0) if !mi(`x')
	local varlab: variable label `x' 
	label var `x'_comm "Disagree: `varlab'"
	}
	
	** flags for invalid missings - to drop if >50% missing variables in index
		foreach x in comm {
		gen E_Stalk_opp_gender_`x'_flag2 = inlist(E_Scomf_opp_gender_girl, .d, .r, .i, .f, .e) |  inlist(E_Scomf_opp_gender_boy, .d, .r, .i, .f, .e) if attrition_el==0 
		replace  E_Stalk_opp_gender_`x'_flag2 = 1 if attrition_el==1 & mi(E_Stalk_opp_gender_`x')

		gen E_Ssit_opp_gender_`x'_flag2 = inlist(E_Ssit_opp_gender_girl, .d, .r, .i, .f, .e) |  inlist(E_Ssit_opp_gender_boy, .d, .r, .i, .f, .e) if attrition_el==0 
		replace E_Ssit_opp_gender_`x'_flag2 = 1 if attrition_el==1 & mi(E_Ssit_opp_gender_`x') 
		
		gen E_Sabsent_sch_hhwork_`x'_flag2 = inlist(E_Sabsent_sch_reason_hhwork, .d, .r, .i, .f, .e) if attrition_el==0 
		replace E_Sabsent_sch_hhwork_`x'_flag2 = 1 if attrition_el==1 & mi(E_Sabsent_sch_hhwork_`x')  
		
		foreach var in E_Scook_clean E_Sdiscourage_work E_Sdiscourage_college {
			gen `var'_`x'_flag2 = inlist(`var', .d, .r, .i, .f, .e) if attrition_el==0 
			replace `var'_`x'_flag2 = 1 if attrition_el==1 & mi(`var'_`x') 	
			}
		}
	 


	local behavior_common E_Stalk_opp_gender_comm E_Ssit_opp_gender_comm E_Scook_clean_comm ///
					E_Sabsent_sch_hhwork_comm E_Sdiscourage_college_comm E_Sdiscourage_work_comm
					
					
	global el_behavior_common_flag2

	foreach y in `behavior_common' {		
			global el_behavior_common_flag2 $el_behavior_common_flag2 `y'_flag2
			}	
			
	* * * * * * * * * * * *

	local behavior_common_flag
	foreach y in `behavior_common' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
		qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition_el==0
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local behavior_common_flag `behavior_common_flag' `y'_flag
				
			}	
		}	
	
	global el_behavior_common_flag `behavior_common_flag'

	cap drop check
	egen E_Sbehavior_index=rowtotal(`behavior_common'), mi
	egen check=rownonmiss(`behavior_common')

	tab check
	qui sum check
	replace E_Sbehavior_index=. if check<`r(max)'   
	replace E_Sbehavior_index=E_Sbehavior_index/`r(max)'
	drop check
	label var E_Sbehavior_index "Self-reported behavior index"

	factor `behavior_common' if B_treat==0 & !mi(E_Steam_id) & attrition_el==0, factors(1)
	predict E_Sbehavior_index_factor if !mi(E_Steam_id) & attrition_el==0
	sum E_Sbehavior_index_factor if B_treat==0 & !mi(E_Steam_id) & attrition_el==0
	replace E_Sbehavior_index_factor  = (E_Sbehavior_index_factor -r(mean))/r(sd)
	
	/*
	
	*/
**Weighted behavior index


	local vars
	foreach var in `behavior_common' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
				
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
	foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
	}
			
	egen E_Sbehavior_index2_i=rsum2(sum_*), anymiss
			
	*cap drop *_temp
	cap drop sum_*
			
	lab var E_Sbehavior_index "Unweighted Behavior Index"
	lab var E_Sbehavior_index2_i "Self-reported behavior index"

	
		****** 2.1a Weighted Behavior index (without imputing) ********
		
		global el1_wts_b " "
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		
		local i=0
			foreach x in `behavior_common' {
				local ++i	
				
				gen `x'_nmi = `x'_flag==0  // variable not missing	
				gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing
				
				gen double sum_`i'=  `x'_kw * `x'_temp  // ind weight * variable value; 0 for missing variables

				// ADDITION FOR EL2 APPLES TO APPLES FOR BEHAVIOR
				* storing EL1 weights 
				local wt: di B[`i',1]
				global el1_wts_b ${el1_wts_b} `x'`wt'
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw) // sum of weights of variables to keep
		
		** weighting the variables		
		egen E_Sbehavior_index2_ni=rsum2(sum_*) , anymiss
		replace E_Sbehavior_index2_ni = E_Sbehavior_index2_ni*(C[1,1]/keep_sum) 
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var E_Sbehavior_index2_ni "Endline Behavior Index"	

	
	
**** NEW VARIABLES: for behavior - including  E_Shh_shopping E_Stake_care_young_sib E_Sdecision_chores
local p2=1

if `p2'==1 {

	**imputing values	
	local behavior_girl E_Stalk_opp_gender_g E_Ssit_opp_gender_g E_Scook_clean_g E_Shh_shopping_g ///
			 E_Stake_care_young_sib_g  E_Sfuture_work_g E_Sdecision_past_tenth_g E_Sdecision_work_g ///
			E_Sdecision_kindofwork_g E_Sdecision_chores_g E_Salone_friend_g E_Sabsent_sch_g E_Sabsent_sch_hhwork_g ///
			E_Sdiscourage_work_g E_Sdiscourage_college_g
	
	local behavior_boy E_Stalk_opp_gender_b E_Ssit_opp_gender_b E_Scook_clean_b E_Shh_shopping_b E_Stake_care_young_sib_b  ///
			 E_Sabsent_sch_hhwork_b E_Sdiscourage_work_b E_Sdiscourage_college_b
	
	local behavior_girl_nv_flag
			
	foreach y in `behavior_girl' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition_el==0 & B_Sgirl==1
				bysort B_Sdistrict B_treat: egen x = mean (`y') if B_Sgirl==1
				qui replace `y'=x if `y'_flag==1
				drop x
				local behavior_girl_nv_flag `behavior_girl_nv_flag' `y'_flag
				
			}	
		}		

	global el_behavior_girl_nv_flag `behavior_girl_nv_flag'
	
	local behavior_boy_nv_flag
			
	foreach y in `behavior_boy' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition_el==0 & B_Sgirl==0
				bysort B_Sdistrict B_treat: egen x = mean (`y') if B_Sgirl==0
				qui replace `y'=x if `y'_flag==1
				drop x
				local behavior_boy_nv_flag `behavior_boy_nv_flag' `y'_flag
				
			}	
		}		

		global el_behavior_boy_nv_flag `behavior_boy_nv_flag'

**Average Girls' Behavior Index

	cap drop check
	egen E_Sbehavior_index_g_nv=rowtotal(`behavior_girl') if B_Sgirl==1 & attrition_el==0, mi
	egen check=rownonmiss(`behavior_girl') if B_Sgirl==1 & attrition_el==0

	tab check if B_Sgirl==1
	qui sum check if B_Sgirl==1
	replace E_Sbehavior_index_g_nv=. if check<`r(max)' & B_Sgirl==1 & attrition_el==0   
	replace E_Sbehavior_index_g_nv=E_Sbehavior_index_g_nv/`r(max)' if B_Sgirl==1 & attrition_el==0
	drop check
	label var E_Sbehavior_index_g_nv "Girls' Behavior index"
	
	
**Weighted Girls' Behavior Index
		
		
		local vars
		foreach var in `behavior_girl' {
				gen `var'_temp=`var' if B_Sgirl==1 
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==1 & attrition_el==0
			}
			
	egen E_Sbehavior_index2_g_i_nv=rsum2(sum_*) if B_Sgirl==1, anymiss
			
	*cap drop *_temp
	cap drop sum_*
			
	lab var E_Sbehavior_index_g_nv "Girls' Unweighted Behavior Index"
	lab var E_Sbehavior_index2_g_i_nv "Girls' Behavior Index"
	
		****** 2.1a Weighted Girls' Behavior index (without imputing) ********
		
		* limit to girls
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `behavior_girl' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==1 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==1 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==1 // ind weight * variable value; 0 for missing variables
			}			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if B_Sgirl==1 // sum of weights of variables to keep
		
		** weighting the variables		
		egen E_Sbehavior_index2_g_ni_nv=rsum2(sum_*) if B_Sgirl==1, anymiss
		replace E_Sbehavior_index2_g_ni_nv = E_Sbehavior_index2_g_ni_nv*(C[1,1]/keep_sum) if B_Sgirl==1
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum			
			
	lab var E_Sbehavior_index2_g_ni_nv "Endline Girls' Behavior Index"	

**Average Boys' Behavior Index

	cap drop check
	egen E_Sbehavior_index_b_nv=rowtotal(`behavior_boy') if B_Sgirl==0 & attrition_el==0, mi
	egen check=rownonmiss(`behavior_boy') if B_Sgirl==0 & attrition_el==0

	tab check if B_Sgirl==0
	qui sum check if B_Sgirl==0
	replace E_Sbehavior_index_b_nv=. if check<`r(max)' &  B_Sgirl==0 & attrition_el==0  
	replace E_Sbehavior_index_b_nv=E_Sbehavior_index_b_nv/`r(max)' if B_Sgirl==0 & attrition_el==0
	drop check
	label var E_Sbehavior_index_b_nv "Boys' Behavior index"
	
	
**Weighted Boys' Behavior Index		
		
		local vars
		foreach var in `behavior_boy' {
				gen `var'_temp=`var' if B_Sgirl==0 
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==0 & attrition_el==0
			}
			
	egen E_Sbehavior_index2_b_i_nv=rsum2(sum_*) if B_Sgirl==0, anymiss
			
	*cap drop *_temp
	cap drop sum_*
			
	lab var E_Sbehavior_index_b_nv "Boys' Unweighted Behavior Index"
	lab var E_Sbehavior_index2_b_i_nv "Boys' Behavior Index"

		****** 2.1a Weighted Boys' Behavior index (without imputing) ********
		
		* limit to girls
		preserve
		keep if B_Sgirl==0
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `behavior_boy' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==0 // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if B_Sgirl==0 // sum of weights of variables to keep
		
		** weighting the variables		
		egen E_Sbehavior_index2_b_ni_nv=rsum2(sum_*) if B_Sgirl==0, anymiss
		replace E_Sbehavior_index2_b_ni_nv = E_Sbehavior_index2_b_ni_nv*(C[1,1]/keep_sum) if B_Sgirl==0
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var E_Sbehavior_index2_b_ni_nv "Endline Boys' Behavior Index"	

	}
	

****BEHAVIOR SUB_INDICES
	

**3.A Interaction with opposite sex (boys and girls)

	local behavior_oppsex E_Stalk_opp_gender_comm E_Ssit_opp_gender_comm
	
	* flags for missing values
	global el_behavior_oppsex_flag
	foreach var in `behavior_oppsex' {
		global el_behavior_oppsex_flag $el_behavior_oppsex_flag `var'_flag
		}

	cap drop check
	egen E_Sbehavior_index_oppsex=rowtotal(`behavior_oppsex'), mi
	egen check=rownonmiss(`behavior_oppsex')

	tab check
	qui sum check
	replace E_Sbehavior_index_oppsex=. if check<`r(max)'   
	replace E_Sbehavior_index_oppsex=E_Sbehavior_index_oppsex/`r(max)'
	drop check
	label var E_Sbehavior_index_oppsex "Behavior sub-index (Interaction with the opp sex)"


**weighted behavior index

	local vars
	foreach var in `behavior_oppsex' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E_Sbehavior_index2_oppsex_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E_Sbehavior_index_oppsex "Unweighted Interaction with the Opposite Sex Sub-Index"
	lab var E_Sbehavior_index2_oppsex_i "Interaction with the Opposite Sex Sub-Index"

** weighted not-imputed
	
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		
		local i=0
			foreach x in `behavior_oppsex' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp  // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw) // sum of weights of variables to keep
		
		** weighting the variables		
		egen E_Sbehavior_index2_oppsex_ni=rsum2(sum_*) , anymiss
		replace E_Sbehavior_index2_oppsex_ni = E_Sbehavior_index2_oppsex_ni*(C[1,1]/keep_sum) 
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var E_Sbehavior_index2_oppsex_ni "Interaction with the opposite sex"	


**3.B Participation in hh chores (boys and girls)

	local behavior_hhchores E_Scook_clean_comm E_Sabsent_sch_hhwork_comm
	
	* flags for missing values
	global el_behavior_hhchores_flag
	foreach var in `behavior_hhchores' {
		global el_behavior_hhchores_flag $el_behavior_hhchores_flag `var'_flag
		}

	cap drop check
	egen E_Sbehavior_index_hhchores=rowtotal(`behavior_hhchores'), mi
	egen check=rownonmiss(`behavior_hhchores')

	tab check
	qui sum check
	replace E_Sbehavior_index_hhchores=. if check<`r(max)'   
	replace E_Sbehavior_index_hhchores=E_Sbehavior_index_hhchores/`r(max)'
	drop check
	label var E_Sbehavior_index_hhchores "Behavior sub-index (Participation in HH Chores)"


**weighted behavior index

	local vars
	foreach var in `behavior_hhchores' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E_Sbehavior_index2_hhchores_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E_Sbehavior_index_hhchores "Unweighted Participation in HH Chores Sub-Index"
	lab var E_Sbehavior_index2_hhchores_i "Participation in HH Chores Sub-Index"

** weighted not-imputed
	
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		
		local i=0
			foreach x in `behavior_hhchores' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp  // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw) // sum of weights of variables to keep
		
		** weighting the variables		
		egen E_Sbehavior_index2_hhchores_ni=rsum2(sum_*) , anymiss
		replace E_Sbehavior_index2_hhchores_ni = E_Sbehavior_index2_hhchores_ni*(C[1,1]/keep_sum) 
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var E_Sbehavior_index2_hhchores_ni "Participation in HH chores"	



**3.C Supporting female relatives' ambitions (boys and girls)

	local behavior_relatives E_Sdiscourage_college_comm E_Sdiscourage_work_comm
	
	* flags for missing values
	global el_behavior_relatives_flag
	foreach var in `behavior_relatives' {
		global el_behavior_relatives_flag $el_behavior_relatives_flag `var'_flag
		}
		
	** average
	cap drop check
	egen E_Sbehavior_index_relatives=rowtotal(`behavior_relatives'), mi
	egen check=rownonmiss(`behavior_relatives')

	tab check
	qui sum check
	replace E_Sbehavior_index_relatives=. if check<`r(max)'   
	replace E_Sbehavior_index_relatives=E_Sbehavior_index_relatives/`r(max)'
	drop check
	label var E_Sbehavior_index_relatives "Behavior sub-index (Supporting Female Relatives' Ambitions)"

**weighted behavior index

	local vars
	foreach var in `behavior_relatives' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E_Sbehavior_index2_relatives_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E_Sbehavior_index_relatives "Unweighted Supporting Female Relatives' Ambitions Sub-Index"
	lab var E_Sbehavior_index2_relatives_i "Supporting Female Relatives' Ambitions Sub-Index"

** weighted not-imputed
	
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		
		local i=0
			foreach x in `behavior_relatives' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp  // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw) // sum of weights of variables to keep
		
		** weighting the variables		
		egen E_Sbehavior_index2_relatives_ni=rsum2(sum_*) , anymiss
		replace E_Sbehavior_index2_relatives_ni = E_Sbehavior_index2_relatives_ni*(C[1,1]/keep_sum) 
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var E_Sbehavior_index2_relatives_ni "Supporting female relatives' ambitions"	
	


**3.D Decision-making (girls only)

	local behavior_decision_girl E_Sfuture_work_g E_Sdecision_past_tenth_g E_Sdecision_work_g E_Sdecision_kindofwork_g E_Sabsent_sch_g

	* flags for missing values
	global el_behavior_decision_girl_flag
	foreach var in `behavior_decision_girl' {
		global el_behavior_decision_girl_flag $el_behavior_decision_girl_flag `var'_flag
		}	
		
	cap drop check
	egen E_Sbehavior_index_decision_g=rowtotal(`behavior_decision_girl') if B_Sgirl==1 & attrition_el==0, mi
	egen check=rownonmiss(`behavior_decision_girl') if B_Sgirl==1 & attrition_el==0

	tab check
	qui sum check
	replace E_Sbehavior_index_decision_g=. if check<`r(max)' & B_Sgirl==1 & attrition_el==0   
	replace E_Sbehavior_index_decision_g=E_Sbehavior_index_decision_g/`r(max)' if B_Sgirl==1 & attrition_el==0
	drop check
	label var E_Sbehavior_index_decision_g "Girls' Behavior index (Decision-making)"


**weighted behavior index

	local vars
	foreach var in `behavior_decision_girl' {
			gen `var'_temp=`var' if B_Sgirl==1 
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==1 & attrition_el==0
			}
			
	egen E_Sbehavior_index2_dec_g_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E_Sbehavior_index_decision_g "Girls' Unweighted Decision-making Sub-Index"
	lab var E_Sbehavior_index2_dec_g_i "Girls' Decision-making Sub-Index"

** weighted not-imputed
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore
		
		local i=0
			foreach x in `behavior_decision_girl' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl  // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw) if B_Sgirl // sum of weights of variables to keep
		
		** weighting the variables		
		egen E_Sbehavior_index2_dec_g_ni=rsum2(sum_*) if B_Sgirl, anymiss
		replace E_Sbehavior_index2_dec_g_ni = E_Sbehavior_index2_dec_g_ni*(C[1,1]/keep_sum) 
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 			
			
	lab var E_Sbehavior_index2_dec_g_ni "Girls' decision-making"	

**3.E Mobility (girls only)

	gen E_Sbehavior_index_mobility_g = E_Salone_friend_g if E_Salone_friend_g_flag==0 & B_Sgirl==1
	gen E_Sbehavior_index2_mobil_g_i = E_Salone_friend_g if E_Salone_friend_g_flag==0 & B_Sgirl==1
	gen E_Sbehavior_index2_mobil_g_ni = E_Salone_friend_g if E_Salone_friend_g_flag==0 & B_Sgirl==1

	lab var E_Sbehavior_index_mobility_g "Girls' Unweighted Mobility Sub-Index"
	lab var E_Sbehavior_index2_mobil_g_i "Girls' Mobility (Dummy)"
	lab var E_Sbehavior_index2_mobil_g_ni "Girls' mobility"

***4. Gender_based discrimination Index

**Average gender_based discrimination index

	rename E_Sfemale_foeticide_reason_y E_Sfemale_foeticide_r_y

	local discrimination E_Sfemale_foeticide E_Sfemale_foeticide_state E_Sfemale_foeticide_r_y E_Sgirls_less_y
	
	local discrimination_flag
	
	foreach y in `discrimination' {
			cap confirm variable `y'_flag
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
		qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition_el==0
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local discrimination_flag `discrimination_flag' `y'_flag
				
			}	
		}		

	global el_discrimination_flag `discrimination_flag'
	
	cap drop check
	egen E_Sdiscrimination_index=rowtotal(`discrimination'), mi
	egen check=rownonmiss(`discrimination')

	tab check
	qui sum check
	replace E_Sdiscrimination_index=. if check<`r(max)'   
	replace E_Sdiscrimination_index=E_Sdiscrimination_index/`r(max)'
	drop check
	label var E_Sdiscrimination_index "Gender-based discrimination index"


**Weighted gender based discrimination index


	local vars
	foreach var in `discrimination' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E_Sdiscrimination_index2_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E_Sdiscrimination_index "Unweighted Gender-based Discrimination Index"
	lab var E_Sdiscrimination_index2_i "Gender-based Discrimination Index"
	
	
** weighted not-imputed
	
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		
		local i=0
			foreach x in `discrimination' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0  // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp  // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw) // sum of weights of variables to keep
		
		** weighting the variables		
		egen E_Sdiscrimination_index2_ni=rsum2(sum_*) , anymiss
		replace E_Sdiscrimination_index2_ni = E_Sdiscrimination_index2_ni*(C[1,1]/keep_sum) 
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 			
			
	lab var E_Sdiscrimination_index2_ni "Awareness of gender-based discrimination"	


***5. Self-esteem index

**Average self-esteem index

	local esteem E_Ssatisfy_y E_Sgood_qly_y E_Sable_do_most_y
	
	local esteem_flag
	
	foreach y in `esteem' {
			cap confirm variable `y'_flag
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
		qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition_el==0
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local esteem_flag `esteem_flag' `y'_flag
				
			}	
		}	
	global el_esteem_flag `esteem_flag'

	cap drop check
	egen E_Sesteem_index=rowtotal(`esteem'), mi
	egen check=rownonmiss(`esteem')

	tab check
	qui sum check
	replace E_Sesteem_index=. if check<`r(max)'   
	replace E_Sesteem_index=E_Sesteem_index/`r(max)'
	drop check
	label var E_Sesteem_index "Self-esteem index"
	
	
**Weighted self-esteem index


	local vars
	foreach var in `esteem' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
 	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A

	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E_Sesteem_index2=rsum2(sum_*), anymiss
			
	cap drop *_temp
	cap drop sum_*
			
	lab var E_Sesteem_index "Unweighted Self-esteem Index"
	lab var E_Sesteem_index2 "Self-esteem Index"


***Boys' self-esteem index

	* flags for missing values
	global el_esteem_boy_flag
	foreach var in `esteem' {
		global el_esteem_boy_flag $el_esteem_boy_flag `var'_flag
		}	

	**average

	cap drop check
	egen E_Sesteem_index_boy=rowtotal(`esteem') if B_Sgirl==0 & attrition_el==0, mi 
	egen check=rownonmiss(`esteem') if B_Sgirl==0 & attrition_el==0

	tab check
	qui sum check
	replace E_Sesteem_index_boy=. if check<`r(max)' & B_Sgirl==0 & attrition_el==0
	replace E_Sesteem_index_boy=E_Sesteem_index_boy/`r(max)' if B_Sgirl==0 & attrition_el==0
	drop check
	label var E_Sesteem_index_boy "Boys' Self-esteem Index"
	
	
**Weighted boys' self-esteem index
	
	local vars
	foreach var in `esteem' {
			gen `var'_temp=`var' if B_Sgirl==0 
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}

	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==0 & attrition_el==0
			}
			
	egen E_Sesteem_index2_boy=rsum2(sum_*), anymiss
			
	cap drop *_temp
	cap drop sum_*
			
	lab var E_Sesteem_index_boy "Unweighted Boys' Self-esteem Index"
	lab var E_Sesteem_index2_boy "Boys' Self-esteem Index"


***Girls' self-esteem index

	* flags for missing values
	global el_esteem_girl_flag
	foreach var in `esteem' {
		global el_esteem_girl_flag $el_esteem_girl_flag `var'_flag
		}	
		
	** average
	cap drop check
	egen E_Sesteem_index_girl=rowtotal(`esteem') if B_Sgirl==1 & attrition_el==0, mi 
	egen check=rownonmiss(`esteem') if B_Sgirl==1 & attrition_el==0

	tab check
	qui sum check
	replace E_Sesteem_index_girl=. if check<`r(max)' & B_Sgirl==1 & attrition_el==0
	replace E_Sesteem_index_girl=E_Sesteem_index_girl/`r(max)' if B_Sgirl==1 & attrition_el==0
	drop check
	label var E_Sesteem_index_girl "Girls' Self-esteem Index"
	
	
**Weighted girls' self-esteem index
	
	local vars
	foreach var in `esteem' {
			gen `var'_temp=`var' if B_Sgirl==1 
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}

	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==1 & attrition_el==0
			}
			
	egen E_Sesteem_index2_girl_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E_Sesteem_index_girl "Unweighted Girls' Self-esteem Index"
	lab var E_Sesteem_index2_girl_i "Girls' Self-esteem Index"
	
** weighted (not-imputed)
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		restore 
		
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		
		local i=0
			foreach x in `esteem' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==1 & attrition_el==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  if B_Sgirl==1 & attrition_el==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  B_Sgirl==1 & attrition_el==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E_Sesteem_index2_girl_ni=rsum2(sum_*) if B_Sgirl==1 & attrition_el==0, anymiss
		replace E_Sesteem_index2_girl_ni = E_Sesteem_index2_girl_ni*(C[1,1]/keep_sum)			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E_Sesteem_index2_girl_ni "Girls' self-esteem"	



	*****************************************************
	** SOCIAL NORMS: SET 1 and 2 (WORK and EDUCATION)
	*****************************************************
	local social E_Sallow_work E_Scommunity_allow_work E_Sallow_college E_Scommunity_allow_college

	foreach x in `social' {
	gen `x'_s=inlist(`x',1) if !mi(`x')
	}

	local social_flag	
	local social2 E_Sallow_work_s E_Scommunity_allow_work_s E_Sallow_college_s E_Scommunity_allow_college_s

	gen E_Scommunity_work_s=inlist(E_Sallow_work,1) & inlist(E_Scommunity_allow_work,0) & inlist(E_Soppose_allow_work,0) ///
			if !mi(E_Sallow_work) & !mi(E_Scommunity_allow_work) & !mi(E_Soppose_allow_work)
	replace E_Scommunity_work_s=1 if E_Sallow_work==1 & E_Scommunity_allow_work==1
	replace E_Scommunity_work_s=0 if E_Sallow_work==0 & E_Scommunity_allow_work==0	
	
	gen E_Scommunity_college_s=inlist(E_Sallow_college,1) & inlist(E_Scommunity_allow_college,0) & inlist(E_Soppose_allow_college,0) ///
			if !mi(E_Sallow_college) & !mi(E_Scommunity_allow_college) & !mi(E_Soppose_allow_college)
	replace E_Scommunity_college_s=1 if E_Sallow_college==1 & E_Scommunity_allow_college==1
	replace E_Scommunity_college_s=0 if E_Sallow_college==0 & E_Scommunity_allow_college==0
	
	la var E_Sallow_work_s "women should be allowed to work"
	la var E_Scommunity_allow_work_s "community thinks women should be allowed to work"
	la var E_Scommunity_work_s "women should be allowed to work and thinks community will not oppose them"
	la var E_Sallow_college_s "women should be allowed to study in college even if it is far away"
	la var E_Scommunity_allow_college_s "community thinks women should be allowed to study in college even if it is far away"
	la var E_Scommunity_college_s "women should be allowed to study in college and thinks community will not oppose them"
	
	
** D measure

// higher D-measure indicate implicit preference for boys

	gen E_D_measure_neg=E_D_measure*(-1)
	label var E_D_measure "Implicit preference for boys"
	label var E_D_measure_neg "Implicit preference for girls"
	
	rename E_iat_boy_good_first E_iat_boygood
	label var E_iat_boygood "Boy/good first"

	rename E_iat_boy_prof_first E_iat_boyprof
	label var E_iat_boyprof "Boy/Professional task first"
	
	
	** changing index value to missing if half or more variables missing
	
	local E_Sgender_index2_i gender
	local E_Sgender_index2_ni gender
	
	local E_Saspiration_index2_i aspiration
	local E_Saspiration_index2_ni aspiration
	local E_Saspiration_boy_index2_i aspiration_boy
	local E_Saspiration_boy_index2_ni aspiration_boy
	
	local E_Sbehavior_index2_g_i behavior_girl
	local E_Sbehavior_index2_b_i behavior_boy 
	local E_Sbehavior_index2_i behavior_common
	local E_Sbehavior_index2_g_ni behavior_girl
	local E_Sbehavior_index2_b_ni behavior_boy 
	local E_Sbehavior_index2_ni behavior_common	
	
	local E_Sgender_index2_educ educ
	local E_Sgender_index2_emp emp
	local E_Sgender_index2_sub sub
	local E_Sbehavior_index2_oppsex behavior_oppsex
	local E_Sbehavior_index2_hhchores behavior_hhchores
	local E_Sbehavior_index2_relatives behavior_relatives
	local E_Sbehavior_index2_dec_g behavior_decision_girl
	local E_Sdiscrimination_index2 discrimination
	local E_Sesteem_index2 esteem
	
	
	/* no drops for indices using new variables */	
	
		
	foreach index in E_Sgender_index2_i E_Sgender_index2_ni E_Saspiration_index2_i E_Saspiration_index2_ni ///
	E_Sbehavior_index2_g_i E_Sbehavior_index2_b_i E_Sbehavior_index2_i E_Sbehavior_index2_g_ni E_Sbehavior_index2_b_ni E_Sbehavior_index2_ni {
	
		rename `index' `index'_nd  // index with no drop
		gen `index'_d = `index'_nd 
		
		local ``index''_n: word count ${el_``index''_flag2}  // total no. of variables 		
		
		di "${el_``index''_flag2}"		
		di "```index''_n'"
		
		tempvar num_miss
		egen `num_miss' = rowtotal(${el_``index''_flag2}) // number of missing variables
	
		count if !mi(`index'_d) // before replacement count
			local ``index''_bef: di %7.0f r(N)
		
		replace `index'_d = . if (`num_miss'/```index''_n')>=.5 // if >=50% of variables missing, index=.
			
		count if !mi(`index'_d) // after replacement count
		local ``index''_aft: di %7.0f r(N)		
	
		}
		

* shortening lables for old version
foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}

********************************************************************************
				*		ENDLINE 2 PRIMARY OUTCOMES: INDICES            *
********************************************************************************

/*
NOTE: Globals with suffix "flag" are to be used as controls in regressions. 
Globals with suffix "flag2" is to help decide how many valid missings there 
are in the data to decide whether to impute the index or not

*/
		**************************************************************
					*		1. Gender Attitudes            *
		**************************************************************

**** 1.1 Generating flags for missing values and imputing with gender-dist-treat means ****

	#delimit ;
	  local gender 
	E2_Swives_less_edu_n E2_Sboy_more_oppo_n E2_Stown_studies_y 
	E2_Swoman_role_home_n E2_Smen_better_suited_n E2_Smarriage_more_imp_n 
		E2_Steacher_suitable_n E2_Sallow_work
	E2_Ssimilar_right_y	E2_Select_woman_y E2_Sman_final_deci_n E2_Swoman_viol_n 	
		E2_Scontrol_daughters_n E2_Sstudy_marry 
		E2_Sgirl_marriage_age_19 E2_Smarriage_age_diff_m
	E2_Sfertility;
	
	#delimit cr
	
		global el2_gender_flag2
	
		foreach y in `gender' {
		
			assert `y'!=. if attrition==0
			cap confirm variable `y'_flag2
			assert _rc!=0 // flag2 should not exist in data 
			
	
				gen `y'_flag2=inlist(`y', .d, .r, .i, .f, .e) if attrition==0
				global el2_gender_flag2 $el2_gender_flag2 `y'_flag2
				
		
		}		


	local gender_flag
	
		foreach y in `gender' {
			cap confirm variable `y'_flag, exact
				di "`_rc'"
			
			* if flag already exisits
			if !_rc{
				replace `y'=. if `y'_flag // change variable value to missing and drop flag
				drop `y'_flag
			}	
			
			count 
				local total = `r(N)'

			qui count if mi(`y') // all missing, including cases of attrition

			if r(N)!=0 {
				gen `y'_flag=mi(`y') if attrition==0
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local gender_flag `gender_flag' `y'_flag
				}
				
		}		
		
	global el2_gender_flag `gender_flag'
	

********************* 1.2 Average Gender Attitudes index ***************************
	
	#delimit ;
	  local gender 
	E2_Swives_less_edu_n E2_Sboy_more_oppo_n E2_Stown_studies_y 
	E2_Swoman_role_home_n E2_Smen_better_suited_n E2_Smarriage_more_imp_n 
		E2_Steacher_suitable_n E2_Sallow_work
	E2_Ssimilar_right_y	E2_Select_woman_y E2_Sman_final_deci_n E2_Swoman_viol_n 	
		E2_Scontrol_daughters_n E2_Sstudy_marry 
		E2_Sgirl_marriage_age_19 E2_Smarriage_age_diff_m
	E2_Sfertility;
	
	#delimit cr

	cap drop check
	egen E2_Sgender_index=rowtotal(`gender'), mi
	egen check=rownonmiss(`gender')
	
	tab check
	qui sum check
	replace E2_Sgender_index=. if check<`r(max)' // change index value to missing if even one variable is missing
	replace E2_Sgender_index=E2_Sgender_index/`r(max)'
	drop check
	label var E2_Sgender_index "Student gender index"
	
	
	//factor 
	local gender E2_Swives_less_edu_n E2_Sboy_more_oppo_n E2_Stown_studies_y E2_Swoman_role_home_n E2_Smen_better_suited_n E2_Smarriage_more_imp_n 	E2_Steacher_suitable_n E2_Sallow_work E2_Ssimilar_right_y E2_Select_woman_y E2_Sman_final_deci_n E2_Swoman_viol_n E2_Scontrol_daughters_n E2_Sstudy_marry E2_Sgirl_marriage_age_19 E2_Smarriage_age_diff_m E2_Sfertility
	
	factor `gender' if B_treat==0  , factors(1)
	predict E2_Sgender_index_factor /* if !mi(E_Steam_id) & attrition_el==0 */
	sum E2_Sgender_index_factor   if B_treat==0 /* & !mi(E_Steam_id) & attrition_el==0 */
	replace E2_Sgender_index_factor = (E2_Sgender_index_factor - r(mean) )/r(sd)
	

********************* 1.2 Weighted Gender Attitudes index **********************
* with EL2 weights + imputed
	local gender E2_Swives_less_edu_n E2_Sboy_more_oppo_n E2_Stown_studies_y E2_Swoman_role_home_n E2_Smen_better_suited_n E2_Smarriage_more_imp_n 	E2_Steacher_suitable_n E2_Sallow_work E2_Ssimilar_right_y E2_Select_woman_y E2_Sman_final_deci_n E2_Swoman_viol_n E2_Scontrol_daughters_n E2_Sstudy_marry E2_Sgirl_marriage_age_19 E2_Smarriage_age_diff_m E2_Sfertility
	
	local vars
	foreach var in `gender' {
			gen `var'_temp=`var'
			
			sum `var'_temp
				local mean=r(mean) 
				
			sum `var'_temp if B_treat==0
			
			* normalizing variables
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			
			}
	
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

				
	local i=0
		foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
		}
		
		egen E2_Sgender_index2_i_e2=rsum2(sum_*), anymiss

	cap drop *_temp
	cap drop sum_*
		
	lab var E2_Sgender_index "Unweighted Gender Attitudes Index"
	lab var E2_Sgender_index2_i_e2 "Gender Attitudes Index"
	

	
********************* 1.2 EL1 Weighted Gender Attitudes index **********************
* EL1 weigths + imputed		
		// 1/8/2020 addition - reweighted index with EL1 weights
	
	

	* matching EL1 and El2 vars
	
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
		
	local el1_wts
	
	local i=0
	local sum = 0
	foreach x in `gender' {
		local ++i
	
		if "`x'" != "E2_Sshy" &  "`x'" != "E2_Slaugh" {
			local pos strpos("${el1_wts}","``x''") + length("``x''") // position from where the required stat starts
			local l = strlen("${el1_wts}")	// length of entire string	
			local string = substr("${el1_wts}", `pos'-`l'-1, . ) // removing the part of string before the relevant weight
			local wt: word 1 of `string' // weight extracted
			}
			
		else {
		
		local wt = 0 
		
			}
		
		local wt_`i' = `wt'
		
		local sum = `sum' + `wt_`i''

		}
		
	
	
	local vars
	foreach var in `gender' {
	
			gen `var'_temp=`var'
			
			local e1 = "``var''" //JAKE: using EL1 to normalize instead of EL2

			sum `e1'
				local mean=r(mean) 
				
			sum `e1' if B_treat==0
			
			* normalizing variables
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			
			}
			
			di `vars'
	

	local i=0		
	foreach x in `vars' {
		local ++i	
	
		gen sum_`i' = `wt_`i''*`x' // wt * variable value
		
		}

		
		egen E2_Sgender_index2_i_e1=rsum2(sum_*), anymiss

	cap drop sum_*
	
	lab var E2_Sgender_index2_i_e1 "Gender Attitudes Index (EL1 weights)"

	
	
****** 1.3a Weighted Gender Attitudes index (without imputing and using EL1 weights) ********

		local i=0
			foreach x in `gender' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * `wt_`i''  if attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			}
			
					
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if attrition==0 // sum of weights of variables to keep
		
		** weighting the variables
		
		egen E2_Sgender_index2_ni_e1=rsum2(sum_*) if attrition==0, anymiss
		replace E2_Sgender_index2_ni_e1 = E2_Sgender_index2_ni_e1*(`el1_sum'/keep_sum)
			
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum
	cap drop *_temp 			//Dropping temp TO renormalize it when using EL2 weights without EL2 statistics not EL1 scalars
			
	lab var E2_Sgender_index2_ni_e1 "Gender Attitudes Index"	
	
****** 1.3a Weighted Gender Attitudes index (without imputing and using EL2 weights) ********
		
		***Recreating temporary variables normalized with Endline2 statistics not Endline1 scalars***
		foreach var in `gender'{
			gen `var'_temp = `var'
			sum `var'_temp
				local mean=r(mean)	
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)' 
		}
		
		
		global el2_wts " "
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		
		local i=0
			foreach x in `gender' {
			
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  if attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			// 1/21/2020 addition for getting EL2 weights
			
			* storing EL1 weights
			local wt: di B[`i',1]
			global el2_wts ${el2_wts} `x'`wt'
			}
		
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  attrition==0 // sum of weights of variables to keep

		local el2_sum: di C[1,1] // sum of EL2 weights
					
		** weighting the variables
		
		egen E2_Sgender_index2_ni_e2=rsum2(sum_*) if attrition==0, anymiss
		replace E2_Sgender_index2_ni_e2 = E2_Sgender_index2_ni_e2*(C[1,1]/keep_sum)
			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
		
	lab var E2_Sgender_index2_ni_e2 "Endline 2 Gender Attitudes Index"
	
* table comparing EL1 and EL2 weights
la var E2_Stown_studies_y "If HH head, would send both children or girl for educ"
la var E2_Sallow_work "Women should be allowed to work outside home"

		cap file close sumstat
		file open sumstat using "$tables/tab_att_wt_compare.tex", write replace

		file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \begin{tabular}{@{\extracolsep{3pt}}{l}*{4}{>{\centering\arraybackslash}m{2cm}}@{}}   \toprule" _n      // table header
		file write sumstat " Variable & EL1 wt. & EL2 wt. & \specialcell{EL1 \% of \\ total wt.} & \specialcell{EL2 \% of \\ total wt.}   \\     " _n         

		file write sumstat "\midrule" _n
		
		foreach x in `gender' {
		
			local varlab: variable label `x'
			
			* EL1 weight
			local pos strpos("${el1_wts}","``x''") + length("``x''") // position from where the required stat starts
			local l = strlen("${el1_wts}")	// length of entire string	
			local string = substr("${el1_wts}", `pos'-`l'-1, . ) // removing the part of string before the relevant weight
			local wt1: word 1 of `string' // weight extracted
			local wt1: di %5.3f float(`wt1')			
	
			* EL1 weight proportion			
			local wt1_prop = (`wt1'/`el1_sum')*100
			local wt1_prop: di %5.2f  `wt1_prop'
			
			* EL2 weight
			local pos strpos("${el2_wts}","`x'") + length("`x'") // position from where the required stat starts
			local l = strlen("${el2_wts}")	// length of entire string	
			local string = substr("${el2_wts}", `pos'-`l'-1, . ) // removing the part of string before the relevant weight
			local wt2: word 1 of `string' // weight extracted
			local wt2: di %5.3f float(`wt2')
			
			* EL2 weight proportion			
			local wt2_prop = (`wt2'/`el2_sum')*100
			local wt2_prop: di %5.2f  `wt2_prop'
			
			if abs(`wt2_prop' - `wt1_prop') >= 2 {
				file write sumstat "\textbf{`varlab'} & `wt1' & `wt2' & `wt1_prop' & `wt2_prop' \\     " _n  
				}
			
			else {
				file write sumstat "`varlab'& `wt1' & `wt2' & `wt1_prop' & `wt2_prop' \\     " _n  
				}

		
			}
			
	file write sumstat "\bottomrule" _n           // table footer
	file write sumstat "\end{tabular}" _n 
	file close sumstat
	
*************** 1.1a Average Gender Attitudes Education index ********************
		
local educ E2_Swives_less_edu_n E2_Sboy_more_oppo_n E2_Stown_studies_y

	* flags for missing values
	global el2_educ_flag
	global el2_educ_flag2
	foreach var in `educ' {
		global el2_educ_flag $el2_educ_flag `var'_flag
		global el2_educ_flag2 $el2_educ_flag2 `var'_flag2
		}
		
		
		
	cap drop check
	egen E2_Sgender_index_educ=rowtotal(`educ'), mi
	egen check=rownonmiss(`educ')

	tab check
	qui sum check
	replace E2_Sgender_index_educ=. if check<`r(max)'   
	replace E2_Sgender_index_educ=E2_Sgender_index_educ/`r(max)'
	drop check
	label var E2_Sgender_index_educ "Student gender index (Education)"

*************** 1.2a Weighted Gender Attitudes Education index ********************

	local vars
	foreach var in `educ' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			}
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

				
	local i=0
		foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
		}
		
		egen E2_Sgender_index2_educ_i=rsum2(sum_*), anymiss
		
	cap drop sum_*		
	
	lab var E2_Sgender_index_educ "Unweighted Education Attitudes Sub-Index"
	lab var E2_Sgender_index2_educ_i "Education Attitudes Sub-Index"
	
** weighted (not-imputed) and using el1 weights
		
		local sum = 0
		local i=0
			foreach x in `educ' {
			local ++i
			local pos strpos("${el1_educ_wts}","``x''") + length("``x''") // position from where the required stat starts
			local l = strlen("${el1_educ_wts}")	// length of entire string	
			local string = substr("${el1_educ_wts}", `pos'-`l'-1, . ) // removing the part of string before the relevant weight
			local wt: word 1 of `string' // weight extracted
				
		else {		
		local wt = 0 		
			}
		
		local wt_`i' = `wt'
		
		local sum = `sum' + `wt_`i''	// overall sum of weights
			
			gen `x'_nmi = `x'_flag==0 if attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * `wt_`i''   if attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		
		* sum of weights of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  attrition==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E2_Sgender_index2_educ_ni=rsum2(sum_*) if attrition==0, anymiss
		replace E2_Sgender_index2_educ_ni = E2_Sgender_index2_educ_ni*(`sum'/keep_sum)			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E2_Sgender_index2_educ_ni "Education Attitudes"	



*************** 1.1b Average Gender Attitudes Employment index ********************

local emp E2_Swoman_role_home_n E2_Smen_better_suited_n E2_Smarriage_more_imp_n E2_Steacher_suitable_n E2_Sallow_work

	* flags for missing values
	global el2_emp_flag
	global el2_emp_flag2
	foreach var in `emp' {
		global el2_emp_flag $el2_emp_flag `var'_flag
		global el2_emp_flag2 $el2_emp_flag2 `var'_flag2
		}
		
	cap drop check
	egen E2_Sgender_index_emp=rowtotal(`emp'), mi
	egen check=rownonmiss(`emp')

	tab check
	qui sum check
	replace E2_Sgender_index_emp=. if check<`r(max)'   
	replace E2_Sgender_index_emp=E2_Sgender_index_emp/`r(max)'
	drop check
	label var E2_Sgender_index_emp "Student gender index (Employment)"

*************** 1.2b Weighted Gender Attitudes Employment index ********************

	
	local vars
	foreach var in `emp' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			}
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

				
	local i=0
		foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
		}
		
		egen E2_Sgender_index2_emp_i=rsum2(sum_*), anymiss
		
	cap drop sum_*		
		
	lab var E2_Sgender_index_emp "Unweighted Employment Attitudes Sub-Index"
	lab var E2_Sgender_index2_emp_i "Employment Attitudes Sub-Index"

** weighted (not-imputed)

		
		local sum = 0
		local i=0
			foreach x in `emp' {
			local ++i
			local pos strpos("${el1_emp_wts}","``x''") + length("``x''") // position from where the required stat starts
			local l = strlen("${el1_emp_wts}")	// length of entire string	
			local string = substr("${el1_emp_wts}", `pos'-`l'-1, . ) // removing the part of string before the relevant weight
			local wt: word 1 of `string' // weight extracted
				
		else {		
		local wt = 0 		
			}
		
		local wt_`i' = `wt'
		
		local sum = `sum' + `wt_`i''	// overall sum of weights
			
			gen `x'_nmi = `x'_flag==0 if attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * `wt_`i''   if attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		
		* sum of weights of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  attrition==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E2_Sgender_index2_emp_ni=rsum2(sum_*) if attrition==0, anymiss
		replace E2_Sgender_index2_emp_ni = E2_Sgender_index2_emp_ni*(`sum'/keep_sum)			


	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E2_Sgender_index2_emp_ni "Employment Attitudes"	
	
******** 1.1c Average Gender Attitudes Female Gender Roles index ***************

local sub E2_Ssimilar_right_y E2_Select_woman_y E2_Sman_final_deci_n E2_Swoman_viol_n ///	
		E2_Scontrol_daughters_n E2_Sstudy_marry ///
		E2_Sgirl_marriage_age_19 E2_Smarriage_age_diff_m

	* flags for missing values
	global el2_sub_flag
	global el2_sub_flag2
	foreach var in `sub' {
		global el2_sub_flag $el2_sub_flag `var'_flag
		global el2_sub_flag2 $el2_sub_flag2 `var'_flag2
		}
		
	cap drop check
	egen E2_Sgender_index_sub=rowtotal(`sub'), mi
	egen check=rownonmiss(`sub')

	tab check
	qui sum check
	replace E2_Sgender_index_sub=. if check<`r(max)'   
	replace E2_Sgender_index_sub=E2_Sgender_index_sub/`r(max)'
	drop check
	label var E2_Sgender_index_sub "Student gender index (Female gender roles)"

******** 1.2c Weighted Gender Attitudes Female Gender Roles index ***************

	local vars
	foreach var in `sub' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			}
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

				
	local i=0
		foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
		}
		
		egen E2_Sgender_index2_sub_i=rsum2(sum_*), anymiss
		
	cap drop sum_*
		
	lab var E2_Sgender_index_sub "Unweighted Attitudes towards Female Gender Roles Sub-Index"
	lab var E2_Sgender_index2_sub_i "Attitudes towards Female Gender Roles Sub-Index"

** weighted (not-imputed)
		local sum = 0
		local i=0
			foreach x in `sub' {
			local ++i
			local pos strpos("${el1_sub_wts}","``x''") + length("``x''") // position from where the required stat starts
			local l = strlen("${el1_sub_wts}")	// length of entire string	
			local string = substr("${el1_sub_wts}", `pos'-`l'-1, . ) // removing the part of string before the relevant weight
			local wt: word 1 of `string' // weight extracted
				
		else {		
		local wt = 0 		
			}
		
		local wt_`i' = `wt'
		
		local sum = `sum' + `wt_`i''	// overall sum of weights
			
			gen `x'_nmi = `x'_flag==0 if attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * `wt_`i''   if attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		
		* sum of weights of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  attrition==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E2_Sgender_index2_sub_ni=rsum2(sum_*) if attrition==0, anymiss
		replace E2_Sgender_index2_sub_ni = E2_Sgender_index2_sub_ni*(`sum'/keep_sum)			


	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E2_Sgender_index2_sub_ni "Attitudes towards Female Gender Roles"	
	
******** 1d  Gender Attitudes Fertility index ***************

	gen E2_Sgender_index_fert = E2_Sfertility if E2_Sfertility_flag==0
	gen E2_Sgender_index2_fert_i = E2_Sfertility  if E2_Sfertility_flag==0
	gen E2_Sgender_index2_fert_ni = E2_Sfertility  if E2_Sfertility_flag==0
	
	lab var E2_Sgender_index_fert "Unweighted Fertility Attitudes Sub-Index"
	lab var E2_Sgender_index2_fert_i "Fertility Attitudes"
	lab var E2_Sgender_index2_fert_ni "Fertility Attitudes"

		
		**************************************************************
					*		2. Aspirations           *
		**************************************************************

		
	local aspiration E2_Stwel_score_exp_m E2_Sdiscuss_educ E2_Scont_educ E2_Shighest_educ_m ///
	E2_Soccupa_25_y E2_Splan_college E2_Scol_course_want_y E2_Scol_course_want_stem E2_Scont_have_job_y
	
		
		global el2_aspiration_flag2
	
		foreach y in `aspiration' {
		
			assert `y'!=. if attrition==0
			
			cap confirm variable `y'_flag2
			assert _rc!=0 // flag2 should not exist in data 
			
				gen `y'_flag2=inlist(`y', .d, .r, .i, .f, .e) if attrition==0
				global el2_aspiration_flag2 $el2_aspiration_flag2 `y'_flag2
						
			}		

	local aspiration_flag
	
		foreach y in `aspiration' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition==0
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local aspiration_flag `aspiration_flag' `y'_flag
				
			}	
		}
		
global el2_aspiration_flag `aspiration_flag'


********************* 2.1 Average Aspirations index ***************************

	cap drop check
	egen E2_Saspiration_index=rowtotal(`aspiration'), mi
	egen check=rownonmiss(`aspiration')

	tab check if B_Sgirl==1	
	qui sum check if B_Sgirl==1	
	replace E2_Saspiration_index=. if check<`r(max)' &  B_Sgirl==1	
	replace E2_Saspiration_index=E2_Saspiration_index/`r(max)' if B_Sgirl==1	
	drop check
	label var E2_Saspiration_index "Student Aspiration index"

	
	//Factor End line 2 aspiration index
	local aspiration E2_Stwel_score_exp_m E2_Sdiscuss_educ E2_Scont_educ E2_Shighest_educ_m ///
	E2_Soccupa_25_y E2_Splan_college E2_Scol_course_want_y E2_Scol_course_want_stem E2_Scont_have_job_y
	
	factor `aspiration' if B_Sgirl==1 & B_treat==0 /* &  !mi(E_Steam_id) & attrition_el==0 */, factors(1)
	predict E2_Saspiration_index_factor if B_Sgirl==1 /*&  !mi(E_Steam_id) & attrition_el==0 */
	sum E2_Saspiration_index_factor if B_Sgirl==1 & B_treat==0  /*& !mi(E_Steam_id) & attrition_el==0 */
	replace E2_Saspiration_index_factor = (E2_Saspiration_index_factor - r(mean))/r(sd)
	
	
	
********************* 2.2 Weighted Aspirations index ***************************
		
		local vars
		foreach var in `aspiration' {
				gen `var'_temp=`var'
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}

	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E2_Saspiration_index2_i=rsum2(sum_*) if B_Sgirl==1, anymiss
			
	cap drop sum_*
			
	lab var E2_Saspiration_index "Unweighted Aspirations Index"
	lab var E2_Saspiration_index2_i "Aspirations Index"
	
	
	
********************* 2.2a Weighted Aspirations index (without imputing) ***************************

		preserve	
		keep if B_Sgirl==1 
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore
		
		local i=0
			foreach x in `aspiration' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==1 & attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  if B_Sgirl==1 & attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			}
		
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if B_Sgirl==1 & attrition==0 // sum of weights of variables to keep

		
		** weighting the variables
		
		egen E2_Saspiration_index2_ni=rsum2(sum_*) if B_Sgirl==1 & attrition==0, anymiss
		replace E2_Saspiration_index2_ni = E2_Saspiration_index2_ni*(C[1,1]/keep_sum)
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
		
	lab var E2_Saspiration_index2_ni "Endline 2 Aspirations Index"
	
		
		**************************************************************
					*		2. Aspirations  (Boys)         *
		**************************************************************

	local aspiration_boy E2_Stwel_score_exp_m E2_Sdiscuss_educ E2_Scont_educ E2_Shighest_educ_m ///
	E2_Soccupa_25_y E2_Splan_college E2_Scol_course_want_y E2_Scol_course_want_stem 

		
	* flags for missing values
	global el2_aspiration_boy_flag
	global el2_aspiration_boy_flag2
	foreach var in `aspiration_boy' {
		global el2_aspiration_boy_flag $el2_aspiration_boy_flag `var'_flag
		global el2_aspiration_boy_flag2 $el2_aspiration_boy_flag2 `var'_flag2
		}

********************* 2.1 Average Aspirations index ***************************

	cap drop check
	egen E2_Saspiration_boy_index=rowtotal(`aspiration_boy')if attrition==0 & B_Sgirl==0, mi
	egen check=rownonmiss(`aspiration_boy') if attrition==0 & B_Sgirl==0

	tab check if B_Sgirl==0 
	qui sum check if B_Sgirl==0 
	replace E2_Saspiration_boy_index=. if check<`r(max)' & attrition==0 & B_Sgirl==0  
	replace E2_Saspiration_boy_index=E2_Saspiration_boy_index/`r(max)' if attrition==0 & B_Sgirl==0
	drop check
	label var E2_Saspiration_boy_index "Student Aspiration index (Boys)"

********************* 2.2 Weighted Aspirations index ***************************
		
		local vars
		foreach var in `aspiration_boy' {
				gen `var'_temp=`var' if B_Sgirl==0
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}

	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if attrition==0 & B_Sgirl==0
			}
			
	egen E2_Saspiration_boy_index2_i=rsum2(sum_*) if B_Sgirl==0 , anymiss
			
	cap drop sum_*
			
	lab var E2_Saspiration_boy_index "Unweighted Aspirations Index (Boys)"
	lab var E2_Saspiration_boy_index2_i "Aspirations Index (Boys)"
	
		****** 2.1a Weighted Aspirations index - boys - (without imputing) ********
		
		* limit to boys
		preserve
		keep if B_Sgirl==0
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `aspiration_boy' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==0 // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if B_Sgirl==0 // sum of weights of variables to keep
		
		** weighting the variables		
		egen E2_Saspiration_boy_index2_ni=rsum2(sum_*) if B_Sgirl==0, anymiss
		replace E2_Saspiration_boy_index2_ni = E2_Saspiration_boy_index2_ni*(C[1,1]/keep_sum) if B_Sgirl==0
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var E2_Saspiration_boy_index2_ni "Endline 2 Aspirations Index (Boys)"	
	
		**************************************************************
						*		3. Behavior           *
		**************************************************************

	**imputing values	
		
	local behavior_girl E2_Sfuture_work E2_Sdec_past_twel_y E2_Sdecision_work_y E2_Sdecision_kindofwork_y E2_Sattendance ///
		E2_Salone_friend  E2_Salone_market E2_Salone_events E2_Salone_past_week E2_Salone_school_y ///	
		E2_Stalk_opp_gen_g E2_Ssit_opp_gen_g E2_Sfriend_opp_gen_g E2_Splay_opp_gen_g ///
		E2_Stalk_week_opp_gen_g E2_Scook_clean_g E2_Sabsent_sch_hhwork_g E2_Sdisc_col_g E2_Sdisc_work_g
		
		
	
	local behavior_boy E2_Stalk_opp_gen_b E2_Ssit_opp_gen_b E2_Sfriend_opp_gen_b ///
		E2_Splay_opp_gen_b E2_Stalk_week_opp_gen_b E2_Scook_clean_b E2_Sabsent_sch_hhwork_b ///
		E2_Sdisc_col_b E2_Sdisc_work_b
		
		foreach var in `behavior_girl' `behavior_boy' {
			count if `var'==.
				di "`var': missing `r(N)'" //JAKE edit			
		
			}

		global el2_behavior_girl_flag2
	

	
		foreach y in `behavior_girl' {
			
			assert `y'!=. if attrition==0
			
			cap confirm variable `y'_flag2
			assert _rc!=0 // flag2 should not exist in data 
			
				gen `y'_flag2=inlist(`y',  .d, .r, .i, .f, .e) if attrition==0
				global el2_behavior_girl_flag2 $el2_behavior_girl_flag2 `y'_flag2
				
		}

	local behavior_girl_flag
			
	foreach y in `behavior_girl' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition==0 & B_Sgirl==1 // NOTE: check gender variable
				bysort B_Sdistrict B_treat: egen x = mean (`y') if B_Sgirl==1
				qui replace `y'=x if `y'_flag==1
				drop x
				local behavior_girl_flag `behavior_girl_flag' `y'_flag
				
			}	
		}
		
	global el2_behavior_girl_flag `behavior_girl_flag'
	
	global el2_behavior_boy_flag2
	
		foreach y in `behavior_boy' {
		
			assert `y'!=. if attrition==0
			
			cap confirm variable `y'_flag2
			assert _rc!=0 // flag2 should not exist in data 
			
				gen `y'_flag2=inlist(`y', .d, .r, .i, .f, .e) if attrition==0
				global el2_behavior_boy_flag2 $el2_behavior_boy_flag2 `y'_flag2
				
			
		}

	local behavior_boy_flag
			
	foreach y in `behavior_boy' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition==0 & B_Sgirl==0
				bysort B_Sdistrict B_treat: egen x = mean (`y') if B_Sgirl==0
				qui replace `y'=x if `y'_flag==1
				drop x
				local behavior_boy_flag `behavior_boy_flag' `y'_flag
				
			}	
		}		

	global el2_behavior_boy_flag `behavior_boy_flag'

	
	// Factor behavior index:
	
	local behavior_girl E2_Sfuture_work E2_Sdec_past_twel_y E2_Sdecision_work_y E2_Sdecision_kindofwork_y E2_Sattendance ///
		E2_Salone_friend  E2_Salone_market E2_Salone_events E2_Salone_past_week E2_Salone_school_y ///	
		E2_Stalk_opp_gen_g E2_Ssit_opp_gen_g E2_Sfriend_opp_gen_g E2_Splay_opp_gen_g ///
		E2_Stalk_week_opp_gen_g E2_Scook_clean_g E2_Sabsent_sch_hhwork_g E2_Sdisc_col_g E2_Sdisc_work_g
		factor `behavior_girl' if B_Sgirl==1, factors(1)
		predict behavior_index_factor_g if B_Sstudent_gender==2
	
	local behavior_boy E2_Stalk_opp_gen_b E2_Ssit_opp_gen_b E2_Sfriend_opp_gen_b ///
		E2_Splay_opp_gen_b E2_Stalk_week_opp_gen_b E2_Scook_clean_b E2_Sabsent_sch_hhwork_b ///
		E2_Sdisc_col_b E2_Sdisc_work_b
		factor `behavior_boy' if B_Sgirl==0, factors(1)
		predict E2_Sbehavior_index_factor_b if B_Sstudent_gender==1 

		
******************* 3.1 Average  Girls' Behavior Index *************************

	cap drop check
	egen E2_Sbehavior_index_g=rowtotal(`behavior_girl') if attrition==0 & B_Sgirl==1, mi
	egen check=rownonmiss(`behavior_girl') if attrition==0 & B_Sgirl==1

	tab check
	qui sum check
	replace E2_Sbehavior_index_g=. if check<`r(max)'  & attrition==0 & B_Sgirl==1
	replace E2_Sbehavior_index_g=E2_Sbehavior_index_g/`r(max)' if attrition==0 & B_Sgirl==1
	drop check
	label var E2_Sbehavior_index_g "Girls' Behavior index"
	
	
******************* 3.2 Weighted Girls' Behavior Index *************************
		
		
		local vars
		foreach var in `behavior_girl' {
				gen `var'_temp=`var' if B_Sgirl==1
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if attrition==0 & B_Sgirl==1
			}
			
	egen E2_Sbehavior_index2_g_i=rsum2(sum_*), anymiss
			
	cap drop sum_*			
	
	lab var E2_Sbehavior_index_g "Girls' Unweighted Behavior Index"
	lab var E2_Sbehavior_index2_g_i "Girls' Behavior Index"
	
			****** 2.1a Weighted behavior index - girls - (without imputing) ********
		
		* limit to boys
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `behavior_girl' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==1 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==1 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==1 // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if B_Sgirl==1 // sum of weights of variables to keep
		
		** weighting the variables		
		egen E2_Sbehavior_index2_g_ni=rsum2(sum_*) if B_Sgirl==1, anymiss
		replace E2_Sbehavior_index2_g_ni = E2_Sbehavior_index2_g_ni*(C[1,1]/keep_sum) if B_Sgirl==1
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var E2_Sbehavior_index2_g_ni "Endline 2 Girls' Behavior Index"	



******************* 3.3 Average Boys' Behavior Index *************************

	cap drop check
	egen E2_Sbehavior_index_b=rowtotal(`behavior_boy') if attrition==0 & B_Sgirl==0, mi
	egen check=rownonmiss(`behavior_boy') if attrition==0 & B_Sgirl==0

	tab check
	qui sum check
	replace E2_Sbehavior_index_b=. if check<`r(max)' & attrition==0 & B_Sgirl==0  
	replace E2_Sbehavior_index_b=E2_Sbehavior_index_b/`r(max)' if attrition==0 & B_Sgirl==0
	drop check
	label var E2_Sbehavior_index_b "Boys' Behavior index"
	
	
******************* 3.4 Weighted Boys' Behavior Index *************************
		
		local vars
		foreach var in `behavior_boy' {
				gen `var'_temp=`var' if B_Sgirl==0
				sum `var'_temp
					local mean=r(mean)
				sum `var'_temp if B_treat==0
				replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
				local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if attrition==0 & B_Sgirl==0
			}
			
	egen E2_Sbehavior_index2_b_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E2_Sbehavior_index_b "Boys' Unweighted Behavior Index"
	lab var E2_Sbehavior_index2_b_i "Boys' Behavior Index"
	
		
			****** 2.1a Weighted behavior index - boys - (without imputing) ********
		
		* limit to boys
		preserve
		keep if B_Sgirl==0
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
		restore 
		
		local i=0
			foreach x in `behavior_boy' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] if B_Sgirl==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp if B_Sgirl==0 // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if B_Sgirl==0 // sum of weights of variables to keep
		
		** weighting the variables		
		egen E2_Sbehavior_index2_b_ni=rsum2(sum_*) if B_Sgirl==0, anymiss
		replace E2_Sbehavior_index2_b_ni = E2_Sbehavior_index2_b_ni*(C[1,1]/keep_sum) if B_Sgirl==0
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var E2_Sbehavior_index2_b_ni "Endline 2 Boys' Behavior Index"	

******************* 3.5 Average Pooled (Common) Behavior Index ******************

	local behavior_common E2_Stalk_opp_gen_comm E2_Ssit_opp_gen_comm E2_Sfriend_opp_gen_comm ///
	E2_Splay_opp_gen_comm E2_Stalk_week_opp_gen_comm ///
	E2_Scook_clean_comm E2_Sabsent_sch_hhwork_comm ///
	E2_Sdisc_col_comm E2_Sdisc_work_comm 

	global el2_behavior_common_flag2
	
		foreach y in `behavior_common' {
			
			assert `y'!=. if attrition==0
			
			cap confirm variable `y'_flag2
			assert _rc!=0 // flag2 should not exist in data 
			

				gen `y'_flag2=inlist(`y', .d, .r, .i, .f, .e) if attrition==0
				global el2_behavior_common_flag2 $el2_behavior_common_flag2 `y'_flag2
		
		}

	
	local behavior_common_flag
	foreach y in `behavior_common' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
		qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition==0
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local behavior_common_flag `behavior_common_flag' `y'_flag
				
			}	
		}	
		
	global el2_behavior_common_flag `behavior_common_flag' 

	cap drop check
	egen E2_Sbehavior_index=rowtotal(`behavior_common'), mi
	egen check=rownonmiss(`behavior_common')

	tab check
	qui sum check
	replace E2_Sbehavior_index=. if check<`r(max)'   
	replace E2_Sbehavior_index=E2_Sbehavior_index/`r(max)'
	drop check
	label var E2_Sbehavior_index "Self-reported behavior index"

	// 29 oct 1023
	local behavior_common E2_Stalk_opp_gen_comm E2_Ssit_opp_gen_comm E2_Sfriend_opp_gen_comm ///
	E2_Splay_opp_gen_comm E2_Stalk_week_opp_gen_comm ///
	E2_Scook_clean_comm E2_Sabsent_sch_hhwork_comm ///
	E2_Sdisc_col_comm E2_Sdisc_work_comm 

	factor `behavior_common' if /* !mi(E_Steam_id) & attrition_el==0 &  */ B_treat==0, factors(1) 
	predict E2_Sbehavior_index_factor /* if !mi(E_Steam_id) & attrition_el==0 */
	sum  E2_Sbehavior_index_factor if B_treat==0 
	replace  E2_Sbehavior_index_factor  = (E2_Sbehavior_index_factor -r(mean))/r(sd)
	

****************** 3.6 Weighted Pooled (Common) Behavior Index *****************

	local vars
	foreach var in `behavior_common' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse  of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E2_Sbehavior_index2_i=rsum2(sum_*), anymiss
			
	*cap drop *_temp
	cap drop sum_*
	
	lab var E2_Sbehavior_index "Unweighted Behavior Index"
	lab var E2_Sbehavior_index2_i "Self-reported behavior index"
	
			****** 2.1a Weighted behavior index - common - (without imputing) ********
		
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		
			
		local i=0
			foreach x in `behavior_common' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1] // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp  // ind weight * variable value; 0 for missing variables
			}
			
					
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))		
				
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw) // sum of weights of variables to keep
		
		** weighting the variables		
		egen E2_Sbehavior_index2_ni=rsum2(sum_*), anymiss
		replace E2_Sbehavior_index2_ni = E2_Sbehavior_index2_ni*(C[1,1]/keep_sum) 
			
	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 
			
			
	lab var E2_Sbehavior_index2_ni "Endline 2 Behavior Index"

**** 3.6.5 Apples to Apples Weighted Pooled (Common) Behavior Index (JAKE addition for R&R) ***

local behavior_common E2_Stalk_opp_gen_comm E2_Ssit_opp_gen_comm E2_Scook_clean_comm ///
		E2_Sabsent_sch_hhwork_comm E2_Sdisc_col_comm E2_Sdisc_work_comm 

local E2_Stalk_opp_gen_comm E_Stalk_opp_gender_comm
local E2_Ssit_opp_gen_comm E_Ssit_opp_gender_comm
local E2_Sfriend_opp_gen_comm
local E2_Splay_opp_gen_comm
local E2_Stalk_week_opp_gen_comm
local E2_Scook_clean_comm E_Scook_clean_comm
local E2_Sabsent_sch_hhwork_comm E_Sabsent_sch_hhwork_comm
local E2_Sdisc_col_comm E_Sdiscourage_college_comm
local E2_Sdisc_work_comm E_Sdiscourage_work_comm

local el1_wts
	
local i=0
local sum = 0
foreach x in `behavior_common' {
	local ++i

	if "`x'" != "E2_Sshy" &  "`x'" != "E2_Slaugh" {
		local pos strpos("${el1_wts_b}","``x''") + length("``x''") // position from where the required stat starts
		local l = strlen("${el1_wts_b}")	// length of entire string	
		local string = substr("${el1_wts_b}", `pos'-`l'-1, . ) // removing the part of string before the relevant weight
		local wt: word 1 of `string' // weight extracted
		}
		
	else {
		local wt = 0 
	}

	local wt_`i' = `wt'
	local sum = `sum' + `wt_`i''
}
	


local vars
foreach var in `behavior_common' {

	gen `var'_temp=`var'		
	local e1 = "``var''" //JAKE: using EL1 to normalize instead of EL2
	sum `e1'
		local mean=r(mean) 
	sum `e1' if B_treat==0
	* normalizing variables
	replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
	local vars `vars' `var'_temp
		
}

di `vars'

local i=0		
foreach x in `vars' {
	local ++i	
	gen sum_`i' = `wt_`i''*`x' // wt * variable value
}
	
egen E2_Sbehavior_index2_i_e1=rsum2(sum_*), anymiss

cap drop sum_*

lab var E2_Sbehavior_index2_i_e1 "Self-reported behavior index (EL1 weights)"

local i=0
foreach x in `behavior_common' {
	local ++i	
	gen `x'_nmi = `x'_flag==0 if attrition==0 // variable not missing	
	gen double `x'_kw = `x'_nmi * `wt_`i''  if attrition==0 // weight of non-misisng variable; 0 if missing
	gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
}
						
* sum of weights of non-missing variables 
egen double keep_sum = rowtotal(*_kw)  if attrition==0 // sum of weights of variables to keep

** weighting the variables
egen E2_Sbehavior_index2_ni_e1=rsum2(sum_*) if attrition==0, anymiss
replace E2_Sbehavior_index2_ni_e1 = E2_Sbehavior_index2_ni_e1*(`el1_sum'/keep_sum)
		
cap drop sum_*
cap drop  *_nmi *_kw keep_sum
cap drop *_temp 
		
lab var E2_Sbehavior_index2_ni_e1 "Self-reported behavior index (EL1 weights)"	

						****BEHAVIOR SUB_INDICES****
						
********************* 3.7 Interaction Index (girls and boys)********************

** average

	local behavior_oppsex E2_Stalk_opp_gen_comm E2_Ssit_opp_gen_comm E2_Sfriend_opp_gen_comm ///
	E2_Splay_opp_gen_comm E2_Stalk_week_opp_gen_comm
	
	* flags for missing values
	global el2_behavior_oppsex_flag
	global el2_behavior_oppsex_flag2
	foreach var in `behavior_oppsex' {
		global el2_behavior_oppsex_flag $el2_behavior_oppsex_flag `var'_flag
		global el2_behavior_oppsex_flag2 $el2_behavior_oppsex_flag2 `var'_flag2
		}


	cap drop check
	egen E2_Sbehavior_index_oppsex=rowtotal(`behavior_oppsex'), mi
	egen check=rownonmiss(`behavior_oppsex')

	tab check
	qui sum check
	replace E2_Sbehavior_index_oppsex=. if check<`r(max)'   
	replace E2_Sbehavior_index_oppsex=E2_Sbehavior_index_oppsex/`r(max)'
	drop check
	label var E2_Sbehavior_index_oppsex "Behavior sub-index (Interaction with the opp sex)"


** weighted 

	local vars
	foreach var in `behavior_oppsex' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E2_Sbehavior_index2_oppsex_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E2_Sbehavior_index_oppsex "Unweighted Interaction with the Opposite Sex Sub-Index"
	lab var E2_Sbehavior_index2_oppsex_i "Interaction with the Opposite Sex Sub-Index"
	
** weighted (not-imputed)
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		
		local i=0
			foreach x in `behavior_oppsex' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  if attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  attrition==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E2_Sbehavior_index2_oppsex_ni=rsum2(sum_*) if attrition==0, anymiss
		replace E2_Sbehavior_index2_oppsex_ni = E2_Sbehavior_index2_oppsex_ni*(C[1,1]/keep_sum)			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E2_Sbehavior_index2_oppsex_ni "Interaction with Opposite Sex"	


************* 3.8 Participation in HH chores (girls and boys) ******************

	local behavior_hhchores E2_Scook_clean_comm E2_Sabsent_sch_hhwork_comm

	* flags for missing values
	global el2_behavior_hhchores_flag
	global el2_behavior_hhchores_flag2
	foreach var in `behavior_hhchores' {
		global el2_behavior_hhchores_flag $el2_behavior_hhchores_flag `var'_flag
		global el2_behavior_hhchores_flag2 $el2_behavior_hhchores_flag2 `var'_flag2
		}
		
** average	
	cap drop check
	egen E2_Sbehavior_index_hhchores=rowtotal(`behavior_hhchores'), mi
	egen check=rownonmiss(`behavior_hhchores')

	tab check
	qui sum check
	replace E2_Sbehavior_index_hhchores=. if check<`r(max)'   
	replace E2_Sbehavior_index_hhchores=E2_Sbehavior_index_hhchores/`r(max)'
	drop check
	label var E2_Sbehavior_index_hhchores "Behavior sub-index (Participation in HH Chores)"


** weighted 

	local vars
	foreach var in `behavior_hhchores' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E2_Sbehavior_index2_hhchores_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E2_Sbehavior_index_hhchores "Unweighted Participation in HH Chores Sub-Index"
	lab var E2_Sbehavior_index2_hhchores_i "Participation in HH Chores Sub-Index"

** weighted (not-imputed)
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		
		local i=0
			foreach x in `behavior_hhchores' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  if attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  attrition==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E2_Sbehavior_index2_hhchores_ni=rsum2(sum_*) if attrition==0, anymiss
		replace E2_Sbehavior_index2_hhchores_ni = E2_Sbehavior_index2_hhchores_ni*(C[1,1]/keep_sum)			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E2_Sbehavior_index2_hhchores_ni "Participation in HH Chores"	

********* 3.9 Supporting female relatives' ambitions (girls and boys) **********

	local behavior_relatives E2_Sdisc_col_comm E2_Sdisc_work_comm
	
	* flags for missing values
	global el2_behavior_relatives_flag
	global el2_behavior_relatives_flag2
	foreach var in `behavior_relatives' {
		global el2_behavior_relatives_flag $el2_behavior_relatives_flag `var'_flag
		global el2_behavior_relatives_flag2 $el2_behavior_relatives_flag2 `var'_flag2
		}
		
** average
	cap drop check
	egen E2_Sbehavior_index_relatives=rowtotal(`behavior_relatives'), mi
	egen check=rownonmiss(`behavior_relatives')

	tab check
	qui sum check
	replace E2_Sbehavior_index_relatives=. if check<`r(max)'   
	replace E2_Sbehavior_index_relatives=E2_Sbehavior_index_relatives/`r(max)'
	drop check
	label var E2_Sbehavior_index_relatives "Behavior sub-index (Supporting Female Relatives' Ambitions)"


** weighted

	local vars
	foreach var in `behavior_relatives' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E2_Sbehavior_index2_relatives_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E2_Sbehavior_index_relatives "Unweighted Supporting Female Relatives' Ambitions Sub-Index"
	lab var E2_Sbehavior_index2_relatives_i "Supporting Female Relatives' Ambitions Sub-Index"

** weighted (not-imputed)
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		
		local i=0
			foreach x in `behavior_relatives' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  if attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  attrition==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E2_Sbehavior_index2_relatives_ni=rsum2(sum_*) if attrition==0, anymiss
		replace E2_Sbehavior_index2_relatives_ni = E2_Sbehavior_index2_relatives_ni*(C[1,1]/keep_sum)			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E2_Sbehavior_index2_relatives_ni "Supporting Female Relatives' Ambitions"	
	
********************* 3.10 Decision-making (girls only) ************************


	local behavior_decision_girl E2_Sfuture_work E2_Sdec_past_twel_y ///
		E2_Sdecision_work_y E2_Sdecision_kindofwork_y E2_Sattendance 
	
	* flags for missing values
	global el2_behavior_decision_girl_flag
	global el2_behavior_decision_girl_flag2
	foreach var in `behavior_decision_girl' {
		global el2_behavior_decision_girl_flag $el2_behavior_decision_girl_flag `var'_flag
		global el2_behavior_decision_girl_flag2 $el2_behavior_decision_girl_flag2 `var'_flag2

		}

**average

	cap drop check
	egen E2_Sbehavior_index_decision_g=rowtotal(`behavior_decision_girl') if B_Sgirl==1 & attrition==0, mi
	egen check=rownonmiss(`behavior_decision_girl') if B_Sgirl==1 & attrition==0

	tab check
	qui sum check
	replace E2_Sbehavior_index_decision_g=. if check<`r(max)' & B_Sgirl==1 & attrition==0   
	replace E2_Sbehavior_index_decision_g=E2_Sbehavior_index_decision_g/`r(max)' if B_Sgirl==1 & attrition==0
	drop check
	label var E2_Sbehavior_index_decision_g "Girls' Behavior index (Decision-making)"


**weighted 

	local vars
	foreach var in `behavior_decision_girl' {
			gen `var'_temp=`var' if B_Sgirl==1 
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==1 & attrition==0
			}
			
	egen E2_Sbehavior_index2_dec_g_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E2_Sbehavior_index_decision_g "Girls' Unweighted Decision-making Sub-Index"
	lab var E2_Sbehavior_index2_dec_g_i "Girls' Decision-making Sub-Index"
	
** weighted (not-imputed)
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		restore

		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		
		local i=0
			foreach x in `behavior_decision_girl' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  if attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  attrition==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E2_Sbehavior_index2_dec_g_ni=rsum2(sum_*) if attrition==0, anymiss
		replace E2_Sbehavior_index2_dec_g_ni = E2_Sbehavior_index2_dec_g_ni*(C[1,1]/keep_sum)			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E2_Sbehavior_index2_dec_g_ni "Girls' Decision-making"	
	
	

********************* 3.11 Mobility (girls only) ************************

	local mobility E2_Salone_friend E2_Salone_market E2_Salone_events E2_Salone_past_week E2_Salone_school_y 
	
	* flags for missing values
	global el2_mobility_flag
	global el2_mobility_flag2
	foreach var in `mobility' {
		global el2_mobility_flag $el2_mobility_flag `var'_flag
		global el2_mobility_flag2 $el2_mobility_flag2 `var'_flag2
		}
		
** average
	cap drop check
	egen E2_Sbehavior_index_mobility_g=rowtotal(`mobility') if B_Sgirl==1 & attrition==0, mi
	egen check=rownonmiss(`mobility') if B_Sgirl==1 & attrition==0

	tab check
	qui sum check
	replace E2_Sbehavior_index_mobility_g=. if check<`r(max)'  & B_Sgirl==1 & attrition==0
	replace E2_Sbehavior_index_mobility_g=E2_Sbehavior_index_mobility_g/`r(max)' if B_Sgirl==1 & attrition==0
	drop check
	label var E2_Sbehavior_index_mobility_g "Girls' Unweighted Mobility Sub-Index"


** weighted

	local vars
	foreach var in `mobility' {
			gen `var'_temp=`var' if B_Sgirl==1 
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==1 & attrition==0
			}
			
	egen E2_Sbehavior_index2_mobil_g_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E2_Sbehavior_index2_mobil_g_i "Girls' Mobility Sub-Index"
	
** weighted (not-imputed)
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		restore
		
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		
		local i=0
			foreach x in `mobility' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  if attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  attrition==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E2_Sbehavior_index2_mobil_g_ni=rsum2(sum_*) if attrition==0, anymiss
		replace E2_Sbehavior_index2_mobil_g_ni = E2_Sbehavior_index2_mobil_g_ni*(C[1,1]/keep_sum)			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E2_Sbehavior_index2_mobil_g_ni "Girls' Mobility"
	

		**************************************************************
						*		4. Scholarship          *
		**************************************************************
		
	local scholar E2_Sscholar_apply
	
	gen E2_Sscholar_index = E2_Sscholar_apply if B_Sgirl==1
	gen E2_Sscholar_index2 = E2_Sscholar_apply if B_Sgirl==1
	
	replace E2_Sscholar_index2 = .s if E2_Ssurvey_type==2 // phone survey
	replace E2_Sscholar_index2 =  0 if E2_Skeep_form==0 // if did not keep the scholarship form

	
	lab var E2_Sscholar_index "Applied to scholarship"
	lab var E2_Sscholar_index2 "Applied to scholarship"
	
	la var E2_Sscholar_apply_strict "Applied to scholarship (with parental form)"
	
		**************************************************************
						*		5. Petition          *
		**************************************************************
		
	local petition E2_Spetition_support	

	gen E2_Spetition_index = E2_Spetition_support
	gen E2_Spetition_index2 = E2_Spetition_support
	
	replace E2_Spetition_index2 = .s if E2_Ssurvey_type==2 // phone survey
	replace E2_Spetition_index2 =  0 if E2_Skeep_petition==0 // if did not keep the petition
	
	lab var E2_Spetition_index "Signed petition"
	lab var E2_Spetition_index2 "Signed petition"
	
	*phone access: B_Saccess_mobile B_Saccess_mobile_brel
	
	replace B_Saccess_mobile = 0 if B_Scellphone_house==0 // if house does not have a cell phone
	
	gen B_Saccess_mobile_brel = !inlist(B_Spurp_mobile, ".r", "1") if !inlist(B_Spurp_mobile, ".s", "")
	replace B_Saccess_mobile_brel = 0 if B_Saccess_mobile==0
		la var B_Saccess_mobile_brel "Phone access, beyond calling relatives"

	foreach y in B_Saccess_mobile B_Saccess_mobile_brel {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
		qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y')
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				
			}	
		}	
		

********************************************************************************
				*		ENDLINE 2 SECONDARY OUTCOMES: INDICES            *
********************************************************************************

		**************************************************************
						*		1. Self_esteem          *
		**************************************************************

**Average self-esteem index

	local esteem E2_Ssatisfy_y E2_Sgood_qly_y E2_Sable_do_most_y
	
		global el2_esteem_flag2
	
		foreach y in `esteem' {
		
			assert `y'!=. if attrition==0
			
			cap confirm variable `y'_flag2
			assert _rc!=0 // flag2 should not exist in data 
			
				gen `y'_flag2=inlist(`y', .d, .r, .i, .f, .e) if attrition==0
				global el2_esteem_flag2 $el2_esteem_flag2 `y'_flag2
				
			}

	
	local esteem_flag
	
	foreach y in `esteem' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
		qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition==0
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local esteem_flag `esteem_flag' `y'_flag
				
			}	
		}	
		
	global el2_esteem_flag 	`esteem_flag'

	cap drop check
	egen E2_Sesteem_index=rowtotal(`esteem'), mi
	egen check=rownonmiss(`esteem')

	tab check
	qui sum check
	replace E2_Sesteem_index=. if check<`r(max)'   
	replace E2_Sesteem_index=E2_Sesteem_index/`r(max)'
	drop check
	label var E2_Sesteem_index "Self-esteem index"
	
	
**Weighted self-esteem index


	local vars
	foreach var in `esteem' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
 	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A

	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x'
			}
			
	egen E2_Sesteem_index2=rsum2(sum_*), anymiss
			
	cap drop *_temp
	cap drop sum_*
			
	lab var E2_Sesteem_index "Unweighted Self-esteem Index"
	lab var E2_Sesteem_index2 "Self-esteem Index"


***Boys' self-esteem index

	local esteem E2_Ssatisfy_y E2_Sgood_qly_y E2_Sable_do_most_y

	cap drop check
	egen E2_Sesteem_index_boy=rowtotal(`esteem') if B_Sgirl==0 & attrition==0, mi 
	egen check=rownonmiss(`esteem') if B_Sgirl==0 & attrition==0

	tab check
	qui sum check
	replace E2_Sesteem_index_boy=. if check<`r(max)' & B_Sgirl==0 & attrition==0
	replace E2_Sesteem_index_boy=E2_Sesteem_index_boy/`r(max)' if B_Sgirl==0 & attrition==0
	drop check
	label var E2_Sesteem_index_boy "Boys' Self-esteem Index"
	
	
**Weighted boys' self-esteem index
	
	local vars
	foreach var in `esteem' {
			gen `var'_temp=`var' if B_Sgirl==0 & attrition==0
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}

	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==0 & attrition==0
			}
			
	egen E2_Sesteem_index2_boy=rsum2(sum_*), anymiss
			
	cap drop *_temp
	cap drop sum_*
			
	lab var E2_Sesteem_index_boy "Unweighted Boys' Self-esteem Index"
	lab var E2_Sesteem_index2_boy "Boys' Self-esteem Index"


***Girls' self-esteem index

	local esteem E2_Ssatisfy_y E2_Sgood_qly_y E2_Sable_do_most_y

	cap drop check
	egen E2_Sesteem_index_girl=rowtotal(`esteem') if B_Sgirl==1 & attrition==0, mi 
	egen check=rownonmiss(`esteem') if B_Sgirl==1 & attrition==0

	tab check
	qui sum check
	replace E2_Sesteem_index_girl=. if check<`r(max)' & B_Sgirl==1 & attrition==0
	replace E2_Sesteem_index_girl=E2_Sesteem_index_girl/`r(max)' if B_Sgirl==1 & attrition==0
	drop check
	label var E2_Sesteem_index_girl "Girls' Self-esteem Index"
	
	
**Weighted girls' self-esteem index
	
	local vars
	foreach var in `esteem' {
			gen `var'_temp=`var' if B_Sgirl==1 & attrition==0
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}

	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A
	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==1 & attrition==0
			}
			
	egen E2_Sesteem_index2_girl_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E2_Sesteem_index_girl "Unweighted Girls' Self-esteem Index"
	lab var E2_Sesteem_index2_girl_i "Girls' Self-esteem Index"
	
** weighted (not-imputed)
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		restore
		
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		
		local i=0
			foreach x in `esteem' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==1 & attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  if B_Sgirl==1 & attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  B_Sgirl==1 & attrition==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E2_Sesteem_index2_girl_ni=rsum2(sum_*) if B_Sgirl==1 & attrition==0, anymiss
		replace E2_Sesteem_index2_girl_ni = E2_Sesteem_index2_girl_ni*(C[1,1]/keep_sum)			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E2_Sesteem_index2_girl_ni "Girls' self-esteem"
	

		**************************************************************
		*		2. Girls' Educational Attainment Index           *
		**************************************************************
		
	
	* change to missing if grade missing
	replace E2_Sstem_stream_y = .i if mi(B_Senrolled)

	local educ_attain E2_Sschool_enrol_y E2_Sstem_stream_y E2_Senrol_eng_comp_voc_y E2_Stake_tuition_y 
	
		
		global el2_educ_attain_flag2
	
		foreach y in `educ_attain' {
		
			assert `y'!=. if attrition==0
			
			cap confirm variable `y'_flag2
			assert _rc!=0 // flag2 should not exist in data 
			
				gen `y'_flag2=inlist(`y', .d, .r, .i, .f, .e) if attrition==0
				global el2_educ_attain_flag2 $el2_educ_attain_flag2 `y'_flag2
				
			}

	local educ_attain_flag
	
	foreach y in `educ_attain' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
		qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition==0 & B_Sgirl==1
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') if B_Sgirl==1
				qui replace `y'=x if `y'_flag==1
				drop x
				local educ_attain_flag `educ_attain_flag' `y'_flag
				
			}	
		}	
		
	global el2_educ_attain_flag `educ_attain_flag'

	
**Average 
	cap drop check
	egen E2_Seduc_attain_index_g=rowtotal(`educ_attain') if B_Sgirl==1 & attrition==0, mi
	egen check=rownonmiss(`educ_attain') if B_Sgirl==1 & attrition==0

	tab check
	qui sum check
	replace E2_Seduc_attain_index_g=. if check<`r(max)' & B_Sgirl==1 & attrition==0
	replace E2_Seduc_attain_index_g=E2_Seduc_attain_index_g/`r(max)' if B_Sgirl==1 & attrition==0
	drop check
	lab var E2_Seduc_attain_index_g "Unweighted Girls' Education Attainment Index"

	
**Weighted 

	local vars
	foreach var in `educ_attain' {
			gen `var'_temp=`var' if B_Sgirl==1 & attrition==0
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
 	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A

	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==1 & attrition==0
			}
			
	egen E2_Seduc_attain_index2_g_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E2_Seduc_attain_index2_g_i "Girls' Education Attainment Index"
	
** weighted (not-imputed)
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		restore
		
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		
		local i=0
			foreach x in `educ_attain' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==1 & attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  if B_Sgirl==1 & attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  B_Sgirl==1 & attrition==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E2_Seduc_attain_index2_g_ni=rsum2(sum_*) if B_Sgirl==1 & attrition==0, anymiss
		replace E2_Seduc_attain_index2_g_ni = E2_Seduc_attain_index2_g_ni*(C[1,1]/keep_sum)			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E2_Seduc_attain_index2_g_ni "Girls' education"

	

		**************************************************************
			*		3. Marriage and Fertility Aspirations        *
		**************************************************************

	local mar_fert_asp E2_Stot_marry_age_m E2_Sfirst_child_age_m E2_Sgirl_boy_pref E2_Sboy_count_m E2_Sprefer_children_y
	
		global el2_mar_fert_asp_flag2
		
		
		foreach y in `mar_fert_asp' {
		
			assert `y'!=. if attrition==0
			
			cap confirm variable `y'_flag2
			assert _rc!=0 // flag2 should not exist in data 
			

				gen `y'_flag2=inlist(`y', .d, .r, .i, .f, .e) if attrition==0
				global el2_mar_fert_asp_flag2 $el2_mar_fert_asp_flag2 `y'_flag2
				
			
		}
	
	local mar_fert_asp_flag
	
	foreach y in `mar_fert_asp' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
		qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition==0
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local mar_fert_asp_flag `mar_fert_asp_flag' `y'_flag
				
			}	
		}	
		
	global el2_mar_fert_asp_flag `mar_fert_asp_flag'

**Average 
	cap drop check
	egen E2_Smar_fert_asp_index=rowtotal(`mar_fert_asp'), mi
	egen check=rownonmiss(`mar_fert_asp')

	tab check
	qui sum check
	replace E2_Smar_fert_asp_index=. if check<`r(max)'   
	replace E2_Smar_fert_asp_index=E2_Smar_fert_asp_index/`r(max)'
	drop check
	lab var E2_Smar_fert_asp_index "Unweighted Marraige and Fertility Aspirations Index"

	
**Weighted 

	local vars
	foreach var in `mar_fert_asp' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
 	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A

	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' 
			}
			
	egen E2_Smar_fert_asp_index2_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E2_Smar_fert_asp_index2_i "Marriage and fertility Aspirations Index"
	
** weighted (not-imputed)
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		
		local i=0
			foreach x in `mar_fert_asp' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  if attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  attrition==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E2_Smar_fert_asp_index2_ni=rsum2(sum_*) if attrition==0, anymiss
		replace E2_Smar_fert_asp_index2_ni = E2_Smar_fert_asp_index2_ni*(C[1,1]/keep_sum)			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E2_Smar_fert_asp_index2_ni "Marriage and fertility aspirations"


		**************************************************************
		*		4. Girls' experience of sexual harassment/assault    *
		**************************************************************
	
	
	local harassed E2_Sharass_slapped_y_g E2_Steas_opp_gen_boy_scl_y E2_Steas_opp_gen_boy_vil_y E2_Stch_opp_gen_boy_scl_y E2_Stch_opp_gen_boy_vil_y
	
		global el2_harassed_flag2
	
		foreach y in `harassed' {
		
			assert `y'!=. if attrition==0
			
			cap confirm variable `y'_flag2
			assert _rc!=0 // flag2 should not exist in data 
			
				gen `y'_flag2=inlist(`y', .d, .r, .i, .f, .e) if attrition==0
				global el2_harassed_flag2 $el2_harassed_flag2 `y'_flag2
				
			}
		
	local harassed_flag
	
	foreach y in `harassed' {
			cap confirm variable `y'_flag, exact
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
		qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if B_Sgirl==1 & attrition==0
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local harassed_flag `harassed_flag' `y'_flag
				
			}	
		}	
		
	global el2_harassed_flag `harassed_flag'

**Average 
	cap drop check
	egen E2_Sharassed_index_g=rowtotal(`harassed'), mi
	egen check=rownonmiss(`harassed')

	tab check
	qui sum check
	replace E2_Sharassed_index_g=. if check<`r(max)' & B_Sgirl==1 & attrition==0 
	replace E2_Sharassed_index_g=E2_Sharassed_index_g/`r(max)' if B_Sgirl==1 & attrition==0
	drop check
	lab var E2_Sharassed_index_g "Unweighted Girls' Harassment Index"
	
	**Weighted 

	local vars
	foreach var in `harassed' {
			gen `var'_temp=`var' if B_Sgirl==1 
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
				}
 	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	dis "`vars'"	
	corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
	matrix Ainv=inv(A)										// inverse of A

	mat list Ainv

	mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

					
	local i=0
			foreach x in `vars' {
			local ++i
			gen sum_`i' = B[`i',1]*`x' if B_Sgirl==1 & attrition==0
			}
			
	egen E2_Sharassed_index2_g_i=rsum2(sum_*), anymiss
			
	cap drop sum_*
			
	lab var E2_Sharassed_index2_g_i "Girls' Harassment Index"	
	
	
** weighted (not-imputed)
		preserve
		keep if B_Sgirl==1
		corrmat `vars' , covmat(A)
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		restore
		
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

		
		local i=0
			foreach x in `harassed' {
			local ++i	
			
			gen `x'_nmi = `x'_flag==0 if B_Sgirl==1 & attrition==0 // variable not missing	
			gen double `x'_kw = `x'_nmi * B[`i',1]  if B_Sgirl==1 & attrition==0 // weight of non-misisng variable; 0 if missing
			
			gen double sum_`i'=  `x'_kw * `x'_temp // ind weight * variable value; 0 for missing variables
			
			}
		
		* overall sum of weights
		mata : st_matrix("C", colsum(st_matrix("B")))
		
		* sum of wieghts of non-missing variables 
		egen double keep_sum = rowtotal(*_kw)  if  B_Sgirl==1 & attrition==0 // sum of weights of variables to keep
			
		** weighting the variables
		
		egen E2_Sharassed_index2_g_ni=rsum2(sum_*) if B_Sgirl==1 & attrition==0, anymiss
		replace E2_Sharassed_index2_g_ni = E2_Sharassed_index2_g_ni*(C[1,1]/keep_sum)			

	cap drop *_temp
	cap drop sum_*
	cap drop  *_nmi *_kw keep_sum 		
		
	lab var E2_Sharassed_index2_g_ni "Girls' experienced harassment"	

		**************************************************************
		*	5. Boys' sexual harassment/assault (List randomization)   *
		**************************************************************
/*
variables: 
E2_Slr_harass_b (overall difference in means)
E2_Slr_harass_b_treat E2_Slr_harass_b_ctrl (means by treatment and control)
E2_Slr_harass_b_sch (school level difference in means)
E2_Slr_harass_b_sch_grade (school-grade level difference in means)
*/

		** groups by school-grade 
		gen E2_Slr_harass_b_sch_grade = .
		levelsof Sschool_id, local(all_sch)
	
		foreach s in `all_sch' {
		
			levelsof B_Sgrade6, local(all_grade)
				
				foreach g in `all_grade' {					
					su E2_Smain_lr_true_with if Sschool_id==`s' & B_Sgrade6==`g'
						if `r(N)'>0 local mean_with = `r(mean)'
						else if `r(N)'==0 local mean_with = .
					su E2_Smain_lr_true_wo if Sschool_id==`s' & B_Sgrade6==`g'
						if `r(N)'>0 local mean_wo = `r(mean)'
						else if `r(N)'==0 local mean_wo = .
						
					replace E2_Slr_harass_b_sch_grade = (`mean_with' - `mean_wo') ///
						if ( !mi(E2_Smain_lr_true_with) | !mi(E2_Smain_lr_true_wo)) & Sschool_id==`s'  & B_Sgrade6==`g'
					}					
			
			}
			
		la var E2_Slr_harass_b_sch_grade "Boys' perpetrated harassment (school-grade level)" 	
		
		order E2_Slr_harass_b_sch_grade, after(E2_Slr_harass_b_sch)
		
	
		**************************************************************
		*	6. Social norms   *
		**************************************************************

	*****************************************************
	** SOCIAL NORMS: SET 1 and 2 (WORK and EDUCATION)
	*****************************************************
	
	cap drop E2_Scommunity_work_s E2_Scommunity_college_s
	local social E2_Sallow_work E2_Scommunity_allow_work E2_Sallow_college E2_Scommunity_allow_college

	foreach x in `social' {
	gen `x'_s=inlist(`x',1) if !mi(`x')
	}
	
	replace E2_Sallow_work_s = . if E2_Sallow_work_flag!=0

	local social2 E2_Sallow_work_s E2_Scommunity_allow_work_s E2_Sallow_college_s E2_Scommunity_allow_college_s

	gen E2_Scommunity_work_s=inlist(E2_Sallow_work,1) & inlist(E2_Scommunity_allow_work,0) & inlist(E2_Soppose_allow_work,0) ///
			if !mi(E2_Sallow_work) & !mi(E2_Scommunity_allow_work) & !mi(E2_Soppose_allow_work)
	replace E2_Scommunity_work_s=1 if E2_Sallow_work==1 & E2_Scommunity_allow_work==1
	replace E2_Scommunity_work_s=0 if E2_Sallow_work==0 & E2_Scommunity_allow_work==0	
	
	gen E2_Scommunity_college_s=inlist(E2_Sallow_college,1) & inlist(E2_Scommunity_allow_college,0) & inlist(E2_Soppose_allow_college,0) ///
			if !mi(E2_Sallow_college) & !mi(E2_Scommunity_allow_college) & !mi(E2_Soppose_allow_college)
	replace E2_Scommunity_college_s=1 if E2_Sallow_college==1 & E2_Scommunity_allow_college==1
	replace E2_Scommunity_college_s=0 if E2_Sallow_college==0 & E2_Scommunity_allow_college==0
	
	la var E2_Sallow_work_s "women should be allowed to work"
	la var E2_Scommunity_allow_work_s "community thinks women should be allowed to work"
	la var E2_Scommunity_work_s "women should be allowed to work and thinks community will not oppose them"
	la var E2_Sallow_college_s "women should be allowed to study in college even if it is far away"
	la var E2_Scommunity_allow_college_s "community thinks women should be allowed to study in college even if it is far away"
	la var E2_Scommunity_college_s "women should be allowed to study in college and thinks community will not oppose them"

	**************************************************************
	*	EL2 SOCIAL DESIRABILITY  *
	**************************************************************
	
	**Social desirability score

	local social E2_Shard_not_enco E2_Sfeel_resent E2_Stoo_little_ability E2_Srebelling_against ///
 E2_Socca_advan  E2_Ssome_forgive E2_Sjealous E2_Sirritated E2_Sdeliberately

 * disgaree with these statements shows higher social desirability bias
	foreach var in `social' {
		local varlab: variable label `var'
		gen `var'_n=1 if `var'==0 
		replace `var'_n=0 if `var'==1
		lab var `var'_n "Disagree: `varlab'"
		}
		
local social E2_Shard_not_enco_n E2_Sfeel_resent_n E2_Stoo_little_ability_n E2_Srebelling_against_n ///
 E2_Socca_advan_n  E2_Ssome_forgive_n E2_Sjealous_n E2_Sirritated_n E2_Sdeliberately_n ///
 E2_Sgood_listen E2_Swill_mistake E2_Scourteous E2_Sirked

	local social_flag
		
	foreach y in `social' {
			cap confirm variable `y'_flag
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y')
				bysort B_Sgirl B_Sdistrict B_treat: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				drop x
				local social_flag `social_flag' `y'_flag
				
			}	
	}
	
	global el2_social_flag `social_flag'
	
		local social E2_Shard_not_enco_n E2_Sfeel_resent_n E2_Stoo_little_ability_n E2_Srebelling_against_n ///
 E2_Socca_advan_n  E2_Ssome_forgive_n E2_Sjealous_n E2_Sirritated_n E2_Sdeliberately_n ///
 E2_Sgood_listen E2_Swill_mistake E2_Scourteous E2_Sirked

	cap drop check
	egen E2_Ssocial_scale=rowtotal(`social'), mi
	egen check=rownonmiss(`social')

	tab check
	qui sum check
	replace E2_Ssocial_scale=. if check<`r(max)'   
	drop check
	
	egen miss = rowmin($el2_social_flag)
	replace E2_Ssocial_scale = . if miss==1 // if all variables missing - endline 2 SDS section not administered
	drop miss
	
	label var E2_Ssocial_scale "Endline 2 social desirability score (index)"
	
	*****


		
local social E2_Shard_not_enco_n E2_Sfeel_resent_n E2_Stoo_little_ability_n E2_Srebelling_against_n ///
 E2_Socca_advan_n  E2_Ssome_forgive_n E2_Sjealous_n E2_Sirritated_n E2_Sdeliberately_n ///
 E2_Sgood_listen E2_Swill_mistake E2_Scourteous E2_Sirked


	local social_bin // social desirability values that are binary (not imputed for missing)
	foreach y in `social' {
		local l: var lab `y'
		gen `y'_bin=`y'
		replace  `y'_bin=. if `y'_flag==1
			la var `y'_bin "`l'"
		local social_bin `social_bin' `y'_bin
		}


	egen E2_Ssocial_scale_int=rowtotal(`social_bin'), mi // if al 13 missing, set to missing
	egen check=rownonmiss(`social_bin')


	tab check
	qui sum check
	replace E2_Ssocial_scale_int=. if check<`r(max)'   // if even 1 is missing, set to missing
	drop check

la var E2_Ssocial_scale_int "Endline 2 Social Desirability Score (0-13)"

	 **************************************************************


	** changing index value to missing if half or more variables missing
	
	local E2_Sgender_index2_i_e2 gender
	local E2_Sgender_index2_i_e1 gender
	local E2_Sgender_index2_ni_e2 gender
	local E2_Sgender_index2_ni_e1 gender
	local E2_Sgender_index2_educ educ
	local E2_Sgender_index2_emp emp
	local E2_Sgender_index2_sub sub
	local E2_Saspiration_index2_i aspiration
	local E2_Saspiration_index2_ni aspiration
	local E2_Saspiration_boy_index2_i aspiration_boy
	local E2_Saspiration_boy_index2_ni aspiration_boy
	local E2_Sbehavior_index2_g_i behavior_girl
	local E2_Sbehavior_index2_b_i behavior_boy 
	local E2_Sbehavior_index2_i behavior_common
	local E2_Sbehavior_index2_g_ni behavior_girl
	local E2_Sbehavior_index2_b_ni behavior_boy 
	local E2_Sbehavior_index2_ni behavior_common
	local E2_Sbehavior_index2_ni_e1 behavior_common //from Jake 
	local E2_Sbehavior_index2_oppsex behavior_oppsex
	local E2_Sbehavior_index2_hhchores behavior_hhchores
	local E2_Sbehavior_index2_relatives behavior_relatives
	local E2_Sbehavior_index2_decision_g behavior_decision_girl
	local E2_Sbehavior_index2_mobility_g mobility
	local E2_Sesteem_index2 esteem
	local E2_Seduc_attain_index2_g educ_attain
	local E2_Smar_fert_asp_index2 mar_fert_asp
	local E2_Sharassed_index2_g harassed 
	
	
	foreach index in E2_Sgender_index2_i_e2 E2_Sgender_index2_i_e1 E2_Sgender_index2_ni_e2 E2_Sgender_index2_ni_e1 ///
		     E2_Saspiration_index2_i E2_Saspiration_boy_index2_i E2_Saspiration_index2_ni E2_Saspiration_boy_index2_ni ///
					 E2_Sbehavior_index2_g_i E2_Sbehavior_index2_b_i   E2_Sbehavior_index2_i E2_Sbehavior_index2_g_ni ///
					 E2_Sbehavior_index2_b_ni   E2_Sbehavior_index2_ni E2_Sbehavior_index2_ni_e1	{
		
		local l: var lab `index'
		rename `index' `index'_nd
		gen `index'_d = `index'_nd
			la var `index'_d "`l'"
		
		local ``index''_n: word count ${el2_``index''_flag2}  // total no. of variables 
		
		
		di "${el2_``index''_flag2}"
		
		
		di "```index''_n'"
		
		tempvar num_miss
		egen `num_miss' = rowtotal(${el2_``index''_flag2}) // number of missing variables
	
		count if !mi(`index'_d) // before replacement count
			local ``index''_bef: di %7.0f r(N)
		
		replace `index'_d = . if (`num_miss'/```index''_n')>=.5 // if >=50% of variables missing, index=.
			
		count if !mi(`index'_d) // after replacement count
			local ``index''_aft: di %7.0f r(N)
			
		}
		

*************************************************
*		Working with merged dataset             *
*************************************************

	
	**Controls
	
	/* Student-level: B_Scaste_sc B_Scaste_st B_Smuslim B_Shh_size B_no_female_sib B_no_male_sib B_Sparent_stay  
	
	
	
	Asset variables: B_Shouse_pukka_y B_Shouse_elec B_Sflush_toilet B_Snonflush_toilet ///
		B_Sown_house B_Phh_durables_1 B_Phh_durables_2 B_Phh_durables_7 B_Snewspaper_house B_Stap_water B_Phh_durables_16
	
	Self-efficacy index: B_Sefficacy_index2 QUESTION: do we want simple average or weighted index?
	
	Social desirability Index: B_Ssocial_scale QUESTION:  do we want simple average or weighted index? (simple average used in endline 1)
	
	School and village characteristics: B_Schild_elect_woman_y E2_Select_woman_y B_q10_guest_teachr ///
	B_fulltime_teacher B_pct_female_teacher B_q12_activity_teachr B_q13_counselor B_q18_pta_meet ///
	B_q21_week1 B_q21_week2 B_q21_week3 B_q21_week4 B_q21_week6 B_q21_week7 ///
	B_coed B_rural
	B_q22_library B_q22_toilets B_q22_electricity B_q22_avail_computers ///
		B_q22_avail_internet B_q22_sports_field B_q22_mid_meal B_q22_auditorium B_q22_avail_edusat
	Cfem_lit_rate Cmale_lit_rate Cfem_lab_part
	*/
	

		
	**Caste

 	replace B_Scaste_sc=B_Pcaste_sc if B_in_sample==1
 	replace B_Scaste_st=B_Pcaste_st if B_in_sample==1 // if parent was surveyed , we take the parents' answer and then impute for missing values
	
	local controls B_Sage B_Scaste_sc B_Sgrade6 B_Scaste_st B_Shouse_pukka_y B_Shouse_elec B_Sflush_toilet B_Snonflush_toilet B_Sown_house ///
				   B_f_illiterate B_f_primary B_f_secondary B_f_parttime B_f_fulltime B_Phh_durables_1 B_Phh_durables_2 ///
				   B_m_illiterate B_m_primary B_m_secondary B_m_parttime B_m_fulltime B_Phh_durables_7 B_Snewspaper_house B_Stap_water ///
				   B_Phh_durables_16 B_Sparent_stay B_Smuslim B_Shindu B_Snumkids B_Scellphone_house B_Pown_land
	  
	local cntrls_flag			  
		
	foreach y in `controls' {
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
				local cntrls_flag `cntrls_flag' `y'_flag
				drop x
			}	
		}		
	
	
	
	gen B_rural=1 if B_urban==0
	replace B_rural=0 if B_urban==1
	replace B_rural=1 if Sschool_id==2711
	
	replace B_coed=0 if Sschool_id==2711
	
     **Extended controls
	 
	 **school controls
	 
	 local cntrls_school B_q10_guest_teachr B_fulltime_teacher B_pct_female_teacher B_q12_activity_teachr B_q13_counselor B_q18_pta_meet ///
			B_q22_library B_q22_toilets B_q22_electricity B_q22_avail_computers B_q22_avail_internet B_q22_sports_field B_q22_mid_meal ///
			B_q22_auditorium B_q22_avail_edusat B_q21_week1 B_q21_week2 B_q21_week3 B_q21_week4 B_q21_week6 B_q21_week7
	
	local cntrls_school_flag			  
		
	foreach y in `cntrls_school' {
			cap confirm variable `y'_flag
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y')
				bysort B_Sdistrict: egen x = mean (`y') 
				qui replace `y'=x if `y'_flag==1
				local cntrls_school_flag `cntrls_school_flag' `y'_flag
				drop x
			}	
		}		
	
*** labelling extended control variables
 la var B_Sage "Student's age"
 la var B_Sgrade6 "Student's grade at baseline"
 la var B_rural "Rural location"
 la var B_Scaste_sc "Scheduled caste"
 la var B_Scaste_st "Scheduled tribe"
 la var B_Smuslim "Muslim"
 la var B_no_female_sib "Number of female siblings"
 la var B_no_male_sib "Number of male siblings"
 la var B_Sparent_stay "Student stays with parents"
 la var B_m_secondary "Mother has completed 8th grade"
 la var B_m_parttime "Mother works part-time"
 la var B_m_fulltime "Mother works full-time"
 la var B_Shouse_pukka_y "House is pukka"
 la var B_Shouse_elec "Dwelling has electricity"
 la var B_Sflush_toilet "Dwelling has flush toilet"
 la var B_Snonflush_toilet "House has a no-flush toilet"
 la var B_Sown_house "Family owns the house"
 la var B_Phh_durables_1 "Household owns radio or tape recorder"
 la var B_Phh_durables_2 "Household owns TV"
 la var B_Phh_durables_7 "Household owns refrigerator"
 la var B_Snewspaper_house "Household gets newspaper daily"
 la var B_Stap_water "Household gets tap water"
 la var B_Phh_durables_16 "Household owns water pump"
 la var B_Sefficacy_index2 "Self-efficacy index"
 la var B_Ssocial_scale "Social desirability score"
 la var B_Pgender_index2_impute "Parent's baseline gender attitudes index"
 la var B_q10_guest_teachr "Number of guest teachers in the school"
 la var B_fulltime_teacher "Number of full-time teachers in the school"
 la var B_pct_female_teacher "Fraction of female teachers"
 la var B_q13_counselor "Availability of counsellor in the school"
 la var B_q18_pta_meet "Number of PTA meetings held in the last year"
 la var B_q22_library "School has a functional library"
 la var B_q22_toilets "School has functional toilets"
 la var B_q22_electricity "School has electricity"
 la var B_q22_avail_computers "School has access to computers"
 la var B_q22_avail_internet "School has access to internet"
 la var B_q22_sports_field "School has sports field"
 la var B_q22_mid_meal "School has mid-day meals" 
 la var B_q22_auditorium "School has auditorium"
 la var B_q22_avail_edusat "School has EduSat"
 la var B_q21_week1 "Bal Sabha sessions: number of times in a week"
 la var B_q21_week6 "Library sessions: number of times in a week"
 la var B_coed "School is co-ed"
 la var Cfem_lit_rate "Village-level adult female literacy rate"
 la var Cmale_lit_rate "Village-level adult male literacy rate"
 la var Cfem_lab_part "Village-level female labor force participation"


** saving variable version before normalizing
gen  E_Sgender_index2_s =  E_Sgender_index2_ni_nd 	
gen  E2_Sgender_index2_s = E2_Sgender_index2_ni_e1_nd   	
	
	 
**normalising the indices - gender (with sub-indices), aspirations, behavior (with sub-indices), self-esteem, social 
** EL2 attitudes w.r.t EL1
local el1  E_Sgender_index2_i_d E_Sgender_index2_i_nd E_Sgender_index2_ni_d E_Sgender_index2_ni_nd E_Sgender_index2_educ_i E_Sgender_index2_educ_ni E_Sgender_index2_emp_i E_Sgender_index2_emp_ni E_Sgender_index2_sub_i E_Sgender_index2_sub_ni E_Sbehavior_index2_ni_nd
local el2  E2_Sgender_index2_i_e1_d E2_Sgender_index2_i_e1_nd E2_Sgender_index2_ni_e1_d E2_Sgender_index2_ni_e1_nd E2_Sgender_index2_educ_i E2_Sgender_index2_educ_ni E2_Sgender_index2_emp_i E2_Sgender_index2_emp_ni E2_Sgender_index2_sub_i E2_Sgender_index2_sub_ni E2_Sbehavior_index2_ni_e1_nd

local i = 0
foreach var in `el2' {
	local ++i
	local var1:  word `i' of `el1'
	local var2:  word `i' of `el2'
	
	su `var1' if B_treat==0
	local mean `r(mean)'
	local sd `r(sd)'

	if `r(N)'>0 {
		replace `var2' = (`var2' - `mean')/`sd'
		}	
	}
	
	* EL1 indices pre-normalization
	foreach var in `el1' {
		gen `var'_p = `var'	
		}
		
		
* imputing missing baseline indices: 
	local base B_Sgender_index2 B_Sgender_index2_ni B_Saspiration_index2 B_Saspiration_boy_index2 B_Saspiration_index2_ni B_Saspiration_boy_index2_ni B_Saspiration_all_index2 ///
		B_Sbehavior_index2_g B_Sbehavior_index2_b B_Sbehavior_index2 B_Sbehavior_index2_g_ni B_Sbehavior_index2_b_ni B_Sbehavior_index2_ni ///
		B_Sgender_index2_educ B_Sgender_index2_educ_ni B_Sgender_index2_emp B_Sgender_index2_emp_ni B_Sgender_index2_sub B_Sgender_index2_sub_ni B_Sesteem_index2  ///
		B_Sbehavior_index2_oppsex B_Sbehavior_index2_oppsex_ni B_Sbehavior_index2_mobil_g B_Sbehavior_index2_mobil_g_ni  ///
		B_Sbehavior_index2_hhchores B_Sbehavior_index2_hhchores_ni B_Sbehavior_index2_dec_g B_Sbehavior_index2_dec_g_ni B_Sesteem_index2_girl B_Sesteem_index2_girl_ni B_Sesteem_index2_boy

foreach var in `base' {
	
	if strpos("`var'", "_ni")>0  { 
		local flagvar = subinstr("`var'", "_ni", "_flag", .)
		gen `flagvar' = mi(`var') // variable indicating if the variable value is missing
		}
		
	else if strpos("`var'", "B_Saspiration_all_index2")>0  { 
		local flagvar = "`var'" + "_flag"
		gen `flagvar' = mi(`var') // variable indicating if the variable value is missing
		}
		
	tempvar mean
	egen `mean' = mean(`var'), by(B_Sdistrict B_Sgirl)

	replace `var' = `mean' if mi(`var')
	
	}
		
// commented out variables in BL are not normalized as they are composed of a single dummy variable. 
local baseline_index B_Sgender_index B_Sgender_index2 B_Sgender_index2_ni B_Sgender_index2_educ B_Sgender_index2_educ_ni ///
		B_Sgender_index2_sub B_Sgender_index2_sub_ni ///
		B_Saspiration_index B_Saspiration_index2 B_Saspiration_index2_ni B_Saspiration_boy_index2_ni B_Saspiration_all_index2 B_Sbehavior_index_g ///
		B_Sbehavior_index2_g B_Sbehavior_index_b B_Sbehavior_index2_b B_Sbehavior_index B_Sbehavior_index2 B_Sesteem_index B_Sesteem_index2 ///
		B_Sbehavior_index2_g_ni B_Sbehavior_index2_b_ni B_Sbehavior_index2_ni ///		
		B_Sgender_index_educ B_Sgender_index2_educ  B_Sgender_index_sub ///
		B_Sgender_index2_sub B_Pgender_index ///
		B_Pgender_index2 B_Pgender_index2_impute B_Sefficacy_index B_Sefficacy_index2 B_Ssocial_scale B_Sesteem_index_girl B_Sesteem_index_boy B_Sesteem_index2_girl B_Sesteem_index2_girl_ni ///
		B_Sesteem_index2_boy 


local endline_index E_Sgender_index E_Sgender_index2_i_d E_Sgender_index2_i_nd E_Sgender_index2_ni_d E_Sgender_index2_ni_nd ///
		E_Saspiration_index E_Saspiration_index2_i_d E_Saspiration_index2_i_nd E_Saspiration_index2_ni_d E_Saspiration_index2_ni_nd ///
		E_Saspiration_boy_index2_i E_Saspiration_boy_index2_ni ///
		E_Sbehavior_index_g E_Sbehavior_index2_g_i_nv E_Sbehavior_index2_g_ni_nv E_Sbehavior_index2_g_i_nd E_Sbehavior_index2_g_i_d E_Sbehavior_index2_g_ni_nd E_Sbehavior_index2_g_ni_d ///
		E_Sbehavior_index2_b_i_nv E_Sbehavior_index2_b_ni_nv E_Sbehavior_index2_b_i_nd E_Sbehavior_index2_b_i_d E_Sbehavior_index2_b_ni_nd E_Sbehavior_index2_b_ni_d ///
		E_Sbehavior_index2_i_nd E_Sbehavior_index2_i_d E_Sbehavior_index2_ni_nd E_Sbehavior_index2_ni_d ///
		E_Sesteem_index E_Sesteem_index2 ///
		E_Sgender_index_educ E_Sgender_index2_educ_i E_Sgender_index2_educ_ni E_Sgender_index_emp E_Sgender_index2_emp_i E_Sgender_index2_emp_ni E_Sgender_index_sub E_Sgender_index2_sub_i E_Sgender_index2_sub_ni ///
		E_Sbehavior_index_oppsex E_Sbehavior_index2_oppsex_i E_Sbehavior_index2_oppsex_ni ///
		E_Sbehavior_index_hhchores E_Sbehavior_index2_hhchores_i E_Sbehavior_index2_hhchores_ni E_Sbehavior_index_relatives E_Sbehavior_index2_relatives_i E_Sbehavior_index2_relatives_ni ///
		E_Sbehavior_index_decision_g E_Sbehavior_index2_dec_g_i E_Sbehavior_index2_dec_g_ni ///
		E_Sdiscrimination_index E_Sdiscrimination_index2_i E_Sdiscrimination_index2_ni E_Sesteem_index_girl E_Sesteem_index_boy E_Sesteem_index2_girl_i E_Sesteem_index2_girl_ni E_Sesteem_index2_boy 
		
// commented out variables in EL2 are normalized w.r.t to el1 equivalent above. 
local endline2_index E2_Sgender_index E2_Sgender_index2_i_e2_nd E2_Sgender_index2_ni_e2_nd ///
	E2_Sgender_index2_i_e2_d E2_Sgender_index2_ni_e2_d  ///
	E2_Saspiration_index E2_Saspiration_index2_i_nd E2_Saspiration_index2_ni_nd E2_Saspiration_index2_i_d E2_Saspiration_index2_ni_d  ///
	E2_Saspiration_boy_index2_i_nd E2_Saspiration_boy_index2_ni_nd E2_Saspiration_boy_index2_i_d E2_Saspiration_boy_index2_ni_d ///
	E2_Sbehavior_index2_g_i_nd E2_Sbehavior_index2_g_ni_nd E2_Sbehavior_index2_g_i_d E2_Sbehavior_index2_g_ni_d ///
	E2_Sbehavior_index2_b_i_nd E2_Sbehavior_index2_b_ni_nd E2_Sbehavior_index2_b_i_d E2_Sbehavior_index2_b_ni_d ///
	E2_Sbehavior_index2_i_nd E2_Sbehavior_index2_ni_nd E2_Sbehavior_index2_i_d E2_Sbehavior_index2_ni_d ///	
	E2_Sbehavior_index_g  E2_Sbehavior_index_b E2_Sbehavior_index E2_Sbehavior_index_oppsex E2_Sbehavior_index2_oppsex_i E2_Sbehavior_index2_oppsex_ni ///
	E2_Sbehavior_index_hhchores E2_Sbehavior_index2_hhchores_i E2_Sbehavior_index2_hhchores_ni ///
	E2_Sbehavior_index_relatives E2_Sbehavior_index2_relatives_i E2_Sbehavior_index2_relatives_ni ///
	E2_Sbehavior_index_decision_g E2_Sbehavior_index2_dec_g_i E2_Sbehavior_index2_dec_g_ni ///
	E2_Sbehavior_index_mobility_g E2_Sbehavior_index2_mobil_g_i E2_Sbehavior_index2_mobil_g_ni ///
	E2_Sesteem_index2 E2_Sesteem_index2_girl_i E2_Sesteem_index2_girl_ni E2_Seduc_attain_index_g E2_Seduc_attain_index2_g_i E2_Seduc_attain_index2_g_ni  ///
	E2_Smar_fert_asp_index E2_Smar_fert_asp_index2_i E2_Smar_fert_asp_index2_ni E2_Sharassed_index_g E2_Sharassed_index2_g_i E2_Sharassed_index2_g_ni E2_Ssocial_scale
	
foreach x in `baseline_index' `endline_index' `endline2_index' {

		qui summ `x' if B_treat==0
		local mean `r(mean)'
		local sd `r(sd)'

		if `r(N)'>0 {
			replace `x' = `x' - `mean'
			replace `x' = `x'/`sd'
			}
	}


	
**making new variables for analysis


**Fixed effects

	egen gender_grade=group(B_Sgirl B_Sclass)
	tabulate gender_grade, gen(gender_grade_)
	
	egen district_gender=group(B_Sdistrict B_Sgirl)
	tabulate district_gender, gen(district_gender_)

	tabulate B_Sdistrict, gen(district_)			

	**IAT 1 (good and bad)
	gen B_D_measure_goodbad = B_D_measure if E_iat_numb==1
	gen E_D_measure_goodbad = E_D_measure if E_iat_numb==1
	gen B_D_measure_goodbad_neg = B_D_measure_neg if E_iat_numb==1
	gen E_D_measure_goodbad_neg = E_D_measure_neg if E_iat_numb==1

	**IAT 2 (occupation)
	gen B_D_measure_occupation = B_D_measure if E_iat_numb==2
	gen E_D_measure_occupation = E_D_measure if E_iat_numb==2
	gen B_D_measure_occupation_neg = B_D_measure_neg if E_iat_numb==2
	gen E_D_measure_occupation_neg = E_D_measure_neg if E_iat_numb==2

	**labelling variables
	lab var E_D_measure_goodbad_neg "IAT: Associates girls with positive words"
	lab var E_D_measure_occupation_neg "IAT: Associates women with market work"
	
	foreach var in B_D_measure_goodbad B_D_measure_goodbad_neg B_D_measure_occupation B_D_measure_occupation_neg {
	
	gen `var'_flag = mi(`var') // value is missing
	
	tempvar mean
	egen `mean' = mean(`var'), by(B_Sdistrict B_Sgirl)

	replace `var' = `mean' if mi(`var')

	}
	
	**normalising the means of the D-measures
	
	**IAT good vs bad	
	foreach x of varlist *_measure_goodbad *_measure_goodbad_neg {
	

		qui summ `x' if B_treat==0 & E_iat_numb==1
		local mean `r(mean)'
		local sd `r(sd)'

		replace `x' = `x' - `mean'
		replace `x' = `x'/`sd'


	}

	**IAT occupation	
	foreach x of varlist *_measure_occupation *_measure_occupation_neg {
	
		qui summ `x' if B_treat==0 & E_iat_numb==2
		local mean `r(mean)'
		local sd `r(sd)'

		replace `x' = `x' - `mean'
		replace `x' = `x'/`sd'

	}
	
**mean of baseline outcome variables as a control variables

	local base_mean B_Sgender_index2 B_Sgender_index2_ni B_Saspiration_index2 B_Saspiration_boy_index2 B_Saspiration_index2_ni B_Saspiration_boy_index2_ni ///
		B_Sbehavior_index2_g B_Sbehavior_index2_b B_Sbehavior_index2 B_Sbehavior_index2_g_ni B_Sbehavior_index2_b_ni B_Sbehavior_index2_ni ///
		B_Sgender_index2_educ B_Sgender_index2_educ_ni B_Sgender_index2_emp B_Sgender_index2_emp_ni B_Sgender_index2_sub B_Sgender_index2_sub_ni B_Sesteem_index2  ///
		B_Sbehavior_index2_oppsex B_Sbehavior_index2_oppsex_ni B_Sbehavior_index2_mobil_g B_Sbehavior_index2_mobil_g_ni  ///
		B_Sbehavior_index2_hhchores B_Sbehavior_index2_hhchores_ni B_Sbehavior_index2_dec_g B_Sbehavior_index2_dec_g_ni B_Sesteem_index2_girl B_Sesteem_index2_girl_ni B_Sesteem_index2_boy

	foreach var in `base_mean'{
	bys Sschool_id B_Sgirl: egen `var'_m = mean (`var')	
	local varlab: variable label `var'
	label var `var'_m "`varlab' (School-gender Avg)"
		}


		
	foreach var in B_D_measure_goodbad B_D_measure_goodbad_neg {
	bys Sschool_id B_Sgirl: egen `var'_m = mean (`var') if E_iat_numb==1
	}

	foreach var in B_D_measure_occupation B_D_measure_occupation_neg {
	bys Sschool_id B_Sgirl: egen `var'_m = mean (`var') if E_iat_numb==2

	}

**heterogeneity analysis
	
	*1. Gender
	gen treat_girl = B_treat*B_Sgirl
	lab var B_Sgirl "Female"
	lab var treat_girl "Treated*Female"
	
	*2. Baseline parent gender attitude index
	summ B_Pgender_index2, detail
	local median = `r(p50)'
	gen B_Pgender_index2_median = 1 if B_Pgender_index2>`median' & !mi(B_Pgender_index2)
	replace B_Pgender_index2_median = 0 if B_Pgender_index2<=`median' & !mi(B_Pgender_index2)
	
	gen treat_Pgender_index2_m = B_Pgender_index2_median*B_treat
	lab var treat_Pgender_index2_m "Treated*Above median baseline parent attitudes"	 
	
	*3. Baseline self-efficacy index
	summ B_Sefficacy_index2, detail
	local median = `r(p50)'
	gen B_Sefficacy_index2_median = 1 if B_Sefficacy_index2>`median' & !mi(B_Sefficacy_index2)
	replace B_Sefficacy_index2_median = 0 if B_Sefficacy_index2<=`median' & !mi(B_Sefficacy_index2)
	
	gen treat_Sefficacy_index2_m = B_Sefficacy_index2_median*B_treat
	lab var treat_Sefficacy_index2_m "Treated*Above median baseline self-efficacy index"
	lab var B_Sefficacy_index2_median "Above median baseline self-efficacy index"


	*4. Census 2011 village female labor force participation rate
	summ Cfem_lab_part, detail
	local median = `r(p50)'
	gen Cfem_lab_part_median = 1 if Cfem_lab_part>`median' & !mi(Cfem_lab_part)
	replace Cfem_lab_part_median = 0 if Cfem_lab_part<=`median' & !mi(Cfem_lab_part)
	
	gen treat_Cfem_lab_part_m = Cfem_lab_part_median*B_treat
	lab var treat_Cfem_lab_part_m "Treated*Above median female labor force participation rate"
	lab var Cfem_lab_part_median "Above median female labor force participation rate"

	*5. Single-sex school
	*gen single_sex=inlist(B_coed,0) if !mi(B_coed)
	gen treat_coed= B_coed*B_treat
	lab var treat_coed "Treated*School is co-educational"

	gen coed_girl=B_coed*B_Sgirl
	lab var coed_girl "School is co-educational*Female"

	gen treat_coed_girl = B_treat*B_coed*B_Sgirl
	lab var treat_coed_girl "Treated*School is co-educational*Female"
	
	lab var B_coed "School is co-educational"

	*6. Whether the respondent has an older sister or not

	gen B_Solder_sister = 0	
		foreach x of numlist 1/11 {	
		replace B_Solder_sister = 1 if B_Ssib_relation`x' == 5 & B_Ssib_age`x' > B_Sage & inlist(B_Ssib_age`x',.d,.s,.r,.)==0
		replace B_Solder_sister = 1 if B_Sparent_relation`x' == 5 & B_Sparent_age`x' > B_Sage & inlist(B_Sparent_age`x',.d,.s,.r,.)==0

		}

	lab var B_Solder_sister "Student has an older sister"
	gen treat_oldersis = B_treat*B_Solder_sister
	lab var treat_oldersis "Treated*Student has an older sister"

	*6b. Whether the boy is the eldest son

	gen B_Soldest_son = 0 if B_Sgirl==0 // only if respondent boy
	
	tempvar miss
	gen `miss'= 0
	foreach x of numlist 1/11 {	
		replace B_Soldest_son = 1 if B_Ssib_relation`x' == 6 & B_Ssib_age`x' < B_Sage & inlist(B_Ssib_age`x',.d,.s,.r,.)==0 & B_Sgirl==0
		replace B_Soldest_son = 1 if B_Sparent_relation`x' == 6 & B_Sparent_age`x' < B_Sage & inlist(B_Sparent_age`x',.d,.s,.r,.)==0 & B_Sgirl==0
		replace `miss' = 1 if inlist(B_Ssib_age`x',.d,.s,.r) & B_Ssib_relation`x' == 6 & B_Sgirl==0 // any one sibling's age has this value
		}
	
	* if older son, but any sibling has missing age, than not sure about being the oldest
	replace B_Soldest_son = . if `miss'==1 & B_Sgirl==0 & B_Soldest_son==1
		
	tempvar numboys	
	gen `numboys' = 0
	foreach x of numlist 1/11 {	
		replace `numboys' = `numboys' + 1 if B_Ssib_relation`x' == 6 
		}
		
	replace B_Soldest_son = 1 if B_Ssibsize==0 & B_Sgirl==0 // no siblings, and male	
	replace B_Soldest_son = 1 if `numboys'==0 & B_Sgirl==0 // no male siblings, and male
	
		
	*7. Average baseline peer attitudes (Average boys and girls attitudes in the classroom)

	**Boys' attitudes
	egen peer_group=group(Sschool_id B_Sgrade6)
	bys peer_group: gen order=_n

	gen B_peer_gender_index2_b=.

	levelsof peer_group
	
	foreach group in `r(levels)'{
		qui sum order if peer_group==`group'
		foreach n of numlist 1/`r(max)'{
			qui sum B_Sgender_index2 if peer_group==`group' & order!=`n' & B_Sgirl==0
			qui replace B_peer_gender_index2_b=r(mean) if peer_group==`group' & order==`n'
			}
	}


	**Girls' attitudes

	gen B_peer_gender_index2_g=.

	levelsof peer_group
	
	foreach group in `r(levels)'{
		qui sum order if peer_group==`group'
		foreach n of numlist 1/`r(max)'{
			qui sum B_Sgender_index2 if peer_group==`group' & order!=`n' & B_Sgirl==1
			qui replace B_peer_gender_index2_g=r(mean) if peer_group==`group' & order==`n'
			}
	}

	drop peer_group order


	lab var B_peer_gender_index2_b "Average baseline boys' gender attitudes"
	lab var B_peer_gender_index2_g "Average baseline girls' gender attitudes"

	
	**Normalising baseline peer attitudes

	foreach x in B_peer_gender_index2_b B_peer_gender_index2_g {

		qui summ `x' if B_treat==0
		local mean `r(mean)'
		local sd `r(sd)'

		replace `x' = `x' - `mean'
		replace `x' = `x'/`sd'

	}



	**Double interactions
	gen treat_boys_gender = B_treat*B_peer_gender_index2_b
	lab var treat_boys_gender "Treated*Average baseline boys' gender attitudes"

	gen treat_girls_gender = B_treat*B_peer_gender_index2_g
	lab var treat_girls_gender "Treated*Average baseline girls' gender attitudes"

	gen boys_gender_girl = B_peer_gender_index2_b*B_Sgirl
	lab var boys_gender_girl "Average baseline boys' gender attitudes*Female"

	gen boys_gender_boy = B_peer_gender_index2_b*B_Sboy
	lab var boys_gender_boy "Average baseline boys' gender attitudes*Male"

	gen girls_gender_girl = B_peer_gender_index2_g*B_Sgirl
	lab var girls_gender_girl "Average baseline girls' gender attitudes*Female"
	
	gen girls_gender_boy = B_peer_gender_index2_g*B_Sboy
	lab var girls_gender_boy "Average baseline girls' gender attitudes*Male"


	**Triple interactions
	gen treat_girls_attitude_girl = B_treat*B_peer_gender_index2_g*B_Sgirl
	lab var treat_girls_attitude_girl "Treated*Average baseline girls' gender attitudes*Female"

	gen treat_girls_attitude_boy = B_treat*B_peer_gender_index2_g*B_Sboy
	lab var treat_girls_attitude_boy "Treated*Average baseline girls' gender attitudes*Male"

	gen treat_boys_attitude_girl = B_treat*B_peer_gender_index2_b*B_Sgirl
	lab var treat_boys_attitude_girl "Treated*Average baseline boys' gender attitudes*Female"

	gen treat_boys_attitude_boy = B_treat*B_peer_gender_index2_g*B_Sboy
	lab var treat_boys_attitude_boy "Treated*Average baseline boys' gender attitudes*Male"


	**8. Whether the girl has attended Meena Manch

	gen B_Smeena_activ_flag=mi(B_Smeena_activ) if B_Sgirl==1
	qui summ B_Smeena_activ if B_Sgirl==1
	local mean `r(mean)' 
	replace B_Smeena_activ = `mean' if B_Smeena_activ_flag==1

	gen treat_meena = B_treat*B_Smeena_activ
	lab var treat_meena "Treated*Girl has attended any Meena Manch activity"


**program awareness

	gen E_Sprogram_aware=1 if E_Sname_organization==1
	replace E_Sprogram_aware=0 if E_Sname_organization==2
	replace E_Sprogram_aware=0 if E_Sname_organization==.d
	lab var E_Sprogram_aware "Aware of program"

	local program E_Sprogram_aware E_Sattend_session
	
	local program_flag			  
	foreach y in `program' {
			cap confirm variable `y'_flag
			if !_rc{
				replace `y'=. if `y'_flag
				drop `y'_flag
			}
			qui count if mi(`y')
			if r(N)!=0{
				gen `y'_flag=mi(`y') if attrition_el==0 & B_treat==1 
				bys B_Sgirl B_Sdistrict: egen x = mean (`y') if B_treat==1 //imputed for gender-district status only
				qui replace `y'=x if `y'_flag==1
				drop x
				local program_flag `program_flag' `y'_flag
				
			}	
		}		
		

**place of interview
		
		gen E_interview_school=1 if E_Splace_of_interview==1
		replace E_interview_school=0 if E_Splace_of_interview==2 | E_Splace_of_interview==3
		lab var E_interview_school "Survey conducted in school"

			qui count if mi(E_interview_school)
			if r(N)!=0{
				gen E_interview_school_flag=mi(E_interview_school) if attrition_el==0
				bys B_Sgirl B_Sdistrict B_treat: egen x = mean (E_interview_school) if attrition_el==0 
				qui replace E_interview_school=x if E_interview_school_flag==1
				drop x
					
		}

**Social desirability index

	summ B_Ssocial_scale, detail
	gen B_Ssocial_scale_belowm=1 if B_Ssocial_scale<`r(p50)'
	replace B_Ssocial_scale_belowm=0 if B_Ssocial_scale>=`r(p50)' //EDIT: Originally ">=" on this line was ">", so 3,075 obs that == median were missing. 

	lab var B_Ssocial_scale_belowm "Low social desirability score"

	**9. Low social desirability score
	gen B_treat_social_scale = B_Ssocial_scale_belowm*B_treat
	label var B_treat_social_scale "Treated*Low social desirability score"

	
**social norms

	gen E_Ssocial_norms=1 if E_Sallow_college == 1 & E_Scommunity_allow_college == 1
	replace E_Ssocial_norms=2 if E_Sallow_college == 1 & E_Scommunity_allow_college == 0
	replace E_Ssocial_norms=3 if E_Sallow_college == 0 & E_Scommunity_allow_college == 1
	replace E_Ssocial_norms=4 if E_Sallow_college == 0 & E_Scommunity_allow_college == 0
	replace E_Ssocial_norms=1 if E_Sallow_work_y == 1 & E_Scommunity_allow_work == 1
	replace E_Ssocial_norms=2 if E_Sallow_work_y == 1 & E_Scommunity_allow_work == 0
	replace E_Ssocial_norms=3 if E_Sallow_work_y == 0 & E_Scommunity_allow_work == 1
	replace E_Ssocial_norms=4 if E_Sallow_work_y == 0 & E_Scommunity_allow_work == 0

	lab var E_Ssocial_norms "Social norms"

	lab def social 1 "Match positive social norm" 2 "Self positive-Society negative" 3 "Self negative-Society positive" 4 "Match negative social norm"
	lab values E_Ssocial_norms social


**Asset Index

	local asset B_Shouse_pukka_y B_Shouse_elec B_Sflush_toilet B_Snonflush_toilet ///
		B_Sown_house B_Phh_durables_1 B_Phh_durables_2 B_Phh_durables_7 B_Snewspaper_house B_Stap_water B_Phh_durables_16


	cap drop check
	egen B_asset_index=rowtotal(`asset'), mi
	egen check=rownonmiss(`asset')

	tab check
	qui sum check
	replace B_asset_index=. if check<`r(max)'   
	replace B_asset_index=B_asset_index/`r(max)'
	drop check
	
** Weighted Baseline Asset Index
	
	local vars
	foreach var in `asset' {
			gen `var'_temp=`var'
			sum `var'_temp
				local mean=r(mean)
			sum `var'_temp if B_treat==0
			replace `var'_temp=(`var'_temp-`mean')/`r(sd)'
			local vars `vars' `var'_temp
			}
	
	* Weight normalized vars by the inverse of the covariance matrix of the normalized vars-- 
	* Set the weight=sum of row entries in the inverted covariance matrix 
	
		dis "`vars'"
		corrmat `vars', covmat(A)			// install corrmat function - saves cov matrix
		matrix Ainv=inv(A)										// inverse of A
		mat list Ainv
		mata: st_matrix("B", rowsum(st_matrix("Ainv")))		// sum of rows 		

				
	local i=0
		foreach x in `vars' {
		local ++i
		gen sum_`i' = B[`i',1]*`x'
		}
		
		egen B_asset_index2=rsum2(sum_*), anymiss
		
	cap drop *_temp
	cap drop sum_*
		
	lab var B_asset_index "Baseline unweighted asset index"
	lab var B_asset_index2 "Asset index"

	**Normalising asset index

	foreach x in B_asset_index B_asset_index2 {

		qui summ `x' if B_treat==0
		local mean `r(mean)'
		local sd `r(sd)'

		replace `x' = `x' - `mean'
		replace `x' = `x'/`sd'

	}

	* Controls to be used for Endline 2 analysis
	#delimit ;
	
	global ctrls B_Scaste_sc B_Scaste_st B_Smuslim B_Shh_size B_no_female_sib B_no_male_sib B_Sparent_stay
		B_Shouse_pukka_y B_Shouse_elec B_Sflush_toilet B_Snonflush_toilet B_Sown_house 
		B_Phh_durables_1 B_Phh_durables_2 B_Phh_durables_7 B_Snewspaper_house B_Stap_water B_Phh_durables_16
		B_Sefficacy_index2
		B_Ssocial_scale
		B_Schild_elect_woman_y E2_Select_woman_y B_q10_guest_teachr B_fulltime_teacher 
		B_pct_female_teacher B_q12_activity_teachr B_q13_counselor B_q18_pta_meet 
		B_q21_week1 B_q21_week2 B_q21_week3 B_q21_week4 B_q21_week6 B_q21_week7
		B_q22_library B_q22_toilets B_q22_electricity B_q22_avail_computers B_q22_avail_internet
		B_q22_sports_field B_q22_mid_meal B_q22_auditorium B_q22_avail_edusat
		B_coed B_rural
		Cfem_lit_rate Cmale_lit_rate Cfem_lab_part;
		
	#delimit cr
	
	***save dataset

sort Sschool_id child_id 
label data "Merged baseline, endline, census data (Breakthrough)"


cap drop __000*

foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}

* main variables - generation and labelling. 
		// final spec: no impute + no drop + EL1 weights for gender + Missing flags in regression
local E2_Sgender_index2 E2_Sgender_index2_ni_e1_nd
local E2_Saspiration_index2 E2_Saspiration_index2_ni_nd
local E2_Sbehavior_index2 E2_Sbehavior_index2_ni_nd
local E2_Sbehavior_index2_g E2_Sbehavior_index2_g_ni_nd
local E2_Sbehavior_index2_b E2_Sbehavior_index2_b_ni_nd

local E_Sgender_index2 E_Sgender_index2_ni_nd
local E_Saspiration_index2 E_Saspiration_index2_ni_nd
local E_Sbehavior_index2 E_Sbehavior_index2_ni_nd
local E_Sbehavior_index2_g E_Sbehavior_index2_g_ni_nd
local E_Sbehavior_index2_b E_Sbehavior_index2_b_ni_nd

foreach index in gender_index2 aspiration_index2 behavior_index2 behavior_index2_g behavior_index2_b {
		* label 
		if strpos("`index'", "gender") >0 local l = "Gender attitudes index"
		if strpos("`index'", "aspiration") >0 local l = "Girls' aspirations index"
		if strpos("`index'", "behavior") >0 local l =  "Self-reported behavior index"
		if strpos("`index'", "_g_") >0 local l =  "Girls' behavior index"
		if strpos("`index'", "_b_") >0  local l =  "Boys' behavior index"

	foreach e in E2_S E_S {
		gen `e'`index' = ``e'`index''
			la var `e'`index' "`l'"	
		}	
	
	* baseline renaming
	rename B_S`index' B_S`index'_i
	rename B_S`index'_m B_S`index'_i_m
	rename B_S`index'_ni B_S`index'
	rename B_S`index'_ni_m B_S`index'_m
	
	foreach b in B_S {
		la var `b'`index' "`l'"	
		la var `b'`index'_m "`l' (school-gender mean)"	
	}	
	
	}
	
	
** sub-indices renaming
*only El1
gen E_Sdiscrimination_index2 = E_Sdiscrimination_index2_ni
local l : var lab E_Sdiscrimination_index2_ni
la var E_Sdiscrimination_index2 "`l'"

* only El2
foreach index in educ_attain_index2_g mar_fert_asp_index2 harassed_index2_g {
	foreach e in E2_S {
		gen `e'`index' = `e'`index'_ni
		local l: var lab   `e'`index'_ni
		la var `e'`index'  "`l'"
		}
	}
	
* both EL1 and EL2	
foreach index in gender_index2_educ gender_index2_emp gender_index2_sub gender_index2_fert ///
	behavior_index2_oppsex behavior_index2_hhchores behavior_index2_relatives behavior_index2_dec_g behavior_index2_mobil_g esteem_index2_girl {

	foreach e in E2_S E_S {
		gen `e'`index' = `e'`index'_ni
		local l: var lab   `e'`index'_ni
		la var `e'`index'  "`l'"
		}	
		
	cap confirm var B_S`index'
	
	* variable exists; changing non-imputed index var name
	if _rc==0{
		rename B_S`index' B_S`index'_i
		rename B_S`index'_m B_S`index'_i_m
		rename B_S`index'_ni B_S`index'	
		rename B_S`index'_ni_m B_S`index'_m
		}	
	
	}	



	
saveold  "$finaldata", replace

*************************************************
*  Generating Endline 2 school averages dataset *
*************************************************

gen num_students = 1 
#delimit ;

local endline2_outcomes 

	E2_Swives_less_edu_n E2_Sboy_more_oppo_n E2_Stown_studies_y 
	E2_Swoman_role_home_n E2_Smen_better_suited_n E2_Smarriage_more_imp_n 
		E2_Steacher_suitable_n E2_Sallow_work
	E2_Ssimilar_right_y	E2_Select_woman_y E2_Sman_final_deci_n E2_Swoman_viol_n 	
		E2_Scontrol_daughters_n E2_Sstudy_marry E2_Sshy E2_Slaugh
		E2_Sgirl_marriage_age_19 E2_Smarriage_age_diff_m
	E2_Sfertility E2_Stwel_score_exp_m E2_Sdiscuss_educ E2_Scont_educ E2_Shighest_educ_m
	E2_Soccupa_25_y E2_Splan_college E2_Scol_course_want_y E2_Scol_course_want_stem E2_Scont_have_job_y
	E2_Sfuture_work E2_Sdec_past_twel_y E2_Sdecision_work_y E2_Sdecision_kindofwork_y E2_Sattendance 
		E2_Salone_friend  E2_Salone_market E2_Salone_events E2_Salone_past_week E2_Salone_school_y 	
		E2_Stalk_opp_gen_g E2_Ssit_opp_gen_g E2_Sfriend_opp_gen_g E2_Splay_opp_gen_g 
		E2_Stalk_week_opp_gen_g E2_Scook_clean_g E2_Sabsent_sch_hhwork_g E2_Sdisc_col_g E2_Sdisc_work_g ;
	
#delimit cr

foreach v in `endline2_outcomes' {
	local l`v' : variable label `v'
	if `"`l`v''"' == "" {
 		local l`v' "`v'"
  	}
}

collapse (sum) num_students (mean) `endline2_outcomes', by(Sschool_id)

foreach v in `endline2_outcomes' {

	label var `v' "`l`v''"
	
	}

label var num_students "Number of students"

save "$deid/el2_school_average", replace

*************************************************
*  Generating Endline 2 Raw Data (without PII   *
*************************************************

use "$deid/endline2_student_raw"
drop *_id
save "$deid/endline2_jpal", replace

/*
Poke around IPA/J-Pal website to figure out what can be used as PII, go through with a fine tool comb, do this for EL1 and EL2, be extra careful 
*/
