
version 17

*******************
*Files and folders*
*******************

global main_loc "C:/Users/toreare/Dropbox/PhD/replication games" 
global do "$main_loc/Main Analysis and Paper/do_files"
adopath + "${do}/_ado"
global deid "$main_loc/Main Analysis and Paper/Analysis data"
global finaldata "$deid/bt_analysis_final"
global mytables "${main_loc}/mytables"

*****************
*Control globals*
*****************
global el_gender_flag "E_Swives_less_edu_n_flag E_Select_woman_y_flag E_Sboy_more_oppo_n_flag E_Stown_studies_y_flag E_Sman_final_deci_n_flag E_Swoman_viol_n_flag E_Scontrol_daughters_n_flag E_Swoman_role_home_n_flag E_Smen_better_suited_n_flag E_Ssimilar_right_y_flag E_Smarriage_more_imp_n_flag E_Steacher_suitable_n_flag E_Sgirl_marriage_age_19_flag E_Smarriage_age_diff_m_flag E_Sstudy_marry_flag E_Sallow_work_y_flag E_Sfertility_flag"
global el_aspiration_flag "E_Sboard_score_median_flag E_Shighest_educ_median_flag E_Sdiscuss_educ_flag E_Soccupa_25_white_flag E_Scont_educ_flag"
global el_behavior_common_flag "E_Stalk_opp_gender_comm_flag E_Ssit_opp_gender_comm_flag E_Scook_clean_comm E_Sabsent_sch_hhwork_comm_flag E_Sdiscourage_college_comm_flag E_Sdiscourage_work_comm_flag"
global el_esteem_girl_flag "E_Ssatisfy_y_flag E_Sgood_qly_y_flag E_Sable_do_most_y_flag"
global el_discrimination_flag "E_Sfemale_foeticide_flag E_Sfemale_foeticide_state_flag E_Sfemale_foeticide_r_y_flag E_Sgirls_less_y_flag"
global el2_gender_flag "E2_Swives_less_edu_n_flag E2_Sboy_more_oppo_n_flag E2_Stown_studies_y_flag  E2_Swoman_role_home_n_flag E2_Smen_better_suited_n_flag E2_Smarriage_more_imp_n_flag   E2_Steacher_suitable_n_flag E2_Sallow_work_flag E2_Ssimilar_right_y_flag  E2_Select_woman_y_flag E2_Sman_final_deci_n_flag E2_Swoman_viol_n_flag   E2_Scontrol_daughters_n_flag E2_Sstudy_marry_flag E2_Sgirl_marriage_age_19_flag   E2_Smarriage_age_diff_m_flag E2_Sfertility_flag"
global el2_aspiration_flag "E2_Stwel_score_exp_m_flag E2_Sdiscuss_educ_flag E2_Scont_educ_flag  E2_Shighest_educ_m_flag E2_Soccupa_25_y_flag E2_Splan_college_flag E2_Scol_course_want_y_flag  E2_Scol_course_want_stem_flag E2_Scont_have_job_y_flag"
global el2_behavior_common_flag "E2_Stalk_opp_gen_comm_flag E2_Ssit_opp_gen_comm_flag E2_Sfriend_opp_gen_comm_flag E2_Splay_opp_gen_comm_flag E2_Stalk_week_opp_gen_comm_flag  E2_Scook_clean_comm_flag E2_Sabsent_sch_hhwork_comm_flag E2_Sdisc_col_comm_flag E2_Sdisc_work_comm_flag"
global el2_esteem_girl_flag "E2_Ssatisfy_y_flag E2_Sgood_qly_y_flag E2_Sable_do_most_y_flag"
global el2_educ_attain_flag "E2_Sschool_enrol_y_flag E2_Sstem_stream_y_flag  E2_Senrol_eng_comp_voc_y_flag E2_Stake_tuition_y_flag"
global el2_harassed_flag "E2_Sharass_slapped_y_g_flag E2_Steas_opp_gen_boy_scl_y_flag E2_Steas_opp_gen_boy_vil_y_flag  E2_Stch_opp_gen_boy_scl_y_flag E2_Stch_opp_gen_boy_vil_y_flag"

********************
*Number of decimals*
********************
global decimals ="4" //Adjust this value to change the number of decimals printed
global fmt = "%`=${decimals}+1'.${decimals}f"
global rounding = "."
loc dec = $decimals
forv n=1/`=`dec'-1' {
	di "n"
	global rounding = "${rounding}0"
}
global rounding = "${rounding}1"
di "$rounding"

cap prog drop myround
prog myround
	syntax, NAME(namelist)
	loc str = strofreal(round(r(`name'),${rounding}))
	
	//Move zero to separate macro
	if strpos("`str'","-")!=0 loc minus = "-"
	di "minus: `minus'"
	loc str = subinstr("`str'","-","",.)
	if strpos("`str'",".")==0 {
		loc str = "`minus'0.`str'"
	}
	else {
		loc str = "`minus'0`str'"
	}
	loc len = strlen(subinstr("`str'","-", "",.))
	while `len'<$decimals+2 {
		loc str="`str'0"
		loc len=`len'+1
	}
	global tmp_val = "`str'"
end

********************
*Show P-value or SE*
********************
global p_se = "se" //Change to "p" to print p-values insted of SE's

*******************************
*Generate latex header strings*
*******************************

cap prog drop table_head
prog table_head
	syntax, COLumns(integer)
	global table_head = `"\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{l*{`=`columns'+1'}{c}}\hline\hline"'
end

cap prog drop latex_header_mistakes
prog latex_header_mistakes
	syntax, TABLE(integer)
	
	assert `table'==6 | `table'==12 | `table'==13
	
	if `table'==6 | `table'==12 {
		global header	 = `""\shortstack{Women\\should be\\allowed\\to work}" "\shortstack{Community\\thinks\\women\\should be\\allowed to\\work}" "\shortstack{Women\\should be al-\\lowed to work\\and thinks\\community\\will not op-\\pose them}" "\shortstack{Women\\should be\\allowed to\\study in\\college even if it \\is far away}" "\shortstack{Community\\thinks women\\should be\\allowed to\\study in col-\\lege even if it\\is far away}" "\shortstack{Women should\\be allowed\\to study in\\college and\\thinks commu-\\nity will not\\oppose them}"  "'
		table_head, columns(6)
		global preheader =  `"prehead("{${table_head} \vspace{-.7em}\\ & \multicolumn{3}{c}{Social norms toward work} & \multicolumn{3}{c}{Social norms toward education} \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}  & \multicolumn{3}{c}{Student agrees:} & \multicolumn{3}{c}{Student agrees:} \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}") "'
		global numbers = `"& (1) & (2) & (3) & (4) & (5) & (6) \\ \hline"'
	}
	if `table'==13 {
		global header	 = `""\shortstack{Girls'\\self-esteem}" "\shortstack{Girls'\\education}" "\shortstack{Marriage\\and fertility\\aspirations\\(girls)}" "\shortstack{Marriage\\and fertility\\aspirations\\(boys)}" "\shortstack{Girls'\\experienced\\harassment}" "\shortstack{Boys'\\perpetrated\\harassment\\(school-grade\\level)}" "'
		global preheader = ""
		global numbers = `"& (1) & (2) & (3) & (4) & (5) & (6) \\ \hline"'
	}
	
end


*********
*Table 6*
*********
//One mistake in the original code (line 1317)
	// Boys regression condition on non-missing steam ID and no attrition at _endline 2_
	// Furthermore: The same code is used to estimate EL2 in table 12, where the girls regression is wrong (line 1230)
use "$finaldata", clear
label var B_treat "Treated"

estimates clear

loc n=1
foreach var of varlist E_Sallow_work_s E_Scommunity_allow_work_s E_Scommunity_work_s E_Sallow_college_s E_Scommunity_allow_college_s E_Scommunity_college_s {
	eststo b`n'    :qui reg `var' B_treat district_? B_Sgrade6 if !B_Sgirl & /*!mi(E2_Steam_id) &*/ attrition==0 , cluster(Sschool_id)
	qui sum `var' if !B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
	myround, name(mean) 
	qui estadd local control_group_mean = "$tmp_val"
	if `n'==1 estadd local pval_joint_hypothesis "`joint_pval2'"
	loc n=`n'+1
}

	//Boys regression - corrected
loc n=1
foreach var of varlist E_Sallow_work_s E_Scommunity_allow_work_s E_Scommunity_work_s E_Sallow_college_s E_Scommunity_allow_college_s E_Scommunity_college_s {
	eststo b`n'_corrected    :qui reg `var' B_treat district_? B_Sgrade6 if !B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
	qui sum `var' if !B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0 
	myround, name(mean) 
	estadd local control_group_mean = "$tmp_val"
	qui count if !B_Sgirl & !mi(`var') & !mi(E_Steam_id) & attrition_el==0 & !(!mi(E2_Steam_id) & attrition==0)
	estadd local obs_dropped = r(N)
	if `n'==1 estadd local pval_joint_hypothesis "`joint_pval2'"
	loc n=`n'+1
}


//Tabulate
	
esttab b?  , ///
	keep(B_treat _cons) ///
	label stats(control_group_mean N /*r2 pval_joint_hypothesis*/, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" /*"\$R^2$" "P-value, joint hypothesis"*/ )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	
	
esttab b?_corrected  , ///
	keep(B_treat _cons) ///
	label stats(control_group_mean N obs_dropped /*r2 pval_joint_hypothesis*/, fmt(${fmt} %9.0fc /*%9.0fc*/ ${fmt}) label("Control group mean" "Number of students" "Observations dropped in authors regression" /*"\$R^2$" "P-value, joint hypothesis"*/ )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	
	
	
//Store
latex_header_mistakes, table(6)	

esttab b? using "${mytables}/mistakes/table6_boys.tex", replace ///
	keep(B_treat _cons) ///
	label stats(control_group_mean N /*r2 pval_joint_hypothesis*/, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" /*"\$R^2$" "P-value, joint hypothesis"*/ )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	///
	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")
	
	
esttab b?_corrected   using "${mytables}/mistakes/table6_boys_corrected.tex", replace ///
	keep(B_treat _cons) ///
	label stats(control_group_mean N obs_dropped /*r2 pval_joint_hypothesis*/, fmt(${fmt} %9.0fc /*%9.0fc*/ ${fmt}) label("Control group mean" "Number of students" "Observations dropped in authors regression" /*"\$R^2$" "P-value, joint hypothesis"*/ )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	///
	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")


**********
*Table 12*
**********

use "$finaldata", clear
label var B_treat "Treated"

//Girls regression - with authors mistake
loc n=1
foreach var of varlist E2_Sallow_work_s E2_Scommunity_allow_work_s E2_Scommunity_work_s E2_Sallow_college_s E2_Scommunity_allow_college_s E2_Scommunity_college_s {
	eststo g`n' :qui reg `var' B_treat district_? B_Sgrade6  ///
		if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
	qui sum `var' if B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0
	myround, name(mean) 
	estadd local control_group_mean = "$tmp_val"
	//estadd mat robust_p=girls`n'
	if `n'==1 estadd local pval_joint_hypothesis "`joint_pval1'"
	loc n=`n'+1
}

//Girls regression  - corrected
loc n=1
foreach var of varlist E2_Sallow_work_s E2_Scommunity_allow_work_s E2_Scommunity_work_s E2_Sallow_college_s E2_Scommunity_allow_college_s E2_Scommunity_college_s {
	eststo g`n'_corrected :qui reg `var' B_treat district_? B_Sgrade6  ///
		if B_Sgirl & !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
	qui sum `var' if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
	myround, name(mean) 
	estadd local control_group_mean = "$tmp_val"
	qui count if B_Sgirl & !mi(`var') &  (!mi(E2_Steam_id) & attrition==0) & !(!mi(E_Steam_id) & attrition_el==0)
	estadd local obs_dropped = r(N)
	//estadd mat robust_p=girls_corrected`n'
	if `n'==1 estadd local pval_joint_hypothesis "`joint_pval2'"
	loc n=`n'+1
}

//Tabulate
esttab g?, keep(B_treat _cons) ///
	label stats(control_group_mean N /*r2 pval_joint_hypothesis*/, fmt(${fmt} %9.0fc /*${fmt}*/) 				    label("Control group mean" "Number of students" /*"R2" "P-value, joint hypothesis"*/ )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)

esttab g?_corrected, keep(B_treat _cons) ///
	label stats(control_group_mean N /*obs_dropped r2 pval_joint_hypothesis*/, fmt(${fmt} %9.0fc /*%9.0fc ${fmt}*/) label("Control group mean" "Number of students" /*"Observations dropped in authors regression" "R2" "P-value, joint hypothesis" */)) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)	

//Store 
latex_header_mistakes, table(12)	
esttab g? using "${mytables}/mistakes/table12_girls.tex", replace ///
	keep(B_treat _cons) ///
	label stats(control_group_mean N /*r2 pval_joint_hypothesis*/, fmt(${fmt} %9.0fc /*${fmt}*/) 				    label("Control group mean" "Number of students" /*"R2" "P-value, joint hypothesis"*/ )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none) ///
	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")
	
esttab g?_corrected using "${mytables}/mistakes/table12_girls_corrected.tex", replace ///
	keep(B_treat _cons) ///
	label stats(control_group_mean N /*obs_dropped r2 pval_joint_hypothesis*/, fmt(${fmt} %9.0fc /*%9.0fc ${fmt}*/) label("Control group mean" "Number of students" /*"Observations dropped in authors regression" "R2" "P-value, joint hypothesis" */)) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)	 ///
	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")

**********
*Table 13*
**********
//Run regs 
use "$finaldata", clear
label var B_treat "Treated"
bys Sschool_id B_Sgrade6: gen uniq_scl_grade = _n==1 
bys Sschool_id B_Sgrade6: egen att_max = min(attrition) // if 1 then should be dropped.
gen E2_Smar_fert_asp_index2_g = E2_Smar_fert_asp_index2 if B_Sgirl == 1
la var E2_Smar_fert_asp_index2_g "Marriage and fertility aspirations (Girls)"
gen E2_Smar_fert_asp_index2_b = E2_Smar_fert_asp_index2 if B_Sgirl == 0
la var E2_Smar_fert_asp_index2_b "Marriage and fertility aspirations (Boys)"
egen E2_Slr = max(E2_Slr_harass_b_sch_grad), by(Sschool_id B_Sgrade6)

estimates clear

eststo t1: reg E2_Sesteem_index2_girl  B_treat /*B_Sesteem_index2_girl*/ district_gender_* gender_grade_* ${el2_esteem_girl_flag}  if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1 , cluster(Sschool_id) //They removed the baseline controll by a mistake
qui sum E2_Sesteem_index2_girl if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"

eststo t1_corrected: reg E2_Sesteem_index2_girl  B_treat B_Sesteem_index2_girl district_gender_* gender_grade_* ${el2_esteem_girl_flag}  	if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1 , cluster(Sschool_id) 
qui sum E2_Sesteem_index2_girl if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd local pval_joint_hypothesis "`joint_pval'"

eststo t2: reg E2_Seduc_attain_index2_g B_treat district_gender_* gender_grade_* ${el2_educ_attain_flag} 						if !mi(E2_Steam_id) & attrition==0  & B_Sgirl==1 , cluster(Sschool_id)
qui sum E2_Seduc_attain_index2_g if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"

eststo t3: reg E2_Smar_fert_asp_index2_g B_treat district_gender_* gender_grade_* 												if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Smar_fert_asp_index2_g if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"

eststo t4: reg E2_Smar_fert_asp_index2_b B_treat district_gender_* gender_grade_* if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
sum E2_Smar_fert_asp_index2_b  if B_treat==0 & !mi(E2_Steam_id) & attrition==0  //if !B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = substr("$tmp_val",2,.)

eststo t4_corrected: reg E2_Smar_fert_asp_index2_b B_treat district_gender_* gender_grade_* 												if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
sum E2_Smar_fert_asp_index2_b  if B_treat==0 & !mi(E2_Steam_id) & attrition==0  //if !B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"

eststo t5: reg E2_Sharassed_index2_g B_treat district_gender_* gender_grade_* ${el2_harassed_flag} 								if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Sharassed_index2_g if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"

eststo t6: reg E2_Slr B_treat B_Sgrade6 district_?  																			if  att_max==0 & uniq_scl_grade==1, cluster(Sschool_id)
qui sum E2_Slr if !B_treat &  att_max==0 & uniq_scl_grade==1
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"

esttab t?, keep(B_treat _cons) ///
	label stats(control_group_mean N /*r2 pval_joint_hypothesis*/, fmt(${fmt} %9.0fc /*${fmt}*/) label("Control group mean" "Number of students" /*"R2" "P-value, joint hypothesis"*/ )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) /*robust_p(fmt(${decimals}) par(`"["' `"]"'))*/)  nonumbers collabels(none)
	
esttab t1_corrected t2 t3 t4_corrected t5 t6, keep(B_treat _cons) ///
	label stats(control_group_mean N /*r2 pval_joint_hypothesis*/, fmt(${fmt} %9.0fc /*${fmt}*/) label("Control group mean" "Number of students" /*"R2" "P-value, joint hypothesis"*/ )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) /*robust_p(fmt(${decimals}) par(`"["' `"]"'))*/)  nonumbers collabels(none)
	
//Store
latex_header_mistakes, table(13)	
esttab t? using "${mytables}/mistakes/table13.tex", replace ///
	keep(B_treat _cons) ///
	label stats(control_group_mean N /*r2 pval_joint_hypothesis*/, fmt(${fmt} %9.0fc /*${fmt}*/) label("Control group mean" "Number of students" /*"R2" "P-value, joint hypothesis"*/ )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) /*robust_p(fmt(${decimals}) par(`"["' `"]"'))*/)  nonumbers collabels(none) ///
	$preheader 	///
	mtitle(${header}) nonumbers posthead("${numbers}")	
	
esttab t1_corrected t2 t3 t4_corrected t5 t6  using "${mytables}/mistakes/table13_Corrected.tex", replace ///
	keep(B_treat _cons) ///
	label stats(control_group_mean N /*r2 pval_joint_hypothesis*/, fmt(${fmt} %9.0fc /*${fmt}*/) label("Control group mean" "Number of students" /*"R2" "P-value, joint hypothesis"*/ )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) /*robust_p(fmt(${decimals}) par(`"["' `"]"'))*/)  nonumbers collabels(none) ///
	$preheader 	///
	mtitle(${header}) nonumbers posthead("${numbers}")






esttab t? using "${mytables}/table13.tex", replace ///	
	keep(B_treat _cons) ///
	label stats(/*control_group_mean*/ N r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc ${fmt}) label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	///
	$preheader 	///
	mtitle(${header}) nonumbers posthead("${numbers}")