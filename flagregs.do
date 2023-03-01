ka
// The code outputs first summary stats for each flag. Then, a regression is run on each flag (separately for each of the three indices) of flags from the *first endline" (equal to 1 if no survey value was registered on that indicator for that child) on that index at baseline.

// I.e. the baseline index is used as INDEPENDENT (x) variable, while the missing flags at endline 1 is used as DEPENDENT variable (y)
// Not sure if this is what you had in mind, but it makes sense as a test of non-random missing values I think. At baseline, there are relatively few flags; if e.g. more "regressive" attitudes make students less likely to answer at the endline, the regressions should show a NEGATIVE relationship between baseline-attitudes and missing flags at endline 1

global main_loc "C:\Users\erlendmf\Downloads\22feb_rep" //insert path where "Main Analysis and Paper" is stored
global deid "$main_loc/Main Analysis and Paper/Analysis data"
global finaldata "$deid/bt_analysis_final"


use "$finaldata", clear



// First, simple summary stats which show more endline obs. are missing, and some indices are missing more than others

tabstat B*flag, stat(n mean sd min max) col(stat) varwidth(30)
tabstat E*flag, stat(n mean sd min max) col(stat) varwidth(30)
tabstat E2*flag, stat(n mean sd min max) col(stat) varwidth(30)


//The two loops below I got from 04a_merge...
//making below loop work
foreach var in B_Sgirl B_Sgrade6 B_Ssocial_scale_int_imp B_Ssocial_scale_belowm B_Ssocial_scale  {
	gen `var'_flag = mi(`var')
}

gen all_flag = 0 // 0 if all variables in the list missing
foreach var in Sage Sgirl Shindu Sgrade6 Scaste_sc mom_age dad_age m_illiterate m_fulltime Sflush_toilet Sgender_index2 Saspiration_index2 Sbehavior_index2 ///
Ssocial_scale Ssocial_scale_int_imp {     
	replace all_flag = 1 if B_`var'_flag==0 
}



///GENDER INDEX 
local gender E_Swives_less_edu_n E_Select_woman_y E_Sboy_more_oppo_n E_Stown_studies_y ///
		E_Sman_final_deci_n E_Swoman_viol_n E_Scontrol_daughters_n E_Swoman_role_home_n ///
		E_Smen_better_suited_n E_Ssimilar_right_y ///
		E_Smarriage_more_imp_n E_Steacher_suitable_n E_Sgirl_marriage_age_19 ///
		E_Smarriage_age_diff_m E_Sstudy_marry E_Sallow_work_y E_Sfertility
	

foreach v of local gender{
	reg `v'_flag B_Sgender_index2
		 estimates store gn_`v'
}
coefplot gn*, drop(_cons) graphregion(color(white)) bgcolor(white)
tabstat    E_Swives_less_edu_n E_Select_woman_y E_Sboy_more_oppo_n E_Stown_studies_y ///
		E_Sman_final_deci_n E_Swoman_viol_n E_Scontrol_daughters_n E_Swoman_role_home_n ///
		E_Smen_better_suited_n E_Ssimilar_right_y ///
		E_Smarriage_more_imp_n E_Steacher_suitable_n E_Sgirl_marriage_age_19 ///
		E_Smarriage_age_diff_m E_Sstudy_marry E_Sallow_work_y E_Sfertility, stat(n mean sd min max) col(stat) varwidth(30)
	
	
// ASPIRATION INDEX	
	
local aspiration E_Sboard_score_median E_Shighest_educ_median E_Sdiscuss_educ E_Soccupa_25_white E_Scont_educ	
	
foreach v of local aspiration{
	reg `v'_flag B_Saspiration_index2
		 estimates store asp_`v'

}

coefplot asp*, drop(_cons) graphregion(color(white)) bgcolor(white)


//BEHAVIOR INDEX note: some est sto names are too long
rename E_Sabsent_sch_hhwork_comm E_Sabsent_sch_hh // for length
rename E_Sabsent_sch_hhwork_comm_flag E_Sabsent_sch_hh_flag // for length
rename E_Sdiscourage_college_comm E_Sdisco_college_comm
rename E_Sdiscourage_college_comm_flag E_Sdisco_college_comm_flag


local behavior_common E_Stalk_opp_gender_comm E_Ssit_opp_gender_comm E_Scook_clean_comm ///
					E_Sabsent_sch_hh E_Sdisco_college_comm E_Sdiscourage_work_comm
					
foreach v of local behavior_common{
	reg `v'_flag B_Sbehavior_index2
		 estimates store b_`v'
		
}				

 // esttab *
 
coefplot gn* asp* b*, drop(_cons) graphregion(color(white)) bgcolor(white) legend(off)