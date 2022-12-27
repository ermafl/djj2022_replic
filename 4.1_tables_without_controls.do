
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

cap log close 
log using "${mytables}/no_controls.log", replace

**********************************
*Program for storing MHT p-values*
**********************************
cap prog drop store_mht_se
prog def store_mht_se
	syntax, varname(varname) [MATname(namelist)] 
	if "`matname'"=="" loc matname= "p"
	mat tmp = r(results)
	//loc rows = rowsof(tmp)
	forv n=1/`=rowsof(tmp)' {
		mat `matname'`n' = tmp[`n',4]
		mat colname `matname'`n' = "`varname'" 
	}
end

*****************
*Randomize seeds*
*****************

//Seed aquired from Random.org: 
	//https://www.random.org/integers/?num=1&min=1&max=214748364&col=1&base=10&format=html&rnd=new

//Random seeds for bootstrapping	
global random_seed "102075515	170841344	172432657	82076506	163642564	72797638	181211043	167797199	188546864	107994139	133340171	141296361	108922382	204827031	118836119	179194323	182776537	6300166	128374154	4497275	158055856	21577597	25939074	204766250	130367613	61341796	209874868	108761437	76636140	206102444	173564191	43495134	39972196	91344813	58253079	157484611	61712907	107897201	155209652	167124813	34014374	202906653	89633861	118377608	110422756	164033491	81888101	86360756	26039804	141496683	174140096	146463707	166480687	131983470	57791298	206130600	160369143	169228059	152426742	62612691	125144812	193023603	128163393	58107287	103354231	149266246	4201921	142297234	154268305	25649044	4761	172796628	21216720	203859534	91858581	91556504	159108049	129915838	4636130	28140400	122054095	70977930	33625832	16839283	38843095	89553047	70483483	88869324	2293861	5773142	130858621	192910700	133794059	16138172	84918645	161430810	29024646	206019352	32213302	182668116"

cap prog drop next_seed
prog next_seed
	syntax, [RESET]
	if "`reset'"!="" global internal_seed "$random_seed"
	assert "$internal_seed"!=""
	gettoken (global) seed (global) internal_seed : (global) internal_seed
	
end

	
******************************
*Number of decimals in tables*
******************************
global decimals ="4"
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
	syntax, [NAME(namelist) GLOBAL(namelist)]
	if "`name'"=="" & "`global'"=="" {
		di as error `"Option "name" or "global" required "'
		exit
	}
	if "`name'"!="" & "`global'"!="" {
		di as error `"cannot specify both options "name" and "global" "'
		exit
	}
	if "`name'"!="" {
		loc str = strofreal(round(r(`name'),${rounding}))
	}
	else if "`global'"!="" {
		if "${`global'}"=="" {
			di as error `"Global "`global'" empty "'
			exit
		}
		loc str = strofreal(round(${`global'},${rounding}))
	}
	di "str `str'"
	
	
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
global p_se = "p"

*****************************************
*Set the number of bootstrap replication*
*****************************************
global bootstrap_replication = "10000" //Add number to reduce number of replications. 

if "${bootstrap_replication}"=="" {
	global mhtreg_bootstrap = ""
	global randcmd_bootstrap = ""
}
else {
	global mhtreg_bootstrap = "bootstrap(${bootstrap_replication})"
	global randcmd_bootstrap = "reps(${bootstrap_replication})"
}

di "$mhtreg_bootstrap"
di "$randcmd_bootstrap"


****************************
*Generate latex header code*
****************************

cap prog drop table_head
prog table_head
	syntax, COLumns(integer)
	global table_head = `"\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{l*{`=`columns'+1'}{c}}\hline\hline"'
end

cap prog drop latex_header
prog latex_header
	syntax, TABLE(integer)
	
	assert `table'>=2 & `table'<=13
	
	if `table'==2 | `table'==3 | `table'==5 {
		global header	 = `""\shortstack{Gender\\attitudes index}" "\shortstack{Girls'\\aspirations index}" "\shortstack{Self-reported\\behavior index}" "'
		global preheader = ""
		global numbers = `"& (1) & (2) & (3) \\ \hline"'
	}
	
	if `table'==4 {
		global header	 = `""Girls" "Boys" "Girls" "Boys""'
		table_head, columns(3)
		global preheader = `"prehead("{${table_head} \vspace{-.7em}\\ & \multicolumn{2}{c}{\shortstack{Gender \\ attitudes index}} & \multicolumn{2}{c}{\shortstack{Self-reported \\ behavior index}} \\ \cmidrule(lr){2-3} \cmidrule(lr){4-5}") "'
		global numbers = `"& (1) & (2) & (3) & (4) \\ \hline"'
	}
	
	if `table'==6 | `table'==12 {
		global header	 = `""\shortstack{Women\\should be\\allowed\\to work}" "\shortstack{Community\\thinks\\women\\should be\\allowed to\\work}" "\shortstack{Women\\should be al-\\lowed to work\\and thinks\\community\\will not op-\\pose them}" "\shortstack{Women\\should be\\allowed to\\study in\\college even if it \\is far away}" "\shortstack{Community\\thinks women\\should be\\allowed to\\study in col-\\lege even if it\\is far away}" "\shortstack{Women should\\be allowed\\to study in\\college and\\thinks commu-\\nity will not\\oppose them}"  "'
		table_head, columns(6)
		global preheader =  `"prehead("{${table_head} \vspace{-.7em}\\ & \multicolumn{3}{c}{Social norms toward work} & \multicolumn{3}{c}{Social norms toward education} \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}  & \multicolumn{3}{c}{Student agrees:} & \multicolumn{3}{c}{Student agrees:} \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}") "'
		global numbers = `"& (1) & (2) & (3) & (4) & (5) & (6) \\ \hline"'
	}
	
	if `table'==7 {
		global header_girls	 = `""\shortstack{Girls'\\self-esteem}" "\shortstack{Awareness of\\gender-based\\discrimination}" "\shortstack{IAT: associates\\girls with posi-\\tive words}" "\shortstack{IAT: associates\\women with\\market work}" "'
		global header_boys	 = `" "\shortstack{Awareness of\\gender-based\\discrimination}" "\shortstack{IAT: associates\\girls with posi-\\tive words}" "\shortstack{IAT: associates\\women with\\market work}" "'
		global preheader = ""
		global numbers_girls = `"& (1) & (2) & (3) & (4) \\ \hline"'
		global numbers_boys = `"& (1) & (2) & (3) \\ \hline"'
	}
	if `table'==8 | `table'==9 {
		global header	 = `""\shortstack{Gender\\attitudes index}" "\shortstack{Girls'\\aspirations index}" "\shortstack{Self-reported\\behavior index}" "\shortstack{Applied to\\scholarship}" "\shortstack{Signed\\petition}" "'
		global preheader = ""
		global numbers = `"& (1) & (2) & (3) & (4) & (5) \\ \hline"'
	}
	if `table'==10 {
		global header	 = `""Girls" "Boys" "Girls" "Boys" "Girls" "Boys""'
		table_head, columns(6)
		global preheader =  `"prehead("{${table_head} \vspace{-.7em}\\ & \multicolumn{2}{c}{\shortstack{Gender attitudes\\index}} & \multicolumn{2}{c}{\shortstack{Self-reported\\behavior index}} & \multicolumn{2}{c}{Signed petition} \\ \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7}")  "'
		global numbers = `"& (1) & (2) & (3) & (4) & (5) & (6) \\ \hline"'
	}
	if `table'==11 {
		global header	 none
		table_head, columns(3)
		global preheader =  `"prehead("{${table_head} \vspace{-.7em}\\ & \multicolumn{3}{c}{Applied to scholarship} \\ \cmidrule(lr){2-4}")  "'
		global numbers = `"& (1) & (2) & (3)  \\ \hline"'
	}
	if `table'==13 {
		global header	 = `""\shortstack{Girls'\\self-esteem}" "\shortstack{Girls'\\education}" "\shortstack{Marriage\\and fertility\\aspirations\\(girls)}" "\shortstack{Marriage\\and fertility\\aspirations\\(boys)}" "\shortstack{Girls'\\experienced\\harassment}" "\shortstack{Boys'\\perpetrated\\harassment\\(school-grade\\level)}" "'
		global preheader = ""
		global numbers = `"& (1) & (2) & (3) & (4) & (5) & (6) \\ \hline"'
	}
	
end

*********
*Table 2*
*********
use if !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear

//Store MHT SE's
next_seed, reset
mhtreg ///
		    (E_Sgender_index2     B_treat /*B_Sgender_index2     district_gender_* gender_grade_* $el_gender_flag       	 */ ) ///
			(E_Saspiration_index2 B_treat /*B_Saspiration_index2 district_? B_Sgrade6  		    $el_aspiration_flag 	 */     ) ///
			(E_Sbehavior_index2   B_treat /*B_Sbehavior_index2   district_gender_* gender_grade_* $el_behavior_common_flag*/ ) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap

store_mht_se, varname(B_treat)			

//Joint hypothesis
next_seed
randcmd ///
	((B_treat) reg E_Sgender_index2     B_treat /*B_Sgender_index2     district_gender_* gender_grade_* $el_gender_flag*/          , cluster(Sschool_id) ) ///
	((B_treat) reg E_Saspiration_index2 B_treat /*B_Saspiration_index2 district_? B_Sgrade6 			  $el_aspiration_flag */	   , cluster(Sschool_id)) ///
	((B_treat) reg E_Sbehavior_index2   B_treat /*B_Sbehavior_index2   district_gender_* gender_grade_* $el_behavior_common_flag */ , cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval "$tmp_val"
			
//Regressions
use "$finaldata", clear

label var E_Sgender_index2 "Gender attitudes index"
label var E_Saspiration_index2 "Girls' aspirations index"
label var E_Sbehavior_index2 "Self-reported behavior index"
label var B_treat "Treated"

estimates clear

eststo: qui reg E_Sgender_index2 B_treat /*B_Sgender_index2 district_gender_* gender_grade_* ///
	$el_gender_flag*/ ///
	if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui sum E_Sgender_index2  if !B_treat & !mi(E_Steam_id) & attrition_el==0
myround, name(mean)
estadd local control_group_mean "$tmp_val"
estadd local pval_joint_hypothesis "`joint_pval'"
estadd mat robust_p=p1

eststo: qui reg E_Saspiration_index2 B_treat /*B_Saspiration_index2 district_? B_Sgrade6 ///
	$el_aspiration_flag*/ ///
	if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui sum E_Saspiration_index2  if !B_treat & !mi(E_Steam_id) & attrition_el==0
myround, name(mean) 
estadd local control_group_mean "$tmp_val"
estadd mat robust_p=p2
	
eststo: qui reg E_Sbehavior_index2 B_treat /*B_Sbehavior_index2 district_gender_* gender_grade_* ///
	$el_behavior_common_flag*/ ///
	if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui sum E_Sbehavior_index2  if !B_treat & !mi(E_Steam_id) & attrition_el==0
myround, name(mean) 
estadd local control_group_mean "$tmp_val"
estadd mat robust_p=p3

//Tabulate
esttab, ///
	/// keep(B_treat _cons) ///
	label stats(control_group_mean N r2 pval_joint_hypothesis, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "R2" "P-value, joint hypothesis")) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none) 

latex_header, table(2)
esttab using "${mytables}/no_controls/table2.tex", replace ///
	///keep(B_treat) ///
	label stats(/*control_group_mean*/ N r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc ${fmt}) label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypothesis")) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none) ///
	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")



*********
*Table 3*
*********
use if !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear

sum B_Ssocial_scale, detail
g highsd_std=1 if B_Ssocial_scale>`r(p50)'
replace highsd_std=0 if B_Ssocial_scale<=`r(p50)' 
g treat_highsd_std = highsd_std*B_treat

//Store MHT SE's
next_seed
mhtreg ///
		    (E_Sgender_index2     				   B_treat highsd_std treat_highsd_std /*B_Sgender_index2     district_gender_* gender_grade_* $el_gender_flag       */	 ) ///
			(E_Saspiration_index2 				   B_treat highsd_std treat_highsd_std /*B_Saspiration_index2 district_? B_Sgrade6  		    $el_aspiration_flag 	   */) ///
			(E_Sbehavior_index2   				   B_treat highsd_std treat_highsd_std /*B_Sbehavior_index2   district_gender_* gender_grade_* $el_behavior_common_flag  */) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(B_treat)			
			
next_seed
mhtreg 		(E_Sgender_index2     highsd_std 	   B_treat 			  treat_highsd_std /*B_Sgender_index2     district_gender_* gender_grade_* $el_gender_flag       	 */) ///
			(E_Saspiration_index2 highsd_std       B_treat 			  treat_highsd_std /*B_Saspiration_index2 district_? B_Sgrade6  		    $el_aspiration_flag 	 */) ///
			(E_Sbehavior_index2   highsd_std 	   B_treat 			  treat_highsd_std /*B_Sbehavior_index2   district_gender_* gender_grade_* $el_behavior_common_flag */ ) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(highsd_std) matname(highsd_std)
			
next_seed			
mhtreg 		(E_Sgender_index2     treat_highsd_std B_treat highsd_std 				   /*B_Sgender_index2     district_gender_* gender_grade_* $el_gender_flag       	 */) ///
			(E_Saspiration_index2 treat_highsd_std B_treat highsd_std 				   /*B_Saspiration_index2 district_? B_Sgrade6  		    $el_aspiration_flag 	 */) ///
			(E_Sbehavior_index2   treat_highsd_std B_treat highsd_std  				   /*B_Sbehavior_index2   district_gender_* gender_grade_* $el_behavior_common_flag  */) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(treat_highsd_std) matname(treat_highsd_std)
			
mat reg1 = B_treat1, highsd_std1, treat_highsd_std1
mat reg2 = B_treat2, highsd_std2, treat_highsd_std2
mat reg3 = B_treat3, highsd_std3, treat_highsd_std3

//Joint hypothesis
next_seed
randcmd ///
	((B_treat)			reg E_Sgender_index2     	B_treat highsd_std treat_highsd_std /*B_Sgender_index2     district_gender_* gender_grade_* $el_gender_flag          */, cluster(Sschool_id)) ///
	((B_treat) 			reg E_Saspiration_index2 	B_treat highsd_std treat_highsd_std /*B_Saspiration_index2 district_? B_Sgrade6  		    $el_aspiration_flag 	 */  , cluster(Sschool_id)) ///
	((B_treat) 			reg E_Sbehavior_index2  	B_treat highsd_std treat_highsd_std /*B_Sbehavior_index2   district_gender_* gender_grade_* $el_behavior_common_flag */, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval1 "$tmp_val"

next_seed
randcmd ///
	((highsd_std) 		reg E_Sgender_index2     	B_treat highsd_std treat_highsd_std /*B_Sgender_index2     district_gender_* gender_grade_* $el_gender_flag          */, cluster(Sschool_id)) ///
	((highsd_std) 		reg E_Saspiration_index2 	B_treat highsd_std treat_highsd_std /*B_Saspiration_index2 district_? B_Sgrade6  		    $el_aspiration_flag 	   */, cluster(Sschool_id)) ///
	((highsd_std)		reg E_Sbehavior_index2  	B_treat highsd_std treat_highsd_std /*B_Sbehavior_index2   district_gender_* gender_grade_* $el_behavior_common_flag */, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval2 "$tmp_val"

next_seed
randcmd ///
	((treat_highsd_std) reg E_Sgender_index2     	B_treat highsd_std treat_highsd_std   /*B_Sgender_index2     district_gender_* gender_grade_* $el_gender_flag          */, cluster(Sschool_id)) ///
	((treat_highsd_std) reg E_Saspiration_index2 	B_treat highsd_std treat_highsd_std   /*B_Saspiration_index2 district_? B_Sgrade6  		    $el_aspiration_flag 	   */, cluster(Sschool_id)) ///
	((treat_highsd_std) reg E_Sbehavior_index2  	B_treat highsd_std treat_highsd_std   /*B_Sbehavior_index2   district_gender_* gender_grade_* $el_behavior_common_flag */, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval3 "$tmp_val"

//Run regressions
u "$finaldata", clear

//generate interactions
sum B_Ssocial_scale, detail
g highsd_std=1 if B_Ssocial_scale>`r(p50)'
replace highsd_std=0 if B_Ssocial_scale<=`r(p50)' 
g treat_highsd_std = highsd_std*B_treat

//Label vars
label var E_Sgender_index2 "Gender attitudes index"
label var E_Saspiration_index2 "Girls' aspirations index"
label var E_Sbehavior_index2 "Self-reported behavior index"
label var B_treat "Treated"
label var highsd_std "High social desirability (Soc. D) score"
label var treat_highsd_std "Treated X High soc. D score"

estimates clear
eststo: qui reg E_Sgender_index2 B_treat highsd_std treat_highsd_std /*B_Sgender_index2 district_gender_* gender_grade_* ///
	$el_gender_flag */ ///
	if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui test B_treat + treat_highsd_std=0
myround, name(p)
estadd local pval_interaction = "$tmp_val"
qui sum E_Sgender_index2  if !B_treat & !mi(E_Steam_id) & attrition_el==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg1
estadd local pval_joint_hypothesis1 "`joint_pval1'"
estadd local pval_joint_hypothesis2 "`joint_pval2'"
estadd local pval_joint_hypothesis3 "`joint_pval3'"

eststo: qui reg E_Saspiration_index2 B_treat highsd_std treat_highsd_std /*B_Saspiration_index2 district_? B_Sgrade6 ///
	$el_aspiration_flag */ ///
	if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui test B_treat + treat_highsd_std=0
myround, name(p)
estadd local pval_interaction = "$tmp_val"
qui sum E_Saspiration_index2  if !B_treat & !mi(E_Steam_id) & attrition_el==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg2

eststo: qui reg E_Sbehavior_index2   B_treat highsd_std treat_highsd_std /*B_Sbehavior_index2 district_gender_* gender_grade_* ///
	$el_behavior_common_flag*/ ///
	if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui test B_treat + treat_highsd_std=0
myround, name(p)
estadd local pval_interaction = "$tmp_val"
qui sum E_Sbehavior_index2  if !B_treat & !mi(E_Steam_id) & attrition_el==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg3

//Tabulate
esttab, /// keep(B_treat highsd_std treat_highsd_std) ///
	label stats(pval_interaction control_group_mean N r2 pval_joint_hypothesis1 pval_joint_hypothesis2 pval_joint_hypothesis3, fmt(${fmt} ${fmt} %9.0fc ${fmt}) ///
	label("p-value: Treated + Treated x High Soc. D=0" "Control group mean" "Number of students" "R2" "P-value, joint hypothesis (Treated)" "P-value, joint hypothesis (High social desirability (Soc. D) score)" "P-value, joint hypothesis (Treated X High soc. D score)" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)

latex_header, table(3)
esttab using "${mytables}/no_controls/table3.tex", replace ///	
	/// keep(B_treat highsd_std treat_highsd_std) ///
	label stats(pval_interaction /*control_group_mean*/ N r2 empty1 pval_joint_hypothesis1 pval_joint_hypothesis2 pval_joint_hypothesis3, fmt(${fmt} /*${fmt}*/ %9.0fc ${fmt}) ///
	label("p-value: Treated + Treated x High Soc. D=0" /*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypotheses:" "   - Treated" "   - High social desirability (Soc. D) score" "   - Treated X High soc. D score" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none) ///
	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")
	
	
*********
*Table 4*
*********
//Store MHT SE's
use if !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear

next_seed
mhtreg ///
		    (E_Sgender_index2   B_treat /*B_Sgender_index2 district_? B_Sgrade6 $el_gender_flag */ if B_Sgirl) ///
			(E_Sgender_index2   B_treat /*B_Sgender_index2 district_? B_Sgrade6 $el_gender_flag */ if !B_Sgirl) ///
			(E_Sbehavior_index2 B_treat /*B_Sbehavior_index2 district_? B_Sgrade6 $el_behavior_common_flag */ if B_Sgirl) ///
			(E_Sbehavior_index2 B_treat /*B_Sbehavior_index2 district_? B_Sgrade6 $el_behavior_common_flag */ if !B_Sgirl) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap

store_mht_se, varname(B_treat)			

//Joint hypothesis
next_seed
randcmd ///
	((B_treat) reg E_Sgender_index2   B_treat /*B_Sgender_index2   district_? B_Sgrade6 $el_gender_flag */ if B_Sgirl          , cluster(Sschool_id) ) ///
	((B_treat) reg E_Sgender_index2   B_treat /*B_Sgender_index2   district_? B_Sgrade6 $el_gender_flag  */if !B_Sgirl 	       , cluster(Sschool_id)) ///
	((B_treat) reg E_Sbehavior_index2 B_treat /*B_Sbehavior_index2 district_? B_Sgrade6 $el_behavior_common_flag */ if B_Sgirl , cluster(Sschool_id)) ///
	((B_treat) reg E_Sbehavior_index2 B_treat /*B_Sbehavior_index2 district_? B_Sgrade6 $el_behavior_common_flag */ if !B_Sgirl, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval "$tmp_val"
			
//Run regressions
use "$finaldata", clear

label var E_Sgender_index2 "Gender attitudes index"
label var E_Sbehavior_index2 "Self-reported behavior index"
label var B_treat "Treated"

estimates clear
eststo: qui reg E_Sgender_index2 B_treat /*B_Sgender_index2 district_? B_Sgrade6 ///
	$el_gender_flag*/ ///
	if B_Sgirl & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)
qui sum E_Sgender_index2  if !B_treat & !mi(E_Steam_id) & attrition_el==0 & B_Sgirl
myround, name(mean)
estadd local control_group_mean "$tmp_val"
estadd local pval_joint_hypothesis "`joint_pval'"
estadd mat robust_p=p1
	
eststo: qui reg E_Sgender_index2 B_treat /*B_Sgender_index2 district_? B_Sgrade6 ///
	$el_gender_flag*/ ///
	if !B_Sgirl & !mi(E_Steam_id) & attrition_el==0, cluster(Sschool_id)	
qui sum E_Sgender_index2  if !B_treat & !mi(E_Steam_id) & attrition_el==0 & !B_Sgirl
myround, name(mean)
estadd local control_group_mean "$tmp_val"
estadd mat robust_p=p2

eststo: qui reg E_Sbehavior_index2 B_treat /*B_Sbehavior_index2 district_? B_Sgrade6 ///
	$el_behavior_common_flag*/ ///
	if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)	
qui sum E_Sbehavior_index2  if !B_treat & !mi(E_Steam_id) & attrition_el==0 & B_Sgirl
myround, name(mean)
estadd local control_group_mean "$tmp_val"
estadd mat robust_p=p3

eststo: qui reg E_Sbehavior_index2 B_treat /*B_Sbehavior_index2 district_? B_Sgrade6 ///
	$el_behavior_common_flag*/ ///
	if !B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)	
qui sum E_Sbehavior_index2  if !B_treat & !mi(E_Steam_id) & attrition_el==0 & !B_Sgirl
myround, name(mean)
estadd local control_group_mean "$tmp_val"
estadd mat robust_p=p3

//Tabulate
esttab, /// keep(B_treat) ///
	label stats(control_group_mean N r2 pval_joint_hypothesis, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)  prehead("$preheader")

latex_header, table(4)
esttab using "${mytables}/no_controls/table4.tex", replace ///
	///keep(B_treat) ///
	label stats(/*control_group_mean*/ N r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc ${fmt}) label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	 ///
	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")
	
	
*********
*Table 5*
*********
use if !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear

gen treat_Pgender_index2 = B_treat * B_Pgender_index2

label var E_Sgender_index2 "Gender attitudes index"
label var E_Sbehavior_index2 "Self-reported behavior index"
label var B_treat "Treated"
label var treat_Pgender_index2 "Treated x baseline parent attitudes"

//Store MHT SE's
next_seed
mhtreg ///
		    (E_Sgender_index2      B_treat treat_Pgender_index2 B_Pgender_index2 /*B_Sgender_index2     $el_gender_flag          district_gender_* gender_grade_* c.B_Pgender_index2#c.(B_Sgender_index2     $el_gender_flag          district_gender_* gender_grade_*) */) ///
			(E_Saspiration_index2  B_treat treat_Pgender_index2 B_Pgender_index2 /*B_Saspiration_index2 $el_aspiration_flag      district_?        B_Sgrade6      c.B_Pgender_index2#c.(B_Saspiration_index2 $el_aspiration_flag      district_?        B_Sgrade6     ) */) ///
			(E_Sbehavior_index2    B_treat treat_Pgender_index2 B_Pgender_index2 /*B_Sbehavior_index2   $el_behavior_common_flag district_gender_* gender_grade_* c.B_Pgender_index2#c.(B_Sbehavior_index2   $el_behavior_common_flag district_gender_* gender_grade_*) */) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(B_treat)			
			
next_seed	
mhtreg ///
		    (E_Sgender_index2      treat_Pgender_index2 B_treat B_Pgender_index2 /*B_Sgender_index2     $el_gender_flag          district_gender_* gender_grade_* c.B_Pgender_index2#c.(B_Sgender_index2     $el_gender_flag          district_gender_* gender_grade_*) */) ///
			(E_Saspiration_index2  treat_Pgender_index2 B_treat B_Pgender_index2 /*B_Saspiration_index2 $el_aspiration_flag      district_?        B_Sgrade6      c.B_Pgender_index2#c.(B_Saspiration_index2 $el_aspiration_flag      district_?        B_Sgrade6     ) */) ///
			(E_Sbehavior_index2    treat_Pgender_index2 B_treat B_Pgender_index2 /*B_Sbehavior_index2   $el_behavior_common_flag district_gender_* gender_grade_* c.B_Pgender_index2#c.(B_Sbehavior_index2   $el_behavior_common_flag district_gender_* gender_grade_*) */) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(treat_Pgender_index2) matname(treat_Pgender_index2)

//mat colnames treat_Pgender_index21 = "c.B_treat#c.B_Pgender_index2"
//mat colnames treat_Pgender_index22 = "c.B_treat#c.B_Pgender_index2"
//mat colnames treat_Pgender_index23 = "c.B_treat#c.B_Pgender_index2"

mat reg1 = B_treat1, treat_Pgender_index21
mat reg2 = B_treat2, treat_Pgender_index22
mat reg3 = B_treat3, treat_Pgender_index23


//Joint hypothesis
next_seed
randcmd ///
			((B_treat) reg E_Sgender_index2      B_treat treat_Pgender_index2 B_Pgender_index2 /*B_Sgender_index2     $el_gender_flag          district_gender_* gender_grade_* c.B_Pgender_index2#c.(B_Sgender_index2     $el_gender_flag          district_gender_* gender_grade_*)*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Saspiration_index2  B_treat treat_Pgender_index2 B_Pgender_index2 /*B_Saspiration_index2 $el_aspiration_flag      district_?        B_Sgrade6      c.B_Pgender_index2#c.(B_Saspiration_index2 $el_aspiration_flag      district_?        B_Sgrade6     )*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Sbehavior_index2    B_treat treat_Pgender_index2 B_Pgender_index2 /*B_Sbehavior_index2   $el_behavior_common_flag district_gender_* gender_grade_* c.B_Pgender_index2#c.(B_Sbehavior_index2   $el_behavior_common_flag district_gender_* gender_grade_*)*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval1 "$tmp_val"

next_seed
randcmd ///
			((treat_Pgender_index2) reg E_Sgender_index2      B_treat treat_Pgender_index2 B_Pgender_index2 /*B_Sgender_index2     $el_gender_flag          district_gender_* gender_grade_* c.B_Pgender_index2#c.(B_Sgender_index2     $el_gender_flag          district_gender_* gender_grade_*)*/, cluster(Sschool_id)) ///
			((treat_Pgender_index2) reg E_Saspiration_index2  B_treat treat_Pgender_index2 B_Pgender_index2 /*B_Saspiration_index2 $el_aspiration_flag      district_?        B_Sgrade6      c.B_Pgender_index2#c.(B_Saspiration_index2 $el_aspiration_flag      district_?        B_Sgrade6     )*/, cluster(Sschool_id)) ///
			((treat_Pgender_index2) reg E_Sbehavior_index2    B_treat treat_Pgender_index2 B_Pgender_index2 /*B_Sbehavior_index2   $el_behavior_common_flag district_gender_* gender_grade_* c.B_Pgender_index2#c.(B_Sbehavior_index2   $el_behavior_common_flag district_gender_* gender_grade_*)*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval2 "$tmp_val"

//Run regressions
u "$finaldata", clear

gen treat_Pgender_index2 = B_treat * B_Pgender_index2

label var E_Sgender_index2 "Gender attitudes index"
label var E_Sbehavior_index2 "Self-reported behavior index"
label var B_treat "Treated"
label var treat_Pgender_index2 "Treated x baseline parent attitudes"

estimates clear
eststo: qui reg E_Sgender_index2  B_treat treat_Pgender_index2 B_Pgender_index2 ///
							/*B_Sgender_index2 $el_gender_flag district_gender_* gender_grade_* ///
	  c.B_Pgender_index2#c.(B_Sgender_index2 $el_gender_flag district_gender_* gender_grade_*)*/ ///
	if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui sum E_Sgender_index2 if !B_treat & !mi(E_Steam_id) & attrition_el==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg1
estadd local pval_joint_hypothesis1 "`joint_pval1'"
estadd local pval_joint_hypothesis2 "`joint_pval2'"

eststo: qui reg E_Saspiration_index2  B_treat treat_Pgender_index2 B_Pgender_index2 /// girls only
							/*B_Saspiration_index2 $el_aspiration_flag district_? B_Sgrade6  ///
	  c.B_Pgender_index2#c.(B_Saspiration_index2 $el_aspiration_flag district_? B_Sgrade6 )*/ ///
	if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui sum E_Sgender_index2 if !B_treat & !mi(E_Steam_id) & attrition_el==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg2

eststo: qui reg E_Sbehavior_index2  B_treat treat_Pgender_index2 B_Pgender_index2 ///
							/*B_Sbehavior_index2 $el_behavior_common_flag district_gender_* gender_grade_* ///
	  c.B_Pgender_index2#c.(B_Sbehavior_index2 $el_behavior_common_flag district_gender_* gender_grade_*)*/ ///
	if !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui sum E_Sbehavior_index2 if !B_treat & !mi(E_Steam_id) & attrition_el==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg3
	
//Tabulate
esttab, ///keep(B_treat treat_Pgender_index2) ///
	label stats(control_group_mean N r2 pval_joint_hypothesis1 pval_joint_hypothesis2, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none) 

latex_header, table(5)
esttab using "${mytables}/no_controls/table5.tex", replace ///
	///keep(B_treat treat_Pgender_index2) ///
	label stats(control_group_mean N r2 empty pval_joint_hypothesis1 pval_joint_hypothesis2, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "\$R^2$" "P-value, joint hypotheses:" "   - Treated" "   - Treated × baseline parent attitudes" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none) ///
	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")
	
*********
*Table 6*
*********
//One mistake in the original code (line 1317)
	// Boys regression condition on non-missing steam ID and no attrition at _endline 2_
	// Furthermore: The same code is used to estimate EL2 in table 12, where the girls regression is wrong (line 1230)

//Store MHT SE's
		//Girls
use if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear
label var B_treat "Treated"
next_seed
mhtreg ///
		    (E_Sallow_work_s 				B_treat /*district_? B_Sgrade6*/) ///
			(E_Scommunity_allow_work_s 		B_treat /*district_? B_Sgrade6*/) ///
			(E_Scommunity_work_s 			B_treat /*district_? B_Sgrade6*/) ///
			(E_Sallow_college_s 			B_treat /*district_? B_Sgrade6*/) ///
			(E_Scommunity_allow_college_s 	B_treat /*district_? B_Sgrade6*/) ///
			(E_Scommunity_college_s 		B_treat /*district_? B_Sgrade6*/) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(girls)			

	//Boys - with authors mistake
use if !B_Sgirl & !mi(E2_Steam_id) & attrition==0  using "$finaldata", clear
label var B_treat "Treated"
next_seed
mhtreg ///
		    (E_Sallow_work_s 				B_treat /*district_? B_Sgrade6*/) ///
			(E_Scommunity_allow_work_s 		B_treat /*district_? B_Sgrade6*/) ///
			(E_Scommunity_work_s 			B_treat /*district_? B_Sgrade6*/) ///
			(E_Sallow_college_s 			B_treat /*district_? B_Sgrade6*/) ///
			(E_Scommunity_allow_college_s 	B_treat /*district_? B_Sgrade6*/) ///
			(E_Scommunity_college_s 		B_treat /*district_? B_Sgrade6*/) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(boys_mistake)
	
	//Boys
use if !B_Sgirl & !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear
label var B_treat "Treated"
next_seed
mhtreg ///
		    (E_Sallow_work_s 				B_treat /*district_? B_Sgrade6*/) ///
			(E_Scommunity_allow_work_s 		B_treat /*district_? B_Sgrade6*/) ///
			(E_Scommunity_work_s 			B_treat /*district_? B_Sgrade6*/) ///
			(E_Sallow_college_s 			B_treat /*district_? B_Sgrade6*/) ///
			(E_Scommunity_allow_college_s 	B_treat /*district_? B_Sgrade6*/) ///
			(E_Scommunity_college_s 		B_treat /*district_? B_Sgrade6*/) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(boys)

//Joint hypothesis
		//Girls
use if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear
label var B_treat "Treated"
next_seed
randcmd ///
			((B_treat) reg E_Sallow_work_s 					B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Scommunity_allow_work_s 		B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Scommunity_work_s 				B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Sallow_college_s 				B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Scommunity_allow_college_s 	B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Scommunity_college_s 			B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval1 "$tmp_val"

	//Boys regression, with mistake
use if !B_Sgirl & !mi(E2_Steam_id) & attrition==0  using "$finaldata", clear
label var B_treat "Treated"
next_seed
randcmd ///
			((B_treat) reg E_Sallow_work_s 					B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Scommunity_allow_work_s 		B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Scommunity_work_s 				B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Sallow_college_s 				B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Scommunity_allow_college_s 	B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Scommunity_college_s 			B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval2 "$tmp_val"

	//Boys regression
use if !B_Sgirl & !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear
label var B_treat "Treated"
next_seed
randcmd ///
			((B_treat) reg E_Sallow_work_s 					B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Scommunity_allow_work_s 		B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Scommunity_work_s 				B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Sallow_college_s 				B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Scommunity_allow_college_s 	B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Scommunity_college_s 			B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval3 "$tmp_val"

//Run regressions
	
use "$finaldata", clear
label var B_treat "Treated"

label var E_Sallow_work_s 				"Women should be allowed to work"
label var E_Scommunity_allow_work_s 	"Community thinks women should be allowed to work"
label var E_Scommunity_work_s 			"Women should be allowed to work and thinks community will not oppose them"
label var E_Sallow_college_s 			"Women should be allowed to study in college even if it is far away"
label var E_Scommunity_allow_college_s  "Community thinks women should be allowed to study in college even if it is far away"
label var E_Scommunity_college_s		"Women should be allowed to study in college and thinks community will not oppose them"



estimates clear

	//Girls regression
loc n=1
foreach var of varlist E_Sallow_work_s E_Scommunity_allow_work_s E_Scommunity_work_s E_Sallow_college_s E_Scommunity_allow_college_s E_Scommunity_college_s {
	eststo g`n'   :qui reg `var' B_treat /*district_? B_Sgrade6*/ if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
	qui sum `var' if B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0
	myround, name(mean) 
	estadd local control_group_mean = "$tmp_val"
	estadd mat robust_p=girls`n'
	if `n'==1 estadd local pval_joint_hypothesis "`joint_pval1'"
	loc n=`n'+1	
}

	//Boys regression - with authors mistake
loc n=1
foreach var of varlist E_Sallow_work_s E_Scommunity_allow_work_s E_Scommunity_work_s E_Sallow_college_s E_Scommunity_allow_college_s E_Scommunity_college_s {
	eststo b`n'    :qui reg `var' B_treat /*district_? B_Sgrade6*/ if !B_Sgirl & !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
	qui sum `var' if !B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
	myround, name(mean) 
	estadd local control_group_mean = "$tmp_val"
	estadd mat robust_p=boys_mistake`n'
	if `n'==1 estadd local pval_joint_hypothesis "`joint_pval2'"
	loc n=`n'+1
}

	//Boys regression - corrected
loc n=1
foreach var of varlist E_Sallow_work_s E_Scommunity_allow_work_s E_Scommunity_work_s E_Sallow_college_s E_Scommunity_allow_college_s E_Scommunity_college_s {
	eststo b`n'_corrected    :qui reg `var' B_treat /*district_? B_Sgrade6*/ if !B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
	qui sum `var' if !B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0 
	myround, name(mean) 
	estadd local control_group_mean = "$tmp_val"
	qui count if !B_Sgirl & !mi(`var') & !mi(E_Steam_id) & attrition_el==0 & !(!mi(E2_Steam_id) & attrition==0)
	estadd local obs_dropped = r(N)
	estadd mat robust_p=boys`n'
	if `n'==1 estadd local pval_joint_hypothesis "`joint_pval2'"
	loc n=`n'+1
}

//Tabulate
esttab g?, ///keep(B_treat) ///
	label stats(control_group_mean N r2 pval_joint_hypothesis, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)

esttab b?, ///keep(B_treat) ///
	label stats(control_group_mean N r2 pval_joint_hypothesis, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)	

esttab b?_corrected, ///keep(B_treat) ///
	label stats(control_group_mean N obs_dropped r2 pval_joint_hypothesis, fmt(${fmt} %9.0fc %9.0fc ${fmt}) label("Control group mean" "Number of students" "Observations dropped in authors regression" "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)	

//Store 
latex_header, table(6)
esttab g? using "${mytables}/no_controls/table6_girls.tex", replace ///
	///keep(B_treat) ///
	label stats(/*control_group_mean*/ N r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc ${fmt}) label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none) ///
	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")
	
esttab b?  using "${mytables}/no_controls/table6_boys.tex", replace ///
	///keep(B_treat) ///
	label stats(/*control_group_mean*/ N r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc ${fmt}) label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	///
	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")
	
esttab b?_corrected  using "${mytables}/no_controls/table6_boys_corrected.tex", replace ///
	///keep(B_treat) ///
	label stats(/*control_group_mean*/ N /*obs_dropped*/ r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc /*%9.0fc*/ ${fmt}) label(/*"Control group mean"*/ "Number of students" /*"Observations dropped in authors regression"*/ "\$R^2$" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	///
	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")




*********
*Table 7*
*********
//Store MHT SE's
use if B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear
label var B_treat "Treated"
		//Girls
next_seed
mhtreg ///
		    (E_Sesteem_index2_girl      B_treat /*B_Sesteem_index2_girl district_? B_Sgrade6 ${el_esteem_girl_flag}*/) ///
			(E_Sdiscrimination_index2   B_treat /*district_? B_Sgrade6 ${el_discrimination_flag}*/) ///
			(E_D_measure_goodbad_neg    B_treat /*B_D_measure_goodbad_neg district_? B_Sgrade6 B_D_measure_goodbad_neg_flag B_D_measure_goodbad_neg_flag*/) ///
			(E_D_measure_occupation_neg B_treat /*B_D_measure_occupation_neg district_? B_Sgrade6 B_D_measure_occupation_neg_flag B_D_measure_occupation_neg_flag*/) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(girls)			
	
	//Boys
use if !B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear
label var B_treat "Treated"
next_seed
mhtreg ///
		    (E_Sdiscrimination_index2   B_treat /*district_? B_Sgrade6 ${el_discrimination_flag}*/) ///
			(E_D_measure_goodbad_neg    B_treat /*B_D_measure_goodbad_neg district_? B_Sgrade6 B_D_measure_goodbad_neg_flag B_D_measure_goodbad_neg_flag*/) ///
			(E_D_measure_occupation_neg B_treat /*B_D_measure_occupation_neg district_? B_Sgrade6 B_D_measure_occupation_neg_flag B_D_measure_occupation_neg_flag*/) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(boys)

mat boys4=boys3
mat boys3=boys2
mat boys2=boys1 
mat drop boys1

//Joint hypothesis
		//Girls
use if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear
label var B_treat "Treated"
next_seed
randcmd ///
			((B_treat) reg E_Sesteem_index2_girl      B_treat /*B_Sesteem_index2_girl district_? B_Sgrade6 ${el_esteem_girl_flag}*/, cluster(Sschool_id)) ///
			((B_treat) reg E_Sdiscrimination_index2   B_treat /*district_? B_Sgrade6 ${el_discrimination_flag}*/, cluster(Sschool_id)) ///
			((B_treat) reg E_D_measure_goodbad_neg    B_treat /*B_D_measure_goodbad_neg district_? B_Sgrade6 B_D_measure_goodbad_neg_flag B_D_measure_goodbad_neg_flag*/, cluster(Sschool_id)) ///
			((B_treat) reg E_D_measure_occupation_neg B_treat /*B_D_measure_occupation_neg district_? B_Sgrade6 B_D_measure_occupation_neg_flag B_D_measure_occupation_neg_flag*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval1 "$tmp_val"

	//Boys regression
use if !B_Sgirl & !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear
label var B_treat "Treated"
next_seed
randcmd ///
			((B_treat) reg E_Sdiscrimination_index2   B_treat /*district_? B_Sgrade6 ${el_discrimination_flag}*/, cluster(Sschool_id)) ///
			((B_treat) reg E_D_measure_goodbad_neg    B_treat /*B_D_measure_goodbad_neg district_? B_Sgrade6 B_D_measure_goodbad_neg_flag B_D_measure_goodbad_neg_flag*/, cluster(Sschool_id)) ///
			((B_treat) reg E_D_measure_occupation_neg B_treat /*B_D_measure_occupation_neg district_? B_Sgrade6 B_D_measure_occupation_neg_flag B_D_measure_occupation_neg_flag*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval2 "$tmp_val"

//Run regressions
use "$finaldata", clear
label var B_treat "Treated"

estimates clear
//girls
eststo g1: qui reg E_Sesteem_index2_girl B_treat /*B_Sesteem_index2_girl district_? B_Sgrade6 ///
		${el_esteem_girl_flag} */ ///
		if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui sum E_Sesteem_index2_girl if B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=girls1
estadd local pval_joint_hypothesis "`joint_pval1'"
		
eststo g2: qui reg E_Sdiscrimination_index2 B_treat /*district_? B_Sgrade6 ///
		${el_discrimination_flag} */ ///
		if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)		
qui sum E_Sdiscrimination_index2 if B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=girls2

eststo g3: qui reg E_D_measure_goodbad_neg B_treat /*B_D_measure_goodbad_neg district_? B_Sgrade6 ///
		B_D_measure_goodbad_neg_flag B_D_measure_goodbad_neg_flag*/ ///
		if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui sum E_D_measure_goodbad_neg if B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=girls3
		
eststo g4: qui reg E_D_measure_occupation_neg B_treat /*B_D_measure_occupation_neg district_? B_Sgrade6 ///
		B_D_measure_occupation_neg_flag B_D_measure_occupation_neg_flag*/ ///
		if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui sum E_D_measure_occupation if B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=girls4

//Boys
eststo b2: qui reg E_Sdiscrimination_index2 B_treat /*district_? B_Sgrade6 ///
		${el_discrimination_flag}*/  ///
		if !B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)		
qui sum E_Sdiscrimination_index2 if !B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd local pval_joint_hypothesis "`joint_pval2'"
estadd mat robust_p=boys2

eststo b3: qui reg E_D_measure_goodbad_neg B_treat /*B_D_measure_goodbad_neg district_? B_Sgrade6 ///
		B_D_measure_goodbad_neg_flag B_D_measure_goodbad_neg_flag*/ ///
		if !B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui sum E_D_measure_goodbad_neg if !B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=boys3

eststo b4: qui reg E_D_measure_occupation_neg B_treat /*B_D_measure_occupation_neg district_? B_Sgrade6 ///
		B_D_measure_occupation_neg_flag B_D_measure_occupation_neg_flag*/ ///
		if !B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
qui sum E_D_measure_occupation if !B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=boys4

//Tabulate
esttab g?, ///keep(B_treat) ///
	p label stats(control_group_mean N r2 pval_joint_hypothesis, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)
	
esttab b?, ///keep(B_treat) ///
	p label stats(control_group_mean N r2 pval_joint_hypothesis, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)
		
		
//Store	
latex_header, table(7)
esttab g? using "${mytables}/no_controls/table7_girls.tex", replace ///	
	///keep(B_treat) ///
	p label stats(/*control_group_mean*/ N r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc ${fmt}) label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none) ///
	$preheader ///
	mtitle(${header_girls}) nonumbers posthead("${numbers_girls}")
	
esttab b? using "${mytables}/no_controls/table7_boys.tex", replace ///	
	///keep(B_treat) ///
	p label stats(/*control_group_mean*/ N r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc ${fmt}) label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none) ///
	$preheader ///
	mtitle(${header_boys}) nonumbers posthead("${numbers_boys}")
*********
*Table 8*
*********
u if !mi(E2_Steam_id) & attrition==0 using "$finaldata", clear
//Store MHT SE's
next_seed
mhtreg ///
		    (E2_Sgender_index2 		B_treat /*B_Sgender_index2  district_gender_* gender_grade_* ${el2_gender_flag}*/) ///
			(E2_Saspiration_index2 	B_treat /*B_Saspiration_index2 district_? B_Sgrade6 $el2_aspiration_flag if B_Sgirl*/) ///
			(E2_Sbehavior_index2  	B_treat /*B_Sbehavior_index2 district_gender_* gender_grade_* ${el2_behavior_common_flag}*/) ///
			(E2_Sscholar_index2 	B_treat /*district_? B_Sgrade6*/) ///
			(E2_Spetition_index2  	B_treat /*district_gender_* gender_grade_**/) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat)			

//Joint hypothesis
next_seed
randcmd ///
	((B_treat) reg E2_Sgender_index2	 	B_treat /*B_Sgender_index2  district_gender_* gender_grade_* ${el2_gender_flag}*/, cluster(Sschool_id)) ///
	((B_treat) reg E2_Saspiration_index2 	B_treat /*B_Saspiration_index2 district_? B_Sgrade6 $el2_aspiration_flag if B_Sgirl*/, cluster(Sschool_id)) ///
	((B_treat) reg E2_Sbehavior_index2  	B_treat /*B_Sbehavior_index2 district_gender_* gender_grade_* ${el2_behavior_common_flag}*/, cluster(Sschool_id)) ///
	((B_treat) reg E2_Sscholar_index2 		B_treat /*district_? B_Sgrade6*/) ///
	((B_treat) reg E2_Spetition_index2  	B_treat /*district_gender_* gender_grade_* */) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval "$tmp_val"



//Run regressions


use "$finaldata", clear

label var E2_Sgender_index2 "Gender attitudes index"
label var E2_Saspiration_index2 "Girls' aspirations index"
label var E2_Sbehavior_index2 "Self-reported behavior index"
label var E2_Sscholar_index2 "Applied to scholarship"
label var E2_Spetition_index2 "Signed petition"
label var B_treat "Treated"

estimates clear
eststo: qui  reg E2_Sgender_index2 B_treat /*B_Sgender_index2  district_gender_* gender_grade_* ///
	${el2_gender_flag} */ ///
	if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Sgender_index2 if !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd local pval_joint_hypothesis "`joint_pval'"
estadd mat robust_p=p1
	
eststo: qui reg E2_Saspiration_index2 B_treat /*B_Saspiration_index2 district_? B_Sgrade6 ///
	$el2_aspiration_flag */ ///
	if B_Sgirl & !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Saspiration_index2 if !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=p2

eststo: qui reg E2_Sbehavior_index2  B_treat /*B_Sbehavior_index2 district_gender_* gender_grade_* ///
	${el2_behavior_common_flag}*/ ///
	if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Sbehavior_index2 if !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=p3

eststo: qui reg E2_Sscholar_index2 B_treat /*district_? B_Sgrade6*/ ///
	if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1, cluster(Sschool_id) // no baseline index or flags
qui sum E2_Sscholar_index2 if !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean)
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=p4

eststo: qui reg E2_Spetition_index2  B_treat /*district_gender_* gender_grade_* */ ///
	if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) // no baseline index or flags
qui sum E2_Spetition_index2 if !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=p5

esttab, ///keep(B_treat) ///
	p label stats(control_group_mean N r2 pval_joint_hypothesis, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)
	
latex_header, table(8)
esttab using "${mytables}/no_controls/table8.tex", replace ///	
	///keep(B_treat) ///
	p label stats(/*control_group_mean*/ N r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc ${fmt}) label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	///
 	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")
	
*********
*Table 9*
*********
use if !mi(E2_Steam_id) & attrition==0 using "$finaldata", clear

sum B_Ssocial_scale, detail
g highsd_std=1 if B_Ssocial_scale>`r(p50)'
replace highsd_std=0 if B_Ssocial_scale<=`r(p50)' 
g treat_highsd_std = highsd_std*B_treat

//Store MHT SE's
next_seed
mhtreg ///
		    (E2_Sgender_index2 								B_treat highsd_std treat_highsd_std /*B_Sgender_index2  district_gender_* gender_grade_* ${el2_gender_flag} */) ///
			(E2_Saspiration_index2 							B_treat highsd_std treat_highsd_std /*B_Saspiration_index2 district_? B_Sgrade6 $el2_aspiration_flag*/) ///
			(E2_Sbehavior_index2 							B_treat highsd_std treat_highsd_std /*B_Sbehavior_index2 district_gender_* gender_grade_* $el2_behavior_common_flag*/) ///
			(E2_Sscholar_index2 							B_treat highsd_std treat_highsd_std /*district_? B_Sgrade6 */) ///
			(E2_Spetition_index2 							B_treat highsd_std treat_highsd_std /*district_gender_* gender_grade_* */ ) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(B_treat)			
			
next_seed
mhtreg ///
		    (E2_Sgender_index2 			highsd_std 			B_treat treat_highsd_std  /*B_Sgender_index2  district_gender_* gender_grade_* ${el2_gender_flag} */) ///
			(E2_Saspiration_index2 		highsd_std			B_treat treat_highsd_std  /*B_Saspiration_index2 district_? B_Sgrade6 $el2_aspiration_flag*/) ///
			(E2_Sbehavior_index2 		highsd_std			B_treat treat_highsd_std  /*B_Sbehavior_index2 district_gender_* gender_grade_* $el2_behavior_common_flag*/) ///
			(E2_Sscholar_index2 		highsd_std 			B_treat treat_highsd_std  /*district_? B_Sgrade6 */) ///
			(E2_Spetition_index2 		highsd_std			B_treat treat_highsd_std  /*district_gender_* gender_grade_* */) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(highsd_std) matname(highsd_std)
			
next_seed		
mhtreg ///
		    (E2_Sgender_index2 			treat_highsd_std 	B_treat highsd_std  /*B_Sgender_index2  district_gender_* gender_grade_* ${el2_gender_flag} */) ///
			(E2_Saspiration_index2 		treat_highsd_std	B_treat highsd_std  /*B_Saspiration_index2 district_? B_Sgrade6 $el2_aspiration_flag*/) ///
			(E2_Sbehavior_index2 		treat_highsd_std	B_treat highsd_std  /*B_Sbehavior_index2 district_gender_* gender_grade_* $el2_behavior_common_flag*/) ///
			(E2_Sscholar_index2 		treat_highsd_std 	B_treat highsd_std  /*district_? B_Sgrade6 */) ///
			(E2_Spetition_index2 		treat_highsd_std	B_treat highsd_std  /*district_gender_* gender_grade_* */) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(treat_highsd_std) matname(treat_highsd_std)
			
mat reg1 = B_treat1, highsd_std1, treat_highsd_std1
mat reg2 = B_treat2, highsd_std2, treat_highsd_std2
mat reg3 = B_treat3, highsd_std3, treat_highsd_std3
mat reg4 = B_treat4, highsd_std4, treat_highsd_std4
mat reg5 = B_treat5, highsd_std5, treat_highsd_std5

//Joint hypothesis
next_seed
randcmd ///
		    ((B_treat) reg E2_Sgender_index2 		B_treat highsd_std treat_highsd_std /*B_Sgender_index2  district_gender_* gender_grade_* ${el2_gender_flag} 			*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Saspiration_index2 	B_treat highsd_std treat_highsd_std /*B_Saspiration_index2 district_? B_Sgrade6 $el2_aspiration_flag 		   			*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Sbehavior_index2 		B_treat highsd_std treat_highsd_std /*B_Sbehavior_index2 district_gender_* gender_grade_* $el2_behavior_common_flag 	*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Sscholar_index2 		B_treat highsd_std treat_highsd_std /*district_? B_Sgrade6 															    */, cluster(Sschool_id)) ///
			((B_treat) reg E2_Spetition_index2 		B_treat highsd_std treat_highsd_std /*district_gender_* gender_grade_* 												    */, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval1 "$tmp_val"

next_seed
randcmd ///
		    ((highsd_std) reg E2_Sgender_index2 		B_treat highsd_std treat_highsd_std /*B_Sgender_index2  district_gender_* gender_grade_* ${el2_gender_flag} 			*/, cluster(Sschool_id)) ///
			((highsd_std) reg E2_Saspiration_index2 	B_treat highsd_std treat_highsd_std /*B_Saspiration_index2 district_? B_Sgrade6 $el2_aspiration_flag 		   			*/, cluster(Sschool_id)) ///
			((highsd_std) reg E2_Sbehavior_index2 		B_treat highsd_std treat_highsd_std /*B_Sbehavior_index2 district_gender_* gender_grade_* $el2_behavior_common_flag 	*/, cluster(Sschool_id)) ///
			((highsd_std) reg E2_Sscholar_index2 		B_treat highsd_std treat_highsd_std /*district_? B_Sgrade6 															*/, cluster(Sschool_id)) ///
			((highsd_std) reg E2_Spetition_index2 		B_treat highsd_std treat_highsd_std /*district_gender_* gender_grade_* 												*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval2 "$tmp_val"

next_seed
randcmd ///
		    ((treat_highsd_std) reg E2_Sgender_index2 		B_treat highsd_std treat_highsd_std /*B_Sgender_index2  district_gender_* gender_grade_* ${el2_gender_flag} 				*/, cluster(Sschool_id)) ///
			((treat_highsd_std) reg E2_Saspiration_index2 	B_treat highsd_std treat_highsd_std /*B_Saspiration_index2 district_? B_Sgrade6 $el2_aspiration_flag 		   				*/, cluster(Sschool_id)) ///
			((treat_highsd_std) reg E2_Sbehavior_index2 	B_treat highsd_std treat_highsd_std /*B_Sbehavior_index2 district_gender_* gender_grade_* $el2_behavior_common_flag 	*/, cluster(Sschool_id)) ///
			((treat_highsd_std) reg E2_Sscholar_index2 		B_treat highsd_std treat_highsd_std /*district_? B_Sgrade6 																*/, cluster(Sschool_id)) ///
			((treat_highsd_std) reg E2_Spetition_index2 	B_treat highsd_std treat_highsd_std /*district_gender_* gender_grade_* 												*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval3 "$tmp_val"

//Run regressions
use "$finaldata", clear

summ B_Ssocial_scale, detail
gen highsd_std=1 if B_Ssocial_scale>`r(p50)'
replace highsd_std=0 if B_Ssocial_scale<=`r(p50)'
g treat_highsd_std = highsd_std*B_treat

label var B_treat "Treated"
label var highsd_std "High social desirability (Soc. D) score"
label var treat_highsd_std "Treated X High soc. D score"

estimates clear

eststo: qui reg E2_Sgender_index2 B_treat highsd_std treat_highsd_std ///
	///B_Sgender_index2  district_gender_* gender_grade_* ///
	///${el2_gender_flag} ///
	if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
test  B_treat+treat_highsd_std=0
myround, name(p) 
estadd local pval_interaction = "$tmp_val"
qui sum E2_Sgender_index2 if !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg1
estadd local pval_joint_hypothesis1 "`joint_pval1'"
estadd local pval_joint_hypothesis2 "`joint_pval2'"
estadd local pval_joint_hypothesis3 "`joint_pval3'"

eststo: qui reg E2_Saspiration_index2 B_treat highsd_std treat_highsd_std ///
	///B_Saspiration_index2 district_? B_Sgrade6 ///
	///$el2_aspiration_flag ///
	if B_Sgirl & !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
test  B_treat+treat_highsd_std=0
myround, name(p) 
estadd local pval_interaction = "$tmp_val"
qui sum E2_Saspiration_index2 if !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg2

eststo: qui reg E2_Sbehavior_index2 B_treat   highsd_std treat_highsd_std ///
	///B_Sbehavior_index2 district_gender_* gender_grade_* ///
	///$el2_behavior_common_flag ///
	if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id) //Weird
test  B_treat+treat_highsd_std=0
myround, name(p) 
estadd local pval_interaction = "$tmp_val"
qui sum E2_Sbehavior_index2 if !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg3

eststo: qui reg E2_Sscholar_index2 B_treat highsd_std treat_highsd_std ///
	///district_? B_Sgrade6 ///
	if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1, cluster(Sschool_id) // no baseline index or flags
test  B_treat+treat_highsd_std=0
myround, name(p) 
estadd local pval_interaction = "$tmp_val"
qui sum E2_Sscholar_index2 if !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg4

eststo: qui reg E2_Spetition_index2 B_treat highsd_std treat_highsd_std ///
	///district_gender_* gender_grade_* ///
	if !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) // no baseline index or flags
test  B_treat+treat_highsd_std=0
myround, name(p) 
estadd local pval_interaction = "$tmp_val"
qui sum E2_Spetition_index2 if !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg5

esttab, ///keep(B_treat highsd_std treat_highsd_std) ///
	label stats(pval_interaction control_group_mean N r2 pval_joint_hypothesis1 pval_joint_hypothesis2 pval_joint_hypothesis3, fmt(${fmt} ${fmt} %9.0fc ${fmt}) label("p-value: Treated + Treated x High Soc. D=0" "Control group mean" "Number of students" "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)

latex_header, table(9)
esttab using "${mytables}/no_controls/table9.tex", replace ///
	///keep(B_treat highsd_std treat_highsd_std) ///
	label stats(pval_interaction /*control_group_mean*/ N r2 empty pval_joint_hypothesis1 pval_joint_hypothesis2 pval_joint_hypothesis3, fmt(${fmt} /*${fmt}*/ %9.0fc ${fmt}) label("p-value: Treated + Treated x High Soc. D=0" /*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypotheses:" "   - Treated" "   - High social desirability score" "   - Treated × High Soc. D score")) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	///
	$preheader ///
	mtitle(${header}) nonumbers posthead("${numbers}")
	
	
	
	
**********
*Table 10*
**********
//Store MHT SE's
	//Girls
u if B_Sgirl & !mi(E2_Steam_id) & attrition==0  using "$finaldata", clear
next_seed
mhtreg ///
		    (E2_Sgender_index2 	  B_treat /* B_Sgender_index2  district_? B_Sgrade6 ${el2_gender_flag}*/) ///
			(E2_Sbehavior_index2  B_treat /* B_Sbehavior_index2 district_? B_Sgrade6 ${el2_behavior_common_flag}*/) ///
			(E2_Spetition_index2  B_treat /* district_? B_Sgrade6*/) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(girls)	

	//Boys
u if !B_Sgirl & !mi(E2_Steam_id) & attrition==0  using "$finaldata", clear
next_seed
mhtreg ///
		    (E2_Sgender_index2    B_treat /* B_Sgender_index2  district_? B_Sgrade6 ${el2_gender_flag}*/) ///
			(E2_Sbehavior_index2  B_treat /* B_Sbehavior_index2 district_? B_Sgrade6 ${el2_behavior_common_flag}*/) ///
			(E2_Spetition_index2  B_treat /* district_? B_Sgrade6*/) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(boys)	
//Joint hypothesis

	//Girls
u if B_Sgirl & !mi(E2_Steam_id) & attrition==0  using "$finaldata", clear
next_seed
randcmd ///
		    ((B_treat) reg E2_Sgender_index2 	B_treat /* B_Sgender_index2  district_? B_Sgrade6 ${el2_gender_flag}*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Sbehavior_index2  B_treat /* B_Sbehavior_index2 district_? B_Sgrade6 ${el2_behavior_common_flag}*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Spetition_index2  B_treat /* district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval1 "$tmp_val"

	//Boys
u if !B_Sgirl & !mi(E2_Steam_id) & attrition==0  using "$finaldata", clear
next_seed
randcmd ///
		    ((B_treat) reg E2_Sgender_index2 	B_treat /* B_Sgender_index2  district_? B_Sgrade6 ${el2_gender_flag}*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Sbehavior_index2  B_treat /* B_Sbehavior_index2 district_? B_Sgrade6 ${el2_behavior_common_flag}*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Spetition_index2  B_treat /* district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval2 "$tmp_val"


use "$finaldata", clear

label var E2_Sgender_index2 "Gender attitudes index"
label var E2_Saspiration_index2 "Girls' aspirations index"
label var E2_Sbehavior_index2 "Self-reported behavior index"
label var E2_Sscholar_index2 "Applied to scholarship"
label var E2_Spetition_index2 "Signed petition"
label var B_treat "Treated"

estimates clear
//Girls
eststo g1: qui  reg E2_Sgender_index2 B_treat /*B_Sgender_index2  district_? B_Sgrade6 ///
	${el2_gender_flag} */ ///
	if B_Sgirl ///
	& !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Sgender_index2 if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=girls1
estadd local pval_joint_hypothesis1 "`joint_pval1'"
estadd local pval_joint_hypothesis2 "`joint_pval2'"

eststo g2: qui reg E2_Sbehavior_index2  B_treat /*B_Sbehavior_index2 district_? B_Sgrade6 ///
	${el2_behavior_common_flag} */ ///
	if B_Sgirl ///
	& !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Sbehavior_index2 if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=girls2

eststo g3: qui reg E2_Spetition_index2  B_treat ///district_? B_Sgrade6 ///
	if B_Sgirl ///
	& !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) // no baseline index or flags
qui sum E2_Spetition_index2 if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=girls3

//Boys
eststo b1: qui  reg E2_Sgender_index2 B_treat /*B_Sgender_index2  district_? B_Sgrade6 ///
	${el2_gender_flag} */ ///
	if !B_Sgirl ///
	& !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Sgender_index2 if !B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=boys1

eststo b2: qui reg E2_Sbehavior_index2  B_treat /*B_Sbehavior_index2 district_? B_Sgrade6 ///
	${el2_behavior_common_flag} */ ///
	if !B_Sgirl ///
	& !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Sbehavior_index2 if !B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=boys2

eststo b3: qui reg E2_Spetition_index2  B_treat ///district_? B_Sgrade6 ///
	if !B_Sgirl ///
	& !mi(E2_Steam_id) & attrition==0, cluster(Sschool_id) // no baseline index or flags
qui sum E2_Spetition_index2 if !B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=boys3

esttab g1 b1 g2 b2 g3 b3, ///keep(B_treat) ///
	label stats(control_group_mean N r2 pval_joint_hypothesis1 pval_joint_hypothesis2, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "R2" "P-value, joint hypothesis (girls)" "P-value, joint hypothesis (boys)" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)	

latex_header, table(10)	
esttab g1 b1 g2 b2 g3 b3 using "${mytables}/no_controls/table10.tex", replace ///	
	///keep(B_treat) ///
	label stats(/*control_group_mean*/ N r2 empty pval_joint_hypothesis1 pval_joint_hypothesis2, fmt(/*${fmt}*/ %9.0fc ${fmt}) label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypotheses:" "   - Girls" "   - Boys"  )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	///
	$preheader 	///
	mtitle(${header}) nonumbers posthead("${numbers}")
	
	
**********
*Table 11*
**********
u if  !mi(E2_Steam_id) & attrition==0 using "$finaldata", clear

summ B_Saspiration_index2 if B_Sgirl==1, detail
local median= `r(p50)'
gen B_Saspiration_index2_abm=1 if B_Saspiration_index2>`median' & !mi(B_Saspiration_index2)  & B_Sgirl==1
replace B_Saspiration_index2_abm=0 if B_Saspiration_index2<=`median' & !mi(B_Saspiration_index2)  & B_Sgirl==1

g treat_aspiration_index2 = B_treat*B_Saspiration_index2
g treat_aspiration_index2_abm = B_treat*B_Saspiration_index2_abm
g treat_disc_edu_goals = B_treat*B_Sdisc_edu_goals

label var treat_aspiration_index2 "Treated X BL aspiration index"
label var treat_aspiration_index2_abm "Treated x Above-median BL aspirations"
label var treat_disc_edu_goals "Treated x Has discussed educ goals with parent"
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

keep if B_Sgirl

//Adjusted p-values
next_seed
mhtreg ///
		    (E2_Sscholar_index2 B_treat treat_aspiration_index2       B_Saspiration_index2      /*B_Sgrade6 district_? B_Saspiration_index2_flag*/) ///
			(E2_Sscholar_index2 B_treat treat_aspiration_index2_abm   B_Saspiration_index2_abm  /*B_Sgrade6 district_? B_Saspiration_index2_abm_flag*/) ///
			(E2_Sscholar_index2 B_treat treat_disc_edu_goals 	      B_Sdisc_edu_goals  	    /*B_Sgrade6 district_? B_Sdisc_edu_goals_flag */) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(B_treat)		

next_seed
mhtreg ///
		    (E2_Sscholar_index2 treat_aspiration_index2   		B_treat  B_Saspiration_index2      /*B_Sgrade6 district_? B_Saspiration_index2_flag*/) ///
			(E2_Sscholar_index2 treat_aspiration_index2_abm		B_treat  B_Saspiration_index2_abm  /*B_Sgrade6 district_? B_Saspiration_index2_abm_flag*/) ///
			(E2_Sscholar_index2 treat_disc_edu_goals      		B_treat  B_Sdisc_edu_goals  	   /*B_Sgrade6 district_? B_Sdisc_edu_goals_flag */) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(treat_aspiration_index2) matname(other)	

mat colnames other1 = "treat_aspiration_index2"
mat colnames other2 = "treat_aspiration_index2_abm"
mat colnames other3 = "treat_disc_edu_goals"

mat reg1 = B_treat1, other1
mat reg2 = B_treat2, other2
mat reg3 = B_treat3, other3

//Joint hypothesis
next_seed
randcmd ///
			((B_treat) reg E2_Sscholar_index2 B_treat treat_aspiration_index2       B_Saspiration_index2      /*B_Sgrade6 district_? B_Saspiration_index2_flag*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Sscholar_index2 B_treat treat_aspiration_index2_abm   B_Saspiration_index2_abm  /*B_Sgrade6 district_? B_Saspiration_index2_abm_flag*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Sscholar_index2 B_treat treat_disc_edu_goals 	        B_Sdisc_edu_goals  	      /*B_Sgrade6 district_? B_Sdisc_edu_goals_flag*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval1 "$tmp_val"

next_seed
randcmd ///
			((treat_aspiration_index2) 		reg E2_Sscholar_index2 B_treat treat_aspiration_index2        B_Saspiration_index2      /*B_Sgrade6 district_? B_Saspiration_index2_flag*/, cluster(Sschool_id)) ///
			((treat_aspiration_index2_abm) 	reg E2_Sscholar_index2 B_treat treat_aspiration_index2_abm    B_Saspiration_index2_abm  /*B_Sgrade6 district_? B_Saspiration_index2_abm_flag*/, cluster(Sschool_id)) ///
			((treat_disc_edu_goals) 		reg E2_Sscholar_index2 B_treat treat_disc_edu_goals 	      B_Sdisc_edu_goals  	    /*B_Sgrade6 district_? B_Sdisc_edu_goals_flag*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval2 "$tmp_val"

//Run regs
use "$finaldata", clear

summ B_Saspiration_index2 if B_Sgirl==1, detail
local median= `r(p50)'
gen B_Saspiration_index2_abm=1 if B_Saspiration_index2>`median' & !mi(B_Saspiration_index2)  & B_Sgirl==1
replace B_Saspiration_index2_abm=0 if B_Saspiration_index2<=`median' & !mi(B_Saspiration_index2)  & B_Sgirl==1

g treat_aspiration_index2 = B_treat*B_Saspiration_index2
g treat_aspiration_index2_abm = B_treat*B_Saspiration_index2_abm
g treat_disc_edu_goals = B_treat*B_Sdisc_edu_goals

label var treat_aspiration_index2 "Treated X BL aspiration index"
label var treat_aspiration_index2_abm "Treated x Above-median BL aspirations"
label var treat_disc_edu_goals "Treated x Has discussed educ goals with parent"

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

estimates clear
eststo: reg E2_Sscholar_index2 B_treat treat_aspiration_index2 B_Saspiration_index2  ///
	///B_Sgrade6 district_? B_Saspiration_index2_flag ///
	if B_Sgirl==1 & !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Sscholar_index2 if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg1
estadd local pval_joint_hypothesis1 "`joint_pval1'"
estadd local pval_joint_hypothesis2 "`joint_pval2'"

	
eststo: reg E2_Sscholar_index2 B_treat treat_aspiration_index2_abm B_Saspiration_index2_abm ///
	///B_Sgrade6 district_? B_Saspiration_index2_abm_flag ///
	if B_Sgirl==1 & !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Sscholar_index2 if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg2

eststo: reg E2_Sscholar_index2 B_treat treat_disc_edu_goals B_Sdisc_edu_goals ///
	///B_Sgrade6 district_? B_Sdisc_edu_goals_flag ///
	if B_Sgirl==1 & !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Sscholar_index2 if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0 
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=reg3

esttab, ///keep(B_treat  treat_aspiration_index2 treat_aspiration_index2_abm treat_disc_edu_goals ) ///
	label stats(control_group_mean N r2 pval_joint_hypothesis1 pval_joint_hypothesis2, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "R2" "P-value, joint hypothesis (Treated)" "P-value, joint hypothesis (interactions)" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)	

latex_header, table(11)	
esttab using "${mytables}/no_controls/table11.tex", replace ///
	///keep(B_treat  treat_aspiration_index2 treat_aspiration_index2_abm treat_disc_edu_goals ) ///
	label stats(/*control_group_mean*/ N r2 empty pval_joint_hypothesis1 pval_joint_hypothesis2, fmt(/*${fmt}*/ %9.0fc ${fmt}) label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypotheses:" "   - Treated" "   - Interactions")) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)		///
	$preheader 	///
	nomtitles nonumbers posthead("${numbers}")
		
	
**********
*Table 12*
**********
//Girls regression - with authors mistake
use if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear
label var B_treat "Treated"
next_seed
mhtreg ///
		    (E2_Sallow_work_s 				B_treat /*district_? B_Sgrade6*/) ///
			(E2_Scommunity_allow_work_s 	B_treat /*district_? B_Sgrade6*/) ///
			(E2_Scommunity_work_s 			B_treat /*district_? B_Sgrade6*/) ///
			(E2_Sallow_college_s 			B_treat /*district_? B_Sgrade6*/) ///
			(E2_Scommunity_allow_college_s 	B_treat /*district_? B_Sgrade6*/) ///
			(E2_Scommunity_college_s 		B_treat /*district_? B_Sgrade6*/) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(girls)			

	//Girls regression  - corrected
use if B_Sgirl & !mi(E2_Steam_id) & attrition==0  using "$finaldata", clear
label var B_treat "Treated"
next_seed
mhtreg ///
		    (E2_Sallow_work_s 				B_treat /*district_? B_Sgrade6*/) ///
			(E2_Scommunity_allow_work_s 	B_treat /*district_? B_Sgrade6*/) ///
			(E2_Scommunity_work_s 			B_treat /*district_? B_Sgrade6*/) ///
			(E2_Sallow_college_s 			B_treat /*district_? B_Sgrade6*/) ///
			(E2_Scommunity_allow_college_s 	B_treat /*district_? B_Sgrade6*/) ///
			(E2_Scommunity_college_s 		B_treat /*district_? B_Sgrade6*/) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(girls_corrected)
	
	//Boys
use if !B_Sgirl & !mi(E2_Steam_id) & attrition==0  using "$finaldata", clear
label var B_treat "Treated"
next_seed
mhtreg ///
		    (E2_Sallow_work_s 				B_treat /*district_? B_Sgrade6*/) ///
			(E2_Scommunity_allow_work_s 	B_treat /*district_? B_Sgrade6*/) ///
			(E2_Scommunity_work_s 			B_treat /*district_? B_Sgrade6*/) ///
			(E2_Sallow_college_s 			B_treat /*district_? B_Sgrade6*/) ///
			(E2_Scommunity_allow_college_s 	B_treat /*district_? B_Sgrade6*/) ///
			(E2_Scommunity_college_s 		B_treat /*district_? B_Sgrade6*/) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat) matname(boys)

//Joint hypothesis
		//Girls regression - with authors mistake
use if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 using "$finaldata", clear
label var B_treat "Treated"
next_seed
randcmd ///
			((B_treat) reg E2_Sallow_work_s 				B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Scommunity_allow_work_s 		B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Scommunity_work_s 			B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Sallow_college_s 				B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Scommunity_allow_college_s 	B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Scommunity_college_s 			B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval1 "$tmp_val"

	//Boys regression, with mistake
use if B_Sgirl & !mi(E2_Steam_id) & attrition==0  using "$finaldata", clear
label var B_treat "Treated"
next_seed
randcmd ///
			((B_treat) reg E2_Sallow_work_s 				B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Scommunity_allow_work_s 		B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Scommunity_work_s 			B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Sallow_college_s 				B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Scommunity_allow_college_s 	B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Scommunity_college_s 			B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval2 "$tmp_val"

	//Boys regression
use if !B_Sgirl & !mi(E2_Steam_id) & attrition==0  using "$finaldata", clear
label var B_treat "Treated"
next_seed
randcmd ///
			((B_treat) reg E2_Sallow_work_s 				B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Scommunity_allow_work_s 		B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Scommunity_work_s 			B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Sallow_college_s 				B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Scommunity_allow_college_s 	B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			((B_treat) reg E2_Scommunity_college_s 			B_treat /*district_? B_Sgrade6*/, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval3 "$tmp_val"

//Run regressions

use "$finaldata", clear
label var B_treat "Treated"

//Girls regression - with authors mistake
loc n=1
foreach var of varlist E2_Sallow_work_s E2_Scommunity_allow_work_s E2_Scommunity_work_s E2_Sallow_college_s E2_Scommunity_allow_college_s E2_Scommunity_college_s {
	eststo g`n' :qui reg `var' B_treat /*district_? B_Sgrade6*/  ///
		if B_Sgirl & !mi(E_Steam_id) & attrition_el==0 , cluster(Sschool_id)
	qui sum `var' if B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0
	myround, name(mean) 
	estadd local control_group_mean = "$tmp_val"
	estadd mat robust_p=girls`n'
	if `n'==1 estadd local pval_joint_hypothesis "`joint_pval1'"
	loc n=`n'+1
}

//Girls regression  - corrected
loc n=1
foreach var of varlist E2_Sallow_work_s E2_Scommunity_allow_work_s E2_Scommunity_work_s E2_Sallow_college_s E2_Scommunity_allow_college_s E2_Scommunity_college_s {
	eststo g`n'_corrected :qui reg `var' B_treat /*district_? B_Sgrade6*/  ///
		if B_Sgirl & !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
	qui sum `var' if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
	myround, name(mean) 
	estadd local control_group_mean = "$tmp_val"
	qui count if B_Sgirl & !mi(`var') &  (!mi(E2_Steam_id) & attrition==0) & !(!mi(E_Steam_id) & attrition_el==0)
	estadd local obs_dropped = r(N)
	estadd mat robust_p=girls_corrected`n'
	if `n'==1 estadd local pval_joint_hypothesis "`joint_pval2'"
	loc n=`n'+1
}

//Boys regression
loc n=1
foreach var of varlist E2_Sallow_work_s E2_Scommunity_allow_work_s E2_Scommunity_work_s E2_Sallow_college_s E2_Scommunity_allow_college_s E2_Scommunity_college_s {
	eststo b`n'  :qui reg `var' B_treat /*district_? B_Sgrade6*/  /// 
		if !B_Sgirl & !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
	qui sum `var' if !B_Sgirl & !B_treat & !mi(E_Steam_id) & attrition_el==0
	myround, name(mean) 
	estadd local control_group_mean = "$tmp_val"
	estadd mat robust_p=boys`n'
	if `n'==1 estadd local pval_joint_hypothesis "`joint_pval3'"
	loc n=`n'+1
}

//Tabulate
esttab g?, ///keep(B_treat) ///
	label stats(control_group_mean N r2 pval_joint_hypothesis, fmt(${fmt} %9.0fc ${fmt}) 				    label("Control group mean" "Number of students" "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)

esttab g?_corrected, ///keep(B_treat) ///
	label stats(control_group_mean N obs_dropped r2 pval_joint_hypothesis, fmt(${fmt} %9.0fc %9.0fc ${fmt}) label("Control group mean" "Number of students" "Observations dropped in authors regression" "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)	

esttab b?, ///keep(B_treat) ///
	label stats(control_group_mean N r2 pval_joint_hypothesis, fmt(${fmt} %9.0fc ${fmt})             label("Control group mean" "Number of students"  "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)	

//Store
latex_header, table(12)	
esttab g? using "${mytables}/no_controls/table12_girls.tex", replace ///
	///keep(B_treat) ///
	label stats(/*control_group_mean*/ N r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc ${fmt}) 				    label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none) ///
	$preheader 	///
	mtitle(${header}) nonumbers posthead("${numbers}")
	
esttab g?_corrected using "${mytables}/no_controls/table12_girls_corrected.tex", replace ///
	///keep(B_treat) ///
	label stats(/*control_group_mean*/ N r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc ${fmt}) 				    label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	///
	$preheader 	///
	mtitle(${header}) nonumbers posthead("${numbers}")
	
esttab b? using "${mytables}/no_controls/table12_boys.tex", replace ///
	///keep(B_treat) ///
	label stats(/*control_group_mean*/ N r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc ${fmt}) 				    label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	///
	$preheader 	///
	mtitle(${header}) nonumbers posthead("${numbers}")




**********
*Table 13*
**********
use "$finaldata", clear
label var B_treat "Treated"
bys Sschool_id B_Sgrade6: gen uniq_scl_grade = _n==1 
bys Sschool_id B_Sgrade6: egen att_max = min(attrition) // if 1 then should be dropped.
gen E2_Smar_fert_asp_index2_g = E2_Smar_fert_asp_index2 if B_Sgirl == 1
la var E2_Smar_fert_asp_index2_g "Marriage and fertility aspirations (Girls)"
gen E2_Smar_fert_asp_index2_b = E2_Smar_fert_asp_index2 if B_Sgirl == 0
la var E2_Smar_fert_asp_index2_b "Marriage and fertility aspirations (Boys)"
egen E2_Slr = max(E2_Slr_harass_b_sch_grad), by(Sschool_id B_Sgrade6)

keep if !mi(E2_Steam_id) & attrition==0 
	//Adjusted p-values
next_seed
mhtreg ///
		    (E2_Sesteem_index2_girl    B_treat /*B_Sesteem_index2_girl district_gender_* gender_grade_* ${el2_esteem_girl_flag} 	*/ if B_Sgirl) ///
			(E2_Seduc_attain_index2_g  B_treat /*district_gender_* gender_grade_* ${el2_educ_attain_flag} 							*/ if B_Sgirl) ///
			(E2_Smar_fert_asp_index2_g B_treat /*district_gender_* gender_grade_**/) ///
			(E2_Smar_fert_asp_index2_b B_treat /*district_gender_* gender_grade_**/) ///
			(E2_Sharassed_index2_g     B_treat /*district_gender_* gender_grade_* ${el2_harassed_flag}*/) ///
			(E2_Slr B_treat /*B_Sgrade6 district_?  																				*/ if  att_max==0) ///
			, cluster(Sschool_id) cltype(3) seed(${seed}) robust $mhtreg_bootstrap
store_mht_se, varname(B_treat)			

	//Joint hypothesis
next_seed
randcmd ///
	((B_treat) reg E2_Sesteem_index2_girl 	 B_treat /*B_Sesteem_index2_girl district_gender_* gender_grade_* ${el2_esteem_girl_flag} 		*/ if B_Sgirl, cluster(Sschool_id)) ///
	((B_treat) reg E2_Seduc_attain_index2_g	 B_treat /*district_gender_* gender_grade_* ${el2_educ_attain_flag} 							*/ if B_Sgirl, cluster(Sschool_id)) ///
	((B_treat) reg E2_Smar_fert_asp_index2_g B_treat /*district_gender_* gender_grade_*														*/			 , cluster(Sschool_id)) ///
	((B_treat) reg E2_Smar_fert_asp_index2_b B_treat /*district_gender_* gender_grade_*														*/			 , cluster(Sschool_id)) ///
	((B_treat) reg E2_Slr 					 B_treat /*B_Sgrade6 district_?  																*/ if  att_max==0, cluster(Sschool_id)) ///
			, treatvars(B_treat) groupvar(Sschool_id) seed(${seed}) $randcmd_bootstrap
global tmp = e(REqn)[4,3]	
myround, global(tmp)	
loc joint_pval "$tmp_val"





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

/*eststo t1: reg E2_Sesteem_index2_girl  B_treat B_Sesteem_index2_girl district_gender_* gender_grade_* ${el2_esteem_girl_flag}  if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1 , cluster(Sschool_id) //They removed the baseline controll by a mistake
qui sum E2_Sesteem_index2_girl if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"*/

eststo t1: reg E2_Sesteem_index2_girl  B_treat /*B_Sesteem_index2_girl district_gender_* gender_grade_* ${el2_esteem_girl_flag}  	*/ if !mi(E2_Steam_id) & attrition==0 & B_Sgirl==1 , cluster(Sschool_id) 
qui sum E2_Sesteem_index2_girl if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=p1
estadd local pval_joint_hypothesis "`joint_pval'"

eststo t2: reg E2_Seduc_attain_index2_g B_treat /*district_gender_* gender_grade_* ${el2_educ_attain_flag} 							*/ if !mi(E2_Steam_id) & attrition==0  & B_Sgirl==1 , cluster(Sschool_id)
qui sum E2_Seduc_attain_index2_g if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=p2

eststo t3: reg E2_Smar_fert_asp_index2_g B_treat /*district_gender_* gender_grade_* 												*/ if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Smar_fert_asp_index2_g if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=p3

/*eststo t4: reg E2_Smar_fert_asp_index2_b B_treat district_gender_* gender_grade_*  if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
sum E2_Smar_fert_asp_index2_b  if B_treat==0 & !mi(E2_Steam_id) & attrition==0  //if !B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"*/

eststo t4: reg E2_Smar_fert_asp_index2_b B_treat /*district_gender_* gender_grade_* 												*/ if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
sum E2_Smar_fert_asp_index2_b  if B_treat==0 & !mi(E2_Steam_id) & attrition==0  //if !B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=p4

eststo t5: reg E2_Sharassed_index2_g B_treat /*district_gender_* gender_grade_* ${el2_harassed_flag} 								*/ if !mi(E2_Steam_id) & attrition==0 , cluster(Sschool_id)
qui sum E2_Sharassed_index2_g if B_Sgirl & !B_treat & !mi(E2_Steam_id) & attrition==0
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=p5

eststo t6: reg E2_Slr B_treat /*B_Sgrade6 district_?  																				*/ if  att_max==0 & uniq_scl_grade==1, cluster(Sschool_id)
qui sum E2_Slr if !B_treat &  att_max==0 & uniq_scl_grade==1
myround, name(mean) 
estadd local control_group_mean = "$tmp_val"
estadd mat robust_p=p6

esttab t?, ///keep(B_treat) ///
	label stats(control_group_mean N r2 pval_joint_hypothesis, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "R2" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)
	
latex_header, table(13)	
esttab t? using "${mytables}/no_controls/table13.tex", replace ///	
	///keep(B_treat) ///
	label stats(/*control_group_mean*/ N r2 pval_joint_hypothesis, fmt(/*${fmt}*/ %9.0fc ${fmt}) label(/*"Control group mean"*/ "Number of students" "\$R^2$" "P-value, joint hypothesis" )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_p(fmt(${decimals}) par(`"["' `"]"')))  /*nonumbers*/ collabels(none)	///
	$preheader 	///
	mtitle(${header}) nonumbers posthead("${numbers}")

	
	
/*esttab t1_corrected t2 t3 t4_corrected t5 t6, keep(B_treat) ///
	label stats(control_group_mean N r2 /*pval_joint_hypothesis*/, fmt(${fmt} %9.0fc ${fmt}) label("Control group mean" "Number of students" "R2" /*"P-value, joint hypothesis"*/ )) ///
	cells(b(fmt(${decimals})) ${p_se}(fmt(${decimals}) par) robust_se(fmt(${decimals}) par(`"["' `"]"')))  nonumbers collabels(none)*/
	



cap log close


