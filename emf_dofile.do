
version 14
set more off


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * * * * * PART 1:  SET DIRECTORY (EDIT HERE) * * * * * * * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

global main_loc "C:\Users\erlendmf\Downloads\22feb_rep" //insert path where "Main Analysis and Paper" is stored
global do "$main_loc/Main Analysis and Paper/do_files"
adopath + "${do}/_ado"
	
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * PART 2: SET DATA PATHS * * * * * * * * * * *  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	
**********************
***** 2.2  DATASETS
**********************

global deid "$main_loc/Main Analysis and Paper/Analysis data"

********* BASELINE
global baseline_student_raw "$deid/baseline_student_raw"
global baseline_student_final "$deid/baseline_student_final"

global baseline_parent_raw "$deid/baseline_parent_raw"
global baseline_parent_final "$deid/baseline_parent_final"

global baseline_iat_raw "$deid/baseline_iat_raw"
global baseline_iat_cleaned "$deid/baseline_iat_cleaned"

global baseline "$deid/baseline_all_final" 
global baseline_sch "$deid/baseline_school_cleaned"

global baseline_cen "$deid/baseline_census_cleaned"

********* ENDLINE 1
global endline1_student_raw "$deid/endline1_student_raw"

global endline1_iat_raw1 "$deid/endline1_iat_raw1"
global endline1_iat_raw2 "$deid/endline1_iat_raw2"
global endline1_iat_cleaned "$deid/endline1_iat_cleaned"

global endline "$deid/endline1_student_final"

* school data
global endline_school_scert_raw "$deid/endline_school_scert_raw"
global endline_school_scert_final "$deid/endline_school_scert_final"
global endline_school_board_cleaned "$deid/endline_school_board_cleaned"

global endline_sch "$deid/endline1_school_final"


********* ENDLINE 2
global endline2_student_raw "$deid/endline2_student_raw"
global endline2_student_final "$deid/endline2_student_final"

********* Final data for analysis  
global finaldata "$deid/bt_analysis_final"

* output : tables and figures
global mainpaper "Main Analysis and Paper/Paper"
global tables "Main Analysis and Paper/Tables/paper_tables"
//global othtables "Main Analysis and Paper/Tables/other_internal_tables/EL2"
global figures "Main Analysis and Paper/Figures"
global slides "Main Analysis and Paper/Tables/beamer_tables"

* ad-hoc analysis
global ad_hoc "$main_loc/Main Analysis and Paper/ad-hoc analysis"



do "$do/04a_merge_indices_mod13dec.do" // combines all datasets and generates main variables and the FACTOR indices as comptued for the replication

do "$do/04b_tables_mod.do" // generates main and appendix tables, slide tables
//Relevant for the replication are two tables only: el1_primary_outcomes_basic_combined.tex and el2_primary_outcomes_basic_combined. 
//To get the original author versions of these tables, simply run the original authors' master do file, with their "04a_merge_indices" and "04b_tables" do-files.

do "$do/index_persistence.do" //Produces "Table 26 Intra-individual correlation ("persistence") of main indices" in the appendix of the replication report
do "$do/flagregs.do" //Produces Figure 3: Baseline attitudes and end line 1 missing values in the appendix of the replication report