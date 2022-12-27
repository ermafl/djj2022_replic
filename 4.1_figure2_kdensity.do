use "$finaldata", clear

label var E_Sgender_index2 "Gender attitudes index"
label var E_Saspiration_index2 "Girls' aspirations index"
label var E_Sbehavior_index2 "Self-reported behavior index"
label var B_treat "Treated"

set scheme plotplain
		
qui sum E_Sgender_index2 if B_treat
		loc T=r(mean)
qui sum E_Sgender_index2 if !B_treat
		loc C=r(mean)
twoway  (kdensity E_Sgender_index2    	if  B_treat & !mi(E_Steam_id) & attrition_el==0, bw(.15) lc(black) lp(solid)) ///
		(kdensity E_Sgender_index2 		if !B_treat & !mi(E_Steam_id) & attrition_el==0, bw(.15) lc(gs10) lp(dash)) ///
		, legend(order(1 "Treated" 2 "Control")) ///
		xline(`T', lc(black) lp(solid)) ///
		xline(`C', lc(gs10) lp(dash)) ///
		ytitle("Density") xtitle("Gender index") 
graph export "${mytables}/fig1.pdf", replace

qui sum E_Saspiration_index2 if B_treat
		loc T=r(mean)
qui sum E_Saspiration_index2 if !B_treat
		loc C=r(mean)
twoway  (kdensity E_Saspiration_index2 	if  B_treat & !mi(E_Steam_id) & attrition_el==0, bw(.15) lc(black) lp(solid)) ///
		(kdensity E_Saspiration_index2 	if !B_treat & !mi(E_Steam_id) & attrition_el==0, bw(.15) lc(gs10) lp(dash)) ///
		, legend(order(1 "Treated" 2 "Control")) ///
		xline(`T', lc(black) lp(solid)) ///
		xline(`C', lc(gs10) lp(dash)) ///
		ytitle("Density") xtitle("Aspiration index")
graph export "${mytables}/fig2.pdf", replace
		
qui sum E_Sbehavior_index2 if B_treat
		loc T=r(mean)
qui sum E_Sbehavior_index2 if !B_treat
		loc C=r(mean)
twoway  (kdensity E_Sbehavior_index2 	if  B_treat & !mi(E_Steam_id) & attrition_el==0, bw(.15) lc(black) lp(solid)) ///
		(kdensity E_Sbehavior_index2 	if !B_treat & !mi(E_Steam_id) & attrition_el==0, bw(.15) lc(gs10) lp(dash)) ///
		, legend(order(1 "Treated" 2 "Control")) ///
		xline(`T', lc(black) lp(solid)) ///
		xline(`C', lc(gs10) lp(dash)) ///
		ytitle("Density") xtitle("Behavior index")
graph export "${mytables}/fig3.pdf", replace
