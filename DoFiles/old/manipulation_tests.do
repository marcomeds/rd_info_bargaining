
********************
version 17.0
********************
/* 
/*******************************************************************************
* Name of file:	
* Author:	Isaac M
* Machine:	Isaac M 											
* Date of creation:	May. 05, 2022
* Last date of modification:  May. 16, 2022 
* Modifications: Added manipulation plot for multi-running var comparison		
* Files used:     
* Files created:  

* Purpose: Manipulation tests. Follows "Simple Local Polynomial Density Estimators"  Matias D. Cattaneoa, Michael Jansson,and Xinwei Ma. (2020)
*******************************************************************************/
*/

use "$directorio\DB\survey_data_2m.dta", clear
merge 1:1 id_actor using "$directorio\DB\treatment_data.dta", keep(3)

*keep if main_treatment!=1  & !missing(main_treatment)

gen corte_dw = salario_diario>211 if !missing(salario_diario)
gen corte_tenure = antiguedad>2.67 if !missing(antiguedad)

gen quadrant = 1 if corte_dw==1 & corte_tenure==1
replace quadrant = 2 if corte_dw==1 & corte_tenure==0
replace quadrant = 3 if corte_dw==0 & corte_tenure==0
replace quadrant = 4 if corte_dw==0 & corte_tenure==1

*Index for running variable along two dimensions
su antiguedad
generate double aux1 = (abs((2.67 - antiguedad)/`r(sd)'))^2
su salario_diario
generate double aux2 = (abs((211 - salario_diario)/`r(sd)'))^2
egen normp = rowtotal(aux1 aux2), missing
replace normp = . if missing(antiguedad) | missing(salario_diario)
replace normp = (normp)^(1/2)

*-------------------------------------------------------------------------------

  
*Manipulation testing based on density discontinuity
rddensity antiguedad if inlist(quadrant,1,2), c(2.67) all plot   
rddensity antiguedad if inlist(quadrant,1,2), c(2.67) plot all  graph_opt(graphregion(color(white)) legend(off) xtitle("Tenure")  xline(2.67, lcolor(black)) note("Conventional p-value :  `:display %5.2f `e(pv_p)''") caption("Robust p-value : `:display %5.2f `e(pv_q)''", size(small))) ///
cirl_opt(color(navy%60)) esll_opt(color(navy) lwidth(medthick)) histl_opt(barw(`=min(3*`e(h_l)',2.67)/(ceil(max(sqrt(`e(N_l)'),10*log(`e(N_l)')/log(10))))') color(navy%30))  ///
cirr_opt(color(maroon%60)) eslr_opt(color(maroon) lwidth(medthick)) histr_opt(barw(`=3*`e(h_r)'/(ceil(max(sqrt(`e(N_r)'),10*log(`e(N_r)')/log(10))))') color(maroon%30))  
graph export "$directorio/Figuras/mtp_tenure_1_2.pdf", replace

rddensity antiguedad if inlist(quadrant,3,4), c(2.67)  all plot 
rddensity antiguedad if inlist(quadrant,3,4), c(2.67) plot all  graph_opt(graphregion(color(white)) legend(off) xtitle("Tenure")  xline(2.67, lcolor(black)) note("Conventional p-value :  `:display %5.2f `e(pv_p)''") caption("Robust p-value : `:display %5.2f `e(pv_q)''", size(small))) ///
cirl_opt(color(navy%60)) esll_opt(color(navy) lwidth(medthick)) histl_opt(barw(`=min(3*`e(h_l)',2.67)/(ceil(max(sqrt(`e(N_l)'),10*log(`e(N_l)')/log(10))))') color(navy%30))  ///
cirr_opt(color(maroon%60)) eslr_opt(color(maroon) lwidth(medthick)) histr_opt(barw(`=3*`e(h_r)'/(ceil(max(sqrt(`e(N_r)'),10*log(`e(N_r)')/log(10))))') color(maroon%30))  
graph export "$directorio/Figuras/mtp_tenure_3_4.pdf", replace


rddensity salario_diario if inlist(quadrant,2,3), c(211) all plot 
rddensity salario_diario if inlist(quadrant,2,3), c(211) plot all  graph_opt(graphregion(color(white)) legend(off) xtitle("Daily wage")  xline(211, lcolor(black)) note("Conventional p-value :  `:display %5.2f `e(pv_p)''") caption("Robust p-value : `:display %5.2f `e(pv_q)''", size(small))) ///
cirl_opt(color(navy%60)) esll_opt(color(navy) lwidth(medthick)) histl_opt(barw(`=min(3*`e(h_l)',211)/(ceil(max(sqrt(`e(N_l)'),10*log(`e(N_l)')/log(10))))') color(navy%30))  ///
cirr_opt(color(maroon%60)) eslr_opt(color(maroon) lwidth(medthick)) histr_opt(barw(`=3*`e(h_r)'/(ceil(max(sqrt(`e(N_r)'),10*log(`e(N_r)')/log(10))))') color(maroon%30))  
graph export "$directorio/Figuras/mtp_dw_2_3.pdf", replace


rddensity salario_diario if inlist(quadrant,4,1), c(211) all plot   
rddensity salario_diario if inlist(quadrant,4,1), c(211) plot all  graph_opt(graphregion(color(white)) legend(off) xtitle("Daily wage")  xline(211, lcolor(black)) note("Conventional p-value :  `:display %5.2f `e(pv_p)''") caption("Robust p-value : `:display %5.2f `e(pv_q)''", size(small))) ///
cirl_opt(color(navy%60)) esll_opt(color(navy) lwidth(medthick)) histl_opt(barw(`=min(3*`e(h_l)',211)/(ceil(max(sqrt(`e(N_l)'),10*log(`e(N_l)')/log(10))))') color(navy%30))  ///
cirr_opt(color(maroon%60)) eslr_opt(color(maroon) lwidth(medthick)) histr_opt(barw(`=3*`e(h_r)'/(ceil(max(sqrt(`e(N_r)'),10*log(`e(N_r)')/log(10))))') color(maroon%30))  
graph export "$directorio/Figuras/mtp_dw_4_1.pdf", replace


cap drop t index
gen t = (corte_dw==0 & corte_tenure==1) if !missing(corte_dw) & !missing(corte_tenure)
gen index = normp*(2*t-1)
rddensity index if inlist(quadrant,2,4), c(0) all plot   
rddensity index if inlist(quadrant,2,4), c(0) plot all  graph_opt(graphregion(color(white)) legend(off) xtitle("L2 distance to center")  xline(0, lcolor(black)) note("Conventional p-value :  `:display %5.2f `e(pv_p)''") caption("Robust p-value : `:display %5.2f `e(pv_q)''", size(small))) ///
cirl_opt(color(navy%60)) esll_opt(color(navy) lwidth(medthick)) histl_opt(barw(`=min(3*`e(h_l)',211)/(ceil(max(sqrt(`e(N_l)'),10*log(`e(N_l)')/log(10))))') color(navy%30))  ///
cirr_opt(color(maroon%60)) eslr_opt(color(maroon) lwidth(medthick)) histr_opt(barw(`=3*`e(h_r)'/(ceil(max(sqrt(`e(N_r)'),10*log(`e(N_r)')/log(10))))') color(maroon%30))  
graph export "$directorio/Figuras/mtp_index_2_4.pdf", replace

cap drop t index
gen t = (corte_dw==1 & corte_tenure==1) if !missing(corte_dw) & !missing(corte_tenure)
gen index = normp*(2*t-1)
rddensity index if inlist(quadrant,1,3), c(0) all plot   
rddensity index if inlist(quadrant,1,3), c(0) plot all  graph_opt(graphregion(color(white)) legend(off) xtitle("L2 distance to center")  xline(0, lcolor(black)) note("Conventional p-value :  `:display %5.2f `e(pv_p)''") caption("Robust p-value : `:display %5.2f `e(pv_q)''", size(small))) ///
cirl_opt(color(navy%60)) esll_opt(color(navy) lwidth(medthick)) histl_opt(barw(`=min(3*`e(h_l)',211)/(ceil(max(sqrt(`e(N_l)'),10*log(`e(N_l)')/log(10))))') color(navy%30))  ///
cirr_opt(color(maroon%60)) eslr_opt(color(maroon) lwidth(medthick)) histr_opt(barw(`=3*`e(h_r)'/(ceil(max(sqrt(`e(N_r)'),10*log(`e(N_r)')/log(10))))') color(maroon%30))  
graph export "$directorio/Figuras/mtp_index_1_3.pdf", replace

