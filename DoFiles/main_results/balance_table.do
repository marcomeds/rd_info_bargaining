
********************
version 17.0
********************
/* 
/*******************************************************************************
* Name of file:	
* Author:	Isaac M
* Machine:	Isaac M 											
* Date of creation:	May. 12, 2022
* Last date of modification:   
* Modifications:		
* Files used:     
* Files created:  

* Purpose: Regression discontinuity analysis for predetermined variables along each dimension making pairwise comparisons in the tenure-daily wage plane.
*******************************************************************************/
*/

clear all
use "$directorio\DB\survey_data_2m.dta", clear
merge 1:1 id_actor using "$directorio\DB\treatment_data.dta", keep(3)

keep if main_treatment!=1 & !missing(main_treatment)
gen corte_dw = salario_diario>211 if !missing(salario_diario)
gen corte_tenure = antiguedad>2.67 if !missing(antiguedad)

gen quadrant = 1 if corte_dw==1 & corte_tenure==1
replace quadrant = 2 if corte_dw==1 & corte_tenure==0
replace quadrant = 3 if corte_dw==0 & corte_tenure==0
replace quadrant = 4 if corte_dw==0 & corte_tenure==1
*-------------------------------------------------------------------------------

*Index for running variable along two dimensions
su antiguedad
generate double aux1 = (abs((2.67 - antiguedad)/`r(sd)'))^2
su salario_diario
generate double aux2 = (abs((211 - salario_diario)/`r(sd)'))^2
egen normp = rowtotal(aux1 aux2), missing
replace normp = . if missing(antiguedad) | missing(salario_diario)
replace normp = (normp)^(1/2)


local controls nivel_de_felicidad  high_school reclutamiento dummy_confianza horas_sem  dummy_sarimssinfo  c_min_indem  c_min_total top_demandado  mujer

*-------------------------------------------------------------------------------



foreach t in 2 3 {
capture erase "$directorio/Tables/reg_results/rd_balance_t`t'.xls"
capture erase "$directorio/Tables/reg_results/rd_balance_t`t'.txt"

***********************		   		Tenure				************************
* Pooled
foreach var of varlist `controls' {
	rdrobust `var' antiguedad if main_treatment==`t', c(2.67) all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5)
	su `var' if e(sample) & corte_tenure==0
	outreg2 using "$directorio/Tables/reg_results/rd_balance_t`t'.xls", addstat(Left bandwidth,  e(h_l), Right bandwidth,  e(h_r), Effective obs (left), e(N_h_l), Effective obs (right), e(N_h_r), p, e(p), q, e(q), Control mean, `r(mean)') addtext(Kernel,  `e(kernel)', bwselect, `e(bwselect)', vce, `e(vce_select)') 
}


***********************		   		  Wage				************************
* Pooled
foreach var of varlist `controls' {
	rdrobust `var' salario_diario if main_treatment==`t', c(211) all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5)
	su `var' if e(sample) & corte_dw==0
	outreg2 using "$directorio/Tables/reg_results/rd_balance_t`t'.xls", addstat(Left bandwidth,  e(h_l), Right bandwidth,  e(h_r), Effective obs (left), e(N_h_l), Effective obs (right), e(N_h_r), p, e(p), q, e(q), Control mean, `r(mean)') addtext(Kernel,  `e(kernel)', bwselect, `e(bwselect)', vce, `e(vce_select)') 
}


********************		   	 Tenure & Wage				********************

* II vs IV
cap drop t index
gen t = (corte_dw==0 & corte_tenure==1) if !missing(corte_dw) & !missing(corte_tenure)
gen index = normp*(2*t-1)
foreach var of varlist `controls' {
	rdrobust `var' index if main_treatment==`t' & inlist(quadrant,2,4), c(0) all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5)
	su `var' if e(sample) & quadrant==2
	outreg2 using "$directorio/Tables/reg_results/rd_balance_t`t'.xls", addstat(Left bandwidth,  e(h_l), Right bandwidth,  e(h_r), Effective obs (left), e(N_h_l), Effective obs (right), e(N_h_r), p, e(p), q, e(q), Control mean, `r(mean)') addtext(Kernel,  `e(kernel)', bwselect, `e(bwselect)', vce, `e(vce_select)') 
}


}