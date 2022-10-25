
********************
version 17.0
********************
/* 
/*******************************************************************************
* Name of file:	
* Author:	Isaac M
* Machine:	Isaac M 											
* Date of creation:	May. 14, 2022  
* Last date of modification: 
* Modifications: 
* Files used:     
* Files created:  

* Purpose: Regression discontinuity plots for control. Robustness check
*******************************************************************************/
*/


use "$directorio\DB\survey_data_2m.dta", clear
merge 1:1 id_actor using "$directorio\DB\treatment_data.dta", keep(3)

keep if !missing(main_treatment)
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


*-------------------------------------------------------------------------------

foreach var of varlist conflicto_arreglado {
foreach t in 1 {
***********************		   		Tenure				************************
* Pooled 
rd_plot `var' antiguedad  if main_treatment==`t', cutoff(2.67) p(1) q(2) kernel(triangular) bwselect(mserd) vce(nncluster fecha_alta 5) level(90)
graph export "$directorio/Figuras/rdplot_`var'_tenure_`t'.pdf", replace


***********************		   		  Wage				************************
* Pooled 
rd_plot `var' salario_diario  if main_treatment==`t', cutoff(211) p(1) q(2) kernel(triangular) bwselect(mserd) vce(nncluster fecha_alta 5) level(90)
graph export "$directorio/Figuras/rdplot_`var'_dw_`t'.pdf", replace


********************		   	 Tenure & Wage				********************
* II vs IV
cap drop t index
gen t = (corte_dw==0 & corte_tenure==1) if !missing(corte_dw) & !missing(corte_tenure)
gen index = normp*(2*t-1)
rd_plot `var' index  if main_treatment==`t' & inlist(quadrant,2,4), cutoff(0) p(1) q(2) kernel(triangular) bwselect(mserd) vce(nncluster fecha_alta 5) 	level(90)
graph export "$directorio/Figuras/rdplot_`var'_2_4_`t'.pdf", replace


}
}












