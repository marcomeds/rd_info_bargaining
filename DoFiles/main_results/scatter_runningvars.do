
********************
version 17.0
********************
/* 
/*******************************************************************************
* Name of file:	
* Author:	Isaac M
* Machine:	Isaac M 											
* Date of creation:	May. 05, 2022
* Last date of modification:   
* Modifications:		
* Files used:     
* Files created:  

* Purpose: Scatter plot for the multiple running variables
*******************************************************************************/
*/


use "$directorio\DB\survey_data_2m.dta", clear
merge 1:1 id_actor using "$directorio\DB\treatment_data.dta", keep(2 3)

*keep if main_treatment!=1  & !missing(main_treatment)

gen corte_dw = salario_diario>211 if !missing(salario_diario)
gen corte_tenure = antiguedad>2.67 if !missing(antiguedad)

gen quadrant = 1 if corte_dw==1 & corte_tenure==1
replace quadrant = 2 if corte_dw==1 & corte_tenure==0
replace quadrant = 3 if corte_dw==0 & corte_tenure==0
replace quadrant = 4 if corte_dw==0 & corte_tenure==1
*-------------------------------------------------------------------------------


*Graph scatter running variables
twoway (scatter antiguedad salario_diario  if main_treatment==1 & inrange(antiguedad,0,11) & inrange(salario_diario,0,1000) & quadrant==1, ms(Oh) msize(small) color(navy%60)) ///
	(scatter antiguedad salario_diario  if main_treatment==1 & inrange(antiguedad,0,11) & inrange(salario_diario,0,1000) & quadrant==2, ms(Oh) msize(small) color(maroon%60)) ///
	(scatter antiguedad salario_diario  if main_treatment==1 & inrange(antiguedad,0,11) & inrange(salario_diario,0,1000) & quadrant==3, ms(Oh) msize(small) color(dkgreen%60)) ///
	(scatter antiguedad salario_diario  if main_treatment==1 & inrange(antiguedad,0,11) & inrange(salario_diario,0,1000) & quadrant==4, ms(Oh) msize(small) color(purple%60)) ///	
	(scatter antiguedad salario_diario  if main_treatment==2 & inrange(antiguedad,0,11) & inrange(salario_diario,0,1000) & quadrant==1, ms(Sh) msize(small) color(navy)) ///
	(scatter antiguedad salario_diario  if main_treatment==2 & inrange(antiguedad,0,11) & inrange(salario_diario,0,1000) & quadrant==2, ms(Sh) msize(small) color(maroon)) ///
	(scatter antiguedad salario_diario  if main_treatment==2 & inrange(antiguedad,0,11) & inrange(salario_diario,0,1000) & quadrant==3, ms(Sh) msize(small) color(dkgreen)) ///
	(scatter antiguedad salario_diario  if main_treatment==2 & inrange(antiguedad,0,11) & inrange(salario_diario,0,1000) & quadrant==4, ms(Sh) msize(small) color(purple)) ///	
	(scatter antiguedad salario_diario  if main_treatment==3 & inrange(antiguedad,0,11) & inrange(salario_diario,0,1000) & quadrant==1, ms(Th) msize(small) color(navy)) ///
	(scatter antiguedad salario_diario  if main_treatment==3 & inrange(antiguedad,0,11) & inrange(salario_diario,0,1000) & quadrant==2, ms(Th) msize(small) color(maroon)) ///
	(scatter antiguedad salario_diario  if main_treatment==3 & inrange(antiguedad,0,11) & inrange(salario_diario,0,1000) & quadrant==3, ms(Th) msize(small) color(dkgreen)) ///
	(scatter antiguedad salario_diario  if main_treatment==3 & inrange(antiguedad,0,11) & inrange(salario_diario,0,1000) & quadrant==4, ms(Th) msize(small) color(purple)) ///
	, yline(2.67, lcolor(black) lwidth(medthick)) xline(211, lcolor(black) lwidth(medthick)) xtitle("Daily wage") ytitle("Tenure") graphregion(color(white)) legend(order(1 "[61, 90]" 2 "[42, 59]" 3 "[51, 67]" 4 "[68, 98]") cols(4))
	
graph export "$directorio/Figuras/scatter_rddesign.pdf", replace

