
********************
version 17.0
********************
/* 
/*******************************************************************************
* Name of file:	
* Author:	Isaac M
* Machine:	Isaac M 											
* Date of creation:	May. 16, 2022
* Last date of modification:   
* Modifications:		
* Files used:     
* Files created:  

* Purpose: Placebo cutoffs around true cutoff.
*******************************************************************************/
*/


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
*Cutoffs
range cutoff_tenure .67 4.67 100
replace cutoff_tenure = 2.67 if _n==101
levelsof cutoff_tenure, local(c_vals_tenure) 
	
range cutoff_dw 111 311 100
replace cutoff_dw = 211 if _n==101
levelsof cutoff_dw, local(c_vals_dw) 	
			
			
foreach var of varlist conflicto_arreglado {
foreach t in 2 3 {
	matrix rd_1_2_`t' = J(101,4,.)
	matrix rd_3_4_`t' = J(101,4,.)
	matrix rd_23_14_`t' = J(101,4,.)

	matrix rd_2_3_`t' = J(101,4,.)
	matrix rd_1_4_`t' = J(101,4,.)
	matrix rd_12_34_`t' = J(101,4,.)
	
	local j = 1
	noi di " "
	noi _dots 0, title(Loop through cutoffs) reps(100)
	noi di " "
	foreach c of local c_vals_tenure {
		***********************		   		Tenure				************************
		* I vs II
		qui rdrobust `var' antiguedad if main_treatment==`t' & inlist(quadrant,1,2), c(`c') all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5) covs(salario_diario `controls')
		matrix rd_1_2_`t'[`j',1] = e(tau_bc)
		matrix rd_1_2_`t'[`j',2] = e(tau_bc) - invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_1_2_`t'[`j',3] = e(tau_bc) + invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_1_2_`t'[`j',4] = `c'

		* III vs IV
		qui rdrobust `var' antiguedad if main_treatment==`t' & inlist(quadrant,3,4), c(`c') all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5) covs(salario_diario `controls')
		matrix rd_3_4_`t'[`j',1] = e(tau_bc)
		matrix rd_3_4_`t'[`j',2] = e(tau_bc) - invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_3_4_`t'[`j',3] = e(tau_bc) + invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_3_4_`t'[`j',4] = `c'		

		* Pooled
		qui rdrobust `var' antiguedad if main_treatment==`t', c(`c') all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5) covs(salario_diario `controls')
		matrix rd_23_14_`t'[`j',1] = e(tau_bc)
		matrix rd_23_14_`t'[`j',2] = e(tau_bc) - invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_23_14_`t'[`j',3] = e(tau_bc) + invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_23_14_`t'[`j',4] = `c'		
		
		noi _dots `j' 0	
		local j = `j' + 1
	}
	
	local j = 1
	noi di " "
	noi _dots 0, title(Loop through cutoffs) reps(100)
	noi di " "
	foreach c of local c_vals_dw {
		***********************		   		  Wage				************************

		* III vs II
		qui rdrobust `var' salario_diario if main_treatment==`t' & inlist(quadrant,2,3), c(`c') all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5) covs(antiguedad `controls')
		matrix rd_2_3_`t'[`j',1] = e(tau_bc)
		matrix rd_2_3_`t'[`j',2] = e(tau_bc) - invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_2_3_`t'[`j',3] = e(tau_bc) + invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_2_3_`t'[`j',4] = `c'
		
		* IV vs I
		qui rdrobust `var' salario_diario if main_treatment==`t' & inlist(quadrant,4,1), c(`c') all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5) covs(antiguedad `controls')
		matrix rd_1_4_`t'[`j',1] = e(tau_bc)
		matrix rd_1_4_`t'[`j',2] = e(tau_bc) - invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_1_4_`t'[`j',3] = e(tau_bc) + invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_1_4_`t'[`j',4] = `c'

		* Pooled
		qui rdrobust `var' salario_diario if main_treatment==`t', c(`c') all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5) covs(antiguedad `controls')
		matrix rd_12_34_`t'[`j',1] = e(tau_bc)
		matrix rd_12_34_`t'[`j',2] = e(tau_bc) - invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_12_34_`t'[`j',3] = e(tau_bc) + invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_12_34_`t'[`j',4] = `c'
		
		noi _dots `j' 0	
		local j = `j' + 1
	}

}
}


********************************************************************************

forvalues t = 2/3 {
	foreach id in 1_2_`t' 3_4_`t' 23_14_`t' {
		cap drop rd_beta rd_lo rd_hi cutoff
		matrix colnames rd_`id' = rd_beta rd_lo rd_hi cutoff
		svmat rd_`id', names(col)


		twoway (rcap rd_hi rd_lo cutoff, color(gs7) yline(0, lcolor(black)) ) ///
				(scatter rd_beta cutoff, msymbol(O) color(navy%70) xline(2.67, lcolor(maroon%50))) ///
				(scatter rd_beta cutoff if rd_hi<0, msymbol(O) color(maroon%30)) ///
				(scatter rd_beta cutoff if rd_lo>0, msymbol(O) color(maroon%30)) ///
				(scatter rd_beta cutoff if inrange(cutoff,2.655,2.675), msymbol(S) color(maroon)) ///
				, legend(off) graphregion(color(white)) xtitle("Cutoff") ytitle("RD estimate")
		graph export "$directorio/Figuras/placebo_cut_`id'.pdf", replace
	}
}


forvalues t = 2/3 {
	foreach id in 2_3_`t' 1_4_`t' 12_34_`t' {
		cap drop rd_beta rd_lo rd_hi cutoff
		matrix colnames rd_`id' = rd_beta rd_lo rd_hi cutoff
		svmat rd_`id', names(col)


		twoway (rcap rd_hi rd_lo cutoff if abs(rd_hi)<1 & abs(rd_lo)<1, color(gs7) yline(0, lcolor(black)) ) ///
				(scatter rd_beta cutoff, msymbol(O) color(navy%70) xline(211, lcolor(maroon%50))) ///
				(scatter rd_beta cutoff if rd_hi<0, msymbol(O) color(maroon%30)) ///
				(scatter rd_beta cutoff if rd_lo>0, msymbol(O) color(maroon%30)) ///
				(scatter rd_beta cutoff if inrange(cutoff,210.5,211.5), msymbol(S) color(maroon)) ///
				, legend(off) graphregion(color(white)) xtitle("Cutoff") ytitle("RD estimate")
		graph export "$directorio/Figuras/placebo_cut_`id'.pdf", replace
	}
}


