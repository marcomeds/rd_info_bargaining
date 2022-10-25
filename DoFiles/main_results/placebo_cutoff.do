
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


*-------------------------------------------------------------------------------
*Cutoffs
gen cutoff_tenure = .
replace cutoff_tenure = 0.19 in 1
replace cutoff_tenure = 0.30 in 2
replace cutoff_tenure = 0.75 in 3
replace cutoff_tenure = 1.68 in 4
replace cutoff_tenure = 2.67 in 5
replace cutoff_tenure = 4.26 in 6
replace cutoff_tenure = 9.45 in 7
replace cutoff_tenure = 12.89 in 8
replace cutoff_tenure = 7.5 in 9
replace cutoff_tenure = 8.5 in 10

gen cutoff_dw = .
replace cutoff_dw = 106 in 1
replace cutoff_dw = 133 in 2
replace cutoff_dw = 171 in 3
replace cutoff_dw = 211 in 4
replace cutoff_dw = 233 in 5
replace cutoff_dw = 346 in 6
replace cutoff_dw = 533 in 7
replace cutoff_dw = 666 in 8
replace cutoff_dw = 450 in 9
replace cutoff_dw = 500 in 10

levelsof cutoff_tenure, local(c_vals_tenure) 
levelsof cutoff_dw, local(c_vals_dw) 	


foreach var of varlist conflicto_arreglado {
foreach t in 2 3 {
	matrix rd_tenure_`t' = J(10,4,.)
	matrix rd_dw_`t' = J(10,4,.)
	
	rdbwselect `var' antiguedad if main_treatment==`t', c(2.67) all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5)
	local h_r = `e(h_mserd)'
	local h_l = `e(h_mserd)'
	local b_r = `e(b_mserd)'
	local b_l = `e(b_mserd)' 
	
	
	local j = 1
	noi di " "
	noi _dots 0, title(Loop through cutoffs) reps(10)
	noi di " "
	foreach c of local c_vals_tenure {
		***********************		   		Tenure				************************
		* Pooled
		qui rdrobust `var' antiguedad if main_treatment==`t', c(`c') all kernel(triangular) p(1) q(2) vce(nncluster fecha_alta 5) h(`h_l' `h_r') b(`b_l' `b_r')
		matrix rd_tenure_`t'[`j',1] = e(tau_bc)
		matrix rd_tenure_`t'[`j',2] = e(tau_bc) - invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_tenure_`t'[`j',3] = e(tau_bc) + invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_tenure_`t'[`j',4] = `c'		
		
		noi _dots `j' 0	
		local j = `j' + 1
	}
	
	
		rdbwselect `var' salario_diario if main_treatment==`t', c(211) all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5)
	local h_r = `e(h_mserd)'
	local h_l = `e(h_mserd)'
	local b_r = `e(b_mserd)'
	local b_l = `e(b_mserd)' 
	
	
	local j = 1
	noi di " "
	noi _dots 0, title(Loop through cutoffs) reps(10)
	noi di " "
	foreach c of local c_vals_dw {
		***********************		   		  Wage				************************
		* Pooled
		qui rdrobust `var' salario_diario if main_treatment==`t', c(`c') all kernel(triangular) p(1) q(2) vce(nncluster fecha_alta 5) h(`h_l' `h_r') b(`b_l' `b_r')
		matrix rd_dw_`t'[`j',1] = e(tau_bc)
		matrix rd_dw_`t'[`j',2] = e(tau_bc) - invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_dw_`t'[`j',3] = e(tau_bc) + invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_dw_`t'[`j',4] = `c'
		
		noi _dots `j' 0	
		local j = `j' + 1
	}

}
}


********************************************************************************

forvalues t = 2/3 {
	cap drop rd_beta rd_lo rd_hi cutoff
	matrix colnames rd_tenure_`t' = rd_beta rd_lo rd_hi cutoff
	svmat rd_tenure_`t', names(col)


	twoway (rcap rd_hi rd_lo cutoff, color(gs7) yline(0, lcolor(black))) ///
			(scatter rd_beta cutoff, msymbol(O) color(navy%70) xline(2.67, lcolor(maroon%50))) ///
			(scatter rd_beta cutoff if rd_hi<0, msymbol(O) color(maroon%30)) ///
			(scatter rd_beta cutoff if rd_lo>0, msymbol(O) color(maroon%30)) ///
			(scatter rd_beta cutoff if inrange(cutoff,2.655,2.675), msymbol(S) color(maroon)) ///
			, legend(off) graphregion(color(white)) xtitle("Tenure Cutoff") ytitle("RD estimate")
	graph export "$directorio/Figuras/placebo_cut_tenure_`t'.pdf", replace
}


forvalues t = 2/3 {
	cap drop rd_beta rd_lo rd_hi cutoff
	matrix colnames rd_dw_`t' = rd_beta rd_lo rd_hi cutoff
	svmat rd_dw_`t', names(col)


	twoway (rcap rd_hi rd_lo cutoff, color(gs7) yline(0, lcolor(black))) ///
			(scatter rd_beta cutoff, msymbol(O) color(navy%70) xline(211, lcolor(maroon%50))) ///
			(scatter rd_beta cutoff if rd_hi<0, msymbol(O) color(maroon%30)) ///
			(scatter rd_beta cutoff if rd_lo>0, msymbol(O) color(maroon%30)) ///
			(scatter rd_beta cutoff if inrange(cutoff,210.5,211.5), msymbol(S) color(maroon)) ///
			, legend(off) graphregion(color(white)) xtitle("Daily wage Cutoff") ytitle("RD estimate")
	graph export "$directorio/Figuras/placebo_cut_dw_`t'.pdf", replace
}


