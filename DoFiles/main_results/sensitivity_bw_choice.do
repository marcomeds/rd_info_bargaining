
********************
version 17.0
********************
/* 
/*******************************************************************************
* Name of file:	
* Author:	Isaac M
* Machine:	Isaac M 											
* Date of creation:	May. 19, 2022
* Last date of modification:   
* Modifications:		
* Files used:     
* Files created:  

* Purpose: Sensitivity to Bandwidth Choice
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

cap drop t index
gen t = (corte_dw==0 & corte_tenure==1) if !missing(corte_dw) & !missing(corte_tenure)
gen index = normp*(2*t-1)

*-------------------------------------------------------------------------------


foreach var of varlist conflicto_arreglado {
foreach t in 2 3 {
	matrix rd_tenure_`t' = J(7,4,.)
	matrix rd_dw_`t' = J(7,4,.)
	matrix rd_index_`t' = J(7,4,.)
	
	
	qui rdbwselect `var' antiguedad if main_treatment==`t', c(2.67) all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5)
	local h_r = `e(h_mserd)'
	local h_l = `e(h_mserd)'	
		
	local j = 1
	noi di " "
	noi _dots 0, title(Loop through radius) reps(7)
	noi di " "
	foreach c in 0.8 0.9 0.95 1 1.05 1.10 1.2 {
		***********************		   		Tenure				************************
		* Pooled	
		qui rdrobust `var' antiguedad if main_treatment==`t', c(2.67) all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5) h(`=`c'*`h_l'' `=`c'*`h_r'')
		matrix rd_tenure_`t'[`j',1] = e(tau_bc)
		matrix rd_tenure_`t'[`j',2] = e(tau_bc) - invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_tenure_`t'[`j',3] = e(tau_bc) + invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_tenure_`t'[`j',4] = `c'		
		
		noi _dots `j' 0	
		local j = `j' + 1
	}
	
	
	qui rdbwselect `var' salario_diario if main_treatment==`t', c(211) all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5)
	local h_r = `e(h_mserd)'
	local h_l = `e(h_mserd)'
	
	local j = 1
	noi di " "
	noi _dots 0, title(Loop through radius) reps(7)
	noi di " "
	foreach c in 0.8 0.9 0.95 1 1.05 1.10 1.2 {
		***********************		   		  Wage				************************
		* Pooled
		qui rdrobust `var' salario_diario if main_treatment==`t', c(211) all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5) h(`=`c'*`h_l'' `=`c'*`h_r'')
		matrix rd_dw_`t'[`j',1] = e(tau_bc)
		matrix rd_dw_`t'[`j',2] = e(tau_bc) - invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_dw_`t'[`j',3] = e(tau_bc) + invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_dw_`t'[`j',4] = `c'
		
		noi _dots `j' 0	
		local j = `j' + 1
	}
	
	
	qui rdbwselect `var' index if main_treatment==`t' & inlist(quadrant,2,4), c(0) all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5)
	local h_r = `e(h_mserd)'
	local h_l = `e(h_mserd)'
	
	local j = 1
	noi di " "
	noi _dots 0, title(Loop through radius) reps(7)
	noi di " "
	foreach c in 0.8 0.9 0.95 1 1.05 1.10 1.2 {
		***********************		   	Tenure & Wage			************************
		* II vs IV
		qui rdrobust `var' index if main_treatment==`t' & inlist(quadrant,2,4), c(0) all kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5) h(`=`c'*`h_l'' `=`c'*`h_r'')
		matrix rd_index_`t'[`j',1] = e(tau_bc)
		matrix rd_index_`t'[`j',2] = e(tau_bc) - invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_index_`t'[`j',3] = e(tau_bc) + invnormal(1-10/200)*e(se_tau_rb)
		matrix rd_index_`t'[`j',4] = `c'
		
		noi _dots `j' 0	
		local j = `j' + 1
	}

}
}


********************************************************************************

forvalues t = 2/3 {
	cap drop rd_beta rd_lo rd_hi bw
	matrix colnames rd_tenure_`t' = rd_beta rd_lo rd_hi bw
	svmat rd_tenure_`t', names(col)


	twoway (rcap rd_hi rd_lo bw, color(gs7) yline(0, lcolor(black))) ///
			(scatter rd_beta bw, msymbol(O) color(navy%70)) ///
			(scatter rd_beta bw if rd_hi<0, msymbol(O) color(maroon%30)) ///
			(scatter rd_beta bw if rd_lo>0, msymbol(O) color(maroon%30)) ///
			(scatter rd_beta bw if bw==1, msymbol(S) color(maroon)) ///
			, legend(off) graphregion(color(white)) xtitle("Bandwidth") ytitle("RD estimate")
	graph export "$directorio/Figuras/bwsensitivity_tenure_`t'.pdf", replace
}


forvalues t = 2/3 {
	cap drop rd_beta rd_lo rd_hi bw
	matrix colnames rd_dw_`t' = rd_beta rd_lo rd_hi bw
	svmat rd_dw_`t', names(col)


	twoway (rcap rd_hi rd_lo bw, color(gs7) yline(0, lcolor(black))) ///
			(scatter rd_beta bw, msymbol(O) color(navy%70)) ///
			(scatter rd_beta bw if rd_hi<0, msymbol(O) color(maroon%30)) ///
			(scatter rd_beta bw if rd_lo>0, msymbol(O) color(maroon%30)) ///
			(scatter rd_beta bw if bw==1, msymbol(S) color(maroon)) ///
			, legend(off) graphregion(color(white)) xtitle("Bandwidth") ytitle("RD estimate")
	graph export "$directorio/Figuras/bwsensitivity_dw_`t'.pdf", replace
}


forvalues t = 2/3 {
	cap drop rd_beta rd_lo rd_hi bw
	matrix colnames rd_index_`t' = rd_beta rd_lo rd_hi bw
	svmat rd_index_`t', names(col)


	twoway (rcap rd_hi rd_lo bw, color(gs7) yline(0, lcolor(black))) ///
			(scatter rd_beta bw, msymbol(O) color(navy%70)) ///
			(scatter rd_beta bw if rd_hi<0, msymbol(O) color(maroon%30)) ///
			(scatter rd_beta bw if rd_lo>0, msymbol(O) color(maroon%30)) ///
			(scatter rd_beta bw if bw==1, msymbol(S) color(maroon)) ///
			, legend(off) graphregion(color(white)) xtitle("Bandwidth") ytitle("RD estimate")
	graph export "$directorio/Figuras/bwsensitivity_index_`t'.pdf", replace
}
