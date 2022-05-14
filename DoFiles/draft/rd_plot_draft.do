
*Bandwidth optimal selection.
rdbwselect conflicto_arreglado antiguedad if main_treatment==2 & inlist(quadrant,1,2), c(2.67) kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5) covs(salario_diario `controls') 
local h_r = `e(h_mserd)'
local h_l = `e(h_mserd)'


*Residual computation 
	*Above threshold
reg conflicto_arreglado salario_diario `controls' if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,2.67,`=2.67+3*`h_r'')
predict res_y_r if e(sample), residual
su conflicto_arreglado if e(sample) 
replace res_y_r = res_y_r + `r(mean)'

reg antiguedad salario_diario `controls' if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,2.67,`=2.67+3*`h_r'')
predict res_x_r if e(sample), residual 
su antiguedad if e(sample) 
replace res_x_r = res_x_r + `r(mean)'
	*Below threshold
reg conflicto_arreglado salario_diario `controls' if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,`=2.67-3*`h_l'',2.67)
predict res_y_l if e(sample), residual
su conflicto_arreglado if e(sample) 
replace res_y_l = res_y_l + `r(mean)'

reg antiguedad salario_diario `controls' if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,`=2.67-3*`h_l'',2.67)
predict res_x_l if e(sample), residual 
su antiguedad if e(sample) 
replace res_x_l = res_x_l + `r(mean)'


*Binning computation for binscatter
	*Above threshold
xtile xq_r = res_x_r, nq(20)
bysort xq_r : egen mn_x_r = mean(res_x_r)  if !missing(xq_r)
bysort xq_r : egen mn_y_r = mean(res_y_r) if !missing(xq_r)
	*Below threshold
xtile xq_l = res_x_l, nq(20)
bysort xq_l : egen mn_x_l = mean(res_x_l)  if !missing(xq_l)
bysort xq_l : egen mn_y_l = mean(res_y_l)  if !missing(xq_l)


*Bspline fitting
	*Above threshold
bspline if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,2.67,`=2.67+3*`h_r''), xvar(antiguedad) knots(2.67 `=2.67+`h_r'' `=2.67+2*`h_r'' `=2.67+3*`h_r'') p(1) gen(_bs_r)
		*Simple
reg conflicto_arreglado _bs_r* if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,2.67, `=2.67+3*`h_r''), noconstant vce(cluster fecha_alta)
cap drop bs_r
predict bs_r if e(sample)
predict bs_se_r if e(sample), stdp
		*CI
gen hi_bs_r = bs_r + invnormal(1-10/200)*bs_se_r
gen lo_bs_r = bs_r - invnormal(1-10/200)*bs_se_r
		*Covariates
reg conflicto_arreglado _bs_r* salario_diario `controls' if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,2.67, `=2.67+3*`h_r''), noconstant vce(cluster fecha_alta)
cap drop bs_c_r
gen bs_c_r = _bs_r1*e(b)[1,1] + _bs_r2*e(b)[1,2] + _bs_r3*e(b)[1,3] + _bs_r4*e(b)[1,4]  if e(sample)
local j = 5
foreach var of varlist salario_diario `controls' {
	su `var' if e(sample), meanonly
	local pr`j' = `r(mean)'*e(b)[1,`j']
	replace bs_c_r = bs_c_r + `pr`j''
	local j = `j' + 1
}

	
	*Below threshold
bspline if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,`=2.67-3*`h_l'',2.67), xvar(antiguedad) knots(`=2.67-3*`h_l'' `=2.67-2*`h_l'' `=2.67-`h_l'' 2.67) p(1) gen(_bs_l)
		*Simple
reg conflicto_arreglado _bs_l*  if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,`=2.67-3*`h_l'',2.67), noconstant vce(cluster fecha_alta)
cap drop bs_l
predict bs_l if e(sample)
predict bs_se_l if e(sample), stdp
		*CI
gen hi_bs_l = bs_l + invnormal(1-10/200)*bs_se_l
gen lo_bs_l = bs_l - invnormal(1-10/200)*bs_se_l
		*Covariates
reg conflicto_arreglado _bs_l* salario_diario `controls' if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,`=2.67-3*`h_l'',2.67), noconstant vce(cluster fecha_alta)
cap drop bs_c_l
gen bs_c_l = _bs_l1*e(b)[1,1] + _bs_l2*e(b)[1,2] + _bs_l3*e(b)[1,3] + _bs_l4*e(b)[1,4] if e(sample)
local j = 5
foreach var of varlist salario_diario `controls' {
	su `var' if e(sample), meanonly
	local pr`j' = `r(mean)'*e(b)[1,`j']
	replace bs_c_l = bs_c_l + `pr`j''
	local j = `j' + 1
}


local controls nivel_de_felicidad trabaja_actualmente  high_school reclutamiento dummy_confianza horas_sem  dummy_sarimssinfo  c_min_indem  c_min_total top_demandado  mujer

qui rdbwselect conflicto_arreglado antiguedad if main_treatment==2 & inlist(quadrant,1,2), c(2.67) kernel(triangular) p(1) q(2) bwselect(mserd) vce(nncluster fecha_alta 5) covs(salario_diario `controls') 
local h_r = `e(h_mserd)'
local h_l = `e(h_mserd)'

*-------------------------------------------------------------------------------
*	RD graph
twoway (scatter mn_y_l mn_x_l, msymbol(O) msize(small) color(navy%70) xline(2.67, lcolor(black))) ///
		(scatter mn_y_r mn_x_r , msymbol(O) msize(small) color(maroon%70)) ///
		(lpoly  mn_y_l mn_x_l if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,`=2.67-3*`h_l'',2.67) , kernel(triangle) deg(1) bw(`=`h_l'/2') lpattern(dot) lcolor(navy)) ///
		(lpoly  mn_y_r mn_x_r if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,2.67,`=2.67+3*`h_r'') , kernel(triangle) deg(1) bw(`=`h_r'/2') lpattern(dot) lcolor(maroon)) ///
		(lpolyci conflicto_arreglado antiguedad if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,`=2.67-3*`h_l'',2.67), kernel(triangle) deg(1) bw(`=`h_l'/2') lpattern(dash) lcolor(navy) level(90) ciplot(rarea) acolor(navy%10) fintensity(inten70)) ///
		(lpolyci conflicto_arreglado antiguedad if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,2.67,`=2.67+3*`h_r''), kernel(triangle) deg(1) bw(`=`h_r'/2') lpattern(dash) lcolor(maroon) level(90) ciplot(rarea) acolor(maroon%10) fintensity(inten70)) ///
		(rarea lo_bs_l hi_bs_l antiguedad if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,`=2.67-3*`h_l'',2.67), sort color(navy%15)) ///
		(rarea lo_bs_r hi_bs_r antiguedad if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,2.67,`=2.67+3*`h_r''), sort color(maroon%15)) ///		
		(line bs_l antiguedad if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,`=2.67-3*`h_l'',2.67), sort lpattern(dash_dot) lcolor(navy)) ///
		(line bs_r antiguedad if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,2.67,`=2.67+3*`h_r''), sort lpattern(dash_dot) lcolor(maroon)) ///		
		(line bs_c_l antiguedad if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,`=2.67-3*`h_l'',2.67), sort lpattern(solid) lcolor(navy)) ///
		(line bs_c_r antiguedad if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad,2.67,`=2.67+3*`h_r''), sort lpattern(solid) lcolor(maroon)) ///
		, graphregion(color(white)) xtitle("Tenure") ytitle("Settlement") legend(off) ylab(0 0.2 0.4 0.6 0.8 1.0 1.2 1.4)
		

	