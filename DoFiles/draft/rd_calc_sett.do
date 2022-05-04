use "$directorio\DB\survey_data_2m.dta", clear
merge 1:1 id_actor using "$directorio\DB\treatment_data.dta", keep(3)


replace monto = . if monto<0

gen corte_dw = salario_diario>211 if !missing(salario_diario)
gen corte_tenure = antiguedad>2.67 if !missing(antiguedad)

gen quadrant = 1 if corte_dw==1 & corte_tenure==1
replace quadrant = 2 if corte_dw==1 & corte_tenure==0
replace quadrant = 3 if corte_dw==0 & corte_tenure==0
replace quadrant = 4 if corte_dw==0 & corte_tenure==1

rdrobust conflicto_arreglado antiguedad if main_treatment!=1 & !missing(main_treatment)  , c(2.67) 

rdrobust conflicto_arreglado antiguedad if main_treatment==2 & !missing(main_treatment)  , c(2.67) 
rdrobust conflicto_arreglado antiguedad if main_treatment==3 & !missing(main_treatment)  , c(2.67) 


rdrobust monto_del_convenio antiguedad if main_treatment!=1 & !missing(main_treatment)  , c(2.67) 

rdrobust monto_del_convenio antiguedad if main_treatment==2 & !missing(main_treatment)  , c(2.67) 
rdrobust monto_del_convenio antiguedad if main_treatment==3 & !missing(main_treatment)  , c(2.67) 




rdrobust entablo_demanda antiguedad if main_treatment!=1 & !missing(main_treatment)  , c(2.67) 

rdrobust entablo_demanda antiguedad if main_treatment==2 & !missing(main_treatment)  , c(2.67) 
rdrobust entablo_demanda antiguedad if main_treatment==3 & !missing(main_treatment)  , c(2.67) 


rdrobust confirmo_cita antiguedad if main_treatment==3 & !missing(main_treatment)  , c(2.67) 






rdrobust conflicto_arreglado antiguedad if main_treatment!=1 & !missing(main_treatment)  , c(2.67) covs(salario_diario mujer)



rdrobust conflicto_arreglado antiguedad if main_treatment!=1 & !missing(main_treatment)  & corte_dw==1, c(2.67)

rdrobust conflicto_arreglado antiguedad if main_treatment!=1 & !missing(main_treatment)  & corte_dw==0, c(2.67)

gen c1 = 2.67 in 1
*replace c1 = 2.67 in 2
gen c2 = 211 in 1
*replace c2 = 400 in 2


gen t = (corte_dw==0 & corte_tenure==1) if !missing(corte_dw) & !missing(corte_tenure)

generate double aux1 = abs(2.67 - antiguedad)
generate double aux2 = abs(211 - salario_diario)
egen xnorm = rowmin(aux1 aux2)
replace xnorm = xnorm*(2*t-1)


rdms conflicto_arreglado antiguedad salario_diario t if main_treatment!=1 & !missing(main_treatment) & inlist(quadrant, 1,2) , cvar(c1 c2) xnorm(xnorm)

rdms conflicto_arreglado antiguedad salario_diario t if main_treatment!=1 & !missing(main_treatment) & inlist(quadrant, 1,2,3) , cvar(c1 c2) xnorm(xnorm)

rdms conflicto_arreglado antiguedad salario_diario t if main_treatment!=1 & !missing(main_treatment) & inlist(quadrant, 1,3) , cvar(c1 c2) xnorm(xnorm)

rdms conflicto_arreglado antiguedad salario_diario t if main_treatment!=1 & !missing(main_treatment) & inlist(quadrant, 1,4) , cvar(c1 c2) xnorm(xnorm)


rdrobust conflicto_arreglado salario_diario if main_treatment!=1 & !missing(main_treatment)  & corte_tenure==1, c(211) 


*Otro outcome es la expectation para los de la frontera y dentro de un cuadrante si los que estan alejados se comportan distinto.


rdms monto_del_convenio antiguedad salario_diario t if main_treatment!=1 & !missing(main_treatment) & inlist(quadrant, 2,4) , cvar(c1 c2) xnorm(xnorm)
rdms monto_del_convenio antiguedad salario_diario t if main_treatment==2 & !missing(main_treatment) & inlist(quadrant, 2,4) , cvar(c1 c2) xnorm(xnorm)
rdms monto_del_convenio antiguedad salario_diario t if main_treatment==3 & !missing(main_treatment) & inlist(quadrant, 2,4) , cvar(c1 c2) xnorm(xnorm)



rdms conflicto_arreglado antiguedad salario_diario t if main_treatment!=1 & !missing(main_treatment) & inlist(quadrant, 2,4) , cvar(c1 c2) xnorm(xnorm)
rdms conflicto_arreglado antiguedad salario_diario t if main_treatment==2 & !missing(main_treatment) & inlist(quadrant, 2,4) , cvar(c1 c2) xnorm(xnorm)
rdms conflicto_arreglado antiguedad salario_diario t if main_treatment==3 & !missing(main_treatment) & inlist(quadrant, 2,4) , cvar(c1 c2) xnorm(xnorm)


rdms entablo_demanda antiguedad salario_diario t if main_treatment!=1 & !missing(main_treatment) & inlist(quadrant, 2,4) , cvar(c1 c2) xnorm(xnorm)
rdms entablo_demanda antiguedad salario_diario t if main_treatment==2 & !missing(main_treatment) & inlist(quadrant, 2,4) , cvar(c1 c2) xnorm(xnorm)
rdms entablo_demanda antiguedad salario_diario t if main_treatment==3 & !missing(main_treatment) & inlist(quadrant, 2,4) , cvar(c1 c2) xnorm(xnorm)











rdrobust conflicto_arreglado antiguedad if main_treatment!=1  & !missing(main_treatment) & inrange(salario_diario,212,1000), c(2.67) 
rdplot conflicto_arreglado antiguedad if main_treatment!=1  & !missing(main_treatment) & inrange(salario_diario,212,1000), c(2.67) kernel(triangular) h(`e(h_l)' `e(h_r)') p(1)
rdrobust conflicto_arreglado antiguedad if main_treatment!=1  & !missing(main_treatment) & inrange(salario_diario,212,1000), c(2.67) 


rdplot conflicto_arreglado antiguedad if main_treatment!=1 & !missing(main_treatment)  & inrange(salario_diario,0,211), c(2.67)




rdrobust conflicto_arreglado antiguedad if main_treatment!=1 , c(2.67)


rdplot conflicto_arreglado antiguedad if main_treatment!=1  , c(2.67)
mat list  e(coef_l) 
mat list e(coef_r) 


rdplot conflicto_arreglado antiguedad if main_treatment!=1  & !missing(main_treatment) , c(2.67)

rdbwselect conflicto_arreglado antiguedad if main_treatment!=1  & !missing(main_treatment) , c(2.67)
rdrobust conflicto_arreglado antiguedad if main_treatment!=1 & !missing(main_treatment) , c(2.67)


rdrobust conflicto_arreglado antiguedad if main_treatment==1 & !missing(main_treatment) , c(2.67)
rdplot conflicto_arreglado antiguedad if main_treatment==1  & !missing(main_treatment) , c(2.67)


reg conflicto_arreglado  i.corte_tenure##c.antiguedad salario_diario mujer if main_treatment!=1 & inrange(antiguedad,0,4)  & inrange(salario_diario,212,1000), r



******************


rdrobust conflicto_arreglado salario_diario if main_treatment!=1  & !missing(main_treatment) , c(211)
rdrobust conflicto_arreglado salario_diario if main_treatment==1  & !missing(main_treatment) , c(211)



use "$directorio\DB\iniciales_dem.dta" , clear
merge m:1 junta exp anio using  "$directorio\DB\seguimiento_dem.dta", nogen
keep junta exp anio id_actor modo_termino modo_termino_ofirec modo_termino_exp fecha_termino_ofirec cantidad_ofirec cantidad_inegi cantidad_otorgada convenio_pagado_completo cantidad_pagada sueldo_estadistico periodicidad_sueldo_estadistic periodicidad_sueldo_estadistic numero_horas_laboradas
*Keep unique id_actor
duplicates drop
drop if missing(id_actor)
*CAUTION (review)
duplicates drop junta exp anio id_actor, force
duplicates tag id_actor, gen(tg)
drop if tg>0

merge m:1 id_actor using "$directorio\DB\survey_data_2m.dta", keep(2 3) nogen 
merge m:1 id_actor using "$directorio\DB\treatment_data.dta", keep(2 3) nogen





su cantidad_ofirec cantidad_inegi cantidad_otorgada convenio_pagado_completo cantidad_pagada 

rdrobust monto_del_convenio salario_diario if main_treatment!=1  & !missing(main_treatment) , c(211)
rdrobust monto_del_convenio salario_diario if main_treatment==1  & !missing(main_treatment) , c(211)


rdrobust monto_del_convenio antiguedad if main_treatment!=1 & !missing(main_treatment) , c(2.67)
rdrobust monto_del_convenio antiguedad if main_treatment==1 & !missing(main_treatment) , c(2.67)




use "$directorio\DB\survey_data_2w.dta", clear
merge 1:1 id_actor using "$directorio\DB\treatment_data.dta", keep(3)


reg conflicto_arreglado i.corte_dw##i.corte_tenure mujer, r

reg conflicto_arreglado i.corte_dw##i.corte_tenure mujer if main_treatment!=1 & inrange(salario_diario,100,300) & inrange(antiguedad,0,4), r


reg conflicto_arreglado i.corte_dw##i.corte_tenure##i.main_treatment mujer  if inrange(salario_diario,100,300) & inrange(antiguedad,0,4), r




use "$directorio\DB\survey_data_2m.dta", clear
merge 1:1 id_actor using "$directorio\DB\treatment_data.dta", keep(3)

gen corte_dw = salario_diario>211 if !missing(salario_diario)
gen corte_tenure = antiguedad>2.67 if !missing(antiguedad)

xtile perc_antiguedad = antiguedad, nq(5)

reg conflicto_arreglado i.perc_antiguedad  i.corte_dw##i.corte_tenure if main_treatment!=1 & !missing(main_treatment) , r


gen low_tenure = antiguedad<0.89 if antiguedad<=2.67

reg conflicto_arreglado i.low_tenure  i.corte_dw if main_treatment!=1 & !missing(main_treatment) , r



