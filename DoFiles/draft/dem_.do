
use "$directorio\DB\iniciales_dem_p3.dta" , clear
merge m:1 junta exp anio using  "$directorio\DB\seguimiento_dem_p3.dta", nogen

*Daily wage
gen salario_diario = sueldo_estadistico if periodicidad_sueldo_estadistic==0
replace salario_diario = sueldo_estadistico/30 if periodicidad_sueldo_estadistic==1
replace salario_diario = sueldo_estadistico/15 if periodicidad_sueldo_estadistic==2
replace salario_diario = sueldo_estadistico/7 if periodicidad_sueldo_estadistic==3

*Tenure
gen antiguedad = (fecha_salida - fecha_entrada)/365 

keep junta exp anio id_actor genero anio_nacimiento codigo_postal codigo_postal_1 codigo_postal_2 codigo_postal_3 codigo_postal_4 codigo_postal_5 codigo_postal_6 tipo_abogado reclutamiento giro_empresa trabajador_confianza salario_diario numero_horas_laboradas reinstalacion indemnizacion_constitucional salarios_caidos prima_antiguedad vacaciones prima_vacacional aguinaldo horas_extra indemnizacion_20_dias_anio_ser prima_dominical descanso_semanal descanso_obligatorio cuotas_sar_imss_info utilidades nulidad antiguedad accion_principal modo_termino

foreach var of varlist genero anio_nacimiento codigo_postal codigo_postal_1 codigo_postal_2 codigo_postal_3 codigo_postal_4 codigo_postal_5 codigo_postal_6 tipo_abogado reclutamiento giro_empresa trabajador_confianza salario_diario numero_horas_laboradas reinstalacion indemnizacion_constitucional salarios_caidos prima_antiguedad vacaciones prima_vacacional aguinaldo horas_extra indemnizacion_20_dias_anio_ser prima_dominical descanso_semanal descanso_obligatorio cuotas_sar_imss_info utilidades nulidad antiguedad accion_principal modo_termino {
	local vara = substr("`var'",1,28)
	rename `var' `vara'_law
}


*Keep unique id_actor
duplicates drop
drop if missing(id_actor)
*CAUTION (review)
duplicates drop junta exp anio id_actor, force
duplicates tag id_actor, gen(tg)
drop if tg>0

merge m:1 id_actor using "$directorio\DB\survey_data_2m.dta", keep(2 3) nogen 
merge m:1 id_actor using "$directorio\DB\treatment_data.dta", keep(2 3) nogen

*keep if main_treatment!=1  & !missing(main_treatment)

gen corte_dw = salario_diario>211 if !missing(salario_diario)
gen corte_tenure = antiguedad>2.67 if !missing(antiguedad)

gen quadrant = 1 if corte_dw==1 & corte_tenure==1
replace quadrant = 2 if corte_dw==1 & corte_tenure==0
replace quadrant = 3 if corte_dw==0 & corte_tenure==0
replace quadrant = 4 if corte_dw==0 & corte_tenure==1
*-------------------------------------------------------------------------------



gen antiguedad_c  = antiguedad-2.67
	
rdrobust conflicto_arreglado antiguedad if main_treatment==2 & inlist(quadrant,1,2), c(2.67) all kernel(uniform) p(1) q(2) bwselect(mserd) vce(hc1)

reg conflicto_arreglado i.corte_tenure##c.antiguedad_c if main_treatment==2 & inlist(quadrant,1,2) & inrange(antiguedad, 2.67-`e(h_l)', 2.67+`e(h_r)'), robust
	
	