/*
Cleaning Iniciales de Demanda. 
Author : Isaac Meza
*/


********************************************************************************
*******************************               **********************************
********************************************************************************

import excel $directorio\Raw\iniciales_dem\dulce.xlsx , firstrow  clear

drop auxiliar junta expediente anio

foreach var of varlist * {
	local clean_name = stritrim(trim(itrim(lower("`var'"))))
	local clean_name = subinstr("`clean_name'", "á", "a", .)
	local clean_name = subinstr("`clean_name'", "é", "e", .)
	local clean_name = subinstr("`clean_name'", "í", "i", .)
	local clean_name = subinstr("`clean_name'", "ó", "o", .)
	local clean_name = subinstr("`clean_name'", "ú", "u", .)
	local clean_name = subinstr("`clean_name'", "Á", "a", .)
	local clean_name = subinstr("`clean_name'", "É", "e", .)
	local clean_name = subinstr("`clean_name'", "Í", "i", .)
	local clean_name = subinstr("`clean_name'", "Ó", "o", .)
	local clean_name = subinstr("`clean_name'", "Ú", "u", .)
	local clean_name = subinstr("`clean_name'", "â", "a", .)
	local clean_name = subinstr("`clean_name'", "ê", "e", .)
	local clean_name = subinstr("`clean_name'", "î", "i", .)
	local clean_name = subinstr("`clean_name'", "ô", "o", .)
	local clean_name = subinstr("`clean_name'", "ù", "u", .)
	local clean_name = subinstr("`clean_name'", "Â", "a", .)
	local clean_name = subinstr("`clean_name'", "Ê", "e", .)
	local clean_name = subinstr("`clean_name'", "Î", "i", .)
	local clean_name = subinstr("`clean_name'", "Ô", "o", .)
	local clean_name = subinstr("`clean_name'", "Û", "u", .)
	cap rename `var' `clean_name'
}

*Manual cleaning
rename lugaurl liga_url
rename fechadetermino fecha_termino
rename mododetermino modo_termino 
rename cantidadcobrada1si0no2no cantidad_cobrada
rename coincideactor1si0no2noe coincide_actor
replace id_actor = stritrim(trim(itrim(upper(id_actor))))
destring coincide_actor, replace force

*Recover junta/exp/anio
split expediente, parse("_")
forvalues i = 1/3 {
	destring expediente`i', replace
}
drop expediente
rename (expediente1  expediente2  expediente3) (junta exp anio)

*Categorical variables
foreach var of varlist estatus modo_termino ubicacion tratamiento {
	replace `var' = stritrim(trim(itrim(upper(`var'))))
	encode `var', gen(`var'_)
	drop `var'
	rename `var'_ `var'
}

*Cleaning of dates
gen fecha_termino_ = date(fecha_termino, "DMY")
drop fecha_termino
rename fecha_termino_ fecha_termino
format fecha_termino %td

*Cleaning of amount
split cantidad, parse(";")
forvalues i = 1/2 {
	destring cantidad`i', replace force ignore("$" ",")
}
drop cantidad

duplicates drop
duplicates drop junta exp anio id_actor, force
tempfile temp_idactor
save `temp_idactor'


********************************************************************************
*******************************               **********************************
********************************************************************************

import excel $directorio\Raw\iniciales_dem\append_nuevas_calidad.xlsx, firstrow clear

*drop irrelevant variables
drop A capturista

rename (junta expediente año) (junta exp anio)

foreach var of varlist junta exp anio derecho_propio_c_clv giro_defendant_c_clv num_demandados_totales num_demandados_completos num_abogados_proemio num_abogados_proemio_completos incrementos_sal_c_clv contrato_c_clv lugar_trabajo_c_clv horario_inicio_jornada_c_clv horario_fin_jornada_c_clv especifican_dias_c_clv salario_int_clv periodo_sal_int_clv fecha_despido_c_clv lugar_despido_c_clv motivo_despido_c_clv cargo_despide_clv presentado_tiempo_forma_c_clv personalidad_c_clv firma_trabajador_c_clv {
	destring `var', replace force ignore("J" "_FISC")
}

*Manual cleaning
replace anio = 2018 if anio==18
replace anio = 2019 if anio==19
replace anio = 2017 if anio==2917

duplicates drop 
duplicates drop  junta exp anio, force
tempfile temp_exp
save `temp_exp'


********************************************************************************
*******************************               **********************************
********************************************************************************

import excel $directorio\Raw\iniciales_dem\append_nuevas_iniciales.xlsx , firstrow  clear

foreach var of varlist * {
	local varlabel : var label `var'
	local clean_name = stritrim(trim(itrim(lower("`varlabel'"))))
	local clean_name = subinstr("`clean_name'", " ","_",.)
	local clean_name = subinstr("`clean_name'", "/","_",.)
	local clean_name = subinstr("`clean_name'", "#","",.)
	local clean_name = subinstr("`clean_name'", "ñ","ni",.)
	local clean_name = subinstr("`clean_name'", "_de_","_",.)
	local clean_name = subinstr("`clean_name'", "_del_","_",.)
	local clean_name = subinstr("`clean_name'", "_la_","_",.)
	local clean_name = subinstr("`clean_name'", "_las_","_",.)	
	local clean_name = subinstr("`clean_name'", "_por_","_",.)	
	local clean_name = subinstr("`clean_name'", "_el_","_",.)	
	local clean_name = subinstr("`clean_name'", "_en_","_",.)	
	local clean_name = subinstr("`clean_name'", "_que_","_",.)		
	local clean_name = substr("`clean_name'",1,30)
	cap rename `var' `clean_name'
}

*drop irrelevant variables
drop id_actor 


*Cleaning of dates
foreach var of varlist fecha* {
	gen `var'_ = date(`var',"MDY")
	destring `var', replace force 
	* Fix to recover dates
	replace `var' = `var' + 21916
	tostring `var', replace 
	replace `var' = substr(`var',1,4) + "/" + substr(`var',5,2) + "/" + substr(`var',7,2)
	replace `var'_ = date(`var', "YMD") if missing(`var'_)
	format `var'_ %td
	drop `var'
	rename `var'_ `var'
}


*Destring numeric variables
foreach var of varlist anio exp junta prevencion numero_actores_totales genero anio_nacimiento codigo_postal* numero_demandados_totales tipo_demandado_* codemanda_sar_imss_info tipo_abogado reclutamiento giro_empresa trabajador_confianza causa sueldo_base periodicidad_sueldo_base sueldo_estadistico periodicidad_sueldo_estadistic tipo_jornada numero_horas_laboradas periodicidad_horas_laboradas reinstalacion indemnizacion_constitucional indemnizacion_monto salarios_caidos salarios_caidos_monto prima_antiguedad prima_antiguedad_monto vacaciones razon_dias_periodo_vacacional dias_totales_vacaciones vacaciones_monto prima_vacacional prima_vacacional_porcentaje prima_vacacional_monto aguinaldo razon_dias_anio_aguinaldo dias_totales_aguinaldo aguinaldo_monto horas_extra horas_extra_semana horas_extra_monto total_horas_extra_ indemnizacion_20_dias_anio_ser diasmonto prima_dominical prima_dominical_monto descanso_semanal descanso_semanal_monto descanso_obligatorio descanso_obligatorio_monto cuotas_sar_imss_info utilidades utilidades_monto salarios_devengados_numero_dia otras_prestaciones_monto nulidad trabajo_proyecto {
	destring `var', replace force ignore("J" "," "$")
}

*Categorical variables
foreach var of varlist accion_principal tipo_prevencion {
	encode `var', gen(`var'_)
	drop `var'
	rename `var'_ `var'
}


*Manual cleaning
replace anio = 2018 if inlist(anio, 2918,3018)
drop if missing(junta)
duplicates drop 

*Recover missing values in duplicated observations
foreach var of varlist prevencion numero_actores_totales genero anio_nacimiento codigo_postal* numero_demandados_totales tipo_demandado_* codemanda_sar_imss_info tipo_abogado reclutamiento giro_empresa trabajador_confianza causa sueldo_base periodicidad_sueldo_base sueldo_estadistico periodicidad_sueldo_estadistic tipo_jornada numero_horas_laboradas periodicidad_horas_laboradas reinstalacion indemnizacion_constitucional indemnizacion_monto salarios_caidos salarios_caidos_monto prima_antiguedad prima_antiguedad_monto vacaciones razon_dias_periodo_vacacional dias_totales_vacaciones vacaciones_monto prima_vacacional prima_vacacional_porcentaje prima_vacacional_monto aguinaldo razon_dias_anio_aguinaldo dias_totales_aguinaldo aguinaldo_monto horas_extra horas_extra_semana horas_extra_monto total_horas_extra_ indemnizacion_20_dias_anio_ser diasmonto prima_dominical prima_dominical_monto descanso_semanal descanso_semanal_monto descanso_obligatorio descanso_obligatorio_monto cuotas_sar_imss_info utilidades utilidades_monto salarios_devengados_numero_dia otras_prestaciones_monto nulidad trabajo_proyecto accion_principal tipo_prevencion fecha* {
	sort junta exp anio nombre_actor `var'
	by junta exp anio nombre_actor : replace `var' = `var'[_n-1] if missing(`var')
}

duplicates drop junta exp anio nombre_actor, force
order junta exp anio nombre_actor

*Merge with previous datasets
merge m:1 junta exp anio using `temp_exp', nogen keep(1 3)
merge m:m junta exp anio using `temp_idactor', nogen keep(1 3)

*Save dataset
save "$directorio\DB\iniciales_dem_p3.dta" , replace