/*
Cleaning Administrative Data. The source for this dataset is the 'modulo' at the entrance of the MCLC
Author : Isaac Meza
*/


********************************************************************************
*									  ARM 2							  		   *
********************************************************************************
import excel "$directorio\Raw\base_control_encuestas.xlsx", sheet("ALEATORIZACIONES") ///
	firstrow clear
rename tratamiento tratamiento_grupo
rename fechas date
save "$directorio\_aux\randomization_arm2.dta", replace



********************************************************************************
*						   	Names of 'demandados'					  		   *
********************************************************************************
import excel "$directorio\Raw\base_control_encuestas.xlsx", sheet("T_DEMANDADOS") ///
	firstrow clear
*Drop example row	
drop if _n==1	
drop if missing(id_main)
keep id_main nombre_demandado

*Reshape to wide
bysort id_main : gen j=_n
reshape wide nombre_demandado, i(id_main) j(j)

save "$directorio\_aux\demandados_tr.dta", replace
	  


********************************************************************************
*									  TD							  		   *
********************************************************************************
*Join nivel_educativo
insheet using "$directorio\Raw\gf2w.csv", clear
keep foliotrabajador Hastaqu
rename foliotrabajador id_actor
duplicates drop 

gen grado_de_estudios = .
replace grado_de_estudios = 1 if strpos(Hastaqu, "Primaria")!=0 
replace grado_de_estudios = 2 if strpos(Hastaqu, "Secundaria")!=0 
replace grado_de_estudios = 3 if strpos(Hastaqu, "Preparatoria")!=0 
replace grado_de_estudios = 4 if strpos(Hastaqu, "Licenciatura")!=0

bysort id_actor : egen grado_estudios = mean(grado_de_estudios)

keep id_actor grado_estudios
duplicates drop 

tempfile temp
save `temp'


import delimited "$directorio\_aux\treatment_data.csv", clear
duplicates drop
drop if missing(fecha_alta)
drop if id_main<16 & !missing(id_main)


gen date=date(fecha_alta, "YMD")
drop if date<date("01-01-2017","DMY")
format date %td
label variable date fecha_alta

foreach var of varlist fecha_alta fecha_salida fecha_entrada c_fecha_entrada cita_fecha {
	gen `var'_=date(`var', "YMD")
	drop `var'
	rename `var'_ `var'
	format `var' %td
	}				  
			

*Join nivel_educativo
merge 1:1 id_actor using `temp', nogen keep(1 3) keepusing(id_actor grado_estudios)
replace nivel_educativo = grado_estudios if missing(nivel_educativo)
drop grado_estudios

****************************************
*             Covariates
****************************************

*Tenure	
replace antiguedad = (fecha_salida - fecha_entrada)/365 if missing(antiguedad)
replace antiguedad = . if antiguedad<0 | antiguedad>60
		 
* Hack porque no hay antiguedad antes de esa fecha
replace antiguedad = . if fecha_alta <= date("2017-07-14","YMD")

*Imputation with mean for missing values
qui su antiguedad
replace antiguedad = `r(mean)' if missing(antiguedad)

*Daily wage
*Imputation with mean for missing values
qui su salario_diario
replace salario_diario = `r(mean)' if missing(salario_diario)

gen na_prob = 0
replace na_prob = 1 if missing(prob_ganar)

gen na_cant = 0
replace na_cant = 1 if missing(cantidad_ganar)

gen na_prob_mayor = 0
replace na_prob_mayor = 1 if missing(prob_mayor)

gen na_cant_mayor = 0
replace na_cant_mayor = 1 if missing(cant_mayor)

gen retail = (giro==46) if !missing(giro)

gen outsourcing = (giro==56) if !missing(giro)

gen mujer = (genero=="Mujer") if !missing(genero)

gen high_school = inlist(nivel_educativo, 3, 4) if !missing(nivel_educativo)

gen diurno = (tipo_jornada=="Diurno") if !missing(tipo_jornada)

gen top_sue = (top_demandado==1) if !missing(top_demandado)

gen big_size = inrange(tamanio_establecimiento,3,4) if !missing(tamanio_establecimiento)


*Predicting covariates of attrition
gen telefono_fijo_ = (length(telefono_fijo)>=5)
drop telefono_fijo
rename telefono_fijo_ telefono_fijo

gen telefono_cel_ = (length(telefono_cel)>=5)
drop telefono_cel
rename telefono_cel_ telefono_cel

gen email_ = !missing(email)
drop email
rename email_ email

gen colonia_ = !missing(colonia)
drop colonia
rename colonia_ colonia

destring codigo_postal, replace force
gen m_cp = missing(codigo_postal)

*Dummy Monday | Tuesday
gen dow = dow( date )
gen mon_tue = inrange(dow,1,2)



****************************************
*         Treatment variables
****************************************

*Separate arm 2 into 2A & 2B
merge m:1 date using "$directorio\_aux\randomization_arm2.dta", nogen keep(1 3)
replace grupo_tratamiento = tratamiento_grupo if grupo_tratamiento=="2" & !missing(tratamiento_grupo)
replace grupo_tratamiento = "2A" if grupo_tratamiento=="2"

*Treatment variable with all arms
encode grupo_tratamiento, gen(treatment)

*Main treatment variable
gen tratamiento = substr(grupo_tratamiento,1,1)
label define main_tr  1 "Control" 2 "Calculator" 3 "Calculator + letter"
encode tratamiento, gen(main_treatment) 
label values main_treatment main_tr
*Keep sample when experiment was ran for the 3 treatments
replace main_treatment = . if fecha_alta>=date("20/08/2018","DMY")


*A/B treatment
gen grupo = substr(grupo_tratamiento,2,1)
label define ab_tr  1 "A" 2 "B" 
encode grupo, gen(ab_treatment) 
label values ab_treatment ab_tr


*Fix NEW A/B (replacing 'pre-earthquake')
replace ab_treatment = . if inrange(date,date("19-09-2017","DMY"),date("20-08-2018","DMY"))

*NEW A/B!!!
*replace group=. if date<date("20-08-2018","DMY")

*Drop redundant treatment variables
drop tratamiento_voluntario grupo_tratamiento tratamiento_grupo tratamiento grupo 

*Paste names of 'demandados'
merge m:1 id_main using "$directorio\_aux\demandados_tr.dta", nogen keep(1 3)
replace nombre_demandado1 = nombre_empresa if missing(nombre_demandado1)
drop nombre_empresa

*Baseline expectations
replace prob_ganar = prob_ganar/100 if prob_ganar>1
replace prob_ganar_treat = prob_ganar_treat/100 if prob_ganar_treat>1


*IMPORTANT!!!: Comment this line to get full dataset
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*keep if inrange(date,date("01-01-2015","DMY"),date("01-02-2018","DMY"))
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

save "$directorio\DB\treatment_data.dta", replace



********************************************************************************
*						     	Base informatica					  		   *
********************************************************************************
do "$directorio\DoFiles\cleaning\expedientes_cleaning.do"

*Name cleaning
do "$directorio\DoFiles\cleaning\clean_exact_match.do"
global threshold_similscore = 0.94
do "$directorio\DoFiles\cleaning\clean_fuzzy_match.do"

