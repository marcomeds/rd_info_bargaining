/*
********************
version 17.0
********************
 
/*******************************************************************************
* Name of file:	
* Author:	Isaac M
* Machine:	Isaac M 											
* Date of creation:	January 21, 2022
* Last date of modification:   
* Modifications:		
* Files used:     
* Files created:  

* Purpose: 

*******************************************************************************/
*/

********************************************************************************
use  "$directorio\_aux\iniciales_dem_p3.dta", clear
keep junta exp anio 
merge m:m junta exp anio using "$directorio\DB\seguimiento_aud_p3.dta", keepusing(junta exp anio) nogen

duplicates drop 
egen id_exp = group(junta exp anio)
tempfile tempid
save `tempid'

use  "$directorio\_aux\iniciales_dem_p3.dta", clear
merge m:1  junta exp anio using `tempid', nogen keep(1 3)
save  "$directorio\_aux\iniciales_dem_p3.dta", replace

use  "$directorio\DB\seguimiento_aud_p3.dta", clear
merge m:1  junta exp anio using `tempid', nogen keep(1 3)
save  "$directorio\_aux\seguimiento_aud_p3.dta", replace
********************************************************************************

use  "$directorio\_aux\iniciales_dem_p3.dta", clear
matchit  id_exp  nombre_ac  using "$directorio\_aux\seguimiento_aud_p3.dta" , idu(id_exp) txtu(nombre_ac)
save  "$directorio\_aux\matchit_p3.dta", replace

********************************************************************************

use "$directorio\_aux\matchit_p3.dta",clear
keep if id_exp==id_exp1
duplicates drop 

*Manual join
keep if similscore>0.73
drop if id_exp==804 & nombre_ac=="vanessa torija hernandez"  & similscore!=1
drop if id_exp==804 & nombre_ac=="susana torija hernandez"  & similscore!=1

joinby id_exp nombre_ac using "$directorio\_aux\iniciales_dem_p3.dta", unmatched(using)
duplicates drop
replace nombre_ac1 = nombre_ac if missing(nombre_ac1)
drop nombre_ac _merge
rename nombre_ac1 nombre_ac

save  "$directorio\DB\iniciales_dem_p3.dta", replace

/* To merge with seguimiento de audiencias use : */
/*

merge m:m id_exp nombre_ac using "$directorio\_aux\seguimiento_aud_p3.dta", nogen
merge m:1 junta exp anio using "$directorio\DB\seguimiento_dem_p3.dta", nogen
duplicates drop

*/

