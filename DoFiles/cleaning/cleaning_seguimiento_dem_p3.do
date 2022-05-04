/*
********************
version 17.0
********************
 
/*******************************************************************************
* Name of file:	
* Author:	Isaac M 
* Machine:	Isaac M 											
* Date of creation:	November. 11, 2021  
* Last date of modification: 
* Modifications: 	
* Files used:     
		- 
* Files created:  

* Purpose: Cleaning 'seguimientos de demanda' coded by Marco & Emiliano following the lawsuits of the casefiles graded by Laura.

*******************************************************************************/
*/


import excel "$directorio\Raw\seguimientos\seguimiento_dem.xlsx", sheet("Hoja 1") ///
	cellrange(B4:U586) firstrow clear
	
rename (Junta Expediente Año MododetérminoOFIREC FechadetérminoFechadeúlti Cantidadotorgadaenlaudoaco CantidadpagadaINEGI MododetérminoEXPEDIENTE N CANTIDADOTORGADAENCONVENIOO DummylaudoconveniopagadoCOM Cantidadpagada) ///
	(junta exp anio modo_termino_ofirec fecha_termino_ofirec_ cantidad_ofirec cantidad_inegi modo_termino_exp fecha_termino_exp_ cantidad_otorgada convenio_pagado_completo cantidad_pagada)
	
replace modo_termino_exp = "CONTINUA" if modo_termino_exp=="CONTNUA"	
replace modo_termino_ofirec = "xAG" if modo_termino_ofirec=="AG"
*End mode
foreach var of varlist modo_termino_ofirec modo_termino_exp {
	replace `var' = stritrim(trim(itrim(upper(`var'))))
}

*Quantity
foreach var of varlist cantidad_ofirec cantidad_inegi cantidad_otorgada cantidad_pagada {
	destring `var', replace ignore(",")
}

*Date
gen fecha_termino_ofirec = date(fecha_termino_ofirec_, "DMY")
format fecha_termino_ofirec %td
gen fecha_termino_exp = date(fecha_termino_exp_, "DMY")
format fecha_termino_exp %td

*Latest end mode
gen fecha_termino = max(fecha_termino_ofirec, fecha_termino_exp)
format fecha_termino %td

gen modo_termino = ""
replace modo_termino = modo_termino_ofirec if fecha_termino==fecha_termino_ofirec
replace modo_termino = modo_termino_exp if fecha_termino==fecha_termino_exp

*Label values
foreach var of varlist modo* {
	encode `var', gen(`var'_)
	drop `var'
	rename `var'_ `var'
}
	
*Dummy
replace convenio_pagado_completo = 0 if missing(convenio_pagado_completo)
replace convenio_pagado_completo = . if convenio_pagado_completo>1

keep junta exp anio modo_termino_ofirec fecha_termino_ofirec cantidad_ofirec cantidad_inegi modo_termino_exp fecha_termino_exp  cantidad_otorgada convenio_pagado_completo cantidad_pagada modo_termino fecha_termino
order junta exp anio modo_termino_ofirec fecha_termino_ofirec cantidad_ofirec cantidad_inegi modo_termino_exp fecha_termino_exp  cantidad_otorgada convenio_pagado_completo cantidad_pagada modo_termino fecha_termino
					
save "$directorio\DB\seguimiento_dem_p3.dta", replace	
