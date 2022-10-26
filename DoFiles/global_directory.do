
********************
version 17.0
********************
/* 
/*******************************************************************************
* Name of file:	
* Author:	Isaac M 
* Machine:	Isaac M 											
* Date of creation:	May. 04, 2022  
* Last date of modification: 
* Modifications: 	
* Files used:     
		- 
* Files created:  

* Purpose : Defines the path of the project folder & some global variables

*******************************************************************************/
*/


clear
set more off
local user "Marco"

if "`user'" == "Marco" {
	global directory "/Users/marcomedina/ITAM Seira Research Dropbox/Marco Alejandro Medina/rd_information_bargaining"
	cd "$directory"
}

*Set significance
global star "star(* 0.1 ** 0.05 *** 0.01)"

*Set covariates
global controls = "mujer antiguedad salario_diario"

*Set scheme
set scheme white_tableau, perm

	
	
	
	
	
	