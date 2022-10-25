
********************
version 17.0
********************
/* 
/*******************************************************************************
* Name of file:	
* Author:	Isaac M
* Machine:	Isaac M 											
* Date of creation:	May. 05, 2022
* Last date of modification:   
* Modifications:		
* Files used:     
* Files created:  

* Purpose: Single running variable RD.
*******************************************************************************/
*/


use "$directorio\DB\survey_data_2m.dta", clear
merge 1:1 id_actor using "$directorio\DB\treatment_data.dta", keep(3)



*-------------------------------------------------------------------------------

******************************
* 			RD model		 *
******************************

*npregress kernel conflicto_arreglado antiguedad

*rdrobust conflicto_arreglado antiguedad if main_treatment!=1 & !missing(main_treatment)  , c(2.67) kernel(uniform)
gen W2 = (antiguedad>2.67) if !missing(antiguedad)
*
gen tenure_c = antiguedad-2.67

rdrobust conflicto_arreglado antiguedad if main_treatment!=1 & !missing(main_treatment) & inrange(salario_diario,211,261) , c(2.67) kernel(uniform) 
reg conflicto_arreglado i.W2##c.tenure_c if main_treatment!=1 & !missing(main_treatment) & inrange(antiguedad,2.67-`e(h_l)',2.67+`e(h_l)'), r


range X2_c -1 1 100
range X1_c -40 40 100 


tempvar W1 W2 X1c X2c quadrant
local X1 salario_diario
local X2 antiguedad

*RD boundary definition
gen `W1' = (`X1'>211) if !missing(`X1')
gen `W2' = (`X2'>2.67) if !missing(`X2')

*Centered variables
gen `X1c' = `X1'-211
gen `X2c' = `X2'-2.67

*Quadrants
gen `quadrant' = 1 if `W1'==1 & `W2'==1
replace `quadrant' = 2 if `W1'==1 & `W2'==0
replace `quadrant' = 3 if `W1'==0 & `W2'==0
replace `quadrant' = 4 if `W1'==0 & `W2'==1






reg conflicto_arreglado 1.`W1' 1.`W2' 1.`W1'#1.`W2' c.`X1c' c.`X2c' ///
	c.`X1c'#c.`X2c'  c.`X1c'#1.`W1' c.`X2c'#1.`W2' c.`X1c'#1.`W2' c.`X2c'#1.`W1' ///
	c.`X1c'#c.`X2c'#1.`W1' c.`X1c'#c.`X2c'#1.`W2' c.`X1c'#1.`W1'#1.`W2' c.`X2c'#1.`W1'#1.`W2' ///
	c.`X1c'#c.`X2c'#1.`W1'#1.`W2' if main_treatment!=1 & !missing(main_treatment) & inrange(antiguedad,2.67-0.9,2.67+0.9) & inrange(salario_diario,211-40,211+40) , vce(cluster fecha_alta)

	

*Effect of W1 = 1|W2 = 0, X1 = c1:
*β1 + β10*X2
gen eff_w1 = e(b)[1,1] + e(b)[1,10]*X2_c if X2_c<0


*Effect of W1 = 1|W2 = 1, X1 = c1:
*(β1 + β3) + (β10 + β14)X2
gen eff_w1_w2 = e(b)[1,1] + e(b)[1,3] + (e(b)[1,10]+e(b)[1,14])*X2_c if X2_c>0

*Effect of W2 = 1|W1 = 0, X2 = c2:
*β2 + β9*X1
gen eff_w2 = e(b)[1,2] + e(b)[1,9]*X1_c if X1_c<0


*Effect of W2 = 1|W1 = 1, X2 = c2:
*(β2 + β3) + (β9 + β13)X1 
gen eff_w2_w1 = e(b)[1,2] + e(b)[1,3] + (e(b)[1,9]+e(b)[1,13])*X1_c if X1_c>0


twoway (line eff_w1 X2_c) (line eff_w1_w2 X2_c) 

twoway (line eff_w2 X1_c) (line eff_w2_w1 X1_c) 