
###################################################

# Treatment data

###################################################



## Leemos los datos nuevos

# Seleccionamos lo que conservaremos de la base de brazos 1

to_keep = c('id_actor',
            'nombre_trabajador',
            'nombre_empresa' ,
            'fecha_alta', 
            'tratamiento', 
            'fecha_despido', 
            'fecha_entrada',
            'giro', 
            'nivel_enojo',
            'prob_ganar', 
            'cantidad_ganar',
            'salario',
            'per_salario', 
            'codigo_postal',
            'colonia',
            'telefono_fijo',
            'telefono_celular',
            'genero',
            'tomo_Uber',
            'curp')


tratamientos_1 = read_excel('../Raw/captacion_tratamientos_1.xlsx', sheet = 'BaseControl',
                            col_types = c("numeric", 
                                          "text", "date", "text", "text", 
                                          "date", "numeric", "text", "numeric", 
                                          "text", "text", "text", "text", "text",  
                                          "text", "text", "text", "text", "date", 
                                          "date", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "numeric", "text", 
                                          "date", "text", "text", "text", 
                                          "text", "text", "text"),
                            skip = 3) %>%
  filter(dummy_efectivo == 1) %>%
  select(one_of(to_keep)) %>%
  mutate_at(vars(starts_with('fecha')), limpia_fechas) %>%
  mutate(tomo_Uber = ifelse(tomo_Uber != 0 & tomo_Uber != 1, NA, tomo_Uber))

tratamientos_23_actores = read_excel('../Raw/base_modulo_informacion.xlsm', sheet = 'T_ACTORES')
tratamientos_23_main = read_excel('../Raw/base_modulo_informacion.xlsm', sheet = 'T_MAIN')
tratamientos_23_dom = read_excel('../Raw/base_modulo_informacion.xlsm', sheet = 'T_DOMICILIOS')


tratamientos_23_actores = left_join(tratamientos_23_actores, tratamientos_23_dom)
tratamientos_23 = left_join(tratamientos_23_actores, tratamientos_23_main)

rm(tratamientos_23_actores, tratamientos_23_main)

## Modificamos los nombres de tratamientos 1 para poder hacer el join

diccionario = read_excel('../Raw/diccionario.xlsx') %>%
  select(-definicion) %>%
  filter(nombre_1 %in% to_keep) %>%
  na.omit()

setnames(tratamientos_1, old = diccionario$nombre_1, new = diccionario$nombre_2)

## Creamos salario diario y dummies de thresholds para cada dataset

t_1 = tratamientos_1 %>%
  filter(nombre_actor != 'NO QUISO SER TRATADO') %>%
  mutate(telefono_fijo = as.character(telefono_fijo),
         telefono_cel = as.character(telefono_cel),
         sueldo_per = recode(as.character(sueldo_per),
                             '1' = '1', '2' = '7', '3' = '15', '4' = '30', 'S' = '7', .default = ''),
         sueldo_per = as.numeric(sueldo_per),
         sueldo = as.numeric(gsub('[^0-9.]', '', sueldo)))


t_1$sueldo <-  as.numeric(t_1$sueldo)
t_1$sueldo_per <-  as.numeric(t_1$sueldo_per)
t_1$salario_diario <-  as.numeric(t_1$sueldo/t_1$sueldo_per)


t_1 = t_1 %>%
         mutate(prob_mayor = dummy_mayor(prob_ganar, .75, T),
         cant_mayor = dummy_mayor(cantidad_ganar, 180, F, salario = salario_diario),
         genero = recode(genero, `1` = 'Mujer', `0` = 'Hombre')) %>%
  mutate_at(vars(prob_ganar, cantidad_ganar, giro, nivel_enojo), as.numeric)





t_23 = tratamientos_23 %>%
  mutate_at(vars(contains('no_sabe')), function(x) as.numeric(recode(x, '1' = '1', '2' = '0', .default = ''))) %>%
  setNames(nm = gsub('(.*)(_ganar)(_act){0,1}_(no_sabe)', '\\1_mayor\\3', names(.))) %>%
  mutate_at(vars(prob_ganar, prob_ganar_act, cantidad_ganar, cantidad_ganar_act), function(x) ifelse(x == 0, NA, x)) %>%
  mutate_at(vars(prob_ganar, prob_ganar_act), function(x) if_else(x>1, x/100, x)) %>%
  mutate_at(vars(starts_with('fecha')), limpia_fechas) %>%
  mutate(id_actor = as.character(id_actor),
         telefono_fijo = as.character(telefono_fijo),
         telefono_cel = as.character(telefono_cel),
         sueldo_per = as.numeric(recode(sueldo_per,
                             'Diario' = '1', 'Semanal' = '7',
                             'Quincenal' = '15', 'Mensual' = '30')),
         salario_diario = sueldo/sueldo_per,
         grupo_tratamiento = as.character(grupo_tratamiento),
         prob_mayor = dummy_mayor(prob_ganar, .75, T,
                                  dummy_var = prob_mayor),
         cant_mayor = dummy_mayor(cantidad_ganar, 180, F, 
                                  salario = salario_diario, 
                                  dummy_var = cantidad_mayor),
         update_prob = (prob_ganar_act - prob_ganar)/prob_ganar,
         update_comp = (cantidad_ganar_act - cantidad_ganar)/cantidad_ganar,
         prob_mayor_treat = dummy_mayor(prob_ganar_act, .75, T, 
                                        dummy_var = prob_mayor_act),
         cant_mayor_treat = dummy_mayor(cantidad_ganar_act, 180, F,
                                        salario = salario_diario, 
                                        dummy_var = cantidad_mayor_act),
         switched_prob = prob_mayor > prob_mayor_treat,
         switched_comp = cant_mayor > cant_mayor_treat) %>%
  filter(tratamiento_voluntario == 0,
         fecha_alta >= '2017-05-15') %>% 
  dplyr::rename(prob_ganar_treat = prob_ganar_act,
         cantidad_ganar_treat = cantidad_ganar_act) %>%
  select(-ends_with('act')) %>%
  mutate(tomo_Uber = as.numeric(tomo_Uber))


# Juntamos y anonimizamos los datos

treatment_data = bind_rows(t_1, t_23) %>%
  filter(!is.na(nombre_actor),
         !is.na(id_actor)) %>% 
  mutate(prob_num = !is.na(prob_ganar),
         prob_coarse = !is.na(prob_mayor),
         cantidad_num = !is.na(cantidad_ganar),
         cantidad_coarse = !is.na(cant_mayor),
         top_demandado = !grepl('NINGUNO', top_demandado),
         genero = ifelse(genero == '', NA, genero),
         angry = as.numeric(recode(nivel_enojo, '1' = '1', '2' = '1', '3' = '0', '4' = '0'))) %>%
  select(-one_of(private_vars), -cantidad_mayor) %>%
  mutate_if(is.logical, as.numeric) %>%
  distinct()

# Quito acentos y cosas feas de los nombres
setnames(treatment_data, normalize_names(names(treatment_data), remove_punct = F))

# Guardamos los datos
write.csv(treatment_data, '../_aux/treatment_data.csv', na = '', row.names = F)

rm(tratamientos_1, tratamientos_23, t_1, t_23, diccionario)
