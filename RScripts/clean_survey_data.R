
###################################################

# Two weeks

###################################################


base_2s = read_excel('../Raw/base_control_encuestas.xlsx', sheet = 'BASE_ENCUESTAS_2S', skip = 2) %>%
  setNames(., nm = normalize_names(names(.))) %>%
  filter(status_encuesta == 1) %>%
  select(-one_of(private_vars), -nombre_actor) %>%
  mutate_at(vars(-starts_with('fecha'), -id_actor, -especifique), as.numeric) %>%
  mutate(origen = 'excel',
          prob_num_survey = !is.na(as.numeric(probabilidad_de_ganar)),
          cantidad_num_survey = !is.na(as.numeric(monto_que_espera_recibir)))


###################################################

# Mutate all data 

###################################################


surveys_2w = base_2s %>%
  left_join(treatment_data_selected) %>%
  mutate_at(vars(starts_with('fecha')), limpia_fechas) %>%
  mutate(sabemos_cantidad = !is.na(monto_del_convenio),
         sabemos_fecha_arreglo = !is.na(fecha_del_arreglo),
         probabilidad_de_ganar = if_else(probabilidad_de_ganar > 1, 
                                         probabilidad_de_ganar/100, 
                                         probabilidad_de_ganar),
         dias_sal = monto_que_espera_recibir/salario_diario,
         mas_6m_aux = corte_exp(dias_sal, 180, mas_o_menos_de_6_meses_de_sueldo),
         mas_75_aux = corte_exp(probabilidad_de_ganar, .75, mas_o_menos_de_75),
         prob_coarse_survey = !is.na(mas_75_aux),
         cantidad_coarse_survey = !is.na(mas_6m_aux),
         # Fix a variables de updating para los NA: le inputamos la media de la dummy
         prob_ganar_fixed_survey = inputar_media_cutoff(probabilidad_de_ganar, mas_75_aux),
         cantidad_ganar_fixed_survey = inputar_media_cutoff(monto_que_espera_recibir, mas_6m_aux, 180*salario_diario, 90*salario_diario),
         prob_ganar_fixed = inputar_media_cutoff(prob_ganar, prob_mayor),
         cantidad_ganar_fixed = inputar_media_cutoff(cantidad_ganar, cant_mayor, 180*salario_diario, 90*salario_diario),
         update_prob_survey = (probabilidad_de_ganar - prob_ganar)/prob_ganar,
         update_comp_survey = (monto_que_espera_recibir - cantidad_ganar)/cantidad_ganar,
         update_prob_fixed_survey = (prob_ganar_fixed_survey - prob_ganar_fixed)/prob_ganar_fixed,
         update_comp_fixed_survey = (cantidad_ganar_fixed_survey - cantidad_ganar_fixed)/cantidad_ganar_fixed,
         switched_prob_survey = prob_mayor > mas_75_aux,
         switched_comp_survey = cant_mayor > mas_6m_aux,
         tiempo_arreglo = fecha_del_arreglo - fecha_alta,
         tiempo_encuesta = fecha_ultimo_intento_encuesta - fecha_alta, 
         # Creo variables de hablo_con, condicionales.
         cond_hablo_con_privado = ha_hablado_con_abogado_privado,
         cond_hablo_con_publico = ha_hablado_con_abogado_publico,
         hablo_con_abogado = ha_hablado_con_abogado_privado == 1 | ha_hablado_con_abogado_publico == 1) %>%
  select(-prob_ganar, -cantidad_ganar, -prob_mayor, -cant_mayor) %>%
  rename(prob_ganar_survey = probabilidad_de_ganar,
         cantidad_ganar_survey = monto_que_espera_recibir,
         prob_mayor_survey = mas_75_aux,
         cant_mayor_survey = mas_6m_aux) %>%
  select(-mas_o_menos_de_6_meses_de_sueldo, -mas_o_menos_de_75, -dias_sal, -fecha_alta, -salario_diario) %>%
  mutate_at(vars(-one_of(vars_2s_nofix), -starts_with('fecha')), arregla_na) %>%
  # Knowledge de expectativas sÃ³lo para los que podrÃ?an tenerlas
  mutate_at(vars(ends_with('_survey')), function(x) ifelse(.[['conflicto_arreglado']] == 1, NA, x)) %>%
  mutate_at(vars(contains('update')), function(x) ifelse(abs(x) > 1, NA, x)) %>%
  # Talked to public sÃ³lo si talked to some lawyer
  mutate_at(vars(starts_with('cond_hablo_con')), function(x) ifelse(.[['ha_hablado_con_abogado_privado']] == 0 & .[['ha_hablado_con_abogado_publico']] == 0, NA, x)) %>%
  mutate_if(is.logical, as.numeric) %>%
  mutate_if(is.difftime, as.numeric) %>%
  filter(!is.na(id_actor))



###################################################

# Two months

###################################################


#######################################

# Excel data

#######################################



base_2m = read_excel('../Raw/base_control_encuestas.xlsx', sheet = 'BASE_ENCUESTAS_2.5M', skip = 2) %>%
  setNames(., nm = normalize_names(names(.))) %>%
  filter(status_encuesta == 1) %>%
  select(-one_of(private_vars), -nombre_actor) %>%
  mutate_at(vars(-starts_with('fecha'), -id_actor), as.numeric) %>%
  rename(entablo_demanda = demando_a_su_expatron) %>%
  mutate_at(vars(starts_with('fecha')), limpia_fechas) %>%
  mutate(origen = 'excel',
         prob_num_survey = !is.na(as.numeric(probabilidad_de_ganar)),
         cantidad_num_survey = !is.na(as.numeric(monto_que_espera_recibir)),
         tiempo_arreglar_asunto = cut(visitas_a_la_junta_y_procuraduria*promedio_de_horas_por_visita, 
                                      breaks = c(0, 2, 5, 10, 15, 20, 29.9, Inf), labels = F)) %>%
  mutate_if(is.logical, as.numeric)


#######################################

# Gforms data

#######################################


############### PELIGRO: era demasiado problema estar haciendo rename de estos nombres, 
# entonces le estoy pegando un vector de nombres bonitos, POR POSICIÃ“N
# Esto tiene que arreglarse ASAP

gforms = read_csv('../Raw/encuestas_gforms.csv') %>%
  filter(as.numeric(row.names(.)) > 9,
         `Estatus de la encuesta` == 'Exitosa') %>%
  setNames(names_gftodf) %>%
  select(-starts_with('X')) %>% 
  # Junto variables que estÃ¡n en varias columnas
  join_vars('entablo_demanda') %>%
  join_vars('demando_con_abogado_privado') %>%
  join_vars('que_elemento_es_el_mas_importante_al_elaborar_expectativas') %>%
  join_vars('minimo_que_aceptaria_para_solucionar_conflicto', numeric = T) %>%
  join_vars('especifique_cobro') %>%
  # Modifico y hago recodes para que se parezcan a las variables de excel
  mutate_at(vars(prob_num_survey, cantidad_num_survey), function(x) !grepl('^No sabe$', x)) %>%
  mutate_if(is.character, function(x) gsub('No sabe|No quiere', '', x, ignore.case = T)) %>%
  mutate_at(vars(starts_with('fecha')), dmy) %>%
  # Recodes varios para igualar con Excel
  mutate(status_encuesta = 1,
         demando_con_abogado_privado = if_else(demando_con_abogado_privado == 'Privado', 1, 0),
         esquema_de_cobro_pago_para_iniciar_juicio = dummy_from_mixedcol(cuota_por_iniciar_juicio),
         esquema_de_cobro_porcentaje = dummy_from_mixedcol(porcentaje),
         esquema_de_cobro_otro = !(especifique_cobro == 'No' | especifique_cobro == ''),
         demando_con_abogado_publico = entablo_demanda == 'Sí' & !demando_con_abogado_privado,
         ultimo_mes_alguien_del_hogar_a_comprado_casa_o_terreno_o_vehiculo = grepl_mod('terreno', ultimo_mes_base),
         ultimo_mes_alguien_del_hogar_a_comprado_electrodomestico_o_dispositivo_electronico = grepl_mod('electrónico', ultimo_mes_base),
         donde_lo_contacto = as.numeric(recode(donde_lo_contacto, 'En la Junta' = '1', 'En la procuraduria' = '2')),
         comparacion_con_el_trabajo_anterior = as.numeric(recode(comparacion_con_el_trabajo_anterior, 'MEJOR' = '1',
                                                                 'PEOR' = '2', 'IGUAL' = '3')),
         tiempo_arreglar_asunto = as.numeric(recode(tiempo_arreglar_asunto,
                                                    '0-2 horas' = '1',
                                                    '2.01 - 5 horas' = '2',
                                                    '5.01 - 10 horas' = '3',
                                                    '10.01 - 15 horas' = '4',
                                                    '15.01 - 20 horas' = '5',
                                                    '20.01 - 30 horas' = '6',
                                                    'Más de 30 horas' = '7')),
         origen = 'gforms') %>%
  mutate_at(vars(starts_with('mas_o_menos')), grepl_mod, expr = 'Más') %>%
  mutate_at(vars(enojo_con_la_empresa, como_lo_consiguio, que_elemento_es_el_mas_importante_al_elaborar_expectativas), aux_recode) %>%
  mutate_at(vars(cuota_por_iniciar_juicio), function(x) as.numeric(gsub('[^0-9]', '', x))) %>%
  # Dummy vars
  mutate_at(vars(conflicto_arreglado:entablo_demanda,
                 se_registro_el_acuerdo_ante_la_junta,
                 tramito_citatorio,
                 pidio_ser_reinstalado,
                 quiere_cambiar_abogado,
                 starts_with('ultimos_3_meses'),
                 trabaja_actualmente,
                 busca_trabajo,
                 asistio_patron_a_la_cita), grepl_mod, expr = 'Sí') %>%
  mutate_if(check_to_numeric, as.numeric) %>%
  mutate_if(is.logical, as.numeric)


# Filter out duplicate cases in Excel

filter_out = select(gforms, id_actor)

base_2m = base_2m %>%
          anti_join(filter_out)

rm(filter_out)

#######################################

# OPM data

#######################################

asignaciones = read_excel('../Raw/base_control_encuestas.xlsx', 
                          sheet = 'TRANSFERENCIA_EMPRESA_ENCUESTAS', 
                          skip = 2) %>%
                setNames(nm = normalize_names(names(.))) %>%
                select(id_actor = folio_del_trabajador,
                       id_opm = folio_del_trabajador_encuesta_empresa)

opm = read.dta13('../Raw/encuesta_empresa.dta') %>%
  mutate_at(vars(q_2, q2a, q2a_i, q2a_i_I, q2a_iv, q_3, q_3b, q_3b_ii,
                 q_3b_ii_I, q_3b_vi, q3c, q3f, q3h_i, q3i_i,
                 q4, q5, q7, q7b), aux_dummy) %>%
  mutate(q_1 = match(q_1, 2:11),
         q_3b_i = match(q_3b_i, 1:2),
         q_3b_iii = match(q_3b_iii, 1:8),
         q3e = match(q3e, 2:11),
         q3g = match(q3g, 1:4),
         q7a = match(q7a, 1:3)) %>%
  # Implementamos los comentarios de OPM con los valores de No, No sabe y No quiere
  mutate_at(vars(q_3b_iv, q_3b_v), function(x) ifelse(x %in% 1:3, NA, x)) %>%
  mutate_at(vars(q3h), function(x) ifelse(x %in% c(1, 9999), NA, x)) %>%
  mutate_at(vars(q3i, q3k, q7b_i), function(x) ifelse(x == 1, NA, x)) %>%
  # Recode de los nombres de OPM
  rename_opm() %>%
  join_vars('entablo_demanda', numeric = T) %>%
  join_vars('demando_con_abogado_publico', numeric = T) %>%
  join_vars('ultimo_mes') %>%
  mutate(promedio_de_horas_en_traslado = tiempo_trayecto_h + tiempo_trayecto_m/60,
         demando_con_abogado_privado = entablo_demanda == 1 & !demando_con_abogado_publico,
         esquema_de_cobro_porcentaje = grepl('[0-9]+', porcentaje),
         ultimo_mes_alguien_del_hogar_a_comprado_casa_o_terreno_o_vehiculo = grepl_mod('1', ultimo_mes),
         ultimo_mes_alguien_del_hogar_a_comprado_electrodomestico_o_dispositivo_electronico = grepl_mod('3', ultimo_mes),
         probabilidad_de_ganar = ifelse(probabilidad_de_ganar %in% c(1, 2, 9999) | probabilidad_de_ganar == 0, NA, probabilidad_de_ganar),
         monto_que_espera_recibir = ifelse(monto_que_espera_recibir == 1, NA, monto_que_espera_recibir),
         cantidad_num_survey = !is.na(monto_que_espera_recibir),
         prob_num_survey = !is.na(probabilidad_de_ganar),
         origen = 'opm') %>%
  mutate_if(is.logical, as.numeric) %>%
  left_join(asignaciones) %>%
  select(-id_opm)

###################################################

# Join all 2 months data

###################################################


# Tomar los casos terminados antes de los 2m y meterlos a esas encuestas

joint_2m = bind_rows(base_2m, gforms, opm) %>%
            # Fix temporal porque cometieron un error en un id
            filter(id_actor != '196_1')

vars_shared = c('conflicto_arreglado', 'reinstalacion', 'monto_del_convenio', 'fecha_del_arreglo', 'tiempo_arreglo')

ended_2w_vars = surveys_2w %>%
  filter(conflicto_arreglado == 1) %>%
  select(id_actor, one_of(vars_shared)) %>%
  inner_join(select(joint_2m, -one_of(vars_shared)))

ended_2w_ids = select(ended_2w_vars, id_actor)

surveys_2m = joint_2m %>%
            anti_join(ended_2w_ids) %>%
            bind_rows(ended_2w_vars) %>%
            left_join(treatment_data_selected) %>%
            mutate(sabemos_cantidad = !is.na(monto_del_convenio),
                   sabemos_fecha_arreglo = !is.na(fecha_del_arreglo),
                   probabilidad_de_ganar = if_else(probabilidad_de_ganar > 1, 
                                                   probabilidad_de_ganar/100, 
                                                   probabilidad_de_ganar),
                   dias_sal = monto_que_espera_recibir/salario_diario,
                   mas_6m_aux = corte_exp(dias_sal, 180, mas_o_menos_de_6_meses_de_sueldo),
                   mas_75_aux = corte_exp(probabilidad_de_ganar, .75, mas_o_menos_de_75),
                   prob_coarse_survey = !is.na(mas_75_aux),
                   cantidad_coarse_survey = !is.na(mas_6m_aux),
                   # Fix a variables de updating para los NA: le inputamos la media de la dummy
                   prob_ganar_fixed_survey = inputar_media_cutoff(probabilidad_de_ganar, mas_75_aux),
                   cantidad_ganar_fixed_survey = inputar_media_cutoff(monto_que_espera_recibir, mas_6m_aux, 180*salario_diario, 90*salario_diario),
                   prob_ganar_fixed = inputar_media_cutoff(prob_ganar, prob_mayor),
                   cantidad_ganar_fixed = inputar_media_cutoff(cantidad_ganar, cant_mayor, 180*salario_diario, 90*salario_diario),
                   update_prob_survey = (probabilidad_de_ganar - prob_ganar)/prob_ganar,
                   update_comp_survey = (monto_que_espera_recibir - cantidad_ganar)/cantidad_ganar,
                   update_prob_fixed_survey = (prob_ganar_fixed_survey - prob_ganar_fixed)/prob_ganar_fixed,
                   update_comp_fixed_survey = (cantidad_ganar_fixed_survey - cantidad_ganar_fixed)/cantidad_ganar_fixed,
                   switched_prob_survey = prob_mayor > mas_75_aux,
                   switched_comp_survey = cant_mayor > mas_6m_aux,
                   timestamp = dmy_hms(timestamp)) %>%
            mutate_at(vars(fecha_arreglo, fecha_alta), as.Date) %>%
            mutate(tiempo_arreglo = fecha_arreglo - fecha_alta,
                   tiempo_encuesta = fecha_ultimo_intento_encuesta - fecha_alta) %>%
            select(-prob_ganar, -cantidad_ganar, -prob_mayor, -cant_mayor) %>%
            rename(prob_ganar_survey = probabilidad_de_ganar,
                   cantidad_ganar_survey = monto_que_espera_recibir,
                   prob_mayor_survey = mas_75_aux,
                   cant_mayor_survey = mas_6m_aux) %>%
            mutate_at(vars(-one_of(vars_2m_nofix), -starts_with('fecha'), -starts_with('tiempo')), arregla_na) %>%
            # Knowledge de expectativas sÃ³lo para los que podrÃ?an tenerlas
            mutate_at(vars(ends_with('_survey')), function(x) ifelse(.[['conflicto_arreglado']] == 1, NA, x)) %>%
            mutate_if(is.logical, as.numeric) %>%
            mutate_if(is.difftime, as.numeric) %>%
            filter(!is.na(id_actor)) %>%
            setNames(gsub('ultimos_3_meses_', '', names(.))) %>%
            setNames(gsub('ultimo_mes_alguien_del_hogar_a_', '', names(.))) %>%
            select(-starts_with('mas_o_menos')) %>%
            filter(id_actor != '145',
                   # Quitamos a los del temblor
                   fecha_alta != '2017-09-19') %>%
            select(-fecha_alta, -salario_diario)

# Agrego solved_2m para la encuesta de dos semanas, para IVregs
surveys_2w = surveys_2w %>%
            left_join(select(surveys_2m, id_actor, conflicto_arreglado_2m = conflicto_arreglado))
  
# Arreglo para que treatment_data tenga todas las variables demogrÃ¡ficas
treatment_data = left_join(treatment_data, select(surveys_2w, id_actor, grado_de_estudios)) %>%
                  mutate(mas_secundaria = if_else(is.na(nivel_educativo), grado_de_estudios > 2,
                                                  nivel_educativo > 2)) 

surveys_2w = surveys_2w %>% select(-grado_de_estudios)

# Save data

# Resave treatment_data
write.csv(treatment_data, '../DB/treatment_data.csv', na = '', row.names = F)

treatment_data %>%
  mutate(genero = as.numeric(recode(genero, 'Mujer' = '1', 'Hombre' = '0'))) %>%
  saveRDS('../DB/treatment_data.RDS')

# Survey data

write.csv(surveys_2w, '../DB/survey_data_2w.csv', na = '', row.names = F)
write.csv(surveys_2m, '../DB/survey_data_2m.csv', na = '', row.names = F)

saveRDS(surveys_2w, '../DB/survey_data_2w.RDS')
saveRDS(surveys_2m, '../DB/survey_data_2m.RDS')
