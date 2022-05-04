
###################################################

# Two weeks

###################################################


base_2s = read_excel('../Raw/base_control_encuestas.xlsx', sheet = 'BASE_ENCUESTAS_2S', skip = 2) %>%
  setNames(., nm = normalize_names(names(.))) %>%
  filter(status_encuesta == 1) %>%
  select(-one_of(private_vars), -nombre_actor) %>%
  mutate_at(vars(starts_with('fecha')), limpia_fechas) %>%
  mutate_at(vars(-starts_with('fecha'), -id_actor, -especifique), as.numeric) %>%
  mutate(origen = 'excel',
         prob_num_survey = !is.na(as.numeric(probabilidad_de_ganar)),
         cantidad_num_survey = !is.na(as.numeric(monto_que_espera_recibir)))  %>%
  mutate_if(is.logical, as.numeric)





###################################################

# Two months

###################################################



base_2m = read_excel('../Raw/base_control_encuestas.xlsx', sheet = 'BASE_ENCUESTAS_2.5M', skip = 2) %>%
  setNames(., nm = normalize_names(names(.))) %>%
  filter(status_encuesta == 1) %>%
  select(-one_of(private_vars), -nombre_actor) %>%
  mutate_at(vars(-starts_with('fecha'), -id_actor), as.numeric) %>%
  dplyr::rename(entablo_demanda = demando_a_su_expatron) %>%
  mutate_at(vars(starts_with('fecha')), limpia_fechas) %>%
  dplyr::rename(comprado_casa_o_terreno = ultimo_mes_alguien_del_hogar_a_comprado_casa_o_terreno_o_vehiculo,
         comprado_electrodomestico = ultimo_mes_alguien_del_hogar_a_comprado_electrodomestico_o_dispositivo_electronico) %>%
  mutate(origen = 'excel',
         prob_num_survey = !is.na(as.numeric(probabilidad_de_ganar)),
         cantidad_num_survey = !is.na(as.numeric(monto_que_espera_recibir)),
         tiempo_arreglar_asunto = cut(visitas_a_la_junta_y_procuraduria*promedio_de_horas_por_visita, 
                                      breaks = c(0, 2, 5, 10, 15, 20, 29.9, Inf), labels = F)) %>%
  mutate_if(is.logical, as.numeric)



# Save data

write.csv(base_2s, '../_aux/survey_data_2w.csv', na = '', row.names = F)
write.csv(base_2m, '../_aux/survey_data_2m.csv', na = '', row.names = F)
