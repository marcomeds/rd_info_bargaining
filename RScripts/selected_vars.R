###################################################

# Baseline

###################################################


vars_t = c('salario_diario',
           'mujer', 
           'prob_ganar',
           'cantidad_ganar', 
           'prob_mayor',
           'cant_mayor',
           'carta_renuncia',
           'c_min_total',
           'prob_num',
           'prob_coarse',
           'cantidad_num',
           'cantidad_coarse')

vars_t_nofix = c('prob_ganar',
                 'cantidad_ganar',
                 'c_min_total',
                 'prob_num', 
                 'prob_coarse',
                 'cantidad_num',
                 'cantidad_coarse')

###################################################

# Two weeks

###################################################

# Pendiente: revisar
vars_2s = c('conflicto_arreglado', 
            'entablo_demanda', 
            'se_registro_el_acuerdo_ante_la_junta', 
            'tiempo_arreglo', 
            'sabemos_cantidad', 
            'sabemos_fecha_arreglo', 
            'se_registro_el_acuerdo_ante_la_junta',
            'ha_hablado_con_abogado_publico',
            'ha_hablado_con_abogado_privado',
            'firmo_carta_poder',
            'tramito_citatorio',
            'sigue_buscando_alternativas_de_abogados_privados',
            'piensa_hacer_algo_respecto_al_despido',
            'prob_num',
            'prob_coarse',
            'update_prob',
            'update_comp',
            'switched_prob',
            'switched_comp',
            'cantidad_num',
            'cantidad_coarse',
            'mas_secundaria',
            'probabilidad_de_ganar',
            'monto_que_espera_recibir',
            'mas_75_aux',
            'mas_6m_aux',
            'prob_ganar',
            'cantidad_ganar', 
            'prob_mayor',
            'cant_mayor')


vars_2s_nofix = c('id_actor',
                  'origen',
                  'monto_del_convenio',
                  'donde_lo_contacto',
                  'como_lo_encontro',
                  'especifique',
                  'porcentaje',
                  'cuota_por_iniciar_juicio',
                  'especifique2',
                  'que_piensa_hacer',
                  'especifique4',
                  'prob_ganar_survey',
                  'prob_mayor_survey',
                  'cantidad_ganar_survey',
                  'cant_mayor_survey',
                  'hace_cuanto_anios',
                  'salario_diario', 
                  'tiempo_arreglo')


###################################################

# Two months

###################################################

vars_2m = c(vars_2s,
            'nivel_de_felicidad',
            'demando_con_abogado_publico',
            'enojo_con_la_empresa',
            'busca_trabajo',
            'visitas_a_la_junta_y_procuraduria',
            'ultimos_3_meses_ha_dejado_de_pagar_servicio_basico',
            'ultimos_3_meses_le_ha_faltado_dinero_para_comida',                                  
            'ultimo_mes_alguien_del_hogar_a_comprado_casa_o_terreno_o_vehiculo',
            'ultimo_mes_alguien_del_hogar_a_comprado_electrodomestico_o_dispositivo_electronico')

vars_2m_nofix = c('id_actor',
                  'origen',
                  'cantidad_ofrecida_de_recarga',
                  'nivel_de_felicidad',
                  'monto_del_convenio',
                  'como_lo_consiguio',
                  'porcentaje',
                  'cuota_por_iniciar_juicio',
                  'donde_lo_contacto',
                  'cuantos_dias_de_salario_corresponde_la_indem_const',
                  'nivel_de_satisfaccion_abogado',
                  'enojo_con_la_empresa',
                  'prob_ganar_survey',
                  'cantidad_ganar_survey',
                  'prob_num_survey',
                  'cantidad_num_survey',
                  'prob_mayor_survey',
                  'cant_mayor_survey',
                  'que_elemento_es_el_mas_importante_al_elaborar_expectativas',
                  'minimo_que_aceptaria_para_solucionar_conflicto',
                  'comparacion_con_el_trabajo_anterior',
                  'probabilidad_de_que_encuentre_trabajo_en_los_prox_3_meses',
                  'visitas_a_la_junta_y_procuraduria',
                  'promedio_de_horas_por_visita',
                  'promedio_de_horas_en_traslado',
                  'tiempo_arreglar_asunto',
                  'timestamp',
                  'nombre_encuestador')
