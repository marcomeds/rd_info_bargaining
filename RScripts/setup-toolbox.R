
# Librerías

instalar <- function(paquete) {
  if (!require(paquete,character.only = TRUE, 
               quietly = TRUE, 
               warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), 
                     dependencies = TRUE, 
                     repos = "http://cran.us.r-project.org")
    library(paquete, 
            character.only = TRUE, 
            quietly = TRUE, 
            warn.conflicts = FALSE)
  }
}

paquetes <- c('dplyr', 'lubridate', 'ggplot2', 'tidyr', 'knitr', 'broom',
              'devtools', 'readxl', 'stringr', 'data.table', 'sandwich',
              'lmtest', 'stargazer', 'ggthemes', 'googlesheets', 'googledrive', 'readr',
              'readstata13')

lapply(paquetes, instalar)
rm(paquetes, instalar)


options(stringsAsFactors = F, scipen = 999)

# Normalizar nombres

normalize_names = function(nombres, remove_punct = T){
  text = nombres
  
  if(remove_punct){text = gsub('[[:punct:]]', '', nombres, perl = T)}
  
   text %>%
    str_trim(., 'both') %>%
    gsub(' ', '_', .) %>%
    tolower(.) %>%
    gsub('¿', '', .) %>%     
    gsub('ú', 'u', .) %>%
    gsub('ü', 'u', .) %>%
    gsub('á', 'a', .) %>%
    gsub('ó', 'o', .) %>%
    gsub('é', 'e', .) %>%
    gsub('í', 'i', .) %>%
    gsub('ñ', 'ni', .)
}

grepl_mod = function(expr, x){
  res = grepl(expr, x)
  res[is.na(x)] <- NA
  res[x == ''] <- NA
  return(res)
}

# Auxiliares para contar nas y sustituirlos cuando es necesario

aux_n = function(x, na.rm = T){
  if(na.rm){sum(!is.na(x))}
}

arregla_na = function(x){
  num = as.numeric(x)
  if_else(is.na(num), 0, num)
}

aux_varname = function(x){
  x %>% 
    gsub('_aux_n', '', .) %>% 
    gsub('_mean', '', .)
}


format_strings = function(x, multiply = T){
  x_1 = as.numeric(x)
  aux = x_1
  if(multiply){
    aux = ifelse(x_1>1, x_1, x_1*100)
  }
  num = format(round(aux, 2), digits = 2, nsmall = 2)
  ifelse(is.na(x_1), '-', num)
}


limpia_fechas <- function(date){
  if(is.POSIXct(date) | is.Date(date)){
    fecha = date %>% as.Date()
  } else {
    fecha = as.Date(as.numeric(date), origin = '1899-12-30')
    fecha[grepl('\\/', date)] <- dmy(date[grepl('\\/', date)])
    fecha[as.numeric(date) == 0] <- NA
    fecha
  }
  return(fecha)
}

# Crear dummies de expectativas

# Para treatment data

dummy_mayor = function(x, cutoff, prob, salario = NULL, dummy_var = NULL){
  num = as.numeric(x)
  
  if(prob){
    num = if_else(num > 1, num/100, num)
  } else {
    num = num/salario
  }
  
  dummy = if_else(num > cutoff, 1, 0)
  
  if(is.null(dummy_var)){
    dummy[x == '(+)'] <- 1
    dummy[x == '(-)'] <- 0
  } else {
    dummy[dummy_var == 0] <- 0
    dummy[dummy_var == 1] <- 1
  }

  dummy
}


aux_recode = function(x){
  gsub('([A-Z])(.*)', '\\1', x) %>%
    match(., LETTERS)
}

check_to_numeric = function(x){
  check = sum(grepl('^[0-9]+$', x))/sum(!is.na(x)) > 0.1
  
  if(sum(grepl('^[0-9]+_1$', x))/length(x) > 0.1){
    return(FALSE)
  } else {
    return(check)
  }
}

# Para surveys

corte_exp = function(x_cont, corte, x_cat){
  num = as.numeric(x_cont)
  aux = if_else(num >  corte, 1, 0)
  cat = as.numeric(x_cat)
  ifelse(!is.na(aux), 
         aux,
         ifelse(!is.na(cat), cat, NA))
}

quita_nosabe = function(x){
  gsub('no sabe|no dice', '', x, ignore.case = T) 
}

# Survey summarizing

summarize_survey = function(df, base_suf = NULL){
  df %>%
    summarise_all(funs(mean, aux_n), na.rm = T) %>%
    gather(var, stat) %>%
    mutate(mean = grepl('_mean', var),
           var = aux_varname(var)) %>%
    spread(mean, stat) %>%
    rename('n' = 'FALSE', 'mean' = 'TRUE') %>%
    mutate(mean = format_strings(mean)) %>%
    setNames(nm = c('var', paste0(c('n', 'mean'), base_suf)))
}


join_vars = function(df, prefix, numeric = F){
  
cols = df %>% 
          select(starts_with(prefix)) %>% names()
          
res =  df %>% 
    mutate_at(vars(starts_with(prefix)), 
              function(x) if_else(is.na(x), '', as.character(x))) %>%
    unite_(prefix, cols, sep = '', remove = T)

if(numeric){
  res = res %>% 
        mutate_at(vars(contains(prefix)), as.numeric)
}

return(res)
}


dummy_from_mixedcol = function(x){
  grepl_mod('[0-9]+', x) %>%
    as.numeric()
}



###################### Aux para datos OPM


aux_dummy = function(x){
  ifelse(x == 2, 0, 
         ifelse(x == 1, 1, NA))
}





inputar_media = function(x){
  media = mean(x, na.rm = T)
  
  x[is.na(x)] <- media
  
  return(x)
}



# Fix para inputar la media en expectativas

inputar_media_cutoff = function(x, cutoff_dummy, yes = .875, no = .37){
  estimate = if_else(cutoff_dummy == 1, yes, no)
  
  if_else(is.na(x), estimate, x/1)
}
