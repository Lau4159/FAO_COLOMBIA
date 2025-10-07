####################################################################################################
#### Proyecto de sistema de monitoreamiento de sistemas agroalimentarios (Procesamiento de datos)  #
#### Departamentos priorizados: Barranquilla (Atlántico) y Bucaramanga (Santander)                 #
#### Autor de script: Diego Andrés Cardoso López                                               #####
####################################################################################################

rm(list=ls())
paquetes = c('tidyverse','ggplot2','readxl','tidyr','dplyr','forecast','data.table','rugarch','strucchange', 'dynlm','coefplot','modelsummary', 'mapview',
             'scales','tseries','zoo','janitor', 'lubridate', 'openxlsx', 'tidyquant','car','EventStudy', 'quantmod','MSwM', 'arm', 'broom','nonnest2',
             'Ecdat', 'vars','MASS','urca','tsDyn','haven','tsutils','dyn','mFilter','anytime','vidiris','astsa','xts','foreign','timsac','lmtest','sf')
# paquetes_faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
# if(length(paquetes_faltantes)) install.packages(paquetes_faltantes)
sapply(paquetes,require,character.only=T)

###-------------------------------------------------------------------------###
#### Procesamiento de información de INS_SIVIGILA_ETA (ETA Colectivos)

# Ruta donde están los archivos
ruta <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/022_INS_SIVIGILA-ETA/"

# Listar los archivos Excel
archivos <- list.files(ruta, pattern = "^Datos_\\d{4}\\.xlsx$", full.names = TRUE)

# Filtrar solo los que están entre 2017 y 2023
archivos <- archivos[
  dplyr::between(as.integer(sub("^.*Datos_(\\d{4})\\.xlsx$", "\\1", archivos)), 2018, 2023)
]

# Función auxiliar: convertir a numérico (mantiene NA si no es convertible)
to_numeric_safely <- function(x) {
  # pasa a character, elimina espacios, cambia coma por punto y quita símbolos no numéricos
  v <- as.character(x)
  v <- gsub("\\s+", "", v)
  v <- gsub(",", ".", v)
  v <- gsub("[^0-9.+-Ee]", "", v)  # permite notación científica y signos
  suppressWarnings(as.numeric(v))
}

# Lista para guardar todas las hojas de todos los archivos
lst_indicadores <- list()

# Loop por cada archivo
for (archivo in archivos) {
  anio <- as.integer(sub("^.*Datos_(\\d{4})\\.xlsx$", "\\1", archivo))
  nombre_archivo <- paste0("eta_", anio)
  
  # Obtener las hojas
  hojas <- excel_sheets(archivo)
  
  # Loop por cada hoja
  for (hoja in hojas) {
    nombre_indicador <- paste0(nombre_archivo, "_", make_clean_names(hoja))
    
    df <- read_excel(archivo, sheet = hoja) |>
      clean_names() |>
      mutate(origen = nombre_indicador, ano = anio) |>
      # Convertir TODO a numérico excepto 'origen'
      mutate(across(-origen, to_numeric_safely))
    
    assign(nombre_indicador, df, envir = .GlobalEnv)
    lst_indicadores[[nombre_indicador]] <- df
    
    cat("Cargado:", nombre_indicador, "->", nrow(df), "filas x", ncol(df), "columnas\n")
  }
}

# Unir todas las hojas en un solo dataframe general
eta_indicadores_all <- bind_rows(lst_indicadores)

##### ETA
# =========================
# Normalizador de códigos (preserva ceros a la izquierda)
# =========================
normalize_code <- function(x, width) {
  x_chr <- as.character(x)
  x_chr <- str_replace_all(x_chr, "[^0-9]", "")
  x_chr <- ifelse(x_chr == "", NA_character_, x_chr)
  str_pad(x_chr, width = width, pad = "0")
}

# =========================
# Maestro DIVIPOLA (deptos y municipios)
# =========================
divipola_path <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/091_SHP_MGN2018_INTGRD_MPIO/DIVIPOLA_Municipios.xlxs"
if (!file.exists(divipola_path)) {
  divipola_path <- sub("\\.xlxs$", ".xlsx", divipola_path)
}
divipola <- readxl::read_excel(divipola_path) %>% janitor::clean_names()

# Se asume que las columnas del maestro ya limpiadas son: codigo_d, nombre_d, codigo_m, nombre_m
dep_ref <- divipola %>%
  transmute(
    COD_DANE_DPTO_D    = normalize_code(codigo_d, 2),
    DEPARTAMENTO_D_REF = str_squish(as.character(nombre_d))
  ) %>% distinct()

mun_ref <- divipola %>%
  transmute(
    COD_DANE_MUNIC_D = normalize_code(codigo_m, 5),
    MUNICIPIO_D_REF  = str_squish(as.character(nombre_m))
  ) %>% distinct()

# Referencia por MUNICIPIO (trae municipio y su departamento oficial)
territorio_ref <- mun_ref %>%
  mutate(COD_DANE_DPTO_D = substr(COD_DANE_MUNIC_D, 1, 2)) %>%
  left_join(dep_ref, by = "COD_DANE_DPTO_D") %>%
  distinct(COD_DANE_MUNIC_D, .keep_all = TRUE) %>%
  dplyr::select(COD_DANE_MUNIC_D, MUNICIPIO_D_REF, DEPARTAMENTO_D_REF)

# =========================
# Transformación de Black a Silver
# =========================
# === ETA: fecha por (año + semana en bloques de 4 por mes) + códigos desde cod_num + nombres oficiales ===
eta_indicadores_all <- eta_indicadores_all %>%
  mutate(
    ano       = as.integer(ano),
    semana    = pmin(pmax(as.integer(semana), 1L), 53L),
    
    # Mes aproximado por bloques de 4 semanas (1–4=ene, 5–8=feb, ...), capado a 12
    mes           = pmin(12L, ceiling(semana / 4)),
    semana_mes    = ((semana - 1L) %% 4L) + 1L,
    
    # Primer lunes del mes
    primer_dia_mes = as.Date(sprintf("%04d-%02d-01", ano, mes)),
    w              = lubridate::wday(primer_dia_mes, week_start = 1),  # 1 = lunes
    primer_lunes   = primer_dia_mes + days((8 - w) %% 7),
    
    # Fecha que crece 7 días por cada 'semana_mes'
    fecha_completa = primer_lunes + weeks(semana_mes - 1L),
    
    # Derivados de fecha
    dia        = day(fecha_completa),
    trimestre  = sprintf("%04d-T%d", ano, quarter(fecha_completa)),
    semestre   = sprintf("%04d-S%d", ano, if_else(quarter(fecha_completa) <= 2, 1L, 2L)),
    
    # Códigos DANE desde cod_num (texto, 5 dígitos)
    cod_num          = normalize_code(cod_mun, 5),
    COD_DANE_MUNIC_D = cod_num,
    COD_DANE_DPTO_D  = substr(COD_DANE_MUNIC_D, 1, 2),
    
    # Inicializa nombres (se llenan con DIVIPOLA)
    DEPARTAMENTO_D = NA_character_,
    MUNICIPIO_D    = NA_character_,
    
    # ORIGEN (placeholders)
    COD_DANE_DPTO_O  = NA_character_,
    DEPARTAMENTO_O   = NA_character_,
    COD_DANE_MUNIC_O = NA_character_,
    MUNICIPIO_O      = NA_character_
  ) %>%
  relocate(
    fecha_completa, ano, semana, mes, dia, trimestre, semestre,
    COD_DANE_DPTO_D, DEPARTAMENTO_D, COD_DANE_MUNIC_D, MUNICIPIO_D,
    .before = 1
  ) %>%
  # BUSCARX por código municipal: trae MUNICIPIO_D y DEPARTAMENTO_D oficiales
  left_join(territorio_ref, by = "COD_DANE_MUNIC_D") %>%
  mutate(
    MUNICIPIO_D    = coalesce(MUNICIPIO_D_REF, MUNICIPIO_D),
    DEPARTAMENTO_D = coalesce(DEPARTAMENTO_D_REF, DEPARTAMENTO_D)
  ) %>%
  dplyr::select(-MUNICIPIO_D_REF, -DEPARTAMENTO_D_REF, -primer_dia_mes, -w, -primer_lunes)


saveRDS(eta_indicadores_all,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/022_INS_SIVIGILA-ETA.rds", compress="gzip")

### Golden
eta_indicadores_all <- eta_indicadores_all %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,
                                                             DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D,cod_pre,agua, alimentos,
                                                             pers_pers, cont_ambie, otro, desconocid ,cas_ningun, cas_nausea, cas_vomito, cas_diarre, cas_fiebre,
                                                             cas_dolabd, cas_cefale, cas_deshid, cas_cianos, cas_mialgi, cas_altral,
                                                             cas_mareo, cas_estre, cas_escalo,cas_parest,cas_icteri, cas_acolia, 
                                                             cas_lesmac, cas_anorex, total_enf, total_exp, total_hom, total_muj,
                                                             total_vivo, total_muer,ajuste) %>% filter(ajuste!="7")

saveRDS(eta_indicadores_all,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/022_INS_SIVIGILA-ETA.rds", compress="gzip")
saveRDS(eta_indicadores_all,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/SIVIGILA_ETA/data/022_INS_SIVIGILA-ETA.rds", compress="gzip")
