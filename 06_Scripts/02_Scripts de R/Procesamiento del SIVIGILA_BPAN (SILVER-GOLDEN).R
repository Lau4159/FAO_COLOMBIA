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
#### Procesamiento de infromación de INS_SIVIGILA_BPAN
###---------------------------------------------------------------------------


# Ruta donde están los archivos
ruta <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/021_INS_SIVIGILA-BPAN/"

# Listar los archivos de Excel
archivos <- list.files(ruta, pattern = "^Datos_\\d{4}\\.xls$", full.names = TRUE)

# Lista para guardar todas las hojas de todos los archivos
lst_indicadores <- list()

# Loop por cada archivo
for (archivo in archivos) {
  anio <- as.integer(sub("^.*Datos_(\\d{4})\\.xls$", "\\1", archivo))
  nombre_archivo <- paste0("bpan_", anio)
  
  # Obtener los nombres de las hojas
  hojas <- excel_sheets(archivo)
  
  # Loop por cada hoja
  for (hoja in hojas) {
    nombre_indicador <- paste0(nombre_archivo, "_", make_clean_names(hoja))
    
    df <- read_excel(archivo, sheet = hoja) |>
      clean_names() |>
      mutate(origen = nombre_indicador, ano = anio)
    
    assign(nombre_indicador, df, envir = .GlobalEnv)
    lst_indicadores[[nombre_indicador]] <- df
    
    cat("Cargado:", nombre_indicador, "->", nrow(df), "filas x", ncol(df), "columnas\n")
  }
}

# (Opcional) Unir todas las hojas en un solo dataframe general
bpan_indicadores_all <- bind_rows(lst_indicadores)

###### INS_SIVIGILA_BPAN
### Selección de información y cambio de nombres
# Tabla de referencia: código depto -> nombre depto
# --- Normalizador de códigos (preserva ceros a la izquierda) ---
normalize_code <- function(x, width) {
  x_chr <- as.character(x)
  x_chr <- str_replace_all(x_chr, "[^0-9]", "")
  x_chr <- ifelse(x_chr == "", NA_character_, x_chr)
  str_pad(x_chr, width = width, pad = "0")
}

# --- Leer maestro DIVIPOLA (corrige .xlxs -> .xlsx si aplica) ---
divipola_path <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/091_SHP_MGN2018_INTGRD_MPIO/DIVIPOLA_Municipios.xlxs"
if (!file.exists(divipola_path)) {
  divipola_path <- sub("\\.xlxs$", ".xlsx", divipola_path)
}
divipola <- readxl::read_excel(divipola_path) %>% janitor::clean_names()
# Se asume que las columnas del maestro, ya limpiadas, son: codigo_d, nombre_d, codigo_m, nombre_m
dep_ref <- divipola %>%
  transmute(
    COD_DANE_DPTO_D     = normalize_code(codigo_d, 2),
    DEPARTAMENTO_D_REF  = str_squish(as.character(nombre_d))
  ) %>% distinct()

mun_ref <- divipola %>%
  transmute(
    COD_DANE_MUNIC_D  = normalize_code(codigo_m, 5),
    MUNICIPIO_D_REF   = str_squish(as.character(nombre_m))
  ) %>% distinct()

############################################################
#### Procesamiento de Black a Silver
############################################################


bpan_indicadores_all <- bpan_indicadores_all %>%
  mutate(
    # fecha exacta desde fec_not (YYYY-MM-DD)
    fecha_completa = as.Date(fec_not),
    
    # derivados de la fecha
    ano       = lubridate::year(fecha_completa),
    mes       = lubridate::month(fecha_completa),
    dia       = lubridate::day(fecha_completa),
    trimestre = sprintf("%04d-T%d", ano, lubridate::quarter(fecha_completa)),
    semestre  = sprintf("%04d-S%d", ano, if_else(lubridate::quarter(fecha_completa) <= 2, 1L, 2L)),
    
    # DESTINO de Notificación
    COD_DANE_DPTO_D  = normalize_code(cod_dpto_n, 2),
    DEPARTAMENTO_D   = as.character(departamento_notificacion),
    COD_DANE_MUNIC_D = normalize_code(cod_mun_n, 5),
    MUNICIPIO_D      = as.character(municipio_notificacion),
    
    # ORIGEN - Ocurrencia (usando concat_ocurrencia)
    COD_DANE_DPTO_O_raw  = normalize_code(cod_dpto_o, 2),
    COD_DANE_MUNIC_O_raw = normalize_code(cod_mun_o, 3),
    
    # tu concatenado de ocurrencia (2 + 5 dígitos)
    concat_ocurrencia = paste0(COD_DANE_DPTO_O_raw, COD_DANE_MUNIC_O_raw),
    
    # asigna los campos finales de ORIGEN a partir del concat normalizado
    COD_DANE_DPTO_O  = substr(concat_ocurrencia, 1, 2),
    COD_DANE_MUNIC_O = substr(concat_ocurrencia, 1, 5),
    
    # inicializa nombres (serán reemplazados por DIVIPOLA)
    DEPARTAMENTO_O = NA_character_,
    MUNICIPIO_O    = NA_character_
  ) %>%
  relocate(
    fecha_completa, ano, mes, dia, trimestre, semestre,
    COD_DANE_DPTO_D, DEPARTAMENTO_D, COD_DANE_MUNIC_D, MUNICIPIO_D,
    COD_DANE_DPTO_O, DEPARTAMENTO_O, COD_DANE_MUNIC_O, MUNICIPIO_O,
    .before = 1
  ) %>%
  # ---- Reemplazar NOMBRES de DESTINO por oficiales (DIVIPOLA)
  left_join(dep_ref, by = "COD_DANE_DPTO_D") %>%
  mutate(DEPARTAMENTO_D = coalesce(DEPARTAMENTO_D_REF, DEPARTAMENTO_D)) %>%
  dplyr::select(-DEPARTAMENTO_D_REF) %>%
  left_join(mun_ref, by = "COD_DANE_MUNIC_D") %>%
  mutate(MUNICIPIO_D = coalesce(MUNICIPIO_D_REF, MUNICIPIO_D)) %>%
  dplyr::select(-MUNICIPIO_D_REF) %>%
  # ---- Reemplazar NOMBRES de ORIGEN por oficiales (DIVIPOLA)
  left_join(dep_ref, by = c("COD_DANE_DPTO_O"  = "COD_DANE_DPTO_D")) %>%
  mutate(DEPARTAMENTO_O = coalesce(DEPARTAMENTO_D_REF, DEPARTAMENTO_O)) %>%
  dplyr::select(-DEPARTAMENTO_D_REF) %>%
  left_join(mun_ref, by = c("COD_DANE_MUNIC_O" = "COD_DANE_MUNIC_D")) %>%
  mutate(MUNICIPIO_O = coalesce(MUNICIPIO_D_REF, MUNICIPIO_O)) %>%
  dplyr::select(-MUNICIPIO_D_REF)

saveRDS(bpan_indicadores_all,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/021_INS_SIVIGILA-BPAN.rds", compress = "gzip")


### Golden
bpan_indicadores_all <- bpan_indicadores_all %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,
                                                               DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D,COD_DANE_DPTO_O,
                                                               COD_DANE_MUNIC_O,DEPARTAMENTO_O,MUNICIPIO_O,consecutive,ajuste,
                                                               edad,uni_med, sexo, tip_ss, per_etn, gru_pob, tip_cas, pac_hos,
                                                               con_fin, cbmte,confirmados) %>% filter(ajuste=="7")

saveRDS(bpan_indicadores_all,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/021_INS_SIVIGILA-BPAN.rds", compress = "gzip")
saveRDS(bpan_indicadores_all,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/SIVIGILA_BPAN/data/021_INS_SIVIGILA-BPAN.rds", compress = "gzip")
