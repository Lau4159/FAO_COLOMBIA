####################################################################################################
#### Proyecto de sistema de monitoreamiento de sistemas agroalimentarios (Procesamiento de datos)  #
#### Departamentos priorizados: Barranquilla (Atlántico) y Bucaramanga (Santander)                 #
#### Autor de script: Diego Andrés Cardoso López                                               #####
####################################################################################################

#### Procesamiento de la información del Evaluación Agropecuaria - EVA

rm(list=ls())
paquetes = c('tidyverse','ggplot2','readxl','tidyr','dplyr','forecast','data.table','rugarch','strucchange', 'dynlm','coefplot','modelsummary', 'mapview',
             'scales','tseries','zoo','janitor', 'lubridate', 'openxlsx', 'tidyquant','car','EventStudy', 'quantmod','MSwM', 'arm', 'broom','nonnest2',
             'Ecdat', 'vars','MASS','urca','tsDyn','haven','tsutils','dyn','mFilter','anytime','vidiris','astsa','xts','foreign','timsac','lmtest','sf')
# paquetes_faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
# if(length(paquetes_faltantes)) install.packages(paquetes_faltantes)
sapply(paquetes,require,character.only=T)

######################################################
##### Procesamiento del EVA Agrícola
######################################################

# --- Definición de ruta donde están los archivos originales ---
ruta <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/011_UPRA_EVA-A/EVA/"

# --- Identificar todos los archivos Excel que empiezan con año (ej. 2019.xlsx, 2020_algo.xlsx) ---
archivos <- list.files(path = ruta, pattern = "^\\d{4}.*\\.xlsx$", full.names = TRUE)

# --- Loop: cargar cada archivo y crear objeto con nombre eva_agricola_YYYY ---
# Lee desde la fila 3 (skip = 3) y limpia nombres de columnas
for (archivo in archivos) {
  base <- basename(archivo)
  anio <- sub("^(\\d{4}).*$", "\\1", base)                  # extrae el año del nombre del archivo
  obj  <- paste0("eva_agricola_", anio)                     # crea el nombre del objeto
  
  df <- read_excel(archivo, skip = 3) |> clean_names()       # lectura y limpieza de encabezados
  assign(obj, df, envir = .GlobalEnv)                       # guarda como objeto en el entorno global
  cat("Cargado:", obj, "->", nrow(df), "filas x", ncol(df), "columnas\n")
}

######################################################
# Ajustes en años posteriores a 2021
# Nota: los archivos recientes (2022-2024) acumulan info de años previos
#       por eso se ajustan para tomar solo el año correspondiente.
######################################################

# Fila del encabezado en cada base (distinto según el año)
hdr_row <- c(eva_agricola_2022 = 2,
             eva_agricola_2023 = 3,
             eva_agricola_2024 = 4)

# Verificar cuáles de esos objetos existen en el entorno
present <- names(hdr_row)[sapply(names(hdr_row), exists)]

# Corregir encabezados según corresponda
for (nm in present) {
  df <- get(nm); r <- hdr_row[[nm]]
  nms <- make_clean_names(as.character(unlist(df[r, ], use.names = FALSE))) # genera nombres limpios
  df  <- df[-(1:r), , drop = FALSE]  # elimina filas de encabezado
  names(df) <- nms                   # asigna nombres corregidos
  assign(nm, df, envir = .GlobalEnv) # reemplaza el objeto en el entorno
}

######################################################
# Normalización de nombres y tipos en cada año (2019–2021)
# Se renombran columnas clave para que sean homogéneas
######################################################

# --- 2019 ---
eva_agricola_2019 <- eva_agricola_2019 %>%
  rename(
    area_sembrada_ha   = area_sembrada,
    area_cosechada_ha  = area_cosechada,
    produccion_t       = produccion,
    rendimiento_t_ha   = rendimiento
  ) %>%
  mutate(
    area_sembrada_ha   = suppressWarnings(as.numeric(area_sembrada_ha)),
    area_cosechada_ha  = suppressWarnings(as.numeric(area_cosechada_ha)),
    produccion_t       = suppressWarnings(as.numeric(produccion_t)),
    rendimiento_t_ha   = suppressWarnings(as.numeric(rendimiento_t_ha))
  )

# --- 2020 ---
eva_agricola_2020 <- eva_agricola_2020 %>%
  rename(
    area_sembrada_ha   = area_sembrada,
    area_cosechada_ha  = area_cosechada,
    produccion_t       = produccion,
    rendimiento_t_ha   = rendimiento
  ) %>%
  mutate(
    area_sembrada_ha   = suppressWarnings(as.numeric(area_sembrada_ha)),
    area_cosechada_ha  = suppressWarnings(as.numeric(area_cosechada_ha)),
    produccion_t       = suppressWarnings(as.numeric(produccion_t)),
    rendimiento_t_ha   = suppressWarnings(as.numeric(rendimiento_t_ha))
  )

# --- 2021 ---
eva_agricola_2021 <- eva_agricola_2021 %>%
  rename(
    area_sembrada_ha   = area_sembrada,
    area_cosechada_ha  = area_cosechada,
    produccion_t       = produccion,
    rendimiento_t_ha   = rendimiento
  ) %>%
  mutate(
    area_sembrada_ha   = suppressWarnings(as.numeric(area_sembrada_ha)),
    area_cosechada_ha  = suppressWarnings(as.numeric(area_cosechada_ha)),
    produccion_t       = suppressWarnings(as.numeric(produccion_t)),
    rendimiento_t_ha   = suppressWarnings(as.numeric(rendimiento_t_ha))
  )

######################################################
# Filtrar únicamente el año correcto en bases posteriores a 2021
######################################################

eva_agricola_2022 <- eva_agricola_2022 %>% filter(ano == 2022)
eva_agricola_2023 <- eva_agricola_2023 %>% filter(ano == 2023)
eva_agricola_2024 <- eva_agricola_2024 %>% filter(ano == 2024)

######################################################
# Función auxiliar: convertir a numérico y eliminar filas incompletas
######################################################

# Columnas que deben ser numéricas
cols_num <- c("area_sembrada_ha", "area_cosechada_ha", 
              "produccion_t", "rendimiento_t_ha", "ano", "codigo_del_cultivo")

# Función para limpieza
clean_numeric_drop_na <- function(df, cols = cols_num) {
  present <- intersect(cols, names(df))     # columnas que existen en el df
  if (length(present) == 0) return(df)      # si no hay, devuelve igual
  df |>
    mutate(across(all_of(present), ~ suppressWarnings(as.numeric(.)))) |>  # convertir a numérico
    filter(if_all(all_of(present), ~ !is.na(.)))                          # eliminar filas con NA
}

# Aplicar la función a cada objeto eva_agricola_YYYY
objs <- ls(pattern = "^eva_agricola_\\d{4}$")
for (nm in objs) {
  assign(nm, clean_numeric_drop_na(get(nm)), envir = .GlobalEnv)
}

######################################################
# Unión de todas las bases EVA Agrícola (2019–2024)
######################################################

eva_agricola <- bind_rows(
  eva_agricola_2019, eva_agricola_2020, eva_agricola_2021,
  eva_agricola_2022, eva_agricola_2023, eva_agricola_2024
)

# Eliminar periodo duplicado/indeseado de 2019A
eva_agricola <- eva_agricola %>% filter(periodo != "2019A") 

######################################################
# Ajustes finales: asegurar formato y nombres de cultivo
######################################################

# Convertir código a texto para evitar pérdida de ceros
eva_agricola <- eva_agricola %>%
  mutate(codigo_del_cultivo = as.character(codigo_del_cultivo))

# Identificar columna del nombre de cultivo (ajustar si el nombre es distinto)
nom_col <- "cultivo"

# Reemplazar el nombre del cultivo por el primero no vacío observado por código
eva_agricola <- eva_agricola %>%
  group_by(codigo_del_cultivo) %>%
  mutate(!!nom_col := first(na.omit(.data[[nom_col]]))) %>%
  ungroup()

####------------------------------------------------------------------------####
##### Procesamiento de infromación de BLACK a SILVER a GOLDEN
####------------------------------------------------------------------------####

####### EVA Agropecuaria
#### Procesamiento del EVA, agregando información de año cosechado y año sembrado
eva_agricola <- eva_agricola %>% mutate(ano_sembrado=ano,
                                        ano_cosechado=ifelse(periodo=="2019B" | periodo=="2020A" | periodo=="2020",2020,
                                                             ifelse(periodo=="2020B" | periodo=="2021A" | periodo=="2021", 2021,
                                                                    ifelse(periodo=="2021B" | periodo=="2022A" | periodo=="2022", 2022,
                                                                           ifelse(periodo=="2022B" | periodo=="2023A" | periodo=="2023", 2023, 
                                                                                  ifelse(periodo=="2023B" | periodo=="2024A" | periodo=="2024",2024,2025  # Sí desea ser actualización, el usuario debe hacer un nuevo el condicional adicional para seguir con el proceso
                                                                                  ))))),
                                        desagregacion_cultivo =ifelse(desagregacion_cultivo == "Caña panelera" | desagregacion_cultivo == "Caña Panelera","Caña Panelera",
                                                                      ifelse(desagregacion_cultivo == "Caña Miel" | desagregacion_cultivo == "Caña miel","Caña Miel",
                                                                             ifelse(desagregacion_cultivo == "Caña de Azúcar" | desagregacion_cultivo == "Caña de azúcar" | desagregacion_cultivo == "Caña de Azucar", "Caña de Azúcar", desagregacion_cultivo))),
                                        cultivo= case_when(
                                          str_squish(str_to_lower(cultivo)) %in% c("caña", "cana") ~
                                            coalesce(desagregacion_cultivo, cultivo),
                                          TRUE ~ cultivo)) %>% filter(ano_cosechado!=2025) %>% filter(ano_sembrado!=2019)

# --- Normalizador de códigos (preserva ceros a la izquierda) ---
normalize_code <- function(x, width) {
  x_chr <- as.character(x)
  x_chr <- str_replace_all(x_chr, "[^0-9]", "")
  x_chr <- ifelse(x_chr == "", NA_character_, x_chr)
  str_pad(x_chr, width = width, pad = "0")
}

# --- Ruta al maestro DIVIPOLA (auto-corrección de extensión si viene .xlxs) ---
divipola_path <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/091_SHP_MGN2018_INTGRD_MPIO/DIVIPOLA_Municipios.xlxs"
if (!file.exists(divipola_path)) {
  divipola_path <- sub("\\.xlxs$", ".xlsx", divipola_path)
}

# --- Leer maestro y normalizar encabezados ---
divipola <- readxl::read_excel(divipola_path) %>% janitor::clean_names()

# --- Helper para elegir columnas por patrón (tolerante a nombres distintos) ---
pick_col <- function(df, patterns) {
  nm <- names(df)
  for (p in patterns) {
    hit <- which(grepl(p, nm, ignore.case = TRUE))
    if (length(hit)) return(nm[hit[1]])
  }
  stop("No se encontró una columna que coincida con: ", paste(patterns, collapse = " | "))
}

# Intentar detectar columnas típicas de DIVIPOLA (DANE)
col_dpto_code <- pick_col(divipola, c("codigo_d"))
col_dpto_name <- pick_col(divipola, c("nombre_d"))
col_muni_code <- pick_col(divipola, c("codigo_m"))
col_muni_name <- pick_col(divipola, c("nombre_m"))

# --- Tablas de referencia (una fila por código) ---
dep_ref <- divipola %>%
  transmute(
    COD_DANE_DPTO_D    = normalize_code(.data[[col_dpto_code]], 2),
    DEPARTAMENTO_D_REF = str_squish(as.character(.data[[col_dpto_name]]))
  ) %>%
  distinct()

mun_ref <- divipola %>%
  transmute(
    COD_DANE_MUNIC_D = normalize_code(.data[[col_muni_code]], 5),
    MUNICIPIO_D_REF  = str_squish(as.character(.data[[col_muni_name]]))
  ) %>%
  distinct()


# ================================
# Transformación de Black a Silver
# ================================
eva_agricola <- eva_agricola %>%
  mutate(
    # --- tiempo ---
    ano  = suppressWarnings(as.integer(ano)),
    mes  = 1L,
    dia  = 1L,
    fecha_completa = ymd(sprintf("%04d-%02d-%02d", ano, mes, dia)),
    trimestre      = sprintf("%04d-T%d", ano, ((mes - 1) %/% 3) + 1),
    semestre       = sprintf("%04d-S%d", ano, if_else(mes <= 6, 1L, 2L)),
    
    # --- DESTINO (desde tus columnas actuales) ---
    COD_DANE_DPTO_D  = normalize_code(codigo_dane_departamento, 2),
    DEPARTAMENTO_D   = as.character(departamento),
    COD_DANE_MUNIC_D = normalize_code(codigo_dane_municipio, 5),
    MUNICIPIO_D      = as.character(municipio),
    
    # --- ORIGEN (placeholder) ---
    COD_DANE_DPTO_O  = NA_character_,
    DEPARTAMENTO_O   = NA_character_,
    COD_DANE_MUNIC_O = NA_character_,
    MUNICIPIO_O      = NA_character_
  ) %>%
  relocate(
    fecha_completa, ano, mes, dia, trimestre, semestre,
    COD_DANE_DPTO_D, DEPARTAMENTO_D, COD_DANE_MUNIC_D, MUNICIPIO_D,
    .before = 1
  ) %>%
  # --- BUSCARX: nombre de DEPARTAMENTO por código (prioriza maestro DIVIPOLA) ---
  left_join(dep_ref, by = "COD_DANE_DPTO_D") %>%
  mutate(DEPARTAMENTO_D = coalesce(DEPARTAMENTO_D_REF, DEPARTAMENTO_D)) %>%
  dplyr::select(-DEPARTAMENTO_D_REF) %>%
  # --- BUSCARX: nombre de MUNICIPIO por código (prioriza maestro DIVIPOLA) ---
  left_join(mun_ref, by = "COD_DANE_MUNIC_D") %>%
  mutate(MUNICIPIO_D = coalesce(MUNICIPIO_D_REF, MUNICIPIO_D)) %>%
  dplyr::select(-MUNICIPIO_D_REF)

##### Guardado de datos de Black a Silver
saveRDS(eva_agricola,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/011_UPRA_EVA-A.rds")


### Selección de variables - Transformación de datos de silver a golden
eva_agricola <- eva_agricola %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,
                                               DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D, grupo_cultivo, 
                                               cultivo,ano_sembrado,ano_cosechado, periodo, area_sembrada_ha,area_cosechada_ha,
                                               produccion_t,rendimiento_t_ha,ciclo_del_cultivo,codigo_del_cultivo) %>% mutate(produccion_t=ifelse(cultivo=="Caña de Azúcar" | cultivo=="Caña Miel" | cultivo=="Caña Panelera",produccion_t/10,produccion_t)) %>%
  mutate(rendimiento_t_ha=ifelse(cultivo=="Caña de Azúcar" | cultivo=="Caña Miel" | cultivo=="Caña Panelera",rendimiento_t_ha/10,rendimiento_t_ha))


saveRDS(eva_agricola,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/011_UPRA_EVA-A.rds")
saveRDS(eva_agricola,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/EVA_A/data/011_UPRA_EVA-A.rds")
