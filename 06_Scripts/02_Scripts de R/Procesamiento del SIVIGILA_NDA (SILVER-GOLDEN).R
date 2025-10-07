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

###------------------------------------------------------------------------###
#### Procesamiento de información de INS-SIVIGILA-NDA

# ---- Conversión segura: numérica si NO añade NAs; si no, se conserva como texto
to_numeric_or_keep <- function(x) {
  x_chr <- as.character(x)
  orig_missing <- is.na(x_chr) | str_squish(x_chr) == ""
  
  # Limpieza: espacios -> fuera; comas -> punto; quitar todo lo que no sea dígito/.+/E/e/-
  v <- gsub("\\s+", "", x_chr)        # quita espacios
  v <- gsub(",",  ".", v)             # coma decimal -> punto
  v <- gsub("[^0-9Ee.+-]", "", v, perl = TRUE)  # ← guion al final, evita rango inválido
  
  x_num <- suppressWarnings(as.numeric(v))
  
  # ¿aparecen NAs nuevos?
  if (any(is.na(x_num) & !orig_missing)) x_chr else x_num
}

###------------------------------------------------------------------------###
#### Procesamiento de información de INS-SIVIGILA-NDA

ruta <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/023_INS_SIVIGILA-NDA/Niños_Desnutrición_Aguda/"

archivos <- list.files(
  ruta,
  pattern = "^Datos_\\d{4}\\.(xls|xlsx)$",
  full.names = TRUE
)

# Filtro de años (ajustaste a 2016–2023)
archivos <- archivos[
  dplyr::between(as.integer(sub("^.*Datos_(\\d{4})\\.(?:xls|xlsx)$", "\\1", archivos)), 2016, 2023)
]

# =========================
# Conversión segura (columna a columna)
# - Numérica si NO agrega NAs nuevos
# - Si no, conserva texto original
# =========================
to_numeric_or_keep <- function(x) {
  x_chr <- as.character(x)
  orig_missing <- is.na(x_chr) | str_squish(x_chr) == ""
  
  v <- str_replace_all(x_chr, "\\s+", "")   # quitar espacios
  v <- str_replace_all(v, ",", ".")         # coma decimal -> punto
  # dejar solo dígitos, e/E, +, -, punto  (guion ESCAPADO)
  v <- gsub("[^0-9eE+\\.\\-]", "", v, perl = TRUE)
  
  x_num <- suppressWarnings(as.numeric(v))
  
  # ¿introduce NAs nuevos?
  if (any(is.na(x_num) & !orig_missing)) x_chr else x_num
}

# =========================
# Carga y procesamiento por archivo/hoja
# =========================
lst_nda <- list()

for (archivo in archivos) {
  anio <- as.integer(sub("^.*Datos_(\\d{4})\\.(?:xls|xlsx)$", "\\1", archivo))
  pref <- paste0("nda_", anio)
  
  hojas <- readxl::excel_sheets(archivo)
  
  for (hoja in hojas) {
    nombre_indicador <- paste0(pref, "_", janitor::make_clean_names(hoja))
    
    df <- readxl::read_excel(archivo, sheet = hoja) |>
      janitor::clean_names() |>
      dplyr::mutate(
        origen = nombre_indicador,
        ano    = anio
      ) |>
      # Intentar convertir TODO excepto 'origen' (id de procedencia)
      dplyr::mutate(across(-origen, to_numeric_or_keep))
    
    assign(nombre_indicador, df, envir = .GlobalEnv)
    lst_nda[[nombre_indicador]] <- df
    
    cat("Cargado:", nombre_indicador, "->", nrow(df), "filas x", ncol(df), "columnas\n")
  }
}

# =========================
# Armonización de tipos ANTES del bind_rows
# =========================

# 1) Mismo set de columnas en todas las tablas
all_cols <- unique(unlist(lapply(lst_nda, names)))

lst_nda <- lapply(lst_nda, function(df) {
  faltan <- setdiff(all_cols, names(df))
  if (length(faltan)) df[faltan] <- NA_character_  # añade faltantes como texto
  df <- df[all_cols]                               # mismo orden de columnas
  df
})

# 2) Si cualquier hoja tiene una columna en texto, todas pasan esa columna a texto
cols_texto <- Reduce(
  union,
  lapply(lst_nda, function(df) names(df)[sapply(df, is.character)])
)

if (length(cols_texto)) {
  lst_nda <- lapply(lst_nda, function(df) {
    df[cols_texto] <- lapply(df[cols_texto], as.character)
    df
  })
}

# =========================
# Unión final (sin errores de tipos)
# =========================
nda_indicadores_all <- dplyr::bind_rows(lst_nda)


# NIÑOS CON DESNUTRICIÓN AGUDA
#################################################
# ============================================================
# Normalizador de códigos
# ============================================================
normalize_code <- function(x, width) {
  x_chr <- as.character(x)
  x_chr <- stringr::str_replace_all(x_chr, "[^0-9]", "")
  x_chr <- ifelse(x_chr == "", NA_character_, x_chr)
  stringr::str_pad(x_chr, width = width, pad = "0")
}

# ============================================================
# Maestro DIVIPOLA
# ============================================================
divipola_path <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/091_SHP_MGN2018_INTGRD_MPIO/DIVIPOLA_Municipios.xlxs"
if (!file.exists(divipola_path)) {
  divipola_path <- sub("\\.xlxs$", ".xlsx", divipola_path)
}

divipola <- readxl::read_excel(divipola_path) %>%
  janitor::clean_names()

# Tablas de referencia
dep_ref <- divipola %>%
  transmute(
    COD_DANE_DPTO = normalize_code(codigo_d, 2),
    DEPARTAMENTO_REF = stringr::str_squish(as.character(nombre_d))
  ) %>% distinct()

mun_ref <- divipola %>%
  transmute(
    COD_DANE_MUNIC = normalize_code(codigo_m, 5),
    MUNICIPIO_REF  = stringr::str_squish(as.character(nombre_m))
  ) %>% distinct()

territorio_ref <- mun_ref %>%
  mutate(COD_DANE_DPTO = substr(COD_DANE_MUNIC, 1, 2)) %>%
  left_join(dep_ref, by = "COD_DANE_DPTO") %>%
  distinct(COD_DANE_MUNIC, .keep_all = TRUE) %>%
  dplyr::select(COD_DANE_MUNIC, MUNICIPIO_REF, DEPARTAMENTO_REF)

# ============================================================
# Transformación de Black a Silver
# ============================================================
nda_indicadores_all <- nda_indicadores_all %>%
  dplyr::mutate(
    ano    = as.integer(ano),
    semana = pmin(pmax(as.integer(semana), 1L), 53L),
    
    # Mes aproximado por bloques de 4 semanas (1–4=ene, 5–8=feb, ...), capado a 12
    mes        = pmin(12L, ceiling(semana / 4)),
    semana_mes = ((semana - 1L) %% 4L) + 1L,
    
    # Primer lunes del mes
    primer_dia_mes = as.Date(sprintf("%04d-%02d-01", ano, mes)),
    w              = lubridate::wday(primer_dia_mes, week_start = 1),  # 1 = lunes
    primer_lunes   = primer_dia_mes + lubridate::days((8 - w) %% 7),
    
    # Fecha que crece 7 días por cada 'semana_mes'
    fecha_completa = primer_lunes + lubridate::weeks(semana_mes - 1L),
    
    # Derivados de fecha
    dia        = lubridate::day(fecha_completa),
    trimestre  = sprintf("%04d-T%d", ano, lubridate::quarter(fecha_completa)),
    semestre   = sprintf("%04d-S%d", ano, if_else(lubridate::quarter(fecha_completa) <= 2, 1L, 2L)),
    
    # Clave municipal (texto, 5 dígitos) desde cod_mun_o
    cod_num_n          = normalize_code(cod_mun_n, 5),
    COD_DANE_MUNIC_D   = cod_num_n,
    COD_DANE_DPTO_D    = substr(COD_DANE_MUNIC_D, 1, 2),
    
    # Inicializa nombres (se llenan con DIVIPOLA)
    DEPARTAMENTO_D = NA_character_,
    MUNICIPIO_D    = NA_character_,
    
    # ORIGEN (placeholders, si luego los llenas)
    COD_DANE_DPTO_O  = NA_character_,
    DEPARTAMENTO_O   = NA_character_,
    COD_DANE_MUNIC_O = NA_character_,
    MUNICIPIO_O      = NA_character_
  ) %>%
  dplyr::relocate(
    fecha_completa, ano, semana, mes, dia, trimestre, semestre,
    COD_DANE_DPTO_D, DEPARTAMENTO_D, COD_DANE_MUNIC_D, MUNICIPIO_D,
    .before = 1
  ) %>%
  # === JOIN CORREGIDO: usa nombres de territorio_ref ===
  dplyr::left_join(territorio_ref, by = c("COD_DANE_MUNIC_D" = "COD_DANE_MUNIC")) %>%
  dplyr::mutate(
    MUNICIPIO_D    = dplyr::coalesce(MUNICIPIO_REF, MUNICIPIO_D),
    DEPARTAMENTO_D = dplyr::coalesce(DEPARTAMENTO_REF, DEPARTAMENTO_D)
  ) %>%
  dplyr::select(
    -MUNICIPIO_REF, -DEPARTAMENTO_REF,
    -primer_dia_mes, -w, -primer_lunes
  )

nda_indicadores_all <- nda_indicadores_all %>%
  mutate(cod_dpto_o = str_pad(as.character(cod_dpto_o), width = 2, pad = "0"),
         cod_mun_o  = str_pad(as.character(cod_mun_o),  width = 3, pad = "0"),
         concat = str_c(cod_dpto_o, cod_mun_o),
  ) %>% dplyr::left_join(territorio_ref, by = c("concat" = "COD_DANE_MUNIC"))

nda_indicadores_all <- nda_indicadores_all %>% mutate(COD_DANE_DPTO_O= cod_dpto_o,
                                                      DEPARTAMENTO_O = DEPARTAMENTO_REF,
                                                      MUNICIPIO_O = MUNICIPIO_REF,
                                                      COD_DANE_MUNIC_O = concat)



saveRDS(nda_indicadores_all,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/023_INS_SIVIGILA-NDA.rds", compress="gzip")

### Golden
nda_indicadores_all <- nda_indicadores_all %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,
                                                             DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D,COD_DANE_DPTO_O,
                                                             DEPARTAMENTO_O,MUNICIPIO_O,COD_DANE_MUNIC_O,cod_pre, edad,sexo,
                                                             confirmados,pac_hos,tip_cas, ajuste) %>% filter(ajuste!="7")

saveRDS(nda_indicadores_all,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/023_INS_SIVIGILA-NDA.rds", compress="gzip")
saveRDS(nda_indicadores_all,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/NDA_APP/data/023_INS_SIVIGILA-NDA.rds", compress="gzip")
