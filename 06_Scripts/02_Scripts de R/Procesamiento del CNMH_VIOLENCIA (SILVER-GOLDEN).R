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


###----------------------------------------------------------------------------------------------###
#### Procesamiento de información de centro de memoria historica (Establecimeinto de microdatos)

# Carpeta y corte
dir_path <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/061_Centro Nacional de Memoria Histórica_Casos_Violencia/Corte_202503"
corte    <- "202503"   # por si luego quieres cambiar el corte

# Mapeo código -> categoría
map_cat <- c(
  AB = "Acciones bélicas",
  AP = "Ataques Poblacionales",
  AS = "Asesinatos Selectivos",
  AT = "Atentados Terroristas",
  DB = "Daños Bienes Civiles",
  DF = "Desaparición Forzada",
  MA = "Masacres",
  MI = "Minas",
  RU = "Reclutamiento y utilización",
  SE = "Secuestros",
  VS = "Violencia Sexual"
)

# Buscar archivos VictimasXX_202503.(xls|xlsx)
files <- list.files(
  dir_path,
  pattern = paste0("^Victimas([A-Z]{2})_", corte, "\\.(xls|xlsx)$"),
  full.names = TRUE
)

stopifnot(length(files) > 0)

# Función para leer un archivo (si tiene varias hojas, las une)
leer_victimas <- function(path) {
  # Extraer el XX del nombre
  base <- basename(path)
  code <- str_match(base, "^Victimas([A-Z]{2})_\\d+\\.(?:xls|xlsx)$")[,2]
  
  # Si hubiera varias hojas, leer todas y unir
  hs <- readxl::excel_sheets(path)
  dfs <- map(hs, ~ readxl::read_excel(path, sheet = .x, col_names = TRUE) %>%
               janitor::clean_names())
  df  <- bind_rows(dfs)
  
  df %>%
    mutate(
      codigo_categoria = code,
      categoria        = unname(map_cat[code]),
      fuente_archivo   = base
    )
}

# Leer todas las bases y unirlas en una sola con la columna 'categoria'
victimas_todas <- files %>% map_df(leer_victimas)

#### Limpieza de información
victimas_todas <- victimas_todas %>% filter(municipio != "SIN INFORMACION")
victimas_todas <- victimas_todas %>% filter(ano != "0000")

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

####--------------------------------------------------------------------####
#### Transformación de datos de black a silver

victimas_todas <- merge(victimas_todas,territorio_ref, by.x = "codigo_dane_de_municipio",by.y = "COD_DANE_MUNIC_D",all.x = T,all.y = T)
victimas_todas <- merge(victimas_todas,dep_ref, by = "DEPARTAMENTO_D_REF",all = T)

victimas_todas <- victimas_todas %>%
  mutate(
    # tomar ANO/MES/DIA de la base y tiparlos
    ano = suppressWarnings(as.integer(ano)),
    mes = suppressWarnings(as.integer(mes)),
    dia = suppressWarnings(as.integer(dia)),
    
    # fecha y derivados
    fecha_completa = ymd(sprintf("%04d-%02d-%02d", ano, mes, dia)),
    trimestre      = sprintf("%04d-T%d", ano, ((mes - 1L) %/% 3L) + 1L),
    semestre       = sprintf("%04d-S%d", ano, if_else(mes <= 6L, 1L, 2L))
  ) %>%
  dplyr::filter(!is.na(fecha_completa))

victimas_todas <- victimas_todas %>%
  mutate(
    DEPARTAMENTO_D_REF = coalesce(na_if(str_squish(DEPARTAMENTO_D_REF), ""), "EXTERIOR"),
    MUNICIPIO_D_REF    = coalesce(na_if(str_squish(MUNICIPIO_D_REF), ""),    "EXTERIOR")
  )

for (nm in c("COD_DANE_MUNIC_D","COD_DANE_DPTO_D")) {
  if (!nm %in% names(victimas_todas)) victimas_todas[[nm]] <- NA_character_
}

victimas_todas <- victimas_todas %>%
  mutate(
    COD_DANE_MUNIC_D = if_else(
      str_to_upper(str_squish(MUNICIPIO_D_REF)) == "EXTERIOR" &
        (is.na(COD_DANE_MUNIC_D) | str_squish(COD_DANE_MUNIC_D) == ""),
      "EX",
      as.character(COD_DANE_MUNIC_D)
    ),
    COD_DANE_DPTO_D = if_else(
      str_to_upper(str_squish(MUNICIPIO_D_REF)) == "EXTERIOR" &
        (is.na(COD_DANE_DPTO_D) | str_squish(COD_DANE_DPTO_D) == ""),
      "EX",
      as.character(COD_DANE_DPTO_D)
    )
  )

victimas_todas <- victimas_todas %>%
  ungroup() %>%
  dplyr::select(-COD_DANE_MUNIC_D)

# renombrar una o varias columnas
victimas_todas <- victimas_todas  %>%
  rename(
    MUNICIPIO_D   = MUNICIPIO_D_REF,
    COD_DANE_MUNIC_D = codigo_dane_de_municipio, 
    DEPARTAMENTO_D = DEPARTAMENTO_D_REF
  )



saveRDS(victimas_todas,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/061_Centro Nacional de Memoria Histórica_Casos_Violencia.rds", compress = "gzip")

### Golden
victimas_todas <- victimas_todas %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,
                                                   DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D,sexo,ocupacion,calidad_de_la_victima_o_la_baja,
                                                   categoria, codigo_categoria)

saveRDS(victimas_todas,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/061_Centro Nacional de Memoria Histórica_Casos_Violencia.rds", compress = "gzip")
saveRDS(victimas_todas,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/CNMH_CASOS_VIOLENCIA/data/061_Centro Nacional de Memoria Histórica_Casos_Violencia.rds", compress = "gzip")
