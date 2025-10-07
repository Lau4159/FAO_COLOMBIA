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

####-------------------------------------------------------------------------------------------###
#### Procesamiento de información del DNP - IMDM
ruta <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/071_DNP_Terridata_IDM/TerriData_Dim8.xlsx/TerriData_Dim8.xlsx"

# 1) Leo solo el encabezado para obtener los nombres originales
hdr <- read_excel(ruta, n_max = 0)
# Los normalizo igual que tú para ubicar el índice de 'dato_numerico'
idx_dn <- which(janitor::make_clean_names(names(hdr)) == "dato_numerico")

# 2) Armo vector de tipos y fuerzo esa columna a 'numeric'
col_types <- rep("guess", ncol(hdr))
if (length(idx_dn) == 1) col_types[idx_dn] <- "numeric"

# 3) Leo el archivo con ese esquema de tipos (y escaneo completo para evitar malas inferencias)
df <- read_excel(ruta, col_types = col_types, guess_max = Inf) %>% clean_names()

# 4) Filtras MDM y te quedas con el valor numérico
df_mdm <- df %>%
  filter(indicador == "MDM") %>%
  # aquí 'dato_numerico' ya viene como número
  dplyr::select(codigo_departamento, departamento,codigo_entidad, entidad, ano, mes, valor = dato_numerico, unidad_de_medida, fuente)

glimpse(df_mdm)

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


#--------------------------------------------
#### Transformación de datos de Black a Silver
#--------------------------------------------------
df_mdm <- merge(df_mdm,territorio_ref, by.x = "codigo_entidad",by.y = "COD_DANE_MUNIC_D",all.x = T,all.y = T)
df_mdm <- df_mdm %>% filter(codigo_departamento != '01')

# renombrar una o varias columnas
df_mdm <- df_mdm  %>%
  rename(
    MUNICIPIO_D   = MUNICIPIO_D_REF,
    COD_DANE_MUNIC_D = codigo_entidad,
    COD_DANE_DPTO_D = codigo_departamento,
    DEPARTAMENTO_D = DEPARTAMENTO_D_REF
  )

df_mdm <- df_mdm %>%
  mutate(
    # tomar ANO/MES/DIA de la base y tiparlos
    ano = suppressWarnings(as.integer(ano)),
    mes = suppressWarnings(as.integer(mes)),
    dia = 1L,
    
    # fecha y derivados
    fecha_completa = ymd(sprintf("%04d-%02d-%02d", ano, mes, dia)),
    trimestre      = sprintf("%04d-T%d", ano, ((mes - 1L) %/% 3L) + 1L),
    semestre       = sprintf("%04d-S%d", ano, if_else(mes <= 6L, 1L, 2L))
  ) %>%
  dplyr::filter(!is.na(fecha_completa))

saveRDS(df_mdm,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/071_DNP_Terridata_IDM.rds", compress = "gzip")

### Golden
df_mdm <- df_mdm %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D,
                                   unidad_de_medida,fuente,valor)

saveRDS(df_mdm,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/071_DNP_Terridata_IDM.rds", compress = "gzip")
saveRDS(df_mdm,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/DNP-IDM/data/071_DNP_Terridata_IDM.rds", compress = "gzip")
