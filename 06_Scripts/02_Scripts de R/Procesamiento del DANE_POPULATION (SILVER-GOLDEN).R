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


###--------------------------------------------------------------------------------------###
#### Procesamiento de información de DANE Proyecciones de población

dir_path <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/051_DANE_Proyecciones_P"
procesar_proy <- function(dir_path, base_name, skip_rows = 11) {
  archivo <- list.files(
    dir_path,
    pattern = paste0("^", base_name, "\\.(xls|xlsx)$"),
    full.names = TRUE
  )
  stopifnot(length(archivo) == 1)
  
  df <- readxl::read_excel(archivo, skip = skip_rows, col_names = TRUE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      dp   = stringr::str_pad(as.character(.data$dp),   2, pad = "0"),
      dpmp = stringr::str_pad(as.character(.data$dpmp), 5, pad = "0"),
      anio = suppressWarnings(as.integer(.data$ano))
    )
  
  df_long <- df |>
    tidyr::pivot_longer(
      cols = tidyselect::matches("^(hombres|mujeres|total)_(\\d+|85_y_mas)$"),
      names_to = c("sexo", "edad_raw"),
      names_pattern = "^(hombres|mujeres|total)_(\\d+|85_y_mas)$",
      values_to = "poblacion"
    ) |>
    dplyr::mutate(
      edad       = dplyr::if_else(.data$edad_raw == "85_y_mas", 85L, as.integer(.data$edad_raw)),
      edad_grupo = dplyr::if_else(.data$edad_raw == "85_y_mas", "85_y_mas", "edad_simple"),
      poblacion  = suppressWarnings(as.numeric(.data$poblacion))
    ) |>
    dplyr::select(-.data$edad_raw) |>
    dplyr::rename(
      dpto_cod = dp,
      dpto_nom = dpnom,
      muni_cod = dpmp,
      muni_nom = mpio,
      area_geo = area_geografica
    ) |>
    dplyr::select(
      dpto_cod, dpto_nom, muni_cod, muni_nom, anio, area_geo,
      sexo, edad, edad_grupo, poblacion, dplyr::everything()
    ) |>
    dplyr::filter(stringr::str_to_lower(stringr::str_trim(area_geo)) != "total")
  
  return(df_long)
}

# Procesar cada período
base_85_94 <- "DCD-area-sexo-edad-proypoblacion-Mun-1985-1994"
base_95_04 <- "DCD-area-sexo-edad-proypoblacion-Mun-1995-2004"
base_05_17 <- "DCD-area-sexo-edad-proypoblacion-Mun-2005-2017_VP"


df_long_1985_1994 <- procesar_proy(dir_path, base_85_94)
df_long_1995_2004 <- procesar_proy(dir_path, base_95_04)
df_long_2005_2017 <- procesar_proy(dir_path, base_05_17)

# Unir ambos periodos
df_long_1985_2017 <- bind_rows(df_long_1985_1994, df_long_1995_2004,df_long_2005_2017)

##### Procesamiento de información para los años de 2018 - 2042
dir_path   <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/051_DANE_Proyecciones_P"
base_18_42 <- "PPED-AreaSexoEdadMun-2018-2042_VP"
sheet_name <- "PobMunicipalxÁreaSexoEdad"

archivo <- list.files(dir_path, pattern = paste0("^", base_18_42, "\\.(xls|xlsx)$"), full.names = TRUE)
stopifnot(length(archivo) == 1)

# A8:F* (fila 8 como header, columnas 1-6)
id_2018_2042 <- readxl::read_excel(
  path  = archivo,
  sheet = sheet_name,
  range = readxl::cell_limits(ul = c(8, 1), lr = c(NA, 6)),
  col_names = TRUE
) |>
  janitor::clean_names() |>
  dplyr::mutate(
    dp   = stringr::str_pad(as.character(dp),   2, pad = "0"),
    dpmp = stringr::str_pad(as.character(dpmp), 5, pad = "0"),
    anio = suppressWarnings(as.integer(ano))
  ) |> drop_na()

### Ahora procesamiento de datos para hombres y mujeres
# Convierte letras de columna de Excel (p. ej., "J", "HC") a índice numérico
excel_col_index <- function(col_letters) {
  letters <- strsplit(toupper(col_letters), "")[[1]]
  sum((match(letters, LETTERS)) * 26^(rev(seq_along(letters)) - 1))
}

# Cargar un rango: filas 1–8 fuera, fila 9 encabezado; columnas J:HC
cargar_rango_J_HC <- function(dir_path, base_name, sheet_name,
                              header_row = 9, from_col = "J", to_col = "HC") {
  archivo <- list.files(
    dir_path,
    pattern = paste0("^", base_name, "\\.(xls|xlsx)$"),
    full.names = TRUE
  )
  stopifnot(length(archivo) == 1)
  
  ul_col <- excel_col_index(from_col)   # J -> 10
  lr_col <- excel_col_index(to_col)     # HC -> 211
  
  readxl::read_excel(
    path      = archivo,
    sheet     = sheet_name,
    col_names = TRUE,
    # fila 9 como header; columnas J:HC
    range     = readxl::cell_limits(ul = c(header_row, ul_col), lr = c(NA, lr_col))
  ) |>
    janitor::clean_names()
}

# === Rutas / nombres ===
dir_path   <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/051_DANE_Proyecciones_P"
sheet_name <- "PobMunicipalxÁreaSexoEdad"

# Base A (2018–2042)
base_a <- "PPED-AreaSexoEdadMun-2018-2042_VP"
df_a   <- cargar_rango_J_HC(dir_path, base_a, sheet_name, header_row = 9, from_col = "J", to_col = "HC")

fusion_sin_id <- dplyr::bind_cols(id_2018_2042,df_a)

##### Ahora tengo que hacer un reshape de wide a long
fusion_long <- fusion_sin_id %>%
  # 1) Nombres limpios
  janitor::clean_names() %>%
  # 2) Normalización de sufijos:
  #    "..._<edad>_ano(s)_y_mas" -> "..._<edad>_y_mas"
  dplyr::rename_with(~ gsub("_(\\d+)_(?:ano|anos)_y_mas$", "_\\1_y_mas", .x, perl = TRUE)) %>%
  #    "..._<edad>_ano" o "..._<edad>_anos" -> "..._<edad>"
  dplyr::rename_with(~ gsub("_(\\d+)_(?:ano|anos)$",       "_\\1",       .x, perl = TRUE)) %>%
  dplyr::rename_with(~ gsub("__+", "_", .x)) %>%
  # 3) Identificadores
  dplyr::mutate(
    dp   = stringr::str_pad(as.character(.data$dp),   2, pad = "0"),
    dpmp = stringr::str_pad(as.character(.data$dpmp), 5, pad = "0"),
    anio = suppressWarnings(as.integer(.data$ano))
  ) %>%
  # 4) Wide -> Long (cubre ..._0..99 y ..._100_y_mas)
  tidyr::pivot_longer(
    cols = tidyselect::matches("^(hombres|mujeres|total)_(\\d+|\\d+_y_mas)$"),
    names_to = c("sexo", "edad_raw"),
    names_pattern = "^(hombres|mujeres|total)_(\\d+|\\d+_y_mas)$",
    values_to = "poblacion"
  ) %>%
  dplyr::mutate(
    edad = as.integer(gsub("_y_mas$", "", .data$edad_raw)),   # "100_y_mas"->100
    edad_colapsada = dplyr::if_else(.data$edad >= 85, 85L, .data$edad),
    edad_grupo     = dplyr::if_else(.data$edad >= 85, "85_y_mas", "edad_simple"),
    poblacion      = suppressWarnings(as.numeric(.data$poblacion))
  ) %>%
  dplyr::select(-.data$edad_raw) %>%
  dplyr::rename(
    dpto_cod = dp,  dpto_nom = dpnom,
    muni_cod = mpio, muni_nom = dpmp,
    area_geo = area_geografica
  ) %>%
  dplyr::select(
    dpto_cod, dpto_nom, muni_cod, muni_nom, anio, area_geo,
    sexo, edad, edad_colapsada, edad_grupo, poblacion, dplyr::everything()
  ) %>%
  dplyr::filter(stringr::str_to_lower(stringr::str_trim(area_geo)) != "total") %>% drop_na()

### Homogeanizacion de bases de datos

df_long_1985_2017 <- df_long_1985_2017[,c(1:8,10,11)]
fusion_long <- fusion_long[,c(1:7,9,11,12)]
fusion_long <- fusion_long %>%
  rename(
    edad = edad_colapsada
  )
fusion_long <- fusion_long %>% group_by(dpto_cod,dpto_nom,muni_cod,muni_nom,anio,area_geo,sexo,edad,ano) %>% summarise(poblacion=sum(poblacion))
lkp_muni <- df_long_1985_2017 %>% distinct(muni_cod, muni_nom)  # asume unicidad

fusion_long <- fusion_long %>%
  dplyr::select(-muni_nom) %>%
  left_join(lkp_muni, by = "muni_cod")

fusion_long <- fusion_long[,c(2:4,11,5:8,10,9)]
names(fusion_long)[4] <- "muni_nom"


#### Ahora vamos a fusionar estas bases de datos
poblacion_proyecciones <- bind_rows( fusion_long,df_long_1985_2017)


#### DANE - Proyecciones de Población
#-----------------------------------------#
poblacion_proyecciones <- poblacion_proyecciones %>%
  ungroup() %>% filter(sexo!="total") %>%
  dplyr::select(-5)

normalize_code <- function(x, width) {
  x_chr <- as.character(x)
  x_chr <- str_replace_all(x_chr, "[^0-9]", "")
  x_chr <- ifelse(x_chr == "", NA_character_, x_chr)
  str_pad(x_chr, width = width, pad = "0")
}

# =========================
# Transformación de datos de black a silver
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


#### Transformación de datos de Black a Silver
#### Merge de bases 
poblacion_proyecciones <- merge(poblacion_proyecciones,territorio_ref, by.x = "muni_cod",by.y = "COD_DANE_MUNIC_D",all.x = T,all.y = T)
poblacion_proyecciones <- poblacion_proyecciones %>%
  mutate(muni_cod = str_squish(as.character(muni_cod))) 

poblacion_proyecciones <- poblacion_proyecciones %>%
  filter(!str_to_lower(muni_cod) %in% c("00une", "00lza"))


# renombrar una o varias columnas
poblacion_proyecciones <- poblacion_proyecciones  %>%
  rename(
    COD_DANE_MUNIC_D = muni_cod,
    MUNICIPIO_D   = MUNICIPIO_D_REF,
    COD_DANE_DPTO_D  = dpto_cod,
    DEPARTAMENTO_D = DEPARTAMENTO_D_REF
  )


# Modificacion de periodicidad (Generación de variables adicionales de tiempo)

poblacion_proyecciones <- poblacion_proyecciones %>%
  mutate(
    ano = suppressWarnings(as.integer(ano)),
    mes = 1L,
    dia = 1L,
    fecha_completa = ymd(sprintf("%04d-%02d-%02d", ano, mes, dia)),
    trimestre      = sprintf("%04d-T1", ano),
    semestre       = sprintf("%04d-S1", ano))


saveRDS(poblacion_proyecciones,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/051_DANE_Proyecciones_P.rds", compress = "gzip")

### Golden
poblacion_proyecciones <- poblacion_proyecciones %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,
                                                                   DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D,area_geo,sexo,edad,poblacion)


poblacion_proyecciones_total <- poblacion_proyecciones %>% group_by(ano,COD_DANE_DPTO_D,COD_DANE_MUNIC_D,DEPARTAMENTO_D,MUNICIPIO_D) %>% summarise(poblacion = sum(poblacion))


saveRDS(poblacion_proyecciones,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/051_DANE_Proyecciones_P.rds", compress = "gzip")
saveRDS(poblacion_proyecciones_total,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/051_DANE_Proyecciones_P_total.rds", compress = "gzip")

saveRDS(poblacion_proyecciones,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/DANE_POPULATION/data/051_DANE_Proyecciones_P.rds", compress = "gzip")
saveRDS(poblacion_proyecciones_total,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/DNP-IDM/data/051_DANE_Proyecciones_P_total.rds", compress = "gzip")
saveRDS(poblacion_proyecciones_total,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/NDA_APP/data/051_DANE_Proyecciones_P_total.rds", compress = "gzip")
saveRDS(poblacion_proyecciones_total,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/SIVIGILA_BPAN/data/051_DANE_Proyecciones_P_total.rds", compress = "gzip")
saveRDS(poblacion_proyecciones_total,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/SIVIGILA_ETA/data/051_DANE_Proyecciones_P_total.rds", compress = "gzip")
saveRDS(poblacion_proyecciones_total,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/UPRA_APADT/data/051_DANE_Proyecciones_P_total.rds", compress = "gzip")
saveRDS(poblacion_proyecciones_total,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/UPRA_FA/data/051_DANE_Proyecciones_P_total.rds", compress = "gzip")






