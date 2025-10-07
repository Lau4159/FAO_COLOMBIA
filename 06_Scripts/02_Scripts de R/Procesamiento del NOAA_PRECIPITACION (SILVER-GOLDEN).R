####################################################################################################
#### Proyecto de sistema de monitoreamiento de sistemas agroalimentarios (Procesamiento de datos)  #
#### Departamentos priorizados: Barranquilla (Atlántico) y Bucaramanga (Santander)                 #
#### Autor de script: Diego Andrés Cardoso López                                                   #
####################################################################################################

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

# --- Paquetes (asegurar instalación) ---
pkgs <- c(
  "tidyverse","ggplot2","readxl","tidyr","dplyr","forecast","data.table","rugarch","strucchange","dynlm",
  "coefplot","modelsummary","mapview","scales","tseries","zoo","janitor","lubridate","openxlsx","tidyquant",
  "car","quantmod","MSwM","arm","broom","nonnest2","R.utils","Ecdat","vars","MASS","urca","tsDyn","haven",
  "tsutils","dyn","mFilter","anytime","astsa","xts","foreign","timsac","lmtest","sf","fs","raster","exactextractr"
  # OJO: si querías 'viridis' (no 'vidiris'), agrégalo también
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

#-------------------------------------------------------------------------------------------------#
# 0) Rutas y datos vectoriales
#-------------------------------------------------------------------------------------------------#
ruta_base <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/131_NOAA_Precipitación/"
dir_chirps <- file.path(ruta_base, "CHIRPS")
fs::dir_create(dir_chirps)

# Cargar shapefile municipal (MAGNA-SIRGAS) y llevar a WGS84 (EPSG:4326) para alinear con CHIRPS
munic_colombia <- sf::st_read(
  "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/091_SHP_MGN2018_INTGRD_MPIO/MGN_ANM_MPIOS.shp",
  quiet = FALSE
)
# Asegurar CRS a 4326
if (sf::st_crs(munic_colombia)$epsg != 4326) {
  munic_colombia <- sf::st_transform(munic_colombia, 4326)
}

#-------------------------------------------------------------------------------------------------#
# 1) Descarga y descompresión de CHIRPS mensual (global 0.05º)
#    Rango de años/meses ajustable
#-------------------------------------------------------------------------------------------------#
base_url <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs"
anios <- 2012:2025
meses <- 1:12

for (yy in anios) {
  for (mm in meses) {
    file_name <- sprintf("chirps-v2.0.%d.%02d.tif", yy, mm)
    gz_file   <- paste0(file_name, ".gz")
    url       <- paste0(base_url, "/", gz_file)
    dest_gz   <- file.path(dir_chirps, gz_file)
    dest_tif  <- file.path(dir_chirps, file_name)
    
    # Saltar si ya existe el .tif descomprimido
    if (file.exists(dest_tif)) next
    
    # Intentar descargar .gz y descomprimir
    tryCatch({
      utils::download.file(url, destfile = dest_gz, mode = "wb", quiet = TRUE)
      # Descomprimir con R.utils::gunzip
      R.utils::gunzip(dest_gz, destname = dest_tif, remove = FALSE, overwrite = TRUE)
      message("OK: ", file_name)
    }, error = function(e) {
      message("No disponible o error con: ", url, " — ", conditionMessage(e))
      # Si falla, borrar residuo .gz corrupto si existe
      if (file.exists(dest_gz)) unlink(dest_gz)
    })
  }
}

#-------------------------------------------------------------------------------------------------#
# 2) Extracción: precipitación promedio por municipio (promedio espacial del raster dentro del polígono)
#    exactextractr::exact_extract calcula estadísticas zonales precisas (maneja celdas parciales).
#-------------------------------------------------------------------------------------------------#
archivos_chirps <- list.files(dir_chirps, pattern = "\\.tif$", full.names = TRUE)
if (!length(archivos_chirps)) stop("No se encontraron .tif en: ", dir_chirps)

# Ordenar por nombre para reproducibilidad
archivos_chirps <- sort(archivos_chirps)

# Función auxiliar para parsear fecha desde el nombre: chirps-v2.0.YYYY.MM.tif
parse_fecha <- function(path) {
  nm <- basename(path)
  # posición: "chirps-v2.0." (12 chars) -> año en 13:16 ; mes en 18:19
  yy <- as.integer(substr(nm, 13, 16))
  mm <- as.integer(substr(nm, 18, 19))
  list(yy = yy, mm = mm, date = lubridate::make_date(yy, mm, 1))
}

# IMPORTANTE: raster::raster espera CRS long/lat; ya transformamos munic a 4326
# exactextractr funciona con RasterLayer directamente
resultados_col <- purrr::map_dfr(archivos_chirps, function(archivo) {
  # Parseo de fecha
  info <- parse_fecha(archivo)
  yy <- info$yy; mm <- info$mm; fecha <- info$date
  
  # Cargar raster CHIRPS mensual
  r <- raster::raster(archivo)
  
  # Asegurarnos de que el raster esté en lon/lat 4326 (CHIRPS lo está; esto es defensivo)
  # if (!is.na(raster::crs(r)) && !grepl("4326", raster::crs(r))) {
  #   # En general no hace falta; reprojectar global CHIRPS es costoso. Mejor mantener 4326.
  # }
  
  # Estadística zonal (promedio) por polígono municipal
  # exactextractr retorna un vector con la estadística por geometría en el orden de 'munic_colombia'
  vals <- exactextractr::exact_extract(r, munic_colombia, 'mean')
  
  tibble::tibble(
    COD_DANE_DPTO_D  = munic_colombia$DPTO_CCDGO,
    COD_DANE_MUNIC_D = munic_colombia$MPIO_CDPMP,
    ano   = yy,
    month = mm,
    fecha = fecha,
    precip_mm = vals
  )
})

#####################################################
## Transformación de datos de BLACK - SILVER

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
# 2b) Armar calendario y corregir códigos + merge con territorio_ref
# =========================
resultados_col <- resultados_col %>%
  # Normaliza y cero-rellena códigos que vienen del shapefile
  mutate(
    COD_DANE_DPTO_D  = normalize_code(COD_DANE_DPTO_D, 2),
    COD_DANE_MUNIC_D = normalize_code(COD_DANE_MUNIC_D, 5)
  ) %>%
  # Asegurar 'fecha' mensual y derivar calendario completo
  mutate(
    # 'fecha' ya venía de parse_fecha(), igual la reforzamos:
    fecha          = as.Date(lubridate::make_date(ano, month, 1)),
    fecha_completa = fecha,
    ano            = lubridate::year(fecha),
    mes            = lubridate::month(fecha),
    dia            = lubridate::day(fecha),              # será 1 (primer día de mes)
    semana         = as.integer(strftime(fecha, "%V")),  # semana ISO del 1er día del mes
    trimestre      = sprintf("%04d-T%d", ano, lubridate::quarter(fecha)),
    semestre       = sprintf("%04d-S%d", ano, ifelse(lubridate::quarter(fecha) <= 2, 1, 2)),
    
    # Inicializa nombres (se llenan con DIVIPOLA)
    DEPARTAMENTO_D = NA_character_,
    MUNICIPIO_D    = NA_character_,
    
    # ORIGEN (placeholders, si los necesitas)
    COD_DANE_DPTO_O  = NA_character_,
    DEPARTAMENTO_O   = NA_character_,
    COD_DANE_MUNIC_O = NA_character_,
    MUNICIPIO_O      = NA_character_
  ) %>%
  # Reordena columnas de tiempo y territorio al inicio
  relocate(
    fecha_completa, ano, mes, semana, dia, trimestre, semestre,
    COD_DANE_DPTO_D, DEPARTAMENTO_D, COD_DANE_MUNIC_D, MUNICIPIO_D,
    .before = 1
  ) %>%
  # Nombres oficiales por código municipal (DIVIPOLA)
  left_join(territorio_ref, by = "COD_DANE_MUNIC_D") %>%
  mutate(
    MUNICIPIO_D    = dplyr::coalesce(MUNICIPIO_D, MUNICIPIO_D_REF),
    DEPARTAMENTO_D = dplyr::coalesce(DEPARTAMENTO_D, DEPARTAMENTO_D_REF)
  ) %>%
  dplyr::select(-MUNICIPIO_D_REF, -DEPARTAMENTO_D_REF)

saveRDS(resultados_col,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/131_NOAA_Precipitación.rds", compress = "gzip")

### Golden
resultados_col <- resultados_col %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,
                                                             DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D, precip_mm)

saveRDS(resultados_col,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/131_NOAA_Precipitación.rds", compress = "gzip")
saveRDS(resultados_col,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/NOAA_PRECIPITATION/data/131_NOAA_Precipitación.rds", compress = "gzip")

