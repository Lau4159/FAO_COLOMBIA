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


###---------------------------------------------------------------------------------------------###
### Procesamiento de infromación de UPRA Hectareas Potenciales de riego

# 1) Ruta al shapefile (.shp) de Frontera Agropecuaria
#    (debe existir el .shp junto con .dbf, .shx, .prj, etc.)
ruta_shp <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/014_UPRA_APADT/APADT_Nov2018.shp"  # <-- cámbiala dependiendo de como tengas el almacenamiento

# 2) Leer el shapefile
fa_sf_APADT <- st_read(ruta_shp, quiet = TRUE)

# 3) Obtener la tabla de atributos (sin geometría)
fa_atrib_APADT <- fa_sf_APADT |>
  st_drop_geometry() |>
  clean_names()       # opcional: nombres en snake_case

# 4) Echar un vistazo
glimpse(fa_atrib_APADT)
head(fa_atrib_APADT)

# Leer shapefile
fa_sf_APADT <- st_read(ruta_shp, quiet = TRUE) |>
  st_make_valid() |>
  st_zm(drop = TRUE, what = "ZM")

mapview(fa_sf_APADT)
windows(width=10, height=7)
plot(st_geometry(fa_sf_APADT))

ruta_APADT   <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/014_UPRA_APADT/APADT_Nov2018.shp"
ruta_mpio <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/091_SHP_MGN2018_INTGRD_MPIO/MGN_ANM_MPIOS.shp"

# ==== Lectura y saneo básico ====
APADT   <- st_read(ruta_APADT,   quiet = TRUE) |> st_make_valid() |> st_zm(drop = TRUE, what = "ZM")
mpio <- st_read(ruta_mpio, quiet = TRUE) |> st_make_valid() |> st_zm(drop = TRUE, what = "ZM")

# Proyección de área (metros) - Colombia
APADT3116   <- st_transform(APADT,   3116)   # MAGNA-SIRGAS / Colombia Bogota
mpio3116 <- st_transform(mpio, 3116)

# ==== Selecciona columnas de ID/nombres del MGN ====
# Cambia estos nombres si en tu archivo difieren
COD_MPIO <- "MPIO_CDPMP"
NOM_MPIO <- "MPIO_CNMBR"
NOM_DPTO <- "DPTO_CCDGO"

mpio3116 <- mpio3116 |> dplyr::select(COD_MPIO = all_of(COD_MPIO),
                                      NOM_MPIO = all_of(NOM_MPIO),
                                      NOM_DPTO = all_of(NOM_DPTO))

# ==== Área total por municipio (m² y ha) ====
area_mpio <- mpio3116 |>
  mutate(area_mpio_m2 = as.numeric(st_area(geometry)),
         area_mpio_ha = area_mpio_m2 / 10000) |>
  st_drop_geometry() |>
  dplyr::select(COD_MPIO, NOM_DPTO, NOM_MPIO, area_mpio_m2, area_mpio_ha)

# ==== Intersección APADT ∩ municipio y suma de áreas ====
# (partimos cada municipio por los polígonos APADT y sumamos el área resultante)
APADT_mpio <- st_intersection(mpio3116, APADT3116 |> dplyr::select(geometry))

area_APADT <- APADT_mpio |>
  mutate(area_APADT_m2 = as.numeric(st_area(geometry))) |>
  st_drop_geometry() |>
  group_by(COD_MPIO, NOM_DPTO, NOM_MPIO) |>
  summarise(area_APADT_m2 = sum(area_APADT_m2), .groups = "drop") |>
  mutate(area_APADT_ha = area_APADT_m2 / 10000)

# ==== Proporción APADT / área municipal ====
APADT_proporcion_munip <- area_mpio |>
  left_join(area_APADT, by = c("COD_MPIO","NOM_DPTO","NOM_MPIO")) |>
  mutate(area_APADT_m2 = coalesce(area_APADT_m2, 0),
         area_APADT_ha = coalesce(area_APADT_ha, 0),
         prop_APADT = area_APADT_m2 / area_mpio_m2) |>
  arrange(desc(prop_APADT))

# Vista rápida
head(resumen, 10)

# ==== (Opcional) mapa coroplético de la proporción ====
library(ggplot2)
mpio_prop <- mpio3116 |>
  left_join(APADT_proporcion_munip |> dplyr::select(COD_MPIO, prop_APADT), by = "COD_MPIO")

ggplot(mpio_prop) +
  geom_sf(aes(fill = prop_APADT), color = "gray30", linewidth = 0.1) +
  scale_fill_continuous(name = "APADT / Área municipal",
                        labels = scales::percent_format(accuracy = 1),
                        na.value = "white") +
  labs(title = "Proporción de Frontera Agropecuaria por municipio",
       caption = "Fuente: UPRA; MGN 2018 (IGAC). Área en EPSG:3116") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), axis.title = element_blank())

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

####-------------------------------------------------
#### Transformación de datos de black a silver
#### Proporcion de las hectáreas potenciales de riego
APADT_proporcion_munip <- merge(APADT_proporcion_munip,territorio_ref, by.x = "COD_MPIO",by.y = "COD_DANE_MUNIC_D",all.x = T,all.y = T)

# renombrar una o varias columnas
APADT_proporcion_munip <- APADT_proporcion_munip  %>%
  rename(
    MUNICIPIO_D   = MUNICIPIO_D_REF,
    COD_DANE_MUNIC_D = COD_MPIO,
    COD_DANE_DPTO_D = NOM_DPTO,
    DEPARTAMENTO_D = DEPARTAMENTO_D_REF
  )

APADT_proporcion_munip <- APADT_proporcion_munip %>%
  mutate(
    # fijar fecha a NOV de 2018
    ano = 2018L,
    mes = 11L,
    dia = 1L,
    
    # fecha y derivados
    fecha_completa = ymd("2024-04-01"),
    trimestre      = sprintf("%04d-T%d", ano, ((mes - 1L) %/% 3L) + 1L),
    semestre       = sprintf("%04d-S%d", ano, if_else(mes <= 6L, 1L, 2L))
  )

saveRDS(APADT_proporcion_munip,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/014_UPRA_APADT.rds", compress = "gzip")

### Golden
APADT_proporcion_munip <- APADT_proporcion_munip %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D,
                                                                   area_mpio_ha, area_APADT_ha, prop_APADT)

saveRDS(APADT_proporcion_munip,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/014_UPRA_APADT.rds", compress = "gzip")
saveRDS(APADT_proporcion_munip,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/UPRA_APADT/data/014_UPRA_APADT.rds", compress = "gzip")
