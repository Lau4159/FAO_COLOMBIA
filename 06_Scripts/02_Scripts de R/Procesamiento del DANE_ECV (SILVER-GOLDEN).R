####################################################################################################
#### Proyecto de sistema de monitoreamiento de sistemas agroalimentarios (Procesamiento de datos)  #
#### Departamentos priorizados: Barranquilla (Atlántico) y Bucaramanga (Santander)                 #
#### Autor de script: Diego Andrés Cardoso López                                               #####
####################################################################################################

rm(list=ls())
paquetes = c('tidyverse','ggplot2','readxl','tidyr','dplyr','forecast','data.table','rugarch','strucchange', 'dynlm','coefplot','modelsummary', 'mapview',
             'scales','tseries','zoo','janitor', 'lubridate', 'openxlsx', 'tidyquant','car','EventStudy', 'quantmod','MSwM', 'arm', 'broom','nonnest2',
             'Ecdat', 'vars','MASS','urca','tsDyn','haven','tsutils','dyn','mFilter','anytime','vidiris','astsa','xts','foreign','timsac','lmtest','sf')
sapply(paquetes, require, character.only=T)

#-------------------------------------------------------------------------#
### Procesamiento de información de la Encuesta de Calidad de Vida (ECV)
#-------------------------------------------------------------------------#

base_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/052_DANE_ECV"

# ========================
# Año 2021
# ========================
ruta2021 <- file.path(base_dir, "2021-ECV")

hogar2021    <- as.data.frame(read_dta(file.path(ruta2021, "Caracteristicas y composicion del hogar.dta")))
names(hogar2021) <- tolower(names(hogar2021))

vivienda2021 <- as.data.frame(read_dta(file.path(ruta2021, "Datos de la vivienda.dta")))
names(vivienda2021) <- tolower(names(vivienda2021))

municipio2021 <- as.data.frame(read_dta(file.path(ruta2021, "Municipio de aplicación de la encuesta.dta")))
names(municipio2021) <- tolower(names(municipio2021))

salud2021    <- as.data.frame(read_dta(file.path(ruta2021, "Salud.dta")))
names(salud2021) <- tolower(names(salud2021))

ecv2021_hs <- left_join(hogar2021, salud2021,
                        by = c("directorio","secuencia_p","orden","fex_c"))
ecv2021 <- left_join(ecv2021_hs, vivienda2021,
                     by = c("directorio","secuencia_p","fex_c"))
ecv2021 <- left_join(ecv2021, municipio2021,
                     by = c("directorio"))

ecv2021 <- ecv2021 %>% mutate(anio=2021L)


# ========================
# Año 2022
# ========================
ruta2022 <- file.path(base_dir, "2022-ECV")

hogar2022    <- as.data.frame(read_dta(file.path(ruta2022, "Caracteristicas y composicion del hogar.dta")))
names(hogar2022) <- tolower(names(hogar2022))

vivienda2022 <- as.data.frame(read_dta(file.path(ruta2022, "Datos de la vivienda.dta")))
names(vivienda2022) <- tolower(names(vivienda2022))

municipio2022 <- as.data.frame(read_dta(file.path(ruta2022, "Municipio de aplicación de la encuesta.dta")))
names(municipio2022) <- tolower(names(municipio2022))

salud2022    <- as.data.frame(read_dta(file.path(ruta2022, "Salud.dta")))
names(salud2022) <- tolower(names(salud2022))

ecv2022_hs <- left_join(hogar2022, salud2022,
                        by = c("directorio","secuencia_p","orden","fex_c"))
ecv2022 <- left_join(ecv2022_hs, vivienda2022,
                     by = c("directorio","secuencia_p","fex_c"))
ecv2022 <- left_join(ecv2022, municipio2022,
                     by = c("directorio"))

ecv2022 <- ecv2022 %>% mutate(anio=2022L)
# ========================
# Año 2023
# ========================
ruta2023 <- file.path(base_dir, "2023-ECV")

hogar2023    <- as.data.frame(read_dta(file.path(ruta2023, "Caracteristicas y composicion del hogar.dta")))
names(hogar2023) <- tolower(names(hogar2023))

vivienda2023 <- as.data.frame(read_dta(file.path(ruta2023, "Datos de la vivienda.dta")))
names(vivienda2023) <- tolower(names(vivienda2023))

salud2023    <- as.data.frame(read_dta(file.path(ruta2023, "Salud.dta")))
names(salud2023) <- tolower(names(salud2023))

ecv2023_hs <- left_join(hogar2023, salud2023,
                        by = c("directorio","secuencia_p","orden","fex_c"))
ecv2023 <- left_join(ecv2023_hs, vivienda2023,
                     by = c("directorio","secuencia_p","fex_c"))
ecv2023 <- ecv2023 %>% mutate(anio=2023L)

# ========================
# Año 2024
# ========================
ruta2024 <- file.path(base_dir, "2024-ECV")

hogar2024    <- as.data.frame(read_dta(file.path(ruta2024, "Caracteristicas y composicion del hogar.dta")))
names(hogar2024) <- tolower(names(hogar2024))

vivienda2024 <- as.data.frame(read_dta(file.path(ruta2024, "Datos de la vivienda.dta")))
names(vivienda2024) <- tolower(names(vivienda2024))

salud2024    <- as.data.frame(read_dta(file.path(ruta2024, "Salud.dta")))
names(salud2024) <- tolower(names(salud2024))

ecv2024_hs <- left_join(hogar2024, salud2024,
                        by = c("directorio","secuencia_p","orden","fex_c"))
ecv2024 <- left_join(ecv2024_hs, vivienda2024,
                     by = c("directorio","secuencia_p","fex_c"))
ecv2024 <- ecv2024 %>% mutate(anio=2024L)

ecv <- bind_rows(ecv2021,ecv2022,ecv2023,ecv2024)

ecv <- ecv %>% dplyr::select(directorio, secuencia_p, orden.x, fex_c, anio, p1_departamento, clase, p1_municipio,
                             p6020, p6040, p1707, p1707s1, p3003, p3003s1) %>% rename(sexo=p6020,
                                                                                      edad = p6040)

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


ecv <- ecv %>% mutate(muni_code = paste(p1_departamento, p1_municipio, sep = ""))

ecv_indicadores_all <- ecv %>%
  mutate(
    # --- Tiempo (derivado solo de 'anio') ---
    ano            = as.integer(anio),
    fecha_completa = as.Date(sprintf("%04d-01-01", ano)),
    mes            = 1L,
    semana         = 1L,
    dia            = 1L,
    trimestre      = sprintf("%04d-T1", ano),
    semestre       = sprintf("%04d-S1", ano),
    
    # --- Códigos DANE desde ECV ---
    COD_DANE_DPTO_D  = normalize_code(p1_departamento, 2),
    COD_DANE_MUNIC_D = ifelse(
      is.na(p1_departamento) | is.na(p1_municipio),
      NA_character_,
      paste0(normalize_code(p1_departamento, 2),
             stringr::str_pad(p1_municipio, width = 3, pad = "0"))
    ),
    
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
    fecha_completa, ano, mes, semana, dia, trimestre, semestre,
    COD_DANE_DPTO_D, DEPARTAMENTO_D, COD_DANE_MUNIC_D, MUNICIPIO_D,
    .before = 1
  ) %>%
  # Nombres oficiales por código municipal
  left_join(territorio_ref, by = "COD_DANE_MUNIC_D") %>%
  mutate(
    MUNICIPIO_D    = dplyr::coalesce(MUNICIPIO_D, MUNICIPIO_D_REF),
    DEPARTAMENTO_D = dplyr::coalesce(DEPARTAMENTO_D, DEPARTAMENTO_D_REF)
  ) %>%
  dplyr::select(-MUNICIPIO_D_REF, -DEPARTAMENTO_D_REF)


saveRDS(ecv_indicadores_all,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/052_DANE_ECV.rds", compress = "gzip")

### Golden
ecv_indicadores_all <- ecv_indicadores_all %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,
                                                             DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D, directorio, secuencia_p,
                                                             orden.x, fex_c, clase, sexo, edad, p1707,p1707s1,p3003,p3003s1) %>% mutate(clase = ifelse(clase==1,"Cabecera municipal","Centros Poblados y Rural Disperso"))

saveRDS(ecv_indicadores_all,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/052_DANE_ECV.rds", compress = "gzip")
saveRDS(ecv_indicadores_all,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/DANE_ECV/data/052_DANE_ECV.rds", compress = "gzip")


