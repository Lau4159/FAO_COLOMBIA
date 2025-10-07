####################################################################################################
#### Proyecto de sistema de monitoreamiento de sistemas agroalimentarios (Procesamiento de datos)  #
#### Departamentos priorizados: Barranquilla (Atlántico) y Bucaramanga (Santander)                 #
#### Autor de script: Diego Andrés Cardoso López                                               #####
####################################################################################################

#### Procesamiento de la información del ICA_Censo Pecuario - Bovino

rm(list=ls())
paquetes = c('tidyverse','ggplot2','readxl','tidyr','dplyr','forecast','data.table','rugarch','strucchange', 'dynlm','coefplot','modelsummary', 'mapview',
             'scales','tseries','zoo','janitor', 'lubridate', 'openxlsx', 'tidyquant','car','EventStudy', 'quantmod','MSwM', 'arm', 'broom','nonnest2',
             'Ecdat', 'vars','MASS','urca','tsDyn','haven','tsutils','dyn','mFilter','anytime','vidiris','astsa','xts','foreign','timsac','lmtest','sf')
# paquetes_faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
# if(length(paquetes_faltantes)) install.packages(paquetes_faltantes)
sapply(paquetes,require,character.only=T)

###-------------------------------------------------------------------------###
### ICA_Censo Pecuario - Bovino

###### Inventario de BOVINO, cuando haya mas disponibilidad de datos lo puedes cargar acá (Únicamente es cambiar la dirección de guardado de :
ica_pecuaria_2020 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/101_ICA_CensoPecuario-Bovino/BOVINOS-CENSO-2020.xlsx",
                                sheet = "BOVINOS Y PREDIOS", skip = 4) |>
  clean_names() |>
  mutate(ano = 2020) 

ica_pecuaria_2020 <- ica_pecuaria_2020 %>% rename(total_bovinos = total_bovinos_2019)

ica_pecuaria_2021 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/101_ICA_CensoPecuario-Bovino/CENSO-BOVINO-2021.xlsx",
                                sheet = "BOVINOS Y PREDIOS", skip = 4) |>
  clean_names() |>
  mutate(ano = 2021) 

ica_pecuaria_2022 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/101_ICA_CensoPecuario-Bovino/CENSOS-BOVINOS-2022.xlsx",
                                sheet = "BOVINOS Y PREDIOS", skip = 4) |>
  clean_names() |>
  mutate(ano = 2022)

ica_pecuaria_2023 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/101_ICA_CensoPecuario-Bovino/CENSOS-BOVINOS-2023-Final.xls",
                                sheet = "BOVINOS Y PREDIOS", skip = 4) |>
  janitor::clean_names() |>
  mutate(ano = 2023) |>
  dplyr::select(-(18:256))

ica_pecuaria_2024 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/101_ICA_CensoPecuario-Bovino/CENSO-BOVINO-FINAL.xlsx",
                                sheet = "BOVINOS Y PREDIOS", skip = 6) |>
  clean_names() |>
  mutate(ano = 2024)

ica_pecuaria_2025 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/101_ICA_CensoPecuario-Bovino/CENSO-BOVINO-2025_14-5-25.xlsx",
                                sheet = "BOVINOS Y PREDIOS", skip = 1) |>
  clean_names() |>
  mutate(ano = 2025) |>
  dplyr::select(-(18:20))


# Eliminar la columna 'fincas_con_bovinos' sin usar select()
objs <- c("ica_pecuaria_2020","ica_pecuaria_2021",
          "ica_pecuaria_2022","ica_pecuaria_2023","ica_pecuaria_2024","ica_pecuaria_2025")

for (nm in objs) {
  if (exists(nm, inherits = FALSE)) {
    df <- get(nm, inherits = FALSE)
    drop_idx <- grepl("finca", names(df), ignore.case = TRUE)  # contiene "finca"
    if (any(drop_idx)) {
      message("Eliminadas en ", nm, ": ", paste(names(df)[drop_idx], collapse = ", "))
      df <- df[, !drop_idx, drop = FALSE]
    }
    assign(nm, df, envir = .GlobalEnv)
  }
}

ica_pecuaria_2021 <- ica_pecuaria_2021[, -(13), drop = FALSE]
ica_pecuaria_2022 <- ica_pecuaria_2022[, -(13), drop = FALSE]

# Ahora sí, unir
ica_pecuaria <- bind_rows(
  ica_pecuaria_2020, ica_pecuaria_2021,
  ica_pecuaria_2022, ica_pecuaria_2023, ica_pecuaria_2024, ica_pecuaria_2025
)

ica_pecuaria <- ica_pecuaria %>% drop_na()

### ICA_Censo Pecuario Bufalos
####### Procesamiento de información de Bufalo, Caprino, Ovino y Equino.

ica_pecuaria_BCOE_2020 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/103_ICA_CensoPecuario-Bufalo/BUFALOS-CENSO-2020.xlsx",
                                     sheet = "BUFALOS Y PREDIOS", skip = 4) |>
  clean_names() |>
  mutate(ano = 2020L)

ica_pecuaria_BCOE_2020[["predios_con_bufalos"]] <- NULL

ica_pecuaria_BCOE_2021 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/103_ICA_CensoPecuario-Bufalo/CENSO-BUFALO-202.xlsx",
                                     sheet = "BUFALOS Y PREDIOS", skip = 4) |>
  clean_names() |> dplyr::select(c(1:3, 12)) |>
  mutate(ano = 2021L)  

ica_pecuaria_BCOE_2022 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/103_ICA_CensoPecuario-Bufalo/CENSOS-BUFALOS-2022.xlsx",
                                     sheet = "BOVINOS Y PREDIOS", skip = 4) |>
  clean_names() |> dplyr::select(c(1:3, 12)) |>
  mutate(ano = 2022L)  

ica_pecuaria_BCOE_2023 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/103_ICA_CensoPecuario-Bufalo/CENSOS-BUFALOS-2023-Final.xls",
                                     sheet = "BUFALOS Y PREDIOS", skip = 4) |>
  clean_names() |> dplyr::select(c(1:3, 12)) |>
  mutate(ano = 2023L)

ica_pecuaria_BCOE_2024 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/103_ICA_CensoPecuario-Bufalo/CENSO-BUFALINO-FINAL.xlsx",
                                     sheet = "BUFALOS Y PREDIOS", skip = 7) |>
  clean_names() |> dplyr::select(c(1:3, 12)) |>
  mutate(ano = 2024L)  

ica_pecuaria_BCOE_2025 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/103_ICA_CensoPecuario-Bufalo/CENSOS-BUFALOS-2025.xlsx",
                                     sheet = "BUFALOS Y PREDIOS", skip = 5) |>
  clean_names() |> dplyr::select(c(1:3, 12)) |>
  mutate(ano = 2025L)

ica_pecuaria_BCOE_2023 <- ica_pecuaria_BCOE_2023 %>% rename(total_bufalos = total_bufalos_2022)

ica_pecuaria_BCOE <- bind_rows(
  ica_pecuaria_BCOE_2020,ica_pecuaria_BCOE_2021, ica_pecuaria_BCOE_2022,
  ica_pecuaria_BCOE_2023,ica_pecuaria_BCOE_2024,ica_pecuaria_BCOE_2025)

### ICA_Censo Pecuario EQUINO, CAPRINO, Ovino

ica_pecuaria_BCOE_2020_1 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/105_ICA_CensoPecuario-Caprino/Equinos-Caprinos-Ovinos-CENSOS-2020.xlsx",
                                       sheet = "EQUINOS_CAPRINOS_OVINOS", skip = 4) |>
  clean_names() |>
  mutate(ano = 2020L)

ica_pecuaria_BCOE_2021_1 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/105_ICA_CensoPecuario-Caprino/CENSOS-EQUINOS-OVINOS-CAPRINOS-2021.xlsx",
                                       sheet = "EQUINOS-CAPRINOS-OVINOS", skip = 4) |>
  clean_names() |>
  mutate(ano = 2021L)

ica_pecuaria_BCOE_2022_1 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/105_ICA_CensoPecuario-Caprino/CENSOS-OTRAS-ESPECIES-2022.xlsx",
                                       sheet = "EQUINOS-CAPRINOS-OVINOS", skip = 4) |>
  clean_names() |>
  mutate(ano = 2022L)

ica_pecuaria_BCOE_2023_1 <- read_excel(
  "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/105_ICA_CensoPecuario-Caprino/CENSOS-CAPRINOS-Y-OVINOS-2023-Final.xls",
  sheet = "CAPRINOS-OVINOS", skip = 4
) |>
  clean_names() |>
  dplyr::select(1:3, 6, 9) |>
  mutate(
    total_equinos = 0L,
    ano = 2023L,
    # convertir a texto y si tiene 4 dígitos, agregar 0 a la izquierda
    codigo_municipio = as.character(codigo_municipio),
    codigo_municipio = ifelse(
      nchar(codigo_municipio) == 4,
      str_pad(codigo_municipio, width = 5, pad = "0"),
      codigo_municipio
    )
  )

ica_pecuaria_BCOE_2024_1 <- read_excel(
  "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/105_ICA_CensoPecuario-Caprino/censo-poblacion-ovina-por-municipio-y-departamento-2024_.xlsx",
  sheet = "CAPRINOS-OVINOS", skip = 8
) |>
  clean_names() |>
  dplyr::select(1:3, 6, 9) |>
  mutate(
    total_equinos = 0L,
    ano = 2024L,
    # convertir a texto y si tiene 4 dígitos, agregar 0 a la izquierda
    cod= as.character(cod),
    cod = ifelse(
      nchar(cod) == 4,
      str_pad(cod, width = 5, pad = "0"),
      cod
    )
  )


ica_pecuaria_BCOE_2025_1 <- read_excel(
  "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/105_ICA_CensoPecuario-Caprino/CENSO-OVINOS-Y-CAPRINOS-2025.xlsx",
  sheet = "OVINOS Y CAPRINOS", skip = 1
) |>
  clean_names() |>
  dplyr::select(1:3, 6, 9) |>
  mutate(
    total_equinos = 0L,
    ano = 2025L,
    cod = as.character(cod),
    cod = ifelse(
      nchar(cod) == 4,
      str_pad(cod, width = 5, pad = "0"),
      cod
    )
  )


ica_pecuaria_BCOE_2024_1 <- ica_pecuaria_BCOE_2024_1 %>% rename(codigo_municipio = cod)
ica_pecuaria_BCOE_2025_1 <- ica_pecuaria_BCOE_2025_1 %>% rename(codigo_municipio = cod)

ica_pecuaria_BCOE_1 <- bind_rows(
  ica_pecuaria_BCOE_2020_1,ica_pecuaria_BCOE_2021_1, ica_pecuaria_BCOE_2022_1,
  ica_pecuaria_BCOE_2023_1,ica_pecuaria_BCOE_2024_1,ica_pecuaria_BCOE_2025_1)

ica_pecuaria_BCOE <- merge(ica_pecuaria_BCOE,ica_pecuaria_BCOE_1,by=(c("departamento","municipio","ano","codigo_municipio")))
ica_pecuaria_BCOE <- ica_pecuaria_BCOE %>% drop_na()

##### ICA_Censo Aviar

ica_pecuaria_aviar_2020 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/104_ICA_CensoPecuario-Aviar/AVES-CENSOS-2020.xlsx",
                                      sheet = "AVES Y PREDIOS", skip = 4) |>
  clean_names() |> dplyr::select(1:3, 16, 17,19,21) |> 
  mutate(ano = 2020L)

ica_pecuaria_aviar_2020 <- ica_pecuaria_aviar_2020 %>% rename(total_aves_capacidad_instalada = total_aves_capacidad_instalada_2019)
ica_pecuaria_aviar_2020 <- ica_pecuaria_aviar_2020  %>% rename(total_aves_capacidad_ocupada = total_aves_capacidad_ocupada_2019)
ica_pecuaria_aviar_2020 <- ica_pecuaria_aviar_2020 %>% rename(total_aves_traspatio = total_aves_traspatio_2019)
ica_pecuaria_aviar_2020 <- ica_pecuaria_aviar_2020  %>% rename(total_aves_capacidad_ocupada_mas_aves_traspatio = total_aves_capacidad_ocupada_mas_aves_traspatio_2019)


ica_pecuaria_aviar_2021 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/104_ICA_CensoPecuario-Aviar/CENSO-AVES-2021.xlsx",
                                      sheet = "AVES Y PREDIOS", skip = 4) |>
  clean_names() |> dplyr::select(1:3, 16, 17,19,21) |>
  mutate(ano = 2021L)

ica_pecuaria_aviar_2021 <- ica_pecuaria_aviar_2021 %>% rename(total_aves_capacidad_instalada = total_aves_capacidad_instalada_2021)
ica_pecuaria_aviar_2021 <- ica_pecuaria_aviar_2021  %>% rename(total_aves_capacidad_ocupada = total_aves_capacidad_ocupada_2021)
ica_pecuaria_aviar_2021 <- ica_pecuaria_aviar_2021 %>% rename(total_aves_traspatio = total_aves_traspatio_2021)
ica_pecuaria_aviar_2021 <- ica_pecuaria_aviar_2021  %>% rename(total_aves_capacidad_ocupada_mas_aves_traspatio = total_aves_capacidad_ocupada_mas_aves_traspatio_2021)


ica_pecuaria_aviar_2022 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/104_ICA_CensoPecuario-Aviar/CENSOS-AVES-2022.xlsx",
                                      sheet = "AVES Y PREDIOS", skip = 4) |>
  clean_names() |> dplyr::select(1:3, 16, 17,19,21) |>
  mutate(ano = 2022L)

ica_pecuaria_aviar_2023 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/104_ICA_CensoPecuario-Aviar/CENSOS-AVES-2023-Final.xls",
                                      sheet = "AVES_Y_PREDIOS", skip = 4) |>
  clean_names() |> dplyr::select(1:3, 16, 17,19,21) |>
  mutate(ano = 2023L)

ica_pecuaria_aviar_2024 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/104_ICA_CensoPecuario-Aviar/TABLA-DE-POBLACION-AVIAR-2024_final.xlsx",
                                      sheet = "AVES Y PREDIOS", skip = 8) |>
  clean_names() |> dplyr::select(1:3, 16, 17,19,21) |>
  mutate(ano = 2024L)

ica_pecuaria_aviar_2025 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/104_ICA_CensoPecuario-Aviar/CENSO-AVES-2025.xlsx",
                                      sheet = "AVES Y PREDIOS", skip = 1) |>
  clean_names() |> dplyr::select(1:3, 16, 17,19,21) |>
  mutate(ano = 2025L)

ica_pecuaria_aviar <- bind_rows(
  ica_pecuaria_aviar_2020,ica_pecuaria_aviar_2021, ica_pecuaria_aviar_2022,
  ica_pecuaria_aviar_2023,ica_pecuaria_aviar_2024,ica_pecuaria_aviar_2025)

ica_pecuaria_aviar <- ica_pecuaria_aviar %>% drop_na()

##### ICA_Censo Porcino
ica_pecuaria_PORCINO_2020 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/102_ICA_CensoPecuario-Porcino/PORCINOS-CENSOS-2020.xlsx",
                                        sheet = "PORCINOS Y PREDIOS", skip = 4) |>
  clean_names() |> dplyr::select(1:3, 20) |>
  mutate(ano = 2020L)

ica_pecuaria_PORCINO_2020 <- ica_pecuaria_PORCINO_2020 %>% rename(total_porcinos = total_porcinos_2019)


ica_pecuaria_PORCINO_2021 <- read_excel(
  "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/102_ICA_CensoPecuario-Porcino/CENSO-PORCINO-2021.xlsx",
  sheet = "PORCINOS_Y_PREDIOS", skip = 4
) |>
  clean_names() |>
  dplyr::select(1:3, 31) |>
  mutate(
    ano = 2021L,
    # a texto + limpia no-dígitos + si tiene 4 dígitos, agrega 0 a la izquierda
    codigo_municipio = as.character(codigo_municipio),
    codigo_municipio = str_replace_all(codigo_municipio, "[^0-9]", ""),
    codigo_municipio = ifelse(
      nchar(codigo_municipio) == 4,
      str_pad(codigo_municipio, width = 5, pad = "0"),
      codigo_municipio
    )
  ) |>
  rename(total_porcinos = total_porcinos_produccion_comercial_y_tecnificada)

ica_pecuaria_PORCINO_2022 <- read_excel(
  "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/102_ICA_CensoPecuario-Porcino/CENSO-PORCINOS-2022.xlsx",
  sheet = "PORCINOS_Y_PREDIOS", skip = 5
) |>
  clean_names() |>
  dplyr::select(1:3, 32) |>
  mutate(
    ano = 2022L,
    # a texto, limpia no-dígitos y si tiene 4 dígitos agrega 0 a la izquierda
    codigo_municipio = as.character(codigo_municipio),
    codigo_municipio = str_replace_all(codigo_municipio, "[^0-9]", ""),
    codigo_municipio = if_else(
      nchar(codigo_municipio) == 4,
      str_pad(codigo_municipio, width = 5, pad = "0"),
      codigo_municipio,
      missing = NA_character_
    )
  ) |>
  rename(total_porcinos = total_cerdos)

ica_pecuaria_porcino_2023 <- read_excel("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/012_UPRA_EVA-P/EVA_Pecuaria/Base_2019_2024.xlsx",
                                        sheet = "InvPorcino_20212024", skip = 3) |>
  clean_names() |>
  mutate(ano = as.integer(ano)) |>
  filter(ano == 2023)

# Quitar cualquier columna con “insumo” en el nombre (sin select)
ica_pecuaria_porcino_2023 <- ica_pecuaria_porcino_2023[, !grepl("insumo", names(ica_pecuaria_porcino_2023), ignore.case = TRUE)]

# Variables de identificación
id_vars <- c("codigo_dane_departamento","departamento",
             "codigo_dane_municipio","municipio","ano")

# Conversión a numérico segura para el resto
ica_pecuaria_porcino_2023 <- ica_pecuaria_porcino_2023 %>%
  mutate(across(setdiff(names(ica_pecuaria_porcino_2023), id_vars),
                ~ suppressWarnings(as.numeric(.))))

# Helper para sumar columnas opcionales
sum_pick <- function(...) sum(c_across(any_of(c(...))), na.rm = TRUE)

# Agregación por municipio–año
ica_pecuaria_porcino_2023 <- ica_pecuaria_porcino_2023 %>%
  group_by(across(all_of(id_vars))) %>%
  summarise(
    # Comercial familiar
    reproductores_familiar = sum_pick(
      "hembras_cria_comercial_familiar",
      "hembras_reemplazo_comercial_familiar",
      "machos_reproductores_comercial_familiar"
    ),
    crecimiento_familiar = sum_pick(
      "porcinos_lactantes_comercial_familiar","porcionos_lactantes_comercial_familiar",
      "porcinos_precebo_comercial_familiar","porcionos_precebo_comercial_familiar",
      "porcinos_levante_comercial_familiar","porcionos_levante_comercial_familiar",
      "porcinos_ceba_comercial_familiar","porcionos_ceba_comercial_familiar"
    ),
    predios_familiar = sum_pick(
      "total_predios_porcinos_comercial_familiar",
      "total_predios_porcionos_comercial_familiar"
    ),
    
    # Comercial industrial
    reproductores_industrial = sum_pick(
      "hembras_cria_comercial_industrial",
      "hembras_reemplazo_comercial_industrial",
      "machos_reproductores_comercial_industrial"
    ),
    crecimiento_industrial = sum_pick(
      "porcinos_lactantes_comercial_industrial","porcionos_lactantes_comercial_industrial",
      "porcinos_precebo_comercial_industrial","porcionos_precebo_comercial_industrial",
      "porcinos_levante_comercial_industrial","porcionos_levante_comercial_industrial",
      "porcinos_ceba_comercial_industrial","porcionos_ceba_comercial_industrial"
    ),
    predios_industrial = sum_pick(
      "total_predios_porcinos_comercial_industrial",
      "total_predios_porcionos_comercial_industrial"
    ),
    
    # Producción tecnificada
    reproductores_tecnificado = sum_pick(
      "hembras_cria_produccion_tecnificada",
      "hembras_reemplazo_produccion_tecnificada",
      "machos_reproductores_produccion_tecnificada"
    ),
    crecimiento_tecnificado = sum_pick(
      "porcinos_lactantes_produccion_tecnificada",
      "porcinos_precebo_produccion_tecnificada",
      "porcinos_levante_produccion_tecnificada",
      "porcinos_ceba_produccion_tecnificada"
    ),
    predios_tecnificado = sum_pick("total_predios_produccion_tecnificada"),
    
    # Traspatio
    reproductores_traspatio = sum_pick(
      "hembras_cria_traspatio",
      "hembras_reemplazo_traspatio",
      "machos_reproductores_traspatio"
    ),
    crecimiento_traspatio = sum_pick(
      "porcinos_lactantes_traspatio",
      "porcinos_precebo_traspatio",
      "porcinos_levante_traspatio",
      "porcinos_ceba_traspatio"
    ),
    predios_traspatio = sum_pick("total_predios_traspatio"),
    .groups = "drop_last"
  ) %>%
  # Totales por sistema y total general
  mutate(
    animales_familiar    = reproductores_familiar   + crecimiento_familiar,
    animales_industrial  = reproductores_industrial + crecimiento_industrial,
    animales_tecnificado = reproductores_tecnificado+ crecimiento_tecnificado,
    animales_traspatio   = reproductores_traspatio  + crecimiento_traspatio,
    reproductores_total  = rowSums(across(starts_with("reproductores_")), na.rm = TRUE),
    crecimiento_total    = rowSums(across(starts_with("crecimiento_")),   na.rm = TRUE),
    predios_total        = rowSums(across(starts_with("predios_")),       na.rm = TRUE),
    animales_total       = rowSums(across(starts_with("animales_")),      na.rm = TRUE),
    total_porcinos       = animales_total
  ) %>%
  ungroup()

# Dejar solo las columnas solicitadas
ica_pecuaria_porcino_2023 <- ica_pecuaria_porcino_2023[
  c("departamento",
    "municipio","codigo_dane_municipio",
    "ano","total_porcinos")
]

ica_pecuaria_PORCINO_2023 <- ica_pecuaria_porcino_2023 %>% rename(codigo_municipio = codigo_dane_municipio)

ica_pecuaria_PORCINO_2024 <- read_excel(
  "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/102_ICA_CensoPecuario-Porcino/CENSOS-PORCINOS-2024_FINAL.xlsx",
  sheet = "PORCINOS Y PREDIOS ",  # (ojo: trae espacio al final)
  skip = 2
) |>
  clean_names() |>
  dplyr::select(1:3, 32) |>
  mutate(
    ano = 2024L,
    codigo_municipio = as.character(codigo_municipio),
    codigo_municipio = str_replace_all(codigo_municipio, "[^0-9]", ""),
    codigo_municipio = if_else(
      nchar(codigo_municipio) == 4,
      str_pad(codigo_municipio, width = 5, pad = "0"),
      codigo_municipio,
      missing = NA_character_
    )
  ) |>
  rename(total_porcinos = total_cerdos)

ica_pecuaria_PORCINO_2025 <- read_excel(
  "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/102_ICA_CensoPecuario-Porcino/CENSOS-PORCINOS-2025.xlsx",
  sheet = "PORCINOS Y PREDIOS", skip = 2
) |>
  clean_names() |>
  dplyr::select(1:3, 32) |>
  mutate(
    ano = 2025L,
    codigo_municipio = as.character(codigo_municipio),
    codigo_municipio = str_replace_all(codigo_municipio, "[^0-9]", ""),
    codigo_municipio = if_else(
      nchar(codigo_municipio) == 4,
      str_pad(codigo_municipio, width = 5, pad = "0"),
      codigo_municipio,
      missing = NA_character_
    )
  ) |>
  rename(total_porcinos = total_cerdos)
ica_pecuaria_porcino <- bind_rows(
  ica_pecuaria_PORCINO_2020,ica_pecuaria_PORCINO_2021, ica_pecuaria_PORCINO_2022,
  ica_pecuaria_PORCINO_2023,ica_pecuaria_PORCINO_2024,ica_pecuaria_PORCINO_2025)

ica_pecuaria_porcino <- ica_pecuaria_porcino %>% drop_na()

####------------------------------------------------------------------------####
##### Procesamiento de información de BLACK a SILVER a GOLDEN
####------------------------------------------------------------------------####
##########------------------------------------------------------##########
#### ICA Pecuario (Información BCOE)

### Selección de información y cambio de nombres
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
# Procesamiento de BLACK a SILVER
# ================================
ica_pecuaria_BCOE <- ica_pecuaria_BCOE %>%
  mutate(
    # --- tiempo ---
    ano  = suppressWarnings(as.integer(ano)),
    mes  = 1L,
    dia  = 1L,
    fecha_completa = ymd(sprintf("%04d-%02d-%02d", ano, mes, dia)),
    trimestre      = sprintf("%04d-T%d", ano, ((mes - 1) %/% 3) + 1),
    semestre       = sprintf("%04d-S%d", ano, if_else(mes <= 6, 1L, 2L)),
    
    # --- DESTINO (desde tus columnas actuales) ---
    COD_DANE_DPTO_D = substr(normalize_code(codigo_municipio, 5), 1, 2),
    DEPARTAMENTO_D   = as.character(departamento),
    COD_DANE_MUNIC_D = normalize_code(codigo_municipio, 5),
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

saveRDS(ica_pecuaria_BCOE,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/103_ICA_CensoPecuario-BCOE.rds", compress = "gzip")


### Golden
ica_pecuaria_BCOE <- ica_pecuaria_BCOE %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,
                                                         DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D,total_bufalos,total_equinos,
                                                         total_caprinos,total_ovinos)

saveRDS(ica_pecuaria_BCOE,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/103_ICA_CensoPecuario-BCOE.rds", compress = "gzip")
saveRDS(ica_pecuaria_BCOE,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/ICA_P/data/103_ICA_CensoPecuario-BCOE.rds", compress = "gzip")


####### ICA Pecuaria - Aves
### Selección de información y cambio de nombres
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
# Transformación de datos de Black a Silver
# ================================
ica_pecuaria_aviar <- ica_pecuaria_aviar %>%
  mutate(
    # --- tiempo ---
    ano  = suppressWarnings(as.integer(ano)),
    mes  = 1L,
    dia  = 1L,
    fecha_completa = ymd(sprintf("%04d-%02d-%02d", ano, mes, dia)),
    trimestre      = sprintf("%04d-T%d", ano, ((mes - 1) %/% 3) + 1),
    semestre       = sprintf("%04d-S%d", ano, if_else(mes <= 6, 1L, 2L)),
    
    # --- DESTINO (desde tus columnas actuales) ---
    COD_DANE_DPTO_D  = substr(normalize_code(codigo_municipio, 5), 1, 2),
    DEPARTAMENTO_D   = as.character(departamento),
    COD_DANE_MUNIC_D = normalize_code(codigo_municipio, 5),
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

saveRDS(ica_pecuaria_aviar,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/104_ICA_CensoPecuario-Aviar.rds", compress = "gzip")

### Transformación de información de Silver a Black
ica_pecuaria_aviar <- ica_pecuaria_aviar %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,
                                                           DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D,total_aves_capacidad_instalada,
                                                           total_aves_capacidad_ocupada,total_aves_traspatio,total_aves_capacidad_ocupada_mas_aves_traspatio)

saveRDS(ica_pecuaria_aviar,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/104_ICA_CensoPecuario-Aviar.rds", compress = "gzip")
saveRDS(ica_pecuaria_aviar,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/ICA_P/data/104_ICA_CensoPecuario-Aviar.rds", compress = "gzip")


###### ICA Pecuaria Porcinos
### Selección de información y cambio de nombres
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
ica_pecuaria_porcino <- ica_pecuaria_porcino %>%
  mutate(
    # --- tiempo ---
    ano  = suppressWarnings(as.integer(ano)),
    mes  = 1L,
    dia  = 1L,
    fecha_completa = ymd(sprintf("%04d-%02d-%02d", ano, mes, dia)),
    trimestre      = sprintf("%04d-T%d", ano, ((mes - 1) %/% 3) + 1),
    semestre       = sprintf("%04d-S%d", ano, if_else(mes <= 6, 1L, 2L)),
    
    # --- DESTINO (desde tus columnas actuales) ---
    COD_DANE_DPTO_D  = substr(normalize_code(codigo_municipio, 5), 1, 2),
    DEPARTAMENTO_D   = as.character(departamento),
    COD_DANE_MUNIC_D = normalize_code(codigo_municipio, 5),
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

saveRDS(ica_pecuaria_porcino,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/102_ICA_CensoPecuario-Porcino.rds", compress = "gzip")

### Golden
ica_pecuaria_porcino <- ica_pecuaria_porcino %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,
                                                               DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D,total_porcinos)

saveRDS(ica_pecuaria_porcino,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/102_ICA_CensoPecuario-Porcino.rds", compress = "gzip")
saveRDS(ica_pecuaria_porcino,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/ICA_P/data/102_ICA_CensoPecuario-Porcino.rds", compress = "gzip")

###### ICA Pecuario (Bovinos)
### Selección de información y cambio de nombres
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
ica_pecuaria <- ica_pecuaria %>%
  mutate(
    # --- tiempo ---
    ano  = suppressWarnings(as.integer(ano)),
    mes  = 1L,
    dia  = 1L,
    fecha_completa = ymd(sprintf("%04d-%02d-%02d", ano, mes, dia)),
    trimestre      = sprintf("%04d-T%d", ano, ((mes - 1) %/% 3) + 1),
    semestre       = sprintf("%04d-S%d", ano, if_else(mes <= 6, 1L, 2L)),
    
    # --- DESTINO (desde tus columnas actuales) ---
    COD_DANE_DPTO_D  = substr(normalize_code(codigo_municipio, 5), 1, 2),
    DEPARTAMENTO_D   = as.character(departamento),
    COD_DANE_MUNIC_D = normalize_code(codigo_municipio, 5),
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

saveRDS(ica_pecuaria,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/101_ICA_CensoPecuario-Bovino.rds", compress = "gzip")

### Golden
ica_pecuaria<- ica_pecuaria %>% dplyr::select(fecha_completa,ano,mes,dia,trimestre,semestre,COD_DANE_DPTO_D,
                                              DEPARTAMENTO_D,COD_DANE_MUNIC_D,MUNICIPIO_D,terneras_1_ano,
                                              terneros_1_ano,hembras_1_2_anos,machos_1_2_anos,hembras_2_3_anos,machos_2_3_anos,
                                              hembras_3_anos,hembras_3_anos,total_bovinos)

saveRDS(ica_pecuaria,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/101_ICA_CensoPecuario-Bovino.rds", compress = "gzip")
saveRDS(ica_pecuaria,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/ICA_P/data/101_ICA_CensoPecuario-Bovino.rds", compress = "gzip")


