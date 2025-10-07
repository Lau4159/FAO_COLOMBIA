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

###-----------------------------------------------------------------------------------------------###
### Procesamiento de información de CFA

anios <- 2010:2024
trimestres <- 1:4


# Se crea un data frame vacío
base_FINAGRO_CFA <- data.frame()

for (x in anios) {
  for (y in trimestres) {
    
  # Construir el path del archivo dinámicamente
    file_path <- paste0("C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_ORIGINAL/081_FINAGRO_CFA/Desembolsos_Crédito_", x, "T", y, ".csv")
    
    # Validar si el archivo existe antes de leerlo
        if (file.exists(file_path)) {
    
    # Leer el CSV
    temporal <- read_csv(file = file_path, locale = locale(encoding = "UTF-16LE"))
    
    # Se ajusta el formato de las variables a string
    temporal$ id_credito <- as.character(temporal$id_credito)
    temporal$ periodicidad_tasa_indexacion <- as.character(temporal$ periodicidad_tasa_indexacion)
    
    # Hacer el append al data frame final
    base_FINAGRO_CFA <- bind_rows(base_FINAGRO_CFA, temporal)
    }
    }
}

# Estandarización de las variables
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(fecha_completa = fecha_credito)
base_FINAGRO_CFA$ano <- year(base_FINAGRO_CFA$fecha_completa)
base_FINAGRO_CFA$mes <- month(base_FINAGRO_CFA$fecha_completa)
base_FINAGRO_CFA$dia <- day(base_FINAGRO_CFA$fecha_completa)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(COD_DANE_MUNIC_D=cod_municipio)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(TIPO_CARTERA=tipo_cartera)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(TIPO_INTERMEDIARIO=tipo_intermediario)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(TIPO_PRODUCTOR=tipo_productor)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(TIPO_BENEFICIARIO=tipo_beneficiario)
base_FINAGRO_CFA$TIPO_PRODUCTOR [base_FINAGRO_CFA$TIPO_PRODUCTOR=="Pequeño ppib"]<-"Pequeño PPIB"
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(TIPO_PERSONA=tipo_persona)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(SEXO=sexo)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(COD_DESTINO=cod_destino)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(LINEA_CREDITO=linea_credito)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(CADENA=cadena)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(ESLABON_CADENA=eslabon_cadena)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(VALOR_CREDITO=valor_credito)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(NOMBRE_PROGRAMA=nombre_programa)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(INDICADOR_LEC=indicador_lec)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(VALOR_SUBSIDIO=valor_subsidio)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% rename(NUEVO=nuevo)
base_FINAGRO_CFA <- base_FINAGRO_CFA %>% dplyr::select(-valor_inversion, -plazo, -periodo_gracia, -cuotas, -cuotas_capital, -tasa_indexacion, -periodicidad_tasa_indexacion, -puntos_adicionales, -garantia_fag)


base_FINAGRO_CFA$COD_DANE_MUNIC_D <- as.character(base_FINAGRO_CFA$COD_DANE_MUNIC_D)
base_FINAGRO_CFA$COD_DANE_MUNIC_D <- ifelse(nchar(base_FINAGRO_CFA$COD_DANE_MUNIC_D) == 4, paste0("0", base_FINAGRO_CFA$COD_DANE_MUNIC_D), base_FINAGRO_CFA$COD_DANE_MUNIC_D)
base_FINAGRO_CFA$ COD_DANE_DPTO_D <- substr(base_FINAGRO_CFA$COD_DANE_MUNIC_D, 1, 2)


saveRDS(base_FINAGRO_CFA,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_SILVER/081_FINAGRO_CFA.rds", compress = "gzip")

###Ahora se pasa al gold que es la información con la agregación por municipio y mes
collapse_base_FINAGRO_CFA <- base_FINAGRO_CFA %>%   group_by(COD_DANE_DPTO_D, COD_DANE_MUNIC_D, TIPO_CARTERA, TIPO_INTERMEDIARIO, TIPO_PRODUCTOR, TIPO_PERSONA, SEXO, COD_DESTINO, LINEA_CREDITO, CADENA, ESLABON_CADENA, ano, mes) %>%   summarise(VALOR_CREDITO= sum(VALOR_CREDITO, na.rm = TRUE), NUMERO_CREDITO = n(), NUMERO_NUEVOS_CREDITOS = sum(NUEVO, na.rm = TRUE))
saveRDS(collapse_base_FINAGRO_CFA,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/01_Datos de matriz de indicadores/DATA_GOLDEN_Indicadores/081_FINAGRO_CFA.rds", compress = "gzip")
saveRDS(collapse_base_FINAGRO_CFA,file = "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/FINAGRO_CFA/data/081_FINAGRO_CFA.rds", compress = "gzip")





