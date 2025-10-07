# =========================================================
# app_finagro_moderno.R — Tendencias + Indicadores + Mapas (estilo moderno)
# (CRS normalizado; deptos desde municipios; nombres desde shapefiles)
# =========================================================
suppressWarnings({
  library(shiny); library(dplyr); library(plotly)
  library(scales); library(ggplot2); library(networkD3)
  library(sf); library(leaflet); library(bslib)
})

options(stringsAsFactors = FALSE, scipen = 999)
options(shiny.maxRequestSize = 100*1024^2)
sf::sf_use_s2(FALSE)

# ---------- Rutas ----------
data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/FINAGRO_CFA/data"
finagro_path   <- file.path(data_dir, "081_FINAGRO_CFA.rds")
ruta_shp_mpios <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dptos <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")  # opcional

# ---------- Datos ----------
finagro <- readRDS(finagro_path) %>%
  dplyr::mutate(
    COD_DANE_DPTO_D  = suppressWarnings(as.integer(COD_DANE_DPTO_D)),
    COD_DANE_MUNIC_D = suppressWarnings(as.integer(COD_DANE_MUNIC_D))
  )

# ---------- Helpers ----------
fmt_int   <- function(x) number(x, big.mark = ".", decimal.mark = ",", accuracy = 1)
fmt_cop   <- function(x) paste0("$", number(x, big.mark = ".", decimal.mark = ",", accuracy = 1))
fmt_mmilM <- function(x) paste0("$", number(x/1e9, big.mark = ".", decimal.mark = ",", accuracy = 0.1), " Mil M")
fmt_milM  <- function(x) paste0(number(x/1e9, big.mark = ".", decimal.mark = ",", accuracy = 0.1), " Mil M")
mes_labels <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

pick_first <- function(nms, candidates) { hit <- intersect(candidates, nms); if (!length(hit)) return(NA_character_); hit[1] }
safe_int_str <- function(x, width) sprintf(paste0("%0", width, "d"), suppressWarnings(as.integer(x)))

pal_bin_safe <- function(palette, x, bins = 5){
  dom <- x[is.finite(x)]; if (!length(dom)) dom <- c(0,1)
  if (length(unique(dom))==1) dom <- unique(dom)+c(-1e-9,1e-9)
  leaflet::colorBin(palette, domain = dom, bins = bins, na.color = "#f0f0f0")
}

normalize_to_wgs84 <- function(sf_obj){
  crs_now <- sf::st_crs(sf_obj)
  if (is.na(crs_now)) {
    bb <- sf::st_bbox(sf_obj)
    if (bb["xmin"] > -85 && bb["xmax"] < -65 && bb["ymin"] > -5 && bb["ymax"] < 15) {
      sf_obj <- sf::st_set_crs(sf_obj, 4326)   # lon/lat Colombia
    } else {
      sf_obj <- sf::st_set_crs(sf_obj, 4686)   # MAGNA-SIRGAS geográfico
    }
  }
  sf_obj <- sf::st_transform(sf_obj, 4326)
  sf_obj <- sf::st_make_valid(sf_obj)
  sf_obj
}

# ---------- Shapes ----------
# --- Municipios ---
mpios_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE) %>% normalize_to_wgs84()
nm_mpios  <- names(mpios_raw)
mun_code_col <- pick_first(nm_mpios, c("MPIO_CDPMP","COD_MPIO","COD_DANE_MPIO","COD_DANE","COD_MUN","MPIO","CODIGO_MPIO","DPTOMPIO"))
mun_name_col <- pick_first(nm_mpios, c("MPIO_CNMBR","NOMBRE_MPIO","MUNICIPIO","MUN_CNMBR","NOM_MPIO","NOMBRE"))
stopifnot(!is.na(mun_code_col), !is.na(mun_name_col))

mpios_sf <- mpios_raw %>%
  dplyr::mutate(
    COD_MUN5  = safe_int_str(.data[[mun_code_col]], 5),
    COD_DPTO2 = substr(COD_MUN5, 1, 2),
    NOM_MPIO  = as.character(.data[[mun_name_col]])
  ) %>%
  dplyr::select(COD_MUN5, COD_DPTO2, NOM_MPIO, geometry)

# --- Dptos (opcionales) para nombres ---
dptos_raw <- tryCatch(sf::st_read(ruta_shp_dptos, quiet = TRUE) %>% normalize_to_wgs84(), error = function(e) NULL)
if (!is.null(dptos_raw)) {
  nm_dptos  <- names(dptos_raw)
  dep_code_col_d <- pick_first(nm_dptos, c("DPTO_CCDGO","COD_DEPTO","DPTO","COD_DPTO","COD_DPT","CODIGO_DEPTO"))
  dep_name_col_d <- pick_first(nm_dptos, c("DPTO_CNMBR","NOMBRE_DPT","NOMBRE_DEP","NOM_DPTO","DEPARTAMENTO","NOMBRE"))
  if (!is.na(dep_code_col_d) && !is.na(dep_name_col_d)) {
    dptos_pre <- dptos_raw %>%
      dplyr::mutate(
        COD_DPTO2 = safe_int_str(.data[[dep_code_col_d]], 2),
        NOM_DPTO  = as.character(.data[[dep_name_col_d]])
      ) %>%
      dplyr::select(COD_DPTO2, NOM_DPTO, geometry)
  } else dptos_pre <- NULL
} else dptos_pre <- NULL

# --- Dptos desde municipios (unión en CRS proyectado) ---
dptos_union <- mpios_sf %>%
  sf::st_transform(3116) %>%                       # MAGNA-SIRGAS / Colombia Bogotá
  dplyr::group_by(COD_DPTO2) %>%
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  sf::st_transform(4326)

nombres_dep <- tryCatch(
  dptos_pre %>% sf::st_drop_geometry() %>% dplyr::select(COD_DPTO2, NOM_DPTO) %>% dplyr::distinct(),
  error = function(e) tibble::tibble(COD_DPTO2 = unique(dptos_union$COD_DPTO2),
                                     NOM_DPTO  = paste0("DPTO ", unique(dptos_union$COD_DPTO2)))
)

dptos_sf <- dptos_union %>%
  dplyr::left_join(nombres_dep, by = "COD_DPTO2") %>%
  dplyr::mutate(NOM_DPTO = ifelse(is.na(NOM_DPTO), paste0("DPTO ", COD_DPTO2), NOM_DPTO)) %>%
  dplyr::select(COD_DPTO2, NOM_DPTO, geometry)

# --- Agregados para mapas ---
finagro_depto_map <- finagro %>%
  dplyr::mutate(COD_DPTO2 = safe_int_str(COD_DANE_DPTO_D, 2)) %>%
  dplyr::group_by(COD_DPTO2) %>%
  dplyr::summarise(
    monto    = mean(VALOR_CREDITO, na.rm = TRUE),
    creditos = mean(NUMERO_CREDITO, na.rm = TRUE),
    .groups  = "drop"
  )

finagro_mpio_map <- finagro %>%
  dplyr::mutate(COD_MUN5 = safe_int_str(COD_DANE_MUNIC_D, 5)) %>%
  dplyr::group_by(COD_MUN5) %>%
  dplyr::summarise(
    monto    = mean(VALOR_CREDITO, na.rm = TRUE),
    creditos = mean(NUMERO_CREDITO, na.rm = TRUE),
    .groups  = "drop"
  )

# **IMPORTANTE**: tras el join, forzar clase sf
mapa_depto <- dptos_sf %>%
  dplyr::left_join(finagro_depto_map, by = "COD_DPTO2") %>%
  sf::st_as_sf()

mapa_mpio  <- mpios_sf  %>%
  dplyr::left_join(finagro_mpio_map,  by = "COD_MUN5") %>%
  sf::st_as_sf()

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bs_theme(
    version = 5, primary = "#2563eb",
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius"  = "0.9rem",
    "font-size-base" = "0.97rem"
  ),
  tags$head(tags$style(HTML("
    .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
    h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
    .data-note{font-size:13px;color:#6b7280;margin:0 0 16px}
    .filters{background:#fff;border:1px solid #eaecef;border-radius:16px;
             padding:14px 16px;margin-bottom:16px;box-shadow:0 4px 14px rgba(0,0,0,.06)}
    .filters-grid{display:grid;grid-template-columns:repeat(3,minmax(220px,1fr));gap:12px}
    .filters-grid.maps{grid-template-columns:repeat(1,minmax(220px,1fr))}
    .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;text-transform:uppercase;color:#6b7280;margin-bottom:6px}
    .selectize-input,.form-control{min-height:42px;border-radius:10px}
    .selectize-input{padding:10px 12px}
    .card{background:#fff;border:1px solid #eaecef;border-radius:16px;padding:14px;box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px}
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
    .grid-4{display:grid;grid-template-columns:repeat(4,1fr);gap:12px}
    .metric-value{font-size:28px;font-weight:800;color:#111827;margin:2px 0 0}
    .metric-sub{font-size:12px;color:#6b7280;margin-top:2px}
  "))),
  div(class="wrap",
      h3("FINAGRO"),
      div(class="data-note","Tendencias, indicadores y mapas — versión con estilo moderno."),
      
      tabsetPanel(
        id = "tabs_finagro", type = "tabs",
        
        tabPanel(
          "Tendencias históricas", br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter",
                      div(class="filter-label","Departamento"),
                      selectInput("depto_t1", NULL,
                                  choices = c("Todos", sort(unique(dptos_sf$NOM_DPTO))),
                                  selected = "Todos"))
              )
          ),
          div(class="grid-4",
              div(class="card", div(class="card-title","Monto total histórico"),
                  uiOutput("hist_monto_txt"), div(class="metric-sub","Suma de VALOR_CREDITO")),
              div(class="card", div(class="card-title","Créditos históricos"),
                  uiOutput("hist_creditos_txt"), div(class="metric-sub","Suma de NUMERO_CREDITO")),
              div(class="card", div(class="card-title","Monto promedio histórico"),
                  uiOutput("hist_prom_txt"), div(class="metric-sub","Monto / Nº créditos")),
              div(class="card", div(class="card-title","Créditos a mujeres"),
                  uiOutput("hist_mujeres_txt"), div(class="metric-sub","% sobre total de créditos"))
          ),
          fluidRow(
            column(6, div(class="card", div(class="card-title","Monto total por año"),
                          plotlyOutput("hist_monto_total", height = 330))),
            column(6, div(class="card", div(class="card-title","Monto por tipo de productor"),
                          plotlyOutput("hist_tipo_productor", height = 330)))
          ),
          fluidRow(
            column(6, div(class="card", div(class="card-title","Monto por sexo / persona"),
                          plotlyOutput("hist_sexo", height = 330))),
            column(6, div(class="card", div(class="card-title","Monto promedio por crédito"),
                          plotlyOutput("hist_promedio", height = 330)))
          )
        ),
        
        tabPanel(
          "Indicadores", br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter",
                      div(class="filter-label","Año"),
                      selectInput("ano", NULL, choices = sort(unique(finagro$ano)),
                                  selected = max(finagro$ano))),
                  div(class="filter",
                      div(class="filter-label","Tipo de productor"),
                      selectInput("productor", NULL,
                                  choices = c("Todos", sort(unique(finagro$TIPO_PRODUCTOR))),
                                  selected = "Todos")),
                  div(class="filter",
                      div(class="filter-label","Sexo / Tipo de persona"),
                      selectInput("sexo", NULL,
                                  choices = c("Todos", "Hombre", "Mujer", "Jurídico"),
                                  selected = "Todos")),
                  div(class="filter",
                      div(class="filter-label","Departamento"),
                      selectInput("depto_t2", NULL,
                                  choices = c("Todos", sort(unique(dptos_sf$NOM_DPTO))),
                                  selected = "Todos"))
              )
          ),
          div(class="grid-4",
              div(class="card", div(class="card-title","Total créditos"),
                  uiOutput("ind_total_creditos_txt")),
              div(class="card", div(class="card-title","Monto total"),
                  uiOutput("ind_total_monto_txt")),
              div(class="card", div(class="card-title","Monto promedio por crédito"),
                  uiOutput("ind_monto_prom_txt")),
              div(class="card", div(class="card-title","Créditos a mujeres"),
                  uiOutput("ind_pct_mujeres_txt"))
          ),
          fluidRow(
            column(6, div(class="card", div(class="card-title","Evolución mensual"),
                          plotlyOutput("serie_tiempo", height = 330))),
            column(6, div(class="card", div(class="card-title","Top 5 líneas de crédito (Monto)"),
                          plotlyOutput("top_lineas", height = 330)))
          ),
          fluidRow(
            column(6, div(class="card", div(class="card-title","Créditos por eslabón de cadena"),
                          plotlyOutput("eslabon_cadena", height = 330))),
            column(6, div(class="card", div(class="card-title","Flujo Línea → Eslabón"),
                          sankeyNetworkOutput("sankey", height = "330px")))
          )
        ),
        
        tabPanel(
          "Mapas", br(),
          div(class="filters",
              div(class="filters-grid maps",
                  div(class="filter",
                      div(class="filter-label","Selecciona un departamento"),
                      selectInput("depto_sel_m", NULL,
                                  choices = c("Todos", sort(unique(mapa_depto$NOM_DPTO))),
                                  selected = "Todos"))
              )
          ),
          fluidRow(
            column(7, div(class="card", div(class="card-title","Mapa interactivo"),
                          leafletOutput("mapa_m", height = 600))),
            column(5, div(class="card", div(class="card-title","Top 5"),
                          plotOutput("top5_m", height = 600)))
          )
        )
      )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  base_data <- reactive({
    finagro %>%
      dplyr::mutate(
        SEXO2     = ifelse(is.na(SEXO), "Jurídico", as.character(SEXO)),
        COD_DPTO2 = safe_int_str(COD_DANE_DPTO_D, 2),
        COD_MUN5  = safe_int_str(COD_DANE_MUNIC_D, 5)
      ) %>%
      dplyr::left_join(dptos_sf %>% sf::st_drop_geometry() %>% dplyr::select(COD_DPTO2, NOM_DPTO),
                       by = "COD_DPTO2")
  })
  
  # ----- TENDENCIAS -----
  base_t1 <- reactive({
    df <- base_data()
    dep <- input$depto_t1
    if (!is.null(dep) && dep != "Todos") df <- df %>% dplyr::filter(NOM_DPTO == dep)
    df
  })
  output$hist_monto_txt <- renderUI({ tags$div(class="metric-value", fmt_mmilM(sum(base_t1()$VALOR_CREDITO, na.rm=TRUE))) })
  output$hist_creditos_txt <- renderUI({ tags$div(class="metric-value", fmt_int(sum(base_t1()$NUMERO_CREDITO, na.rm=TRUE))) })
  output$hist_prom_txt <- renderUI({
    df <- base_t1(); n <- sum(df$NUMERO_CREDITO, na.rm=TRUE); m <- sum(df$VALOR_CREDITO, na.rm=TRUE)
    tags$div(class="metric-value", fmt_cop(if(n>0) m/n else 0))
  })
  output$hist_mujeres_txt <- renderUI({
    df <- base_t1(); tot <- sum(df$NUMERO_CREDITO, na.rm=TRUE); muj <- sum(df$NUMERO_CREDITO[df$SEXO2=="Mujer"], na.rm=TRUE)
    pct <- if (tot>0) 100*muj/tot else 0
    tags$div(class="metric-value", paste0(number(pct, accuracy=0.1, decimal.mark=","), "%"))
  })
  output$hist_monto_total <- renderPlotly({
    df <- base_t1() %>% dplyr::group_by(ano) %>% dplyr::summarise(monto=sum(VALOR_CREDITO,na.rm=TRUE)/1e9,.groups="drop")
    ggplotly(ggplot(df, aes(ano,monto))+geom_line(color="darkgreen",size=1.2)+geom_point(color="darkgreen")+labs(x="Año",y="Miles de Millones")+theme_minimal())
  })
  output$hist_tipo_productor <- renderPlotly({
    df <- base_t1() %>% dplyr::group_by(ano,TIPO_PRODUCTOR) %>% dplyr::summarise(monto=sum(VALOR_CREDITO,na.rm=TRUE)/1e9,.groups="drop")
    ggplotly(ggplot(df, aes(ano,monto,color=TIPO_PRODUCTOR))+geom_line(size=1.2)+labs(x="Año",y="Miles de Millones",color="Tipo productor")+theme_minimal())
  })
  output$hist_sexo <- renderPlotly({
    df <- base_t1() %>% dplyr::group_by(ano,SEXO2) %>% dplyr::summarise(monto=sum(VALOR_CREDITO,na.rm=TRUE)/1e9,.groups="drop")
    ggplotly(ggplot(df, aes(ano,monto,color=SEXO2))+geom_line(size=1.2)+labs(x="Año",y="Miles de Millones",color="Sexo / Persona")+theme_minimal())
  })
  output$hist_promedio <- renderPlotly({
    df <- base_t1() %>% dplyr::group_by(ano) %>% dplyr::summarise(m=sum(VALOR_CREDITO,na.rm=TRUE),n=sum(NUMERO_CREDITO,na.rm=TRUE),.groups="drop") %>% dplyr::mutate(prom=ifelse(n>0,m/n,0)/1e9)
    ggplotly(ggplot(df, aes(ano,prom))+geom_line(color="purple",size=1.2)+geom_point(color="purple")+labs(x="Año",y="Promedio (Miles de Millones)")+theme_minimal())
  })
  
  # ----- INDICADORES -----
  base_filtrada <- reactive({
    df <- base_data() %>% dplyr::filter(ano == input$ano)
    if (!is.null(input$productor) && input$productor!="Todos") df <- df %>% dplyr::filter(TIPO_PRODUCTOR==input$productor)
    if (!is.null(input$sexo)      && input$sexo!="Todos")      df <- df %>% dplyr::filter(SEXO2==input$sexo)
    if (!is.null(input$depto_t2)  && input$depto_t2!="Todos")  df <- df %>% dplyr::filter(NOM_DPTO==input$depto_t2)
    df
  })
  output$ind_total_creditos_txt <- renderUI({ tags$div(class="metric-value", fmt_int(sum(base_filtrada()$NUMERO_CREDITO, na.rm=TRUE))) })
  output$ind_total_monto_txt    <- renderUI({ tags$div(class="metric-value", fmt_mmilM(sum(base_filtrada()$VALOR_CREDITO, na.rm=TRUE))) })
  output$ind_monto_prom_txt     <- renderUI({
    n <- sum(base_filtrada()$NUMERO_CREDITO, na.rm=TRUE); m <- sum(base_filtrada()$VALOR_CREDITO, na.rm=TRUE)
    tags$div(class="metric-value", fmt_cop(if(n>0) m/n else 0))
  })
  output$ind_pct_mujeres_txt    <- renderUI({
    df <- base_filtrada(); tot <- sum(df$NUMERO_CREDITO, na.rm=TRUE); muj <- sum(df$NUMERO_CREDITO[df$SEXO2=="Mujer"], na.rm=TRUE)
    tags$div(class="metric-value", paste0(number(if (tot>0) 100*muj/tot else 0, accuracy=0.1, decimal.mark=","), "%"))
  })
  output$serie_tiempo <- renderPlotly({
    df <- base_filtrada() %>% dplyr::group_by(mes) %>% dplyr::summarise(creditos=sum(NUMERO_CREDITO,na.rm=TRUE), monto=sum(VALOR_CREDITO,na.rm=TRUE)/1e9,.groups="drop") %>% dplyr::mutate(mes_lbl=factor(mes,levels=1:12,labels=mes_labels))
    plot_ly(df, x=~mes_lbl) %>%
      add_lines(y=~creditos, name="Créditos", line=list(color="steelblue"), yaxis="y1") %>%
      add_lines(y=~monto, name="Miles de Millones", line=list(color="darkgreen", dash="dot"), yaxis="y2") %>%
      layout(yaxis=list(title="Número de créditos"), yaxis2=list(title="Miles de Millones", overlaying="y", side="right"))
  })
  output$top_lineas <- renderPlotly({
    df <- base_filtrada() %>% dplyr::group_by(LINEA_CREDITO) %>% dplyr::summarise(monto=sum(VALOR_CREDITO,na.rm=TRUE),.groups="drop") %>%
      dplyr::arrange(dplyr::desc(monto)) %>% dplyr::slice_head(n=5) %>% dplyr::mutate(monto_m=monto/1e9)
    ggplotly(ggplot(df, aes(monto_m, reorder(LINEA_CREDITO,monto_m), text=paste0("Línea: ",LINEA_CREDITO,"<br>Monto: ",fmt_milM(monto))))+geom_col(fill="#3b82f6")+coord_flip()+labs(x="Miles de Millones", y=NULL)+theme_minimal(), tooltip="text")
  })
  output$eslabon_cadena <- renderPlotly({
    df <- base_filtrada() %>% dplyr::group_by(ESLABON_CADENA) %>% dplyr::summarise(creditos=sum(NUMERO_CREDITO,na.rm=TRUE),.groups="drop")
    ggplotly(ggplot(df, aes(creditos, reorder(ESLABON_CADENA,creditos)))+geom_col(fill="steelblue")+labs(x="Número de créditos", y=NULL)+theme_minimal())
  })
  output$sankey <- renderSankeyNetwork({
    df_links <- base_filtrada() %>% dplyr::group_by(LINEA_CREDITO,ESLABON_CADENA) %>% dplyr::summarise(value=sum(VALOR_CREDITO,na.rm=TRUE)/1e9,.groups="drop")
    nodos <- data.frame(name=c(unique(df_links$LINEA_CREDITO), unique(df_links$ESLABON_CADENA)))
    df_links$source <- match(df_links$LINEA_CREDITO, nodos$name)-1
    df_links$target <- match(df_links$ESLABON_CADENA, nodos$name)-1
    sankeyNetwork(Links=df_links, Nodes=nodos, Source="source", Target="target", Value="value", NodeID="name", units="Miles de Millones", fontSize=10, nodeWidth=30)
  })
  
  # ----- MAPAS -----
  mapa_sel <- reactive({
    if (is.null(input$depto_sel_m) || input$depto_sel_m=="Todos") {
      list(tipo="depto", shp=mapa_depto)
    } else {
      cod <- mapa_depto$COD_DPTO2[mapa_depto$NOM_DPTO==input$depto_sel_m][1]
      shp <- mapa_mpio %>% dplyr::filter(COD_DPTO2==cod)
      list(tipo="mpio", shp=shp)
    }
  })
  
  output$mapa_m <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng=-74.3, lat=4.6, zoom=5)
  })
  
  observe({
    sel <- mapa_sel(); shp <- sel$shp
    
    # Guardas: asegurar que es sf y que tiene geometría no vacía
    req(inherits(shp, "sf"))
    req(!is.null(sf::st_geometry(shp)))
    req(nrow(shp) > 0)
    req(!all(sf::st_is_empty(shp)))
    
    if (sel$tipo=="depto") {
      pal <- pal_bin_safe("YlGnBu", shp$monto, bins=5)
      bb  <- sf::st_bbox(shp)
      leafletProxy("mapa_m") %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(
          data=shp,
          layerId=~COD_DPTO2,
          fillColor=~pal(monto),
          color="#444", weight=0.6, opacity=1, fillOpacity=0.8,
          label=~paste0(NOM_DPTO,"<br>","Prom. monto: ",fmt_milM(monto),"<br>","Prom. créditos: ",fmt_int(creditos)),
          labelOptions=labelOptions(direction="auto")
        ) %>%
        leaflet::addLegend("bottomright", pal=pal, values=shp$monto,
                           title="Promedio monto crédito<br>(Miles de millones)") %>%
        fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    } else {
      pal <- pal_bin_safe("YlOrRd", shp$monto, bins=5)
      bb  <- sf::st_bbox(shp)
      leafletProxy("mapa_m") %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(
          data=shp,
          layerId=~COD_MUN5,
          fillColor=~pal(monto),
          color="#666", weight=0.4, opacity=1, fillOpacity=0.7,
          label=~paste0(NOM_MPIO,"<br>","Prom. monto: ",fmt_milM(monto),"<br>","Prom. créditos: ",fmt_int(creditos)),
          labelOptions=labelOptions(direction="auto")
        ) %>%
        leaflet::addLegend("bottomright", pal=pal, values=shp$monto,
                           title="Promedio monto crédito<br>(Miles de millones)") %>%
        fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  observeEvent(input$mapa_m_shape_click, {
    click <- input$mapa_m_shape_click
    if (!is.null(click$id) && nchar(click$id)==2){
      nom <- mapa_depto$NOM_DPTO[mapa_depto$COD_DPTO2==click$id][1]
      if (!is.na(nom)) updateSelectInput(session, "depto_sel_m", selected=nom)
    }
  })
  
  output$top5_m <- renderPlot({
    sel <- mapa_sel(); shp <- sel$shp
    if (nrow(shp)==0) return(NULL)
    if (sel$tipo=="depto") {
      df <- shp %>% sf::st_drop_geometry() %>% dplyr::transmute(nombre=NOM_DPTO, valor=monto)
      titulo <- "Top 5 departamentos — Prom. monto"
    } else {
      df <- shp %>% sf::st_drop_geometry() %>% dplyr::transmute(nombre=NOM_MPIO, valor=monto)
      titulo <- paste0("Top 5 municipios — ", input$depto_sel_m)
    }
    df <- df %>% dplyr::filter(is.finite(valor)) %>% dplyr::arrange(dplyr::desc(valor)) %>% dplyr::slice_head(n=5)
    ggplot(df, aes(x=reorder(nombre, valor), y=valor/1e9)) +
      geom_col(fill="#3b82f6") +
      geom_text(aes(label=number(valor/1e9, big.mark=".", decimal.mark=",", accuracy=0.1)), hjust=-0.1, size=4) +
      coord_flip() +
      labs(title=titulo, x=NULL, y="Monto (miles de millones)") +
      scale_y_continuous(labels=label_number(big.mark=".", decimal.mark=","), expand=expansion(mult=c(0,0.1))) +
      theme_minimal(base_size=12) + theme(plot.title=element_text(face="bold"))
  })
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)

