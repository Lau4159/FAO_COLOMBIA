# =========================================================
# Dashboard ICA Pecuaria — Mapa + Serie temporal + Top-10
# (Depto/Mpio) con 2 tabs. Fix: alturas, arranque seguro,
# y SIN usar `{ ... }` en el RHS del pipe nativo.
# =========================================================

# ---- Paquetes ----
pkgs <- c(
  "shiny","bslib","shinyWidgets",
  "tidyverse","data.table","janitor","lubridate","scales",
  "sf","leaflet","htmltools","plotly","stringr","DT"
)
suppressWarnings(invisible(lapply(pkgs, require, character.only = TRUE)))
options(stringsAsFactors = FALSE)

# <-- agrega estas dos líneas:
validate <- shiny::validate
need     <- shiny::need

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Helper para tomar el primer valor seguro (evita usar `{}` tras pipe)
safe_first <- function(x, default = "?"){
  x <- x[!is.na(x)]
  if (length(x) == 0) default else x[1]
}

# ---- Rutas / datos GOLDEN ----
golden_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/ICA_P/data"
ica_bovino  <- readRDS(file.path(golden_dir, "101_ICA_CensoPecuario-Bovino.rds"))
ica_porcino <- readRDS(file.path(golden_dir, "102_ICA_CensoPecuario-Porcino.rds"))
ica_bcoe    <- readRDS(file.path(golden_dir, "103_ICA_CensoPecuario-BCOE.rds"))
ica_aviar   <- readRDS(file.path(golden_dir, "104_ICA_CensoPecuario-Aviar.rds"))

# ---- Rutas shapefiles ----
ruta_shp_mpios <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/ICA_P/data/shp/MGN_ANM_MPIOS.shp"
ruta_shp_dptos <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/ICA_P/data/shp/MGN_ANM_DPTOS.shp"
mpios_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)
dptos_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)

# =========================================================
# 1) Normalizadores y shapefiles estandarizados
# =========================================================
norm2 <- function(x) stringr::str_pad(as.character(x), 2, pad="0")
norm5 <- function(x) stringr::str_pad(as.character(x), 5, pad="0")

# --- MUNICIPIOS ---
stopifnot("MPIO_CDPMP" %in% names(mpios_raw))
muni_name_cands <- c("MUNICIPIO_D","MPIO_CNMBR","NOMBRE_MPIO","NOMBRE_MUNICIP","MUNICIPIO","NOMBRE")
muni_name_col <- muni_name_cands[muni_name_cands %in% names(mpios_raw)][1]
stopifnot(!is.na(muni_name_col))
dpto2_cands_mpio <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DEPTO","DPTO_COD")
dpto2_mpio_col <- dpto2_cands_mpio[dpto2_cands_mpio %in% names(mpios_raw)][1]

mpios_sf <- mpios_raw %>%
  mutate(
    CODMUN      = norm5(.data[["MPIO_CDPMP"]]),
    DPTO2       = if (!is.na(dpto2_mpio_col)) norm2(.data[[dpto2_mpio_col]]) else substr(CODMUN, 1, 2),
    MUNICIPIO_D = .data[[muni_name_col]]
  ) %>% st_transform(4326) %>% st_make_valid()

# --- DEPARTAMENTOS ---
depto_name_cands <- c("DEPARTAMENTO_D","DPTO_CNMBR","NOMBRE_DPT","NOMBRE_DEPTO","DEPARTAMEN","DEPARTAMENTO","NOMBRE")
depto_code_cands <- c("DPTO_CCDGO","COD_DPTO","DPTO","CODIGO_DEPTO","DPTO_COD")
depto_name_col <- depto_name_cands[depto_name_cands %in% names(dptos_raw)][1]
depto_code_col <- depto_code_cands[depto_code_cands %in% names(dptos_raw)][1]
stopifnot(!is.na(depto_name_col), !is.na(depto_code_col))
dptos_sf <- dptos_raw %>%
  mutate(
    DPTO2          = norm2(.data[[depto_code_col]]),
    DEPARTAMENTO_D = .data[[depto_name_col]]
  ) %>% st_transform(4326) %>% st_make_valid()

# =========================================================
# 2) GOLDEN — estandarizar
# =========================================================
std_golden <- function(df, valor_col, etiqueta){
  df <- janitor::clean_names(df)
  pick_first <- function(nms, cands) { hit <- cands[cands %in% nms]; if (!length(hit)) NA_character_ else hit[1] }
  mcol <- pick_first(names(df), c("cod_dane_munic_d","cod_dane_mpio","cod_mpio","cod_municipio","codigo_mpio","mpio"))
  ycol <- pick_first(names(df), c("ano","anio","year"))
  dcol <- pick_first(names(df), c("dpto_ccdgo","cod_dpto","dpto","codigo_dpto","dpto_cod"))
  dncol<- pick_first(names(df), c("departamento_d","departamento","nombre_depto","departamen"))
  stopifnot(!is.na(ycol), !is.na(mcol))
  vcol <- tolower(valor_col); stopifnot(vcol %in% names(df))
  df %>% mutate(
    year   = .data[[ycol]],
    CODMUN = norm5(.data[[mcol]]),
    DPTO2  = if (!is.na(dcol)) norm2(.data[[dcol]]) else substr(CODMUN, 1, 2),
    DEPARTAMENTO_D = if (!is.na(dncol)) .data[[dncol]] else NA_character_,
    valor  = suppressWarnings(as.numeric(.data[[vcol]]))
  ) %>%
    group_by(year, CODMUN, DPTO2, DEPARTAMENTO_D) %>%
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    mutate(especie = etiqueta)
}

need_one <- function(df, opts){ nm <- opts[opts %in% names(df)][1]; if (is.na(nm)) stop(paste("No encuentro:", paste(opts, collapse=", "))); nm }

# Bovinos
if ("total_bovinos" %in% names(ica_bovino)) {
  bov_total_col <- "total_bovinos"
} else {
  cols <- grep("^(hembras|machos|terneros|terneras)_", names(ica_bovino), value = TRUE)
  stopifnot(length(cols) > 0)
  ica_bovino[["tmp_bov_total"]] <- rowSums(sapply(ica_bovino[cols], function(x) suppressWarnings(as.numeric(x))), na.rm = TRUE)
  bov_total_col <- "tmp_bov_total"
}
g_bov  <- std_golden(ica_bovino, bov_total_col, "Bovinos")

# Porcinos
stopifnot("total_porcinos" %in% names(ica_porcino))
g_porc <- std_golden(ica_porcino, "total_porcinos", "Porcinos")

# BCOE
g_buf <- std_golden(ica_bcoe, need_one(ica_bcoe, c("total_bufalos","total_búfalos")), "Búfalos")
g_equ <- std_golden(ica_bcoe, need_one(ica_bcoe, c("total_equinos")), "Equinos")
g_cap <- std_golden(ica_bcoe, need_one(ica_bcoe, c("total_caprinos")), "Caprinos")
g_ovi <- std_golden(ica_bcoe, need_one(ica_bcoe, c("total_ovinos")),  "Ovinos")

# Aviar: ocupada + traspatio
prep_aves_combo <- function(df){
  df <- janitor::clean_names(df)
  pick_first <- function(nms, cands){ hit <- cands[cands %in% nms]; if (!length(hit)) NA_character_ else hit[1] }
  col_ocup <- pick_first(names(df), c("total_aves_capacidad_ocupada","total_aves_ocupada"))
  col_trap <- pick_first(names(df), c("total_aves_traspatio","total_aves_trapatio","total_aves_trapAtio"))
  stopifnot(!is.na(col_ocup), !is.na(col_trap))
  df$aves_ocupada_mas_traspatio <- suppressWarnings(as.numeric(df[[col_ocup]])) +
    suppressWarnings(as.numeric(df[[col_trap]]))
  df
}
ica_aviar_combo <- prep_aves_combo(ica_aviar)
g_aves <- std_golden(ica_aviar_combo, "aves_ocupada_mas_traspatio", "Aves")

# GOLDEN unificada
golden <- bind_rows(g_bov, g_porc, g_buf, g_equ, g_cap, g_ovi, g_aves)

# =========================================================
# 3) UI — Tabset con 2 tabs (EVA-style) para ICA
# =========================================================
especies_all <- sort(unique(golden$especie))

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      h2#app-title { text-align:center; margin-top: 10px; margin-bottom: 5px; }
      .card { background: #ffffff; border: 1px solid #e6e6e6; border-radius: 16px;
              padding: 12px; box-shadow: 0 1px 6px rgba(0,0,0,0.05); }
      .filter-label { font-weight: 600; margin-bottom: 4px; }
      .top-filters .col-sm-3 { margin-bottom: 10px; }
    "))
  ),
  
  h2("Explorador territorial de indicadores pecuarios (ICA)", id = "app-title"),
  
  bslib::navset_tab(id = "tabs",
                    
                    # =========================
                    # === Tab 1: Explorador ===
                    # =========================
                    bslib::nav_panel("Explorador territorial de indicadores pecuarios",
                                     fluidRow(
                                       class = "top-filters",
                                       column(3, div(class="filter-label","Indicador"),
                                              shinyWidgets::pickerInput("especie", NULL,
                                                                        choices = especies_all, selected = especies_all[1],
                                                                        options = list(`live-search` = TRUE))
                                       ),
                                       column(3, div(class="filter-label","Año"), uiOutput("anio_ui")),
                                       column(3, div(class="filter-label","Departamento"), uiOutput("depto_ui")),
                                       column(3, div(class="filter-label","Municipio"), uiOutput("muni_ui"))
                                     ),
                                     fluidRow(
                                       column(
                                         width = 5,
                                         div(class = "card",
                                             h5(class = "section-title", uiOutput("titulo_mapa")),
                                             div(
                                               style = "display:flex; gap:10px; align-items:center; margin-bottom:8px;",
                                               actionButton("volver", "◀ Volver a Departamentos", class = "btn btn-light"),
                                               strong(textOutput("nivel_txt", inline = TRUE))
                                             ),
                                             leafletOutput("mapa", height = 560)  # altura fija
                                         )
                                       ),
                                       column(
                                         width = 7,
                                         div(class = "card",
                                             h5(class = "section-title", "Indicadores clave"),
                                             fluidRow(
                                               column(6, uiOutput("kpi_total")),
                                               column(6, uiOutput("kpi_prom"))
                                             )
                                         ),
                                         div(class = "card",
                                             h5(class = "section-title", uiOutput("titulo_serie")),
                                             plotlyOutput("serie", height = "260px")
                                         ),
                                         div(class = "card",
                                             h5(class = "section-title", uiOutput("titulo_top")),
                                             plotlyOutput("top10", height = "320px")
                                         )
                                       )
                                     )
                    ),
                    
                    # =========================
                    # === Tab 2: CLUSTERS   ===
                    # =========================
                    bslib::nav_panel("Análisis de aglomeración y especies representativas",
                                     fluidRow(
                                       column(
                                         width = 6,
                                         div(class="card",
                                             h5("Clusters LISA por municipios"),
                                             leafletOutput("map_clusters", height = 620)
                                         )
                                       ),
                                       column(
                                         width = 6,
                                         div(class="card",
                                             h5("Resumen de clusters (municipios)"),
                                             DTOutput("clus_resumen")
                                         )
                                       )
                                     )
                    )
  )
)

# =========================================================
# 4) SERVER
# =========================================================
server <- function(input, output, session){
  # --- UI dinámico ---
  output$anio_ui <- renderUI({
    req(input$especie)
    yy <- golden |> filter(especie==input$especie) |> pull(year) |> unique() |> sort()
    selectInput("anio", label = NULL, choices = yy, selected = max(yy))
  })
  output$depto_ui <- renderUI({
    dpts <- dptos_sf |> st_drop_geometry() |> select(DPTO2, DEPARTAMENTO_D) |> distinct() |> arrange(DEPARTAMENTO_D)
    shinyWidgets::pickerInput("depto", label = NULL,
                              choices  = c("Todos", stats::setNames(dpts$DPTO2, dpts$DEPARTAMENTO_D)),
                              selected = "Todos", options  = list(`live-search` = TRUE))
  })
  output$muni_ui <- renderUI({
    base <- mpios_sf |> st_drop_geometry()
    if (!is.null(input$depto) && input$depto != "Todos") base <- filter(base, DPTO2 == input$depto)
    muni <- base |> transmute(CODMUN, MUNICIPIO_D) |> arrange(MUNICIPIO_D)
    shinyWidgets::pickerInput("muni", label = NULL,
                              choices  = c("Todos", stats::setNames(muni$CODMUN, muni$MUNICIPIO_D)),
                              selected = "Todos", options  = list(`live-search` = TRUE))
  })
  output$nivel_txt <- renderText({ if (is.null(input$depto) || input$depto == "Todos") "Nivel: Departamentos" else "Nivel: Municipios" })
  observeEvent(input$volver, ignoreInit = TRUE, {
    updatePickerInput(session,"depto", selected="Todos")
    updatePickerInput(session,"muni",  selected="Todos")
  })
  
  # --- Datos base seguros ---
  datos_base <- reactive({
    req(input$especie)
    df <- golden |> filter(especie==input$especie)
    if (!is.null(input$anio))  df <- df |> filter(year==input$anio)
    if (!is.null(input$depto) && input$depto!="Todos") df <- df |> filter(DPTO2==input$depto)
    if (!is.null(input$muni)  && input$muni!="Todos")  df <- df |> filter(CODMUN==input$muni)
    validate(need(nrow(df)>0, "Sin datos para los filtros actuales"))
    df
  })
  datos_dpto <- reactive({
    df <- datos_base() |> group_by(DPTO2) |> summarise(valor = sum(as.numeric(valor), na.rm=TRUE), .groups="drop")
    dptos_sf |> left_join(df, by="DPTO2") |> mutate(valor = ifelse(is.na(valor), 0, valor))
  })
  datos_mpio <- reactive({
    df <- datos_base() |> group_by(CODMUN) |> summarise(valor = sum(as.numeric(valor), na.rm=TRUE), .groups="drop")
    mpios_sf |> left_join(df, by="CODMUN") |> mutate(valor = ifelse(is.na(valor), 0, valor))
  })
  datos_depto_munis <- reactive({
    req(input$especie, input$anio)
    df <- golden |> filter(especie == input$especie, year == input$anio)
    if (!is.null(input$depto) && input$depto != "Todos") df <- df |> filter(DPTO2 == input$depto)
    df |> group_by(CODMUN) |> summarise(valor = sum(as.numeric(valor), na.rm = TRUE), .groups = "drop")
  })
  
  # --- KPIs ---
  output$kpi_total <- renderUI({
    v <- sum(datos_depto_munis()$valor, na.rm = TRUE)
    bslib::value_box(title = "Total (dpto/país)", value = scales::comma(v))
  })
  output$kpi_prom <- renderUI({
    vals <- datos_depto_munis()$valor
    v <- mean(vals, na.rm = TRUE)
    bslib::value_box(title = "Promedio por municipio (dpto/país)", value = scales::comma(v))
  })
  
  # --- Títulos ---
  output$titulo_mapa  <- renderUI({
    req(input$especie, input$anio)
    lev <- if (is.null(input$depto) || input$depto=="Todos") "Departamentos" else "Municipios"
    strong(paste0(input$especie," por ", tolower(lev)," — ", input$anio))
  })
  output$titulo_serie <- renderUI({
    geo <- if (!is.null(input$muni) && input$muni!="Todos") {
      nm <- (mpios_sf |> st_drop_geometry() |> filter(CODMUN==input$muni))$MUNICIPIO_D %||% "Municipio"; paste0(nm," (municipio)")
    } else if (!is.null(input$depto) && input$depto!="Todos") {
      (dptos_sf |> st_drop_geometry() |> filter(DPTO2==input$depto))$DEPARTAMENTO_D %||% "Departamento"
    } else "Colombia"
    strong(paste0("Evolución temporal de ", input$especie, " — ", geo))
  })
  output$titulo_top   <- renderUI({ req(input$especie, input$anio); strong(paste0("Top 10 municipios — ", input$especie, " en ", input$anio)) })
  
  # --- Paletas ---
  pal_depto <- function(v) leaflet::colorNumeric("YlGnBu", domain = v, na.color = "#f0f0f0")
  pal_mpio  <- function(v) leaflet::colorNumeric("YlOrRd", domain = v, na.color = "#f0f0f0")
  
  # --- Badge filtros (sin `{}` tras pipe) ---
  indi_titulo <- reactive({ req(input$especie); paste0(input$especie, " — ", input$anio %||% "") })
  badge_filtros <- reactive({
    dep_txt <- if (is.null(input$depto) || input$depto == "Todos") {
      "Todos"
    } else {
      tmp <- dptos_sf %>%
        sf::st_drop_geometry() %>%
        dplyr::filter(DPTO2 == input$depto) %>%
        dplyr::pull(DEPARTAMENTO_D)
      safe_first(tmp, "?")
    }
    mun_txt <- if (is.null(input$muni) || input$muni == "Todos") {
      "Todos"
    } else {
      tmp <- mpios_sf %>%
        sf::st_drop_geometry() %>%
        dplyr::filter(CODMUN == input$muni) %>%
        dplyr::pull(MUNICIPIO_D)
      safe_first(tmp, "?")
    }
    htmltools::HTML(sprintf(
      '<div style="background:#fff;padding:6px 10px;border-radius:8px;
                   box-shadow:0 1px 6px rgba(0,0,0,.15);font-size:12px;line-height:1.35;">
         <b>Indicador:</b> %s<br>
         <b>Año:</b> %s<br>
         <b>Departamento:</b> %s<br>
         <b>Municipio:</b> %s
       </div>',
      htmltools::htmlEscape(input$especie %||% ""), htmltools::htmlEscape(input$anio %||% ""),
      htmltools::htmlEscape(dep_txt), htmltools::htmlEscape(mun_txt)
    ))
  })
  
  # --- Mapa inicial ---
  output$mapa <- renderLeaflet({
    req(input$especie, input$anio)
    sf_m <- datos_dpto(); pal <- pal_depto(sf_m$valor)
    leaflet(sf_m) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        layerId = ~DPTO2,
        fillColor = ~pal(valor),
        weight = 0.7, color = "#666", fillOpacity = 0.9,
        label  = ~DEPARTAMENTO_D,
        labelOptions = leaflet::labelOptions(direction = "auto", textsize = "12px", sticky = TRUE,
                                             opacity = 0.9, style = list("font-weight" = "600")),
        highlightOptions = leaflet::highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) %>%
      addLegend(position = "bottomright", pal = pal, values = ~valor, title = indi_titulo()) %>%
      addControl(badge_filtros(), position = "topright", layerId = "badge_filtros")
  })
  observe({
    req(input$especie, input$anio)
    leafletProxy("mapa") %>% removeControl("badge_filtros") %>%
      addControl(badge_filtros(), position = "topright", layerId = "badge_filtros")
  })
  
  # --- Redibujo por nivel ---
  dibujar_deptos <- function() {
    sf_m <- datos_dpto(); pal <- pal_depto(sf_m$valor)
    leafletProxy("mapa", data = sf_m) %>% clearPopups() %>% clearShapes() %>% clearControls() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(layerId=~DPTO2, fillColor=~pal(valor), weight=0.7, color="#666", fillOpacity=0.9,
                  label=~DEPARTAMENTO_D, labelOptions = leaflet::labelOptions(direction="auto", textsize="12px", sticky=TRUE,
                                                                              opacity=0.9, style=list("font-weight"="600")),
                  highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
      addLegend("bottomright", pal=pal, values=sf_m$valor, title=indi_titulo()) %>%
      addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  }
  dibujar_mpios <- function() {
    sf_m <- datos_mpio(); if (!is.null(input$depto) && input$depto != "Todos") sf_m <- sf_m |> filter(DPTO2 == input$depto)
    pal <- pal_mpio(sf_m$valor)
    leafletProxy("mapa", data = sf_m) %>% clearPopups() %>% clearShapes() %>% clearControls() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(layerId=~MUNICIPIO_D, fillColor=~pal(valor), weight=0.4, color="#666", fillOpacity=0.9,
                  label=~MUNICIPIO_D, labelOptions = leaflet::labelOptions(direction="auto", textsize="11px", sticky=TRUE,
                                                                           opacity=0.9, style=list("font-weight"="600")),
                  highlightOptions = leaflet::highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
      addLegend("bottomright", pal=pal, values=sf_m$valor, title=indi_titulo()) %>%
      addControl(badge_filtros(), position="topright", layerId="badge_filtros")
  }
  observeEvent(input$depto, {
    req(input$especie, input$anio)
    if (is.null(input$depto) || input$depto == "Todos") { dibujar_deptos() }
    else {
      dibujar_mpios()
      geom <- dptos_sf |> filter(DPTO2 == input$depto)
      if (nrow(geom) == 1) { bb <- st_bbox(geom); leafletProxy("mapa") |> fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]) }
    }
  }, ignoreInit = TRUE)
  observeEvent(input$muni, {
    req(input$especie, input$anio)
    if (!is.null(input$muni) && input$muni != "Todos") {
      dibujar_mpios()
      geom <- mpios_sf |> filter(CODMUN == input$muni)
      if (nrow(geom) == 1) { bb <- st_bbox(geom); leafletProxy("mapa") |> fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]) }
    } else { if (!is.null(input$depto) && input$depto != "Todos") dibujar_mpios() else dibujar_deptos() }
  }, ignoreInit = TRUE)
  observeEvent(input$mapa_shape_click, {
    req(input$especie, input$anio)
    cl <- input$mapa_shape_click; req(cl$id)
    if (is.null(input$depto) || input$depto=="Todos") {
      updatePickerInput(session, "depto", selected = cl$id)
    }
  })
  
  # --- Serie temporal ---
  output$serie <- renderPlotly({
    req(input$especie)
    df <- golden |> filter(especie == input$especie)
    if (!is.null(input$depto) && input$depto != "Todos") df <- df |> filter(DPTO2 == input$depto)
    if (!is.null(input$muni)  && input$muni  != "Todos") df <- df |> filter(CODMUN == input$muni)
    ts <- df |> group_by(year) |> summarise(valor = sum(as.numeric(valor), na.rm = TRUE), .groups = "drop") |>
      mutate(date = as.Date(paste0(as.integer(year), "-01-01")), valor = as.numeric(valor)) |>
      arrange(date) |> filter(is.finite(valor))
    validate(need(nrow(ts)>0, "Sin datos para los filtros actuales"))
    p <- ggplot(ts, aes(x = date, y = valor,
                        text = paste0("Año: ", format(date, "%Y"), "<br>Valor: ", scales::comma(valor)))) +
      geom_line(linewidth = 1.2, color = "#2c7fb8", na.rm = TRUE) +
      geom_point(size = 2.5, color = "#08306b") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(labels = scales::label_comma()) +
      labs(x = NULL, y = NULL) + theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 40, r = 20, t = 10, b = 40))
  })
  
  # --- Top-10 ---
  output$top10 <- renderPlotly({
    df <- datos_base() |>
      group_by(CODMUN) |> summarise(valor = sum(valor, na.rm=TRUE), .groups="drop") |>
      left_join(mpios_sf |> st_drop_geometry() |> select(CODMUN, MUNICIPIO_D, DPTO2), by="CODMUN") |>
      left_join(dptos_sf |> st_drop_geometry() |> select(DPTO2, DEPARTAMENTO_D), by="DPTO2")
    top <- df |> arrange(desc(valor)) |> slice_head(n=10) |>
      mutate(lbl = paste0(MUNICIPIO_D," (",DEPARTAMENTO_D,")"))
    validate(need(nrow(top)>0, "Sin datos para el Top-10"))
    p <- ggplot(top, aes(x=reorder(lbl, valor), y=valor, text=paste0(lbl," — ",scales::comma(valor)))) +
      geom_col() + coord_flip() + labs(x=NULL, y=NULL) + theme_minimal(base_size = 12)
    ggplotly(p, tooltip="text") %>% layout(margin = list(l=160,r=10))
  })
  
  # ===========================
  # Tab 2: contenido mínimo
  # ===========================
  output$map_clusters <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -74.3, lat = 4.6, zoom = 5)
  })
  output$clus_resumen <- renderDT({
    tibble::tibble(
      categoria = c("High-High","Low-Low","High-Low","Low-High","No significativo"),
      municipios = c(NA,NA,NA,NA,NA)
    ) |> datatable(options = list(dom = "t", pageLength = 5), rownames = FALSE)
  })
}

# ---- Lanzar app ----
shinyApp(ui, server)

