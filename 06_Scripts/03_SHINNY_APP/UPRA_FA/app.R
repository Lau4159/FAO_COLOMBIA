# app_upra.R — UPRA FA (Total municipal) con layout estilo BPAN
suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr); library(tidyr)
  library(scales); library(htmltools); library(plotly)
  library(stringi); library(readr); library(tibble)
})
options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# ---------- Rutas ----------
local_data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/UPRA_FA/data"
app_root     <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
rel_data_dir <- file.path(app_root, "data")
data_dir <- if (dir.exists(rel_data_dir)) rel_data_dir else local_data_dir

ruta_upra      <- file.path(data_dir, "013_UPRA_FA_Proporcion FA_Total municipal.rds")
ruta_pob       <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")
ruta_shp_mpios <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dptos <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

must_exist <- c(ruta_upra, ruta_pob, ruta_shp_mpios, ruta_shp_dptos)
miss <- must_exist[!file.exists(must_exist)]
if (length(miss)) stop("Faltan archivos. data_dir usado: ", data_dir, "\n", paste("-", miss, collapse = "\n"))
check_shp_parts <- function(shp){ b <- sub("\\.shp$", "", shp); req <- paste0(b, c(".shp",".dbf",".shx",".prj")); req[!file.exists(req)] }
miss_shp <- c(check_shp_parts(ruta_shp_mpios), check_shp_parts(ruta_shp_dptos))
if (length(miss_shp)) stop("Faltan componentes de shapefile:\n", paste("-", miss_shp, collapse = "\n"))

# ---------- Utils ----------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
NUP <- function(x) toupper(norm_txt(x))
num_or_na <- function(x) suppressWarnings(readr::parse_number(as.character(x)))
pick_col <- function(df, primary, pattern){
  nms <- names(df); if (primary %in% nms) return(primary)
  alt <- nms[grepl(pattern, nms, ignore.case = TRUE)]; if (length(alt)) alt[1] else NA_character_
}
safe_pull <- function(df, col) if (!is.na(col) && col %in% names(df)) df[[col]] else NA

make_pal_bin <- function(values, palette = "Blues", n_bins = 6){
  vals <- suppressWarnings(as.numeric(values)); vals <- vals[is.finite(vals)]
  if (!length(vals)) vals <- 0
  qs <- stats::quantile(vals, probs = seq(0, 1, length.out = n_bins), na.rm = TRUE)
  qs <- unique(as.numeric(qs)); if (length(qs) < 3) qs <- pretty(vals, n = n_bins)
  bins <- sort(unique(c(min(vals, na.rm = TRUE), qs, max(vals, na.rm = TRUE))))
  leaflet::colorBin(palette, domain = vals, bins = bins, na.color = "#f0f0f0")
}
fmt_pct <- function(x) ifelse(is.na(x), "NA", scales::percent(x, accuracy = 0.1))
fmt_num <- function(x, digs = 1) ifelse(is.na(x), "NA", scales::comma(x, big.mark = ","))

# ---------- Cargar UPRA + población ----------
upra_raw <- readRDS(ruta_upra)
pob_raw  <- readRDS(ruta_pob)

col_fecha_u   <- pick_col(upra_raw, "fecha_completa", "fecha|date")
col_ano_u     <- pick_col(upra_raw, "ano", "^a(n|ñ)o$")
col_mes_u     <- pick_col(upra_raw, "mes", "mes")
col_dep_cod_u <- pick_col(upra_raw, "COD_DANE_DPTO_D", "DPTO|DEPTO|DANE.*DEP|COD.*DEP|DEPART")
col_dep_nom_u <- pick_col(upra_raw, "DEPARTAMENTO_D", "DEPARTA")
col_mun_cod_u <- pick_col(upra_raw, "COD_DANE_MUNIC_D", "MUNI.*COD|COD.*MUNI|DANE.*MUNI")
col_mun_nom_u <- pick_col(upra_raw, "MUNICIPIO_D", "MUNICIP")
col_area_mpio <- pick_col(upra_raw, "area_mpio_ha", "area.*mpio|mpio.*ha|area.*municip")
col_area_fa   <- pick_col(upra_raw, "area_fa_ha", "area.*fa.*ha|fa.*ha")
col_prop_fa   <- pick_col(upra_raw, "prop_fa", "prop.*fa|porc.*fa|particip.*fa")

upra <- tibble(
  fecha_completa = safe_pull(upra_raw, col_fecha_u),
  ano            = suppressWarnings(as.integer(safe_pull(upra_raw, col_ano_u))),
  mes            = suppressWarnings(as.integer(safe_pull(upra_raw, col_mes_u))),
  COD_DANE_DPTO  = norm_txt(safe_pull(upra_raw, col_dep_cod_u)),
  DEPARTAMENTO   = norm_txt(safe_pull(upra_raw, col_dep_nom_u)),
  COD_DANE_MUNI  = norm_txt(safe_pull(upra_raw, col_mun_cod_u)),
  MUNICIPIO      = norm_txt(safe_pull(upra_raw, col_mun_nom_u)),
  area_mpio_ha   = num_or_na(safe_pull(upra_raw, col_area_mpio)),
  area_fa_ha     = num_or_na(safe_pull(upra_raw, col_area_fa)),
  prop_fa        = suppressWarnings(as.numeric(safe_pull(upra_raw, col_prop_fa)))
)
if (max(upra$prop_fa, na.rm = TRUE) > 1.5) upra$prop_fa <- upra$prop_fa / 100
upra$COD_DANE_MUNI <- sprintf("%05s", gsub("\\D", "", upra$COD_DANE_MUNI))

col_ano_p   <- pick_col(pob_raw, "ano", "^a(n|ñ)o$|year")
col_mun_p   <- pick_col(pob_raw, "COD_DANE_MUNIC_D", "MUNI.*COD|COD.*MUNI|DANE.*MUNI|MPIO|CODMUN|COD_MUN5")
col_pob_tot <- pick_col(pob_raw, "poblacion", "poblaci(o|ó)n|total|p_total|POB|pob$")

pob <- tibble(
  ano           = suppressWarnings(as.integer(safe_pull(pob_raw, col_ano_p))),
  COD_DANE_MUNI = sprintf("%05s", gsub("\\D", "", norm_txt(safe_pull(pob_raw, col_mun_p)))),
  pob_total     = suppressWarnings(as.numeric(safe_pull(pob_raw, col_pob_tot)))
) %>% dplyr::group_by(ano, COD_DANE_MUNI) %>%
  dplyr::summarise(pob_total = sum(pob_total, na.rm = TRUE), .groups = "drop")

base_upra <- upra %>%
  dplyr::left_join(pob, by = c("ano", "COD_DANE_MUNI")) %>%
  dplyr::mutate(fa_per_capita_ha = dplyr::if_else(is.finite(area_fa_ha / pob_total), area_fa_ha / pob_total, NA_real_))

# ---------- Shapes ----------
mpios_raw <- sf::st_read(ruta_shp_mpios, quiet = TRUE)
dptos_raw <- sf::st_read(ruta_shp_dptos, quiet = TRUE)

mpios_sf <- mpios_raw %>%
  dplyr::mutate(
    COD_MUN5 = if ("MPIO_CDPMP" %in% names(.)) sprintf("%05d", as.integer(MPIO_CDPMP))
    else if ("COD_MPIO" %in% names(.)) sprintf("%05d", as.integer(COD_MPIO))
    else stop("Shp municipios: falta MPIO_CDPMP/COD_MPIO"),
    COD_DPTO2   = substr(COD_MUN5, 1, 2),
    MUNICIPIO_N = if ("MPIO_CNMBR" %in% names(.)) as.character(MPIO_CNMBR)
    else if ("NOMBRE_MPIO" %in% names(.)) as.character(NOMBRE_MPIO)
    else "MUNICIPIO"
  ) %>% sf::st_transform(4326) %>% sf::st_make_valid()

dptos_sf <- dptos_raw %>%
  dplyr::mutate(
    COD_DPTO2 = if ("DPTO_CCDGO" %in% names(.)) sprintf("%02d", as.integer(DPTO_CCDGO))
    else if ("COD_DEPTO" %in% names(.)) sprintf("%02d", as.integer(COD_DEPTO))
    else stop("Shp deptos: falta DPTO_CCDGO/COD_DEPTO"),
    DEPARTAMENTO_N = if ("DEPARTAMENTO_D" %in% names(.)) as.character(DEPARTAMENTO_D)
    else if ("DPTO_CNMBR" %in% names(.)) as.character(DPTO_CNMBR)
    else if ("NOMBRE_DEPTO" %in% names(.)) as.character(NOMBRE_DEPTO)
    else COD_DPTO2
  ) %>% sf::st_transform(4326) %>% sf::st_make_valid()

# Lookups (normalizados)
dpt_lookup <- base_upra %>% dplyr::select(COD_DANE_DPTO, DEPARTAMENTO) %>%
  dplyr::mutate(COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO)),
                DEP_NORM = NUP(DEPARTAMENTO)) %>%
  dplyr::distinct()
dptos_sf$DEP_NORM_SHP <- NUP(dptos_sf$DEPARTAMENTO_N)

# ---------- UI ----------
ui <- fluidPage(
  theme = bs_theme(
    version = 5, primary = "#2563eb",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    "border-radius" = "0.9rem", "font-size-base" = "0.98rem"
  ),
  tags$head(tags$style(HTML("
    .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
    h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
    .data-note{font-size:13px;color:#6b7280;margin:0 0 16px}
    .filters{background:#fff;border:1px solid #eaecef;border-radius:16px;padding:14px 16px;margin-bottom:16px;box-shadow:0 4px 14px rgba(0,0,0,.06)}
    .filters-grid{display:grid;grid-template-columns:repeat(6,minmax(180px,1fr));gap:12px}
    .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;text-transform:uppercase;color:#6b7280;margin-bottom:6px}
    .selectize-input,.form-control{min-height:42px;border-radius:10px}
    .selectize-input{padding:10px 12px}
    .card{background:#fff;border:1px solid #eaecef;border-radius:16px;padding:12px;box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px}
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
  "))),
  div(class="wrap",
      h3("UPRA — Frontera Agropecuaria"),
      div(class="data-note", HTML("Exploración de proporción y área de FA a nivel municipal / departamental.")),
      tabsetPanel(
        id = "tabs_upra", type = "tabs",
        tabPanel(
          "Exploración FA", br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter", div(class="filter-label","Año"), uiOutput("anio_ui")),
                  div(class="filter", div(class="filter-label","Departamento"),
                      selectInput("f_dep", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Municipio"),
                      selectInput("f_mun", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Indicador"),
                      selectInput("f_ind", NULL, choices = c("Proporción FA (0–1)" = "prop_fa",
                                                             "Área FA (ha)" = "area_fa_ha",
                                                             "FA por persona (ha/hab)" = "fa_per_capita_ha"),
                                  selected = "prop_fa")),
                  div(class="filter", div(class="filter-label","Filtro por proporción FA"),
                      sliderInput("f_prop", NULL, min=0, max=1, value=c(0,1), step=0.01)),
                  div(class="filter", div(class="filter-label","Acción"),
                      tagList(
                        actionLink("btn_reset","← Limpiar filtros"),
                        br(),
                        actionLink("btn_back_co","⤺ Volver a Colombia")
                      ))
              )
          ),
          fluidRow(
            column(6,
                   div(class="card",
                       div(class="card-title","Mapa — indicador seleccionado"),
                       leafletOutput("map_upra", height = 700)
                   )
            ),
            column(6,
                   div(class="card", div(class="card-title","Top-10 municipios por indicador"),
                       plotlyOutput("bar_top", height = 310)),
                   div(class="card", div(class="card-title","Promedio por departamento (indicador)"),
                       plotlyOutput("bar_depto", height = 310))
            )
          )
        )
      )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session){
  
  # Placeholder vacío para plotly
  empty_plot <- function(texto = "Sin datos para los filtros seleccionados.") {
    plotly::plotly_empty(type = "scatter", mode = "markers") %>%
      plotly::layout(
        annotations = list(x = 0.5, y = 0.5, text = texto, showarrow = FALSE,
                           xref = "paper", yref = "paper", font = list(size = 14)),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        margin = list(l=10, r=10, b=10, t=10)
      )
  }
  
  # Año UI
  output$anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(base_upra$ano)))
    selectInput("anio", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  # Combos dependientes
  observeEvent(input$anio, {
    df <- base_upra %>% dplyr::filter(ano == input$anio)
    deps <- df %>% dplyr::distinct(DEPARTAMENTO) %>% dplyr::arrange(DEPARTAMENTO) %>% dplyr::pull()
    sel <- if (!is.null(input$f_dep) && input$f_dep %in% deps) input$f_dep else "Todos"
    updateSelectInput(session, "f_dep", choices = c("Todos", deps), selected = sel)
    updateSelectInput(session, "f_mun", choices = "Todos", selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(input$f_dep, {
    df <- base_upra %>% dplyr::filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") {
      mpios <- df %>% dplyr::filter(DEPARTAMENTO == input$f_dep) %>%
        dplyr::distinct(MUNICIPIO) %>% dplyr::arrange(MUNICIPIO) %>% dplyr::pull()
      updateSelectInput(session, "f_mun", choices = c("Todos", mpios), selected = "Todos")
    } else {
      updateSelectInput(session, "f_mun", choices = "Todos", selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_reset, {
    yrs <- sort(unique(na.omit(base_upra$ano)))
    updateSelectInput(session, "anio", selected = max(yrs, na.rm = TRUE))
    updateSelectInput(session, "f_dep", selected = "Todos")
    updateSelectInput(session, "f_mun", selected = "Todos")
    updateSelectInput(session, "f_ind", selected = "prop_fa")
    updateSliderInput(session, "f_prop", value = c(0,1))
  })
  observeEvent(input$btn_back_co, { updateSelectInput(session, "f_dep", selected = "Todos") })
  
  # Base filtrada
  base_filtrada <- reactive({
    req(input$anio)
    d <- base_upra %>% dplyr::filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") d <- d %>% dplyr::filter(DEPARTAMENTO == input$f_dep)
    if (!is.null(input$f_mun) && input$f_mun != "Todos") d <- d %>% dplyr::filter(MUNICIPIO == input$f_mun)
    d %>% dplyr::filter(prop_fa >= input$f_prop[1], prop_fa <= input$f_prop[2])
  })
  
  # Mapa base
  output$map_upra <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74.3, 4.6, 5)
  })
  
  # Helper: código dpto desde nombre
  get_cod_from_dep_name <- function(dep_name){
    if (is.null(dep_name) || dep_name == "Todos") return(NA_character_)
    dep_norm <- NUP(dep_name)
    i <- which(dptos_sf$DEP_NORM_SHP == dep_norm)[1]
    if (is.finite(i)) return(dptos_sf$COD_DPTO2[i])
    j <- which(dpt_lookup$DEP_NORM == dep_norm)[1]
    if (is.finite(j)) return(dpt_lookup$COD_DPTO2[j])
    NA_character_
  }
  
  # convertir nombre shapefile -> nombre combo
  map_shp_dep_to_base <- function(nom_shp) {
    dep_norm <- NUP(nom_shp)
    match_txt <- dpt_lookup$DEPARTAMENTO[match(dep_norm, dpt_lookup$DEP_NORM)]
    ifelse(is.na(match_txt) | !nzchar(match_txt), nom_shp, match_txt)
  }
  
  # Mapa (deptos -> mpios) sin left_join sobre sf
  observe({
    d <- base_filtrada()
    if (is.null(d) || nrow(d) == 0) {
      leafletProxy("map_upra") %>% clearShapes() %>% clearControls()
      return(invisible(NULL))
    }
    
    ind <- input$f_ind
    label <- switch(ind, "prop_fa"="Proporción FA", "area_fa_ha"="Área FA (ha)", "fa_per_capita_ha"="FA por persona (ha/hab)")
    
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      dd <- d %>% dplyr::group_by(COD_DANE_DPTO, DEPARTAMENTO) %>%
        dplyr::summarise(
          valor = if (ind == "area_fa_ha") sum(.data[[ind]], na.rm=TRUE) else mean(.data[[ind]], na.rm=TRUE),
          .groups="drop"
        ) %>%
        dplyr::mutate(COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO)))
      
      shp <- dptos_sf
      idx <- match(shp$COD_DPTO2, dd$COD_DPTO2)
      shp$valor <- dd$valor[idx]
      shp$DEPARTAMENTO <- dd$DEPARTAMENTO[idx]
      shp$nombre <- dplyr::coalesce(shp$DEPARTAMENTO, shp$DEPARTAMENTO_N)
      shp$etq <- paste0(
        "<b>", shp$nombre, "</b><br>", label, ": ",
        ifelse(ind=="prop_fa", fmt_pct(shp$valor),
               ifelse(ind=="area_fa_ha", fmt_num(shp$valor, 1),
                      format(round(shp$valor,6), big.mark=",")))
      )
      
      pal <- make_pal_bin(shp$valor, "YlGnBu")
      leafletProxy("map_upra", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(layerId=~COD_DPTO2, fillColor=~pal(valor), color="#666", weight=0.7, fillOpacity=0.9,
                    label=~lapply(etq, HTML),
                    highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        addLegend("bottomright", pal = pal, values = ~valor, title = label)
      
    } else {
      sel_cod <- get_cod_from_dep_name(input$f_dep)
      if (is.na(sel_cod) || !nzchar(sel_cod)) {
        sel_cod <- d$COD_DANE_DPTO %>% unique() %>% sprintf("%02d", as.integer(.)) %>% .[1]
      }
      
      dd <- d %>%
        dplyr::mutate(COD_MUN5 = sprintf("%05d", as.integer(COD_DANE_MUNI))) %>%
        dplyr::group_by(COD_DANE_MUNI, MUNICIPIO) %>%
        dplyr::summarise(
          valor = if (ind == "area_fa_ha") sum(.data[[ind]], na.rm=TRUE) else mean(.data[[ind]], na.rm=TRUE),
          area_fa_ha = sum(area_fa_ha, na.rm = TRUE),
          prop_fa = mean(prop_fa, na.rm = TRUE),
          pob_total = mean(pob_total, na.rm = TRUE),
          fa_per_capita_ha = mean(fa_per_capita_ha, na.rm = TRUE),
          .groups = "drop"
        ) %>% dplyr::mutate(COD_MUN5 = sprintf("%05d", as.integer(COD_DANE_MUNI)))
      
      shp <- mpios_sf %>% dplyr::filter(COD_DPTO2 == sel_cod)
      idx <- match(shp$COD_MUN5, sprintf("%05d", as.integer(dd$COD_DANE_MUNI)))
      shp$valor <- dd$valor[idx]
      shp$MUNICIPIO <- dd$MUNICIPIO[idx]
      shp$area_fa_ha <- dd$area_fa_ha[idx]
      shp$prop_fa <- dd$prop_fa[idx]
      shp$pob_total <- dd$pob_total[idx]
      shp$fa_per_capita_ha <- dd$fa_per_capita_ha[idx]
      
      shp$MUNICIPIO <- dplyr::coalesce(shp$MUNICIPIO, shp$MUNICIPIO_N)
      shp$etq <- paste0(
        "<b>", shp$MUNICIPIO, "</b><br>", label, ": ",
        ifelse(ind=="prop_fa", fmt_pct(shp$valor),
               ifelse(ind=="area_fa_ha", fmt_num(shp$valor, 1),
                      format(round(shp$valor,6), big.mark=","))),
        "<br>Área FA (ha): ", fmt_num(shp$area_fa_ha, 1),
        "<br>Proporción FA: ", fmt_pct(shp$prop_fa),
        "<br>Población: ", fmt_num(shp$pob_total, 1),
        "<br>FA por persona: ", ifelse(is.na(shp$fa_per_capita_ha), "NA", format(round(shp$fa_per_capita_ha,6), big.mark=","))
      )
      
      pal <- make_pal_bin(shp$valor, "YlOrRd"); bb <- sf::st_bbox(shp)
      
      leafletProxy("map_upra", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(layerId=~COD_MUN5, fillColor=~pal(valor), color="#666", weight=0.4, fillOpacity=0.9,
                    label=~lapply(etq, HTML),
                    highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        addLegend("bottomright", pal = pal, values = ~valor, title = label) %>%
        fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  # Click en dpto (drill-down)
  observeEvent(input$map_upra_shape_click, {
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      click <- input$map_upra_shape_click; req(click$id)
      cod <- sprintf("%02d", as.integer(click$id))
      nom_shp <- dptos_sf$DEPARTAMENTO_N[match(cod, dptos_sf$COD_DPTO2)]
      nom_combo <- map_shp_dep_to_base(nom_shp)
      if (!is.na(nom_combo) && nzchar(nom_combo)) updateSelectInput(session, "f_dep", selected = nom_combo)
    }
  }, ignoreInit = TRUE)
  
  # Top-10 municipios
  output$bar_top <- renderPlotly({
    d <- base_filtrada()
    if (is.null(d) || NROW(d) == 0) return(empty_plot())
    
    ind <- input$f_ind
    d1 <- d %>%
      dplyr::group_by(DEPARTAMENTO, MUNICIPIO) %>%
      dplyr::summarise(
        val = if (ind == "area_fa_ha") sum(.data[[ind]], na.rm = TRUE) else mean(.data[[ind]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(val)) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::mutate(lbl = paste0(MUNICIPIO, " (", DEPARTAMENTO, ")"))
    
    if (!NROW(d1) || all(!is.finite(d1$val))) return(empty_plot())
    
    plotly::plot_ly(d1 %>% dplyr::arrange(val),
                    x = ~val,
                    y = ~factor(lbl, levels = d1$lbl[order(d1$val)]),
                    type = "bar", orientation = "h",
                    hovertemplate = "%{y}<br>Valor: %{x:,.6f}<extra></extra>"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Indicador"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l=10, r=10, b=10, t=10)
      )
  })
  
  # Promedio por dpto
  output$bar_depto <- renderPlotly({
    d <- base_filtrada()
    if (is.null(d) || NROW(d) == 0) return(empty_plot())
    
    ind <- input$f_ind
    d2 <- d %>%
      dplyr::group_by(DEPARTAMENTO) %>%
      dplyr::summarise(val = mean(.data[[ind]], na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(val)) %>%
      dplyr::slice_head(n = 15)
    
    if (!NROW(d2) || all(!is.finite(d2$val))) return(empty_plot())
    
    plotly::plot_ly(d2 %>% dplyr::arrange(val),
                    x = ~val,
                    y = ~factor(DEPARTAMENTO, levels = d2$DEPARTAMENTO[order(d2$val)]),
                    type = "bar", orientation = "h",
                    hovertemplate = "%{y}<br>Promedio: %{x:,.6f}<extra></extra>"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Promedio del indicador"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l=10, r=10, b=10, t=10)
      )
  })
}

shinyApp(ui, server)

