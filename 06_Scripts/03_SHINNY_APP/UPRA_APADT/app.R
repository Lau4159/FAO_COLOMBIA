# app_apadt.R — UPRA APADT (3 cuadrantes, estilo BPAN)
suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(leaflet); library(sf); library(dplyr); library(tidyr)
  library(scales); library(htmltools); library(plotly)
  library(stringi); library(readr)
})
options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# ---------- Rutas ----------
local_data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/UPRA_APADT/data"
app_root     <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
rel_data_dir <- file.path(app_root, "data")
data_dir     <- if (dir.exists(rel_data_dir)) rel_data_dir else local_data_dir

ruta_apadt    <- file.path(data_dir, "014_UPRA_APADT.rds")
ruta_pob      <- file.path(data_dir, "051_DANE_Proyecciones_P_total.rds")
ruta_shp_mun  <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")
ruta_shp_dep  <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

must_exist <- c(ruta_apadt, ruta_pob, ruta_shp_mun, ruta_shp_dep)
miss <- must_exist[!file.exists(must_exist)]
if (length(miss)) stop("Faltan archivos. data_dir: ", data_dir, "\n", paste("-", miss, collapse = "\n"))
check_shp_parts <- function(shp){ b <- sub("\\.shp$", "", shp); req <- paste0(b, c(".shp",".dbf",".shx",".prj")); req[!file.exists(req)] }
miss_shp <- c(check_shp_parts(ruta_shp_mun), check_shp_parts(ruta_shp_dep))
if (length(miss_shp)) stop("Faltan componentes de shapefile:\n", paste("-", miss_shp, collapse = "\n"))

# ---------- Utils ----------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
NUP      <- function(x) toupper(norm_txt(x))
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

# ---------- Cargar APADT + población ----------
apadt_raw <- readRDS(ruta_apadt)
pob_raw   <- readRDS(ruta_pob)

# Mapear columnas de la base APADT (tolerante a nombres)
col_ano     <- pick_col(apadt_raw, "ano", "^a(n|ñ)o$|year")
col_mes     <- pick_col(apadt_raw, "mes", "mes")
col_dep_cod <- pick_col(apadt_raw, "COD_DANE_DPTO_D", "DPTO|DEPTO|DANE.*DEP|COD.*DEP|DEPART")
col_dep_nom <- pick_col(apadt_raw, "DEPARTAMENTO_D", "DEPARTA")
col_mun_cod <- pick_col(apadt_raw, "COD_DANE_MUNIC_D", "MUNI.*COD|COD.*MUNI|DANE.*MUNI|COD_MUN5|MPIO")
col_mun_nom <- pick_col(apadt_raw, "MUNICIPIO_D", "MUNICIP")

# área municipal (ha)
col_area_mpio <- pick_col(apadt_raw, "area_mpio_ha", "area.*mpio.*ha|mpio.*ha|area.*municip.*ha")
# área de APADT (ha)
col_area_ind  <- pick_col(apadt_raw, "area_apadt_ha", "area.*(apadt|apt|apto).*ha|area.*aprovech.*ha|ha_apadt|apadt_ha")
# proporción APADT (0–1 ó 0–100)
col_prop_ind  <- pick_col(apadt_raw, "prop_apadt", "prop.*(apadt|apt|apto)|porc.*(apadt|apt)")

if (is.na(col_area_ind) & is.na(col_prop_ind)) {
  stop("No se encontraron columnas de indicador (área/proporción) en 014_UPRA_APADT.rds")
}

apadt <- tibble::tibble(
  ano            = suppressWarnings(as.integer(safe_pull(apadt_raw, col_ano))),
  mes            = suppressWarnings(as.integer(safe_pull(apadt_raw, col_mes))),
  COD_DANE_DPTO  = sprintf("%02s", gsub("\\D","", norm_txt(safe_pull(apadt_raw, col_dep_cod)))),
  DEPARTAMENTO   = norm_txt(safe_pull(apadt_raw, col_dep_nom)),
  COD_DANE_MUNI  = sprintf("%05s", gsub("\\D","", norm_txt(safe_pull(apadt_raw, col_mun_cod)))),
  MUNICIPIO      = norm_txt(safe_pull(apadt_raw, col_mun_nom)),
  area_mpio_ha   = num_or_na(safe_pull(apadt_raw, col_area_mpio)),
  area_ind_ha    = num_or_na(safe_pull(apadt_raw, col_area_ind)),
  prop_ind       = suppressWarnings(as.numeric(safe_pull(apadt_raw, col_prop_ind)))
)

# Normalizar proporción a 0–1 si viniera 0–100
if (!all(is.na(apadt$prop_ind))) {
  mx <- suppressWarnings(max(apadt$prop_ind, na.rm = TRUE))
  if (is.finite(mx) && mx > 1.5) apadt$prop_ind <- apadt$prop_ind / 100
}

# Población
col_ano_p   <- pick_col(pob_raw, "ano", "^a(n|ñ)o$|year")
col_mun_p   <- pick_col(pob_raw, "COD_DANE_MUNIC_D", "MUNI.*COD|COD.*MUNI|DANE.*MUNI|MPIO|CODMUN|COD_MUN5")
col_pob_tot <- pick_col(pob_raw, "poblacion", "poblaci(o|ó)n|total|p_total|POB|pob$")

pob <- tibble::tibble(
  ano           = suppressWarnings(as.integer(safe_pull(pob_raw, col_ano_p))),
  COD_DANE_MUNI = sprintf("%05s", gsub("\\D", "", norm_txt(safe_pull(pob_raw, col_mun_p)))),
  pob_total     = suppressWarnings(as.numeric(safe_pull(pob_raw, col_pob_tot)))
) %>%
  dplyr::group_by(ano, COD_DANE_MUNI) %>%
  dplyr::summarise(pob_total = sum(pob_total, na.rm = TRUE), .groups = "drop")

base <- apadt %>%
  left_join(pob, by = c("ano","COD_DANE_MUNI")) %>%
  mutate(
    # si no venía proporción, la calculamos con área_ind_ha / área_mpio_ha
    prop_ind = dplyr::coalesce(prop_ind, if_else(area_mpio_ha > 0, area_ind_ha/area_mpio_ha, NA_real_)),
    ind_per_capita_ha = if_else(is.finite(area_ind_ha / pob_total), area_ind_ha / pob_total, NA_real_)
  )

# ---------- Shapes ----------
mun_raw <- sf::st_read(ruta_shp_mun, quiet = TRUE)
dep_raw <- sf::st_read(ruta_shp_dep, quiet = TRUE)

mun_sf <- mun_raw %>%
  mutate(
    COD_MUN5 = if ("MPIO_CDPMP" %in% names(.)) sprintf("%05d", as.integer(MPIO_CDPMP))
    else if ("COD_MPIO" %in% names(.)) sprintf("%05d", as.integer(COD_MPIO))
    else stop("Shp municipios: falta MPIO_CDPMP/COD_MPIO"),
    COD_DPTO2   = substr(COD_MUN5, 1, 2),
    MUNICIPIO_N = if ("MPIO_CNMBR" %in% names(.)) as.character(MPIO_CNMBR)
    else if ("NOMBRE_MPIO" %in% names(.)) as.character(NOMBRE_MPIO)
    else "MUNICIPIO"
  ) %>% st_transform(4326) %>% st_make_valid()

dep_sf <- dep_raw %>%
  mutate(
    COD_DPTO2 = if ("DPTO_CCDGO" %in% names(.)) sprintf("%02d", as.integer(DPTO_CCDGO))
    else if ("COD_DEPTO" %in% names(.)) sprintf("%02d", as.integer(COD_DEPTO))
    else stop("Shp deptos: falta DPTO_CCDGO/COD_DEPTO"),
    DEPARTAMENTO_N = if ("DEPARTAMENTO_D" %in% names(.)) as.character(DEPARTAMENTO_D)
    else if ("DPTO_CNMBR" %in% names(.)) as.character(DPTO_CNMBR)
    else if ("NOMBRE_DEPTO" %in% names(.)) as.character(NOMBRE_DEPTO)
    else COD_DPTO2
  ) %>% st_transform(4326) %>% st_make_valid()

# Lookups
dep_lookup <- base %>% select(COD_DANE_DPTO, DEPARTAMENTO) %>%
  mutate(COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO)),
         DEP_NORM = NUP(DEPARTAMENTO)) %>%
  distinct()
dep_sf$DEP_NORM_SHP <- NUP(dep_sf$DEPARTAMENTO_N)

# ---------- Indicadores disponibles ----------
inds <- list()
inds[["Proporción APADT (0–1)"]] <- "prop_ind"
if (!all(is.na(base$area_ind_ha)))         inds[["Área APADT (ha)"]] <- "area_ind_ha"
if (!all(is.na(base$ind_per_capita_ha)))   inds[["APADT por persona (ha/hab)"]] <- "ind_per_capita_ha"

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
      h3("UPRA — APADT"),
      div(class="data-note","Exploración de proporción y área de APADT a nivel municipal / departamental."),
      tabsetPanel(
        id = "tabs_apadt", type = "tabs",
        tabPanel(
          "Exploración APADT", br(),
          div(class="filters",
              div(class="filters-grid",
                  div(class="filter", div(class="filter-label","Año"), uiOutput("anio_ui")),
                  div(class="filter", div(class="filter-label","Departamento"),
                      selectInput("f_dep", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Municipio"),
                      selectInput("f_mun", NULL, choices = "Todos", selected = "Todos")),
                  div(class="filter", div(class="filter-label","Indicador"),
                      selectInput("f_ind", NULL, choices = inds, selected = names(inds)[1] %||% NULL)),
                  div(class="filter", div(class="filter-label","Filtro por proporción"),
                      uiOutput("prop_slider_ui")),
                  div(class="filter", div(class="filter-label","Acción"),
                      tagList(
                        actionLink("btn_reset","← Limpiar filtros"),
                        br(), actionLink("btn_back_co","⤺ Volver a Colombia")
                      ))
              )
          ),
          fluidRow(
            column(6,
                   div(class="card",
                       div(class="card-title","Mapa — indicador seleccionado"),
                       leafletOutput("map_apadt", height = 700)
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
  
  # Año UI
  output$anio_ui <- renderUI({
    yrs <- sort(unique(na.omit(base$ano)))
    selectInput("anio", NULL, choices = yrs, selected = max(yrs, na.rm = TRUE))
  })
  
  # Slider de proporción: sólo si el indicador es prop_ind
  output$prop_slider_ui <- renderUI({
    if (is.null(input$f_ind)) return(NULL)
    if (input$f_ind == "prop_ind") sliderInput("f_prop", NULL, min=0, max=1, value=c(0,1), step=0.01)
    else div(style="padding-top:10px;color:#6b7280;","—")
  })
  
  # Combos dependientes
  observeEvent(input$anio, {
    df <- base %>% filter(ano == input$anio)
    deps <- df %>% distinct(DEPARTAMENTO) %>% arrange(DEPARTAMENTO) %>% pull()
    sel <- if (!is.null(input$f_dep) && input$f_dep %in% deps) input$f_dep else "Todos"
    updateSelectInput(session, "f_dep", choices = c("Todos", deps), selected = sel)
    updateSelectInput(session, "f_mun", choices = "Todos", selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(input$f_dep, {
    df <- base %>% filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") {
      mpios <- df %>% filter(DEPARTAMENTO == input$f_dep) %>% distinct(MUNICIPIO) %>%
        arrange(MUNICIPIO) %>% pull()
      updateSelectInput(session, "f_mun", choices = c("Todos", mpios), selected = "Todos")
    } else {
      updateSelectInput(session, "f_mun", choices = "Todos", selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_reset, {
    yrs <- sort(unique(na.omit(base$ano)))
    updateSelectInput(session, "anio", selected = max(yrs, na.rm = TRUE))
    updateSelectInput(session, "f_dep", selected = "Todos")
    updateSelectInput(session, "f_mun", selected = "Todos")
    updateSelectInput(session, "f_ind", selected = names(inds)[1] %||% NULL)
    if (!is.null(input$f_prop)) updateSliderInput(session, "f_prop", value = c(0,1))
  })
  observeEvent(input$btn_back_co, { updateSelectInput(session, "f_dep", selected = "Todos") })
  
  # Base filtrada
  base_filtrada <- reactive({
    req(input$anio)
    d <- base %>% filter(ano == input$anio)
    if (!is.null(input$f_dep) && input$f_dep != "Todos") d <- d %>% filter(DEPARTAMENTO == input$f_dep)
    if (!is.null(input$f_mun) && input$f_mun != "Todos") d <- d %>% filter(MUNICIPIO == input$f_mun)
    if (!is.null(input$f_ind) && input$f_ind == "prop_ind" && !is.null(input$f_prop)) {
      d <- d %>% filter(prop_ind >= input$f_prop[1], prop_ind <= input$f_prop[2])
    }
    d
  })
  
  # Mapa base
  output$map_apadt <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74.3, 4.6, 5)
  })
  
  # Helper: código dpto por nombre (normalizado)
  get_cod_from_dep_name <- function(dep_name){
    if (is.null(dep_name) || dep_name == "Todos") return(NA_character_)
    dep_norm <- NUP(dep_name)
    i <- which(dep_sf$DEP_NORM_SHP == dep_norm)[1]
    if (is.finite(i)) return(dep_sf$COD_DPTO2[i])
    j <- which(dep_lookup$DEP_NORM == dep_norm)[1]
    if (is.finite(j)) return(dep_lookup$COD_DPTO2[j])
    NA_character_
  }
  
  # Convertir nombre del shapefile al que existe en el combo
  map_shp_dep_to_base <- function(nom_shp) {
    dep_norm <- NUP(nom_shp)
    match_txt <- dep_lookup$DEPARTAMENTO[match(dep_norm, dep_lookup$DEP_NORM)]
    ifelse(is.na(match_txt) | !nzchar(match_txt), nom_shp, match_txt)
  }
  
  # Mapa (deptos -> mpios)
  observe({
    d <- base_filtrada()
    if (is.null(d) || nrow(d) == 0) {
      leafletProxy("map_apadt") %>% clearShapes() %>% clearControls()
      return(invisible(NULL))
    }
    
    ind_key <- input$f_ind %||% names(inds)[1]
    # traducir etiqueta
    label <- switch(input$f_ind,
                    "prop_ind"            = "Proporción APADT",
                    "area_ind_ha"         = "Área APADT (ha)",
                    "ind_per_capita_ha"   = "APADT por persona (ha/hab)",
                    "Indicador"
    )
    ind <- input$f_ind
    
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      dd <- d %>% group_by(COD_DANE_DPTO, DEPARTAMENTO) %>%
        summarise(
          valor = if (ind == "area_ind_ha") sum(.data[[ind]], na.rm=TRUE) else mean(.data[[ind]], na.rm=TRUE),
          .groups="drop"
        ) %>%
        mutate(
          # <- PARCHE: crear COD_DPTO2 sin forzar a integer (evita NAs por coerción)
          COD_DPTO2 = dplyr::if_else(
            !is.na(COD_DANE_DPTO) & nzchar(COD_DANE_DPTO) & nchar(COD_DANE_DPTO) %in% c(1,2),
            sprintf("%02s", COD_DANE_DPTO),
            sprintf("%02d", suppressWarnings(as.integer(COD_DANE_DPTO)))
          )
        )
      
      shp <- dep_sf %>%
        dplyr::left_join(dd %>% dplyr::select(COD_DPTO2, valor, DEPARTAMENTO), by="COD_DPTO2") %>%
        dplyr::mutate(
          nombre = dplyr::coalesce(DEPARTAMENTO, DEPARTAMENTO_N, COD_DPTO2),
          etq = paste0(
            "<b>", nombre, "</b><br>", label, ": ",
            dplyr::case_when(
              ind == "prop_ind" ~ scales::percent(valor, 0.1),
              ind == "area_ind_ha" ~ scales::comma(valor, 1),
              TRUE ~ format(round(valor, 6), big.mark = ",")
            )
          )
        )
      
      pal <- make_pal_bin(shp$valor, "YlGnBu")
      leafletProxy("map_apadt", data = shp) %>%
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
        mutate(COD_MUN5 = sprintf("%05d", as.integer(COD_DANE_MUNI))) %>%
        group_by(COD_DANE_MUNI, MUNICIPIO) %>%
        summarise(
          valor = if (ind == "area_ind_ha") sum(.data[[ind]], na.rm=TRUE) else mean(.data[[ind]], na.rm=TRUE),
          area_ind_ha = sum(area_ind_ha, na.rm = TRUE),
          prop_ind = mean(prop_ind, na.rm = TRUE),
          pob_total = mean(pob_total, na.rm = TRUE),
          ind_per_capita_ha = mean(ind_per_capita_ha, na.rm = TRUE),
          .groups = "drop"
        ) %>% mutate(COD_MUN5 = sprintf("%05d", as.integer(COD_DANE_MUNI)))
      
      shp <- mun_sf %>% filter(COD_DPTO2 == sel_cod) %>%
        left_join(dd %>% select(COD_MUN5, MUNICIPIO, valor, area_ind_ha, prop_ind, ind_per_capita_ha, pob_total), by="COD_MUN5") %>%
        mutate(MUNICIPIO = coalesce(MUNICIPIO, MUNICIPIO_N),
               etq = paste0("<b>", MUNICIPIO, "</b><br>", label, ": ",
                            if (ind=="prop_ind") percent(valor,0.1) else if (ind=="area_ind_ha") comma(valor,1) else format(round(valor,6), big.mark=","),
                            "<br>Área APADT (ha): ", comma(area_ind_ha,1),
                            "<br>Proporción APADT: ", ifelse(is.na(prop_ind),"NA", percent(prop_ind,0.01)),
                            "<br>Población: ", comma(pob_total),
                            "<br>APADT por persona: ", ifelse(is.na(ind_per_capita_ha),"NA", format(round(ind_per_capita_ha,6), big.mark=","))))
      pal <- make_pal_bin(shp$valor, "YlOrRd"); bb <- sf::st_bbox(shp)
      
      leafletProxy("map_apadt", data = shp) %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(layerId=~COD_MUN5, fillColor=~pal(valor), color="#666", weight=0.4, fillOpacity=0.9,
                    label=~lapply(etq, HTML),
                    highlightOptions = highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
        addLegend("bottomright", pal = pal, values = ~valor, title = label) %>%
        fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
  })
  
  # Click depto -> drill-down (match por código para evitar problemas de nombre)
  observeEvent(input$map_apadt_shape_click, {
    if (is.null(input$f_dep) || input$f_dep == "Todos") {
      click <- input$map_apadt_shape_click; req(click$id)
      cod <- sprintf("%02d", as.integer(click$id))
      nom_base <- dep_lookup$DEPARTAMENTO[match(cod, dep_lookup$COD_DPTO2)]
      if (!is.na(nom_base) && nzchar(nom_base)) {
        updateSelectInput(session, "f_dep", selected = nom_base)
      }
    }
  }, ignoreInit = TRUE)
  
  # Top-10 municipios (validación robusta sin validate())
  output$bar_top <- renderPlotly({
    d <- base_filtrada()
    req(!is.null(d), nrow(d) > 0)
    
    ind <- input$f_ind %||% "prop_ind"
    
    d1 <- d %>%
      dplyr::group_by(DEPARTAMENTO, MUNICIPIO) %>%
      dplyr::summarise(
        val = if (ind == "area_ind_ha") sum(.data[[ind]], na.rm = TRUE) else mean(.data[[ind]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(val)) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::mutate(lbl = paste0(MUNICIPIO, " (", DEPARTAMENTO, ")"))
    
    if (nrow(d1) == 0) {
      return(htmltools::HTML("<div style='padding:8px;color:#6b7280'>Sin datos para los filtros seleccionados.</div>"))
    }
    
    plotly::plot_ly(
      d1 %>% dplyr::arrange(val),
      x = ~val,
      y = ~factor(lbl, levels = d1$lbl[order(d1$val)]),
      type = "bar",
      orientation = "h",
      hovertemplate = "%{y}<br>Valor: %{x:,.6f}<extra></extra>"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Indicador"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 10, r = 10, b = 10, t = 10)
      )
  })
  
  # Promedio por dpto (validación robusta sin validate())
  output$bar_depto <- renderPlotly({
    d <- base_filtrada()
    req(!is.null(d), nrow(d) > 0)
    
    ind <- input$f_ind %||% "prop_ind"
    
    d2 <- d %>%
      dplyr::group_by(DEPARTAMENTO) %>%
      dplyr::summarise(val = mean(.data[[ind]], na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(val)) %>%
      dplyr::slice_head(n = 15)
    
    if (nrow(d2) == 0) {
      return(htmltools::HTML("<div style='padding:8px;color:#6b7280'>Sin datos para los filtros seleccionados.</div>"))
    }
    
    plotly::plot_ly(
      d2 %>% dplyr::arrange(val),
      x = ~val,
      y = ~factor(DEPARTAMENTO, levels = d2$DEPARTAMENTO[order(d2$val)]),
      type = "bar",
      orientation = "h",
      hovertemplate = "%{y}<br>Promedio: %{x:,.6f}<extra></extra>"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Promedio del indicador"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 10, r = 10, b = 10, t = 10)
      )
  })
}

shinyApp(ui, server)
