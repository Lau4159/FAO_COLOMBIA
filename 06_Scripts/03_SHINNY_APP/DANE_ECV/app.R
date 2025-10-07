# app.R — ECV Consumo — 2 Tabs:
# 1) Binarios (p1707, p3003)  2) Frecuencias por categoría como indicadores propios

suppressWarnings({
  library(shiny); library(bslib); library(shinyWidgets)
  library(dplyr); library(tidyr); library(readr); library(stringi); library(scales)
  library(leaflet); library(sf); library(htmltools); library(plotly)
  library(haven)
})
options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# ---------------- Rutas ----------------
app_root <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
data_dir <- file.path(app_root, "data")
ruta_ecv     <- file.path(data_dir, "052_DANE_ECV.rds")
ruta_shp_dep <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")

if (!file.exists(ruta_ecv)) stop("No se encuentra el archivo: ", ruta_ecv)
chk <- function(shp){ b <- sub("\\.shp$","",shp); req <- paste0(b,c(".shp",".dbf",".shx",".prj")); req[!file.exists(req)] }
if (length(chk(ruta_shp_dep))) stop("Faltan partes del SHP deptos en /data/shp (shp/dbf/shx/prj)")

# ---------------- Utils ----------------
`%||%` <- function(x,y) if (is.null(x)||length(x)==0) y else x
norm_txt <- function(x) stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII")

safe_as_char <- function(x){
  if (inherits(x, "haven_labelled")) {
    as.character(haven::as_factor(x, levels = "labels", ordered = FALSE))
  } else {
    as.character(x)
  }
}
safe_as_num <- function(x){
  if (inherits(x, "haven_labelled")) {
    suppressWarnings(as.numeric(haven::zap_labels(x)))
  } else if (is.numeric(x)) {
    as.numeric(x)
  } else {
    suppressWarnings(as.numeric(readr::parse_number(as.character(x))))
  }
}
numish <- function(x) safe_as_num(x)

w_prop <- function(x, w){
  ok <- is.finite(x) & is.finite(w) & w>0
  if(!any(ok)) return(NA_real_)
  sum(w[ok]*x[ok]) / sum(w[ok])
}
norm_dep2 <- function(x){
  x <- gsub("\\D","",as.character(x)); x[nchar(x)==0] <- NA
  stringi::stri_pad_left(x,2,"0")
}
norm_mun5 <- function(x){
  x <- gsub("\\D","",as.character(x)); x[nchar(x)==0] <- NA
  stringi::stri_pad_left(x,5,"0")
}
make_pal_bin <- function(values, palette="YlGnBu", n_bins=6){
  vals <- suppressWarnings(as.numeric(values)); vals <- vals[is.finite(vals)]
  if (!length(vals)) vals <- 0
  qs <- stats::quantile(vals, probs = seq(0,1,length.out=n_bins), na.rm=TRUE)
  qs <- unique(as.numeric(qs)); if (length(qs)<3) qs <- pretty(vals, n=n_bins)
  bins <- sort(unique(c(min(vals,na.rm=TRUE), qs, max(vals,na.rm=TRUE))))
  leaflet::colorBin(palette, domain = vals, bins = bins, na.color = "#f0f0f0")
}

# ---------------- Cargar base ----------------
ecv_raw <- readRDS(ruta_ecv)
names(ecv_raw) <- tolower(names(ecv_raw))

col_ano     <- if ("anio" %in% names(ecv_raw)) "anio" else names(ecv_raw)[grepl("^a(n|ñ)o$|year", names(ecv_raw), TRUE)][1]
col_dep_cod <- if ("cod_dane_dpto_d" %in% names(ecv_raw)) "cod_dane_dpto_d" else names(ecv_raw)[grepl("cod.*dane.*dpto|dpto.*(ccdgo|cod)|cod.*dep", names(ecv_raw), TRUE)][1]
col_dep_nom <- if ("departamento_d" %in% names(ecv_raw)) "departamento_d" else names(ecv_raw)[grepl("^departa", names(ecv_raw), TRUE)][1]
col_mun_cod <- if ("cod_dane_mp_d" %in% names(ecv_raw)) "cod_dane_mp_d" else names(ecv_raw)[grepl("cod.*dane.*(mpio|mun)|mpio.*(ccdgo|cod)|cod.*muni", names(ecv_raw), TRUE)][1]
col_mun_nom <- if ("municipio_d" %in% names(ecv_raw)) "municipio_d" else names(ecv_raw)[grepl("^muni|municipio", names(ecv_raw), TRUE)][1]
col_clase   <- if ("clase" %in% names(ecv_raw)) "clase" else names(ecv_raw)[grepl("^clase$|^area$|urb|rur", names(ecv_raw), TRUE)][1]
col_sexo    <- if ("p6020" %in% names(ecv_raw)) "p6020" else names(ecv_raw)[grepl("^p?6020|sexo", names(ecv_raw), TRUE)][1]
col_edad    <- if ("p6040" %in% names(ecv_raw)) "p6040" else names(ecv_raw)[grepl("^p?6040|edad", names(ecv_raw), TRUE)][1]
col_w       <- if ("fex_c.x" %in% names(ecv_raw)) "fex_c.x" else names(ecv_raw)[grepl("^fex|factor|pondera|expans", names(ecv_raw), TRUE)][1]

need <- c(col_ano,col_dep_cod,col_dep_nom,col_sexo,col_edad,col_w)
if (any(is.na(need))) stop("Faltan columnas clave en ECV (anio, cod_dane_dpto_d, departamento_d, p6020, p6040, fex_c.x).")

has_p1707   <- "p1707"   %in% names(ecv_raw)
has_p1707s1 <- "p1707s1" %in% names(ecv_raw)
has_p3003   <- "p3003"   %in% names(ecv_raw)
has_p3003s1 <- "p3003s1" %in% names(ecv_raw)

# CLASE exacta (1 = Cabecera municipal; 2 = Centros Poblados y Rural Disperso)
mk_clase_exact <- function(v){
  if (all(is.na(v))) return(rep(NA_character_, length(v)))
  vnum <- suppressWarnings(as.integer(numish(v)))
  out  <- ifelse(vnum == 1, "Cabecera municipal",
                 ifelse(vnum == 2, "Centros Poblados y Rural Disperso", NA_character_))
  need_txt <- is.na(out)
  if (any(need_txt)) {
    txt <- tolower(norm_txt(v[need_txt]))
    lbl <- rep(NA_character_, length(txt))
    lbl[grepl("cabecera", txt)] <- "Cabecera municipal"
    lbl[grepl("centro|centros|poblado|poblados|rural|disperso", txt)] <- "Centros Poblados y Rural Disperso"
    out[need_txt] <- lbl
  }
  out
}

ecv <- tibble::tibble(
  anio           = suppressWarnings(as.integer(numish(ecv_raw[[col_ano]]))),
  COD_DANE_DPTO2 = norm_dep2(ecv_raw[[col_dep_cod]]),
  DEPARTAMENTO   = norm_txt(ecv_raw[[col_dep_nom]]),
  COD_DANE_MPIO2 = if (!is.na(col_mun_cod)) norm_mun5(ecv_raw[[col_mun_cod]]) else NA_character_,
  MUNICIPIO      = if (!is.na(col_mun_nom)) norm_txt(ecv_raw[[col_mun_nom]]) else NA_character_,
  CLASE_LBL      = if (!is.na(col_clase)) mk_clase_exact(ecv_raw[[col_clase]]) else NA_character_,
  p6020          = numish(ecv_raw[[col_sexo]]),
  p6040          = numish(ecv_raw[[col_edad]]),
  fexp           = suppressWarnings(as.numeric(numish(ecv_raw[[col_w]]))),
  p1707          = if (has_p1707) { val <- numish(ecv_raw[["p1707"]]); ifelse(val %in% c(1,2), val, NA_real_) } else NA_real_,
  p1707s1        = if (has_p1707s1) safe_as_char(ecv_raw[["p1707s1"]]) else NA_character_,
  p1707s1_code   = if (has_p1707s1) suppressWarnings(as.integer(numish(ecv_raw[["p1707s1"]]))) else NA_integer_,
  p3003          = if (has_p3003) { val <- numish(ecv_raw[["p3003"]]); ifelse(val %in% c(1,2), val, NA_real_) } else NA_real_,
  p3003s1        = if (has_p3003s1) safe_as_char(ecv_raw[["p3003s1"]]) else NA_character_,
  p3003s1_code   = if (has_p3003s1) suppressWarnings(as.integer(numish(ecv_raw[["p3003s1"]]))) else NA_integer_
) %>%
  mutate(
    p6020_lbl = dplyr::case_when(p6020==1 ~ "Hombre", p6020==2 ~ "Mujer", TRUE ~ "Sin dato"),
    edad_grupo = cut(p6040, breaks=c(-Inf,11,17,26,40,59,Inf),
                     labels=c("0–11","12–17","18–26","27–40","41–59","60+"), right=TRUE)
  )

# ---------------- Indicadores ----------------
inds_binarios <- c(
  "¿Consume bebidas azucaradas? (p1707)"   = "p1707",
  "¿Consume alimentos de paquete? (p3003)" = "p3003"
)
inds_aplican_12mas <- c("p1707","p1707s1","p3003","p3003s1")

freq_labels <- c(
  "1 Todos los días de la semana (dos o más veces al día)",
  "2 Todos los días de la semana (una vez al día)",
  "3 Cuatro a seis veces a la semana",
  "4 Dos o tres veces a la semana",
  "5 Una vez a la semana",
  "6 Menos de una vez por semana"
)

freq_ind_map <- tibble::tibble(
  var  = rep(c("p1707s1","p3003s1"), each=6),
  code = rep(1:6, times=2),
  label_base = rep(c("Bebidas azucaradas", "Paquetes"), each=6),
  label_cat  = rep(freq_labels, times=2)
) %>%
  mutate(
    key   = paste(var, code, sep=":"),
    label = paste0(label_base, " — ", label_cat)
  )
freq_choices_ind <- setNames(freq_ind_map$key, freq_ind_map$label)

# ---------------- Shapes ----------------
dep_sf <- sf::st_read(ruta_shp_dep, quiet=TRUE) %>%
  dplyr::mutate(
    COD_DPTO2 = dplyr::coalesce(
      if ("DPTO_CCDGO" %in% names(.)) sprintf("%02d", suppressWarnings(as.integer(.data$DPTO_CCDGO))) else NA_character_,
      if ("COD_DEPTO"   %in% names(.)) sprintf("%02d", suppressWarnings(as.integer(.data$COD_DEPTO))) else NA_character_
    ),
    DEPTO_N = dplyr::coalesce(
      as.character(.$DEPARTAMENTO_D %||% NA),
      as.character(.$DPTO_CNMBR %||% NA),
      as.character(.$NOMBRE_DEPTO %||% COD_DPTO2))
  ) %>% sf::st_transform(4326) %>% sf::st_make_valid()

# ---------------- UI ----------------
ui <- fluidPage(
  theme = bs_theme(version=5, primary="#2563eb",
                   base_font=bslib::font_google("Inter"),
                   heading_font=bslib::font_google("Inter Tight")),
  tags$head(tags$style(HTML("
    .wrap{max-width:1200px;margin:0 auto;padding:16px 20px 32px;}
    .filters{background:#fff;border:1px solid #eaecef;border-radius:16px;padding:14px 16px;margin-bottom:12px;box-shadow:0 2px 10px rgba(0,0,0,.05)}
    .filters-grid-6{display:grid;grid-template-columns:repeat(6,minmax(170px,1fr));gap:12px}
    .filters-grid-2{display:grid;grid-template-columns:repeat(2,minmax(170px,1fr));gap:12px;margin-top:8px}
    .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;text-transform:uppercase;color:#6b7280;margin-bottom:6px}
    .grid-2{display:grid;grid-template-columns:1fr 1fr;gap:12px;align-items:start}
    .card{background:#fff;border:1px solid #eaecef;border-radius:16px;padding:14px;box-shadow:0 2px 10px rgba(0,0,0,.05)}
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px}
    .kpi{display:flex;gap:16px;flex-wrap:wrap}
    .kpi .item{background:#f9fafb;border:1px solid #e5e7eb;border-radius:12px;padding:10px 12px;min-width:180px}
    .kpi .item b{display:block;font-size:20px}

    /* Responsive */
    @media (max-width:1200px){
      .filters-grid-6{grid-template-columns:repeat(3,minmax(160px,1fr));}
    }
    @media (max-width:800px){
      .filters-grid-6{grid-template-columns:repeat(2,minmax(150px,1fr));}
      .filters-grid-2{grid-template-columns:1fr;}
    }
  "))),
  div(class="wrap",
      h3("ECV — Consumo de bebidas azucaradas y alimentos de paquete"),
      tabsetPanel(type="tabs", id="tabs",
                  
                  # -------- TAB 1: BINARIOS --------
                  tabPanel("Exploración (Binarios)",
                           div(class="filters",
                               div(class="filters-grid-6",
                                   div(div(class="filter-label","Año"), uiOutput("anio_ui")),
                                   div(div(class="filter-label","Departamento"), uiOutput("dep_ui")),
                                   div(div(class="filter-label","Municipio"), uiOutput("mun_ui")),
                                   div(div(class="filter-label","Clase"),
                                       pickerInput("f_clase", NULL, multiple=TRUE,
                                                   choices=c("Cabecera municipal","Centros Poblados y Rural Disperso"),
                                                   selected=c("Cabecera municipal","Centros Poblados y Rural Disperso"),
                                                   options=list(`actions-box`=TRUE))),
                                   div(div(class="filter-label","Sexo"),
                                       pickerInput("f_sexo", NULL, multiple=TRUE,
                                                   choices=c("Hombre","Mujer"), selected=c("Hombre","Mujer"),
                                                   options=list(`actions-box`=TRUE))),
                                   div(div(class="filter-label","Grupo de edad"),
                                       pickerInput("f_edad", NULL, multiple=TRUE,
                                                   choices=levels(ecv$edad_grupo), selected=levels(ecv$edad_grupo),
                                                   options=list(`actions-box`=TRUE)))
                               ),
                               div(class="filters-grid-2",
                                   div(div(class="filter-label","Indicador"),
                                       selectInput("f_ind_bin", NULL, choices = inds_binarios, selected="p1707")),
                                   div(div(class="filter-label","Categoría / Valor"), uiOutput("cat_ui_bin"))
                               )
                           ),
                           div(id="kpi_card_bin", class="card",
                               div(class="card-title","Resumen nacional (ponderado)"),
                               htmlOutput("kpis_bin")),
                           div(class="grid-2",
                               div(
                                 div(class="card",
                                     div(class="card-title","Mapa — Prevalencia ponderada (%)"),
                                     leafletOutput("mapa_bin", height = 320)),
                                 div(class="card", style="margin-top:12px",
                                     div(class="card-title","Serie temporal — Promedio ponderado (%)"),
                                     div(style="margin-bottom:6px;",
                                         prettySwitch("ts_scope_bin", "Usar departamento seleccionado en la serie", FALSE, status="primary")),
                                     htmlOutput("ts_label_bin"),
                                     plotlyOutput("ts_prev_bin", height = 300))
                               ),
                               div(class="card",
                                   div(class="card-title","Prevalencia por departamento (%)"),
                                   plotlyOutput("bars_all_bin", height = "720px"))
                           )
                  ),
                  
                  # -------- TAB 2: FRECUENCIAS (cada categoría = indicador) --------
                  tabPanel("Frecuencias (por categoría)",
                           div(class="filters",
                               div(class="filters-grid-6",
                                   div(div(class="filter-label","Año"), uiOutput("anio_ui2")),
                                   div(div(class="filter-label","Departamento"), uiOutput("dep_ui2")),
                                   div(div(class="filter-label","Municipio"), uiOutput("mun_ui2")),
                                   div(div(class="filter-label","Clase"),
                                       pickerInput("f_clase2", NULL, multiple=TRUE,
                                                   choices=c("Cabecera municipal","Centros Poblados y Rural Disperso"),
                                                   selected=c("Cabecera municipal","Centros Poblados y Rural Disperso"),
                                                   options=list(`actions-box`=TRUE))),
                                   div(div(class="filter-label","Sexo"),
                                       pickerInput("f_sexo2", NULL, multiple=TRUE,
                                                   choices=c("Hombre","Mujer"), selected=c("Hombre","Mujer"),
                                                   options=list(`actions-box`=TRUE))),
                                   div(div(class="filter-label","Grupo de edad"),
                                       pickerInput("f_edad2", NULL, multiple=TRUE,
                                                   choices=levels(ecv$edad_grupo), selected=levels(ecv$edad_grupo),
                                                   options=list(`actions-box`=TRUE)))
                               ),
                               div(class="filters-grid-2",
                                   div(div(class="filter-label","Indicador (categoría única)"),
                                       selectInput("f_ind_freq_key", NULL, choices = freq_choices_ind,
                                                   selected = freq_ind_map$key[1])),
                                   div() # Placeholder por si luego agregas algo más
                               )
                           ),
                           div(id="kpi_card_freq", class="card",
                               div(class="card-title","Resumen nacional (ponderado)"),
                               htmlOutput("kpis_freq")),
                           div(class="grid-2",
                               div(
                                 div(class="card",
                                     div(class="card-title","Mapa — Prevalencia ponderada (%)"),
                                     leafletOutput("mapa_freq", height = 320)),
                                 div(class="card", style="margin-top:12px",
                                     div(class="card-title","Serie temporal — Promedio ponderado (%)"),
                                     div(style="margin-bottom:6px;",
                                         prettySwitch("ts_scope_freq", "Usar departamento seleccionado en la serie", FALSE, status="primary")),
                                     htmlOutput("ts_label_freq"),
                                     plotlyOutput("ts_prev_freq", height = 300))
                               ),
                               div(class="card",
                                   div(class="card-title","Prevalencia por departamento (%)"),
                                   plotlyOutput("bars_all_freq", height = "720px"))
                           )
                  )
                  
      )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session){
  
  yrs_avail <- ecv %>%
    dplyr::filter(is.finite(fexp), fexp>0, !is.na(anio)) %>%
    dplyr::distinct(anio) %>% dplyr::arrange(anio) %>% dplyr::pull(anio)
  yr_latest <- max(yrs_avail, na.rm=TRUE)
  
  output$anio_ui  <- renderUI({ selectInput("anio",  NULL, choices=yrs_avail, selected=yr_latest) })
  output$anio_ui2 <- renderUI({ selectInput("anio2", NULL, choices=yrs_avail, selected=yr_latest) })
  
  mk_dep_choices <- function(){
    ecv %>% dplyr::filter(!is.na(DEPARTAMENTO), nzchar(DEPARTAMENTO)) %>%
      dplyr::distinct(DEPARTAMENTO) %>% dplyr::arrange(DEPARTAMENTO) %>% dplyr::pull(DEPARTAMENTO)
  }
  output$dep_ui  <- renderUI({ selectInput("f_dep",  NULL, choices=c("Todos", mk_dep_choices()), selected="Todos") })
  output$dep_ui2 <- renderUI({ selectInput("f_dep2", NULL, choices=c("Todos", mk_dep_choices()), selected="Todos") })
  
  mk_mun_choices <- function(dep_sel){
    d <- ecv
    if (!is.null(dep_sel) && dep_sel!="Todos") d <- d %>% dplyr::filter(DEPARTAMENTO==dep_sel)
    d %>% dplyr::filter(!is.na(MUNICIPIO), nzchar(MUNICIPIO)) %>%
      dplyr::distinct(MUNICIPIO) %>% dplyr::arrange(MUNICIPIO) %>% dplyr::pull(MUNICIPIO)
  }
  output$mun_ui  <- renderUI({
    ch <- c("Todos", mk_mun_choices(input$f_dep %||% "Todos"))
    selectInput("f_mun", NULL, choices = unique(ch), selected = "Todos")
  })
  output$mun_ui2 <- renderUI({
    ch <- c("Todos", mk_mun_choices(input$f_dep2 %||% "Todos"))
    selectInput("f_mun2", NULL, choices = unique(ch), selected = "Todos")
  })
  
  output$cat_ui_bin <- renderUI({
    ind <- input$f_ind_bin; if (is.null(ind)) return(NULL)
    if (ind=="p1707")  selectInput("f_bin_p1707", NULL, choices=c("1 — Sí consume"=1,"2 — No consume"=2), selected=1)
    else               selectInput("f_bin_p3003", NULL, choices=c("1 — Sí consume"=1,"2 — No consume"=2), selected=1)
  })
  
  apply_common_filters <- function(d, sexo, edad, ind_var, clase=NULL){
    if (!is.null(sexo) && length(sexo)) d <- d %>% dplyr::filter(p6020_lbl %in% sexo)
    if (!is.null(edad) && length(edad)) d <- d %>% dplyr::filter(edad_grupo %in% edad)
    if (!is.null(clase) && length(clase)) d <- d %>% dplyr::filter(CLASE_LBL %in% clase | is.na(CLASE_LBL))
    if (!is.null(ind_var) && ind_var %in% inds_aplican_12mas) d <- d %>% dplyr::filter(p6040 >= 12 | is.na(p6040))
    d %>% dplyr::filter(is.finite(fexp), fexp>0, !is.na(DEPARTAMENTO), nzchar(DEPARTAMENTO))
  }
  
  mk_event_bin <- function(d, ind){
    v <- d[[ind]]
    yes <- ifelse(ind=="p1707", input$f_bin_p1707 %||% 1, input$f_bin_p3003 %||% 1)
    vnum <- suppressWarnings(as.numeric(v))
    ifelse(is.na(vnum), NA_real_, as.numeric(vnum==as.numeric(yes)))
  }
  
  lookup_freq <- function(key){
    row <- freq_ind_map[freq_ind_map$key == key, ]
    if (nrow(row)==0) return(list(var="p1707s1", code=1, label=""))
    list(var = row$var[[1]], code = row$code[[1]], label = row$label[[1]])
  }
  
  mk_event_freq_single <- function(d, key){
    lk <- lookup_freq(key)
    code_col <- paste0(lk$var, "_code")
    vcode <- NULL
    if (code_col %in% names(d)) vcode <- suppressWarnings(as.integer(d[[code_col]]))
    if (is.null(vcode) || all(is.na(vcode))) {
      vcode_num <- suppressWarnings(as.integer(readr::parse_number(as.character(d[[lk$var]]))))
      if (all(is.na(vcode_num))) {
        txt <- tolower(trimws(as.character(d[[lk$var]])))
        vcode <- rep(NA_integer_, length(txt))
        vcode[grepl("dos o mas", txt) & grepl("semana", txt)] <- 1
        vcode[grepl("una vez al dia", txt)]                    <- 2
        vcode[grepl("cuatro a seis", txt)]                     <- 3
        vcode[grepl("dos o tres", txt)]                        <- 4
        vcode[grepl("^una vez| una vez a la semana", txt)]     <- 5
        vcode[grepl("menos de una", txt)]                      <- 6
      } else vcode <- vcode_num
    }
    vcode[!vcode %in% 1:6] <- NA_integer_
    ifelse(is.na(vcode), NA_real_, ifelse(vcode == lk$code, 1, 0))
  }
  
  # -------- TAB 1: BINARIOS --------
  base_anio_bin <- reactive({
    req(input$anio)
    d <- ecv %>% dplyr::filter(anio==as.integer(input$anio))
    if (!is.null(input$f_dep) && input$f_dep!="Todos") d <- d %>% dplyr::filter(DEPARTAMENTO==input$f_dep)
    if (!is.null(input$f_mun) && input$f_mun!="Todos") d <- d %>% dplyr::filter(MUNICIPIO==input$f_mun)
    apply_common_filters(d, input$f_sexo, input$f_edad, input$f_ind_bin, clase=input$f_clase)
  })
  
  base_ts_bin <- reactive({
    d <- ecv
    use_dep <- isTRUE(input$ts_scope_bin) && !is.null(input$f_dep) && input$f_dep!="Todos"
    if (use_dep) d <- d %>% dplyr::filter(DEPARTAMENTO==input$f_dep)
    apply_common_filters(d, input$f_sexo, input$f_edad, input$f_ind_bin, clase=input$f_clase)
  })
  
  output$kpis_bin <- renderUI({
    d <- base_anio_bin(); req(nrow(d)>0)
    ev <- mk_event_bin(d, input$f_ind_bin %||% "p1707")
    prev <- w_prop(as.numeric(ev), d$fexp)*100
    n    <- sum(is.finite(ev)&is.finite(d$fexp)&d$fexp>0)
    pob  <- sum(d$fexp[is.finite(d$fexp)&d$fexp>0], na.rm=TRUE)
    HTML(sprintf('<div class="kpi">
      <div class="item"><span>Prevalencia ponderada</span><b>%s</b></div>
      <div class="item"><span>n válidos</span><b>%s</b></div>
      <div class="item"><span>Personas ponderadas</span><b>%s</b></div>
      <div class="item"><span>Año</span><b>%s</b></div></div>',
                 ifelse(is.finite(prev), sprintf("%.1f%%",prev), "NA"),
                 comma(n), comma(round(pob)), input$anio))
  })
  
  output$mapa_bin <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74.3,4.6,5)
  })
  observe({
    d <- base_anio_bin(); req(nrow(d)>0)
    ev <- mk_event_bin(d, input$f_ind_bin %||% "p1707")
    dd <- d %>% dplyr::mutate(evento=as.numeric(ev)) %>%
      dplyr::group_by(COD_DANE_DPTO2, DEPARTAMENTO) %>%
      dplyr::summarise(prev=w_prop(evento,fexp)*100, .groups="drop")
    shp <- dep_sf %>% dplyr::left_join(dd, by=c("COD_DPTO2"="COD_DANE_DPTO2")) %>%
      dplyr::mutate(DEPARTAMENTO=dplyr::coalesce(DEPARTAMENTO, DEPTO_N),
                    etq=paste0("<b>",DEPARTAMENTO,"</b><br>Prevalencia: ",
                               ifelse(is.finite(prev),sprintf('%.1f%%',prev),"NA")))
    pal <- make_pal_bin(shp$prev, "YlGnBu")
    leafletProxy("mapa_bin", data=shp) %>% clearShapes() %>% clearControls() %>%
      addPolygons(layerId=~COD_DPTO2, fillColor=~pal(prev), color="#666", weight=0.7,
                  fillOpacity=0.9, label=~lapply(etq, HTML),
                  highlightOptions=highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
      addLegend("bottomright", pal=pal, values=~prev, title="Prevalencia (%)")
  })
  
  output$bars_all_bin <- renderPlotly({
    d <- base_anio_bin(); req(nrow(d)>0)
    ev <- mk_event_bin(d, input$f_ind_bin %||% "p1707")
    dd <- d %>% dplyr::mutate(evento=as.numeric(ev)) %>%
      dplyr::group_by(DEPARTAMENTO) %>%
      dplyr::summarise(prev=w_prop(evento,fexp)*100, .groups="drop") %>%
      dplyr::filter(is.finite(prev)) %>% dplyr::arrange(dplyr::desc(prev)) %>%
      dplyr::mutate(Departamento=factor(DEPARTAMENTO, levels=rev(DEPARTAMENTO)),
                    prev_lab=sprintf("%.1f",prev))
    plot_ly(dd, x=~prev, y=~Departamento, type="bar", orientation="h",
            text=~prev_lab, textposition="auto",
            hovertemplate="<b>%{y}</b><br>Prevalencia: %{x:.1f}%<extra></extra>") %>%
      layout(xaxis=list(title="Prevalencia (%)", zeroline=FALSE),
             yaxis=list(title="", automargin=TRUE),
             margin=list(l=140,r=10,t=10,b=40))
  })
  
  output$ts_label_bin <- renderUI({
    use_dep <- isTRUE(input$ts_scope_bin) && !identical(input$f_dep,"Todos")
    HTML(sprintf('<div style="color:#6b7280;font-size:12px;margin-bottom:4px">%s</div>',
                 if (use_dep) paste0("Alcance de la serie: Departamento — ", htmltools::htmlEscape(input$f_dep))
                 else "Alcance de la serie: Nacional"))
  })
  
  output$ts_prev_bin <- renderPlotly({
    d <- base_ts_bin(); req(nrow(d)>0)
    ev <- mk_event_bin(d, input$f_ind_bin %||% "p1707")
    dt <- d %>% dplyr::mutate(evento=as.numeric(ev)) %>%
      dplyr::group_by(anio) %>% dplyr::summarise(prev=w_prop(evento,fexp)*100, .groups="drop") %>%
      dplyr::filter(!is.na(anio), is.finite(prev)) %>% dplyr::arrange(anio)
    plot_ly(dt, x=~anio, y=~prev, type="scatter", mode="lines+markers",
            text=~sprintf("%.1f%%",prev),
            hovertemplate="Año: %{x}<br>Promedio: %{y:.1f}%%<extra></extra>") %>%
      layout(xaxis=list(title="Año", dtick=1, tickmode="linear"),
             yaxis=list(title="Prevalencia ponderada (%)", rangemode="tozero"),
             margin=list(l=60,r=10,t=10,b=40))
  })
  
  # -------- TAB 2: FRECUENCIAS --------
  base_anio_freq <- reactive({
    req(input$anio2, input$f_ind_freq_key)
    ind_var <- lookup_freq(input$f_ind_freq_key)$var
    d <- ecv %>% dplyr::filter(anio==as.integer(input$anio2))
    if (!is.null(input$f_dep2) && input$f_dep2!="Todos") d <- d %>% dplyr::filter(DEPARTAMENTO==input$f_dep2)
    if (!is.null(input$f_mun2) && input$f_mun2!="Todos") d <- d %>% dplyr::filter(MUNICIPIO==input$f_mun2)
    apply_common_filters(d, input$f_sexo2, input$f_edad2, ind_var, clase=input$f_clase2)
  })
  
  base_ts_freq <- reactive({
    req(input$f_ind_freq_key)
    ind_var <- lookup_freq(input$f_ind_freq_key)$var
    d <- ecv
    use_dep <- isTRUE(input$ts_scope_freq) && !is.null(input$f_dep2) && input$f_dep2!="Todos"
    if (use_dep) d <- d %>% dplyr::filter(DEPARTAMENTO==input$f_dep2)
    apply_common_filters(d, input$f_sexo2, input$f_edad2, ind_var, clase=input$f_clase2)
  })
  
  output$kpis_freq <- renderUI({
    d <- base_anio_freq(); req(nrow(d)>0)
    ev <- mk_event_freq_single(d, input$f_ind_freq_key)
    prev <- w_prop(as.numeric(ev), d$fexp)*100
    n    <- sum(is.finite(ev)&is.finite(d$fexp)&d$fexp>0)
    pob  <- sum(d$fexp[is.finite(d$fexp)&d$fexp>0], na.rm=TRUE)
    HTML(sprintf('<div class="kpi">
      <div class="item"><span>Prevalencia ponderada</span><b>%s</b></div>
      <div class="item"><span>n válidos</span><b>%s</b></div>
      <div class="item"><span>Personas ponderadas</span><b>%s</b></div>
      <div class="item"><span>Año</span><b>%s</b></div></div>',
                 ifelse(is.finite(prev), sprintf("%.1f%%",prev), "NA"),
                 comma(n), comma(round(pob)), input$anio2))
  })
  
  output$mapa_freq <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-74.3,4.6,5)
  })
  observe({
    d <- base_anio_freq(); req(nrow(d)>0)
    ev <- mk_event_freq_single(d, input$f_ind_freq_key)
    dd <- d %>% dplyr::mutate(evento=as.numeric(ev)) %>%
      dplyr::group_by(COD_DANE_DPTO2, DEPARTAMENTO) %>%
      dplyr::summarise(prev=w_prop(evento,fexp)*100, .groups="drop")
    shp <- dep_sf %>% dplyr::left_join(dd, by=c("COD_DPTO2"="COD_DANE_DPTO2")) %>%
      dplyr::mutate(DEPARTAMENTO=dplyr::coalesce(DEPARTAMENTO, DEPTO_N),
                    etq=paste0("<b>",DEPARTAMENTO,"</b><br>Prevalencia: ",
                               ifelse(is.finite(prev),sprintf('%.1f%%',prev),"NA")))
    pal <- make_pal_bin(shp$prev, "YlGnBu")
    leafletProxy("mapa_freq", data=shp) %>% clearShapes() %>% clearControls() %>%
      addPolygons(layerId=~COD_DPTO2, fillColor=~pal(prev), color="#666", weight=0.7,
                  fillOpacity=0.9, label=~lapply(etq, HTML),
                  highlightOptions=highlightOptions(color="black", weight=2, bringToFront=TRUE)) %>%
      addLegend("bottomright", pal=pal, values=~prev, title="Prevalencia (%)")
  })
  
  output$bars_all_freq <- renderPlotly({
    d <- base_anio_freq(); req(nrow(d)>0)
    ev <- mk_event_freq_single(d, input$f_ind_freq_key)
    dd <- d %>% dplyr::mutate(evento=as.numeric(ev)) %>%
      dplyr::group_by(DEPARTAMENTO) %>%
      dplyr::summarise(prev=w_prop(evento,fexp)*100, .groups="drop") %>%
      dplyr::filter(is.finite(prev)) %>% dplyr::arrange(dplyr::desc(prev)) %>%
      dplyr::mutate(Departamento=factor(DEPARTAMENTO, levels=rev(DEPARTAMENTO)),
                    prev_lab=sprintf("%.1f",prev))
    plot_ly(dd, x=~prev, y=~Departamento, type="bar", orientation="h",
            text=~prev_lab, textposition="auto",
            hovertemplate="<b>%{y}</b><br>Prevalencia: %{x:.1f}%<extra></extra>") %>%
      layout(xaxis=list(title="Prevalencia (%)", zeroline=FALSE),
             yaxis=list(title="", automargin=TRUE),
             margin=list(l=140,r=10,t=10,b=40))
  })
  
  output$ts_label_freq <- renderUI({
    use_dep <- isTRUE(input$ts_scope_freq) && !identical(input$f_dep2,"Todos")
    lk <- lookup_freq(input$f_ind_freq_key)
    HTML(sprintf('<div style="color:#6b7280;font-size:12px;margin-bottom:4px">%s<br/><span style="font-weight:600">%s</span></div>',
                 if (use_dep) paste0("Alcance de la serie: Departamento — ", htmltools::htmlEscape(input$f_dep2))
                 else "Alcance de la serie: Nacional",
                 htmltools::htmlEscape(lk$label)))
  })
  
  output$ts_prev_freq <- renderPlotly({
    d <- base_ts_freq(); req(nrow(d)>0)
    ev <- mk_event_freq_single(d, input$f_ind_freq_key)
    dt <- d %>% dplyr::mutate(evento=as.numeric(ev)) %>%
      dplyr::group_by(anio) %>% dplyr::summarise(prev=w_prop(evento,fexp)*100, .groups="drop") %>%
      dplyr::filter(!is.na(anio), is.finite(prev)) %>% dplyr::arrange(anio)
    plot_ly(dt, x=~anio, y=~prev, type="scatter", mode="lines+markers",
            text=~sprintf("%.1f%%",prev),
            hovertemplate="Año: %{x}<br>Promedio: %{y:.1f}%%<extra></extra>") %>%
      layout(xaxis=list(title="Año", dtick=1, tickmode="linear"),
             yaxis=list(title="Prevalencia ponderada (%)", rangemode="tozero"),
             margin=list(l=60,r=10,t=10,b=40))
  })
}

shinyApp(ui, server)

