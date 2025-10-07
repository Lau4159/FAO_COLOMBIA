# =========================================================
# app_cnmh_dashboard.R — Histórico + Territorial (estilo moderno)
# =========================================================
suppressWarnings({
  library(shiny); library(dplyr); library(ggplot2); library(plotly)
  library(lubridate); library(scales); library(sf); library(leaflet); library(bslib)
})

options(stringsAsFactors = FALSE, scipen = 999)

# ---- Arreglo definitivo de validaciones (evita choque con plotly::validate) ----
validate <- shiny::validate
need     <- shiny::need

# ---------- Rutas y carga ----------
data_dir  <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/CNMH_CASOS_VIOLENCIA/data"
cnmh_path <- file.path(data_dir, "061_Centro Nacional de Memoria Histórica_Casos_Violencia.rds")
ruta_shp_dptos <- file.path(data_dir, "shp", "MGN_ANM_DPTOS.shp")
ruta_shp_mpios <- file.path(data_dir, "shp", "MGN_ANM_MPIOS.shp")

cnmh <- readRDS(cnmh_path) %>% filter(!is.na(fecha_completa))
min_f <- min(cnmh$fecha_completa, na.rm = TRUE)
max_f <- max(cnmh$fecha_completa, na.rm = TRUE)

# Choices globales para filtros del Tab 1
deps_all  <- sort(unique(na.omit(cnmh$DEPARTAMENTO_D)))
munis_all <- sort(unique(na.omit(cnmh$MUNICIPIO_D)))

# Shapes
dptos_sf <- st_read(ruta_shp_dptos, quiet = TRUE) %>%
  mutate(COD_DPTO2 = sprintf("%02d", as.integer(DPTO_CCDGO))) %>%
  st_transform(4326) %>%
  dplyr::select(DPTO_CCDGO, DPTO_CNMBR, COD_DPTO2, geometry)

mpios_sf <- st_read(ruta_shp_mpios, quiet = TRUE) %>%
  mutate(COD_MUN5 = sprintf("%05d", as.integer(MPIO_CDPMP)),
         COD_DPTO2 = substr(COD_MUN5, 1, 2)) %>%
  st_transform(4326) %>%
  dplyr::select(MPIO_CDPMP, MPIO_CNMBR, COD_MUN5, COD_DPTO2, geometry)

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = bs_theme(
    version = 5, primary = "#2563eb",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter Tight"),
    "border-radius" = "0.9rem", "font-size-base" = "0.95rem"
  ),
  tags$head(tags$style(HTML("
    .wrap{max-width:1360px;margin:0 auto;padding:16px 20px 32px;}
    h3{font-weight:700;letter-spacing:.2px;margin-bottom:8px}
    .data-note{font-size:13px;color:#6b7280;margin:0 0 16px}
    .filters{background:#fff;border:1px solid #eaecef;border-radius:16px;
             padding:14px 16px;margin-bottom:16px;box-shadow:0 4px 14px rgba(0,0,0,.06)}
    .filters-grid{display:grid;grid-template-columns:repeat(4,minmax(180px,1fr));gap:12px}
    .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;
                  text-transform:uppercase;color:#6b7280;margin-bottom:6px}
    .card{background:#fff;border:1px solid #eaecef;border-radius:16px;padding:12px;
          box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px}
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
  "))),
  
  div(class="wrap",
      h3("CNMH — Explorador de Casos de Violencia"),
      div(class="data-note","Aplicación interactiva para explorar los casos registrados en el Centro Nacional de Memoria Histórica."),
      
      tabsetPanel(
        id="tabs_cnmh", type="tabs",
        
        # ---------- TAB 1: HISTÓRICO ----------
        tabPanel("Histórico de casos", br(),
                 div(class="filters",
                     div(class="filters-grid",
                         div(class="filter", div(class="filter-label","Agrupar por"),
                             radioButtons("freq", NULL,
                                          choices=c("Día","Mes","Trimestre","Año"),
                                          selected="Mes", inline=TRUE)),
                         div(class="filter", div(class="filter-label","Rango de fechas"),
                             dateRangeInput("rango", NULL,
                                            start=min_f, end=max_f,
                                            min=min_f, max=max_f, separator=" a ")),
                         div(class="filter", div(class="filter-label","Categoría"),
                             selectInput("categoria", NULL,
                                         choices=c("Todas", sort(unique(na.omit(cnmh$categoria)))),
                                         selected="Todas")),
                         div(class="filter", div(class="filter-label","Departamento"),
                             selectInput("dep_hist", NULL,
                                         choices=c("Todos", deps_all), selected="Todos")),
                         div(class="filter", div(class="filter-label","Municipio"),
                             selectizeInput("mun_hist", NULL,
                                            choices=c("Todos", munis_all), selected="Todos",
                                            options=list(placeholder="Seleccione municipio...")))
                     )
                 ),
                 div(class="card",
                     div(class="card-title","Serie histórica de casos"),
                     plotlyOutput("ts_hist", height=450)
                 )
        ),
        
        # ---------- TAB 2: TERRITORIO ----------
        tabPanel("Casos por territorio", br(),
                 div(class="filters",
                     div(class="filters-grid",
                         div(class="filter", div(class="filter-label","Año"),
                             selectInput("ano", NULL, choices=sort(unique(cnmh$ano)), selected=max(cnmh$ano))),
                         div(class="filter", div(class="filter-label","Categoría"),
                             selectInput("cat", NULL, choices=c("Todas", sort(unique(cnmh$categoria))), selected="Todas")),
                         div(class="filter", div(class="filter-label","Acción"),
                             tagList(
                               actionButton("volver","← Volver a Departamentos", class="btn btn-outline-primary btn-sm")
                             ))
                     )
                 ),
                 
                 fluidRow(
                   column(3, div(class="card", div(class="card-title","Casos totales"), textOutput("vb_total"))),
                   column(3, div(class="card", div(class="card-title","% Mujeres"), textOutput("vb_mujeres"))),
                   column(3, div(class="card", div(class="card-title","% Civiles"), textOutput("vb_civiles"))),
                   column(3, div(class="card", div(class="card-title","Entidad más afectada"), textOutput("vb_topent")))
                 ),
                 
                 fluidRow(
                   column(7,
                          div(class="card",
                              div(class="card-title","Mapa — Casos"),
                              leafletOutput("mapa", height=600)
                          )
                   ),
                   column(5,
                          div(class="card",
                              div(class="card-title","Top-10 entidades"),
                              plotlyOutput("top10", height=280)
                          ),
                          div(class="card",
                              div(class="card-title","Composición de víctimas"),
                              plotlyOutput("comp_victimas", height=280)
                          )
                   )
                 )
        )
      )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  # --- Dep -> Mun dinámico (Tab 1) ---
  observeEvent(input$dep_hist, {
    if (is.null(input$dep_hist) || input$dep_hist == "Todos") {
      ch <- c("Todos", munis_all)
    } else {
      ch <- cnmh %>%
        filter(DEPARTAMENTO_D == input$dep_hist) %>%
        distinct(MUNICIPIO_D) %>%
        arrange(MUNICIPIO_D) %>%
        pull(MUNICIPIO_D)
      ch <- c("Todos", ch)
    }
    updateSelectizeInput(session, "mun_hist", choices = ch, selected = "Todos", server = TRUE)
  }, ignoreInit = TRUE)
  
  # ================= HISTÓRICO =================
  agg_ts <- reactive({
    req(input$rango, input$freq)
    df <- cnmh %>%
      filter(fecha_completa >= input$rango[1],
             fecha_completa <= input$rango[2])
    
    if (!is.null(input$categoria) && input$categoria != "Todas") {
      df <- df %>% filter(categoria == input$categoria)
    }
    if (!is.null(input$dep_hist) && input$dep_hist != "Todos") {
      df <- df %>% filter(DEPARTAMENTO_D == input$dep_hist)
    }
    if (!is.null(input$mun_hist) && input$mun_hist != "Todos") {
      df <- df %>% filter(MUNICIPIO_D == input$mun_hist)
    }
    
    unidad <- switch(input$freq,
                     "Día"="day", "Mes"="month",
                     "Trimestre"="quarter", "Año"="year")
    
    df %>%
      mutate(fecha_g = floor_date(fecha_completa, unit = unidad)) %>%
      count(fecha_g, name = "casos") %>%
      arrange(fecha_g)
  })
  
  output$ts_hist <- renderPlotly({
    dd <- agg_ts()
    validate(need(nrow(dd) > 0, "Sin datos en el rango seleccionado."))
    
    plot_ly(dd, x=~fecha_g, y=~casos, type="scatter", mode="lines",
            line=list(width=2, color="#2563eb")) %>%
      layout(
        hovermode="x unified",
        xaxis=list(
          rangeselector=list(
            buttons=list(
              list(count=1, step="year", stepmode="backward", label="1A"),
              list(count=5, step="year", stepmode="backward", label="5A"),
              list(count=10, step="year", stepmode="backward", label="10A"),
              list(step="all", label="Todo")
            )
          ),
          rangeslider=list(visible=TRUE)
        ),
        yaxis=list(title="Casos", rangemode="tozero"),
        margin=list(l=40, r=20, b=40, t=10)
      )
  })
  
  # ================= TERRITORIO =================
  vista <- reactiveVal("depto")
  depto_sel <- reactiveVal(NULL)
  
  base_filtrada <- reactive({
    req(input$ano)
    df <- cnmh %>% filter(ano == input$ano)
    if (input$cat != "Todas") df <- df %>% filter(categoria == input$cat)
    if (vista() == "mpio" && !is.null(depto_sel())) df <- df %>% filter(COD_DANE_DPTO_D == depto_sel())
    df
  })
  
  agg_data <- reactive({
    df <- base_filtrada()
    if (vista() == "depto"){
      df %>% group_by(COD_DANE_DPTO_D, DEPARTAMENTO_D) %>%
        summarise(casos = n(), .groups = "drop") %>%
        mutate(COD_DPTO2 = sprintf("%02d", as.integer(COD_DANE_DPTO_D)))
    } else {
      df %>% group_by(COD_DANE_MUNIC_D, MUNICIPIO_D) %>%
        summarise(casos = n(), .groups = "drop") %>%
        mutate(COD_MUN5 = sprintf("%05d", as.integer(COD_DANE_MUNIC_D)))
    }
  })
  
  output$vb_total   <- renderText({ comma(nrow(base_filtrada())) })
  output$vb_mujeres <- renderText({
    pct <- mean(base_filtrada()$sexo == "MUJER", na.rm = TRUE) * 100
    if (is.nan(pct)) pct <- 0
    paste0(round(pct, 1), "%")
  })
  output$vb_civiles <- renderText({
    pct <- mean(base_filtrada()$calidad_de_la_victima_o_la_baja == "CIVIL", na.rm = TRUE) * 100
    if (is.nan(pct)) pct <- 0
    paste0(round(pct, 1), "%")
  })
  output$vb_topent <- renderText({
    df <- agg_data() %>% arrange(desc(casos))
    if (nrow(df) > 0) {
      top <- if (vista() == "depto") df$DEPARTAMENTO_D[1] else df$MUNICIPIO_D[1]
      paste0(top, ": ", comma(df$casos[1]))
    } else "Sin datos"
  })
  
  output$mapa <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -74, lat = 4.5, zoom = 5)
  })
  
  observe({
    df_join <- agg_data()
    shp <- if (vista() == "depto") {
      dptos_sf %>% left_join(df_join, by = "COD_DPTO2")
    } else {
      validate(need(!is.null(depto_sel()), "Haz clic en un departamento para ver sus municipios."))
      mpios_sf %>% filter(COD_DPTO2 == depto_sel()) %>%
        left_join(df_join, by = "COD_MUN5")
    }
    validate(need(nrow(shp) > 0, "Sin datos para el mapa."))
    
    pal <- colorBin("YlOrRd", domain = shp$casos, bins = 6, na.color = "#f0f0f0")
    leafletProxy("mapa", data = shp) %>%
      clearShapes() %>% clearControls() %>%
      addPolygons(
        layerId = if (vista() == "depto") ~COD_DPTO2 else ~COD_MUN5,
        fillColor = ~pal(casos), color = "#444", weight = 0.5, fillOpacity = 0.8,
        label = ~paste0(if (vista() == "depto") DPTO_CNMBR else MPIO_CNMBR,
                        "<br>Casos: ", ifelse(is.na(casos), "0", comma(casos))),
        highlightOptions = highlightOptions(weight = 1.5, color = "#111", bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~casos, title = "Número de casos")
  })
  
  observeEvent(input$mapa_shape_click, {
    if (vista() == "depto") {
      click <- input$mapa_shape_click
      if (!is.null(click$id)) { vista("mpio"); depto_sel(click$id) }
    }
  })
  observeEvent(input$volver, { vista("depto"); depto_sel(NULL) })
  
  output$top10 <- renderPlotly({
    df <- agg_data() %>% arrange(desc(casos)) %>% slice_head(n = 10)
    validate(need(nrow(df) > 0, "Sin datos para el Top-10."))
    
    df <- df %>%
      mutate(nombre = if (vista() == "depto") DEPARTAMENTO_D else MUNICIPIO_D)
    
    p <- ggplot(df, aes(x = reorder(nombre, casos), y = casos,
                        text = paste0(nombre, "<br>Casos: ", comma(casos)))) +
      geom_col(fill = "#3b82f6") + coord_flip() +
      labs(x = NULL, y = "Casos") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$comp_victimas <- renderPlotly({
    df <- base_filtrada() %>%
      count(sexo, calidad_de_la_victima_o_la_baja, name = "casos") %>%
      mutate(sexo    = ifelse(is.na(sexo), "Sin info", sexo),
             calidad = ifelse(is.na(calidad_de_la_victima_o_la_baja), "Sin info", calidad_de_la_victima_o_la_baja))
    validate(need(nrow(df) > 0, "Sin datos para la composición de víctimas."))
    
    p <- ggplot(df, aes(x = calidad, y = casos, fill = sexo,
                        text = paste0("Sexo: ", sexo, "<br>Casos: ", comma(casos)))) +
      geom_col(position = "stack") +
      labs(x = "Calidad", y = "Casos", fill = "Sexo") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)

