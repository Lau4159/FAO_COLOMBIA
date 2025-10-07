# =========================================================
# app_poblacion_dashboard_v2.R — Proyecciones DANE (pirámide decenal)
# =========================================================
suppressWarnings({
  library(shiny); library(dplyr); library(tidyr); library(ggplot2); library(plotly)
  library(scales); library(bslib)
})

options(stringsAsFactors = FALSE, scipen = 999)

# ---------- Ruta y carga ----------
data_dir <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/DANE_POPULATION/data"
pob_path <- file.path(data_dir, "051_DANE_Proyecciones_P.rds")
pob <- readRDS(pob_path)   # <- corregido

# Normalizamos columnas
if (!"DEPARTAMENTO_D" %in% names(pob) && "DEPARTAMENTO" %in% names(pob)) {
  pob$DEPARTAMENTO_D <- pob$DEPARTAMENTO
}
if (!"MUNICIPIO_D" %in% names(pob) && "MUNICIPIO" %in% names(pob)) {
  pob$MUNICIPIO_D <- pob$MUNICIPIO
}

# Asegurar tipos básicos
if (!is.numeric(pob$ano))       suppressWarnings(pob$ano <- as.integer(pob$ano))
if (!is.numeric(pob$edad))      suppressWarnings(pob$edad <- as.integer(pob$edad))
if (!is.numeric(pob$poblacion)) suppressWarnings(pob$poblacion <- as.numeric(pob$poblacion))

# Helpers
fmt_comma <- function(x) comma(x, big.mark = ".", decimal.mark = ",")

# ---- NUEVO: grupos de edad decenales (0–9, 10–19, ..., 80+) ----
make_dec <- function(edad) {
  e <- pmin(pmax(edad, 0), 80)
  brks   <- seq(0, 80, 10)
  labels <- c(paste0(seq(0,70,10), "-", seq(9,79,10)), "80+")
  cuts <- cut(e, breaks = c(brks, Inf), right = FALSE, labels = labels)
  factor(cuts, levels = labels)  # deja 0–9 abajo y 80+ arriba en coord_flip()
}

anos  <- sort(unique(pob$ano))
deps  <- sort(unique(na.omit(pob$DEPARTAMENTO_D)))
munis <- sort(unique(na.omit(pob$MUNICIPIO_D)))
areas <- c("Todas", sort(unique(na.omit(pob$area_geo))))
sexos <- c("Ambos", sort(unique(na.omit(pob$sexo))))

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
    .filters-grid{display:grid;grid-template-columns:repeat(5,minmax(160px,1fr));gap:12px}
    .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;
                  text-transform:uppercase;color:#6b7280;margin-bottom:6px}
    .card{background:#fff;border:1px solid #eaecef;border-radius:16px;padding:12px;
          box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px}
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
    .kpi{font-weight:800;font-size:28px;color:#111827}
    .kpi-sub{font-size:12px;color:#6b7280;margin-top:-4px}
  "))),
  
  div(class="wrap",
      h3("Proyecciones de Población — DANE"),
      div(class="data-note","Filtros y visualizaciones de población nacional, departamental y municipal."),
      
      # ---------- Filtros ----------
      div(class="filters",
          div(class="filters-grid",
              div(class="filter", div(class="filter-label","Año"),
                  selectInput("f_ano", NULL, choices = anos, selected = max(anos, na.rm = TRUE))),
              div(class="filter", div(class="filter-label","Departamento"),
                  selectInput("f_dep", NULL, choices = c("Todos", deps), selected = "Todos")),
              div(class="filter", div(class="filter-label","Municipio"),
                  selectizeInput("f_mun", NULL, choices = c("Todos", munis), selected = "Todos",
                                 options = list(placeholder = "Seleccione municipio..."))),
              div(class="filter", div(class="filter-label","Área geográfica"),
                  selectInput("f_area", NULL, choices = areas, selected = "Todas")),
              div(class="filter", div(class="filter-label","Sexo"),
                  selectInput("f_sexo", NULL, choices = sexos, selected = "Ambos"))
          )
      ),
      
      # ---------- KPIs ----------
      fluidRow(
        column(3, div(class="card",
                      div(class="card-title","Población total"),
                      div(class="kpi", textOutput("kpi_pob")),
                      div(class="kpi-sub","Personas según filtros")
        )),
        column(3, div(class="card",
                      div(class="card-title","% Jóvenes (0–14)"),
                      div(class="kpi", textOutput("kpi_jovenes")),
                      div(class="kpi-sub","Participación sobre total")
        )),
        column(3, div(class="card",
                      div(class="card-title","% Adultos mayores (65+)"),
                      div(class="kpi", textOutput("kpi_mayores")),
                      div(class="kpi-sub","Participación sobre total")
        )),
        column(3, div(class="card",
                      div(class="card-title","Razón de dependencia"),
                      div(class="kpi", textOutput("kpi_dep")),
                      div(class="kpi-sub","(0–14 y 65+) / 15–64")
        ))
      ),
      
      # ---------- Visuales ----------
      fluidRow(
        column(6, div(class="card",
                      div(class="card-title","Evolución histórica de la población"),
                      plotlyOutput("plot_hist", height = 360)
        )),
        column(6, div(class="card",
                      div(class="card-title","Pirámide poblacional (intervalos de 10 años)"),
                      plotlyOutput("plot_piramide", height = 420)
        ))
      )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  # --- Actualizar municipios cuando cambia el departamento (server-side) ---
  observeEvent(input$f_dep, {
    if (input$f_dep == "Todos") {
      ch <- c("Todos", sort(unique(na.omit(pob$MUNICIPIO_D))))
    } else {
      ch <- pob %>%
        filter(DEPARTAMENTO_D == input$f_dep) %>%
        distinct(MUNICIPIO_D) %>%
        arrange(MUNICIPIO_D) %>%
        pull(MUNICIPIO_D)
      ch <- c("Todos", ch)
    }
    updateSelectizeInput(session, "f_mun", choices = ch, selected = "Todos", server = TRUE)
  }, ignoreInit = TRUE)
  
  # --- Base filtrada ---
  base_filtrada <- reactive({
    df <- pob
    if (!is.null(input$f_dep)  && input$f_dep  != "Todos") df <- df %>% filter(DEPARTAMENTO_D == input$f_dep)
    if (!is.null(input$f_mun)  && input$f_mun  != "Todos") df <- df %>% filter(MUNICIPIO_D   == input$f_mun)
    if (!is.null(input$f_area) && input$f_area != "Todas") df <- df %>% filter(area_geo      == input$f_area)
    if (!is.null(input$f_sexo) && input$f_sexo != "Ambos") df <- df %>% filter(sexo          == input$f_sexo)
    df
  })
  
  # ================= KPIs =================
  output$kpi_pob <- renderText({
    df <- base_filtrada() %>% filter(ano == input$f_ano)
    fmt_comma(sum(df$poblacion, na.rm = TRUE))
  })
  
  output$kpi_jovenes <- renderText({
    df <- base_filtrada() %>% filter(ano == input$f_ano)
    tot <- sum(df$poblacion, na.rm = TRUE)
    j <- sum(df$poblacion[df$edad <= 14], na.rm = TRUE)
    pct <- if (tot > 0) 100 * j / tot else 0
    paste0(round(pct, 1), "%")
  })
  
  output$kpi_mayores <- renderText({
    df <- base_filtrada() %>% filter(ano == input$f_ano)
    tot <- sum(df$poblacion, na.rm = TRUE)
    m <- sum(df$poblacion[df$edad >= 65], na.rm = TRUE)
    pct <- if (tot > 0) 100 * m / tot else 0
    paste0(round(pct, 1), "%")
  })
  
  output$kpi_dep <- renderText({
    df <- base_filtrada() %>% filter(ano == input$f_ano)
    j <- sum(df$poblacion[df$edad <= 14], na.rm = TRUE)
    m <- sum(df$poblacion[df$edad >= 65], na.rm = TRUE)
    act <- sum(df$poblacion[df$edad >= 15 & df$edad <= 64], na.rm = TRUE)
    ratio <- if (act > 0) (j + m) / act else 0
    round(ratio, 2)
  })
  
  # ================= Gráficas =================
  # Evolución histórica
  output$plot_hist <- renderPlotly({
    df <- base_filtrada() %>%
      group_by(ano) %>%
      summarise(poblacion = sum(poblacion, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(df, aes(x = ano, y = poblacion,
                        text = paste0("Año: ", ano, "<br>Población: ", fmt_comma(poblacion)))) +
      geom_line(color = "#2563eb", linewidth = 1.2) +
      geom_point(color = "#2563eb", size = 2) +
      scale_y_continuous(labels = fmt_comma) +
      labs(x = NULL, y = "Población total") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  # Pirámide (decenios, hombres izq, mujeres der)
  output$plot_piramide <- renderPlotly({
    df <- base_filtrada() %>% 
      filter(ano == input$f_ano) %>%
      mutate(
        grupo_edad = make_dec(edad),
        sexo_lc = tolower(trimws(as.character(sexo))),
        # Normalizamos a dos niveles: hombres/mujeres (soporta 'hombre','hombres','varon', etc.)
        sexo_lc = ifelse(grepl("^h|^m[aá]sc|^var", sexo_lc), "hombres", 
                         ifelse(grepl("^m|^fem", sexo_lc), "mujeres", sexo_lc))
      ) %>%
      group_by(sexo_lc, grupo_edad) %>%
      summarise(pob = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        sexo_lc = ifelse(sexo_lc %in% c("hombre","hombres"), "hombres", "mujeres"),
        pob_plot = ifelse(sexo_lc == "hombres", -pob, pob)
      )
    
    shiny::validate(shiny::need(nrow(df) > 0, "Sin datos para mostrar pirámide."))
    
    p <- ggplot(df, aes(x = grupo_edad, y = pob_plot, fill = sexo_lc,
                        text = paste0("Grupo: ", grupo_edad,
                                      "<br>Sexo: ", tools::toTitleCase(sexo_lc),
                                      "<br>Población: ", fmt_comma(pob)))) +
      geom_col(width = 0.9) +
      coord_flip() +
      scale_y_continuous(labels = function(x) fmt_comma(abs(x))) +
      scale_fill_manual(
        values = c("hombres"="#3b82f6","mujeres"="#f43f5e"),
        breaks = c("hombres","mujeres"),
        labels = c("Hombres","Mujeres"),
        name   = "Sexo"
      ) +
      labs(x = "Grupo de edad (intervalos de 10 años)", y = "Población") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)
