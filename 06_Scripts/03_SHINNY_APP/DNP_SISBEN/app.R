# =========================================================
# app_sisben_dashboard_v2.R — Sisbén (estilo moderno, con histórico A+B)
# =========================================================
suppressWarnings({
  library(shiny); library(dplyr); library(ggplot2); library(plotly)
  library(scales); library(bslib)
})

options(stringsAsFactors = FALSE, scipen = 999)

# ---------- Ruta y carga ----------
data_dir  <- "C:/Users/Dell/Universidad de los andes/FAO-SAT - Documentos/General/06_Scripts/03_SNINNY_APP/DNP_SISBEN/data"
sisben_path <- file.path(data_dir, "031_DNP_SISBEN.rds")
sisben <- readRDS(sisben_path)


# ---------- Etiquetas para i1–i15 (IPM proxy) ----------
i_labels <- c(
  i1  = "Bajo logro educativo",
  i2  = "Analfabetismo",
  i3  = "Inasistencia escolar",
  i4  = "Rezago escolar",
  i5  = "Barreras a servicios de cuidado primera infancia",
  i6  = "Trabajo infantil",
  i7  = "Desempleo de larga duración",
  i8  = "Trabajo informal",
  i9  = "Sin aseguramiento en salud",
  i10 = "Barreras de acceso a servicios de salud",
  i11 = "Sin acceso a fuentes de agua mejorada",
  i12 = "Eliminación inadecuada de excretas",
  i13 = "Material inadecuado de pisos",
  i14 = "Material inadecuado de paredes exteriores",
  i15 = "Hacinamiento crítico"
)

priv_cols <- intersect(names(sisben), paste0("i", 1:15))

# ---------- Helpers ----------
fmt_comma <- function(x) comma(x, big.mark = ".", decimal.mark = ",")

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
    .filters-grid{display:grid;grid-template-columns:repeat(5,minmax(180px,1fr));gap:12px}
    .filter-label{font-size:12px;font-weight:600;letter-spacing:.4px;
                  text-transform:uppercase;color:#6b7280;margin-bottom:6px}
    .card{background:#fff;border:1px solid #eaecef;border-radius:16px;padding:12px;
          box-shadow:0 2px 10px rgba(0,0,0,.05);margin-bottom:12px}
    .card-title{font-weight:700;font-size:16px;margin-bottom:8px;color:#111827}
    .kpi{font-weight:800;font-size:28px;color:#111827}
    .kpi-sub{font-size:12px;color:#6b7280;margin-top:-4px}
  "))),
  
  div(class="wrap",
      h3("Sisbén — Explorador de privaciones"),
      div(class="data-note","Filtros generales e indicadores para apoyar decisiones de política pública."),
      
      # ---------- Filtros ----------
      div(class="filters",
          div(class="filters-grid",
              div(class="filter", div(class="filter-label","Año"),
                  selectInput("f_ano", NULL, choices = sort(unique(sisben$ano)), selected = max(sisben$ano))),
              div(class="filter", div(class="filter-label","Trimestre"),
                  selectInput("f_trim", NULL, choices = c("Todos", sort(unique(sisben$trimestre))), selected = "Todos")),
              div(class="filter", div(class="filter-label","Departamento"),
                  selectInput("f_dep", NULL, choices = c("Todos", sort(unique(sisben$DEPARTAMENTO_D))), selected = "Todos")),
              div(class="filter", div(class="filter-label","Municipio"),
                  selectInput("f_mun", NULL, choices = c("Todos", sort(unique(sisben$MUNICIPIO_D))), selected = "Todos")),
              div(class="filter", div(class="filter-label","Grupo Sisbén"),
                  selectInput("f_grupo", NULL, choices = c("Todos", sort(unique(sisben$grupo))), selected = "Todos"))
          )
      ),
      
      # ---------- KPIs ----------
      fluidRow(
        column(3, div(class="card",
                      div(class="card-title","Población total (ponderada)"),
                      div(class="kpi", textOutput("kpi_pob")),
                      div(class="kpi-sub","Suma de fex.y")
        )),
        column(3, div(class="card",
                      div(class="card-title","% en pobreza (A+B)"),
                      div(class="kpi", textOutput("kpi_ab")),
                      div(class="kpi-sub","Suma ponderada de A+B sobre total")
        )),
        column(3, div(class="card",
                      div(class="card-title","% con ≥1 privación"),
                      div(class="kpi", textOutput("kpi_anypriv")),
                      div(class="kpi-sub","Al menos una i1–i15 activa")
        )),
        column(3, div(class="card",
                      div(class="card-title","Promedio de privaciones"),
                      div(class="kpi", textOutput("kpi_prompriv")),
                      div(class="kpi-sub","Promedio ponderado por persona")
        ))
      ),
      
      # ---------- Visuales ----------
      fluidRow(
        column(6, div(class="card",
                      div(class="card-title","Distribución por grupos (A–D)"),
                      plotlyOutput("plot_grupos", height = 360)
        )),
        column(6, div(class="card",
                      div(class="card-title","Top-10 privaciones (prevalencia ponderada)"),
                      plotlyOutput("plot_priv_top", height = 360)
        ))
      ),
      
      # ---------- Evolución histórica pobreza (A+B) ----------
      fluidRow(
        column(12, div(class="card",
                       div(class="card-title","Evolución histórica — % en pobreza (A+B)"),
                       plotlyOutput("plot_pobreza_hist", height = 360)
        ))
      )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  # --- Filtro dependiente de municipios por departamento ---
  observeEvent(input$f_dep, {
    if (is.null(input$f_dep) || input$f_dep == "Todos"){
      updateSelectInput(session, "f_mun",
                        choices = c("Todos", sort(unique(sisben$MUNICIPIO_D))),
                        selected = "Todos")
    } else {
      ch <- sisben %>%
        filter(DEPARTAMENTO_D == input$f_dep) %>%
        distinct(MUNICIPIO_D) %>%
        arrange(MUNICIPIO_D) %>% pull(MUNICIPIO_D)
      updateSelectInput(session, "f_mun",
                        choices = c("Todos", ch),
                        selected = "Todos")
    }
  }, ignoreInit = TRUE)
  
  # --- Base filtrada ---
  base_filtrada <- reactive({
    df <- sisben %>% filter(ano == input$f_ano)
    if (input$f_trim != "Todos") df <- df %>% filter(trimestre == input$f_trim)
    if (input$f_dep  != "Todos") df <- df %>% filter(DEPARTAMENTO_D == input$f_dep)
    if (input$f_mun  != "Todos") df <- df %>% filter(MUNICIPIO_D   == input$f_mun)
    if (input$f_grupo!= "Todos") df <- df %>% filter(grupo == input$f_grupo)
    df
  })
  
  # --- KPIs ---
  output$kpi_pob <- renderText({
    fmt_comma(sum(base_filtrada()$fex.y, na.rm = TRUE))
  })
  
  output$kpi_ab <- renderText({
    df <- base_filtrada()
    tot <- sum(df$fex.y, na.rm = TRUE)
    ab  <- sum(df$fex.y[df$grupo %in% c("A","B")], na.rm = TRUE)
    pct <- if (tot > 0) 100 * ab / tot else 0
    paste0(format(round(pct, 1), nsmall = 1, decimal.mark = ","), "%")
  })
  
  output$kpi_anypriv <- renderText({
    df <- base_filtrada()
    if (length(priv_cols) == 0) return("0%")
    any_w <- sum((rowSums(df[, priv_cols], na.rm = TRUE) > 0) * df$fex.y, na.rm = TRUE)
    tot_w <- sum(df$fex.y, na.rm = TRUE)
    pct <- if (tot_w > 0) 100 * any_w / tot_w else 0
    paste0(format(round(pct, 1), nsmall = 1, decimal.mark = ","), "%")
  })
  
  output$kpi_prompriv <- renderText({
    df <- base_filtrada()
    if (length(priv_cols) == 0) return("0")
    sum_w <- sum(rowSums(df[, priv_cols], na.rm = TRUE) * df$fex.y, na.rm = TRUE)
    tot_w <- sum(df$fex.y, na.rm = TRUE)
    prom  <- if (tot_w > 0) sum_w / tot_w else 0
    format(round(prom, 2), decimal.mark = ",")
  })
  
  # --- Distribución por grupos ---
  output$plot_grupos <- renderPlotly({
    df <- base_filtrada() %>%
      group_by(grupo) %>%
      summarise(poblacion = sum(fex.y, na.rm = TRUE), .groups = "drop") %>%
      mutate(grupo = factor(grupo, levels = sort(unique(sisben$grupo))))
    
    p <- ggplot(df, aes(x = grupo, y = poblacion, fill = grupo,
                        text = paste0("Grupo: ", grupo, "<br>Población: ", fmt_comma(poblacion)))) +
      geom_col() +
      scale_y_continuous(labels = fmt_comma) +
      labs(x = NULL, y = "Población ponderada", fill = "Grupo") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # --- Top-10 privaciones (prevalencia ponderada) ---
  output$plot_priv_top <- renderPlotly({
    df <- base_filtrada()
    if (length(priv_cols) == 0 || nrow(df) == 0) {
      return(NULL)
    }
    tot_w <- sum(df$fex.y, na.rm = TRUE)
    if (is.na(tot_w) || tot_w == 0) return(NULL)
    
    prev <- lapply(priv_cols, function(v){
      num <- sum(df$fex.y[df[[v]] == 1], na.rm = TRUE)
      data.frame(var = v, prev = num / tot_w, stringsAsFactors = FALSE)
    }) %>% bind_rows()
    
    prev <- prev %>%
      mutate(label = ifelse(var %in% names(i_labels), i_labels[var], var)) %>%
      arrange(desc(prev)) %>% slice_head(n = 10)
    
    p <- ggplot(prev, aes(x = prev, y = reorder(label, prev),
                          text = paste0(label, "<br>Prevalencia: ", percent(prev, accuracy = 0.1, decimal.mark=",")))) +
      geom_col(fill = "#3b82f6") +
      scale_x_continuous(labels = percent_format(accuracy = 1, decimal.mark=",")) +
      labs(x = "Prevalencia ponderada", y = NULL) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # --- Evolución histórica pobreza (A+B) ---
  output$plot_pobreza_hist <- renderPlotly({
    df <- sisben
    
    # aplicar filtros (excepto año, porque queremos serie completa)
    if (input$f_trim != "Todos") df <- df %>% filter(trimestre == input$f_trim)
    if (input$f_dep  != "Todos") df <- df %>% filter(DEPARTAMENTO_D == input$f_dep)
    if (input$f_mun  != "Todos") df <- df %>% filter(MUNICIPIO_D   == input$f_mun)
    
    # calcular % A+B por año
    serie <- df %>%
      group_by(ano) %>%
      summarise(
        total = sum(fex.y, na.rm = TRUE),
        ab    = sum(fex.y[grupo %in% c("A","B")], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(pct_ab = ifelse(total > 0, 100 * ab / total, 0))
    
    # gráfico en barras
    p <- ggplot(serie, aes(x = factor(ano), y = pct_ab,
                           text = paste0("Año: ", ano,
                                         "<br>% A+B: ", round(pct_ab,1), "%"))) +
      geom_col(fill = "#2563eb") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = NULL, y = "% población en pobreza (A+B)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui, server)
