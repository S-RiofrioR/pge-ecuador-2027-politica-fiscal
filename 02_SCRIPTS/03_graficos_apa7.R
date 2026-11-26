# ==============================================================================
# 03_graficos_apa7.R
# Genera 6 figuras en dos versiones (PNG, 300 DPI, 8 x 5 pulgadas):
#   - 03_EXPORT/graficos_informe/     : sin título interno (APA 7 estricto)
#   - 03_EXPORT/graficos_exposicion/  : con título y subtítulo (diapositivas)
# Lee 01_DATOS/processed/resultados_fiscales.rds (generado por el Script 2).
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(patchwork)
  library(glue)
})

ruta_base <- "C:/Users/Oficina/Desktop/COMPONENTE PRACTICO 2 - POLITICA FISCAL"
dir_proc  <- file.path(ruta_base, "01_DATOS", "processed")
dir_inf   <- file.path(ruta_base, "03_EXPORT", "graficos_informe")
dir_exp   <- file.path(ruta_base, "03_EXPORT", "graficos_exposicion")
for (d in c(dir_inf, dir_exp)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

archivo_rds <- file.path(dir_proc, "resultados_fiscales.rds")
if (!file.exists(archivo_rds)) stop("No existe ", archivo_rds, ". Ejecute primero el Script 02.")
res <- readRDS(archivo_rds)

# ------------------------------------------------------------------------------
# Estética común
# ------------------------------------------------------------------------------
marino <- "#1B365D"; acero <- "#4A777A"; pizarra <- "#6C757D"
terracota <- "#B84A39"; ocre <- "#C99700"

fmt_es <- function(x, d = 2) {
  out <- scales::number(x, accuracy = 10^(-d), big.mark = ".", decimal.mark = ",")
  out[is.na(x)] <- "—"
  out
}
fmt_sg <- function(x, d = 2) {
  out <- fmt_es(x, d)
  pos <- !is.na(x) & x > 0
  out[pos] <- paste0("+", out[pos])
  out
}
lab_num <- scales::label_number(big.mark = ".", decimal.mark = ",")

theme_apa <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid        = element_blank(),
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      axis.line         = element_line(colour = "grey30", linewidth = 0.4),
      axis.ticks        = element_line(colour = "grey30", linewidth = 0.3),
      axis.text         = element_text(colour = "grey15"),
      axis.title        = element_text(colour = "grey15"),
      legend.position   = "bottom",
      legend.title      = element_text(face = "plain"),
      plot.title        = element_text(face = "bold", colour = marino, size = base_size + 3, hjust = 0),
      plot.subtitle     = element_text(colour = pizarra, size = base_size, hjust = 0),
      plot.title.position = "plot",
      plot.margin       = margin(8, 12, 8, 8)
    )
}

con_titulo <- function(p, titulo, subtitulo) {
  if (inherits(p, "patchwork")) {
    p + plot_annotation(
      title = titulo, subtitle = subtitulo,
      theme = theme(
        plot.title    = element_text(face = "bold", colour = marino, size = 13, hjust = 0),
        plot.subtitle = element_text(colour = pizarra, size = 9.5, hjust = 0),
        plot.background = element_rect(fill = "white", colour = NA)))
  } else {
    p + labs(title = titulo, subtitle = subtitulo)
  }
}

exportar_fig <- function(nombre, p, titulo, subtitulo) {
  f_inf <- file.path(dir_inf, paste0(nombre, ".png"))
  f_exp <- file.path(dir_exp, paste0(nombre, ".png"))
  ggsave(f_inf, plot = p, width = 8, height = 5, dpi = 300, bg = "white")
  ggsave(f_exp, plot = con_titulo(p, titulo, subtitulo), width = 8, height = 5, dpi = 300, bg = "white")
  message("  [OK] ", nombre)
}

# ---- Cascada (waterfall) reutilizable --------------------------------------------
cascada_datos <- function(etiqueta, valor, tipo) {
  n <- length(valor); inicio <- numeric(n); fin <- numeric(n); acum <- 0
  for (i in seq_len(n)) {
    if (tipo[i] == "total") {
      inicio[i] <- 0; fin[i] <- valor[i]; acum <- valor[i]
    } else {
      inicio[i] <- acum; fin[i] <- acum + valor[i]; acum <- fin[i]
    }
  }
  tibble(etq = str_wrap(etiqueta, 14), x = seq_len(n), valor = valor, tipo = tipo,
         inicio = inicio, fin = fin, ymin = pmin(inicio, fin), ymax = pmax(inicio, fin),
         grupo = case_when(tipo == "total" ~ "Total", valor >= 0 ~ "Aumenta", TRUE ~ "Disminuye"))
}
grafico_cascada <- function(d, base_size = 10, lab_size = 3.4) {
  conn <- d %>% transmute(x0 = x + 0.36, x1 = lead(x) - 0.36, y0 = fin) %>% filter(!is.na(x1))
  ggplot(d) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey30") +
    geom_rect(aes(xmin = x - 0.36, xmax = x + 0.36, ymin = ymin, ymax = ymax, fill = grupo)) +
    geom_segment(data = conn, aes(x = x0, xend = x1, y = y0, yend = y0),
                 linewidth = 0.35, colour = "grey40", linetype = "dashed") +
    geom_text(aes(x = x, y = if_else(valor >= 0, ymax, ymin), label = fmt_sg(valor),
                  vjust = if_else(valor >= 0, -0.5, 1.5)),
              size = lab_size, fontface = "bold") +
    scale_fill_manual(values = c(Total = marino, Aumenta = acero, Disminuye = terracota), guide = "none") +
    scale_x_continuous(breaks = d$x, labels = d$etq) +
    scale_y_continuous(labels = lab_num, expand = expansion(mult = c(0.15, 0.15))) +
    labs(x = NULL, y = "Millones de USD") +
    theme_apa(base_size) + theme(axis.line.x = element_blank())
}

# Valores recurrentes
vc <- res$valores_clave
ind <- res$indicadores
ind_val <- function(k) ind$valor[ind$n == k]

message("Generando figuras en:\n  ", dir_inf, "\n  ", dir_exp)

# ==============================================================================
# FIG 01: PGE 2026 vs 2027
# ==============================================================================
fig01 <- local({
  m <- res$macro
  d <- tibble(
    anio = c("2026", "2027"),
    pge  = c(m$valor_2026[m$indicador == "pge_total"],  m$valor_2027[m$indicador == "pge_total"]),
    pct  = c(m$valor_2026[m$indicador == "pge_pct_pib"], m$valor_2027[m$indicador == "pge_pct_pib"]))
  var_pge <- ind_val(1)
  ymax <- max(d$pge)
  ggplot(d, aes(anio, pge, fill = anio)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = paste0(fmt_es(pge), " M USD\n", fmt_es(pct), "% del PIB")),
              vjust = -0.5, size = 3.5, lineheight = 0.95) +
    annotate("segment", x = 1, xend = 2, y = ymax * 1.17, yend = ymax * 1.17,
             colour = marino, linewidth = 0.5, arrow = arrow(length = unit(0.2, "cm"))) +
    annotate("text", x = 1.5, y = ymax * 1.17, label = paste0(fmt_sg(var_pge), "%"),
             vjust = -0.7, fontface = "bold", colour = marino, size = 4.2) +
    scale_fill_manual(values = c("2026" = pizarra, "2027" = marino), guide = "none") +
    scale_y_continuous(labels = lab_num, expand = expansion(mult = c(0, 0.02)), limits = c(0, ymax * 1.32)) +
    labs(x = NULL, y = "Millones de USD") +
    theme_apa(10)
})

# ==============================================================================
# FIG 02: Composición de ingresos y regla fiscal (panel doble)
# ==============================================================================
fig02 <- local({
  nat <- res$ingresos %>%
    filter(bloque == "naturaleza") %>%
    mutate(concepto = factor(concepto, levels = rev(concepto)))
  pA <- ggplot(nat, aes(monto_usd_m, concepto, fill = concepto)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = paste0(fmt_es(monto_usd_m), "\n(", fmt_es(participacion_pct), "%)")),
              hjust = -0.08, size = 2.7, lineheight = 0.9) +
    scale_fill_manual(values = c("Ingresos corrientes" = marino, "Ingresos de capital" = acero,
                                 "Financiamiento" = ocre), guide = "none") +
    scale_x_continuous(labels = lab_num, expand = expansion(mult = c(0, 0.5))) +
    labs(x = "Composición del PGE 2027 (millones de USD)", y = NULL) +
    theme_apa(8.5)

  r <- res$ingresos %>%
    filter(bloque == "regla_fiscal") %>%
    mutate(tipo  = if_else(str_detect(concepto, "^Ingresos"), "Ingresos", "Gastos"),
           grupo = if_else(str_detect(concepto, "no permanentes"), "No permanentes", "Permanentes"))
  bal <- r %>%
    select(tipo, grupo, monto_usd_m) %>%
    pivot_wider(names_from = tipo, values_from = monto_usd_m) %>%
    mutate(balance = Ingresos - Gastos, ymax = pmax(Ingresos, Gastos))
  pB <- ggplot(r, aes(grupo, monto_usd_m, fill = tipo)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.62) +
    geom_text(aes(label = fmt_es(monto_usd_m)), position = position_dodge(width = 0.7),
              vjust = -0.4, size = 2.6) +
    geom_text(data = bal, aes(x = grupo, y = ymax * 1.22, label = paste0("Balance:\n", fmt_sg(balance), " M")),
              inherit.aes = FALSE, colour = terracota, fontface = "bold", size = 2.9, lineheight = 0.9) +
    scale_fill_manual(values = c(Ingresos = marino, Gastos = pizarra)) +
    scale_y_continuous(labels = lab_num, expand = expansion(mult = c(0, 0.28))) +
    labs(x = "Regla fiscal (Art. 286 CRE)", y = "Millones de USD", fill = NULL) +
    theme_apa(8.5)
  (pA | pB) + plot_layout(widths = c(1, 1.1))
})

# ==============================================================================
# FIG 03: Estructura tributaria y regresividad
# ==============================================================================
fig03 <- local({
  t <- res$tributario %>% mutate(impuesto = fct_reorder(impuesto, monto_usd_m))
  tot <- vc$trib_total
  res_tipo <- res$tributario %>%
    group_by(tipo_impuesto) %>% summarise(m = sum(monto_usd_m), .groups = "drop") %>%
    mutate(p = m / tot * 100)
  gp <- function(tp) res_tipo$p[res_tipo$tipo_impuesto == tp]
  texto <- paste0("Peso en el total tributario\n",
                  "Indirectos: ", fmt_es(gp("Indirecto")), "%\n",
                  "Directos: ", fmt_es(gp("Directo")), "%\n",
                  "Mixtos: ", fmt_es(gp("Mixto")), "%\n",
                  "Razón indirectos/directos: ", fmt_es(gp("Indirecto") / gp("Directo")), " veces")
  xmax <- max(t$monto_usd_m)
  ggplot(t, aes(monto_usd_m, impuesto, fill = tipo_impuesto)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = paste0(fmt_es(monto_usd_m), " (", fmt_es(participacion_pct), "%)")),
              hjust = -0.06, size = 3) +
    annotate("text", x = xmax * 1.55, y = 2.5, label = texto, hjust = 1, size = 3.1, lineheight = 1.05) +
    scale_fill_manual(values = c(Directo = marino, Indirecto = ocre, Mixto = pizarra)) +
    scale_x_continuous(labels = lab_num, expand = expansion(mult = c(0, 0.6))) +
    labs(x = "Millones de USD", y = NULL, fill = "Tipo de impuesto") +
    theme_apa(10)
})

# ==============================================================================
# FIG 04: Prioridades de gasto y metas constitucionales (panel doble)
# ==============================================================================
fig04 <- local({
  gs <- res$gasto %>%
    filter(categoria == "sectorial_consigna") %>%
    mutate(es_pai = str_detect(concepto, "PAI"),
           etq = fct_reorder(str_wrap(concepto, 24), monto_usd_m))
  pA <- ggplot(gs, aes(monto_usd_m, etq, fill = es_pai)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = fmt_es(monto_usd_m)), hjust = -0.08, size = 2.8) +
    scale_fill_manual(values = c(`FALSE` = marino, `TRUE` = acero), guide = "none") +
    scale_x_continuous(labels = lab_num, expand = expansion(mult = c(0, 0.25))) +
    labs(x = "Asignación 2027 (millones de USD; rubros no sumables)", y = NULL) +
    theme_apa(8.5)

  m <- res$metas %>% mutate(sector = factor(sector, levels = sector), xn = as.numeric(sector))
  pB <- ggplot(m, aes(sector, pct_pib, fill = sector)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = paste0(fmt_es(pct_pib), "%")), vjust = 1.5, colour = "white",
              fontface = "bold", size = 3.2) +
    geom_segment(aes(x = xn - 0.36, xend = xn + 0.36, y = meta_pct_pib, yend = meta_pct_pib),
                 colour = terracota, linewidth = 1.1) +
    geom_text(aes(y = pmax(pct_pib, meta_pct_pib),
                  label = paste0("Meta: ", fmt_es(meta_pct_pib, 0), "%\n(", fmt_sg(brecha_pp), " p. p.)")),
              vjust = -0.4, colour = terracota, fontface = "bold", size = 2.9, lineheight = 0.9) +
    scale_fill_manual(values = c(marino, acero), guide = "none") +
    scale_x_discrete(labels = function(x) str_wrap(x, 14)) +
    scale_y_continuous(labels = scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ","),
                       expand = expansion(mult = c(0, 0.3))) +
    labs(x = NULL, y = "% del PIB 2027") +
    theme_apa(8.5)
  (pA | pB) + plot_layout(widths = c(1.15, 1))
})

# ==============================================================================
# FIG 05: Cascada del balance primario al déficit global
# ==============================================================================
fig05 <- local({
  d <- cascada_datos(res$cascada$etiqueta, res$cascada$valor, res$cascada$tipo)
  grafico_cascada(d, base_size = 10, lab_size = 3.6)
})

# ==============================================================================
# FIG 06: Simulación de escenarios fiscales
# ==============================================================================
fig06 <- local({
  e <- res$escenarios %>%
    mutate(esc = factor(c("Presupuestado", "Choque\nsimultáneo", "Con paquete\nde ajuste"),
                        levels = c("Presupuestado", "Choque\nsimultáneo", "Con paquete\nde ajuste")))
  pA <- ggplot(e, aes(esc, balance_global, fill = esc)) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey30") +
    geom_col(width = 0.6) +
    geom_text(aes(label = paste0(fmt_es(balance_global), " M\n(", fmt_es(deficit_pct_pib), "% del PIB)")),
              vjust = 1.15, size = 2.8, lineheight = 0.95, colour = "white", fontface = "bold") +
    scale_fill_manual(values = c(marino, terracota, acero), guide = "none") +
    scale_y_continuous(labels = lab_num, expand = expansion(mult = c(0.05, 0.1))) +
    labs(x = "Déficit global por escenario", y = "Millones de USD") +
    theme_apa(8.5) + theme(axis.line.x = element_blank())

  ch <- e[2, ]; pq <- e[3, ]
  d <- cascada_datos(c("Déficit con choque", "Medidas de ingreso", "Medidas de gasto", "Déficit con paquete"),
                     c(ch$balance_global, pq$medidas_ingreso, pq$medidas_gasto, pq$balance_global),
                     c("total", "delta", "delta", "total"))
  pB <- grafico_cascada(d, base_size = 8.5, lab_size = 2.8) +
    labs(x = "Aporte del paquete de ajuste")
  (pA | pB) + plot_layout(widths = c(1, 1.2))
})

# ------------------------------------------------------------------------------
# Exportación (versión informe y versión exposición)
# ------------------------------------------------------------------------------
edu_pct <- res$metas$pct_pib[1]; sal_pct <- res$metas$pct_pib[2]
fin_pct <- res$ingresos$participacion_pct[res$ingresos$concepto == "Financiamiento"]
reg <- res$ingresos %>% filter(bloque == "regla_fiscal")
def_perm <- reg$monto_usd_m[reg$concepto == "Ingresos permanentes"] - reg$monto_usd_m[reg$concepto == "Gastos permanentes"]
esc <- res$escenarios

exportar_fig("Fig01_Comparativo_PGE_2026_2027", fig01,
             paste0("El PGE 2027 crece ", fmt_es(ind_val(1)), "% frente a 2026"),
             "Presupuesto General del Estado, millones de USD y % del PIB")
exportar_fig("Fig02_Composicion_Ingresos_y_Regla_Fiscal_2027", fig02,
             paste0("El ", fmt_es(fin_pct), "% del PGE depende de financiamiento"),
             paste0("La cuenta permanente (Art. 286 CRE) registra un déficit de ", fmt_es(def_perm), " M USD"))
exportar_fig("Fig03_Estructura_Tributaria_Regresividad_2027", fig03,
             "Estructura tributaria 2027: predominan los impuestos indirectos",
             paste0("Los indirectos aportan ", fmt_es(ind_val(9)), "% de la recaudación; los directos, ",
                    fmt_es(ind_val(10)), "%"))
exportar_fig("Fig04_Prioridades_Gasto_y_Metas_Constitucionales_2027", fig04,
             "Prioridades de gasto y metas constitucionales",
             paste0("Educación: ", fmt_es(edu_pct), "% del PIB (meta 6%); Salud: ", fmt_es(sal_pct),
                    "% del PIB (meta 4%)"))
exportar_fig("Fig05_Cascada_Balance_Primario_a_Deficit_Global_2027", fig05,
             "Del superávit primario al déficit global",
             paste0("Los intereses (", fmt_es(vc$def27 + res$cascada$valor[1]), " M USD) superan el superávit primario"))
exportar_fig("Fig06_Simulacion_Escenarios_Fiscales_2027", fig06,
             "Déficit global bajo tres escenarios fiscales",
             paste0("El choque eleva el déficit a ", fmt_es(esc$deficit_pct_pib[2]),
                    "% del PIB; el paquete lo reduce a ", fmt_es(esc$deficit_pct_pib[3]), "%"))

message("Script 3 finalizado: 12 PNG (6 informe + 6 exposición).")
