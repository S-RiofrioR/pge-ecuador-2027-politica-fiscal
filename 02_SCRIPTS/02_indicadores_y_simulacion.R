# ==============================================================================
# 02_indicadores_y_simulacion.R
# Lee los 5 CSV de 01_DATOS/raw/, calcula indicadores, ejecuta el motor de
# simulación (3 escenarios), guarda resultados en 01_DATOS/processed/ y exporta
# 5 tablas APA 7 (.html y .csv) + resumen de texto plano.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(gt)
  library(glue)
})

ruta_base <- "C:/Users/Oficina/Desktop/COMPONENTE PRACTICO 2 - POLITICA FISCAL"
dir_raw   <- file.path(ruta_base, "01_DATOS", "raw")
dir_proc  <- file.path(ruta_base, "01_DATOS", "processed")
dir_tab   <- file.path(ruta_base, "03_EXPORT", "tablas_apa7")
dir_rep   <- file.path(ruta_base, "03_EXPORT", "reportes")
for (d in c(dir_proc, dir_tab, dir_rep)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Funciones auxiliares
# ------------------------------------------------------------------------------
# Formato español: miles ".", decimales ","
fmt_es <- function(x, d = 2) {
  out <- scales::number(x, accuracy = 10^(-d), big.mark = ".", decimal.mark = ",")
  out[is.na(x)] <- "—"
  out
}
# Igual, con signo "+" para valores positivos
fmt_sg <- function(x, d = 2) {
  out <- fmt_es(x, d)
  pos <- !is.na(x) & x > 0
  out[pos] <- paste0("+", out[pos])
  out
}
# Selecciona exactamente un valor de una columna según una condición lógica
sel1 <- function(df, cond, col = "monto_usd_m") {
  v <- df[[col]][cond]
  stopifnot(length(v) == 1L, !is.na(v))
  v
}
leer <- function(f) {
  readr::read_csv(file.path(dir_raw, f), show_col_types = FALSE,
                  locale = readr::locale(encoding = "UTF-8"))
}
chequeo <- function(desc, a, b, tol = 0.05) {
  ok <- abs(a - b) <= tol
  message(sprintf("  [%s] %s (dif = %s)", if (ok) "OK" else "REVISAR", desc,
                  fmt_es(a - b)))
  invisible(ok)
}

# ------------------------------------------------------------------------------
# 1. Lectura de datos crudos
# ------------------------------------------------------------------------------
macro <- leer("01_macro_comparativo.csv")
ing   <- leer("02_ingresos_balance_2027.csv")
trib  <- leer("03_sistema_tributario_2027.csv")
gasto <- leer("04_gasto_y_asignaciones_2027.csv")
parm  <- leer("05_parametros_simulacion_2027.csv")

mac <- function(ind, anio) {
  v <- macro[[paste0("valor_", anio)]][macro$indicador == ind]
  stopifnot(length(v) == 1L)
  v
}
prm <- function(nombre, col) {
  v <- parm[[col]][parm$parametro == nombre]
  stopifnot(length(v) == 1L)
  v
}

# ---- Valores clave -----------------------------------------------------------
pge26 <- mac("pge_total", 2026);  pge27 <- mac("pge_total", 2027)
pib26 <- mac("pib_nominal", 2026); pib27 <- mac("pib_nominal", 2027)
def27 <- mac("deficit_global", 2027)         # magnitud positiva
sup_prim27 <- mac("superavit_primario", 2027)
int27 <- mac("intereses_deuda", 2027)

ing_corr <- sel1(ing, ing$bloque == "naturaleza" & ing$concepto == "Ingresos corrientes")
ing_cap  <- sel1(ing, ing$bloque == "naturaleza" & ing$concepto == "Ingresos de capital")
financ   <- sel1(ing, ing$bloque == "naturaleza" & ing$concepto == "Financiamiento")
ing_perm <- sel1(ing, ing$bloque == "regla_fiscal" & ing$concepto == "Ingresos permanentes")
gas_perm <- sel1(ing, ing$bloque == "regla_fiscal" & ing$concepto == "Gastos permanentes")
ing_np   <- sel1(ing, ing$bloque == "regla_fiscal" & ing$concepto == "Ingresos no permanentes")
gas_np   <- sel1(ing, ing$bloque == "regla_fiscal" & ing$concepto == "Gastos no permanentes")
trib_total <- sel1(ing, ing$bloque == "subrubro_corriente" & ing$concepto == "Impuestos totales")
ing_sobre_linea <- sel1(ing, ing$bloque == "referencia" & ing$concepto == "Ingresos sobre la línea")
egr_sobre_linea <- sel1(ing, ing$bloque == "referencia" & ing$concepto == "Egresos sobre la línea")
ing_sens <- ing_corr + ing_cap               # 34.456,57 (nota de sensibilidad)

iva    <- sel1(trib, trib$impuesto == "IVA")
trib6  <- sum(trib$monto_usd_m[trib$impuesto != "Otros tributos"])   # 20.408
ind_monto <- sum(trib$monto_usd_m[trib$tipo_impuesto == "Indirecto"])
dir_monto <- sum(trib$monto_usd_m[trib$tipo_impuesto == "Directo"])
mix_monto <- sum(trib$monto_usd_m[trib$tipo_impuesto == "Mixto"])

gasto_fiscal <- sel1(gasto, gasto$categoria == "total_fiscal")
personal  <- sel1(gasto, gasto$concepto == "Gasto en personal")
interes_g <- sel1(gasto, gasto$concepto == "Intereses de la deuda pública")
pai       <- sel1(gasto, gasto$concepto == "Plan Anual de Inversiones - PAI")
edu_bas   <- sel1(gasto, gasto$concepto == "Educación inicial, básica y bachillerato")
univ      <- sel1(gasto, gasto$concepto == "Universidades y escuelas politécnicas")
salud     <- sel1(gasto, gasto$concepto == "Salud")
sub_comb  <- sel1(gasto, gasto$concepto == "Subsidio a combustibles")
sub_elec  <- sel1(gasto, gasto$concepto == "Subsidio sector eléctrico")
iess      <- sel1(gasto, gasto$concepto == "Transferencias Seguridad Social IESS/ISSFA/ISSPOL")
bonos     <- sel1(gasto, gasto$concepto == "Bonos y transferencias MTDH")
edu_total <- edu_bas + univ

# ---- Chequeos de consistencia (solo advertencias informativas) ----------------
message("Chequeos de consistencia:")
chequeo("PGE = corrientes + capital + financiamiento", pge27, ing_corr + ing_cap + financ)
chequeo("Impuestos totales = suma de tributos de la matriz", trib_total, sum(trib$monto_usd_m))
chequeo("Egresos - Ingresos sobre la línea = Déficit global", egr_sobre_linea - ing_sobre_linea, def27)
chequeo("Superávit primario - Intereses = -Déficit global", sup_prim27 - int27, -def27)
chequeo("Intereses (macro) = Intereses (grupo económico)", int27, interes_g)
chequeo("Ingresos corrientes = Impuestos + subrubros no tributarios", ing_corr,
        sum(ing$monto_usd_m[ing$bloque == "subrubro_corriente"]), tol = 0.5)

# ------------------------------------------------------------------------------
# 2. Indicadores fiscales
# ------------------------------------------------------------------------------
indicadores_obl <- tibble(
  n = 1:8,
  indicador = c("Crecimiento del presupuesto",
                "Presión tributaria del PGE",
                "Déficit fiscal respecto al PIB",
                "Dependencia del financiamiento",
                "Participación del IVA",
                "Participación del gasto en personal",
                "Carga de intereses",
                "Participación de la inversión pública"),
  formula = c("(PGE 2027 / PGE 2026 - 1) × 100",
              "Ingresos tributarios / PIB 2027 × 100",
              "Déficit global / PIB 2027 × 100",
              "Financiamiento / PGE 2027 × 100",
              "IVA / Ingresos tributarios × 100",
              "Gasto en personal / Gasto fiscal total × 100",
              "Intereses / Ingresos sin financiamiento × 100",
              "PAI / PGE 2027 × 100"),
  numerador   = c(pge27, trib_total, def27, financ, iva, personal, int27, pai),
  denominador = c(pge26, pib27, pib27, pge27, trib_total, gasto_fiscal, ing_sobre_linea, pge27),
  valor = c((numerador[1] / denominador[1] - 1) * 100,
            numerador[-1] / denominador[-1] * 100),
  unidad = "%",
  grupo  = "Obligatorio"
)

indicadores_comp <- tibble(
  n = 9:15,
  indicador = c("Participación de impuestos indirectos",
                "Participación de impuestos directos",
                "Razón impuestos indirectos / directos",
                "Déficit de cuenta permanente (Art. 286 CRE)",
                "Educación (incl. universidades) como % del PIB",
                "Salud como % del PIB",
                "Educación básica y bachillerato (sin universidades) como % del PIB"),
  formula = c("Impuestos indirectos / Ingresos tributarios × 100",
              "Impuestos directos / Ingresos tributarios × 100",
              "Impuestos indirectos / Impuestos directos",
              "Ingresos permanentes - Gastos permanentes",
              "(Educación básica + Universidades) / PIB 2027 × 100",
              "Salud / PIB 2027 × 100",
              "Educación básica / PIB 2027 × 100"),
  numerador   = c(ind_monto, dir_monto, ind_monto, ing_perm, edu_total, salud, edu_bas),
  denominador = c(trib_total, trib_total, dir_monto, gas_perm, pib27, pib27, pib27),
  valor = c(ind_monto / trib_total * 100,
            dir_monto / trib_total * 100,
            ind_monto / dir_monto,
            ing_perm - gas_perm,
            edu_total / pib27 * 100,
            salud / pib27 * 100,
            edu_bas / pib27 * 100),
  unidad = c("%", "%", "veces", "M USD", "%", "%", "%"),
  grupo  = "Complementario"
)

indicadores_sens <- tibble(
  n = 16:17,
  indicador = c("Presión tributaria (solo 6 tributos principales)",
                "Carga de intereses (ingresos corrientes + capital)"),
  formula = c("Suma de 6 tributos / PIB 2027 × 100",
              "Intereses / (Ingresos corrientes + capital) × 100"),
  numerador   = c(trib6, int27),
  denominador = c(pib27, ing_sens),
  valor = numerador / denominador * 100,
  unidad = "%",
  grupo  = "Sensibilidad"
)

indicadores <- bind_rows(indicadores_obl, indicadores_comp, indicadores_sens)

# ---- Tablas auxiliares procesadas --------------------------------------------
etiquetas_macro <- tribble(
  ~indicador,             ~etiqueta,                              ~tipo_var,
  "pge_total",            "Presupuesto General del Estado (PGE)", "nivel",
  "pge_pct_pib",          "PGE como % del PIB",                   "tasa",
  "pib_nominal",          "PIB nominal",                          "nivel",
  "crecimiento_real_pib", "Crecimiento real del PIB",             "tasa",
  "deflactor_pib",        "Deflactor del PIB",                    "tasa",
  "inflacion_promedio",   "Inflación promedio",                   "tasa",
  "produccion_petrolera", "Producción petrolera",                 "nivel",
  "precio_barril_crudo",  "Precio del barril de crudo",           "nivel",
  "deficit_global",       "Déficit global",                       "nivel",
  "deficit_pct_pib",      "Déficit global como % del PIB",        "tasa",
  "superavit_primario",   "Superávit primario",                   "nivel",
  "intereses_deuda",      "Intereses de la deuda",                "nivel"
)

macro_extra <- tibble(
  indicador   = c("pge_pct_pib", "deficit_pct_pib"),
  unidad      = "%",
  valor_2026  = c(pge26 / pib26 * 100, mac("deficit_global", 2026) / pib26 * 100),
  valor_2027  = c(pge27 / pib27 * 100, def27 / pib27 * 100)
)

macro_proc <- bind_rows(macro, macro_extra) %>%
  mutate(orden = match(indicador, etiquetas_macro$indicador)) %>%
  arrange(orden) %>%
  left_join(etiquetas_macro, by = "indicador") %>%
  mutate(var_abs = valor_2027 - valor_2026,
         var_rel = if_else(tipo_var == "nivel", (valor_2027 / valor_2026 - 1) * 100, NA_real_)) %>%
  select(-orden)

trib_proc <- trib %>%
  mutate(participacion_pct = monto_usd_m / trib_total * 100) %>%
  arrange(desc(monto_usd_m))

gasto_proc <- gasto %>%
  mutate(pct_pib = monto_usd_m / pib27 * 100,
         pct_gasto_fiscal = monto_usd_m / gasto_fiscal * 100)

metas <- tibble(
  sector       = c("Educación (incl. universidades)", "Salud"),
  monto        = c(edu_total, salud),
  pct_pib      = monto / pib27 * 100,
  meta_pct_pib = c(6, 4),
  brecha_pp    = pct_pib - meta_pct_pib,
  brecha_usd   = meta_pct_pib / 100 * pib27 - monto     # positivo = faltante
)

cascada <- tibble(
  etiqueta = c("Superávit primario", "Intereses de la deuda", "Déficit global"),
  valor    = c(sup_prim27, -int27, -def27),
  tipo     = c("total", "delta", "total")
)

# ------------------------------------------------------------------------------
# 3. Motor paramétrico de simulación fiscal
# ------------------------------------------------------------------------------
precio_b <- prm("precio_petroleo", "presupuestado");  precio_e <- prm("precio_petroleo", "estres")
crec_b   <- prm("crecimiento_real_pib", "presupuestado"); crec_e <- prm("crecimiento_real_pib", "estres")
defl     <- prm("deflactor_pib", "estres")
cumpl_b  <- prm("cumplimiento_meta_tributaria", "presupuestado") / 100
cumpl_e  <- prm("cumplimiento_meta_tributaria", "estres") / 100
sens     <- prm("sensibilidad_neta_crudo_1usd", "estres")

# ---- Escenario 1: base ---------------------------------------------------------
bal_base <- -def27                                    # -4.411,85

# ---- Escenario 2: choque simultáneo ---------------------------------------------
d_trib <- trib_total * (cumpl_e - cumpl_b)            # -10% de ingresos tributarios
d_pet  <- (precio_e - precio_b) * sens                # (50 - 66) × sensibilidad
# PIB nominal recalculado con crecimiento real de estrés y deflactor constante
# (se escala el PIB oficial 2027 para conservar su base estadística)
pib_estres <- pib27 * ((1 + crec_e / 100) * (1 + defl / 100)) /
                      ((1 + crec_b / 100) * (1 + defl / 100))
bal_choque <- bal_base + d_trib + d_pet
gap_choque <- -(d_trib + d_pet)                       # brecha fiscal generada (> 0)

# ==============================================================================
# SUPUESTOS EDITABLES - ESCENARIO 3 (Paquete de Política Fiscal)
# Modifique únicamente los porcentajes de esta sección.
# ==============================================================================
# Ingresos (porcentaje sobre la base indicada)
pct_I1 <- 2.5   # Recuperación de cartera vencida / control de evasión SRI (% de tributos en choque)
pct_I2 <- 1.5   # Revisión de gastos tributarios regresivos (% de tributos en choque)
pct_I3 <- 10    # Eficiencia/monetización en empresas públicas (% de ingresos corrientes no tributarios)
# Gasto
pct_G1 <- 15    # Focalización gradual de subsidios a combustibles (% del subsidio)
pct_G2 <- 20    # Recorte en bienes y servicios corrientes no esenciales (% de la base protegida)
pct_G3 <- 6     # Eficiencia en compras públicas / reperfilamiento financiero (% de PAI + residual)
participacion_edu_salud_en_residual <- 0.40  # supuesto: parte del residual de B&S ligada a Educación y Salud
recorte_edu_pct   <- 0   # Educación: 0% de recorte (restricción obligatoria)
recorte_salud_pct <- 0   # Salud: 0% de recorte (restricción obligatoria)
# ==============================================================================
stopifnot("Educación y Salud deben tener 0% de recorte" =
            recorte_edu_pct == 0 && recorte_salud_pct == 0)

trib_choque <- trib_total * cumpl_e
nt_corr     <- sum(ing$monto_usd_m[ing$bloque == "subrubro_corriente" &
                                   ing$concepto != "Impuestos totales"])
resid_bys   <- gas_perm - personal - interes_g - sub_comb - sub_elec - iess - bonos
stopifnot("El residual de bienes y servicios debe ser positivo" = resid_bys > 0)

supuestos_paquete <- tibble(
  id   = c("I1", "I2", "I3", "G1", "G2", "G3"),
  tipo = c(rep("Ingreso", 3), rep("Gasto", 3)),
  medida = c("Recuperación de cartera vencida y control de evasión (SRI)",
             "Revisión de gastos tributarios regresivos",
             "Eficiencia y monetización en empresas públicas",
             "Focalización gradual de subsidios a combustibles",
             "Recorte en bienes y servicios corrientes no esenciales",
             "Eficiencia en compras públicas y reperfilamiento financiero"),
  base_descripcion = c("Ingresos tributarios en escenario de choque",
                       "Ingresos tributarios en escenario de choque",
                       "Ingresos corrientes no tributarios",
                       "Subsidio a combustibles",
                       "Residual de bienes y servicios (excluye parte de Educación y Salud)",
                       "PAI + residual de bienes y servicios"),
  base_monto = c(trib_choque, trib_choque, nt_corr, sub_comb,
                 resid_bys * (1 - participacion_edu_salud_en_residual),
                 pai + resid_bys),
  supuesto_pct = c(pct_I1, pct_I2, pct_I3, pct_G1, pct_G2, pct_G3)
) %>%
  mutate(efecto_usd_m = base_monto * supuesto_pct / 100)

proteccion <- tibble(
  sector      = c("Educación (incl. universidades)", "Salud"),
  asignacion  = c(edu_total, salud),
  recorte_pct = c(recorte_edu_pct, recorte_salud_pct)
) %>%
  mutate(recorte_usd = asignacion * recorte_pct / 100,
         asignacion_post = asignacion - recorte_usd)

ing_med <- sum(supuestos_paquete$efecto_usd_m[supuestos_paquete$tipo == "Ingreso"])
gas_med <- sum(supuestos_paquete$efecto_usd_m[supuestos_paquete$tipo == "Gasto"])
bal_paq <- bal_choque + ing_med + gas_med

escenarios <- tibble(
  escenario = c("Escenario 1: Presupuestado", "Escenario 2: Choque simultáneo",
                "Escenario 3: Con paquete de ajuste"),
  precio_petroleo   = c(precio_b, precio_e, precio_e),
  crecimiento_real  = c(crec_b, crec_e, crec_e),
  cumplimiento_trib = c(cumpl_b, cumpl_e, cumpl_e) * 100,
  pib_nominal       = c(pib27, pib_estres, pib_estres),
  delta_tributario  = c(0, d_trib, d_trib),
  delta_petrolero   = c(0, d_pet, d_pet),
  medidas_ingreso   = c(0, 0, ing_med),
  medidas_gasto     = c(0, 0, gas_med),
  balance_global    = c(bal_base, bal_choque, bal_paq),
  brecha_fiscal     = c(0, gap_choque, gap_choque),
  brecha_cubierta   = c(0, 0, ing_med + gas_med)
) %>%
  mutate(deficit_pct_pib = -balance_global / pib_nominal * 100,
         cobertura_pct   = if_else(brecha_fiscal > 0, brecha_cubierta / brecha_fiscal * 100, NA_real_))

# ------------------------------------------------------------------------------
# 4. Guardado de resultados procesados (.rds y .csv)
# ------------------------------------------------------------------------------
resultados <- list(
  macro             = macro_proc,
  ingresos          = ing,
  tributario        = trib_proc,
  gasto             = gasto_proc,
  indicadores       = indicadores,
  metas             = metas,
  cascada           = cascada,
  escenarios        = escenarios,
  supuestos_paquete = supuestos_paquete,
  proteccion        = proteccion,
  valores_clave     = list(pib26 = pib26, pib27 = pib27, pge26 = pge26, pge27 = pge27,
                           trib_total = trib_total, def27 = def27, gasto_fiscal = gasto_fiscal)
)
saveRDS(resultados, file.path(dir_proc, "resultados_fiscales.rds"))
purrr::iwalk(resultados[names(resultados) != "valores_clave"],
             ~ readr::write_csv(.x, file.path(dir_proc, paste0(.y, ".csv"))))
message("Resultados procesados guardados en: ", dir_proc)

# ------------------------------------------------------------------------------
# 5. Tablas estilo APA 7 (gt -> .html; .csv)
# ------------------------------------------------------------------------------
tabla_apa7 <- function(df, numero, titulo, notas = character(),
                       alinear_der = character(), grupo = NULL) {
  g <- if (is.null(grupo)) gt::gt(df) else gt::gt(df, groupname_col = grupo)
  g <- g %>%
    tab_header(title    = md(paste0("**Tabla ", numero, "**")),
               subtitle = md(paste0("*", titulo, "*"))) %>%
    tab_options(
      table.font.names = c("Times New Roman", "serif"),
      table.font.size = px(13),
      table.background.color = "white",
      heading.align = "left",
      heading.border.bottom.style = "solid",
      heading.border.bottom.width = px(2),
      heading.border.bottom.color = "black",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      table.border.left.style = "none",
      table.border.right.style = "none",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      column_labels.vlines.style = "none",
      column_labels.font.weight = "normal",
      table_body.border.top.style = "none",
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = px(2),
      table_body.border.bottom.color = "black",
      table_body.hlines.style = "none",
      table_body.vlines.style = "none",
      row_group.border.top.style = "none",
      row_group.border.bottom.style = "none",
      row_group.background.color = "white",
      source_notes.border.bottom.style = "none",
      source_notes.font.size = px(11),
      data_row.padding = px(4)
    )
  if (!is.null(grupo)) {
    g <- g %>% tab_style(style = cell_text(style = "italic"),
                         locations = cells_row_groups())
  }
  if (length(alinear_der) > 0) {
    g <- g %>% cols_align(align = "right", columns = all_of(alinear_der))
  }
  for (nt in notas) g <- g %>% tab_source_note(md(nt))
  g
}

exportar_tabla <- function(df, g, nombre) {
  gt::gtsave(g, filename = file.path(dir_tab, paste0(nombre, ".html")))
  readr::write_excel_csv(df, file.path(dir_tab, paste0(nombre, ".csv")))  # BOM: tildes OK en Excel
  message("  [OK] ", nombre)
}
fuente <- "*Fuente.* Ministerio de Economía y Finanzas (MDEP), documentos oficiales del PGE 2027. Elaboración propia."

message("Exportando tablas APA 7 en: ", dir_tab)

# ---- Tabla 1 -----------------------------------------------------------------
t1 <- macro_proc %>%
  transmute(Indicador = etiqueta, Unidad = unidad,
            `2026` = fmt_es(valor_2026), `2027` = fmt_es(valor_2027),
            `Variación absoluta` = fmt_sg(var_abs),
            `Variación relativa (%)` = fmt_sg(var_rel))
g1 <- tabla_apa7(
  t1, 1, "Comparativo macroeconómico y fiscal del PGE, 2026 vs. 2027",
  notas = c("*Nota.* Para variables expresadas en %, la variación absoluta se mide en puntos porcentuales; la variación relativa se omite (—). — = dato no disponible.",
            "Los intereses de la deuda 2027 están pendientes de verificación fina contra el PDF oficial.",
            fuente),
  alinear_der = c("2026", "2027", "Variación absoluta", "Variación relativa (%)"))
exportar_tabla(t1, g1, "Tabla01_Comparativo_Macro_2026_2027")

# ---- Tabla 2 -----------------------------------------------------------------
t2 <- bind_rows(
  trib_proc %>% transmute(Impuesto = impuesto, Tipo = tipo_impuesto,
                          `Monto (M USD)` = fmt_es(monto_usd_m),
                          `Participación (%)` = fmt_es(participacion_pct),
                          `Tarifa vigente` = tarifa_vigente,
                          `Hecho generador` = hecho_generador),
  tibble(Impuesto = "Total tributario oficial", Tipo = "—",
         `Monto (M USD)` = fmt_es(trib_total), `Participación (%)` = fmt_es(100),
         `Tarifa vigente` = "—", `Hecho generador` = "—")
)
g2 <- tabla_apa7(
  t2, 2, "Matriz del sistema tributario del PGE, 2027",
  notas = c(paste0("*Nota.* Los seis tributos principales suman ", fmt_es(trib6),
                   " M USD; \"Otros tributos\" ajusta al total oficial de ",
                   fmt_es(trib_total), " M USD. Impuestos indirectos: ",
                   fmt_es(ind_monto / trib_total * 100), "%; directos: ",
                   fmt_es(dir_monto / trib_total * 100), "%; mixtos: ",
                   fmt_es(mix_monto / trib_total * 100), "%."),
            fuente),
  alinear_der = c("Monto (M USD)", "Participación (%)"))
exportar_tabla(t2, g2, "Tabla02_Matriz_Sistema_Tributario_2027")

# ---- Tabla 3 -----------------------------------------------------------------
fila_gasto <- function(grupo, df) {
  df %>% transmute(Grupo = grupo, Rubro = concepto,
                   `Monto (M USD)` = fmt_es(monto_usd_m),
                   `% del PIB` = fmt_es(pct_pib),
                   `% del gasto fiscal` = fmt_es(pct_gasto_fiscal),
                   `Meta (% del PIB)` = "—", `Brecha (p. p.)` = "—",
                   `¿Sumable?` = if_else(sumable, "Sí", "No"))
}
t3 <- bind_rows(
  fila_gasto("Gasto fiscal total y grupo económico",
             gasto_proc %>% filter(categoria %in% c("total_fiscal", "grupo_economico"))),
  fila_gasto("Asignaciones sectoriales por consigna (no sumables)",
             gasto_proc %>% filter(categoria == "sectorial_consigna")),
  fila_gasto("Subsidios y transferencias (no sumables)",
             gasto_proc %>% filter(categoria == "subsidios")),
  metas %>% transmute(Grupo = "Cumplimiento de metas constitucionales", Rubro = sector,
                      `Monto (M USD)` = fmt_es(monto), `% del PIB` = fmt_es(pct_pib),
                      `% del gasto fiscal` = fmt_es(monto / gasto_fiscal * 100),
                      `Meta (% del PIB)` = fmt_es(meta_pct_pib),
                      `Brecha (p. p.)` = fmt_sg(brecha_pp), `¿Sumable?` = "No")
)
g3 <- tabla_apa7(
  t3, 3, "Asignaciones de gasto y cumplimiento de metas constitucionales, 2027",
  notas = c("*Nota.* Las asignaciones sectoriales y los subsidios se solapan entre sí y con el Plan Anual de Inversiones (PAI); por ello no deben sumarse. La amortización de deuda no se incluye en el gasto fiscal sobre la línea.",
            paste0("La meta de Educación (6% del PIB) se evalúa incluyendo universidades y escuelas politécnicas; solo con educación inicial, básica y bachillerato equivale a ",
                   fmt_es(edu_bas / pib27 * 100), "% del PIB. Meta de Salud: 4% del PIB. Brecha positiva = meta superada."),
            "Gasto en personal, intereses y amortización de deuda están pendientes de verificación fina contra el PDF oficial.",
            fuente),
  alinear_der = c("Monto (M USD)", "% del PIB", "% del gasto fiscal", "Meta (% del PIB)", "Brecha (p. p.)"),
  grupo = "Grupo")
exportar_tabla(t3, g3, "Tabla03_Asignaciones_Gasto_y_Metas_2027")

# ---- Tabla 4 -----------------------------------------------------------------
nm_grupo <- c(Obligatorio = "Indicadores obligatorios",
              Complementario = "Indicadores complementarios",
              Sensibilidad = "Variantes de sensibilidad")
t4 <- indicadores %>%
  transmute(Grupo = unname(nm_grupo[grupo]),
            `N.º` = as.character(n),
            Indicador = indicador,
            Fórmula = formula,
            Numerador = fmt_es(numerador),
            Denominador = fmt_es(denominador),
            Resultado = paste0(fmt_es(valor), if_else(unidad == "%", "%", paste0(" ", unidad))))
g4 <- tabla_apa7(
  t4, 4, "Indicadores fiscales obligatorios y complementarios del PGE, 2027",
  notas = c("*Nota.* Numerador y denominador en millones de USD, salvo en el indicador 1 (PGE 2027 y PGE 2026) y en la razón indirectos/directos. Ingresos tributarios = total oficial (20.703,00 M USD); ingresos sin financiamiento = 34.426,58 M USD (sobre la línea).",
            paste0("Variantes: con los seis tributos (", fmt_es(trib6), " M USD) la presión tributaria es ",
                   fmt_es(trib6 / pib27 * 100), "%; con ingresos corrientes más capital (",
                   fmt_es(ing_sens), " M USD) la carga de intereses es ",
                   fmt_es(int27 / ing_sens * 100), "%."),
            fuente),
  alinear_der = c("N.º", "Numerador", "Denominador", "Resultado"),
  grupo = "Grupo")
exportar_tabla(t4, g4, "Tabla04_Indicadores_Fiscales_Obligatorios_2027")

# ---- Tabla 5 -----------------------------------------------------------------
fila_sim <- function(grupo, concepto, v, d = 2, signo = FALSE) {
  f <- if (signo) fmt_sg else fmt_es
  tibble(Grupo = grupo, Concepto = concepto,
         `Esc. 1: Presupuestado` = f(v[1], d),
         `Esc. 2: Choque simultáneo` = f(v[2], d),
         `Esc. 3: Con paquete de ajuste` = f(v[3], d))
}
e <- escenarios
t5 <- bind_rows(
  fila_sim("Supuestos macroeconómicos", "Precio del crudo (USD/barril)", e$precio_petroleo),
  fila_sim("Supuestos macroeconómicos", "Crecimiento real del PIB (%)", e$crecimiento_real),
  fila_sim("Supuestos macroeconómicos", "Cumplimiento de la meta tributaria (%)", e$cumplimiento_trib, 0),
  fila_sim("Supuestos macroeconómicos", "PIB nominal (M USD)", e$pib_nominal),
  fila_sim("Efectos sobre el balance (M USD)", "Efecto tributario directo", e$delta_tributario, signo = TRUE),
  fila_sim("Efectos sobre el balance (M USD)", "Efecto petrolero neto", e$delta_petrolero, signo = TRUE),
  fila_sim("Efectos sobre el balance (M USD)", "Medidas de ingreso del paquete", e$medidas_ingreso, signo = TRUE),
  fila_sim("Efectos sobre el balance (M USD)", "Medidas de gasto del paquete", e$medidas_gasto, signo = TRUE),
  fila_sim("Resultados", "Balance global (M USD)", e$balance_global),
  fila_sim("Resultados", "Déficit global / PIB (%)", e$deficit_pct_pib),
  fila_sim("Resultados", "Brecha fiscal generada por el choque (M USD)", e$brecha_fiscal),
  fila_sim("Resultados", "Brecha fiscal cubierta (M USD)", e$brecha_cubierta),
  fila_sim("Resultados", "Cobertura de la brecha (%)", e$cobertura_pct),
  purrr::pmap_dfr(supuestos_paquete, function(id, tipo, medida, base_descripcion,
                                              base_monto, supuesto_pct, efecto_usd_m) {
    fila_sim("Detalle de medidas del paquete (M USD)",
             paste0(id, ". ", medida, " (", fmt_es(supuesto_pct, 1), "% de la base)"),
             c(NA, NA, efecto_usd_m))
  })
)
g5 <- tabla_apa7(
  t5, 5, "Simulación de choque simultáneo y paquete de ajuste fiscal, 2027",
  notas = c("*Nota.* Balance global negativo = déficit. Choque: -10% de ingresos tributarios, precio del crudo de 50 USD y crecimiento real de 1,0% con deflactor constante; el efecto petrolero neto usa la sensibilidad configurada (65 M USD por USD 1), pendiente de verificación.",
            "El PIB de estrés escala el PIB oficial 2027 por la relación de factores de crecimiento real. El paquete mantiene 0% de recorte en Educación y Salud; los porcentajes son supuestos editables en el script 02. — = no aplica.",
            fuente),
  alinear_der = c("Esc. 1: Presupuestado", "Esc. 2: Choque simultáneo", "Esc. 3: Con paquete de ajuste"),
  grupo = "Grupo")
exportar_tabla(t5, g5, "Tabla05_Simulacion_Choque_y_Ajuste_2027")

# ------------------------------------------------------------------------------
# 6. Resumen ejecutivo en texto plano
# ------------------------------------------------------------------------------
lin_ind <- paste0(sprintf("%2d", indicadores$n), ". [", substr(indicadores$grupo, 1, 4), "] ",
                  indicadores$indicador, ": ", fmt_es(indicadores$valor), " ", indicadores$unidad,
                  "   <", indicadores$formula, ">")

lin_esc <- purrr::map(seq_len(nrow(escenarios)), function(i) {
  x <- escenarios[i, ]
  c(x$escenario,
    paste0("   PIB nominal: ", fmt_es(x$pib_nominal), " M USD"),
    paste0("   Efecto tributario: ", fmt_sg(x$delta_tributario), " | Efecto petrolero: ", fmt_sg(x$delta_petrolero),
           " | Medidas ingreso: ", fmt_sg(x$medidas_ingreso), " | Medidas gasto: ", fmt_sg(x$medidas_gasto)),
    paste0("   Balance global: ", fmt_es(x$balance_global), " M USD  (déficit/PIB = ",
           fmt_es(x$deficit_pct_pib), "%)"),
    paste0("   Brecha generada: ", fmt_es(x$brecha_fiscal), " M | cubierta: ", fmt_es(x$brecha_cubierta),
           " M | cobertura: ", fmt_es(x$cobertura_pct), "%"),
    "")
}) %>% unlist()

lin_med <- paste0("  ", supuestos_paquete$id, " | ", supuestos_paquete$tipo, " | ", supuestos_paquete$medida,
                  ": base ", fmt_es(supuestos_paquete$base_monto), " x ",
                  fmt_es(supuestos_paquete$supuesto_pct, 1), "% = ",
                  fmt_es(supuestos_paquete$efecto_usd_m), " M USD")

lineas <- c(
  "RESUMEN DE RESULTADOS - COMPONENTE PRÁCTICO 2 DE POLÍTICA FISCAL",
  "Presupuesto General del Estado (PGE) del Ecuador 2027",
  paste0("Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  strrep("=", 90), "",
  "1. INDICADORES FISCALES (millones de USD salvo indicación)", strrep("-", 90),
  lin_ind, "",
  "2. BALANCES, REGLA FISCAL Y METAS", strrep("-", 90),
  paste0("PGE 2026: ", fmt_es(pge26), " | PGE 2027: ", fmt_es(pge27), " M USD"),
  paste0("PIB nominal 2026: ", fmt_es(pib26), " | PIB nominal 2027: ", fmt_es(pib27), " M USD"),
  paste0("Déficit global 2027: ", fmt_es(-def27), " M USD (", fmt_es(def27 / pib27 * 100), "% del PIB)"),
  paste0("Superávit primario: ", fmt_es(sup_prim27), " - Intereses: ", fmt_es(int27),
         " = ", fmt_es(sup_prim27 - int27), " M USD"),
  paste0("Ingresos sobre la línea: ", fmt_es(ing_sobre_linea), " | Egresos sobre la línea: ", fmt_es(egr_sobre_linea)),
  paste0("Regla fiscal (Art. 286 CRE) - cuenta permanente: ", fmt_es(ing_perm), " - ", fmt_es(gas_perm),
         " = ", fmt_es(ing_perm - gas_perm), " M USD"),
  paste0("Regla fiscal - cuenta no permanente: ", fmt_es(ing_np), " - ", fmt_es(gas_np),
         " = ", fmt_es(ing_np - gas_np), " M USD"),
  paste0("Impuestos: indirectos ", fmt_es(ind_monto), " M (", fmt_es(ind_monto / trib_total * 100),
         "%), directos ", fmt_es(dir_monto), " M (", fmt_es(dir_monto / trib_total * 100),
         "%), mixtos ", fmt_es(mix_monto), " M (", fmt_es(mix_monto / trib_total * 100), "%)"),
  paste0("Educación (incl. universidades): ", fmt_es(edu_total), " M = ", fmt_es(edu_total / pib27 * 100),
         "% del PIB (meta 6%; brecha ", fmt_sg(metas$brecha_pp[1]), " p. p.)"),
  paste0("Salud: ", fmt_es(salud), " M = ", fmt_es(salud / pib27 * 100),
         "% del PIB (meta 4%; brecha ", fmt_sg(metas$brecha_pp[2]), " p. p.)"),
  "", "3. SIMULACIÓN FISCAL", strrep("-", 90),
  lin_esc,
  "Medidas del Escenario 3:", lin_med,
  paste0("  Educación: recorte ", fmt_es(recorte_edu_pct, 1), "% | Salud: recorte ",
         fmt_es(recorte_salud_pct, 1), "%"),
  "",
  "4. PARÁMETROS PENDIENTES DE VERIFICACIÓN CONTRA PDF OFICIAL", strrep("-", 90),
  "  - Intereses de la deuda 2027 (5.097,85 M USD) y 2026 (4.469,00 M USD)",
  "  - Gasto en personal (10.057,00 M USD) y amortización estimada (11.374,66 M USD)",
  "  - Sensibilidad neta del crudo (65 M USD por USD 1)"
)
con <- file(file.path(dir_rep, "00_resumen_resultados.txt"), open = "w", encoding = "UTF-8")
writeLines(lineas, con)
close(con)
message("Resumen guardado en: ", file.path(dir_rep, "00_resumen_resultados.txt"))
message("Script 2 finalizado.")
