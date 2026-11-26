# ==============================================================================
# 01_construccion_datos.R
# Construye y exporta los 5 archivos .csv originales a 01_DATOS/raw/
# (UTF-8, punto decimal, sin símbolos de moneda).
# Fuente: Proforma/PGE 2027 (MDEP). Las cifras marcadas con # VERIFICAR_PDF
# deben contrastarse con los PDF oficiales de 00_FUENTES_OFICIALES/.
# ==============================================================================

suppressPackageStartupMessages(library(tidyverse))

ruta_base <- "C:/Users/Oficina/Desktop/COMPONENTE PRACTICO 2 - POLITICA FISCAL"
dir_raw   <- file.path(ruta_base, "01_DATOS", "raw")
dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)

guardar_raw <- function(df, archivo) {
  readr::write_csv(df, file.path(dir_raw, archivo), na = "NA")
  message("  [OK] ", archivo, "  (", nrow(df), " filas)")
}

message("Construyendo datos crudos en: ", dir_raw)

# ------------------------------------------------------------------------------
# 1. Comparativo macroeconómico 2026 vs. 2027
# ------------------------------------------------------------------------------
macro <- tribble(
  ~indicador,             ~unidad,                ~valor_2026, ~valor_2027,
  "pge_total",            "Millones USD",         46255.57,    50213.09,
  "pib_nominal",          "Millones USD",         139046.60,   142724.58,
  "crecimiento_real_pib", "%",                    1.80,        2.50,
  "deflactor_pib",        "%",                    2.10,        2.30,
  "inflacion_promedio",   "%",                    3.20,        2.20,
  "produccion_petrolera", "Millones de barriles", 165.50,      165.60,
  "precio_barril_crudo",  "USD/barril",           53.50,       66.00,
  "deficit_global",       "Millones USD",         5413.80,     4411.85,
  "superavit_primario",   "Millones USD",         NA_real_,    686.00,
  "intereses_deuda",      "Millones USD",         4469.00,     5097.85   # VERIFICAR_PDF
)
guardar_raw(macro, "01_macro_comparativo.csv")

# ------------------------------------------------------------------------------
# 2. Ingresos y balance 2027 (naturaleza, regla fiscal, subrubros, referencias)
# ------------------------------------------------------------------------------
# Nota: ingresos sobre la línea = 34426.58; egresos sobre la línea = 38838.43.
# La diferencia de ~30.00 M respecto a corrientes + capital corresponde a
# recuperación de inversiones bajo la línea.
ingresos_balance <- tribble(
  ~bloque,              ~concepto,                                  ~monto_usd_m, ~participacion_pct,
  "naturaleza",         "Ingresos corrientes",                      24863.05,     49.52,
  "naturaleza",         "Ingresos de capital",                      9593.52,      19.11,
  "naturaleza",         "Financiamiento",                           15756.52,     31.38,
  "regla_fiscal",       "Ingresos permanentes",                     24863.05,     NA_real_,
  "regla_fiscal",       "Gastos permanentes",                       25896.23,     NA_real_,
  "regla_fiscal",       "Ingresos no permanentes",                  9563.52,      NA_real_,
  "regla_fiscal",       "Gastos no permanentes",                    12942.20,     NA_real_,
  "subrubro_corriente", "Impuestos totales",                        20703.00,     NA_real_,
  "subrubro_corriente", "Tasas y contribuciones",                   2341.92,      NA_real_,
  "subrubro_corriente", "Rentas de inversiones y multas",           922.83,       NA_real_,
  "subrubro_corriente", "Transferencias corrientes",                815.56,       NA_real_,
  "subrubro_corriente", "Venta de bienes/servicios y otros",        80.13,        NA_real_,
  "referencia",         "Ingresos sobre la línea",                  34426.58,     NA_real_,
  "referencia",         "Egresos sobre la línea",                   38838.43,     NA_real_
)
guardar_raw(ingresos_balance, "02_ingresos_balance_2027.csv")

# ------------------------------------------------------------------------------
# 3. Sistema tributario 2027
# ------------------------------------------------------------------------------
tributario <- tribble(
  ~impuesto,             ~tipo_impuesto, ~monto_usd_m, ~tarifa_vigente,                                ~hecho_generador,
  "IVA",                 "Indirecto",    10121.00,     "15% general / 0% canasta básica",              "Transferencia de bienes y prestación de servicios gravados",
  "Impuesto a la Renta", "Directo",      6786.00,      "25% sociedades; 0%-37% personas naturales",    "Obtención de rentas gravadas durante el ejercicio fiscal",
  "Aranceles",           "Indirecto",    1641.00,      "Ad valorem y mixto según partida",             "Importación de mercancías",
  "ICE",                 "Indirecto",    895.00,       "Específica y ad valorem",                      "Producción/importación y consumo de bienes y servicios gravados",
  "ISD",                 "Indirecto",    691.00,       "5% general; tarifas diferenciadas productivas","Transferencia o envío de divisas al exterior",
  "Impuesto a vehículos","Directo",      274.00,       "Tabla progresiva según avalúo",                "Propiedad de vehículos motorizados",
  "Otros tributos",      "Mixto",        295.00,       "Varias - ajuste al total tributario oficial",  "Varios (ajuste al total tributario oficial)"
)
guardar_raw(tributario, "03_sistema_tributario_2027.csv")

# ------------------------------------------------------------------------------
# 4. Gasto y asignaciones 2027 (sumable = FALSE evita doble conteo con el PAI)
# ------------------------------------------------------------------------------
gasto <- tribble(
  ~categoria,           ~concepto,                                          ~monto_usd_m, ~sumable,
  "total_fiscal",       "Gasto fiscal total sobre la línea",                38838.43,     FALSE,
  "grupo_economico",    "Gasto en personal",                                10057.00,     TRUE,   # VERIFICAR_PDF
  "grupo_economico",    "Intereses de la deuda pública",                    5097.85,      TRUE,   # VERIFICAR_PDF
  "grupo_economico",    "Amortización estimada de deuda",                   11374.66,     FALSE,  # VERIFICAR_PDF
  "sectorial_consigna", "Educación inicial, básica y bachillerato",         6681.29,      FALSE,
  "sectorial_consigna", "Salud",                                            5855.94,      FALSE,
  "sectorial_consigna", "Gobiernos autónomos descentralizados",             4825.15,      FALSE,
  "sectorial_consigna", "Plan Anual de Inversiones - PAI",                  3000.00,      FALSE,
  "sectorial_consigna", "Universidades y escuelas politécnicas",            1443.35,      FALSE,
  "subsidios",          "Subsidio a combustibles",                          2593.59,      FALSE,
  "subsidios",          "Subsidio sector eléctrico",                        1154.20,      FALSE,
  "subsidios",          "Transferencias Seguridad Social IESS/ISSFA/ISSPOL",4446.82,      FALSE,
  "subsidios",          "Bonos y transferencias MTDH",                      1441.14,      FALSE
)
guardar_raw(gasto, "04_gasto_y_asignaciones_2027.csv")

# ------------------------------------------------------------------------------
# 5. Parámetros de simulación 2027 (presupuestado vs. estrés)
# ------------------------------------------------------------------------------
parametros <- tribble(
  ~parametro,                       ~unidad,                  ~presupuestado, ~estres,
  "precio_petroleo",                "USD/barril",             66,             50,
  "crecimiento_real_pib",           "%",                      2.5,            1.0,
  "deflactor_pib",                  "%",                      2.3,            2.3,
  "cumplimiento_meta_tributaria",   "%",                      100,            90,
  # Millones USD por cada USD 1 de caída neta del crudo (tras ahorro parcial en
  # subsidios a combustibles importados). Parámetro configurable.
  "sensibilidad_neta_crudo_1usd",   "Millones USD por USD 1", 65,             65      # VERIFICAR_PDF
)
guardar_raw(parametros, "05_parametros_simulacion_2027.csv")

message("Script 1 finalizado: 5 archivos CSV en 01_DATOS/raw/")
