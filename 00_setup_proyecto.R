# ==============================================================================
# 00_setup_proyecto.R
# Componente Práctico 2 de Política Fiscal - PGE Ecuador 2027
# Función: crear la estructura de carpetas, instalar paquetes y documentar el
#          proyecto. Ejecutar UNA sola vez (es idempotente: puede repetirse).
# Codificación: UTF-8  (RStudio > File > Reopen with Encoding > UTF-8)
# ==============================================================================

ruta_base <- "C:/Users/Oficina/Desktop/COMPONENTE PRACTICO 2 - POLITICA FISCAL"

# ---- 1. Paquetes -------------------------------------------------------------
paquetes <- c("tidyverse", "scales", "gt", "patchwork", "glue", "fs")
faltan   <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan) > 0) {
  message("Instalando paquetes faltantes: ", paste(faltan, collapse = ", "))
  install.packages(faltan, repos = "https://cloud.r-project.org")
}
invisible(lapply(paquetes, function(p) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

# ---- 2. Estructura de subcarpetas -------------------------------------------
subcarpetas <- c(
  "00_FUENTES_OFICIALES",
  "01_DATOS/raw",
  "01_DATOS/processed",
  "02_SCRIPTS",
  "03_EXPORT/tablas_apa7",
  "03_EXPORT/graficos_informe",
  "03_EXPORT/graficos_exposicion",
  "03_EXPORT/reportes",
  "04_INFORME_Y_EXPOSICION"
)

dir.create(ruta_base, recursive = TRUE, showWarnings = FALSE)
for (sc in subcarpetas) {
  dir.create(file.path(ruta_base, sc), recursive = TRUE, showWarnings = FALSE)
}

# ---- 3. Archivo LEEME_ESTRUCTURA.txt ----------------------------------------
leeme <- c(
  "PROYECTO: Componente Práctico 2 de Política Fiscal",
  "TEMA: Presupuesto General del Estado (PGE) del Ecuador 2027",
  paste0("Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
  strrep("=", 78),
  "",
  "ESTRUCTURA DE CARPETAS",
  strrep("-", 78),
  "00_FUENTES_OFICIALES/       PDF oficiales (MDEP, Registro Oficial, anexos) usados para",
  "                            verificar los parámetros marcados # VERIFICAR_PDF.",
  "01_DATOS/raw/               Los 5 archivos .csv originales construidos por el Script 1.",
  "01_DATOS/processed/         Tablas procesadas, indicadores y simulación (.csv y .rds).",
  "02_SCRIPTS/                 Todos los scripts .R (01, 02, 03 y 99_ejecutar_todo.R).",
  "03_EXPORT/tablas_apa7/      Tablas APA 7 en .html (gt) y .csv.",
  "03_EXPORT/graficos_informe/ Figuras PNG 300 DPI sin título interno (APA 7 estricto).",
  "03_EXPORT/graficos_exposicion/ Figuras PNG 300 DPI con título y subtítulo (diapositivas).",
  "03_EXPORT/reportes/         Resumen de resultados en texto plano para redacción.",
  "04_INFORME_Y_EXPOSICION/    Informe final (Word/PDF) y presentación oral.",
  "",
  "FLUJO DE TRABAJO",
  strrep("-", 78),
  "1. Ejecutar 00_setup_proyecto.R (una vez).",
  "2. Copiar los scripts 01, 02, 03 y 99 dentro de 02_SCRIPTS/.",
  "3. Editar datos en 01_construccion_datos.R (o supuestos del paquete fiscal en",
  "   02_indicadores_y_simulacion.R, bloque 'SUPUESTOS EDITABLES').",
  "4. Ejecutar 99_ejecutar_todo.R: regenera CSV, tablas, resumen y gráficos.",
  "",
  "NORMAS: rutas con '/' y file.path(); archivos .R en UTF-8; source(..., encoding = 'UTF-8')."
)
con <- file(file.path(ruta_base, "LEEME_ESTRUCTURA.txt"), open = "w", encoding = "UTF-8")
writeLines(leeme, con)
close(con)

# ---- 4. Árbol de directorios -------------------------------------------------
cat("\nEstructura del proyecto creada en:\n", ruta_base, "\n\n")
fs::dir_tree(ruta_base)
message("\nSetup completado. Copie ahora los scripts 01, 02, 03 y 99 en 02_SCRIPTS/.")
