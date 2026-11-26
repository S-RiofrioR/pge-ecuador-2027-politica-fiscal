# ==============================================================================
# 99_ejecutar_todo.R   (ubicar en 02_SCRIPTS/)
# Script maestro: ejecuta en secuencia los scripts 01, 02 y 03 para regenerar
# datos, indicadores, simulación, tablas APA 7 y gráficos tras editar un dato.
# ==============================================================================

ruta_base   <- "C:/Users/Oficina/Desktop/COMPONENTE PRACTICO 2 - POLITICA FISCAL"
dir_scripts <- file.path(ruta_base, "02_SCRIPTS")

if (!dir.exists(file.path(ruta_base, "01_DATOS", "raw"))) {
  stop("No existe la estructura del proyecto. Ejecute primero 00_setup_proyecto.R")
}

scripts <- c("01_construccion_datos.R",
             "02_indicadores_y_simulacion.R",
             "03_graficos_apa7.R")

t_inicio <- Sys.time()
for (s in scripts) {
  ruta <- file.path(dir_scripts, s)
  if (!file.exists(ruta)) stop("No se encuentra el script: ", ruta)
  cat("\n", strrep("=", 70), "\n>>> Ejecutando: ", s, "\n", strrep("=", 70), "\n", sep = "")
  # Entorno aislado para evitar colisiones de nombres entre scripts
  source(ruta, encoding = "UTF-8", local = new.env(parent = globalenv()))
}

cat("\n", strrep("=", 70), "\nProceso completo en ",
    round(as.numeric(difftime(Sys.time(), t_inicio, units = "secs")), 1), " segundos.\n",
    "Tablas:   ", file.path(ruta_base, "03_EXPORT", "tablas_apa7"), "\n",
    "Gráficos: ", file.path(ruta_base, "03_EXPORT"), "\n",
    "Resumen:  ", file.path(ruta_base, "03_EXPORT", "reportes", "00_resumen_resultados.txt"), "\n",
    sep = "")
