# 01. Descarga y carga de datos. R

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  readr,
  dplyr,
  purrr,
  stringr,
  foreign)

anios<-2014:2024

#rutas
dir_raw<-"data/raw"
dir_clean<-"data/clean"

if (!dir.exists(dir_raw)) dir.create(dir_raw, recursive = TRUE)
if (!dir.exists(dir_clean)) dir.create(dir_clean, recursive = TRUE)

ruta_archivo_local <- function(anio) {
  file.path(dir_raw, paste0("defunciones_", anio, ".csv"))
}
message("Estructura inicial lista.")
