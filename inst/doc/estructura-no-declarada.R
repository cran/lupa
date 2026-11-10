## ----setup, include=FALSE---------------------------------------------------------------
library(lupa)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
# El ancho se restaura al terminar la viñeta: knitr corre cada una en su propia
# sesion, asi que no se filtra, pero el CRAN Cookbook pide devolver toda opcion
# que se cambie y no cuesta nada cumplirlo.
.opciones_previas <- options(width = 90)

## ----tablas-construidas-----------------------------------------------------------------
library(lupa)

personas <- data.frame(
  id_persona = 101:108,
  nombre = rep(c("Ana", "Bruno", "Carla", "Diego"), each = 2),
  ciudad = rep(c("Montevideo", "Salto", "Belén", "Melo"), each = 2),
  departamento = rep(
    c("Montevideo", "Salto", "Salto", "Cerro Largo"), each = 2
  ),
  stringsAsFactors = FALSE
)

tramites <- data.frame(
  id_tramite = 1001:1010,
  id_persona = c(101, 101, 102, 103, 104, 104, 105, 106, 107, 108),
  tipo = rep(c("alta", "renovación"), 5),
  stringsAsFactors = FALSE
)

personas
tramites

## ----claves-----------------------------------------------------------------------------
claves <- detectar_claves(personas, max_combinacion = 1)
claves[, c(
  "columnas", "n_columnas", "n_filas", "casi_clave", "unicidad_exacta"
)]

## ----relaciones-------------------------------------------------------------------------
detectar_relaciones(
  personas["id_persona"], tramites["id_persona"], muestra = Inf
)

## ----dependencias-----------------------------------------------------------------------
dependencias <- detectar_dependencias(
  personas[c("ciudad", "departamento")],
  muestra = Inf,
  min_observaciones = 4
)
dependencias

## ----cierre, include=FALSE----------------------------------------------------
options(.opciones_previas)

