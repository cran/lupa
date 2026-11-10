## ----setup, include=FALSE---------------------------------------------------------------
library(lupa)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
# El ancho se restaura al terminar la viñeta: knitr corre cada una en su propia
# sesion, asi que no se filtra, pero el CRAN Cookbook pide devolver toda opcion
# que se cambie y no cuesta nada cumplirlo.
.opciones_previas <- options(width = 90)

## ---------------------------------------------------------------------------------------
library(lupa)

analisis <- analizar(
  datos_operativos,
  nombre = "entrega de ejemplo",
  fecha = as.POSIXct("2026-01-15", tz = "UTC")
)
names(analisis)

## ---------------------------------------------------------------------------------------
h <- hallazgos(analisis)
h[h$columna %in% "fecha_evento", c("tipo_hallazgo", "severidad", "n_afectados")]

## ---------------------------------------------------------------------------------------
descubrir_patrones(datos_operativos$codigo_usuario)

## ----echo = FALSE-----------------------------------------------------------------------
stopifnot(isTRUE(all.equal(
  as.data.frame(descubrir_patrones(datos_operativos$codigo_usuario)),
  as.data.frame(analisis$perfil$patrones$codigo_usuario),
  check.attributes = FALSE
)))

## ---------------------------------------------------------------------------------------
inferir_tipo(datos_operativos$fecha_evento)
formatos <- detectar_formatos_fecha(datos_operativos$fecha_evento)
formatos[, c("formato", "n", "proporcion", "estado", "n_ambiguos")]

## ---------------------------------------------------------------------------------------
ambigua <- perfilar(
  data.frame(fecha = c("01/02/2020", "02/03/2020", "03/04/2020")),
  muestra = Inf,
  analizar_dependencias = FALSE
)
campos <- c(
  "columna", "tipo_inferido", "proporcion_tipo_inferido", "estado_tipo_inferido"
)
rbind(
  columnas(analisis)[columnas(analisis)$columna == "fecha_evento", campos],
  columnas(ambigua)[, campos]
)

## ---------------------------------------------------------------------------------------
analisis$variables[, c(
  "columna", "tipo_implicito", "escala_propuesta", "rol", "confianza", "confirmada"
)]

## ---------------------------------------------------------------------------------------
frecuencias <- analisis$distribuciones$frecuencias
frecuencias[frecuencias$columna == "canal", c("valor", "frecuencia", "proporcion")]

cuantiles <- analisis$distribuciones$cuantiles
cuantiles[cuantiles$columna == "monto", c("probabilidad", "valor", "n_analizados")]

## ---------------------------------------------------------------------------------------
analisis$asociaciones[, c("columna_1", "columna_2", "metodo", "asociacion")]

## ----echo = FALSE-----------------------------------------------------------------------
sin_fila_13 <- detectar_asociaciones(datos_operativos[-13, ])
eta_sin_13 <- sin_fila_13$asociacion[
  sin_fila_13$columna_1 == "canal" & sin_fila_13$columna_2 == "monto"
]
stopifnot(length(eta_sin_13) == 1L, eta_sin_13 < 0.9)

## ---------------------------------------------------------------------------------------
analisis$temporal$resumen[, c(
  "columna", "n_presentes", "fecha_minima", "fecha_maxima", "n_fechas_excluidas_parseo"
)]
analisis$temporal$propuestas[, c("columna", "frecuencia_dias", "confirmada")]

## ---------------------------------------------------------------------------------------
analisis$propuesta_modelo[, c("metrica", "atributos", "origen", "incluir")]

## ---------------------------------------------------------------------------------------
analisis$decision_medicion[, c("metrica", "objeto", "medida")]
analisis$tablero[, c("metrica_instanciada", "valor", "orientacion")]

## ----echo = FALSE-----------------------------------------------------------------------
# Lo que dice la prosa: se mide exactamente lo activo, y nada aprendido de la entrega.
stopifnot(
  identical(analisis$decision_medicion$medida, analisis$propuesta_modelo$incluir),
  !any(analisis$propuesta_modelo$incluir[
    grepl("^perfil:", analisis$propuesta_modelo$origen)
  ])
)

## ---------------------------------------------------------------------------------------
zona <- distribucion_valores(datos_operativos["zona"], max_valores = 3)
zona$frecuencias[, c("valor", "frecuencia")]
zona$alcance[, c("columna", "n_distintos_muestra", "n_mostrados", "truncado")]

## ---------------------------------------------------------------------------------------
archivo_rds <- tempfile(fileext = ".rds")
guardar_analisis(analisis, archivo_rds)
recuperado <- leer_analisis(archivo_rds)
str(recuperado$meta$persistencia)
identical(hallazgos(recuperado), hallazgos(analisis))

## ---------------------------------------------------------------------------------------
archivo_html <- reportar(
  analisis,
  archivo = tempfile(fileext = ".html"),
  titulo = "Calidad de la entrega de ejemplo",
  fecha = as.POSIXct("2026-01-15", tz = "UTC")
)
html <- readLines(archivo_html, encoding = "UTF-8", warn = FALSE)
secciones <- regmatches(html, gregexpr("<h2>[^<]*</h2>", html))[[1]]
gsub("</?h2>", "", secciones)

## ---------------------------------------------------------------------------------------
unlink(c(archivo_html, archivo_rds))

## ----cierre, include=FALSE----------------------------------------------------
options(.opciones_previas)

