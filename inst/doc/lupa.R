## ----setup, include = FALSE-------------------------------------------------------------
library(lupa)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
# El ancho se restaura al terminar la viñeta, como pide el CRAN Cookbook.
.opciones_previas <- options(width = 90)

# La impresión de un perfil informa la memoria que ocupa la tabla. Esa cifra
# cambia con la plataforma y la versión de R, y una viñeta no publica tamaños de
# memoria: en los bloques marcados con `sin_memoria = TRUE` se quita esa línea.
.quitar_memoria <- function(gancho) {
  force(gancho)
  function(x, options) {
    if (isTRUE(options$sin_memoria)) {
      lineas <- strsplit(x, "\n", fixed = TRUE)[[1L]]
      lineas <- lineas[!grepl("Memoria: ", lineas, fixed = TRUE)]
      # Cada línea del resumen general llega como un mensaje propio: si la
      # única línea era la de memoria, no queda nada que mostrar.
      if (!length(lineas)) return("")
      x <- paste0(paste(lineas, collapse = "\n"), "\n")
    }
    gancho(x, options)
  }
}
knitr::knit_hooks$set(
  message = .quitar_memoria(knitr::knit_hooks$get("message")),
  output = .quitar_memoria(knitr::knit_hooks$get("output"))
)

## ----datos------------------------------------------------------------------------------
library(lupa)
datos_operativos

## ----perfilar---------------------------------------------------------------------------
perfil <- perfilar(datos_operativos, clave = "id_evento")

## ----imprimir, sin_memoria = TRUE-------------------------------------------------------
perfil

## ----errores----------------------------------------------------------------------------
h <- hallazgos(perfil)
h[h$severidad == "error", c("columna", "tipo_hallazgo", "n_afectados")]

## ----cifras-errores, include = FALSE----------------------------------------------------
errores <- h[h$severidad == "error", ]
n_errores <- nrow(errores)
de_tabla <- errores$tipo_hallazgo[is.na(errores$columna)]

## ----comprobar-errores, echo = FALSE----------------------------------------------------
# La tabla de defectos sembrados promete estos hallazgos.
prometidos <- list(
  c("id_registro", "posible_identificador"), c("codigo_usuario", "faltantes_disfrazados"),
  c("fecha_evento", "formatos_fecha_mixtos"), c("fecha_evento", "faltantes_disfrazados"),
  c("canal", "mayusculas_inconsistentes"), c("canal", "espacios_sobrantes"),
  c("canal", "faltantes_disfrazados"), c("monto", "faltantes_disfrazados"),
  c("sistema", "constante"), c("contacto", "dato_personal_posible")
)
for (par in prometidos) {
  stopifnot(any(h$columna %in% par[1] & h$tipo_hallazgo == par[2]))
}
stopifnot(
  "columnas_duplicadas" %in% h$tipo_hallazgo,
  !any(h$columna %in% "zona"),
  setequal(de_tabla, c("clave_no_unica", "filas_duplicadas")),
  all(c("faltantes_disfrazados", "formatos_fecha_mixtos") %in% errores$tipo_hallazgo)
)

## ----un-hallazgo------------------------------------------------------------------------
canal <- h[h$columna %in% "canal" & h$tipo_hallazgo == "faltantes_disfrazados", ]
writeLines(canal$evidencia)
writeLines(strwrap(canal$sugerencia, 88))

## ----traza------------------------------------------------------------------------------
eventos <- canal$trazabilidad[[1]]$claves$id_evento
eventos

## ----cotejo-----------------------------------------------------------------------------
filas <- datos_operativos$id_evento %in% eventos
print(datos_operativos[filas, c("id_evento", "canal")], quote = TRUE)

## ----comprobar-traza, echo = FALSE------------------------------------------------------
stopifnot(identical(eventos, c("EVT005", "EVT011")))

## ----cobertura--------------------------------------------------------------------------
no_evaluados <- cobertura(perfil)
no_evaluados[, c("diagnostico", "columna")]

## ----motivo-----------------------------------------------------------------------------
motivo <- no_evaluados$motivo[
  no_evaluados$diagnostico == "outliers" & no_evaluados$columna == "monto"
]
writeLines(strwrap(motivo, 88))

## ----comprobar-cobertura, echo = FALSE--------------------------------------------------
stopifnot(
  length(motivo) == 1L,
  !any(h$columna %in% "monto" & h$tipo_hallazgo == "outliers"),
  !any(h$columna %in% c("codigo_usuario", "contacto") & h$tipo_hallazgo == "patron_raro"),
  !any(h$tipo_hallazgo %in% c("ceros_no_permitidos", "negativos_no_permitidos")),
  1 / nrow(datos_operativos) > formals(perfilar)$umbral_patron_raro
)

## ----informe----------------------------------------------------------------------------
archivo <- file.path(tempdir(), "calidad-de-la-entrega.html")
reportar(perfil, archivo = archivo, titulo = "Calidad de la entrega")

## ----secciones, include = FALSE---------------------------------------------------------
html <- paste(readLines(archivo, encoding = "UTF-8", warn = FALSE), collapse = "\n")
secciones <- regmatches(html, gregexpr("<h3[^>]*>.*?</h3>", html, perl = TRUE))[[1]]
secciones <- gsub("<[^>]+>", "", secciones)
stopifnot(length(secciones) > 0L)

## ----plan-------------------------------------------------------------------------------
plan <- planificar_limpieza(perfil, datos_operativos)
plan[plan$columna %in% "canal", c("id_accion", "estrategia", "recomendada", "aplicar")]

## ----justificacion----------------------------------------------------------------------
justificacion <- plan$justificacion[
  plan$columna %in% "canal" & plan$estrategia == "convertir_ausencias_textuales"
]
writeLines(strwrap(justificacion, 88))

## ----aplicar----------------------------------------------------------------------------
plan$aplicar <- plan$columna %in% "canal" & plan$recomendada
limpio <- aplicar(plan, datos_operativos)
limpio$registro[, c("columna", "estrategia", "n_cambiadas", "estado")]

## ----antes-despues----------------------------------------------------------------------
antes <- datos_operativos$canal
despues <- limpio$datos$canal
cambiaron <- is.na(despues) | antes != despues
print(data.frame(id_evento = datos_operativos$id_evento, antes, despues)[cambiaron, ],
      quote = TRUE)

## ----comprobar-limpieza, echo = FALSE---------------------------------------------------
stopifnot(
  identical(sum(cambiaron), 3L),
  identical(antes[cambiaron], c("S/D", "web ", "")),
  identical(despues[!is.na(despues) & cambiaron], "web")
)

## ----cierre, include = FALSE--------------------------------------------------
unlink(archivo)
options(.opciones_previas)

