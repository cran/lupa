## ----setup, include=FALSE---------------------------------------------------------------
library(lupa)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
# El ancho se restaura al terminar la viñeta: knitr corre cada una en su propia
# sesion, asi que no se filtra, pero el CRAN Cookbook pide devolver toda opcion
# que se cambie y no cuesta nada cumplirlo.
.opciones_previas <- options(width = 90)

## ---------------------------------------------------------------------------------------
library(lupa)

datos_administrativos[, c("id_persona", "cedula", "fecha_nacimiento", "sexo", "ingreso")]

## ---------------------------------------------------------------------------------------
perfil <- perfilar(datos_administrativos)
plan <- planificar_limpieza(perfil, datos_administrativos)
plan[, c("id_accion", "columna", "estrategia", "recomendada", "aplicar")]

## ---------------------------------------------------------------------------------------
plan[plan$estado != "lista", c("id_accion", "estrategia", "estado")]

## ---------------------------------------------------------------------------------------
justificaciones <- plan[plan$columna %in% c("sexo", "ingreso"), ]
for (i in seq_len(nrow(justificaciones))) {
  cat(justificaciones$id_accion[i], justificaciones$estrategia[i], "\n")
  writeLines(strwrap(justificaciones$justificacion[i], 86, indent = 2, exdent = 2))
}

## ---------------------------------------------------------------------------------------
normalizacion(acentos = FALSE, puntuacion = TRUE)

## ---------------------------------------------------------------------------------------
plan[!is.na(plan$grupo), c(
  "grupo", "estrategia", "recomendada", "aplicar", "decision_grupo"
)]

## ----eval = FALSE-----------------------------------------------------------------------
# plan <- guiar_limpieza(plan, datos_administrativos)

## ----echo = FALSE-----------------------------------------------------------------------
stopifnot(identical(guiar_limpieza(plan, datos_administrativos), plan))

## ---------------------------------------------------------------------------------------
copia <- datos_administrativos
resultado <- aplicar(plan, datos_administrativos)
identical(datos_administrativos, copia)
resultado$registro[, c("id_accion", "estrategia", "estado", "n_cambiadas")]

## ---------------------------------------------------------------------------------------
data.frame(
  cedula_antes = datos_administrativos$cedula,
  cedula_despues = resultado$datos$cedula,
  sexo_antes = datos_administrativos$sexo,
  sexo_despues = resultado$datos$sexo
)

## ---------------------------------------------------------------------------------------
marcadas <- !is.na(resultado$datos$.grupo_duplicado)
resultado$datos[marcadas, c("id_persona", "cedula", ".fila_duplicada", ".grupo_duplicado")]
attr(resultado$datos, "columnas_duplicadas_marcadas")

## ---------------------------------------------------------------------------------------
sin_accion <- attr(plan, "hallazgos_sin_accion")
sin_accion[, c("hallazgo", "columna", "severidad")]

## ---------------------------------------------------------------------------------------
perfil_despues <- perfilar(resultado$datos)
cambios <- comparar_perfiles(perfil, perfil_despues)
cambios[cambios$aspecto == "hallazgo", c("columna", "cambio", "valor_anterior", "valor_actual")]

## ---------------------------------------------------------------------------------------
tramites <- data.frame(
  id_tramite = 101:112,
  codigo = rep(1:3, each = 4),
  descripcion = rep(c("Alta", "Baja", "Cambio"), each = 4),
  stringsAsFactors = FALSE
)
tramites$descripcion[c(2, 6)] <- NA

perfil_fd <- perfilar(tramites, muestra = Inf)
plan_fd <- planificar_limpieza(perfil_fd, tramites)
imputacion <- grep("^imputar_dependencia_funcional", plan_fd$estrategia)
plan_fd[imputacion, c("id_accion", "estrategia", "recomendada", "aplicar")]
writeLines(strwrap(plan_fd$justificacion[imputacion], 88))
plan_fd$parametros[[imputacion]]$mapa

## ---------------------------------------------------------------------------------------
plan_fd$aplicar[imputacion] <- TRUE
plan_fd$decision_grupo[imputacion] <- "elegida"
resultado_fd <- aplicar(plan_fd, tramites)
resultado_fd$registro[, c("id_accion", "estrategia", "estado", "n_cambiadas")]
data.frame(
  codigo = tramites$codigo,
  antes = tramites$descripcion,
  despues = resultado_fd$datos$descripcion
)

## ----cierre, include=FALSE----------------------------------------------------
options(.opciones_previas)

