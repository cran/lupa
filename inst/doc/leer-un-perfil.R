## ----setup, include = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
# El ancho se restaura al terminar la viñeta, como pide el CRAN Cookbook.
.opciones_previas <- options(width = 90)

## ----perfil-----------------------------------------------------------------------------
library(lupa)
perfil <- perfilar(datos_operativos, clave = "id_evento")
h <- hallazgos(perfil)

## ----severidades------------------------------------------------------------------------
levels(h$severidad)
table(h$severidad)
nrow(h[h$severidad >= "sospechoso", ])

## ----ok---------------------------------------------------------------------------------
h[h$severidad == "ok", c("columna", "tipo_hallazgo", "n_afectados")]

## ----disfrazados------------------------------------------------------------------------
h[h$tipo_hallazgo == "faltantes_disfrazados", c("columna", "severidad", "evidencia")]

## ----declarar---------------------------------------------------------------------------
declarado <- perfilar(datos_operativos["monto"], sentinelas_numericos = -99)
hd <- hallazgos(declarado)
hd[hd$tipo_hallazgo == "faltantes_disfrazados", c("columna", "severidad", "evidencia")]

## ----declarar-resumen-------------------------------------------------------------------
resumen <- c("minimo", "mediana", "n_valores_excluidos_resumen")
rbind(
  por_omision = columnas(perfil)[columnas(perfil)$columna == "monto", resumen],
  declarado = columnas(declarado)[, resumen]
)

## ----comprobar-severidades, echo = FALSE------------------------------------------------
stopifnot(
  is.ordered(h$severidad),
  identical(levels(h$severidad), c("ok", "sospechoso", "error")),
  identical(
    as.character(h$severidad[h$tipo_hallazgo == "faltantes_disfrazados" &
                               h$columna %in% "monto"]),
    "sospechoso"
  ),
  identical(
    as.character(hd$severidad[hd$tipo_hallazgo == "faltantes_disfrazados"]),
    "error"
  )
)

## ----barrera----------------------------------------------------------------------------
c(errores = sum(h$severidad == "error"), no_evaluados = nrow(cobertura(perfil)))

## ----comprobar-campos, echo = FALSE-----------------------------------------------------
stopifnot(identical(names(h), c(
  "columna", "tipo_hallazgo", "severidad", "descripcion", "evidencia",
  "sugerencia", "n_evaluados", "n_afectados", "unidad_conteo",
  "estado_reparacion", "trazabilidad"
)))

## ----mayusculas-------------------------------------------------------------------------
mayus <- h[h$tipo_hallazgo == "mayusculas_inconsistentes", ]
for (campo in c("descripcion", "evidencia", "sugerencia")) {
  writeLines(strwrap(paste0(campo, ": ", mayus[[campo]]), 88, exdent = 2))
}
mayus[, c("n_evaluados", "n_afectados", "unidad_conteo")]
mayus$trazabilidad[[1]]$claves$id_evento

## ----comprobar-mayusculas, echo = FALSE-------------------------------------------------
stopifnot(
  identical(mayus$unidad_conteo, "valor_distinto"),
  identical(as.numeric(mayus$n_afectados), 2),
  length(mayus$trazabilidad[[1]]$claves$id_evento) > mayus$n_afectados
)

## ----cardinalidad-----------------------------------------------------------------------
h[h$tipo_hallazgo == "alta_cardinalidad", c("columna", "severidad", "evidencia")]

## ----accesores--------------------------------------------------------------------------
n_filas(perfil)
nrow(hallazgos(perfil))
nrow(cobertura(perfil))
campos <- c("columna", "tipo_inferido", "n_faltantes_totales", "n_distintos")
columnas(perfil)[1:4, campos]

## ----sql--------------------------------------------------------------------------------
is.null(sql_perfil(perfil))

## ----clave------------------------------------------------------------------------------
perfil$meta$clave$unicidad$estado
perfil$meta$clave$ausencia_nulos$estado
repetida <- h[h$tipo_hallazgo == "clave_no_unica", ]
repetida$trazabilidad[[1]]$localizador
repetida$trazabilidad[[1]]$claves
writeLines(strwrap(repetida$sugerencia, 88))

## ----clave-registro---------------------------------------------------------------------
por_registro <- perfilar(datos_operativos, clave = "id_registro")
hr <- hallazgos(por_registro)
hr[hr$tipo_hallazgo %in% c("clave_no_unica", "filas_duplicadas"),
   c("tipo_hallazgo", "n_afectados")]
datos_operativos[datos_operativos$id_registro == 10,
                 c("id_registro", "monto", "id_evento")]

## ----comprobar-clave, echo = FALSE------------------------------------------------------
traza_registro <- hr$trazabilidad[[which(hr$tipo_hallazgo == "clave_no_unica")]]
traza_copias <- hr$trazabilidad[[which(hr$tipo_hallazgo == "filas_duplicadas")]]
stopifnot(
  identical(perfil$meta$clave$unicidad$estado, "refutada"),
  identical(repetida$trazabilidad[[1]]$localizador, "clave_declarada"),
  setequal(traza_registro$indices_fila, c(1L, 10L, 11L, 12L)),
  setequal(traza_copias$indices_fila, c(1L, 12L))
)

## ----clave-ausentes, warning = FALSE----------------------------------------------------
tramites <- data.frame(
  id_tramite = c("T1", "T2", NA, NA, "T5"),
  monto = c(10, 20, 30, 40, 50)
)
p_tramites <- perfilar(tramites, clave = "id_tramite")
hallazgos(p_tramites)[, c("columna", "tipo_hallazgo", "severidad", "n_afectados")]
p_tramites$meta$clave$unicidad$estado
p_tramites$meta$clave$ausencia_nulos$estado

## ----sin-casos, warning = FALSE---------------------------------------------------------
vacia <- data.frame(id_tramite = rep(NA_character_, 3), monto = c(1, 2, 3))
perfilar(vacia, clave = "id_tramite")$meta$clave$unicidad$estado

## ----comprobar-ausentes, echo = FALSE---------------------------------------------------
stopifnot(
  "clave_con_ausentes" %in% hallazgos(p_tramites)$tipo_hallazgo,
  !"clave_no_unica" %in% hallazgos(p_tramites)$tipo_hallazgo,
  identical(p_tramites$meta$clave$unicidad$estado, "verificada"),
  identical(p_tramites$meta$clave$ausencia_nulos$estado, "refutada")
)

## ----sugerir----------------------------------------------------------------------------
nrow(sugerir_clave(datos_operativos))
candidatas <- sugerir_clave(datos_operativos, umbral_casi = 0.9)
candidatas[, c("columna", "identifica", "sin_faltantes", "tasa_distintos",
               "parecido_nombre")]
writeLines(candidatas$motivo[1])

## ----elegir, eval = FALSE---------------------------------------------------------------
# clave <- elegir_clave(datos_operativos)
# perfil <- perfilar(datos_operativos, clave = clave)

## ----discordancias----------------------------------------------------------------------
expedientes <- data.frame(
  expediente = sprintf("EXP-%02d", 1:6),
  fecha_ingreso = as.Date(c(
    "2023-03-10", "2023-11-02", "2024-01-15", "2023-12-30", "2024-02-01", NA
  )),
  anio_fiscal = c(2023L, 2023L, 2024L, 2024L, 2024L, 2024L),
  anio_archivo = c(2023L, 2023L, 2024L, 2023L, 2024L, 2024L)
)
anio <- senal_redundante(
  c("fecha_ingreso", "anio_fiscal", "anio_archivo"),
  transformacion = list(fecha_ingreso = function(x) as.integer(format(x, "%Y"))),
  nombre = "año del expediente"
)
discordancias <- detectar_discordancias(expedientes, anio)
discordancias[, c("senal", "n_filas", "n_evaluadas", "n_discordantes")]
writeLines(strwrap(discordancias$evidencia, 88))

## ----comprobar-discordancias, echo = FALSE----------------------------------------------
stopifnot(
  identical(as.numeric(discordancias$n_evaluadas), 5),
  identical(as.numeric(discordancias$n_discordantes), 1),
  grepl("fila 4", discordancias$evidencia, fixed = TRUE)
)

## ----granularidades---------------------------------------------------------------------
granularidades()[, c("nivel", "granularidad", "relacional")]

## ----cierre, include = FALSE--------------------------------------------------
options(.opciones_previas)

