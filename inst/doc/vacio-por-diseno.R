## ----setup, include=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
# El ancho se restaura al terminar la viñeta: knitr corre cada una en su propia
# sesion, asi que no se filtra, pero el CRAN Cookbook pide devolver toda opcion
# que se cambie y no cuesta nada cumplirlo.
.opciones_previas <- options(width = 90)
library(lupa)

## ---------------------------------------------------------------------------------------
tiene_auto <- rep(c("Si", "No"), c(600, 1400))
encuesta <- data.frame(
  tiene_auto = tiene_auto,
  marca_auto = ifelse(tiene_auto == "Si", "Ford", NA),
  stringsAsFactors = FALSE
)

perfil <- perfilar(encuesta, analizar_dependencias = FALSE)
h <- hallazgos(perfil)
h[h$tipo_hallazgo == "faltantes", c("columna", "severidad", "n_afectados", "n_evaluados")]

## ---------------------------------------------------------------------------------------
estructural <- h[h$tipo_hallazgo == "posible_ausencia_estructural", ]
estructural[, c("columna", "severidad", "n_afectados")]
writeLines(strwrap(estructural$evidencia, 88))
writeLines(strwrap(estructural$sugerencia, 88))

## ---------------------------------------------------------------------------------------
set.seed(4)
ruidosa <- encuesta
fuera <- sample(which(ruidosa$tiene_auto == "No"), 200)
ruidosa$marca_auto[fuera] <- "Ford"
sum(hallazgos(perfilar(ruidosa, analizar_dependencias = FALSE))$tipo_hallazgo ==
      "posible_ausencia_estructural")

## ---------------------------------------------------------------------------------------
declarado <- perfilar(
  encuesta,
  analizar_dependencias = FALSE,
  aplicabilidad = list(marca_auto = ~ tiene_auto == "Si")
)

sum(hallazgos(declarado)$tipo_hallazgo == "faltantes")
columnas(declarado)[
  columnas(declarado)$columna == "marca_auto",
  c("n", "n_aplicables", "n_no_aplica", "n_faltantes", "prop_faltantes")
]

## ---------------------------------------------------------------------------------------
cobertura_declarada <- cobertura(declarado)
writeLines(strwrap(
  cobertura_declarada$motivo[cobertura_declarada$columna == "marca_auto"], 88
))

## ---------------------------------------------------------------------------------------
historial <- data.frame(
  id = 1:100,
  valido_hasta = c(rep(NA, 40), 41:100)
)
opcional <- perfilar(
  historial, analizar_dependencias = FALSE,
  columnas_opcionales = "valido_hasta"
)
sum(hallazgos(opcional)$tipo_hallazgo == "faltantes")

## ---------------------------------------------------------------------------------------
inconsistente <- data.frame(
  tiene_auto = c("Si", "No", "No", "Si"),
  marca_auto = c("Ford", NA, "Fiat", "VW"),
  stringsAsFactors = FALSE
)
p <- perfilar(
  inconsistente, analizar_dependencias = FALSE,
  aplicabilidad = list(marca_auto = ~ tiene_auto == "Si")
)
fuera_de_universo <- hallazgos(p)[
  hallazgos(p)$tipo_hallazgo == "valor_fuera_de_aplicabilidad",
]
fuera_de_universo[, c("columna", "severidad", "n_afectados")]
writeLines(strwrap(fuera_de_universo$evidencia, 88))
fuera_de_universo$trazabilidad[[1]]$indices_fila

## ---------------------------------------------------------------------------------------
dudoso <- data.frame(
  tiene_auto = c("Si", "No", NA, "Si"),
  marca_auto = c("Ford", NA, NA, "VW"),
  stringsAsFactors = FALSE
)
pd <- perfilar(
  dudoso, analizar_dependencias = FALSE,
  aplicabilidad = list(marca_auto = ~ tiene_auto == "Si")
)
columnas(pd)[
  columnas(pd)$columna == "marca_auto",
  c("n_aplicables", "n_no_aplica", "n_aplicabilidad_indeterminada")
]

## ---------------------------------------------------------------------------------------
set.seed(9)
n <- 900
atributo <- rep(c("pais", "edad", "correo"), each = n / 3)
largo <- data.frame(
  entidad = rep(seq_len(n / 3), 3),
  atributo = atributo,
  valor_texto = ifelse(
    atributo == "edad", NA,
    ifelse(atributo == "pais", sample(c("UY", "uy", "AR"), n, TRUE),
           paste0("a", seq_len(n), "@x.uy"))
  ),
  valor_num = ifelse(atributo == "edad", sample(18:80, n, TRUE), NA),
  stringsAsFactors = FALSE
)

plano <- perfilar(largo, analizar_dependencias = FALSE)
table(as.character(hallazgos(plano)$tipo_hallazgo))

## ---------------------------------------------------------------------------------------
por_grupo <- perfilar_por(
  largo, "atributo", clave = "entidad", min_filas = 30,
  analizar_dependencias = FALSE
)
sum(por_grupo$tipo_hallazgo == "faltantes")
table(as.character(por_grupo$tipo_hallazgo))

## ---------------------------------------------------------------------------------------
head(attr(por_grupo, "cobertura_grupos"), 3)

## ---------------------------------------------------------------------------------------
casi_vacia <- data.frame(
  x = c(rep(NA_character_, 160), rep(letters[1:20], 2)),
  stringsAsFactors = FALSE
)
p <- perfilar(casi_vacia, columnas_opcionales = "x",
              analizar_dependencias = FALSE)
silencio <- hallazgos(p)[hallazgos(p)$tipo_hallazgo == "regla_silencia_ausencia", ]
silencio[, c("columna", "severidad", "n_afectados", "n_evaluados")]
writeLines(strwrap(silencio$evidencia, 88))

## ---------------------------------------------------------------------------------------
rubros <- data.frame(rubro = rep(1:19, length.out = 60))
rubros$rubro[c(5, 9)] <- 99

campos <- c("columna", "minimo", "maximo", "media", "n_outliers",
            "estado_resumen_cuantitativo")
rbind(
  columnas(perfilar(rubros, analizar_dependencias = FALSE))[, campos],
  columnas(perfilar(rubros, analizar_dependencias = FALSE,
                    columnas_codigo = "rubro"))[, campos]
)

## ---------------------------------------------------------------------------------------
edades <- data.frame(edad = c(34, 51, 999, 27, 999, 45, 62, 999, 38, 29))

campos <- c("columna", "n_faltantes_disfrazados", "media", "maximo",
            "n_valores_excluidos_resumen")
rbind(
  columnas(perfilar(edades, analizar_dependencias = FALSE))[, campos],
  columnas(perfilar(edades, analizar_dependencias = FALSE,
                    sentinelas_numericos = 999))[, campos]
)

## ---------------------------------------------------------------------------------------
codigos <- data.frame(codigo = c(1, 66, 9999))
c(
  por_omision = columnas(perfilar(codigos, analizar_dependencias = FALSE))$
    n_faltantes_disfrazados,
  naniar = columnas(perfilar(codigos, analizar_dependencias = FALSE,
                             sentinelas_numericos = sentinelas_naniar))$
    n_faltantes_disfrazados
)

## ----cierre, include=FALSE----------------------------------------------------
options(.opciones_previas)

