# ============================================================
# VERIFICAR Y ACTUALIZAR NOMBRES DE PLANTAS CON rWCVP
# Entrada: Excel, CSV, TXT o TSV
# Salida: Excel y CSV con nombres aceptados y casos a revisar
# ============================================================

# ----------------------------
# 1. CONFIGURACIÓN
# ----------------------------

ruta_archivo <- NULL          # NULL abre una ventana para seleccionar
hoja_excel <- 1               # número o nombre de la hoja
columna_nombres <- NULL       # ej.: "especie"; NULL = detección automática
columna_autores <- NULL       # ej.: "autor"; NULL si no existe
usar_fuzzy <- TRUE            # coincidencia aproximada
umbral_similitud <- 0.90
carpeta_salida <- "resultados_nombres_rWCVP"


# ----------------------------
# 2. INSTALAR PAQUETES
# ----------------------------

paquetes <- c(
  "readxl", "readr", "dplyr", "stringr",
  "purrr", "tibble", "tidyr", "writexl", "remotes"
)

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

if (!requireNamespace("rWCVP", quietly = TRUE)) {
  remotes::install_github("matildabrown/rWCVP", upgrade = "never")
}

if (!requireNamespace("rWCVPdata", quietly = TRUE)) {
  install.packages(
    "rWCVPdata",
    repos = c(
      "https://matildabrown.github.io/drat",
      "https://cloud.r-project.org"
    )
  )
}

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)
library(writexl)
library(rWCVP)


# ----------------------------
# 3. FUNCIONES
# ----------------------------

normalizar_columna <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

leer_archivo <- function(ruta, hoja = 1) {
  ext <- tolower(tools::file_ext(ruta))

  if (ext %in% c("xlsx", "xls")) {
    return(readxl::read_excel(ruta, sheet = hoja))
  }

  if (ext == "csv") {
    return(readr::read_csv(
      ruta,
      show_col_types = FALSE,
      guess_max = 100000
    ))
  }

  if (ext %in% c("txt", "tsv")) {
    return(readr::read_tsv(
      ruta,
      show_col_types = FALSE,
      guess_max = 100000
    ))
  }

  stop("Formato no compatible. Use XLSX, XLS, CSV, TXT o TSV.")
}

detectar_columna_nombres <- function(datos, columna = NULL) {
  if (!is.null(columna)) {
    if (!columna %in% names(datos)) {
      stop(
        paste0(
          "No existe la columna '", columna, "'.\n",
          "Columnas disponibles: ",
          paste(names(datos), collapse = ", ")
        )
      )
    }
    return(columna)
  }

  candidatas <- c(
    "nombre_cientifico", "scientific_name", "scientificname",
    "taxon", "taxon_name", "species", "especie",
    "nombre_especie", "nombre_aceptado", "planta"
  )

  nombres_norm <- normalizar_columna(names(datos))
  posiciones <- match(candidatas, nombres_norm, nomatch = 0)
  posiciones <- posiciones[posiciones > 0]

  if (length(posiciones) == 0) {
    stop(
      paste0(
        "No se detectó la columna de nombres.\n",
        "Escríbala manualmente en columna_nombres.\n",
        "Columnas disponibles: ",
        paste(names(datos), collapse = ", ")
      )
    )
  }

  detectada <- names(datos)[posiciones[1]]
  message("Columna detectada: ", detectada)
  detectada
}

resolver_grupo <- function(df, umbral = 0.90) {
  original <- df$nombre_original[1]
  autor <- df$autor_original[1]
  validas <- df %>% filter(!is.na(wcvp_id))

  if (nrow(validas) == 0) {
    return(tibble(
      id_nombre = df$id_nombre[1],
      nombre_original = original,
      autor_original = autor,
      nombre_coincidente_WCVP = NA_character_,
      nombre_aceptado_WCVP = NA_character_,
      autor_nombre_aceptado = NA_character_,
      familia_WCVP = NA_character_,
      rango_WCVP = NA_character_,
      estado_nombre_original = NA_character_,
      tipo_coincidencia = "Sin coincidencia",
      similitud = NA_real_,
      multiples_coincidencias = FALSE,
      numero_conceptos_aceptados = 0L,
      requiere_revision = "Sí",
      motivo_revision = "No se encontró coincidencia en WCVP",
      nombre_final_sugerido = original
    ))
  }

  ids_aceptados <- unique(na.omit(validas$wcvp_accepted_id))
  nombres_aceptados <- unique(na.omit(validas$nombre_aceptado_WCVP))

  validas <- validas %>%
    mutate(
      prioridad = case_when(
        str_detect(match_type, "^Exact") ~ 1L,
        str_detect(match_type, "^Fuzzy") ~ 2L,
        TRUE ~ 3L
      ),
      similitud_orden = replace_na(match_similarity, 0)
    ) %>%
    arrange(prioridad, desc(similitud_orden))

  elegida <- validas %>% slice(1)

  n_conceptos <- length(ids_aceptados)
  if (n_conceptos == 0) n_conceptos <- length(nombres_aceptados)

  difusa <- str_detect(elegida$match_type, "^Fuzzy")
  ambigua <- n_conceptos > 1
  sin_aceptado <- is.na(elegida$nombre_aceptado_WCVP) ||
    elegida$nombre_aceptado_WCVP == ""

  motivos <- character()
  if (difusa) motivos <- c(motivos, "Coincidencia difusa")
  if (
    difusa &&
    !is.na(elegida$match_similarity) &&
    elegida$match_similarity < umbral
  ) {
    motivos <- c(
      motivos,
      paste0("Similitud inferior a ", umbral)
    )
  }
  if (ambigua) {
    motivos <- c(
      motivos,
      "Múltiples conceptos aceptados"
    )
  }
  if (sin_aceptado) {
    motivos <- c(
      motivos,
      "No se recuperó nombre aceptado"
    )
  }
  if (length(motivos) == 0) {
    motivos <- "Resuelto automáticamente"
  }

  nombre_aceptado_mostrar <- if (ambigua) {
    paste(sort(nombres_aceptados), collapse = " | ")
  } else {
    elegida$nombre_aceptado_WCVP
  }

  sugerido <- if (!ambigua && !sin_aceptado) {
    elegida$nombre_aceptado_WCVP
  } else {
    original
  }

  tibble(
    id_nombre = df$id_nombre[1],
    nombre_original = original,
    autor_original = autor,
    nombre_coincidente_WCVP = elegida$wcvp_name,
    nombre_aceptado_WCVP = nombre_aceptado_mostrar,
    autor_nombre_aceptado = elegida$autor_nombre_aceptado,
    familia_WCVP = elegida$familia_WCVP,
    rango_WCVP = elegida$rango_aceptado_WCVP,
    estado_nombre_original = elegida$wcvp_status,
    tipo_coincidencia = elegida$match_type,
    similitud = elegida$match_similarity,
    multiples_coincidencias = nrow(validas) > 1,
    numero_conceptos_aceptados = n_conceptos,
    requiere_revision = ifelse(
      difusa || ambigua || sin_aceptado,
      "Sí",
      "No"
    ),
    motivo_revision = paste(motivos, collapse = "; "),
    nombre_final_sugerido = sugerido
  )
}


# ----------------------------
# 4. LEER DATOS
# ----------------------------

if (is.null(ruta_archivo)) {
  message("Seleccione el archivo con los nombres de plantas.")
  ruta_archivo <- file.choose()
}

datos_originales <- leer_archivo(ruta_archivo, hoja_excel)

if (nrow(datos_originales) == 0) {
  stop("El archivo no contiene registros.")
}

columna_nombres <- detectar_columna_nombres(
  datos_originales,
  columna_nombres
)

if (
  !is.null(columna_autores) &&
  !columna_autores %in% names(datos_originales)
) {
  stop(
    paste0(
      "No existe la columna de autores '",
      columna_autores,
      "'."
    )
  )
}


# ----------------------------
# 5. LISTA ÚNICA
# ----------------------------

nombres_unicos <- datos_originales %>%
  transmute(
    nombre_original = str_squish(
      as.character(.data[[columna_nombres]])
    ),
    autor_original = if (is.null(columna_autores)) {
      NA_character_
    } else {
      str_squish(
        as.character(.data[[columna_autores]])
      )
    }
  ) %>%
  filter(!is.na(nombre_original), nombre_original != "") %>%
  distinct(nombre_original, autor_original) %>%
  mutate(id_nombre = row_number(), .before = 1)

message("Nombres únicos a verificar: ", nrow(nombres_unicos))


# ----------------------------
# 6. COINCIDENCIA CON WCVP
# ----------------------------

wcvp_names <- rWCVPdata::wcvp_names

if (is.null(columna_autores)) {
  coincidencias <- rWCVP::wcvp_match_names(
    names_df = nombres_unicos,
    wcvp_names = wcvp_names,
    name_col = "nombre_original",
    id_col = "id_nombre",
    fuzzy = usar_fuzzy,
    progress_bar = TRUE
  )
} else {
  coincidencias <- rWCVP::wcvp_match_names(
    names_df = nombres_unicos,
    wcvp_names = wcvp_names,
    name_col = "nombre_original",
    author_col = "autor_original",
    id_col = "id_nombre",
    fuzzy = usar_fuzzy,
    progress_bar = TRUE
  )
}


# ----------------------------
# 7. NOMBRES ACEPTADOS
# ----------------------------

catalogo_aceptados <- wcvp_names %>%
  filter(taxon_status == "Accepted") %>%
  transmute(
    wcvp_accepted_id = plant_name_id,
    nombre_aceptado_WCVP = taxon_name,
    autor_nombre_aceptado = taxon_authors,
    familia_WCVP = family,
    rango_aceptado_WCVP = taxon_rank,
    powo_id_aceptado = powo_id
  )

coincidencias_completas <- coincidencias %>%
  left_join(
    catalogo_aceptados,
    by = "wcvp_accepted_id"
  ) %>%
  arrange(
    id_nombre,
    match_type,
    desc(match_similarity)
  )


# ----------------------------
# 8. RESOLVER RESULTADOS
# ----------------------------

nombres_resueltos <- coincidencias_completas %>%
  group_by(id_nombre) %>%
  group_modify(
    ~ resolver_grupo(
      .x,
      umbral = umbral_similitud
    )
  ) %>%
  ungroup() %>%
  arrange(id_nombre)

revisar_manualmente <- nombres_resueltos %>%
  filter(requiere_revision == "Sí")

sin_coincidencia <- nombres_resueltos %>%
  filter(tipo_coincidencia == "Sin coincidencia")

cambios_taxonomicos <- nombres_resueltos %>%
  filter(
    !is.na(nombre_aceptado_WCVP),
    nombre_original != nombre_aceptado_WCVP,
    numero_conceptos_aceptados <= 1
  )


# ----------------------------
# 9. UNIR A LA BASE ORIGINAL
# ----------------------------

if (is.null(columna_autores)) {
  datos_actualizados <- datos_originales %>%
    mutate(
      nombre_original_WCVP = str_squish(
        as.character(.data[[columna_nombres]])
      )
    ) %>%
    left_join(
      nombres_resueltos %>% select(-autor_original),
      by = c(
        "nombre_original_WCVP" = "nombre_original"
      )
    )
} else {
  datos_actualizados <- datos_originales %>%
    mutate(
      nombre_original_WCVP = str_squish(
        as.character(.data[[columna_nombres]])
      ),
      autor_original_WCVP = str_squish(
        as.character(.data[[columna_autores]])
      )
    ) %>%
    left_join(
      nombres_resueltos,
      by = c(
        "nombre_original_WCVP" = "nombre_original",
        "autor_original_WCVP" = "autor_original"
      )
    )
}


# ----------------------------
# 10. RESUMEN Y EXPORTACIÓN
# ----------------------------

dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

resumen <- tibble(
  indicador = c(
    "Archivo analizado",
    "Columna de nombres",
    "Columna de autores",
    "Registros originales",
    "Nombres únicos",
    "Resueltos sin revisión",
    "Requieren revisión",
    "Sin coincidencia",
    "Cambios taxonómicos detectados",
    "Versión rWCVP",
    "Versión rWCVPdata",
    "Fecha"
  ),
  valor = c(
    normalizePath(ruta_archivo, mustWork = FALSE),
    columna_nombres,
    ifelse(
      is.null(columna_autores),
      "No utilizada",
      columna_autores
    ),
    nrow(datos_originales),
    nrow(nombres_unicos),
    sum(
      nombres_resueltos$requiere_revision == "No",
      na.rm = TRUE
    ),
    nrow(revisar_manualmente),
    nrow(sin_coincidencia),
    nrow(cambios_taxonomicos),
    as.character(packageVersion("rWCVP")),
    as.character(packageVersion("rWCVPdata")),
    as.character(Sys.Date())
  )
)

archivo_salida <- file.path(
  carpeta_salida,
  "nombres_actualizados_rWCVP.xlsx"
)

writexl::write_xlsx(
  list(
    datos_actualizados = datos_actualizados,
    nombres_resueltos = nombres_resueltos,
    cambios_taxonomicos = cambios_taxonomicos,
    revisar_manualmente = revisar_manualmente,
    sin_coincidencia = sin_coincidencia,
    coincidencias_completas = coincidencias_completas,
    resumen = resumen
  ),
  archivo_salida
)

readr::write_csv(
  nombres_resueltos,
  file.path(
    carpeta_salida,
    "nombres_resueltos_rWCVP.csv"
  )
)

readr::write_csv(
  revisar_manualmente,
  file.path(
    carpeta_salida,
    "nombres_para_revision_manual.csv"
  )
)

readr::write_csv(
  datos_actualizados,
  file.path(
    carpeta_salida,
    "base_con_nombres_actualizados.csv"
  )
)

capture.output(
  citation("rWCVP"),
  citation("rWCVPdata"),
  file = file.path(
    carpeta_salida,
    "referencias_rWCVP.txt"
  )
)

cat("\nProceso terminado correctamente.\n")
cat(
  "Resultados guardados en:\n",
  normalizePath(carpeta_salida, mustWork = FALSE),
  "\n\n"
)
print(resumen)
View(nombres_resueltos)
