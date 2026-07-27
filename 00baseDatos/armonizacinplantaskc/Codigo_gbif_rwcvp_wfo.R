library(rWCVP)
library(rWCVPdata)
library(rgbif)
library(dplyr)
library(stringr)

# Actualizar rgbif si fuera necesario
# install.packages("rgbif")

if (packageVersion("rgbif") < package_version("3.8.5")) {
  stop(
    "Debe actualizar rgbif. Ejecute: install.packages('rgbif')"
  )
}

#--------------------------------------------------
# Funciones auxiliares
#--------------------------------------------------

# Devuelve un valor solo cuando existe un único resultado
unico_o_na <- function(x) {
  
  x <- unique(na.omit(as.character(x)))
  
  if (length(x) == 1) {
    return(x)
  }
  
  NA_character_
}

# Extrae una columna, considerando posibles diferencias
# entre versiones de rgbif
extraer_columna <- function(datos, candidatos) {
  
  disponibles <- candidatos[candidatos %in% names(datos)]
  
  if (length(disponibles) == 0) {
    return(rep(NA_character_, nrow(datos)))
  }
  
  as.character(datos[[disponibles[1]]])
}

# Normaliza nombres para compararlos
normalizar_nombre <- function(x) {
  
  x %>%
    str_to_lower() %>%
    str_replace_all("×", "x") %>%
    str_squish() %>%
    na_if("")
}

#--------------------------------------------------
# 1. Preparar la base original
#--------------------------------------------------

columnas_originales <- names(depurado_2)

df <- depurado_2 %>%
  mutate(
    ID_fila = row_number(),
    
    Nombre_original = as.character(Genero.especie),
    
    Nombre_busqueda = as.character(Genero.especie) %>%
      str_replace_all("\\.", " ") %>%
      str_squish() %>%
      na_if("")
  )

#==================================================
# CONSULTA EN rWCVP
#==================================================

#--------------------------------------------------
# 2. Buscar nombres en WCVP
#--------------------------------------------------

resultado_wcvp <- wcvp_match_names(
  names_df = df,
  name_col = "Nombre_busqueda",
  id_col = "ID_fila",
  fuzzy = TRUE,
  progress_bar = TRUE
)

#--------------------------------------------------
# 3. Tabla de nombres aceptados WCVP
#--------------------------------------------------

tabla_aceptados_wcvp <- rWCVPdata::wcvp_names %>%
  filter(taxon_status == "Accepted") %>%
  transmute(
    wcvp_accepted_id = plant_name_id,
    Nombre_aceptado_WCVP_temporal = taxon_name
  ) %>%
  distinct(
    wcvp_accepted_id,
    .keep_all = TRUE
  )

#--------------------------------------------------
# 4. Obtener un solo nombre aceptado por fila
#--------------------------------------------------

wcvp_por_fila <- resultado_wcvp %>%
  left_join(
    tabla_aceptados_wcvp,
    by = "wcvp_accepted_id"
  ) %>%
  group_by(ID_fila) %>%
  summarise(
    Nombre_aceptado_rWCVP =
      unico_o_na(Nombre_aceptado_WCVP_temporal),
    
    .groups = "drop"
  )

#==================================================
# CONSULTA EN GBIF
#==================================================

#--------------------------------------------------
# 5. Preparar nombres únicos para consultar GBIF
#--------------------------------------------------

nombres_gbif <- df %>%
  distinct(Nombre_busqueda) %>%
  filter(
    !is.na(Nombre_busqueda),
    Nombre_busqueda != ""
  ) %>%
  transmute(
    scientificName = Nombre_busqueda
  )

#--------------------------------------------------
# 6. Consultar el Backbone Taxonomy de GBIF
#--------------------------------------------------

resultado_gbif_bruto <- rgbif::name_backbone_checklist(
  name_data = nombres_gbif,
  kingdom = "Plantae",
  strict = TRUE,
  verbose = FALSE,
  bucket_size = 100,
  sleep = 1
)

#--------------------------------------------------
# 7. Extraer resultados entregados por GBIF
#--------------------------------------------------

resultado_gbif <- tibble(
  Nombre_busqueda = extraer_columna(
    resultado_gbif_bruto,
    c(
      "verbatim_scientificName",
      "verbatim_name"
    )
  ),
  
  Nombre_aceptado_GBIF_completo = coalesce(
    extraer_columna(
      resultado_gbif_bruto,
      "acceptedScientificName"
    ),
    
    extraer_columna(
      resultado_gbif_bruto,
      "scientificName"
    )
  ),
  
  Tipo_coincidencia_GBIF = extraer_columna(
    resultado_gbif_bruto,
    "matchType"
  ),
  
  Rango_GBIF = extraer_columna(
    resultado_gbif_bruto,
    "rank"
  )
)

#--------------------------------------------------
# 8. Extraer el nombre canónico sin autoría
#--------------------------------------------------

resultado_gbif$Nombre_aceptado_GBIF <- NA_character_

filas_validas <- which(
  !is.na(resultado_gbif$Nombre_aceptado_GBIF_completo) &
    resultado_gbif$Nombre_aceptado_GBIF_completo != ""
)

if (length(filas_validas) > 0) {
  
  nombres_parseados <- rgbif::name_parse(
    resultado_gbif$Nombre_aceptado_GBIF_completo[
      filas_validas
    ]
  )
  
  # Usar los nombres reales de las columnas:
  # canonicalnamewithmarker y canonicalname
  nombres_canonicos <- dplyr::coalesce(
    as.character(nombres_parseados$canonicalnamewithmarker),
    as.character(nombres_parseados$canonicalname)
  )
  
  resultado_gbif$Nombre_aceptado_GBIF[
    filas_validas
  ] <- nombres_canonicos
}


resultado_gbif %>%
  select(
    Nombre_busqueda,
    Nombre_aceptado_GBIF_completo,
    Nombre_aceptado_GBIF,
    Tipo_coincidencia_GBIF,
    Rango_GBIF
  ) %>%
  head(20)

#--------------------------------------------------
# 9. Conservar resultados válidos de GBIF
#--------------------------------------------------

gbif_por_nombre <- resultado_gbif %>%
  mutate(
    Tipo_coincidencia_GBIF = str_to_upper(
      str_trim(Tipo_coincidencia_GBIF)
    ),
    
    Rango_GBIF = str_to_upper(
      str_trim(Rango_GBIF)
    ),
    
    Nombre_aceptado_GBIF = Nombre_aceptado_GBIF %>%
      str_squish() %>%
      na_if(""),
    
    Nombre_aceptado_GBIF = case_when(
      Tipo_coincidencia_GBIF %in% c("EXACT", "VARIANT") &
        Rango_GBIF %in% c(
          "SPECIES",
          "SUBSPECIES",
          "VARIETY",
          "FORM"
        ) ~ Nombre_aceptado_GBIF,
      
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    Nombre_busqueda,
    Nombre_aceptado_GBIF,
    Tipo_coincidencia_GBIF,
    Rango_GBIF
  ) %>%
  distinct(
    Nombre_busqueda,
    .keep_all = TRUE
  )


### BAse de datos woldflora
#--------------------------------------------------
# INSTALACIÓN: ejecutar una sola vez
#--------------------------------------------------

install.packages(c(
  "WorldFlora",
  "data.table"
))

# Actualizar rWCVPdata
install.packages(
  "rWCVPdata",
  repos = c(
    "https://matildabrown.github.io/drat",
    "https://cloud.r-project.org"
  )
)


#--------------------------------------------------
# DESCARGAR WFO: ejecutar una sola vez
#--------------------------------------------------

carpeta_wfo <- file.path(
  getwd(),
  "WFO_backbone"
)

dir.create(
  carpeta_wfo,
  showWarnings = FALSE
)

WorldFlora::WFO.download(
  save.dir = carpeta_wfo,
  WFO.remember = TRUE,
  timeout = 1200
)


# Cargar la base WFO descargada anteriormente
WorldFlora::WFO.remember()


#==================================================
# CONSULTA EN WORLD FLORA ONLINE
#==================================================

#--------------------------------------------------
# 10. Preparar nombres para consultar WFO
#--------------------------------------------------

nombres_wfo <- df %>%
  distinct(Nombre_busqueda) %>%
  filter(
    !is.na(Nombre_busqueda),
    Nombre_busqueda != "",
    
    # Excluir nombres con códigos o números
    !str_detect(
      Nombre_busqueda,
      "\\d"
    ),
    
    # Exigir al menos género y epíteto específico
    str_count(
      Nombre_busqueda,
      "\\S+"
    ) >= 2,
    
    # Excluir identificaciones abiertas
    !str_detect(
      str_to_lower(Nombre_busqueda),
      "\\b(sp|spp|indet|cf|aff)\\b"
    )
  ) %>%
  select(Nombre_busqueda)


#--------------------------------------------------
# 11. Buscar nombres en WFO
#--------------------------------------------------

resultado_wfo_bruto <- WorldFlora::WFO.match(
  spec.data = as.data.frame(nombres_wfo),
  WFO.data = WFO.data,
  spec.name = "Nombre_busqueda",
  
  # Obtener el nombre aceptado cuando el consultado
  # corresponde a un sinónimo
  acceptedNameUsageID.match = TRUE,
  
  # Permitir correcciones aproximadas
  Fuzzy = 0.1,
  Fuzzy.min = TRUE,
  
  # No aceptar coincidencias únicamente con el género
  Fuzzy.one = FALSE,
  
  # Permitir buscar mediante los dos primeros términos
  Fuzzy.two = TRUE,
  
  # Los nombres con números ya fueron excluidos
  spec.name.nonumber = FALSE,
  
  verbose = TRUE,
  counter = 500
)

 
#--------------------------------------------------
# 12. Seleccionar una coincidencia por nombre
#--------------------------------------------------

resultado_wfo_unico <- WorldFlora::WFO.one(
  WFO.result = resultado_wfo_bruto,
  priority = "Accepted",
  verbose = TRUE,
  counter = 500
)


#--------------------------------------------------
# 13. Preparar nombres aceptados de WFO
#--------------------------------------------------

wfo_resultados_validos <- resultado_wfo_unico %>%
  as_tibble() %>%
  mutate(
    Nombre_aceptado_WFO = as.character(
      scientificName
    ) %>%
      str_squish() %>%
      na_if(""),
    
    Rango_WFO = as.character(
      taxonRank
    ) %>%
      str_to_upper() %>%
      str_trim(),
    
    Estado_WFO = as.character(
      taxonomicStatus
    ) %>%
      str_to_upper() %>%
      str_trim(),
    
    Tipo_coincidencia_WFO = case_when(
      is.na(Matched) | !Matched ~ "NONE",
      Fuzzy %in% TRUE ~ "FUZZY",
      TRUE ~ "EXACT"
    ),
    
    # Conservar solamente resultados a nivel de especie
    # o categorías infraespecíficas
    Nombre_aceptado_WFO = case_when(
      Matched %in% TRUE &
        Rango_WFO %in% c(
          "SPECIES",
          "SUBSPECIES",
          "VARIETY",
          "FORM"
        ) &
        Estado_WFO == "ACCEPTED" ~
        Nombre_aceptado_WFO,
      
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    Nombre_busqueda,
    Nombre_aceptado_WFO,
    Tipo_coincidencia_WFO,
    Rango_WFO
  ) %>%
  distinct(
    Nombre_busqueda,
    .keep_all = TRUE
  )


#--------------------------------------------------
# 14. Incorporar todos los nombres consultados
#--------------------------------------------------

wfo_por_nombre <- df %>%
  distinct(Nombre_busqueda) %>%
  left_join(
    wfo_resultados_validos,
    by = "Nombre_busqueda"
  ) %>%
  distinct(
    Nombre_busqueda,
    .keep_all = TRUE
  )



#--------------------------------------------------
# 15. Unir las tres fuentes taxonómicas
#--------------------------------------------------

resultado_final <- df %>%
  left_join(
    wcvp_por_fila,
    by = "ID_fila"
  ) %>%
  left_join(
    gbif_por_nombre,
    by = "Nombre_busqueda"
  ) %>%
  left_join(
    wfo_por_nombre,
    by = "Nombre_busqueda"
  ) %>%
  mutate(
    # Nombres normalizados exclusivamente para comparar
    GBIF_normalizado =
      normalizar_nombre(Nombre_aceptado_GBIF),
    
    rWCVP_normalizado =
      normalizar_nombre(Nombre_aceptado_rWCVP),
    
    WFO_normalizado =
      normalizar_nombre(Nombre_aceptado_WFO),
    
    # Resultado general solicitado
    Comparacion_tres_fuentes = case_when(
      !is.na(GBIF_normalizado) &
        !is.na(rWCVP_normalizado) &
        !is.na(WFO_normalizado) &
        GBIF_normalizado == rWCVP_normalizado &
        rWCVP_normalizado == WFO_normalizado ~
        "Coinciden",
      
      TRUE ~ "Revisar"
    ),
    
    # Explicación del resultado
    Detalle_comparacion = case_when(
      !is.na(GBIF_normalizado) &
        !is.na(rWCVP_normalizado) &
        !is.na(WFO_normalizado) &
        GBIF_normalizado == rWCVP_normalizado &
        rWCVP_normalizado == WFO_normalizado ~
        "Coinciden las tres fuentes",
      
      !is.na(GBIF_normalizado) &
        !is.na(rWCVP_normalizado) &
        GBIF_normalizado == rWCVP_normalizado &
        is.na(WFO_normalizado) ~
        "Coinciden GBIF y rWCVP; WFO sin resultado",
      
      !is.na(GBIF_normalizado) &
        !is.na(WFO_normalizado) &
        GBIF_normalizado == WFO_normalizado &
        is.na(rWCVP_normalizado) ~
        "Coinciden GBIF y WFO; rWCVP sin resultado",
      
      !is.na(rWCVP_normalizado) &
        !is.na(WFO_normalizado) &
        rWCVP_normalizado == WFO_normalizado &
        is.na(GBIF_normalizado) ~
        "Coinciden rWCVP y WFO; GBIF sin resultado",
      
      !is.na(GBIF_normalizado) &
        !is.na(rWCVP_normalizado) &
        GBIF_normalizado == rWCVP_normalizado ~
        "Coinciden GBIF y rWCVP; WFO es diferente",
      
      !is.na(GBIF_normalizado) &
        !is.na(WFO_normalizado) &
        GBIF_normalizado == WFO_normalizado ~
        "Coinciden GBIF y WFO; rWCVP es diferente",
      
      !is.na(rWCVP_normalizado) &
        !is.na(WFO_normalizado) &
        rWCVP_normalizado == WFO_normalizado ~
        "Coinciden rWCVP y WFO; GBIF es diferente",
      
      is.na(GBIF_normalizado) &
        is.na(rWCVP_normalizado) &
        is.na(WFO_normalizado) ~
        "Sin resultados en las tres fuentes",
      
      is.na(GBIF_normalizado) |
        is.na(rWCVP_normalizado) |
        is.na(WFO_normalizado) ~
        "Falta resultado en una o más fuentes",
      
      TRUE ~
        "Los tres nombres aceptados son diferentes"
    )
  ) %>%
  arrange(ID_fila) %>%
  select(
    all_of(columnas_originales),
    Nombre_aceptado_GBIF,
    Nombre_aceptado_rWCVP,
    Nombre_aceptado_WFO,
    Comparacion_tres_fuentes,
    Detalle_comparacion
  )



# Instalar paquetes si no los tienes
# install.packages(c("readr", "writexl"))

library(readr)
library(writexl)

# Guardar en CSV
write_csv(
  resultado_final,
  "resultado_final.csv",
  na = ""
)

# Guardar en Excel
write_xlsx(
  resultado_final,
  "resultado_final.xlsx"
)

