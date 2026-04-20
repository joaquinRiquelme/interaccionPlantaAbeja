# ========================================================
# 1. Instalar y cargar librerías necesarias
# ========================================================
if (!requireNamespace("rgbif", quietly = TRUE)) install.packages("rgbif")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")

library(rgbif)
library(dplyr)
library(readr)
library(stringr)

# ========================================================
# 2. Cargar y preparar el dataframe (M_060)
# ========================================================
getwd()

base_datos <- readRDS("../00baseDatos/base_datos.RDS")



especies.i <- names(base_datos$matriz_ancha.006)[-1]
especies.i <- names(base_datos$matriz_ancha.007)[-1]
especies.i <- names(base_datos$matriz_ancha.008)[-1]
especies.i <- names(base_datos$matriz_ancha.009)[-1]
especies.i <- names(base_datos$matriz_ancha.010)[-1]
especies.i <- names(base_datos$matriz_ancha.012)[-1]
especies.i <- names(base_datos$matriz_ancha.015)[-1]
especies.i <- names(base_datos$matriz_ancha.016)[-1]
especies.i <- names(base_datos$matriz_ancha.017)[-1]
especies.i <- names(base_datos$matriz_ancha.018)[-1]
especies.i <- names(base_datos$matriz_ancha.036)[-1]
especies.i <- names(base_datos$matriz_ancha.037)[-1]
especies.i <- names(base_datos$matriz_ancha.038)[-1]
especies.i <- names(base_datos$matriz_ancha.039)[-1]
especies.i <- names(base_datos$matriz_ancha.043)[-1]
especies.i <- names(base_datos$matriz_ancha.046)[-1]
especies.i <- names(base_datos$matriz_ancha.047)[-1]
especies.i <- names(base_datos$matriz_ancha.049)[-1]
especies.i <- names(base_datos$matriz_ancha.050)[-1]
especies.i <- names(base_datos$matriz_ancha.058)[-1]


# especies <- base_datos$especies
head(especies.i)

df <- especies.i

head(df)
# Extraer los nombres de especies visitantes desde las columnas (exceptuando la primera columna)
especies_visitantes <- df 

# Formatear nombres: minúsculas y con espacio en lugar de punto
especies_formateadas <- tolower(especies_visitantes)
especies_formateadas <- str_replace_all(especies_formateadas, "\\.", " ")
especies_formateadas <- str_trim(especies_formateadas)

# (Opcional) Eliminar términos como sp, cf, aff
especies_filtradas <- especies_formateadas[!str_detect(especies_formateadas, "\\b(sp|cf|aff)\\b")]

# Extraer el género (primera palabra)
especies_genero <- str_extract(especies_filtradas, "^\\w+")

# ========================================================
# 3. Función para obtener clasificación desde GBIF
# ========================================================
obtener_clasificacion <- function(especie, especie_genero) {
  tryCatch({
    info <- name_backbone(name = especie, strict = FALSE)
    
    if (info$matchType == "NONE") {
      info <- name_backbone(name = especie_genero, strict = FALSE)
    }
    
    if (info$matchType == "NONE") {
      return(data.frame(
        especie = especie,
        especie_genero = especie_genero,
        genero = NA,
        familia = NA,
        orden = NA,
        stringsAsFactors = FALSE
      ))
    } else {
      return(data.frame(
        especie = especie,
        especie_genero = especie_genero,
        genero = info$genus,
        familia = info$family,
        orden = info$order,
        stringsAsFactors = FALSE
      ))
    }
  }, error = function(e) {
    return(data.frame(
      especie = especie,
      especie_genero = especie_genero,
      genero = NA,
      familia = NA,
      orden = NA,
      stringsAsFactors = FALSE
    ))
  })
}

# ========================================================
# 4. Aplicar función a todas las especies visitantes
# ========================================================
clasificacion <- bind_rows(lapply(seq_along(especies_filtradas), function(i) {
  obtener_clasificacion(especies_filtradas[i], especies_genero[i])
}))


head(clasificacion)
table(clasificacion$familia, clasificacion$orden)
table(clasificacion$especie, clasificacion$familia)

head(clasificacion)
sort(unique(clasificacion$familia))
clasificacion.abejas <- subset(clasificacion, 
                               is.element(familia, c("Andrenidae",
                                                     "Halictidae",
                                                     "Apidae",
                                                     "Megachilidae",
                                                     "Melittidae",
                                                     "Colletidae",
                                                     "Stenotritidae")))
table(clasificacion.abejas$especie, clasificacion.abejas$familia)

DIT <- base_datos$DIT
head(DIT)
DIT.i <- subset(DIT, ID=="M_050")
DIT.i

europa <- read.csv("../00baseDatos/baseeuropea/trait_data_input.csv", fileEncoding = "latin1")[-1,]
head(europa)
summary(europa)


europa.i <- europa[,c("Order","Family","Genus","Species","Genus_and_species",
                      "ITD_.inter.tegular_distance.","ITD_.inter.tegular_distance..2")]
head(europa.i)
europa.i$Genus_and_species <- tolower(europa.i$Genus_and_species)
head(clasificacion.abejas)

cruce <- merge(clasificacion.abejas[,c("orden","especie")], europa.i, by.x="especie", by.y="Genus_and_species", all==TRUE)
cruce
setdiff(unique(cruce$especie), unique(clasificacion.abejas$especie))
setdiff(unique(clasificacion.abejas$especie), unique(cruce$especie))

data <- read.csv("your_file.csv", fileEncoding = "latin1")

# Guardar resultados
write_csv(clasificacion, "clasificacion_resultados_M_044COR.csv")


# ========================================================
# 5. Filtrar Hymenoptera y familias de abejas
# ========================================================
# Leer archivo recién guardado (asegurando separación correcta)
resultado_arreglado <- read_csv("clasificacion_resultados_M_001.csv")

# Definir familias de abejas
familias_abejas <- c("Apidae", "Halictidae", "Megachilidae", "Andrenidae", 
                     "Colletidae", "Melittidae", "Stenotritidae")

# Filtrar por orden Hymenoptera y familia de abejas
resultados_abejas <- resultado_arreglado %>%
  filter(orden == "Hymenoptera", familia %in% familias_abejas)

# Ver primeras filas
print(head(resultados_abejas))


# Guardar los resultados en un archivo CSV
write_csv(resultado1, "listado_especies_M_001.csv")
getwd()
