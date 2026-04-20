
## Codigo para unir matrices de Shannon global
# 1. Establecer carpeta de trabajo
setwd("C:/Users/kcoll/Desktop/Shannon-global")  # <--- cambia esto por tu ruta real

# 2. Listar todos los archivos CSV
archivos <- list.files(pattern = "\\.csv$")

# 3. Leer todos los CSV y añadir el nombre del archivo como columna
lista_datos <- lapply(archivos, function(archivo) {
  df <- read.csv(archivo)
  df$Archivo <- archivo  # Agrega el nombre del archivo como columna
  return(df)
})


# 4. Unir en una tabla larga
datos_unidos <- do.call(rbind, lista_datos)

# 5. Extraer nombre base del archivo (sin .csv)
datos_unidos$Sitio <- gsub("\\.csv$", "", datos_unidos$Archivo)

# 6. Eliminar columna de archivo si no la necesitas
datos_unidos$Archivo <- NULL

# 7. Ver una muestra
head(datos_unidos)
print(datos_unidos)

# 8. Guardar el resultado
write.csv(datos_unidos, "shannon_todas_las_matrices.csv", row.names = FALSE)
getwd()
