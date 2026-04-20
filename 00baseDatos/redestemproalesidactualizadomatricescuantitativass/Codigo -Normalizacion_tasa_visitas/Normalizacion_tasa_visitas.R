
##Para revirificar el directorio actual
getwd()

# Lista de dataframes ya cargados - transformarlo
dfchaco0611 <- chaco0611_unida 

# Lista nombrada
matrices <- list(
  dfchaco0611 = chaco0611_unida)
  

# Vector de tiempos de muestreo
tiempos_muestreo <- c(5)

# 💡 Paso clave: convertir a matriz SOLO columnas numéricas y usar los nombres de especies como rownames
matrices_num <- lapply(matrices, function(df) {
  rownames(df) <- df[[1]]  # Primera columna como nombres de fila
  df_numeric <- df[, sapply(df, is.numeric)]  # Solo columnas numéricas
  as.matrix(df_numeric)  # Convertir a matriz
})

# Normalizar matrices
matrices_estandarizadas <- mapply(function(mat, tiempo) mat / tiempo,
                                  matrices_num, tiempos_muestreo, SIMPLIFY = FALSE)

# Nombres de salida para los archivos
nombres_salida <- c("dfchaco0611_est.csv")


# Cambiar el directorio de trabajo temporalmente
setwd("C:/Users/kcoll/Desktop/Tesis -Cap 1/1.0 TESIS/1. Capitulo 1 -redes/RESULTADOS/Resultados-Estandarizados-tasa de visita") 

# Guardar matrices con nombres de fila (especies) - 
mapply(write.csv, matrices_estandarizadas, nombres_salida, row.names = TRUE)


##Sirve para ver donde están los archivos guardados/ muestra Sólo los archivos terminados en "_est.csv"
list.files(pattern = "_est.csv$")

####Útil si quieres ver todos los .csv, incluyendo los no estandarizados.
list.files(pattern = "\\.csv$") 

str(matrices_estandarizadas)

getwd()

# Verifica las primeras filas de las matrices estandarizadas
head(matrices_estandarizadas[[1]])

# Revisa la estructura de cada matriz
sapply(matrices_estandarizadas, dim)

# Obtener los nombres de las especies de abejas (columnas)
especies_abejas <- colnames(dfchaco0611)

# Ver el listado
print(especies_abejas)

# Guardar en un archivo CSV
write.csv(data.frame(Especie = especies_abejas),
          file = "especies_abejaschaco0611.csv",
          row.names = FALSE)
getwd()



