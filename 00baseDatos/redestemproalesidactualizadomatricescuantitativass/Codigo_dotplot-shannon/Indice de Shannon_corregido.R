# ---------------------------------------------------------------------------------
# 📦 Cargar librerías necesarias
# ---------------------------------------------------------------------------------
library(dplyr)   # Para manipulación de datos
library(vegan)   # Para calcular el índice de Shannon


## 1: Cargar y procesar los datos
# Leer los datos desde un archivo CSV (usa el tuyo si lo estás leyendo así)
df <- chaco0611_unida_5

# Convertir la primera columna en nombres de fila
rownames(df) <- df[,1]
df <- df[,-1]  # Eliminar la primera columna ahora que está en rownames


# Suma vertical: total de interacciones por abeja
suma_vertical <- colSums(df, na.rm = TRUE)
print(suma_vertical)

# Suma horizontal: total de interacciones por planta
suma_horizontal <- rowSums(df, na.rm = TRUE)
print(suma_horizontal)
# Agregar la suma horizontal al dataframe
df$suma_horizontal <- suma_horizontal

# Agregar una fila con la suma vertical
df <- rbind(df, c(suma_vertical, sum(suma_horizontal)))
rownames(df)[nrow(df)] <- "suma_vertical"


### 3: Calcular proporción horizontal (por planta)
df_sin_total <- df[-nrow(df), ]  # Quitar fila "suma_vertical"
suma_horizontal <- as.numeric(df_sin_total$suma_horizontal)
suma_horizontal[suma_horizontal == 0] <- NA  # Evitar división por cero

df_horizontal <- df_sin_total
df_horizontal[,-ncol(df)] <- sweep(as.matrix(df_sin_total[,-ncol(df_sin_total)]), 1, suma_horizontal, "/")

###OJO: Mostrar proporciones Horizonatales en consola (por plantA)
print(round(df_horizontal, 3))
# Guardar como CSV
write.csv(df_horizontal, "proporciones_horizontaleschaco0611.csv", row.names = TRUE)
getwd()

### 4: Calcular proporción vertical (por abeja)
df_sin_total <- df[, -ncol(df)]  # Quitar columna "suma_horizontal"
suma_vertical_vector <- as.numeric(df[nrow(df), -ncol(df)])  # Quitar columna de suma horizontal

df_vertical <- df_sin_total
df_vertical[,] <- sweep(as.matrix(df_sin_total), 2, suma_vertical_vector, "/")

###OJO: Mostrar proporciones verticales en consola (por abeja)
print(round(df_vertical, 3))
# Guardar como CSV
write.csv(df_vertical, "proporciones_verticales11.1.csv", row.names = TRUE)
getwd()

## 5: Calcular Índice de Shannon
# Quitar fila y columna de totales
df_sin_totales <- df[-nrow(df), -ncol(df)]

# Convertir a numérico sin perder nombres de fila
df_sin_totales <- as.data.frame(lapply(df_sin_totales, as.numeric))
rownames(df_sin_totales) <- rownames(df)[-nrow(df)]  # Restaurar nombres de planta

# Calcular índice de Shannon global
shannon_global <- diversity(as.vector(as.matrix(df_sin_totales)), index = "shannon")

# Shannon por columna (abejas)
shannon_por_columna <- diversity(df_sin_totales, index = "shannon", MARGIN = 2)

# Shannon por fila (plantas)
shannon_por_fila <- diversity(df_sin_totales, index = "shannon", MARGIN = 1)

print(shannon_por_fila)
print (shannon_por_columna)
print (shannon_global)
# ---------------------------------------------------------------------------------
# Paso 6: Crear y guardar los dataframes con los índices de Shannon

# Global
df_shannon_global <- data.frame(Métrica = "Shannon Global", Valor = shannon_global)

# Por columna
df_shannon_columnas <- data.frame(Especie_Abeja = names(shannon_por_columna),
                                  Shannon_Valor = shannon_por_columna)
# Por fila
df_shannon_filas <- data.frame(Especie_Planta = names(shannon_por_fila),
                               Shannon_Valor = shannon_por_fila)


# Mostrar resultados
print("Índice de Shannon Global:")
print(df_shannon_global)

print("Índice de Shannon por columna:")
print(df_shannon_columnas)

print("Índice de Shannon por fila:")
print(df_shannon_filas)

# Guardar resultados
write.csv(df_shannon_global, "shannon_globalchaco0611.csv", row.names = FALSE)
write.csv(df_shannon_columnas, "shannon_por_columnachaco0611.csv", row.names = FALSE)
write.csv(df_shannon_filas, "shannon_por_filachaco0611.csv", row.names = FALSE)
getwd()



### RESUMEN
library(vegan)

# 1. Shannon por columna (abeja)
shannon_por_columna <- diversity(df_sin_totales, index = "shannon", MARGIN = 2)

# 2. Grado por especie (número de interacciones no cero por columna)
grado_por_especie <- colSums(df_sin_totales > 0)

# 3. Proporción total de interacciones por especie (normalizado por el total)
interacciones_totales <- colSums(df_sin_totales)
proporcion_interacciones <- df_vertical
  
# 4. Crear tabla resumen
tabla_resumen <- data.frame(
  Shannon = round(shannon_por_columna, 3),
  Grado = grado_por_especie,
  Proporcion = round(proporcion_interacciones, 3)
)

# 5. Guardar como CSV
write.csv(tabla_resumen, "resumen_abejas11.1.csv", row.names = TRUE)
getwd()

# 6. Mostrar en consola
print(tabla_resumen)


### PLANTAS
library(vegan)

# 1. Índice de Shannon por fila (planta)
shannon_por_fila <- diversity(df_sin_totales, index = "shannon", MARGIN = 1)

# 2. Grado por planta (número de abejas con las que interactúa)
grado_por_planta <- rowSums(df_sin_totales > 0)

# 3. Proporción más alta de interacción por planta
proporcion_por_fila <- apply(df_horizontal, 1, max, na.rm = TRUE)

# 4. Crear tabla resumen
tabla_resumen_plantas <- data.frame(
  Shannon = round(as.numeric(shannon_por_fila), 3),
  Grado = grado_por_planta,
  Proporcion = round(df_horizontal, 3),
  row.names = rownames(df_sin_totales)
)

# 5. Guardar como CSV
write.csv(tabla_resumen_plantas, "resumen_plantas32.csv", row.names = TRUE)

# 6. Mostrar en consola
print(tabla_resumen_plantas)



----------------------------------------------------
### Para guardar las sumas verticales u horizontales
  write.csv(df_vertical, "suma_vertical.csv")
getwd()
---------------------------------------------------
