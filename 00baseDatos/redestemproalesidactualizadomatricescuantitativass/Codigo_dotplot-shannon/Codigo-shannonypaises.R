# Leer el archivo con los índices de Shannon
df <- read.csv("shannon_resultados.csv")

# Crear un vector de países (uno por fila del archivo)
paises <- c("Chile", "Argentina", "Chile", "Brasil", "Perú")  # <--- ajusta esto según tus datos

# Verifica que el largo coincida
length(paises) == nrow(df)  # Esto debe dar TRUE

# Agregar la columna de países
df$Pais <- paises

# Verificar
head(df)

# Guardar el archivo con la nueva columna
write.csv(df, "shannon_con_paises.csv", row.names = FALSE)
