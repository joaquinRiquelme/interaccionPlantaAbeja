# Cargar la matriz de interacciones
matriz <- read.csv("C:/Users/kcoll/Desktop/Tesis -Cap 1/1.0 TESIS/1. Capitulo 1 -redes/Bases de Datos - originales - Redes agregadas/059_Bezerra et al_2009/M_PL_059.csv", row.names = 1)

# Convertir horas a minutos
total_minutos <- 1392 * 60

# Calcular la tasa por minuto (matriz con las mismas dimensiones)
tasa_por_minuto <- matriz / total_minutos

# Ver resultados
head(tasa_por_minuto)

# (Opcional) Guardar el resultado
write.csv(tasa_por_minuto, "tasa_VISITA_Bezerra_2009.csv")
getwd()
