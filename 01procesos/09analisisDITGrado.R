# Analisis de G vs DIT
# Variaciones de variables
# ==============================================================================
# SCRIPT DE REFERENCIA: MODELADO DE VARIABLES CONTINUAS Y CATEGÓRICAS
# Contexto: Tamaño de Insectos vs. Grado de Generalización de Dieta
# ==============================================================================

# 0. Cargar librerías necesarias
# install.packages(c("ranger", "mgcv", "dplyr"))
library(ranger) # Para Random Forest
library(mgcv)   # Para Modelos Aditivos Generalizados (GAM)
library(dplyr)  # Para manipulación de datos
library(ggplot2)
library(ggpubr) # Para agregar p-values fácilmente
library(moments)

# 1. Simulación de Datos de Ejemplo
set.seed(42) # Para reproducibilidad

# Generamos 200 observaciones simulando una comunidad de insectos
n_obs <- 200


datos_insectos <- read.csv("../02salidas/datos.analisis.categorizado.csv")
datos_insectos <- subset(datos_insectos, !is.na(DIT.final))
head(datos_insectos)

# Clasificamos el tamaño
datos_insectos$tamano_mm <- datos_insectos$DIT.final

install.packages("moments")
skewness(datos_insectos$Z_Grado) 
skewness(datos_insectos$Grado.relativo) 
skewness(datos_insectos$Grado) 


# Respuestas (Y)
# datos_insectos$grado_num <- datos_insectos$Z_Grado
# datos_insectos$grado_num <- datos_insectos$GFE
datos_insectos$grado_num <- datos_insectos$Grado
# datos_insectos$grado_num <- datos_insectos$Grado
datos_insectos <- subset(datos_insectos, !is.na(grado_num))


# ==============================================================================
# ANÁLISIS 1: X Categórica -> Y Continua
# (Ej: Grado de generalización (índice) entre clases de tamaño)
# ==============================================================================

# Anova ----
if(n.categorias.dit!=1 & n.categorias.grado ==1){

  
print("--- 1A. ANOVA Clásico ---")
modelo_anova <- aov(grado_num ~ tamano_clase, data = datos_insectos)
summary(modelo_anova)


# 1. Cargar librerías necesarias

# 4. Visualizar con ggplot2 y ggpubr
png("Anova.png")
ggplot(datos_insectos, aes(x = tamano_clase, y = grado_num, fill = tamano_clase)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  # Añade comparaciones de Tukey (o wilcox/t.test)
  stat_compare_means(method = "anova", label.y = 10) + # P-value global
  stat_compare_means(comparisons = list(c("1.Pequeño", "2.Mediano"), c("2.Mediano", "3.Grande"), c("1.Pequeño", "3.Grande")), 
                     label = "p.signif") + # P-values por pares
  labs(
    title = "Grado de Generalización por Clase de Tamaño",
    x = "Clase de Tamaño del Insecto",
    y = "Índice de Generalización Z score",
    subtitle = "Comparación de distribuciones (Categórica vs Continua)") +
  theme(legend.position = "none") # Ocultamos la leyenda porque el eje X ya lo explica

dev.off()
# Prueba post-hoc si hay diferencias significativas
TukeyHSD(modelo_anova)

}

# Regresion ----  
if(n.categorias.dit==1 & n.categorias.grado ==1){

  

# ==============================================================================
# ANÁLISIS 2: X Continua -> Y Continua
# (Ej: Grado de generalización (índice) en función del tamaño en mm)
# ==============================================================================

print("--- 2A. Regresión Lineal Simple ---")
modelo_lm <- lm(grado_num ~ tamano_mm, data = datos_insectos)
summary(modelo_lm)
plot(modelo_lm)

print("--- 2B. Modelos Aditivos Generalizados (GAM) ---")
# Ideal si sospechamos que la generalización hace una asíntota al llegar a cierto tamaño
modelo_gam <- gam(grado_num ~ s(tamano_mm), data = datos_insectos, method = "REML")
summary(modelo_gam)
plot(grado_num~tamano_mm, datos_insectos)
abline(modelo_lm, col="red")
# plot(modelo_gam, pages = 1) # Para ver la curva de respuesta

# ==============================================================================
# GRÁFICO 2: Continua vs Continua (Regresión / GAM)
# Tipo: Gráfico de Dispersión con Línea de Tendencia
# ==============================================================================
# Se usa geom_smooth() para añadir la curva. Usamos "lm" para regresión lineal 
# o "gam" para relaciones no lineales.

png("RegresionGAM.png")
plot_regresion <- ggplot(datos_insectos, aes(x = tamano_mm, y = grado_num)) +
  geom_point(alpha = 0.5, color = "#2c3e50") + # Puntos de datos
  geom_smooth(method = "gam", color = "#e74c3c", fill = "#e74c3c", alpha = 0.2) + # Curva ajustada
  theme_minimal() +
  labs(
    title = "Relación entre Tamaño y Grado de Generalización",
    subtitle = "Tendencia continua con modelo GAM (Continua vs Continua)",
    x = "Tamaño (mm)",
    y = "Índice de Generalización Z score"
  )

print(plot_regresion)
dev.off()


# Regresion logistica ----
if(n.categorias.dit==1 & n.categorias.grado !=1){
# ==============================================================================
# ANÁLISIS 3: X Continua -> Y Categórica
# (Ej: Probabilidad de ser Generalista según el tamaño en mm)
# ==============================================================================

print("--- 3. Regresión Logística ---")
# IMPORTANTE: glm() toma el segundo nivel del factor alfanuméricamente como el "éxito" (1).
# Levels son "Especialista", "Generalista", por lo que modelará la prob de "Generalista".

modelo_logistico <- glm(clase_especializacion ~ tamano_mm, data = datos_insectos, family = "binomial")
summary(modelo_logistico)
# ==============================================================================
# GRÁFICO 3: Continua vs Categórica (Regresión Logística)
# Tipo: Curva Logística sobre Datos Binarios
# ==============================================================================
# Para ggplot2, necesitamos que la variable categórica sea numérica (0 y 1).
# Creamos una variable temporal para el gráfico: 1 = Generalista, 0 = Especialista.

datos_insectos$clase_especializacion_binaria <- ifelse(datos_insectos$clase_especializacion == "2.Generalista", 1, 0)


library(lme4)
# Ajuste del modelo
modelo_mixto <- glmer(
  clase_especializacion_binaria ~ tamano_mm + (1 | ID), 
  data = datos_geo, 
  family = binomial(link = "logit")
)
# Ver resultados
summary(modelo_mixto)

modelo_mixto2 <- glmer(
  clase_especializacion_binaria ~ tamano_mm + clase_latitud + (1 | ID), 
    data = datos_geo, 
  family = binomial(link = "logit")
)
summary(modelo_mixto2)

anova(modelo_mixto, modelo_mixto2, test = "Chisq")

# 1. Cargar librerías
library(effects)
library(ggplot2)

# 2. Extraer los efectos de la variable 'tamano_mm'
# Esto calcula las probabilidades y los intervalos de confianza
efectos_tamano <- as.data.frame(effect("tamano_mm", modelo_mixto))

# 3. Crear el gráfico con ggplot2
ggplot(efectos_tamano, aes(x = tamano_mm, y = fit)) +
  # Añadimos la banda de confianza
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  # Añadimos la línea de la curva logística
  geom_line(color = "darkblue", size = 1.2) +
  # Opcional: Añadir los datos originales (alfombra de datos)
  geom_rug(data = datos_insectos, aes(y = clase_especializacion_binaria), 
           sides = "b", alpha = 0.3) +
  # Etiquetas y formato
  labs(
    title = "Efecto del Tamaño en la Especialización",
    subtitle = "Modelo logístico de efectos mixtos (ID como intercepto aleatorio)",
    x = "Tamaño (mm)",
    y = "Probabilidad de Especialización"
  ) +
  scale_y_continuous(labels = scales::percent) + # Muestra eje Y en porcentaje
  theme_bw()


# 1. Cargar librerías
library(emmeans)
library(ggplot2)
library(dplyr)

# 2. Generar una malla de datos para las predicciones
# Creamos un rango de tamaño (tamano_mm) para cada ID
grid <- expand.grid(
  tamano_mm = seq(min(datos_insectos$tamano_mm), max(datos_insectos$tamano_mm), length.out = 20),
  ID = unique(datos_insectos$ID)
)

head(grid)

rangos <- datos_insectos |> group_by(ID) |> summarise(t.min=min(tamano_mm,na.rm = TRUE),
                                                      t.max=max(tamano_mm,na.rm = TRUE))

head(rangos)

grid_rangos <- merge(grid, rangos)
grid_rangos$delta.1 <- grid_rangos$tamano_mm- grid_rangos$t.min
grid_rangos$delta.2 <- grid_rangos$tamano_mm- grid_rangos$t.max
head(grid_rangos)

grid_rangos <- subset(grid_rangos, delta.1>=0 & delta.2 <=0)
# 3. Calcular las predicciones incluyendo el efecto aleatorio (re.form = NULL)
# Usamos type = "response" para obtener probabilidades (0 a 1)
grid <- grid_rangos
grid$probabilidad <- predict(modelo_mixto, newdata = grid, type = "response")
head(grid)

grid2 <- merge(grid, unique(datos_geo[,c("ID","clase_latitud")], by="ID"))
grid2 <- grid2[,c("clase_latitud","ID","tamano_mm")]
head(grid2)
grid2$probabilidad <- predict(modelo_mixto2, newdata = grid2, type = "response")
head(grid2)
grid2 <- grid2[order(grid2$clase_latitud, grid2$ID, grid2$tamano_mm),]
grid2$color <- 1
grid2$color[grid2$clase_latitud=="2.Media"] <- 2
head(grid2)

# 4. Calcular también la tendencia global (Efecto fijo puro)
grid_global <- data.frame(
  tamano_mm = seq(min(datos_insectos$tamano_mm), max(datos_insectos$tamano_mm), length.out = 100)
)
grid_global$probabilidad <- predict(modelo_mixto, newdata = grid_global, re.form = NA, type = "response")


grid_global2 <- data.frame(
  clase_latitud=c("1.Baja","2.Media"),
  tamano_mm = seq(min(datos_geo$tamano_mm), max(datos_geo$tamano_mm), length.out = 100)
)
grid_global2$probabilidad <- predict(modelo_mixto2, newdata = grid_global2, re.form = NA, type = "response")
grid_global2 <- grid_global2[order(grid_global2$clase_latitud),]

# 5. Graficar

png("Probabilidad_GeneralistaPorID.png")
ggplot() +
  # Líneas finas para cada ID (Efectos aleatorios)
  geom_line(data = grid, aes(x = tamano_mm, y = probabilidad, group = ID), 
            color = "gray", alpha = 0.4) +
  # Línea gruesa para la tendencia promedio (Efecto fijo)
  geom_line(data = grid_global, aes(x = tamano_mm, y = probabilidad), 
            color = "firebrick", size = 1.5) +
  labs(
    title = "Probabilidad de ser Generalista por ID",
    subtitle = "Gris: Efectos individuales por ID | Rojo: Tendencia global estimada",
    x = "Tamaño (mm)",
    y = "Probabilidad"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal()

dev.off()

# Prob por clase
library(ggeffects)
library(ggplot2)

# 1. Calcular las predicciones para la combinación de tamaño y latitud
# El modelo debe ser: clase ~ tamano_mm + latitud_clase + (1 | ID)
pred_latitud <- ggpredict(modelo_mixto2, terms = c("tamano_mm [all]", "clase_latitud"))

# 2. Graficar
png("../03figuras/ProbabilidadPorLatitud.png")
ggplot(pred_latitud, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  labs(
    title = "Probabilidad de ser Generalista por Latitud",
    x = "Tamaño (mm)",
    y = "Probabilidad Predicha",
    color = "Clase de Latitud",
    fill = "Clase de Latitud"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  theme_minimal()
dev.off()

png("Probabilidad_GeneralistaPorClaseLatitud.png")
ggplot() +
  # Líneas finas para cada ID (Efectos aleatorios)
  geom_line(data = grid2, aes(x = tamano_mm, y = probabilidad, group = clase_latitud), 
            color = grid2$color, alpha = 0.4) +
  # Línea gruesa para la tendencia promedio (Efecto fijo)
  geom_line(data = grid_global2, aes(x = tamano_mm, y = probabilidad), 
            color = "firebrick", size = 1.5) +
  labs(
    title = "Probabilidad de ser Generalista por ID",
    subtitle = "Gris: Efectos individuales por ID | Rojo: Tendencia global estimada",
    x = "Tamaño (mm)",
    y = "Probabilidad"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal()

dev.off()


# 
plot_logistica <- ggplot(datos_insectos, aes(x = tamano_mm, y = clase_especializacion_binaria)) +
  geom_point(alpha = 0.4, size = 2, aes(color = clase_especializacion)) + # Puntos agrupados en 0 y 1
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "black") + # Curva logística
  theme_minimal() +
  labs(
    title = "Probabilidad de tener una Dieta Generalista",
    subtitle = "Curva de regresión logística (Continua vs Categórica)",
    x = "Tamaño (mm)",
    y = "Probabilidad de ser Generalista (1 = Sí, 0 = No)"
  ) +
  scale_y_continuous(breaks = c(0, 0.5, 1))

png("Probabilidad_Generalista.png")
print(plot_logistica)
dev.off()

histogram(~tamano_mm|clase_especializacion, datos_insectos, layout=c(1,2))
table(datos_insectos$clase_especializacion)

# 
library(MASS)

# Ajustar modelo ordinal
datos_insectos$clase_especializacion <- as.factor(datos_insectos$clase_especializacion)
modelo_ord <- polr(clase_especializacion ~ tamano_mm, data = datos_insectos, Hess = TRUE)

# Ver resumen
summary(modelo_ord)

# Calcular p-values
tabla_coef <- coef(summary(modelo_ord))
p <- pnorm(abs(tabla_coef[, "t value"]), lower.tail = FALSE) * 2
tabla_final <- cbind(tabla_coef, "p value" = p)
print(tabla_final)

# Crear datos para predecir
nuevos_datos <- data.frame(
  tamano_mm = seq(min(datos_insectos$tamano_mm), max(datos_insectos$tamano_mm), length.out = 100))

# Predecir probabilidades
predicciones <- cbind(nuevos_datos, predict(modelo_ord, nuevos_datos, type = "probs"))

# Formatear para ggplot (formato largo)
library(tidyr)
pred_long <- pivot_longer(predicciones, cols = c("1.Especialista", "2.Generalista"), 
                          names_to = "Nivel", values_to = "Probabilidad")
pred_long$Nivel <- factor(pred_long$Nivel, levels = c("1.Especialista", "2.Generalista"))

# Graficar


png("LogisticoOrdinal4clases.png")
ggplot(pred_long, aes(x = tamano_mm, y = Probabilidad, color = Nivel)) +
  geom_line(size = 1) +
  labs(title = "Probabilidades Predichas por Nivel de Satisfacción",
       x = "Tamaño mm", y = "Probabilidad") +
  theme_minimal()
dev.off()

png("LogisticoOrdinal3clases.png")
ggplot(pred_long, aes(x = tamano_mm, y = Probabilidad, color = Nivel)) +
  geom_line(size = 1) +
  labs(title = "Probabilidades Predichas por Nivel de Satisfacción",
       x = "Tamaño mm", y = "Probabilidad") +
  theme_minimal()
dev.off()


# Contingencia ----
if(n.categorias.dit!=1 & n.categorias.grado !=1){
# ==============================================================================
# ANÁLISIS 4: X Categórica -> Y Categórica
# (Ej: Asociación entre Clase de Tamaño y Tipo de Dieta)
# ==============================================================================

print("--- 4. Prueba de Chi-cuadrado ---")
tabla_contingencia <- table(datos_insectos$tamano_clase, datos_insectos$clase_especializacion)
print(tabla_contingencia)

prueba_chi <- chisq.test(tabla_contingencia)
print(prueba_chi)

# Cargar la librería principal para gráficos
library(ggplot2)

# ==============================================================================
# GRÁFICO 4: Categórica vs Categórica (Chi-cuadrado)
# Tipo: Gráfico de Barras Apiladas al 100%
# ==============================================================================
# position="fill" estandariza las barras al 100%, lo que permite comparar 
# proporciones de forma directa, independientemente del tamaño muestral de cada clase.

plot_chi <- ggplot(datos_insectos, aes(x = tamano_clase, fill = clase_especializacion)) +
  geom_bar(position = "fill", alpha = 0.8) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) + # Muestra el eje Y como porcentajes
  labs(
    title = "Proporción de Clase de generalización por Clase de Tamaño",
    subtitle = "Asociación de proporciones (Categórica vs Categórica)",
    x = "Clase de Tamaño del Insecto",
    y = "Porcentaje de Individuos",
    fill = "Clase de generalización"
  )

png("ProprcionPorClase.png")
print(plot_chi)
dev.off()




# ==============================================================================
# ANÁLISIS 4: X Categórica -> Y Categórica
# (Ej: Asociación entre Clase de Tamaño y Tipo de Dieta)
# ==============================================================================

print("--- 4. Prueba de Chi-cuadrado ---")

lat.baja <- subset(datos_geo, clase_latitud=="1.Baja")
tabla_contingencia1 <- table(lat.baja$tamano_clase, lat.baja$clase_especializacion)
print(tabla_contingencia1)

prueba_chi1 <- chisq.test(tabla_contingencia1)
print(prueba_chi1)


lat.media <- subset(datos_geo, clase_latitud=="2.Media")
tabla_contingencia2 <- table(lat.media$tamano_clase, lat.media$clase_especializacion)
print(tabla_contingencia2)

prueba_chi2 <- chisq.test(tabla_contingencia2)
print(prueba_chi2)

lat.alta <- subset(datos_geo, clase_latitud=="3.Alta")
tabla_contingencia3 <- table(lat.alta$tamano_clase, lat.alta$clase_especializacion)
print(tabla_contingencia3)

prueba_chi3 <- chisq.test(tabla_contingencia3)
print(prueba_chi3)

# Cargar la librería principal para gráficos
library(ggplot2)

# ==============================================================================
# GRÁFICO 4: Categórica vs Categórica (Chi-cuadrado)
# Tipo: Gráfico de Barras Apiladas al 100%
# ==============================================================================
# position="fill" estandariza las barras al 100%, lo que permite comparar 
# proporciones de forma directa, independientemente del tamaño muestral de cada clase.

plot_chi1 <- ggplot(lat.baja, aes(x = tamano_clase, fill = clase_especializacion)) +
  geom_bar(position = "fill", alpha = 0.8) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) + # Muestra el eje Y como porcentajes
  labs(
    title = "Proporción de Tipo de Dieta por Clase de Tamaño Latitud Baja",
    subtitle = "Asociación de proporciones (Categórica vs Categórica)",
    x = "Clase de Tamaño del Insecto",
    y = "Porcentaje de Individuos",
    fill = "Tipo de Dieta"
  )

print(plot_chi1)

plot_chi2 <- ggplot(lat.media, aes(x = tamano_clase, fill = clase_especializacion)) +
  geom_bar(position = "fill", alpha = 0.8) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) + # Muestra el eje Y como porcentajes
  labs(
    title = "Proporción de Tipo de Dieta por Clase de Tamaño Latitud Media",
    subtitle = "Asociación de proporciones (Categórica vs Categórica)",
    x = "Clase de Tamaño del Insecto",
    y = "Porcentaje de Individuos",
    fill = "Tipo de Dieta"
  )

print(plot_chi2)

plot_chi3 <- ggplot(lat.alta, aes(x = tamano_clase, fill = clase_especializacion)) +
  geom_bar(position = "fill", alpha = 0.8) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) + # Muestra el eje Y como porcentajes
  labs(
    title = "Proporción de Tipo de Dieta por Clase de Tamaño Latitud Alta",
    subtitle = "Asociación de proporciones (Categórica vs Categórica)",
    x = "Clase de Tamaño del Insecto",
    y = "Porcentaje de Individuos",
    fill = "Tipo de Dieta"
  )

print(plot_chi3)



plot(abs(Latitud)~tamano_mm, datos_geo)
     