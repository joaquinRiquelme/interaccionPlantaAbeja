library(ordinal)
library(tidyverse)
library(ggeffects)

set.seed(123)

datos_insectos <- read.csv("../02salidas/datos.analisis.categorizado.csv")
datos_insectos <- subset(datos_insectos, !is.na(DIT.final))
head(datos_insectos)
summary(datos_insectos)

# Categorias en funcion del cuantil de Z_Grado 
datos_insectos$clase_especializacion2 <- ntile(datos_insectos$Z_Grado, n=2)
datos_insectos$clase_especializacion3 <- ntile(datos_insectos$Z_Grado, n=3)
datos_insectos$clase_especializacion4 <- ntile(datos_insectos$Z_Grado, n=4)
datos_insectos$clase_especializacion5 <- ntile(datos_insectos$Z_Grado, n=5)
datos_insectos$clase_especializacion6 <- ntile(datos_insectos$Z_Grado, n=6)
datos_insectos$clase_especializacion7 <- ntile(datos_insectos$Z_Grado, n=7)
datos_insectos$clase_especializacion8 <- ntile(datos_insectos$Z_Grado, n=8)



# 1. Crear datos simulados de ejealias()# 1. Crear datos simulados de ejemplo
datos_ajuste<- unique(data.frame(
  ID = factor(datos_insectos$ID),
  DIT = datos_insectos$DIT.final,
  Esp= datos_insectos$sp.armonizado.x,
  Z_Grado = datos_insectos$Z_Grado,
  GGT2 = factor(datos_insectos$clase_especializacion2),
  GGT3 = factor(datos_insectos$clase_especializacion3),
  GGT4 = factor(datos_insectos$clase_especializacion4),
  GGT5 = factor(datos_insectos$clase_especializacion5),
  GGT6 = factor(datos_insectos$clase_especializacion6),
  GGT7 = factor(datos_insectos$clase_especializacion7),
  GGT8 = factor(datos_insectos$clase_especializacion8)))

png("../03figuras/EstadisticaDescriptiva/Distribucion_DIT.png")
par(mfrow=c(2,1))
hist(datos_ajuste$DIT, xlim=c(0,9), xlab="", las=1, main="Distribución de DIT")
boxplot(datos_ajuste$DIT, horizontal = TRUE, ylim=c(0,9),xlab="DIT (mm)")
dev.off()

sink("../02salidas/Est_DIT.txt")
summary(datos_ajuste$DIT)
sink()

png("../03figuras/EstadisticaDescriptiva/Distribucion_Z_Grado.png")
par(mfrow=c(2,1))
hist(datos_ajuste$Z_Grado, xlim=c(-2,11), xlab="", las=1, main="Distribución de Z Grado")
boxplot(datos_ajuste$Z_Grado, horizontal = TRUE, ylim=c(-2,11),xlab="Z Grado")
dev.off()

sink("../02salidas/Est_Z_Grado.txt")
summary(datos_ajuste$Z_Grado)
sink()


head(datos_ajuste)
summary(datos_ajuste)

# 2. Ajustar el modelo ordinal mixto
# Evaluamos el efecto del Tratamiento (fijo) controlando por Paciente (aleatorio)
modelo_abejas2 <- clmm(GGT2 ~ DIT + (1 | ID) +  (1 | Esp), data = datos_ajuste)
modelo_abejas3 <- clmm(GGT3 ~ DIT + (1 | ID) +  (1 | Esp), data = datos_ajuste)
modelo_abejas4 <- clmm(GGT4 ~ DIT + (1 | ID) +  (1 | Esp), data = datos_ajuste)
modelo_abejas5 <- clmm(GGT5 ~ DIT + (1 | ID) +  (1 | Esp), data = datos_ajuste)
modelo_abejas6 <- clmm(GGT6 ~ DIT + (1 | ID) +  (1 | Esp), data = datos_ajuste)
modelo_abejas7 <- clmm(GGT7 ~ DIT + (1 | ID) +  (1 | Esp), data = datos_ajuste)
modelo_abejas8 <- clmm(GGT8 ~ DIT + (1 | ID) +  (1 | Esp), data = datos_ajuste)


sink("../02salidas/resultadosOrdinalMixto2.txt")
summary(modelo_abejas2)
tabla_coef2 <- coef(summary(modelo_abejas2))
print(tabla_coef2)
sink()

AIC(modelo_abejas3)
AIC(modelo_abejas4)
AIC(modelo_abejas5)
AIC(modelo_abejas6)
AIC(modelo_abejas7)
AIC(modelo_abejas8)

tabla_coef3 <- coef(summary(modelo_abejas3))
tabla_coef4 <- coef(summary(modelo_abejas4))
tabla_coef5 <- coef(summary(modelo_abejas5))
tabla_coef6 <- coef(summary(modelo_abejas6))
tabla_coef7 <- coef(summary(modelo_abejas7))
tabla_coef8 <- coef(summary(modelo_abejas8))


# 3. Ver los resultados del modelo
print(summary(modelo_abejas))

tabla_coef <- coef(summary(modelo_abejas))
tabla_coef

# Predicciones marginales en función de la cobertura y el tipo de manejo
pred_prob <- ggpredict(
  modelo_abejas2, 
  
  terms = c("DIT [all]")
)


table(datos_ajuste$GGT2)

# Gráfico de líneas por nivel de respuesta (Categoría Ordinal)
ggplot(pred_prob,aes(x = x, y = predicted, color = response.level)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = response.level),
    alpha = 0.15, 
    color = NA
  ) +
  facet_wrap(~ group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_viridis_d(name = "Categoría de Generalización trófica", option = "D") +
  scale_fill_viridis_d(name = "Categoría de Generalización trófica", option = "D") +
  labs(
    title = "Comportamiento del Modelo Logístico Ordinal",
    subtitle = "Probabilidad predicha de cada categoría de grado de Generalización trófica",
    x = "DIT (mm)",
    y = "Probabilidad Predicha"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", strip.face = "bold")

# 1. Instalar y cargar librerías necesarias
# install.packages(c("DHARMa", "ggplot2", "patchwork"))
library(DHARMa)
library(ggplot2)
library(patchwork)

# 2. Simular los residuos a partir de tu modelo clmm
# (Se recomiendan al menos 250 o 500 simulaciones para estabilidad)
residuos_abejas <- simulateResiduals(fittedModel = modelo_abejas, n = 250)

# 3. Graficar el diagnóstico general automático
# Esto genera un QQ-plot de residuos y un gráfico de Residuos vs. Predichos
plot(residuos_abejas)





# 1. Cargar librerías necesarias
library(lme4)
library(ggplot2)
library(Matrix)
library(patchwork) # Para organizar los gráficos en un panel

# 2. Preparar datos y ajustar el modelo
set.seed(123)
data(iris)

# iris$Parcela <- rep(1:15, each = 10)

head(iris)

iris$Y <- iris$Petal.Length
# iris$Y <- scale(iris$Petal.Length)
iris$X <- iris$Petal.Width
# iris$X <- scale(iris$Petal.Width)

modelo <- lme4::lmer(Y ~ X + (1 | Species), data = iris)

plot(modelo)



# 2. Agregar los valores predichos por el modelo a los datos originales
iris$Predichos <- predict(modelo)
head(iris$Predichos)

iris$error <- iris$Y- iris$Predichos

xyplot(error~Y, groups=Species, data=iris)

# 3.Crear el gráfico por paneles con ggplot2
ggplot(iris, aes(x = X)) +
  # Puntos observados (datos reales)
  geom_point(aes(y = Y), color = "black", alpha = 0.6, size = 1.5) +
  # Línea de valores predichos por el modelo mixto
  geom_line(aes(y = Predichos), color = "#e74c3c", linewidth = 1) +
  # Crear un panel independiente para cada parcela (matriz de 3x5)
  facet_wrap(~ Species, nrow = 3) +
  # Formato visual limpio
  labs(title = "Valores Predichos vs. Ancho del Sépalo por Parcela",
       subtitle = "Líneas rojas: Predicción del modelo | Puntos negros: Datos reales",
       x = "Ancho del Sépalo (Sepal.Width)",
       y = "Largo del Sépalo (Sepal.Length)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "#ecf0f1"),
        strip.text = element_text(face = "bold"))

# 3. Extraer valores ajustados y residuos
datos_diag <- data.frame(
  Ajustados = fitted(modelo),
  Residuos = residuals(modelo)
)

# 4. Gráfico 1: Residuos vs. Valores Ajustados (Homocedasticidad)
g1 <- ggplot(datos_diag, aes(x = Ajustados, y = Residuos)) +
  geom_point(color = "#3498db", alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", color = "darkblue", se = FALSE, formula = y ~ x) +
  labs(title = "Residuos vs. Valores Ajustados",
       x = "Valores Ajustados (Predichos)",
       y = "Residuos") +
  theme_minimal()

# 5. Gráfico 2: QQ-Plot (Normalidad de los residuos)
g2 <- ggplot(datos_diag, aes(sample = Residuos)) +
  stat_qq(color = "#2ecc71", alpha = 0.7) +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(title = "Gráfico Q-Q Normal",
       x = "Cuantiles Teóricos",
       y = "Cuantiles de los Residuos") +
  theme_minimal()

# 6. Mostrar ambos gráficos juntos
g1 + g2

api <- data.frame(Fam="Apidae", DIT=rnorm(n = 100,  mean = 2.66, sd=1.83))
and <- data.frame(Fam="Andrenidae", DIT=rnorm(n = 100,  mean = 2.96, sd=0.60))
meg <- data.frame(Fam="Megachilidae", DIT=rnorm(n = 100,  mean = 2.7, sd=0.92))
cole <- data.frame(Fam="Colletidae", DIT=rnorm(n = 100,  mean = 1.82, sd=0.88))
ali <- data.frame(Fam="Alictidae", DIT=rnorm(n = 100,  mean = 1.74, sd=0.55))

abejas <- rbind(api,and,meg,cole,ali)

histogram(~DIT|Fam, abejas, layout=c(1,5), as.table=TRUE)

