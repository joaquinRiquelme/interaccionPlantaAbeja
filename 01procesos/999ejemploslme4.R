
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

