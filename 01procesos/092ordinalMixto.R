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



datos_ajuste2 <- unique(datos_ajuste[,c("DIT","Esp")])
png("../03figuras/EstadisticaDescriptiva/Distribucion_DIT.png")
par(mfrow=c(2,1))
boxplot(datos_ajuste2$DIT,  main="Distribución de DIT", horizontal = TRUE, ylim=c(0,9),xlab="", frame=FALSE)
hist(datos_ajuste2$DIT, xlim=c(0,9), xlab="DIT (mm)", las=1, main="", ylim=c(0,100))
abline(v=median(datos_ajuste2$DIT), col="red",lty=2,lwd=2)
text(x=median(datos_ajuste2$DIT)*1.5, y=95, labels = paste("Median =", round(median(datos_ajuste2$DIT),2)))
dev.off()

sink("../02salidas/Est_DIT.txt")
summary(datos_ajuste$DIT)
sink()

png("../03figuras/EstadisticaDescriptiva/Distribucion_Z_Grado.png")
par(mfrow=c(2,1))
boxplot(datos_ajuste$Z_Grado, horizontal = TRUE, ylim=c(-2,11),xlab="",main="Distribución de Z Grado", frame=FALSE)
hist(datos_ajuste$Z_Grado, xlim=c(-2,11),las=1, xlab="Z Grado",main="")
abline(v=median(datos_ajuste$Z_Grado, na.rm=TRUE), col="red",lty=2,lwd=2)
text(x=1.5, y=200, labels = paste("Median =", round(median(datos_ajuste$Z_Grado, na.rm = TRUE),2)))
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

# 3. Ver los resultados del modelo
print(summary(modelo_abejas2))

sink("../02salidas/resultadosOrdinalMixto2.txt")
summary(modelo_abejas2)
tabla_coef2 <- coef(summary(modelo_abejas2))
print(tabla_coef2)
sink()

sink("../02salidas/resultadosOrdinalMixto3.txt")
summary(modelo_abejas3)
tabla_coef3 <- coef(summary(modelo_abejas3))
print(tabla_coef3)
sink()

sink("../02salidas/resultadosOrdinalMixto4.txt")
summary(modelo_abejas4)
tabla_coef4 <- coef(summary(modelo_abejas4))
print(tabla_coef4)
sink()


sink("../02salidas/resultadosOrdinalMixto5.txt")
summary(modelo_abejas5)
tabla_coef5 <- coef(summary(modelo_abejas5))
print(tabla_coef5)
sink()

sink("../02salidas/resultadosOrdinalMixto6.txt")
summary(modelo_abejas6)
tabla_coef6 <- coef(summary(modelo_abejas6))
print(tabla_coef6)
sink()

sink("../02salidas/resultadosOrdinalMixto7.txt")
summary(modelo_abejas7)
tabla_coef7 <- coef(summary(modelo_abejas7))
print(tabla_coef7)
sink()

sink("../02salidas/resultadosOrdinalMixto8.txt")
summary(modelo_abejas8)
tabla_coef8 <- coef(summary(modelo_abejas8))
print(tabla_coef8)
sink()



# Predicciones marginales en función de la cobertura y el tipo de manejo
pred_prob2 <- ggpredict(
  modelo_abejas2, 
  terms = c("DIT [all]")
)

pred_prob3 <- ggpredict(
  modelo_abejas3, 
  terms = c("DIT [all]")
)

pred_prob4 <- ggpredict(
  modelo_abejas4, 
  terms = c("DIT [all]")
)

pred_prob5 <- ggpredict(
  modelo_abejas5, 
  terms = c("DIT [all]")
)

pred_prob6 <- ggpredict(
  modelo_abejas6, 
  terms = c("DIT [all]")
)

pred_prob7 <- ggpredict(
  modelo_abejas7, 
  terms = c("DIT [all]")
)

pred_prob8 <- ggpredict(
  modelo_abejas8, 
  terms = c("DIT [all]")
)


# Gráfico de líneas por nivel de respuesta (Categoría Ordinal)
p2 <- ggplot(pred_prob2,aes(x = x, y = predicted, color = response.level)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = response.level),
    alpha = 0.15, 
    color = NA
  ) +
  facet_wrap(~ group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  scale_fill_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  labs(
    title = "Comportamiento del Modelo Logístico Ordinal",
    subtitle = "Probabilidad predicha de cada categoría de GGT",
    x = "DIT (mm)",
    y = "Probabilidad predicha"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", strip.face = "bold")


ggsave("../03figuras/AnalisisGradoDIT/OrdinalMixto2.eps",
       plot = p2,
       device = cairo_ps,
       width = 16,
       height = 12,
       units = "cm")


# Gráfico de líneas por nivel de respuesta (Categoría Ordinal)
p3 <- ggplot(pred_prob3,aes(x = x, y = predicted, color = response.level)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = response.level),
    alpha = 0.15, 
    color = NA
  ) +
  facet_wrap(~ group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  scale_fill_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  labs(
    title = "Comportamiento del Modelo Logístico Ordinal",
    subtitle = "Probabilidad predicha de cada categoría de GGT",
    x = "DIT (mm)",
    y = "Probabilidad predicha"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", strip.face = "bold")


ggsave("../03figuras/AnalisisGradoDIT/OrdinalMixto3.eps",
       plot = p3,
       device = cairo_ps,
       width = 16,
       height = 12,
       units = "cm")

# Gráfico de líneas por nivel de respuesta (Categoría Ordinal)
p4 <- ggplot(pred_prob4,aes(x = x, y = predicted, color = response.level)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = response.level),
    alpha = 0.15, 
    color = NA
  ) +
  facet_wrap(~ group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  scale_fill_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  labs(
    title = "Comportamiento del Modelo Logístico Ordinal",
    subtitle = "Probabilidad predicha de cada categoría de GGT",
    x = "DIT (mm)",
    y = "Probabilidad predicha"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", strip.face = "bold")


ggsave("../03figuras/AnalisisGradoDIT/OrdinalMixto4.eps",
       plot = p4,
       device = cairo_ps,
       width = 16,
       height = 12,
       units = "cm")

# Gráfico de líneas por nivel de respuesta (Categoría Ordinal)
p5 <- ggplot(pred_prob5,aes(x = x, y = predicted, color = response.level)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = response.level),
    alpha = 0.15, 
    color = NA
  ) +
  facet_wrap(~ group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  scale_fill_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  labs(
    title = "Comportamiento del Modelo Logístico Ordinal",
    subtitle = "Probabilidad predicha de cada categoría de GGT",
    x = "DIT (mm)",
    y = "Probabilidad predicha"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", strip.face = "bold")


ggsave("../03figuras/AnalisisGradoDIT/OrdinalMixto5.eps",
       plot = p5,
       device = cairo_ps,
       width = 16,
       height = 12,
       units = "cm")

# Gráfico de líneas por nivel de respuesta (Categoría Ordinal)
p6 <- ggplot(pred_prob6,aes(x = x, y = predicted, color = response.level)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = response.level),
    alpha = 0.15, 
    color = NA
  ) +
  facet_wrap(~ group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  scale_fill_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  labs(
    title = "Comportamiento del Modelo Logístico Ordinal",
    subtitle = "Probabilidad predicha de cada categoría de GGT",
    x = "DIT (mm)",
    y = "Probabilidad predicha"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", strip.face = "bold")


ggsave("../03figuras/AnalisisGradoDIT/OrdinalMixto6.eps",
       plot = p6,
       device = cairo_ps,
       width = 16,
       height = 12,
       units = "cm")
# Gráfico de líneas por nivel de respuesta (Categoría Ordinal)
p7 <- ggplot(pred_prob7,aes(x = x, y = predicted, color = response.level)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = response.level),
    alpha = 0.15, 
    color = NA
  ) +
  facet_wrap(~ group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  scale_fill_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  labs(
    title = "Comportamiento del Modelo Logístico Ordinal",
    subtitle = "Probabilidad predicha de cada categoría de GGT",
    x = "DIT (mm)",
    y = "Probabilidad predicha"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", strip.face = "bold")


ggsave("../03figuras/AnalisisGradoDIT/OrdinalMixto7.eps",
       plot = p7,
       device = cairo_ps,
       width = 16,
       height = 12,
       units = "cm")

# Gráfico de líneas por nivel de respuesta (Categoría Ordinal)
p8 <- ggplot(pred_prob8,aes(x = x, y = predicted, color = response.level)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = response.level),
    alpha = 0.15, 
    color = NA
  ) +
  facet_wrap(~ group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  scale_fill_viridis_d(name = "Categoría de generalización trófica", option = "D") +
  labs(
    title = "Comportamiento del Modelo Logístico Ordinal",
    subtitle = "Probabilidad predicha de cada categoría de GGT",
    x = "DIT (mm)",
    y = "Probabilidad predicha"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", strip.face = "bold")


ggsave("../03figuras/AnalisisGradoDIT/OrdinalMixto8.eps",
       plot = p8,
       device = cairo_ps,
       width = 16,
       height = 12,
       units = "cm")

