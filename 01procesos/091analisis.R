
datos_insectos2 <- datos_insectos |> 
  group_by(sp.armonizado.x, familia, Family) |>
  summarise(DIT=unique(DIT.final),
            Grado=mean(Grado, na.rm=TRUE),
            Z_Grado=mean(Z_Grado, na.rm=TRUE),
            n.id=length(ID))



histogram(~Z_Grado,subset(datos_insectos,sp.armonizado.x=="Apis mellifera"), layout=c(1,1))
summary(subset(datos_insectos, sp.armonizado.x=="Apis mellifera")$Z_Grado)

datos_insectos2 <- subset(datos_insectos2, n.id>1)

table(datos_insectos$sp.armonizado.x)[table(datos_insectos$sp.armonizado.x)>2]
histogram(~Z_Grado,subset(datos_insectos,sp.armonizado.x=="Bombus hortorum"), layout=c(1,1))
histogram(~Z_Grado,subset(datos_insectos, !is.na(Z_Grado) & sp.armonizado.x=="Bombus hyperboreus"), layout=c(1,1))
summary(subset(datos_insectos, sp.armonizado.x=="Apis mellifera")$Z_Grado)

# hist(datos_insectos2$Z_Grado)


head(datos_insectos2)
# library(lattice)
# histogram(~Z_Grado, datos_insectos2)
# histogram(~Z_Grado|Family, datos_insectos2)

# histogram(~Grado, datos_insectos2)
# histogram(~Grado|Family, datos_insectos2)

# histogram(~DIT, datos_insectos2)
# histogram(~DIT|Family, datos_insectos2)

## 3 categorias datos unicos ----

datos_insectos3 <- datos_insectos2
datos_insectos3$orden.sp <- sample(1:nrow(datos_insectos2), replace = FALSE)
datos_insectos3 <- datos_insectos3[order(datos_insectos3$orden.sp),]
head(datos_insectos3)

datos_insectos2 <- datos_insectos3
datos_insectos2$clase_especializacion <- ntile(datos_insectos2$Z_Grado, n=3)
# datos_insectos2$clase_especializacion <- ntile(datos_insectos2$Grado, n=3)

# datos_insectos2$clase_especializacion[datos_insectos2$Grado==2] <- 1
# datos_insectos2$clase_especializacion[datos_insectos2$Grado==5] <- 2

# table(datos_insectos2$Grado, datos_insectos2$clase_especializacion)
table(datos_insectos2$Z_Grado, datos_insectos2$clase_especializacion)

datos_insectos2$clase_especializacion <- as.factor(datos_insectos2$clase_especializacion)
datos_insectos2$tamano_mm <- datos_insectos2$DIT

modelo_ord3 <- polr(clase_especializacion ~ tamano_mm, data = datos_insectos2, Hess = TRUE)

# Ver resumen
summary(modelo_ord3)

# Calcular p-values
tabla_coef <- coef(summary(modelo_ord3))
p <- pnorm(abs(tabla_coef[, "t value"]), lower.tail = FALSE) * 2
tabla_final <- cbind(tabla_coef, "p value" = p)
print(tabla_final)

# sink("../02salidas/resultados_modelo_ordinal_3.txt")
summary(modelo_ord)
print(tabla_final)
# sink()

# Crear datos para predecir
nuevos_datos <- data.frame(
  tamano_mm = seq(min(datos_insectos2$tamano_mm), max(datos_insectos2$tamano_mm), length.out = 100))

# Predecir probabilidades
predicciones <- cbind(nuevos_datos, predict(modelo_ord3, nuevos_datos, type = "probs"))

# Formatear para ggplot (formato largo)
# library(tidyr)
pred_long <- pivot_longer(predicciones, cols = c("1", "2","3"),
                          names_to = "Nivel", values_to = "Probabilidad")
pred_long$Nivel <- factor(pred_long$Nivel, levels = c("1", "2","3"))

# Graficar

head(pred_long)
# png("LogisticoOrdinalclases.png")
ggplot(pred_long, aes(x = tamano_mm, y = Probabilidad, color = Nivel)) +
  geom_line(linewidth = 1) +
  labs(title = "Probabilidades Predichas por nivel de generalización trófica",
       x = "Tamaño mm", y = "Probabilidad") +
  theme_minimal()
