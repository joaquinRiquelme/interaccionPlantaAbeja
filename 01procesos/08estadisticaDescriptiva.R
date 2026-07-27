library("tidyverse")
library("sf")
library("basemaps")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggplot2")
library("ggspatial")
library("ggmap")

# base_datos <- readRDS("../00baseDatos/base_datos.RDS")
dac <- read.csv("../02salidas/datos.analisis.categorizado.csv")

tabla.e <- data.frame(table(dac$familia))
names(tabla.e)[1] <- "Familia"
tabla.f <- tabla.e[order(tabla.e$Freq, decreasing = TRUE),]
# otras <- sum(tabla.f$Freq[11:nrow(tabla.f)])
# tabla.f <- tabla.f[1:10,]
# tabla.f <- rbind(tabla.f, data.frame(Var1="Otras", 
                                     # Freq=otras))

tabla.f <- tabla.f[order(tabla.f$Familia, decreasing = FALSE),]


tabla.f$porcentaje <- round(tabla.f$Freq / sum(tabla.f$Freq) *100, 1) 
tabla.f$label_pos <- sum(tabla.f$Freq) - cumsum(tabla.f$Freq)+tabla.f$Freq/2



p <- ggplot(tabla.f, aes(x='',y=Freq, fill= Familia))+
  geom_bar(stat="identity", width=1)+
  scale_fill_viridis_d()

p

pie <- p + coord_polar("y",start = -1)+
  theme_void()+
  labs(title='Número de especies de abeja por familia')+
  geom_text(aes(y=label_pos, label=paste0(Freq, "\n", porcentaje, "%" )),
            color="black", size=4)

pie
png("pieFamilias.png", width = 1280, height = 720, units ="px")
pie
dev.off()

png("pieFamilias800.png", width = 800, height = 600, units ="px")
pie
dev.off()

# 
pairs.panels(unique(dac[,c("Grado","DIT.final")]))

boxplot(tamano_mm~tamano_clase,datos_insectos)
abline(h=c(2.2,3.9))

par(mfrow=c(2,1))
hist(datos_insectos$grado_num)
abline(v=(summary(datos_insectos$grado_num)[c(2,3,5)]))
boxplot(datos_insectos$grado_num, horizontal = TRUE)
abline(v=(summary(datos_insectos$grado_num)[c(2,3,5)]))

png("boxplot_ZScore_matriz.png")
boxplot(grado_num~ID, subset(datos_insectos),ylim=c(-1.5,6))
# abline(h=summary(subset(datos_insectos)$grado_num)[3], col="blue")
abline(h=median(datos_insectos$grado_num), col="red", lwd=2)
dev.off()


boxplot(datos_insectos$grado_num, horizontal = TRUE, main="Distribución de Z score de grado, mediana marcada en rojo")
summary(datos_insectos$grado_num)
abline(v=summary(datos_insectos$grado_num)[3], col=c(1,1,2,1,1,1,1)[3], lwd=c(1,1,2,1,1,1,1)[3])
text(x = summary(datos_insectos$grado_num)[3]+0.2, y = 0.5, labels = round(summary(datos_insectos$grado_num)[3],2))


