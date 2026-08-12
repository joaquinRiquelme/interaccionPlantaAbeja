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

dac.sinna <- subset(dac, !is.na(DIT.final) )
dac.sinna <- unique(dac.sinna[,c("DIT.final","familia","sp.armonizado.x")])
library(lattice)


media.dit <- dac.sinna |> group_by(familia) |> summarise(mean=mean(DIT.final, na.rm=TRUE))
media.dit <- media.dit[order(media.dit$mean),]

textos_por_panel <- table(dac.sinna$familia)
textos_por_panel <- paste("n =", textos_por_panel)


media.dit$familia <- factor(media.dit$familia, levels = c("Halictidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Apidae"))
dac.sinna$familia <- factor(dac.sinna$familia, levels = c("Halictidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Apidae"))
levels(dac.sinna$familia)


colores_paneles <- c("orange","skyblue","lightseagreen","yellow","blue","deeppink")
dac.sinna <- dac.sinna[order(dac.sinna$familia),]

png("../03figuras/EstadisticaDescriptiva/DistribucionDITxFamilia.png")
histogram(~ DIT.final | familia, data = dac.sinna,
          layout=c(1,6),
          type="percent",
          as.table=TRUE,
          xlab="DIT (mm)",
          breaks = seq(from=0,to=9,by=0.5),
          ylim=c(0,60),
          panel = function(x, ...) {
            # 1. Obtiene el índice del panel actual
            p.number <- packet.number()
  
            # 2. Dibuja el histograma original del panel
            panel.histogram(x,
                            xlim = c(0,8),
                            col = colores_paneles[p.number], ...)
            
            
            # 3. Agrega el texto en coordenadas específicas (x, y)
            panel.text(x = 7, y = 40, 
                       label = textos_por_panel[p.number], 
                       col = "black", cex = 1)
          }
)
dev.off()

histogram(~DIT.final|familia, dac.sinna)


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


