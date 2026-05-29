library("tidyverse")
library("sf")
library("basemaps")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggplot2")
library("ggspatial")
library("ggmap")

base_datos <- readRDS("../00baseDatos/base_datos.RDS")
especies <- base_datos$especies
head(especies)
especies <- subset(especies, orden=="Hymenoptera" & is.element(familia, 
                                                               c("Andrenidae",
                                                                 "Halictidae",
                                                                 "Apidae",
                                                                 "Megachilidae",
                                                                 "Melittidae",
                                                                 "Colletidae",
                                                                 "Strenitidae")))
table(especies$familia)
tabla.e <- data.frame(table(especies$familia))
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

