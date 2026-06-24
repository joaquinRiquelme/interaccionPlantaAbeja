library(tidyverse)
library(lattice)

GyT <- read.csv("GyTCV2.csv")
head(GyT)
ml <- read.csv("ml.abejas.csv")

# base_datos <- readRDS("../00baseDatos/base_datos.RDS")
# especie <- base_datos$especies
# DIT <- base_datos$DIT 
# head(DIT)
# DIT$Abeja <- gsub(pattern = "_",replacement = ".",x = DIT$Abeja)
# DIT$ID <- NULL
# DIT <- unique(DIT)

# ml <- base_datos$matriz_comp_larga
# # ml$plantas <- NULL
# head(ml)

mlDIT <- merge(ml, GyT, by="sp.armonizado")
mlDIT <- subset(mlDIT, interaccion>0)
mlDIT$interaccion <- NULL
mlDIT$X <- NULL
mlDIT <- unique(mlDIT)
head(mlDIT)
sort(table(mlDIT$esp), decreasing = TRUE)


# mlDIT_sin <- subset(mlDIT, !is.element(especie,c ("Apis.mellifera", "Apis.mellifera.Linnaeus")))

medianas.tot <- unique(mlDIT[,c("DIT.final","sp.armonizado")])$DIT.final

png(filename = file.path("../03figuras/umbral/","histogramaDITGlobal.png"))
# par(mfrow=c(2,1))
median(unique(mlDIT[,c("DIT.final","ID.x","sp.armonizado")])$DIT.final, na.rm=TRUE)
hist(unique(mlDIT[,c("DIT.final","ID.x","sp.armonizado")]$DIT.final),
     main="Mediana de TC a nivel global",
     xlab="DIT (mm)",
     xlim=c(0,8), ylim = c(0,60), las=1, breaks = seq(0,9,0.5))
abline(v=median(unique(mlDIT[,c("DIT.final","ID.x","sp.armonizado")])$DIT.final, na.rm=TRUE), col="red", lwd=2, lty=2)
text(y=50,
     x = median(unique(mlDIT[,c("DIT.final","ID.x","sp.armonizado")])$DIT.final, na.rm=TRUE)+0.2,
     labels=round(median(unique(mlDIT[,c("DIT.final","ID.x","sp.armonizado")])$DIT.final, na.rm=TRUE),2))
dev.off()


mlDIT$log.DIT <- log(mlDIT$DIT.final+0.00001)
boxplot(unique(mlDIT$DIT.final), outline = FALSE)
boxplot(unique(mlDIT$log.DIT), outline = FALSE)
hist(unique(mlDIT$DIT.final))
hist(unique(mlDIT$log.DIT))

# mediana global
mediana.global <- median(unique(mlDIT[,c("DIT.final","ID.x","sp.armonizado")])$DIT.final, na.rm=TRUE)

mlDIT$MG <- NA
mlDIT$MG[mlDIT$DIT.final<=mediana.global] <- "Pequeña"
mlDIT$MG[mlDIT$DIT.final>mediana.global] <- "Grande"

# boxplot(DIT.final~MG, unique(mlDIT[,c("DIT.final","MG","sp.armonizado","ID.x")]))
png(filename = file.path("../03figuras/umbral/","BoxplotMedianaGlobal.png"))
boxplot(DIT.final~MG, unique(mlDIT[,c("DIT.final","MG","sp.armonizado")]),
        xlab="Categoria de tamaño",
        ylim=c(0,9),
        las=1,
        main="DIT por categorias de tamaño Mediana Global")
text(x = c(1.1,2.1),y = 8.5, labels = paste("n ",table(unique(mlDIT[,c("DIT.final","MG","sp.armonizado","ID.x")])$MG)))
text(x = c(1.5,2.5),
     y = c(median(unique(mlDIT[mlDIT$MG=="Grande",c("DIT.final","MG","sp.armonizado","ID.x")]$DIT.final), na.rm=TRUE),
           median(unique(mlDIT[mlDIT$MG=="Pequeña",c("DIT.final","MG","sp.armonizado","ID.x")]$DIT.final), na.rm=TRUE)),
     labels = c(median(unique(mlDIT[mlDIT$MG=="Grande",c("DIT.final","MG","sp.armonizado","ID.x")]$DIT.final), na.rm=TRUE),
           median(unique(mlDIT[mlDIT$MG=="Pequeña",c("DIT.final","MG","sp.armonizado","ID.x")]$DIT.final), na.rm=TRUE)))

dev.off()

# mediana por matriz
mediana.ID <- mlDIT |> group_by(ID.x) |> summarise(mediana.ID = median(unique(DIT.final), na.rm=TRUE))
mediana.ID$mediana.ID[is.na(mediana.ID$mediana.ID)] <- mediana.global

mlDIT2 <- merge(mlDIT, mediana.ID, by="ID.x")

mlDIT2$MM <- NA
mlDIT2$MM[mlDIT2$DIT.final<=mlDIT2$mediana.ID] <- "Pequeña"
mlDIT2$MM[mlDIT2$DIT.final>mlDIT2$mediana.ID] <- "Grande"

png(filename = file.path("../03figuras/umbral/","BoxplotMedianaMatriz.png"))
boxplot(DIT.final~MM, unique(mlDIT2[,c("DIT.final","MM","sp.armonizado")]),
        xlab="Categoria de tamaño",
        ylim=c(0,9),
        las=1,
        main="DIT por categorias de tamaño Mediana por matriz")
text(x = c(1.1,2.1),y = 8.5, labels = paste("n ",table(unique(mlDIT2[,c("DIT.final","MM","sp.armonizado","ID.x")])$MM)))
text(x = c(1.5,2.5),
     y = c(median(unique(mlDIT2[mlDIT2$MM=="Grande",c("DIT.final","MM","sp.armonizado","ID.x")]$DIT.final), na.rm=TRUE),
           median(unique(mlDIT2[mlDIT2$MM=="Pequeña",c("DIT.final","MM","sp.armonizado","ID.x")]$DIT.final), na.rm=TRUE)),
     labels = c(median(unique(mlDIT2[mlDIT2$MM=="Grande",c("DIT.final","MM","sp.armonizado","ID.x")]$DIT.final), na.rm=TRUE),
                median(unique(mlDIT2[mlDIT2$MM=="Pequeña",c("DIT.final","MM","sp.armonizado","ID.x")]$DIT.final), na.rm=TRUE)))

dev.off()

# por percentil

aaaaa <- unique(mlDIT[,c("DIT.final","ID.x")])
aaaaa$cuantil <- ntile(aaaaa$DIT.final, n=3)
histogram(~DIT.final|factor(cuantil), aaaaa, as.table=TRUE)
table(aaaaa$cuantil)

aaaaa$T3 <- NA
aaaaa$T3[aaaaa$DIT.final<= 2.2] <- "G1"
aaaaa$T3[aaaaa$DIT.final> 2.2 & aaaaa$DIT.final<= 3.9] <- "G2"
aaaaa$T3[aaaaa$DIT.final> 3.9 ] <- "G3"

png("tresCategoriasLiteratura.png")
print(histogram(~DIT.final|factor(T3), aaaaa, as.table=TRUE, type = "count", layout=c(3,1)))
dev.off()

png("tresCategoriasCuantiles.png")
print(histogram(~DIT.final|factor(cuantil), aaaaa, as.table=TRUE, type = "count", layout=c(3,1)))
dev.off()


library(ggplot2)
# Ejemplo de código usando un data frame llamado 'datos'
ggplot(aaaaa, aes(x = DIT.final, fill = DIT.final)) +
  # geom_bar(stat = "count", position = "dodge") +
  facet_wrap(~ factor(cuantil) + factor(T3)) +
  theme_minimal() +
  labs(
    title = "Distribución por Categorías",
    x = "Variable Respuesta",
    y = "Frecuencia"
  )

library(lattice)

# Ejemplo de visualización condicionada
densityplot(~ DIT.final | factor(cuantil) + factor(T3), 
            data = aaaaa,
            main = "Densidad de la respuesta por grupos",
            xlab = "Variable Respuesta",
            plot.points = FALSE, # Oculta las marcas de puntos individuales
            auto.key = TRUE, as.table=TRUE)       # Agrega la leyenda

# write.csv(mlDIT.nueva, file = "../02salidas/tabla_umbral_mediana.csv", row.names = FALSE)
# mlDIT.nueva.con <- mlDIT.nueva

# mlDIT.nueva <- c()
# for(i in unique(mlDIT_sin$ID)){
  # mlDIT.i <- subset(mlDIT_sin, ID==i)
  
  # umbral.mediana.i <- median(mlDIT.i$DIT, na.rm = TRUE)
  # png(filename = paste0("../03figuras/umbral/umbral_mediana_",i,".png"))
  # plot(x=mlDIT.i$DIT, y=mlDIT.i$DIT*0, main=i,
  #      xlim=c(0,9),
  #      ylab="",
  #      xlab="DIT (mm)")
  # abline(v=umbral.mediana.i, col="red", lty=2)
  # # dev.off()
  # mlDIT.i$size <- "Pequena"
  # mlDIT.i$size[mlDIT.i$DIT<umbral.mediana.i] <- "Pequena"
  # mlDIT.i$size[mlDIT.i$DIT>umbral.mediana.i] <- "Grande"
  # mlDIT.i$umbral.mediana <- umbral.mediana.i
  # mlDIT.nueva <- rbind(mlDIT.nueva, mlDIT.i)
  
# }
# head(mlDIT.nueva)
# par(mfrow=c(2,1))


# datos.umbral.mediana <- unique(mlDIT.nueva.con$umbral.mediana)
# datos.umbral.mediana.sin <- unique(mlDIT.nueva$umbral.mediana)

# png(filename = file.path("../03figuras/umbral/","medianasXmatriz.png"))
# par(mfrow=c(2,1))
# hist(unique(mlDIT.nueva.con$umbral.mediana),
     # main="Distribucion de medianas por matriz con A. mellifera",
     # xlim=c(0,7), ylim = c(0,10), las=1)
# # abline(v=mean(unique(mlDIT.nueva.con$umbral.mediana)), col="red", lwd=2, lty=2)
# text(y=9,
     # x = mean(unique(mlDIT.nueva.con$umbral.mediana))+0.2,
     # labels=round(mean(unique(mlDIT.nueva.con$umbral.mediana)),2))

# hist(unique(mlDIT.nueva$umbral.mediana), 
     # main="Distribucion de medianas por matriz sin A. mellifera",
     # xlim=c(0,7), ylim = c(0,10), las=1)
# abline(v=mean(unique(mlDIT.nueva$umbral.mediana)), col="red", lwd=2, lty=2)
# text(y=9,
     # x = mean(unique(mlDIT.nueva$umbral.mediana))+0.2,
     # labels=round(mean(unique(mlDIT.nueva$umbral.mediana)),2))

# dev.off()

mlDIT.umbral <- c()
# mlDIT.umbral_sin <- c()

for(i in unique(mlDIT$ID)){
  print(i)
  # i="M_059"
  mlDIT.i <- subset(mlDIT, ID==i)
  plot(x=mlDIT.i$DIT, y=mlDIT.i$DIT*0, main=i,
       xlim=c(0,9),
       ylab="",
       xlab="DIT (mm)")
  for(j in seq(from=1.0,to=9, by=0.5)){
    print(j)
    # j=5
    umbral.j <- j
    mlDIT.j <- mlDIT.i
    mlDIT.j$umbral <- umbral.j
    mlDIT.j$size <- "Pequena"
    mlDIT.j$size[mlDIT.j$DIT<umbral.j] <- "Pequena"
    mlDIT.j$size[mlDIT.j$DIT>=umbral.j] <- "Grande"
    
    mlDIT.j$es.umbral <- FALSE
    if(length(unique(mlDIT.j$size))==1){ 
      mlDIT.j$es.umbral <- FALSE
      mlDIT.umbral <- rbind(mlDIT.umbral, mlDIT.j); next}
    if(length(unique(mlDIT.j$size))==1){ 
      mlDIT.j$es.umbral <- FALSE
      mlDIT.umbral <- rbind(mlDIT.umbral, mlDIT.j); next}
    if(table(mlDIT.j$size)[1]==1){ 
      mlDIT.j$es.umbral <- FALSE
      mlDIT.umbral <- rbind(mlDIT.umbral, mlDIT.j); next}
    if(table(mlDIT.j$size)[2]==1){ 
      mlDIT.j$es.umbral <- FALSE
      mlDIT.umbral <- rbind(mlDIT.umbral, mlDIT.j); next}
    test.j <- t.test(DIT ~ size, data = unique(mlDIT.j))
    if(test.j$p.value < 0.05){
      mlDIT.j$es.umbral <- TRUE
    }
    if(sum(mlDIT.j$es.umbral)>0){
      abline(v=umbral.j, col="blue", lty=2)
    }
    mlDIT.umbral <- rbind(mlDIT.umbral, mlDIT.j)
  
    
  }
  
}

head(mlDIT.umbral)
mlDIT.umbral.true <- subset(mlDIT.umbral, es.umbral==TRUE)
mlDIT.umbral.sin.true <- subset(mlDIT.umbral_sin, es.umbral==TRUE)

umbrales <- unique(mlDIT.umbral.true[,c("ID","umbral","es.umbral")])
umbrales_sin <- unique(mlDIT.umbral.sin.true[,c("ID","umbral","es.umbral")])

# png("../03figuras/umbral/barplotUmbralp0.05.png")
par(mfrow=c(2,1))
barplot(table(umbrales$es.umbral, umbrales$umbral), beside = TRUE, col = c(2), 
        main = "Es umbral? con A. melliferea\n Rojo = verdadero, Negro= falso", 
        xlim = c(0,24),
        ylim = c(0,30))

barplot(table(umbrales_sin$es.umbral, umbrales_sin$umbral), 
        beside = TRUE, col = c(2), 
        main = "Es umbral? sin A. melliferea\n  Rojo = verdadero, Negro= falso ", 
        xlim = c(0,24),
        ylim = c(0,30))
# dev.off()

barplot(table(umbrales_sin$es.umbral, umbrales_sin$umbral),
        beside = TRUE, col = c(2), 
        main = "Es umbral? sin A. melliferea\n  Rojo = verdadero, Negro= falso ", 
        xlim = c(0,24),
        ylim = c(0,30), plot=FALSE)


head(mlDIT.umbral)
table(mlDIT.umbral.true$es.umbral, mlDIT.umbral.true$umbral)
table(table(unique(mlDIT.umbral.true[,c("ID","es.umbral")])))

write.csv(mlDIT.umbral, file = "../02salidas/tabla_umbral.csv", row.names = FALSE)


# 1. Calcular la densidad
dens.mediana <- density(datos.mediana)
dens.mediana.umbral <- density(datos.umbral.mediana)
dens.umbral <- density(umbrales$umbral)

# 2. Graficar
# png("../03figuras/umbral/umbralesTODOS.png")
par(mfrow=c(2,1))
plot(dens.mediana, main = "Gráfico de Densidades de umbrales a nivel global", col = "blue", lwd = 2, ylim=c(0,0.5))
lines(dens.mediana.umbral, col = "red", lwd = 2)
lines(dens.umbral, col = "green", lwd = 2)
legend("topright", legend = c("Mediana global", "Mediana por matriz","Umbral por matriz"),
       col = c("blue","red","green"),
       lwd = 2)
abline(v=mean(datos.mediana), col="blue", lty=2)
abline(v=mean(datos.umbral.mediana), col="red", lty=2)
abline(v=mean(umbrales$umbral), col="green", lty=2)

text(y=0.4,x=mean(datos.mediana),
     labels=round(mean(datos.mediana),2), col="blue")
text(y=0.425,x=mean(datos.umbral.mediana),
     labels=round(mean(datos.umbral.mediana),2), col="red")
text(y=0.45,x=mean(umbrales$umbral),
     labels=round(mean(umbrales$umbral),2), col="green")


plot(dens.mediana.sin, main = "Gráfico de Densidades de umbrales a nivel global sin A. mellifera", col = "blue", lwd = 2, ylim=c(0,0.5))
lines(dens.mediana.umbral.sin, col = "red", lwd = 2)
lines(dens.umbral.sin, col = "green", lwd = 2)
legend("topright", legend = c("Mediana global", "Mediana por matriz","Umbral por matriz"),
       col = c("blue","red","green"),
       lwd = 2)
abline(v=mean(datos.mediana.sin), col="blue", lty=2)
abline(v=mean(datos.umbral.mediana.sin), col="red", lty=2)
abline(v=mean(umbrales_sin$umbral), col="green", lty=2)

text(y=0.4,x=mean(datos.mediana.sin),
     labels=round(mean(datos.mediana.sin),2), col="blue")
text(y=0.45,x=mean(datos.umbral.mediana.sin),
     labels=round(mean(datos.umbral.mediana.sin),2), col="red")
text(y=0.425,x=mean(umbrales_sin$umbral),
     labels=round(mean(umbrales_sin$umbral),2), col="green")

# dev.off()

# todos juntos
# png("../03figuras/umbral/umbralesTODOS_conysin.png")
plot(dens.mediana, main = "Gráfico de Densidades de umbrales a nivel global", col = "blue", lwd = 2, ylim=c(0,0.5))
lines(dens.mediana.umbral, col = "red", lwd = 2)
lines(dens.umbral, col = "green", lwd = 2)
legend("topright", legend = c("Mediana global", "Mediana por matriz","Umbral por matriz"),
       col = c("blue","red","green"),
       lwd = 2)
abline(v=mean(datos.mediana), col="blue", lty=1)
abline(v=mean(datos.umbral.mediana), col="red", lty=1)
abline(v=mean(umbrales$umbral), col="green", lty=1)

text(y=0.4,x=mean(datos.mediana),
     labels=round(mean(datos.mediana),2), col="blue")
text(y=0.425,x=mean(datos.umbral.mediana),
     labels=round(mean(datos.umbral.mediana),2), col="red")
text(y=0.45,x=mean(umbrales$umbral),
     labels=round(mean(umbrales$umbral),2), col="green")


lines(dens.mediana.sin, col = "blue", lwd = 2, lty=2)
lines(dens.mediana.umbral.sin, col = "red", lwd = 2, lty=2)
lines(dens.umbral.sin, col = "green", lwd = 2, lty=2)
legend("topright", 
       legend = c("Mediana global", "Mediana por matriz","Umbral por matriz",
                  "","Todas las especies", "Sin A. mellifera"),
       col = c("blue","red","green", "none","black", "black"),
       lwd = 2, lty=c(1,1,1,0,1,2))
abline(v=mean(datos.mediana.sin), col="blue", lty=2)
abline(v=mean(datos.umbral.mediana.sin), col="red", lty=2)
abline(v=mean(umbrales_sin$umbral), col="green", lty=2)

text(y=0.1,x=mean(datos.mediana.sin),
     labels=round(mean(datos.mediana.sin),2), col="blue")
text(y=0.15,x=mean(datos.umbral.mediana.sin),
     labels=round(mean(datos.umbral.mediana.sin),2), col="red")
text(y=0.125,x=mean(umbrales_sin$umbral),
     labels=round(mean(umbrales_sin$umbral),2), col="green")
# dev.off()
# 

# Gráfico con relleno y alfombra de datos (rug)
plot(dens.mediana, main = "Densidad con Relleno")
polygon(dens.mediana, col = "lightblue", border = "blue")
rug(datos.mediana) # Añade marcas de los datos reales en el eje X

# Superponer sobre un histograma
hist(datos.mediana, prob = TRUE, main = "Histograma + Densidad", col = "gray90")
lines(dens.mediana, col = "red", lwd = 2)
