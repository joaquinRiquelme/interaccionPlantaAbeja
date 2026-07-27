library(tidyverse)
library(lattice)


datos.analisis <- read.csv("../02salidas/datos.analisis.csv")
# GyT <- read.csv("GyTCV2.csv")
head(datos.analisis)
ml <- read.csv("../02salidas/ml.abejas.csv")

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

mlDIT <- merge(ml, datos.analisis, by=c("sp.armonizado"))
mlDIT <- subset(mlDIT, interaccion>0)
mlDIT$interaccion <- NULL
mlDIT$X <- NULL
mlDIT <- unique(mlDIT)
head(mlDIT)
sort(table(mlDIT$esp), decreasing = TRUE)


# mlDIT_sin <- subset(mlDIT, !is.element(especie,c ("Apis.mellifera", "Apis.mellifera.Linnaeus")))

medianas.tot <- unique(mlDIT[,c("DIT.final","sp.armonizado")])$DIT.final


mlDIT$log.DIT <- log(mlDIT$DIT.final+0.00001)
boxplot(unique(mlDIT$DIT.final), outline = FALSE)
boxplot(unique(mlDIT$log.DIT), outline = FALSE)
hist(unique(mlDIT$DIT.final))
hist(unique(mlDIT$log.DIT))


mlDIT$ID <- mlDIT$ID.x
# mediana global
mediana.global <- median(unique(mlDIT[,c("DIT.final","ID","sp.armonizado")])$DIT.final, na.rm=TRUE)
mediana.global2 <- median(unique(mlDIT[,c("DIT.final","sp.armonizado")])$DIT.final, na.rm=TRUE)

mlDIT$MG <- NA
mlDIT$MG[mlDIT$DIT.final <= mediana.global] <- "Pequeña"
mlDIT$MG[mlDIT$DIT.final > mediana.global] <- "Grande"
mlDIT$MG2 <- NA
mlDIT$MG2[mlDIT$DIT.final <= mediana.global2] <- "Pequeña"
mlDIT$MG2[mlDIT$DIT.final > mediana.global2] <- "Grande"

png(filename = file.path("../03figuras/umbral/","MedianaGlobalUnicosyRepetidos.png"), width = 1080)
# par(mfrow=c(1,1))
par(mfrow=c(2,2))
median(unique(mlDIT[,c("DIT.final","ID","sp.armonizado")])$DIT.final, na.rm=TRUE)
hist(unique(mlDIT[,c("DIT.final","ID","sp.armonizado")])$DIT.final,
     main="Mediana de TC a nivel global",
     xlab="DIT (mm)",
     xlim=c(0,8), ylim = c(0,100), 
     las=1, breaks = seq(0,9,0.5))
abline(v=median(unique(mlDIT[,c("DIT.final","ID","sp.armonizado")])$DIT.final, na.rm=TRUE), col="red", lwd=2, lty=2)
text(y=90,
     x = median(unique(mlDIT[,c("DIT.final","ID","sp.armonizado")])$DIT.final, na.rm=TRUE)+0.2,
     labels=round(median(unique(mlDIT[,c("DIT.final","ID","sp.armonizado")])$DIT.final, na.rm=TRUE),2))
# dev.off()



# boxplot(DIT.final~MG, unique(mlDIT[,c("DIT.final","MG","sp.armonizado","ID.x")]))
boxplot(DIT.final~MG, unique(mlDIT[,c("DIT.final","MG","sp.armonizado","ID")]),
        xlab="Categoria de tamaño",
        ylim=c(0,9),
        las=1,
        main="DIT por categorias de tamaño Mediana Global")
text(x = c(1.1,2.1),y = 8.5, labels = paste("n ",table(unique(mlDIT[,c("DIT.final","MG","sp.armonizado","ID")])$MG)))
text(x = c(1.5,2.5),
     y = c(median(unique(mlDIT[mlDIT$MG=="Grande",c("DIT.final","MG","sp.armonizado","ID")]$DIT.final), na.rm=TRUE),
           median(unique(mlDIT[mlDIT$MG=="Pequeña",c("DIT.final","MG","sp.armonizado","ID")]$DIT.final), na.rm=TRUE)),
     labels = c(median(unique(mlDIT[mlDIT$MG=="Grande",c("DIT.final","MG","sp.armonizado","ID")]$DIT.final), na.rm=TRUE),
                median(unique(mlDIT[mlDIT$MG=="Pequeña",c("DIT.final","MG","sp.armonizado","ID")]$DIT.final), na.rm=TRUE)))

# png(filename = file.path("../03figuras/umbral/","histogramaDITGlobalUnicos.png"))
# par(mfrow=c(1,1))
median(unique(mlDIT[,c("DIT.final","sp.armonizado")])$DIT.final, na.rm=TRUE)
hist(unique(mlDIT[,c("DIT.final","sp.armonizado")])$DIT.final,
     main="Mediana de TC a nivel global valores únicos",
     xlab="DIT (mm)",
     xlim=c(0,8), ylim = c(0,100),
     las=1, breaks = seq(0,9,0.5))
abline(v=median(unique(mlDIT[,c("DIT.final","sp.armonizado")])$DIT.final, na.rm=TRUE), col="red", lwd=2, lty=2)
text(y=80,
     x = median(unique(mlDIT[,c("DIT.final","sp.armonizado")])$DIT.final, na.rm=TRUE)+0.2,
     labels=round(median(unique(mlDIT[,c("DIT.final","sp.armonizado")])$DIT.final, na.rm=TRUE),2))
# dev.off()

# png(filename = file.path("../03figuras/umbral/","BoxplotMedianaGlobalUnicos.png"))
boxplot(DIT.final~MG2, unique(mlDIT[,c("DIT.final","MG2","sp.armonizado")]),
        xlab="Categoria de tamaño",
        ylim=c(0,9),
        las=1,
        main="DIT por categorias de tamaño Mediana Global valores únicos")
text(x = c(1.1,2.1),y = 8.5, labels = paste("n ",table(unique(mlDIT[,c("DIT.final","MG2","sp.armonizado")])$MG2)))
text(x = c(1.5,2.5),
     y = c(median(unique(mlDIT[mlDIT$MG2=="Grande",c("DIT.final","MG","sp.armonizado")]$DIT.final), na.rm=TRUE),
           median(unique(mlDIT[mlDIT$MG2=="Pequeña",c("DIT.final","MG","sp.armonizado")]$DIT.final), na.rm=TRUE)),
     labels = c(median(unique(mlDIT[mlDIT$MG2=="Grande",c("DIT.final","MG","sp.armonizado")]$DIT.final), na.rm=TRUE),
           median(unique(mlDIT[mlDIT$MG2=="Pequeña",c("DIT.final","MG","sp.armonizado")]$DIT.final), na.rm=TRUE)))

dev.off()

# mediana por matriz
mediana.ID <- mlDIT |> group_by(ID) |> summarise(mediana.ID = median(unique(DIT.final), na.rm=TRUE))
mediana.ID$mediana.ID[is.na(mediana.ID$mediana.ID)] <- mediana.global

mlDIT2 <- merge(mlDIT, mediana.ID, by="ID")

mlDIT2$MM <- NA
mlDIT2$MM[mlDIT2$DIT.final <= mlDIT2$mediana.ID] <- "Pequeña"
mlDIT2$MM[mlDIT2$DIT.final > mlDIT2$mediana.ID] <- "Grande"

# png(filename = file.path("../03figuras/umbral/","BoxplotMedianaMatriz.png"))
boxplot(DIT.final~MM, unique(mlDIT2[,c("DIT.final","MM","sp.armonizado")]),
        xlab="Categoria de tamaño",
        ylim=c(0,9),
        las=1,
        main="DIT por categorias de tamaño Mediana por matriz")
text(x = c(1.1,2.1),y = 8.5, labels = paste("n ",table(unique(mlDIT2[,c("DIT.final","MM","sp.armonizado","ID")])$MM)))
text(x = c(1.5,2.5),
     y = c(median(unique(mlDIT2[mlDIT2$MM=="Grande",c("DIT.final","MM","sp.armonizado","ID")]$DIT.final), na.rm=TRUE),
           median(unique(mlDIT2[mlDIT2$MM=="Pequeña",c("DIT.final","MM","sp.armonizado","ID")]$DIT.final), na.rm=TRUE)),
     labels = c(median(unique(mlDIT2[mlDIT2$MM=="Grande",c("DIT.final","MM","sp.armonizado","ID")]$DIT.final), na.rm=TRUE),
                median(unique(mlDIT2[mlDIT2$MM=="Pequeña",c("DIT.final","MM","sp.armonizado","ID")]$DIT.final), na.rm=TRUE)))

dev.off()

# por percentil

datosDIT <- unique(mlDIT[,c("DIT.final","ID","sp.armoniado")])
datosDIT$cuantil <- ntile(datosDIT$DIT.final, n=2)
table(datosDIT$cuantil)
histogram(~DIT.final|factor(cuantil), datosDIT, as.table=TRUE)
table(datosDIT$cuantil)

datosDIT$T3 <- NA
datosDIT$T3[datosDIT$DIT.final<= 2.2] <- "G1"
datosDIT$T3[datosDIT$DIT.final> 2.2 & datosDIT$DIT.final<= 3.9] <- "G2"
datosDIT$T3[datosDIT$DIT.final> 3.9 ] <- "G3"

# datos_insectos$tamano_clase[datos_insectos$tamano_mm <= 2.2] <- "1.Pequeño"
# datos_insectos$tamano_clase[datos_insectos$tamano_mm > 2.2 & datos_insectos$tamano_mm <= 3.9] <- "2.Mediano"
# datos_insectos$tamano_clase[datos_insectos$tamano_mm > 3.9] <- "3.Grande"




png("tresCategoriasLiteratura.png")
print(histogram(~DIT.final|factor(T3), datosDIT, as.table=TRUE, type = "count", layout=c(3,1)))
dev.off()

png("CategoriasCuantiles.png")
print(histogram(~DIT.final|factor(cuantil), datosDIT, as.table=TRUE, type = "count"))
dev.off()



# library(ggplot2)
# Ejemplo de código usando un data frame llamado 'datos'
# ggplot(datosDIT, aes(x = DIT.final, fill = DIT.final)) +
  # geom_bar(stat = "count", position = "dodge") +
  # facet_wrap(~ factor(cuantil) + factor(T3)) +
  # theme_minimal() +
  # labs(
    # title = "Distribución por Categorías",
    # x = "Variable Respuesta",
    # y = "Frecuencia"
  # )

library(lattice)

# Ejemplo de visualización condicionada
densityplot(~ DIT.final | factor(cuantil) + factor(T3), 
            data = datosDIT,
            main = "Densidad de la respuesta por grupos",
            xlab = "Variable Respuesta",
            plot.points = FALSE, # Oculta las marcas de puntos individuales
            auto.key = TRUE, as.table=TRUE)       # Agrega la leyenda




# Categorias de grado
datos.analisis$cuantil.grado <- ntile(datos.analisis$Grado, n=n.categorias.grado)
histogram(~Grado|factor(cuantil.grado), datos.analisis, as.table=TRUE)

datos.analisis$categorias.grado <- factor(datos.analisis$cuantil.grado, levels =c(1,2) , labels = valores.grado)


# datos_insectos$clase_especializacion <- NA
# datos_insectos$clase_especializacion[datos_insectos$grado_num <  summary(datos_insectos$grado_num)[3]] <- "1.Especialista"
# datos_insectos$clase_especializacion[datos_insectos$grado_num >=  summary(datos_insectos$grado_num)[3]] <- "2.Generalista"
# datos_insectos$clase_especializacion[datos_insectos$grado_num >  summary(datos_insectos$grado_num)[3] &
# datos_insectos$grado_num <=  summary(datos_insectos$grado_num)[5]] <- "2.2.Generalista"
# datos_insectos$clase_especializacion[datos_insectos$grado_num >  summary(datos_insectos$grado_num)[5]] <- "3.Hipergeneralista"

# head(datos_insectos)

# table(datos_insectos$clase_especializacion)
# summary(as.factor(datos_insectos$clase_especializacion))





datos.analisis$cuantil.latitud <- ntile(abs(datos.analisis$Latitud), n=n.categorias.latitud)
histogram(~abs(Latitud)|factor(cuantil.latitud), datos.analisis, as.table=TRUE)
datos.analisis$categorias.latitud <- factor(datos.analisis$cuantil.latitud, levels =c(1,2) , labels = valores.latitud)

# geograficas <- base_datos$ubicacion.geografica
# geograficas$ID[geograficas$ID=="Santos_2010"] <- "M_076"
# geograficas$ID[geograficas$ID=="M_Var2012"] <- "M_077"
# datos_geo <- merge(datos_insectos, geograficas, by="ID")
# head(datos_geo)
# 
# datos_geo$clase_latitud <- NA
# datos_geo$clase_latitud[abs(datos_geo$Latitud)<30] <- "1.Baja"
# datos_geo$clase_latitud[abs(datos_geo$Latitud)>=30 & abs(datos_geo$Latitud)<90] <- "2.Media"
# datos_geo$clase_latitud[abs(datos_geo$Latitud)>=100] <- "3.Alta"
# table(datos_geo$clase_latitud)
# 
# 




names(datos.analisis2)
datos.analisis2 <- merge(datos.analisis, datosDIT, by=c("ID","DIT.final"))
write.csv(datos.analisis2,"../02salidas/datos.analisis.categorizado.csv",row.names = FALSE)

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
mlDIT$DIT <- mlDIT$DIT.final
mlDIT <- subset(mlDIT, !is.na(DIT))


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
    table(mlDIT.j$size)
    
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
