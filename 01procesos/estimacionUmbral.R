library(tidyverse)

base_datos <- readRDS("../00baseDatos/base_datos.RDS")
especie <- base_datos$especies
DIT <- base_datos$DIT 
head(DIT)
DIT$Abeja <- gsub(pattern = "_",replacement = ".",x = DIT$Abeja)
DIT$ID <- NULL
DIT <- unique(DIT)

ml <- base_datos$matriz_comp_larga
ml$plantas <- NULL
head(ml)

mlDIT <- merge(ml, DIT, by.x="especie", by.y="especies")
mlDIT <- subset(mlDIT, interaccion>0)
mlDIT$interaccion <- NULL
mlDIT$Abeja <- NULL
mlDIT <- unique(mlDIT)

head(mlDIT)

mlDIT.nueva <- c()
for(i in unique(mlDIT$ID)){
  mlDIT.i <- subset(mlDIT, ID==i)
  
  umbral.mediana.i <- median(mlDIT.i$DIT, na.rm = TRUE)
  png(filename = paste0("../03figuras/umbral/umbral_mediana_",i,".png"))
  plot(x=mlDIT.i$DIT, y=mlDIT.i$DIT*0, main=i,
       xlim=c(0,9),
       ylab="",
       xlab="DIT (mm)")
  abline(v=umbral.mediana.i, col="red", lty=2)
  dev.off()
  mlDIT.i$size <- "Pequena"
  mlDIT.i$size[mlDIT.i$DIT<umbral.mediana.i] <- "Pequena"
  mlDIT.i$size[mlDIT.i$DIT>umbral.mediana.i] <- "Grande"
  mlDIT.i$umbral.mediana <- umbral.mediana
  mlDIT.nueva <- rbind(mlDIT.nueva, mlDIT.i)
  
}
head(mlDIT.nueva)
write.csv(mlDIT.nueva, file = "../02salidas/tabla_umbral_mediana.csv", row.names = FALSE)


mlDIT.umbral <- c()
for(i in unique(mlDIT$ID)){
  print(i)
  mlDIT.i <- subset(mlDIT, ID==i)
  plot(x=mlDIT.i$DIT, y=mlDIT.i$DIT*0, main=i,
       xlim=c(0,9),
       ylab="",
       xlab="DIT (mm)")
  for(j in 1:9){
    print(j)
    umbral.j <- j
    abline(v=umbral.j, col="blue", lty=2)
    mlDIT.j <- mlDIT.i
    mlDIT.j$umbral <- umbral.j
    mlDIT.j$size <- "Pequena"
    mlDIT.j$size[mlDIT.j$DIT<umbral.j] <- "Pequena"
    mlDIT.j$size[mlDIT.j$DIT>umbral.j] <- "Grande"
    
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
    
    mlDIT.umbral <- rbind(mlDIT.umbral, mlDIT.j)
  
    
  }
  
}

head(mlDIT.umbral)
table(mlDIT.umbral$es.umbral, mlDIT.umbral$umbral)
write.csv(mlDIT.umbral, file = "../02salidas/tabla_umbral.csv", row.names = FALSE)
