# Estimacion de beta diversidad

# Cargar librerías necesarias (basado en las dependencias de tu código)
library(tidyverse)
library(gtools)
library(igraph)
library(stringr)
library(plyr)
library(gtools)
library(lattice)
library(psych)


# tabla de interaccion planta abeja por matriz
ipa <- read.csv("ml.abejas.csv")
head(ipa)

## 1. Definición de funciones ----

# Función de métrica de beta-diversidad (Disimilitud de Sørensen)
# Faltaba en el código original. La disimilitud es: (b + c) / (2a + b + c)
B01 <- function(x) {
  return((x$b + x$c) / (2 * x$a + x$b + x$c))
}

# Whittaker
Wi <- function(x) {
  return((x$a + x$b + x$c) / ((2 * x$a + x$b + x$c)/2)-1)
}

# Tu función para particionar los conjuntos
betapart <- function(A,B) list(b=sum(!(A %in% B)), c=sum(!(B %in% A)), a=sum(B %in% A))

betapart.wi <- function(A,B) list(b=sum(!(A %in% B)), c=sum(!(B %in% A)), a=sum(B %in% A))

# Tu función betalink original (sin modificaciones)
betalink <- function(n1,n2,bf=B01){
  v1 <- igraph::V(n1)$name
  v2 <- igraph::V(n2)$name
  vs <- v1[v1 %in% v2] 
  beta_S <- bf(betapart(v1, v2))
  
  e1 <- plyr::aaply(igraph::get.edgelist(n1), 1, function(x) stringr::str_c(x[order(x)], collapse='--', paste='_'))
  e2 <- plyr::aaply(igraph::get.edgelist(n2), 1, function(x) stringr::str_c(x[order(x)], collapse='--', paste='_'))
  beta_WN <- bf(betapart(e1, e2))
  
  if(length(vs)>=2)
  {
    sn1 <- igraph::induced.subgraph(n1, which(igraph::V(n1)$name %in% vs))
    sn2 <- igraph::induced.subgraph(n2, which(igraph::V(n2)$name %in% vs))
    se1 <- plyr::aaply(igraph::get.edgelist(sn1), 1, function(x) stringr::str_c(x[order(x)], collapse='--', paste='_'))
    se2 <- plyr::aaply(igraph::get.edgelist(sn2), 1, function(x) stringr::str_c(x[order(x)], collapse='--', paste='_'))
    beta_OS <- bf(betapart(se1, se2))
    beta_ST <- beta_WN - beta_OS
  } else {
    beta_OS <- NaN
    beta_ST <- NaN
  }
  return(list(S = beta_S, OS = beta_OS, WN = beta_WN, ST = beta_ST))
}

## FUNCIÓN PARA PROCESAR FORMATO LARGO ##

#' @title Beta diversidad desde formato largo
#' @description Convierte dos data.frames en formato largo a igraph y mide beta diversidad
#' @param df1 Data.frame 1 (Columnas 1 y 2 deben ser los nodos/especies)
#' @param df2 Data.frame 2 (Columnas 1 y 2 deben ser los nodos/especies)
#' @param directed (boolean) Define si las interacciones tienen dirección
beta_largo <- function(df1, df2, directed = FALSE, bf = B01) {
  
  # graph_from_data_frame asume automáticamente que las primeras 
  # dos columnas representan los nodos que interactúan (from, to).
  red1_igraph <- igraph::graph_from_data_frame(df1, directed = directed)
  red2_igraph <- igraph::graph_from_data_frame(df2, directed = directed)
  
  # Aplicamos la función original de betalink
  resultado <- betalink(red1_igraph, red2_igraph, bf = bf)
  
  return(resultado)
}

calcular_baselga_focal <- function(plantas_R1, plantas_R2) {
  
  # Vectores de nombres de plantas utilizadas por la misma abeja
  plantas_R1 <- unique(plantas_R1)
  plantas_R2 <- unique(plantas_R2)
  
  # Excluir transiciones sin interacciones en alguna fecha
  if (length(plantas_R1) == 0 || length(plantas_R2) == 0) {
    return(data.frame(
      a = NA_integer_, b = NA_integer_, c = NA_integer_,
      BW = NA_real_, BSIM = NA_real_, BSNE = NA_real_
    ))
  }
  
  a <- length(intersect(plantas_R1, plantas_R2))
  b <- length(setdiff(plantas_R1, plantas_R2))
  c <- length(setdiff(plantas_R2, plantas_R1))
  
  # Whittaker / Sorensen binario
  BW <- (b + c) / (2 * a + b + c)
  
  # Baselga: componente de reemplazo
  BSIM <- min(b, c) / (a + min(b, c))
  
  # Baselga: componente de anidamiento resultante
  BSNE <- BW - BSIM
  
  data.frame(a = a, b = b, c = c,
             BW = BW, BSIM = BSIM, BSNE = BSNE)
}


# 2. Analisis por matriz----
## 2.1 Proyecto 79 Chacoff ----
proyecto <- "079_Chacoff"
print(paste("Inicio", proyecto))

RT.chachoff <- list.files(path="../00baseDatos/redestemproalesidactualizadomatricescuantitativass/79_1chacoff_2006 - redes temporales/",
           pattern = ".xlsx", full.names = TRUE)
chacof.l <- c()

for(i in RT.chachoff){
  chacof.i <- readxl::read_excel(i)
  print(dim(chacof.i))
  chacof.i.l <- pivot_longer(chacof.i, cols = 2:136)
  chacof.i.anho <- substr(i, nchar(i)-8, nchar(i)-5)
  chacof.i.l$anho <- chacof.i.anho
  chacof.l <- rbind(chacof.l,chacof.i.l)
  
}

head(chacof.l)
chacof <- subset(chacof.l, value!=0)

# Importante exlucion de año 2006 por diferencias metodologicas
chacof <- subset(chacof, anho!=2006)
names(chacof) <- c("plant_code","pol_code","value","year")
plantas <- read.csv("../00baseDatos/redestemproalesidactualizadomatricescuantitativass/79_1chacoff_2006 - redes temporales/11794503.v4 (Interacciones planta-polinizador (visitas a flores)/plant_names.csv")
insectos <- read.csv("../00baseDatos/redestemproalesidactualizadomatricescuantitativass/79_1chacoff_2006 - redes temporales/11794503.v4 (Interacciones planta-polinizador (visitas a flores)/insect_names.csv")

# abejas <- read.csv("ml.abejas.csv")
# abejas$species_name <- gsub(pattern = " ", replacement = "_", x = abejas$sp.armonizado)
# abejas$species_name2 <- substr(abejas$species_name,  pattern = " ", replacement = "_", x = abejas$sp.armonizado)

# library(rgbif)

# Buscar la familia (por ejemplo, Orchidaceae)

# Aplicar la función a cada nombre de la lista
# familias_info <- lapply(nombres.insectos, function(nombre) {
  # name_backbone(name = nombre, rank = "family")
# })

# Extraer los familyKey en un vector limpio, eliminando los que no se encontraron (NULL)
# familias_keys <- sapply(familias_info, function(x) x$familyName)
# familias_keys <- unlist(familias_keys)

# print(familias_keys)


# nombres.insectos <- insectos$species_name
# nombres.insectos <- nombres.insectos[nombres.insectos!=""]
# familia_info <- name_backbone(name = nombres.insectos, rank = "family" )

# Ver el ID de la familia
# familia_key <- familia_info$familyKey
# print(familia_key)


# insectos <- subset(insectos, is.element(species_name, abejas$species_name))
# setdiff( unique(abejas$species_name),unique(insectos$species_name))
# 
# head(chacof)
# head(plantas)
# head(insectos)

chacof.p <- merge(chacof, plantas, by="plant_code")
chacof.f <- merge(chacof.p, insectos, by="pol_code", suffixes = c("_plant","_pol"))

combi <- combinations(v = as.numeric(unique(chacof.f$year)), 
                      r = 2, 
                      n=length(unique(chacof.f$year)))

combi <- subset(combi, (combi[,2]-combi[,1])==1)

lista.beta <- list()
lista.beta.abeja <- list()
tabla.beta <- c()
tabla.beta.abeja <- c()

datos.rt <- chacof.f
datos.rt$species_name_pol <-str_to_sentence(chacof.f$species_name_pol)
datos.rt$species_name_plant <-str_to_sentence(chacof.f$species_name_plant)

datos.rt <- subset(datos.rt, species_name_pol!="")
baselga.periodo <- c()
baselga.full <- c()

### Ciclo para calculo de beta ----
for(i in 1:nrow(combi)){
  combi.i <- combi[i,]
  id.1 <- combi.i[1]
  id.2 <- combi.i[2]
  
  print(id.1)
  print(id.2)
  
  rt.i1 <- subset(datos.rt, year==id.1) 
  rt.i2 <- subset(datos.rt, year==id.2) 

  m1g <- graph_from_data_frame(rt.i1[,c(5,6)])
  m2g <- graph_from_data_frame(rt.i2[,c(5,6)])

  m1g.abeja <- graph_from_data_frame(rt.i1[,c(6,5)], directed = TRUE)
  m2g.abeja <- graph_from_data_frame(rt.i2[,c(6,5)])
  
  # lista.beta.i <- betalink(m1g, m2g, bf = Wi)
  lista.beta.i <- betalink(m2g, m1g, bf = Wi)
  tabla.beta.i <- as.data.frame(lista.beta.i)
  lista.beta <- c(lista.beta,lista.beta.i)
  
  tabla.beta <- rbind(tabla.beta,tabla.beta.i)
  
  lista.beta.i.abeja <- betalink(m1g, m2g, bf = B01)
  tabla.beta.i.abeja <- as.data.frame(lista.beta.i.abeja)
  lista.beta.abeja <- c(lista.beta.abeja,lista.beta.i.abeja)
  
  tabla.beta.abeja <- rbind(tabla.beta.abeja,tabla.beta.i.abeja)
  
  # pollinators <- V(m1g.abeja)[V(m1g.abeja)$type == TRUE]$name
  print(betalink(m1g, m2g, bf = Wi))
  
  # Baselga por especie de polinizador para el periodo
  pol_species1 <- unique(rt.i1[, 5])
  V(m1g.abeja)$type <- ! (V(m1g.abeja)$name %in% pol_species1)
  
  pol_species2 <- unique(rt.i2[, 5])
  V(m2g.abeja)$type <- ! (V(m2g.abeja)$name %in% pol_species2)
  
  pol1 <- V(m1g.abeja)[V(m1g.abeja)$type == TRUE]$name
  pol2 <- V(m2g.abeja)[V(m2g.abeja)$type == TRUE]$name
  
  pol1y2 <- sort(intersect(pol1, pol2))
  
  for (pol.j in pol1y2) {
    # pol.j <- "Alloscirtetica_sp1"
    
    # Especies de plantas en t1 y t2 para el polinizador j
    plantas.1 <- names(neighbors(m1g.abeja, pol.j))
    plantas.2 <- names(neighbors(m2g.abeja, pol.j))
    
    # Baselga para el periodo y polinizador j
    baselga.j <- calcular_baselga_focal(plantas.1, plantas.2) 
    baselga.j$t1 <- id.1
    baselga.j$t2 <- id.2
    baselga.j$esp <- pol.j
    
    baselga.periodo <- rbind(baselga.periodo, baselga.j)        
  }
  
  # baselga.full <- rbind(baselga.full, baselga.periodo)
}


tabla.beta$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta$periodo.n <- as.numeric(rownames(tabla.beta))

tabla.beta.abeja$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta.abeja$periodo.n <- as.numeric(rownames(tabla.beta.abeja))

### Figura 1 ----
png(paste0("../03figuras/Betadiversidad/Betapart_",proyecto,".png"))
par(mfrow=(c(2,2)))
plot(S~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Turnover S")

plot(OS~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Rewiring OS")

plot(WN~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Whole network WN")

plot(ST~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Asociado a Turnover ST")


dev.off()


# par(mfrow=c(1,2))

### Figura 2 ----
png(paste0("../03figuras/Betadiversidad/TrayectoriaBetadiversidad",proyecto,".png"), width = 1280, height = 720)
plot(WN~periodo.n, tabla.beta, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta$periodo.n+0.2)),
     main="Trayectoría de Betadiversidad Chacoff 79")
     

axis(1, at = c(1:max(tabla.beta$periodo.n)), labels = unique(tabla.beta$periodo))


lines(x=tabla.beta$periodo.n, y=tabla.beta$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$OS, type=c("b"), ylim=c(0,1),
       col=3, lty=3, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$ST, type=c("b"), ylim=c(0,1),
       col=4, lty=4, pch=16)

legend(col=c(1,2,3,4), legend=c("WN","S","OS","ST"), "topright", lty=c(1,2,3,4), pch=16, seg.len = 5)
dev.off()

### Tabla 1----
write.csv(x = tabla.beta, file = "tabla.beta79.csv", row.names = FALSE)

head(chacof.f)

baselga.periodo <- baselga.periodo[order(baselga.periodo$esp),]
baselga.periodo$periodo <- paste(baselga.periodo$t1, baselga.periodo$t2, sep="-")
xyplot(BSIM~factor(periodo)|esp, 
       layout=c(4,4), 
       type=c("b"), data=baselga.periodo, 
       xlab="Periodo",
       as.table=TRUE)

head(baselga.periodo)

histogram(~BSIM|esp, 
       type="count",
          data=baselga.periodo, layout=c(1,1), as.table=TRUE)


histogram(~BSIM|periodo, 
          type="count",
          data=baselga.periodo, layout=c(1,4), as.table=TRUE)


baselga.periodo.filtrado <- subset(baselga.periodo, BSIM != 0 & BSIM!=1)
histogram(~BSIM|periodo, 
          type="count",
          data=baselga.periodo.filtrado, layout=c(1,4), as.table=TRUE)


head(baselga.periodo)

ml.abejas <- read.csv("../02salidas/ml.abejas.csv")
head(ml.abejas)

ml.abejas$polinizador <- gsub(pattern = " ", replacement = "_", 
                              x =ml.abejas$sp.armonizado)


baselga.familia <- merge(baselga.periodo, ml.abejas[,c("polinizador","familia")], 
      by.y="polinizador", 
      by.x="esp")
baselga.familia <- baselga.familia[order(baselga.familia$periodo),]
baselga.familia <- unique(baselga.familia)

### Tabla 2 ----
write.csv(file = paste0("../02salidas/baselga.",proyecto,"especie.csv"),
          x = baselga.familia, row.names = FALSE)

### Figura 3 ----
png(paste0("../03figuras/Betadiversidad/BSIM.Familias.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|familia, 
       layout=c(1,5),
       group=esp,
       type=c("b","g"), data=baselga.familia, 
       xlab="Periodo",
       as.table=TRUE)
dev.off()

### Figura 4 ----
pdf(paste0("../03figuras/Betadiversidad/BSIM.Abejas.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|interaction(esp,  familia, drop = TRUE), 
       layout=c(1,5),
       # group=esp,
       type=c("b","g"), data=baselga.familia, 
       xlab="Periodo",
       as.table=TRUE)
dev.off()

### Figura 5 ----
png(paste0("../03figuras/Betadiversidad/BSIM.Andrenidae.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|interaction(esp, drop = TRUE), 
       layout=c(1,2),
       main="Andrenidae",
       # group=esp,
       type=c("b","g"), data=subset(baselga.familia, familia=="Andrenidae"), 
       xlab="Periodo",
       as.table=TRUE)
dev.off()

png(paste0("../03figuras/Betadiversidad/BSIM.Apidae.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|interaction(esp, drop = TRUE), 
       layout=c(6,2),
       main="Apidae",
       # group=esp,
       type=c("b","g"), 
       data=subset(baselga.familia, familia=="Apidae"), 
       xlab="Periodo",
       as.table=TRUE)
dev.off()

png(paste0("../03figuras/Betadiversidad/BSIM.Colletidae.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|interaction(esp, drop = TRUE), 
       main="Colletidae",
       layout=c(4,2),
       # group=esp,
       type=c("b","g"), 
       data=subset(baselga.familia, familia=="Colletidae"), 
       xlab="Periodo",
       as.table=TRUE)
dev.off()

png(paste0("../03figuras/Betadiversidad/BSIM.Halictidae.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|interaction(esp, drop = TRUE), 
       main="Halictidae",
       layout=c(4,1),
       # group=esp,
       type=c("b","g"), 
       data=subset(baselga.familia, familia=="Halictidae"), 
       xlab="Periodo",
       as.table=TRUE)
dev.off()

png(paste0("../03figuras/Betadiversidad/BSIM.Megachilidae.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|interaction(esp, drop = TRUE), 
       layout=c(3,2),
       main="Megachilidae",
       # group=esp,
       type=c("b","g"), 
       data=subset(baselga.familia, familia=="Megachilidae"), 
       xlab="Periodo",
       as.table=TRUE)
dev.off()


## 2.2 Proyecto 60A Kaiser Bunbury ----
print("Inicio Kaiser060A")
proyecto <- "060.1_KaiserBunbury"
RT.Kaise60 <- list.files(path="../00baseDatos/redestemproalesidactualizadomatricescuantitativass/M_PL_060_KBunbury_temporales/",
                          pattern = ".csv", full.names = TRUE)

RT.Kaise60 <- RT.Kaise60[!(RT.Kaise60 %in% grepv(pattern = "species", x = RT.Kaise60))]
kaiser60.l <- c()

for(i in RT.Kaise60){
  kaiser60.i <- read.csv(i)
  print(dim(kaiser60.i))
  kaiser60.i.l <- pivot_longer(kaiser60.i, cols = 2:ncol(kaiser60.i))
  kaiser60.i.t <- substr(i, nchar(i)-5, nchar(i)-4)
  kaiser60.i.l$t <- kaiser60.i.t
  kaiser60.l <- rbind(kaiser60.l,kaiser60.i.l)
  
}

head(kaiser60.l)
kaiser60 <- subset(kaiser60.l, value!=0)
kaiser60A <- subset(kaiser60, t <= 12)

kaiser60 <- kaiser60A

names(kaiser60) <- c("plant_code","pol_code","value","t")
kaiser60$pol_code <- gsub(pattern = "\\.$", replacement = "", x = kaiser60$pol_code)
kaiser60$pol_code <- gsub(pattern = "[.]", replacement = "_", x = kaiser60$pol_code)

abejas <- read.csv("ml.abejas.csv")
abejas <- subset(abejas, ID=="M_060")
abejas$species_name <- gsub(pattern = " ", replacement = "_", x = abejas$sp.armonizado)
# abejas$species_name2 <- substr(abejas$species_name,  pattern = " ", replacement = "_", x = abejas$sp.armonizado)


sort(union(setdiff(unique(kaiser60$pol_code), unique(abejas$species_name)),
     setdiff(unique(abejas$species_name),unique(kaiser60$pol_code))))

Kaiser60.f <- subset(kaiser60, pol_code %in% abejas$species_name)
# Kaiser60.f <- subset(Kaiser60.f, ID=="M_060")

combi <- combinations(v = as.numeric(unique(Kaiser60.f$t)), 
                      r = 2, 
                      n=length(unique(Kaiser60.f$t)))

combi <- subset(combi, (combi[,2]-combi[,1])==1)

lista.beta <- list()
lista.beta.abeja <- list()
tabla.beta <- c()
tabla.beta.abeja <- c()

datos.rt <- Kaiser60.f
datos.rt$t <- as.numeric(datos.rt$t)

datos.rt$pol_code <- sub(" *$", "", datos.rt$pol_code)

datos.rt$plant_code <-sub(" *$", "", datos.rt$plant_code)
datos.rt$plant_code <-sub(" *$", "", datos.rt$plant_code)
datos.rt$plant_code <- gsub(pattern = " ", replacement = "_", x = datos.rt$plant_code)

# datos.rt <- subset(datos.rt, species_name_pol!="")

baselga.periodo <- c()
baselga.full <- c()

### Ciclo para calculo beta ----
for(i in 1:nrow(combi)){
  combi.i <- combi[i,]
  id.1 <- combi.i[1]
  id.2 <- combi.i[2]
  
  print(id.1)
  print(id.2)
  
  rt.i1 <- subset(datos.rt, t==id.1) 
  rt.i2 <- subset(datos.rt, t==id.2) 
  
  m1g <- graph_from_data_frame(rt.i1[,c(1,2)])
  m2g <- graph_from_data_frame(rt.i2[,c(1,2)])
  
  
  m1g.abeja <- graph_from_data_frame(rt.i1[,c(2,1)])
  m2g.abeja <- graph_from_data_frame(rt.i2[,c(2,1)])
  
  # lista.beta.i <- betalink(m1g, m2g, bf = Wi)
  lista.beta.i <- betalink(m2g, m1g, bf = Wi)
  tabla.beta.i <- as.data.frame(lista.beta.i)
  lista.beta <- c(lista.beta,lista.beta.i)
  
  tabla.beta <- rbind(tabla.beta,tabla.beta.i)
  
  lista.beta.i.abeja <- betalink(m1g, m2g, bf = B01)
  tabla.beta.i.abeja <- as.data.frame(lista.beta.i.abeja)
  lista.beta.abeja <- c(lista.beta.abeja,lista.beta.i.abeja)
  
  tabla.beta.abeja <- rbind(tabla.beta.abeja,tabla.beta.i.abeja)
  
  
  print(betalink(m1g, m2g, bf = Wi))
  
  
  
  # Baselga por especie de polinizador para el periodo
  pol_species1 <- unique(rt.i1[, "species_name_pol"])
  V(m1g.abeja)$type <- ! (V(m1g.abeja)$name %in% pol_species1)
  
  pol_species2 <- unique(rt.i2[, "species_name_pol"])
  V(m2g.abeja)$type <- ! (V(m2g.abeja)$name %in% pol_species2)
  
  pol1 <- V(m1g.abeja)[V(m1g.abeja)$type == TRUE]$name
  pol2 <- V(m2g.abeja)[V(m2g.abeja)$type == TRUE]$name
  
  pol1y2 <- sort(intersect(pol1, pol2))
  
  for (pol.j in pol1y2) {
    # pol.j <- "Alloscirtetica_sp1"
    
    # Especies de plantas en t1 y t2 para el polinizador j
    plantas.1 <- names(neighbors(m1g.abeja, pol.j))
    plantas.2 <- names(neighbors(m2g.abeja, pol.j))
    
    # Baselga para el periodo y polinizador j
    baselga.j <- calcular_baselga_focal(plantas.1, plantas.2) 
    baselga.j$t1 <- id.1
    baselga.j$t2 <- id.2
    baselga.j$esp <- pol.j
    
    baselga.periodo <- rbind(baselga.periodo, baselga.j)        
  }
  
  
  
}

library(lattice)
library(psych)

tabla.beta$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta$periodo.n <- as.numeric(rownames(tabla.beta))

tabla.beta.abeja$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta.abeja$periodo.n <- as.numeric(rownames(tabla.beta.abeja))

### Figura 1----
png(paste0("../03figuras/Betadiversidad/Betapart_",proyecto,".png"))
par(mfrow=(c(2,2)))
plot(S~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Turnover S")

plot(OS~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Rewiring OS")

plot(WN~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Whole network WN")

plot(ST~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Asociado a Turnover ST")


dev.off()
dev.off()

### Figura 2 ----
png(paste0("../03figuras/Betadiversidad/TrayectoriaBetadiversidad",proyecto,".png"), width = 1280, height = 720)
plot(WN~periodo.n, tabla.beta, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta$periodo.n+0.2)),
     main="Trayectoría de Betadiversidad Kaiser 60")


axis(1, at = c(1:max(tabla.beta$periodo.n)), labels = unique(tabla.beta$periodo))


lines(x=tabla.beta$periodo.n, y=tabla.beta$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$OS, type=c("b"), ylim=c(0,1),
      col=3, lty=3, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$ST, type=c("b"), ylim=c(0,1),
      col=4, lty=4, pch=16)

legend(col=c(1,2,3,4), legend=c("WN","S","OS","ST"), "topright", lty=c(1,2,3,4), pch=16, seg.len = 5)
dev.off()


### Tabla 1 ----
write.csv(x = tabla.beta, file = "tabla.beta60.csv", row.names = FALSE)

baselga.periodo <- baselga.periodo[order(baselga.periodo$esp),]
baselga.periodo$periodo <- paste(baselga.periodo$t1, baselga.periodo$t2, sep="-")
xyplot(BSIM~factor(periodo)|esp, 
       layout=c(4,4), 
       type=c("b"), data=baselga.periodo, 
       xlab="Periodo",
       as.table=TRUE)



head(baselga.periodo)

histogram(~BSIM|esp, 
          type="count",
          data=baselga.periodo, layout=c(1,1), as.table=TRUE)


histogram(~BSIM|periodo, 
          type="count",
          data=baselga.periodo, layout=c(1,4), as.table=TRUE)


baselga.periodo.filtrado <- subset(baselga.periodo, BSIM != 0 & BSIM!=1)
histogram(~BSIM|periodo, 
          type="count",
          data=baselga.periodo.filtrado, layout=c(1,4), as.table=TRUE)


head(baselga.periodo)

ml.abejas <- read.csv("../02salidas/ml.abejas.csv")
head(ml.abejas)

ml.abejas$polinizador <- gsub(pattern = " ", replacement = "_", 
                              x =ml.abejas$sp.armonizado)


baselga.familia <- merge(baselga.periodo, ml.abejas[,c("polinizador","familia")], 
                         by.y="polinizador", 
                         by.x="esp")
baselga.familia <- baselga.familia[order(baselga.familia$periodo),]
baselga.familia <- unique(baselga.familia)
### Tabla 2 ----
write.csv(file = paste0("../02salidas/baselga.",proyecto,"especie.csv"),
          x = baselga.familia, row.names = FALSE)


png(paste0("../03figuras/Betadiversidad/BSIM.Familias.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|familia, 
       layout=c(1,5),
       group=esp,
       type=c("b","g"), data=baselga.familia, 
       xlab="Periodo",
       as.table=TRUE)
dev.off()

pdf(paste0("../03figuras/Betadiversidad/BSIM.Abejas.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|interaction(esp,  familia, drop = TRUE), 
       layout=c(1,5),
       # group=esp,
       type=c("b","g"), data=baselga.familia, 
       xlab="Periodo",
       as.table=TRUE)
dev.off()

png(paste0("../03figuras/Betadiversidad/BSIM.Andrenidae.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|interaction(esp, drop = TRUE), 
       layout=c(1,2),
       main="Andrenidae",
       # group=esp,
       type=c("b","g"), data=subset(baselga.familia, familia=="Andrenidae"), 
       xlab="Periodo",
       as.table=TRUE)
dev.off()

png(paste0("../03figuras/Betadiversidad/BSIM.Apidae.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|interaction(esp, drop = TRUE), 
       layout=c(6,2),
       main="Apidae",
       # group=esp,
       type=c("b","g"), 
       data=subset(baselga.familia, familia=="Apidae"), 
       xlab="Periodo",
       as.table=TRUE)
dev.off()

png(paste0("../03figuras/Betadiversidad/BSIM.Colletidae.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|interaction(esp, drop = TRUE), 
       main="Colletidae",
       layout=c(4,2),
       # group=esp,
       type=c("b","g"), 
       data=subset(baselga.familia, familia=="Colletidae"), 
       xlab="Periodo",
       as.table=TRUE)
dev.off()

png(paste0("../03figuras/Betadiversidad/BSIM.Halictidae.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|interaction(esp, drop = TRUE), 
       main="Halictidae",
       layout=c(4,1),
       # group=esp,
       type=c("b","g"), 
       data=subset(baselga.familia, familia=="Halictidae"), 
       xlab="Periodo",
       as.table=TRUE)
dev.off()

png(paste0("../03figuras/Betadiversidad/BSIM.Megachilidae.",proyecto,".png"))
xyplot(BSIM~factor(periodo)|interaction(esp, drop = TRUE), 
       layout=c(3,2),
       main="Megachilidae",
       # group=esp,
       type=c("b","g"), 
       data=subset(baselga.familia, familia=="Megachilidae"), 
       xlab="Periodo",
       as.table=TRUE)
dev.off()
## 2.3 Proyecto 60B Kaiser Bunbury ----
print("Inicio Kaiser060")
RT.Kaise60 <- list.files(path="../00baseDatos/redestemproalesidactualizadomatricescuantitativass/M_PL_060_KBunbury_temporales/",
                         pattern = ".csv", full.names = TRUE)

RT.Kaise60 <- RT.Kaise60[!(RT.Kaise60 %in% grepv(pattern = "species", x = RT.Kaise60))]
kaiser60.l <- c()

for(i in RT.Kaise60){
  kaiser60.i <- read.csv(i)
  print(dim(kaiser60.i))
  kaiser60.i.l <- pivot_longer(kaiser60.i, cols = 2:ncol(kaiser60.i))
  kaiser60.i.t <- substr(i, nchar(i)-5, nchar(i)-4)
  kaiser60.i.l$t <- kaiser60.i.t
  kaiser60.l <- rbind(kaiser60.l,kaiser60.i.l)
  
}

head(kaiser60.l)
kaiser60 <- subset(kaiser60.l, value!=0)
names(kaiser60) <- c("plant_code","pol_code","value","t")
kaiser60$pol_code <- gsub(pattern = "\\.$", replacement = "", x = kaiser60$pol_code)
kaiser60$pol_code <- gsub(pattern = "[.]", replacement = "_", x = kaiser60$pol_code)

abejas <- read.csv("ml.abejas.csv")
abejas <- subset(abejas, ID=="M_060")
abejas$species_name <- gsub(pattern = " ", replacement = "_", x = abejas$sp.armonizado)
# abejas$species_name2 <- substr(abejas$species_name,  pattern = " ", replacement = "_", x = abejas$sp.armonizado)


sort(union(setdiff(unique(kaiser60$pol_code), unique(abejas$species_name)),
           setdiff(unique(abejas$species_name),unique(kaiser60$pol_code))))

Kaiser60.f <- subset(kaiser60, pol_code %in% abejas$species_name)
# Kaiser60.f <- subset(Kaiser60.f, ID=="M_060")

combi <- combinations(v = as.numeric(unique(Kaiser60.f$t)), 
                      r = 2, 
                      n=length(unique(Kaiser60.f$t)))

combi <- subset(combi, (combi[,2]-combi[,1])==1)

lista.beta <- list()
lista.beta.abeja <- list()
tabla.beta <- c()
tabla.beta.abeja <- c()

datos.rt <- Kaiser60.f
datos.rt$t <- as.numeric(datos.rt$t)
baselga.periodo <- c()
baselga.full <- c()

### Ciclo para calculo beta ----
for(i in 1:nrow(combi)){
  combi.i <- combi[i,]
  id.1 <- combi.i[1]
  id.2 <- combi.i[2]
  
  print(id.1)
  print(id.2)
  
  rt.i1 <- subset(datos.rt, t==id.1) 
  rt.i2 <- subset(datos.rt, t==id.2) 
  
  m1g <- graph_from_data_frame(rt.i1[,c(1,2)])
  m2g <- graph_from_data_frame(rt.i2[,c(1,2)])
  
  
  m1g.abeja <- graph_from_data_frame(rt.i1[,c(2,1)])
  m2g.abeja <- graph_from_data_frame(rt.i2[,c(2,1)])
  
  # lista.beta.i <- betalink(m1g, m2g, bf = Wi)
  lista.beta.i <- betalink(m2g, m1g, bf = Wi)
  tabla.beta.i <- as.data.frame(lista.beta.i)
  lista.beta <- c(lista.beta,lista.beta.i)
  
  tabla.beta <- rbind(tabla.beta,tabla.beta.i)
  
  lista.beta.i.abeja <- betalink(m1g, m2g, bf = B01)
  tabla.beta.i.abeja <- as.data.frame(lista.beta.i.abeja)
  lista.beta.abeja <- c(lista.beta.abeja,lista.beta.i.abeja)
  
  tabla.beta.abeja <- rbind(tabla.beta.abeja,tabla.beta.i.abeja)
  
  
  print(betalink(m1g, m2g, bf = Wi))
  
}

library(lattice)
library(psych)

tabla.beta$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta$periodo.n <- as.numeric(rownames(tabla.beta))

tabla.beta.abeja$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta.abeja$periodo.n <- as.numeric(rownames(tabla.beta.abeja))


par(mfrow=(c(2,2)))
plot(S~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Turnover S")

plot(OS~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Rewiring OS")

plot(WN~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Whole network WN")

plot(ST~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Asociado a Turnover ST")


dev.off()




png("TrayectoriaBetadiversidadKaiser060.png", width = 1280, height = 720)
plot(WN~periodo.n, tabla.beta, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta$periodo.n+0.2)),
     main="Trayectoría de Betadiversidad Kaiser 60")


axis(1, at = c(1:max(tabla.beta$periodo.n)), labels = unique(tabla.beta$periodo))


lines(x=tabla.beta$periodo.n, y=tabla.beta$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$OS, type=c("b"), ylim=c(0,1),
      col=3, lty=3, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$ST, type=c("b"), ylim=c(0,1),
      col=4, lty=4, pch=16)

legend(col=c(1,2,3,4), legend=c("WN","S","OS","ST"), "topright", lty=c(1,2,3,4), pch=16, seg.len = 5)
dev.off()

write.csv(x = tabla.beta, file = "tabla.beta60.csv", row.names = FALSE)

plot(WN~periodo.n, tabla.beta.abeja, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta.abeja$periodo.n+0.2)
            
     ))

axis(1, at = c(1:max(tabla.beta.abeja$periodo.n)), labels = unique(tabla.beta.abeja$periodo))


lines(x=tabla.beta.abeja$periodo.n, y=tabla.beta.abeja$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta.abeja$periodo.n, y=tabla.beta.abeja$OS, type=c("b"), ylim=c(0,1),
      col=3, lty=3, pch=16)

lines(x=tabla.beta.abeja$periodo.n, y=tabla.beta.abeja$ST, type=c("b"), ylim=c(0,1),
      col=4, lty=4, pch=16)

## 2.4 Proyecto 61 Kaiser Bunbury ----
print("Inicio Kaiser061")
RT.Kaise61 <- list.files(path="../00baseDatos/redestemproalesidactualizadomatricescuantitativass/M_PL_061_KBunbury_2014_Temporales",
                         pattern = ".csv", full.names = TRUE)

kaiser61.l <- c()



for(i in RT.Kaise61){
  kaiser61.i <- read.csv(i)
  print(dim(kaiser61.i))
  kaiser61.i.l <- pivot_longer(kaiser61.i, cols = 2:ncol(kaiser61.i))
  kaiser61.i.t <- substr(i, nchar(i)-5, nchar(i)-4)
  kaiser61.i.l$t <- kaiser61.i.t
  kaiser61.l <- rbind(kaiser61.l, kaiser61.i.l)
  
}
 
head(kaiser61.l)
kaiser61 <- subset(kaiser61.l, value!=0)

names(kaiser61) <- c("plant_code","pol_code","value","t")
kaiser61$pol_code <- gsub(pattern = "\\.$", replacement = "", x = kaiser61$pol_code)
kaiser61$pol_code <- gsub(pattern = "[.]", replacement = "_", x = kaiser61$pol_code)


kaiser61$t <- as.numeric(kaiser61$t)
kaiser61.1 <- subset(kaiser61, t <= 8)
kaiser61.1$ID <- "M_061.1"

kaiser61.2 <- subset(kaiser61, t > 8 & t <= 16)
kaiser61.2$ID <- "M_061.2"

kaiser61.3 <- subset(kaiser61, t > 16  & t <= 24)
kaiser61.3$ID <- "M_061.3"

kaiser61.4 <- subset(kaiser61, t > 24  & t <= 32)
kaiser61.4$ID <- "M_061.4"

kaiser61.5 <- subset(kaiser61, t >  32 & t <= 40)
kaiser61.5$ID <- "M_061.5"

kaiser61.6 <- subset(kaiser61, t >  40 & t <= 48)
kaiser61.6$ID <- "M_061.6"



abejas <- read.csv("ml.abejas.csv")
abejas <- subset(abejas, ID=="M_061")
abejas$species_name <- gsub(pattern = " ", replacement = "_", x = abejas$sp.armonizado)
# abejas$species_name2 <- substr(abejas$species_name,  pattern = " ", replacement = "_", x = abejas$sp.armonizado)


sort(union(setdiff(unique(kaiser61$pol_code), unique(abejas$species_name)),
           setdiff(unique(abejas$species_name),unique(kaiser61$pol_code))))



## 61.1 ----
proyecto <- "061.1_KaiserBunbury"
Kaiser61.f <- subset(kaiser61.1, pol_code %in% abejas$species_name)
# Kaiser60.f <- subset(Kaiser60.f, ID=="M_060")

combi <- combinations(v = as.numeric(unique(Kaiser61.f$t)), 
                      r = 2, 
                      n=length(unique(Kaiser61.f$t)))

combi <- subset(combi, (combi[,2]-combi[,1])==1)

lista.beta <- list()
lista.beta.abeja <- list()
tabla.beta <- c()
tabla.beta.abeja <- c()

datos.rt <- Kaiser61.f
datos.rt$species_name_pol <-str_to_sentence(datos.rt$pol_code)
datos.rt$species_name_plant <-str_to_sentence(datos.rt$plant_code)

datos.rt <- subset(datos.rt, species_name_pol!="")
baselga.periodo <- c()
baselga.full <- c()

### Ciclo para calculo de beta ----
for(i in 1:nrow(combi)){
  combi.i <- combi[i,]
  id.1 <- combi.i[1]
  id.2 <- combi.i[2]
  
  print(id.1)
  print(id.2)
  
  rt.i1 <- subset(datos.rt, t==id.1) 
  rt.i2 <- subset(datos.rt, t==id.2) 
  
  m1g <- graph_from_data_frame(rt.i1[,c("species_name_plant","species_name_pol")])
  m2g <- graph_from_data_frame(rt.i2[,c("species_name_plant","species_name_pol")])
  
  m1g.abeja <- graph_from_data_frame(rt.i1[,c("species_name_pol","species_name_plant")], directed = TRUE)
  m2g.abeja <- graph_from_data_frame(rt.i2[,c("species_name_pol","species_name_plant")], directed = TRUE)
  
  # lista.beta.i <- betalink(m1g, m2g, bf = Wi)
  lista.beta.i <- betalink(m2g, m1g, bf = Wi)
  tabla.beta.i <- as.data.frame(lista.beta.i)
  lista.beta <- c(lista.beta,lista.beta.i)
  
  tabla.beta <- rbind(tabla.beta,tabla.beta.i)
  
  lista.beta.i.abeja <- betalink(m1g, m2g, bf = B01)
  tabla.beta.i.abeja <- as.data.frame(lista.beta.i.abeja)
  lista.beta.abeja <- c(lista.beta.abeja,lista.beta.i.abeja)
  
  tabla.beta.abeja <- rbind(tabla.beta.abeja,tabla.beta.i.abeja)
  
  # pollinators <- V(m1g.abeja)[V(m1g.abeja)$type == TRUE]$name
  print(betalink(m1g, m2g, bf = Wi))
  
  # Baselga por especie de polinizador para el periodo
  pol_species1 <- unique(rt.i1[, "species_name_pol"])
  V(m1g.abeja)$type <- ! (V(m1g.abeja)$name %in% pol_species1)
  
  pol_species2 <- unique(rt.i2[, "species_name_pol"])
  V(m2g.abeja)$type <- ! (V(m2g.abeja)$name %in% pol_species2)
  
  pol1 <- V(m1g.abeja)[V(m1g.abeja)$type == TRUE]$name
  pol2 <- V(m2g.abeja)[V(m2g.abeja)$type == TRUE]$name
  # 
  # pol1y2 <- sort(intersect(pol1, pol2))
  pol1y2 <- sort(intersect(unique(pol_species1$species_name_pol),
                           unique(pol_species2$species_name_pol)))
  
  for (pol.j in pol1y2) {
    # pol.j <- "Alloscirtetica_sp1"
    
    # Especies de plantas en t1 y t2 para el polinizador j
    plantas.1 <- names(neighbors(m1g.abeja, pol.j))
    plantas.2 <- names(neighbors(m2g.abeja, pol.j))
    
    # Baselga para el periodo y polinizador j
    baselga.j <- calcular_baselga_focal(plantas.1, plantas.2) 
    baselga.j$t1 <- id.1
    baselga.j$t2 <- id.2
    baselga.j$esp <- pol.j
    
    baselga.periodo <- rbind(baselga.periodo, baselga.j)        
  }
  
  # baselga.full <- rbind(baselga.full, baselga.periodo)
}


tabla.beta$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta$periodo.n <- as.numeric(rownames(tabla.beta))

tabla.beta.abeja$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta.abeja$periodo.n <- as.numeric(rownames(tabla.beta.abeja))

### Figura 1 ----
png(paste0("../03figuras/Betadiversidad/Betapart_",proyecto,".png"))
par(mfrow=(c(2,2)))
plot(S~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Turnover S")

plot(OS~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Rewiring OS")

plot(WN~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Whole network WN")

plot(ST~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Asociado a Turnover ST")


dev.off()


# par(mfrow=c(1,2))

### Figura 2 ----
png(paste0("../03figuras/Betadiversidad/TrayectoriaBetadiversidad",proyecto,".png"), width = 1280, height = 720)
plot(WN~periodo.n, tabla.beta, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta$periodo.n+0.2)),
     main="Trayectoría de Betadiversidad 061.1 KB")


axis(1, at = c(1:max(tabla.beta$periodo.n)), labels = unique(tabla.beta$periodo))


lines(x=tabla.beta$periodo.n, y=tabla.beta$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$OS, type=c("b"), ylim=c(0,1),
      col=3, lty=3, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$ST, type=c("b"), ylim=c(0,1),
      col=4, lty=4, pch=16)

legend(col=c(1,2,3,4), legend=c("WN","S","OS","ST"), "topright", lty=c(1,2,3,4), pch=16, seg.len = 5)
dev.off()

### Tabla 1----
tabla.beta$ID <- unique(datos.rt$ID)
write.csv(x = tabla.beta, file = paste0("tabla.beta",proyecto,".csv"), row.names = FALSE)

head(datos.rt)

baselga.periodo <- baselga.periodo[order(baselga.periodo$esp),]
baselga.periodo$periodo <- paste(baselga.periodo$t1, baselga.periodo$t2, sep="-")
xyplot(BSIM~factor(periodo)|esp, 
       layout=c(1,4), 
       type=c("b"), data=baselga.periodo, 
       xlab="Periodo",
       as.table=TRUE)

head(baselga.periodo)

histogram(~BSIM|esp, 
          type="count",
          data=baselga.periodo, layout=c(1,1), as.table=TRUE)


histogram(~BSIM|periodo, 
          type="count",
          data=baselga.periodo, layout=c(1,4), as.table=TRUE)


head(baselga.periodo)

ml.abejas <- read.csv("../02salidas/ml.abejas.csv")
head(ml.abejas)

ml.abejas$polinizador <- gsub(pattern = " ", replacement = "_", 
                              x =ml.abejas$sp.armonizado)


baselga.familia <- merge(baselga.periodo, ml.abejas[,c("polinizador","familia")], 
                         by.y="polinizador", 
                         by.x="esp")
baselga.familia <- baselga.familia[order(baselga.familia$periodo),]
baselga.familia <- unique(baselga.familia)

### Tabla 2 ----
write.csv(file = paste0("../02salidas/baselga.",proyecto,"especie.csv"),
          x = baselga.familia, row.names = FALSE)


## 61.2 ----
proyecto <- "061.2_KaiserBunbury"
Kaiser61.f <- subset(kaiser61.2, pol_code %in% abejas$species_name)
# Kaiser60.f <- subset(Kaiser60.f, ID=="M_060")
Kaiser61.f$t <- Kaiser61.f$t-8
combi <- combinations(v = as.numeric(unique(Kaiser61.f$t)), 
                      r = 2, 
                      n=length(unique(Kaiser61.f$t)))

combi <- subset(combi, (combi[,2]-combi[,1])==1)

lista.beta <- list()
lista.beta.abeja <- list()
tabla.beta <- c()
tabla.beta.abeja <- c()

datos.rt <- Kaiser61.f
datos.rt$species_name_pol <-str_to_sentence(datos.rt$pol_code)
datos.rt$species_name_plant <-str_to_sentence(datos.rt$plant_code)

datos.rt <- subset(datos.rt, species_name_pol!="")

baselga.periodo <- c()
baselga.full <- c()

### Ciclo para calculo de beta ----
for(i in 1:nrow(combi)){
  combi.i <- combi[i,]
  id.1 <- combi.i[1]
  id.2 <- combi.i[2]
  
  print(id.1)
  print(id.2)
  
  rt.i1 <- subset(datos.rt, t==id.1) 
  rt.i2 <- subset(datos.rt, t==id.2) 
  
  m1g <- graph_from_data_frame(rt.i1[,c("species_name_plant","species_name_pol")])
  m2g <- graph_from_data_frame(rt.i2[,c("species_name_plant","species_name_pol")])
  
  m1g.abeja <- graph_from_data_frame(rt.i1[,c("species_name_pol","species_name_plant")], directed = TRUE)
  m2g.abeja <- graph_from_data_frame(rt.i2[,c("species_name_pol","species_name_plant")], directed = TRUE)
  
  # lista.beta.i <- betalink(m1g, m2g, bf = Wi)
  lista.beta.i <- betalink(m2g, m1g, bf = Wi)
  tabla.beta.i <- as.data.frame(lista.beta.i)
  lista.beta <- c(lista.beta,lista.beta.i)
  
  tabla.beta <- rbind(tabla.beta,tabla.beta.i)
  
  lista.beta.i.abeja <- betalink(m1g, m2g, bf = B01)
  tabla.beta.i.abeja <- as.data.frame(lista.beta.i.abeja)
  lista.beta.abeja <- c(lista.beta.abeja,lista.beta.i.abeja)
  
  tabla.beta.abeja <- rbind(tabla.beta.abeja,tabla.beta.i.abeja)
  
  # pollinators <- V(m1g.abeja)[V(m1g.abeja)$type == TRUE]$name
  print(betalink(m1g, m2g, bf = Wi))
  
  # Baselga por especie de polinizador para el periodo
  pol_species1 <- unique(rt.i1[, "species_name_pol"])
  V(m1g.abeja)$type <- ! (V(m1g.abeja)$name %in% pol_species1)
  
  pol_species2 <- unique(rt.i2[, "species_name_pol"])
  V(m2g.abeja)$type <- ! (V(m2g.abeja)$name %in% pol_species2)
  
  pol1 <- V(m1g.abeja)[V(m1g.abeja)$type == TRUE]$name
  pol2 <- V(m2g.abeja)[V(m2g.abeja)$type == TRUE]$name
  # 
  # pol1y2 <- sort(intersect(pol1, pol2))
  pol1y2 <- sort(intersect(unique(pol_species1$species_name_pol),
                           unique(pol_species2$species_name_pol)))
  
  for (pol.j in pol1y2) {
    # pol.j <- "Alloscirtetica_sp1"
    
    # Especies de plantas en t1 y t2 para el polinizador j
    plantas.1 <- names(neighbors(m1g.abeja, pol.j))
    plantas.2 <- names(neighbors(m2g.abeja, pol.j))
    
    # Baselga para el periodo y polinizador j
    baselga.j <- calcular_baselga_focal(plantas.1, plantas.2) 
    baselga.j$t1 <- id.1
    baselga.j$t2 <- id.2
    baselga.j$esp <- pol.j
    
    baselga.periodo <- rbind(baselga.periodo, baselga.j)        
  }
  
  # baselga.full <- rbind(baselga.full, baselga.periodo)
}


tabla.beta$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta$periodo.n <- as.numeric(rownames(tabla.beta))

tabla.beta.abeja$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta.abeja$periodo.n <- as.numeric(rownames(tabla.beta.abeja))

### Figura 1 ----
png(paste0("../03figuras/Betadiversidad/Betapart_",proyecto,".png"))
par(mfrow=(c(2,2)))
plot(S~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Turnover S")

plot(OS~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Rewiring OS")

plot(WN~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Whole network WN")

plot(ST~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Asociado a Turnover ST")


dev.off()


# par(mfrow=c(1,2))

### Figura 2 ----
png(paste0("../03figuras/Betadiversidad/TrayectoriaBetadiversidad",proyecto,".png"), width = 1280, height = 720)
plot(WN~periodo.n, tabla.beta, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta$periodo.n+0.2)),
     main="Trayectoría de Betadiversidad 061.1 KB")


axis(1, at = c(1:max(tabla.beta$periodo.n)), labels = unique(tabla.beta$periodo))


lines(x=tabla.beta$periodo.n, y=tabla.beta$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$OS, type=c("b"), ylim=c(0,1),
      col=3, lty=3, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$ST, type=c("b"), ylim=c(0,1),
      col=4, lty=4, pch=16)

legend(col=c(1,2,3,4), legend=c("WN","S","OS","ST"), "topright", lty=c(1,2,3,4), pch=16, seg.len = 5)
dev.off()

### Tabla 1----
tabla.beta$ID <- unique(datos.rt$ID)
write.csv(x = tabla.beta, file = paste0("tabla.beta",proyecto,".csv"), row.names = FALSE)

head(datos.rt)

baselga.periodo <- baselga.periodo[order(baselga.periodo$esp),]
baselga.periodo$periodo <- paste(baselga.periodo$t1, baselga.periodo$t2, sep="-")
xyplot(BSIM~factor(periodo)|esp, 
       layout=c(1,4), 
       type=c("b"), data=baselga.periodo, 
       xlab="Periodo",
       as.table=TRUE)

head(baselga.periodo)

histogram(~BSIM|esp, 
          type="count",
          data=baselga.periodo, layout=c(1,1), as.table=TRUE)


histogram(~BSIM|periodo, 
          type="count",
          data=baselga.periodo, layout=c(1,4), as.table=TRUE)


head(baselga.periodo)

ml.abejas <- read.csv("../02salidas/ml.abejas.csv")
head(ml.abejas)

ml.abejas$polinizador <- gsub(pattern = " ", replacement = "_", 
                              x =ml.abejas$sp.armonizado)


baselga.familia <- merge(baselga.periodo, ml.abejas[,c("polinizador","familia")], 
                         by.y="polinizador", 
                         by.x="esp")
baselga.familia <- baselga.familia[order(baselga.familia$periodo),]
baselga.familia <- unique(baselga.familia)

### Tabla 2 ----
write.csv(file = paste0("../02salidas/baselga.",proyecto,"especie.csv"),
          x = baselga.familia, row.names = FALSE)


## 61.3 ----
proyecto <- "061.3_KaiserBunbury"
Kaiser61.f <- subset(kaiser61.3, pol_code %in% abejas$species_name)
# Kaiser60.f <- subset(Kaiser60.f, ID=="M_060")
Kaiser61.f$t <- Kaiser61.f$t-16
combi <- combinations(v = as.numeric(unique(Kaiser61.f$t)), 
                      r = 2, 
                      n=length(unique(Kaiser61.f$t)))

combi <- subset(combi, (combi[,2]-combi[,1])==1)

lista.beta <- list()
lista.beta.abeja <- list()
tabla.beta <- c()
tabla.beta.abeja <- c()

datos.rt <- Kaiser61.f
datos.rt$species_name_pol <-str_to_sentence(datos.rt$pol_code)
datos.rt$species_name_plant <-str_to_sentence(datos.rt$plant_code)

datos.rt <- subset(datos.rt, species_name_pol!="")

baselga.periodo <- c()
baselga.full <- c()

### Ciclo para calculo de beta ----
for(i in 1:nrow(combi)){
  combi.i <- combi[i,]
  id.1 <- combi.i[1]
  id.2 <- combi.i[2]
  
  print(id.1)
  print(id.2)
  
  rt.i1 <- subset(datos.rt, t==id.1) 
  rt.i2 <- subset(datos.rt, t==id.2) 
  
  m1g <- graph_from_data_frame(rt.i1[,c("species_name_plant","species_name_pol")])
  m2g <- graph_from_data_frame(rt.i2[,c("species_name_plant","species_name_pol")])
  
  m1g.abeja <- graph_from_data_frame(rt.i1[,c("species_name_pol","species_name_plant")], directed = TRUE)
  m2g.abeja <- graph_from_data_frame(rt.i2[,c("species_name_pol","species_name_plant")], directed = TRUE)
  
  # lista.beta.i <- betalink(m1g, m2g, bf = Wi)
  lista.beta.i <- betalink(m2g, m1g, bf = Wi)
  tabla.beta.i <- as.data.frame(lista.beta.i)
  lista.beta <- c(lista.beta,lista.beta.i)
  
  tabla.beta <- rbind(tabla.beta,tabla.beta.i)
  
  lista.beta.i.abeja <- betalink(m1g, m2g, bf = B01)
  tabla.beta.i.abeja <- as.data.frame(lista.beta.i.abeja)
  lista.beta.abeja <- c(lista.beta.abeja,lista.beta.i.abeja)
  
  tabla.beta.abeja <- rbind(tabla.beta.abeja,tabla.beta.i.abeja)
  
  # pollinators <- V(m1g.abeja)[V(m1g.abeja)$type == TRUE]$name
  print(betalink(m1g, m2g, bf = Wi))
  
  # Baselga por especie de polinizador para el periodo
  pol_species1 <- unique(rt.i1[, "species_name_pol"])
  V(m1g.abeja)$type <- ! (V(m1g.abeja)$name %in% pol_species1)
  
  pol_species2 <- unique(rt.i2[, "species_name_pol"])
  V(m2g.abeja)$type <- ! (V(m2g.abeja)$name %in% pol_species2)
  
  pol1 <- V(m1g.abeja)[V(m1g.abeja)$type == TRUE]$name
  pol2 <- V(m2g.abeja)[V(m2g.abeja)$type == TRUE]$name
  # 
  # pol1y2 <- sort(intersect(pol1, pol2))
  pol1y2 <- sort(intersect(unique(pol_species1$species_name_pol),
                           unique(pol_species2$species_name_pol)))
  
  for (pol.j in pol1y2) {
    # pol.j <- "Alloscirtetica_sp1"
    
    # Especies de plantas en t1 y t2 para el polinizador j
    plantas.1 <- names(neighbors(m1g.abeja, pol.j))
    plantas.2 <- names(neighbors(m2g.abeja, pol.j))
    
    # Baselga para el periodo y polinizador j
    baselga.j <- calcular_baselga_focal(plantas.1, plantas.2) 
    baselga.j$t1 <- id.1
    baselga.j$t2 <- id.2
    baselga.j$esp <- pol.j
    
    baselga.periodo <- rbind(baselga.periodo, baselga.j)        
  }
  
  # baselga.full <- rbind(baselga.full, baselga.periodo)
}


tabla.beta$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta$periodo.n <- as.numeric(rownames(tabla.beta))

tabla.beta.abeja$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta.abeja$periodo.n <- as.numeric(rownames(tabla.beta.abeja))

### Figura 1 ----
png(paste0("../03figuras/Betadiversidad/Betapart_",proyecto,".png"))
par(mfrow=(c(2,2)))
plot(S~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Turnover S")

plot(OS~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Rewiring OS")

plot(WN~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Whole network WN")

plot(ST~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Asociado a Turnover ST")


dev.off()


# par(mfrow=c(1,2))

### Figura 2 ----
png(paste0("../03figuras/Betadiversidad/TrayectoriaBetadiversidad",proyecto,".png"), width = 1280, height = 720)
plot(WN~periodo.n, tabla.beta, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta$periodo.n+0.2)),
     main="Trayectoría de Betadiversidad 061.1 KB")


axis(1, at = c(1:max(tabla.beta$periodo.n)), labels = unique(tabla.beta$periodo))


lines(x=tabla.beta$periodo.n, y=tabla.beta$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$OS, type=c("b"), ylim=c(0,1),
      col=3, lty=3, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$ST, type=c("b"), ylim=c(0,1),
      col=4, lty=4, pch=16)

legend(col=c(1,2,3,4), legend=c("WN","S","OS","ST"), "topright", lty=c(1,2,3,4), pch=16, seg.len = 5)
dev.off()

### Tabla 1----
tabla.beta$ID <- unique(datos.rt$ID)
write.csv(x = tabla.beta, file = paste0("tabla.beta",proyecto,".csv"), row.names = FALSE)

head(datos.rt)

baselga.periodo <- baselga.periodo[order(baselga.periodo$esp),]
baselga.periodo$periodo <- paste(baselga.periodo$t1, baselga.periodo$t2, sep="-")
xyplot(BSIM~factor(periodo)|esp, 
       layout=c(1,4), 
       type=c("b"), data=baselga.periodo, 
       xlab="Periodo",
       as.table=TRUE)

head(baselga.periodo)

histogram(~BSIM|esp, 
          type="count",
          data=baselga.periodo, layout=c(1,1), as.table=TRUE)


histogram(~BSIM|periodo, 
          type="count",
          data=baselga.periodo, layout=c(1,4), as.table=TRUE)


head(baselga.periodo)

ml.abejas <- read.csv("../02salidas/ml.abejas.csv")
head(ml.abejas)

ml.abejas$polinizador <- gsub(pattern = " ", replacement = "_", 
                              x =ml.abejas$sp.armonizado)


baselga.familia <- merge(baselga.periodo, ml.abejas[,c("polinizador","familia")], 
                         by.y="polinizador", 
                         by.x="esp")
baselga.familia <- baselga.familia[order(baselga.familia$periodo),]
baselga.familia <- unique(baselga.familia)

### Tabla 2 ----
write.csv(file = paste0("../02salidas/baselga.",proyecto,"especie.csv"),
          x = baselga.familia, row.names = FALSE)


## 2.5 Proyecto 72 Sabatino ----
print("Inicio Kaiser072")
RT.Sabatino72 <- list.files(path="../00baseDatos/redestemproalesidactualizadomatricescuantitativass/M_PL_072 - Sabatino-Redes temporales/",
                         pattern = ".csv", full.names = TRUE)

Sabatino72.l <- c()

for(i in RT.Sabatino72){
  Sabatino72.i <- read.csv(i)
  print(dim(Sabatino72.i))
  Sabatino72.i.l <- pivot_longer(Sabatino72.i, cols = 2:ncol(Sabatino72.i))
  Sabatino72.i.t <- substr(i, nchar(i)-5, nchar(i)-4)
  Sabatino72.i.l$t <- Sabatino72.i.t
  Sabatino72.l <- rbind(Sabatino72.l,Sabatino72.i.l)
  
}

head(Sabatino72.l)
Sabatino72 <- subset(Sabatino72.l, value!=0)
names(Sabatino72) <- c("plant_code","pol_code","value","t")
Sabatino72$pol_code <- gsub(pattern = "\\.$", replacement = "", x = Sabatino72$pol_code)
Sabatino72$pol_code <- gsub(pattern = "[.]", replacement = "_", x = Sabatino72$pol_code)

abejas <- read.csv("ml.abejas.csv")
abejas <- subset(abejas, ID=="M_072")
abejas$species_name <- gsub(pattern = " ", replacement = "_", x = abejas$sp.armonizado)
# abejas$species_name2 <- substr(abejas$species_name,  pattern = " ", replacement = "_", x = abejas$sp.armonizado)

Sabatino72.f <- subset(Sabatino72, pol_code %in% abejas$species_name)
# Kaiser60.f <- subset(Kaiser60.f, ID=="M_060")

combi <- combinations(v = as.numeric(unique(Sabatino72.f$t)), 
                      r = 2, 
                      n=length(unique(Sabatino72.f$t)))

combi <- subset(combi, (combi[,2]-combi[,1])==1)

lista.beta <- list()
lista.beta.abeja <- list()
tabla.beta <- c()
tabla.beta.abeja <- c()

datos.rt <- Sabatino72.f
datos.rt$t <- as.numeric(datos.rt$t)
baselga.periodo <- c()
baselga.full <- c()

### Ciclo para calculo beta ----
for(i in 1:nrow(combi)){
  combi.i <- combi[i,]
  id.1 <- combi.i[1]
  id.2 <- combi.i[2]
  
  print(id.1)
  print(id.2)
  
  rt.i1 <- subset(datos.rt, t==id.1) 
  rt.i2 <- subset(datos.rt, t==id.2) 
  
  m1g <- graph_from_data_frame(rt.i1[,c(1,2)])
  m2g <- graph_from_data_frame(rt.i2[,c(1,2)])
  
  
  m1g.abeja <- graph_from_data_frame(rt.i1[,c(2,1)])
  m2g.abeja <- graph_from_data_frame(rt.i2[,c(2,1)])
  
  # lista.beta.i <- betalink(m1g, m2g, bf = Wi)
  lista.beta.i <- betalink(m2g, m1g, bf = Wi)
  tabla.beta.i <- as.data.frame(lista.beta.i)
  lista.beta <- c(lista.beta,lista.beta.i)
  
  tabla.beta <- rbind(tabla.beta,tabla.beta.i)
  
  lista.beta.i.abeja <- betalink(m1g, m2g, bf = B01)
  tabla.beta.i.abeja <- as.data.frame(lista.beta.i.abeja)
  lista.beta.abeja <- c(lista.beta.abeja,lista.beta.i.abeja)
  
  tabla.beta.abeja <- rbind(tabla.beta.abeja,tabla.beta.i.abeja)
  
  
  print(betalink(m1g, m2g, bf = Wi))
  
}

library(lattice)
library(psych)

tabla.beta$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta$periodo.n <- as.numeric(rownames(tabla.beta))

tabla.beta.abeja$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta.abeja$periodo.n <- as.numeric(rownames(tabla.beta.abeja))


par(mfrow=(c(2,2)))
plot(S~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Turnover S")

plot(OS~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Rewiring OS")

plot(WN~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Whole network WN")

plot(ST~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Asociado a Turnover ST")


dev.off()



png("TrayectoriaBetadiversidadSabatino072.png", width = 1280, height = 720)
plot(WN~periodo.n, tabla.beta, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta$periodo.n+0.2)),
     main="Trayectoría de Betadiversidad Sabatino 72")


axis(1, at = c(1:max(tabla.beta$periodo.n)), labels = unique(tabla.beta$periodo))


lines(x=tabla.beta$periodo.n, y=tabla.beta$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$OS, type=c("b"), ylim=c(0,1),
      col=3, lty=3, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$ST, type=c("b"), ylim=c(0,1),
      col=4, lty=4, pch=16)

legend(col=c(1,2,3,4), legend=c("WN","S","OS","ST"), "topright", lty=c(1,2,3,4), pch=16, seg.len = 5)
dev.off()

write.csv(x = tabla.beta, file = "tabla.beta72.csv", row.names = FALSE)

plot(WN~periodo.n, tabla.beta.abeja, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta.abeja$periodo.n+0.2)
            
     ))

axis(1, at = c(1:max(tabla.beta$periodo.n)), labels = unique(tabla.beta.abeja$periodo))


lines(x=tabla.beta.abeja$periodo.n, y=tabla.beta.abeja$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta.abeja$periodo.n, y=tabla.beta.abeja$OS, type=c("b"), ylim=c(0,1),
      col=3, lty=3, pch=16)

lines(x=tabla.beta.abeja$periodo.n, y=tabla.beta.abeja$ST, type=c("b"), ylim=c(0,1),
      col=4, lty=4, pch=16)


## 2.6 Proyecto 80 Resasco ----
library(readxl)
print("Inicio Kaiser080")
RT.Resasco80 <- list.files(path="../00baseDatos/redestemproalesidactualizadomatricescuantitativass/M_080_ Resasco et al 2021 Ecology-Temporal",
                            pattern = ".xlsx", full.names = TRUE)

Resasco80.l <- c()

for(i in RT.Resasco80){
  Resasco80.i <- read_xlsx(path = i, sheet=1)
  print(dim(Resasco80.i))
  Resasco80.i.l <- pivot_longer(Resasco80.i, cols = 2:ncol(Resasco80.i))
  Resasco80.i.t <- substr(i, nchar(i)-8, nchar(i)-5)
  Resasco80.i.l$t <- Resasco80.i.t
  Resasco80.l <- rbind(Resasco80.l,Resasco80.i.l)
  
}

head(Resasco80.l)
Resasco80 <- subset(Resasco80.l, value!=0)
names(Resasco80) <- c("plant_code","pol_code","value","t")
Resasco80$pol_code <- gsub(pattern = "\\.$", replacement = "", x = Resasco80$pol_code)
Resasco80$pol_code <- gsub(pattern = "[.]", replacement = "_", x = Resasco80$pol_code)

# abejas <- read.csv("ml.abejas.csv")
# abejas <- subset(abejas, ID=="M_080")
# abejas$species_name <- gsub(pattern = " ", replacement = "_", x = abejas$sp.armonizado)
# abejas$species_name2 <- substr(abejas$species_name,  pattern = " ", replacement = "_", x = abejas$sp.armonizado)

# Sabatino72.f <- subset(Sabatino72, pol_code %in% abejas$species_name)
Resasco80.f <- Resasco80
# Kaiser60.f <- subset(Kaiser60.f, ID=="M_060")

combi <- combinations(v = as.numeric(unique(Resasco80.f$t)), 
                      r = 2, 
                      n=length(unique(Resasco80.f$t)))

combi <- subset(combi, (combi[,2]-combi[,1])==1)

lista.beta <- list()
lista.beta.abeja <- list()
tabla.beta <- c()
tabla.beta.abeja <- c()

datos.rt <- Resasco80.f
datos.rt$t <- as.numeric(datos.rt$t)
baselga.periodo <- c()
baselga.full <- c()

### Ciclo para calculo beta ----
for(i in 1:nrow(combi)){
  combi.i <- combi[i,]
  id.1 <- combi.i[1]
  id.2 <- combi.i[2]
  
  print(id.1)
  print(id.2)
  
  rt.i1 <- subset(datos.rt, t==id.1) 
  rt.i2 <- subset(datos.rt, t==id.2) 
  
  m1g <- graph_from_data_frame(rt.i1[,c(1,2)])
  m2g <- graph_from_data_frame(rt.i2[,c(1,2)])
  
  
  m1g.abeja <- graph_from_data_frame(rt.i1[,c(2,1)])
  m2g.abeja <- graph_from_data_frame(rt.i2[,c(2,1)])
  
  # lista.beta.i <- betalink(m1g, m2g, bf = Wi)
  lista.beta.i <- betalink(m2g, m1g, bf = Wi)
  tabla.beta.i <- as.data.frame(lista.beta.i)
  lista.beta <- c(lista.beta,lista.beta.i)
  
  tabla.beta <- rbind(tabla.beta,tabla.beta.i)
  
  lista.beta.i.abeja <- betalink(m1g, m2g, bf = B01)
  tabla.beta.i.abeja <- as.data.frame(lista.beta.i.abeja)
  lista.beta.abeja <- c(lista.beta.abeja,lista.beta.i.abeja)
  
  tabla.beta.abeja <- rbind(tabla.beta.abeja,tabla.beta.i.abeja)
  
  
  print(betalink(m1g, m2g, bf = Wi))
  
}

library(lattice)
library(psych)

tabla.beta$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta$periodo.n <- as.numeric(rownames(tabla.beta))

tabla.beta.abeja$periodo <- paste(combi[,1],combi[,2], sep="-")
tabla.beta.abeja$periodo.n <- as.numeric(rownames(tabla.beta.abeja))


par(mfrow=(c(2,2)))
plot(S~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Turnover S")

plot(OS~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Rewiring OS")

plot(WN~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Whole network WN")

plot(ST~periodo.n, tabla.beta, type=c("l"), ylim=c(0,1),
     ylab="Asociado a Turnover ST")


dev.off()



png("TrayectoriaBetadiversidadResasco080.png", width = 1280, height = 720)
plot(WN~periodo.n, tabla.beta, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta$periodo.n+0.2)),
     main="Trayectoría de Betadiversidad Resasco80")


axis(1, at = c(1:max(tabla.beta$periodo.n)), labels = unique(tabla.beta$periodo))


lines(x=tabla.beta$periodo.n, y=tabla.beta$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$OS, type=c("b"), ylim=c(0,1),
      col=3, lty=3, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$ST, type=c("b"), ylim=c(0,1),
      col=4, lty=4, pch=16)

legend(col=c(1,2,3,4), legend=c("WN","S","OS","ST"), "topright", lty=c(1,2,3,4), pch=16, seg.len = 5)
dev.off()

write.csv(x = tabla.beta, file = "tabla.beta80.csv", row.names = FALSE)


plot(WN~periodo.n, tabla.beta.abeja, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta.abeja$periodo.n+0.2)
            
     ))

axis(1, at = c(1:max(tabla.beta$periodo.n)), labels = unique(tabla.beta.abeja$periodo))


lines(x=tabla.beta.abeja$periodo.n, y=tabla.beta.abeja$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta.abeja$periodo.n, y=tabla.beta.abeja$OS, type=c("b"), ylim=c(0,1),
      col=3, lty=3, pch=16)

lines(x=tabla.beta.abeja$periodo.n, y=tabla.beta.abeja$ST, type=c("b"), ylim=c(0,1),
      col=4, lty=4, pch=16)



## Codigo obsoleto ----
# 
# chacof06 <- readxl::read_excel("../00baseDatos/redestemproalesidactualizadomatricescuantitativass/79_1chacoff_2006 - redes temporales/79_1chacoff_2006.xlsx")
# head(chacof06)
# 
# c06 <- pivot_longer(chacof06, cols = 2:136)
# c06 <- subset(c06, value!=0)
# 
# chacof07 <- readxl::read_excel("../00baseDatos/redestemproalesidactualizadomatricescuantitativass/79_1chacoff_2006 - redes temporales/79_2Chacof_2007.xlsx")
# head(chacof07)
# c07 <- pivot_longer(chacof07, cols = 2:136)
# c07 <- subset(c07, value!=0)
# 
# 
# 
# 
# m1g <- graph_from_data_frame(c06[,c(1,2)])
# m2g <- graph_from_data_frame(c07[,c(1,2)])
# 
# plot(m1g)
# plot(m2g)
# 
# 
# betalink(m1g, m2g, bf = B01)
# 
# library(gtools)
# 
# 
# 
# library(igraph)
# 
# # 1. Crear un data frame de aristas (relaciones Persona -> Empresa)
# edges <- c07
# 
# # 2. Crear una lista de nodos únicos y definir su tipo (TRUE/FALSE)
# # Identificamos qué nombres pertenecen a cada grupo
# nodos_desde <- unique(edges$...1)
# nodos_hasta <- unique(edges$name)
# 
# vertices <- data.frame(
#   name = c(nodos_desde, nodos_hasta),
#   type = c(rep(FALSE, length(nodos_desde)), rep(TRUE, length(nodos_hasta)))
# )
# 
# # 3. Crear el grafo bipartito
# g <- graph_from_data_frame(d = edges, directed = FALSE, vertices = vertices)
# 
# # 4. Verificar que es bipartito
# is_bipartite(g) # Debe retornar TRUE
# 
# # 5. Graficar con una distribución bipartita
# plot(g, 
#      layout = layout_as_bipartite(g), 
#      vertex.color = c("tomato", "gold")[V(g)$type + 1],
#      vertex.size = 20,
#      vertex.label.cex = 0.8)
# 
# 
# ## 3. EJEMPLO DE USO ##
# # tablas de ejemplo para calculos
# ipa51 <- subset(ipa, ID=="M_051")
# ipa1 <- subset(ipa, ID=="M_001")
# 
# # Matriz 1 en formato largo (Ej: Red de un sitio A)
# # matriz_larga_1 <- data.frame(
# #   sp_origen = c("Planta1", "Planta1", "Planta2", "Planta3"),
# #   sp_destino = c("Abeja1", "Abeja2", "Abeja2", "Mariposa1")
# # )
# matriz_larga_1 <- ipa1[,c("planta","sp.armonizado")]
# matriz_larga_2 <- ipa51[,c("planta","sp.armonizado")]
# 
# matriz_larga_1 <- ipa1[,c("sp.armonizado","planta")]
# matriz_larga_2 <- ipa51[,c("sp.armonizado","planta")]
# 
# 
# # Matriz 2 en formato largo (Ej: Red de un sitio B con algunas especies compartidas)
# # matriz_larga_2 <- data.frame(
# #   sp_origen = c("Planta1", "Planta2", "Planta3", "Planta4"),
# #   sp_destino = c("Abeja2", "Abeja2", "Polilla1", "Mariposa1")
# # )
# 
# # Ejecutar el cálculo
# resultados_beta <- beta_largo(matriz_larga_1, matriz_larga_2, directed = TRUE)
# print(resultados_beta)
# 
# 
# m1g <- graph_from_data_frame(matriz_larga_1)
# m2g <- graph_from_data_frame(matriz_larga_2)
# 
# plot(m1g)
# plot(m2g)
# 
# 
# betalink(m1g, m2g, bf = B01)
# 
# 
# combi <- combinations(n = length(unique(chacof.f$year)), 2)
# 
# # temporales un solo sitio
# # solo saltos continuos de tiempo
# 
# for(i in 1:nrow(combi)){
#   combi.i <- combi[i,]
#   id.1 <- combi.i[1]
#   id.2 <- combi.i[2]
#   
#   print(id.1)
#   
#   
# }
### Inicio analisis Beta-DIT ---- 
# datos.analisis <- read.csv("../02salidas/datos.analisis.csv")
# datos.analisis$polinizador <- gsub(pattern = " ", replacement = "_", 
#                                    x =datos.analisis$sp.armonizado)
# 
# 
# head(datos.analisis)
# 
# unique(baselga.familia$esp)
# 
# resumen.bsim <- baselga.familia |> group_by(esp) |>
#   summarise(BSIM.m = mean(BSIM, na.rm=TRUE),
#             BSIM.sd = sd(BSIM, na.rm=TRUE))
# 
# resumen.bsim <- baselga.familia |> group_by(esp) |>
#   summarise(BSIM.m = mean(BSIM, na.rm=TRUE),
#             BSIM.sd = sd(BSIM, na.rm=TRUE))
# 
# resumen.bsim.m <- baselga.familia |> group_by(esp) |>
#   summarise(BSIM.m = mean(BSIM, na.rm=TRUE),
#             BSIM.sd = sd(BSIM, na.rm=TRUE))
# str(baselga.familia)
# baselga.familia$esp <- as.factor(baselga.familia$esp)
# baselga.familia |> group_by(esp) |> summarise(n=length(esp))
# 
# datos.analisis.79 <- subset(datos.analisis, ID=="M_079")
# 
# baselga.dit <- merge(resumen.bsim.m, 
#                      datos.analisis.79[,c("polinizador","DIT.final","Grado", "familia")], 
#                      by.x = "esp", by.y="polinizador", all.x=TRUE)
# 
# 
# plot(BSIM.m~DIT.final, baselga.dit)
# plot(BSIM~DIT.final, baselga.dit)
# plot(BSIM.sd~DIT.final, baselga.dit)
# 
# 
# plot(BSIM.m~Grado, baselga.dit, col=as.numeric(factor(baselga.dit$familia)))
# lm(BSIM.m~Grado+familia, baselga.dit)
# # plot(BSIM~Grado, baselga.dit, col=as.numeric(factor(baselga.dit$familia)))
# plot(BSIM~Grado, baselga.dit)#, col=as.numeric(factor(baselga.dit$familia)))
# abline(lm(BSIM~Grado, baselga.dit), col=2, lty=2, lwd=2)
# 
# abline(a=0.50768, b = -0.13268 )
# abline(a=0.50768, b = -0.13268 + 0.18185, col=2)
# abline(a=0.50768, b = -0.13268 + 0.04762, col=3)
# abline(a=0.50768, b = -0.13268 + 0.39771, col=4)
# abline(a=0.50768, b = -0.13268 + 0.20564, col=5)
# 
# plot(BSIM.sd~Grado, baselga.dit)

