# Estimacion de beta diversidad

ipa <- read.csv("ml.abejas.csv")
head(ipa)

ipa51 <- subset(ipa, ID=="M_051")
ipa1 <- subset(ipa, ID=="M_001")


# Cargar librerías necesarias (basado en las dependencias de tu código)
library(igraph)
library(stringr)
library(plyr)

## 1. DEFINIR LAS FUNCIONES BASE Y LA MÉTRICA FALTANTE ##

# Función de métrica de beta-diversidad (Disimilitud de Sørensen)
# Faltaba en el código original. La disimilitud es: (b + c) / (2a + b + c)
B01 <- function(x) {
  return((x$b + x$c) / (2 * x$a + x$b + x$c))
}

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

## 2. FUNCIÓN PARA PROCESAR FORMATO LARGO ##

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

## 3. EJEMPLO DE USO ##

# Matriz 1 en formato largo (Ej: Red de un sitio A)
# matriz_larga_1 <- data.frame(
#   sp_origen = c("Planta1", "Planta1", "Planta2", "Planta3"),
#   sp_destino = c("Abeja1", "Abeja2", "Abeja2", "Mariposa1")
# )
matriz_larga_1 <- ipa1[,c("planta","sp.armonizado")]
matriz_larga_2 <- ipa51[,c("planta","sp.armonizado")]

matriz_larga_1 <- ipa1[,c("sp.armonizado","planta")]
matriz_larga_2 <- ipa51[,c("sp.armonizado","planta")]


# Matriz 2 en formato largo (Ej: Red de un sitio B con algunas especies compartidas)
# matriz_larga_2 <- data.frame(
#   sp_origen = c("Planta1", "Planta2", "Planta3", "Planta4"),
#   sp_destino = c("Abeja2", "Abeja2", "Polilla1", "Mariposa1")
# )

# Ejecutar el cálculo
resultados_beta <- beta_largo(matriz_larga_1, matriz_larga_2, directed = TRUE)
print(resultados_beta)


m1g <- graph_from_data_frame(matriz_larga_1)
m2g <- graph_from_data_frame(matriz_larga_2)

plot(m1g)
plot(m2g)


betalink(m1g, m2g, bf = B01)


combi <- combinations(n = length(unique(chacof.f$year)), 2)

# temporales un solo sitio
# solo saltos continuos de tiempo

for(i in 1:nrow(combi)){
  combi.i <- combi[i,]
  id.1 <- combi.i[1]
  id.2 <- combi.i[2]
  
  print(id.1)
  
  
}

library(tidyverse)
library(gtools)
library(rgra)

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
names(chacof) <- c("plant_code","pol_code","value","year")
plantas <- read.csv("../00baseDatos/redestemproalesidactualizadomatricescuantitativass/79_1chacoff_2006 - redes temporales/11794503.v4 (Interacciones planta-polinizador (visitas a flores)/plant_names.csv")
insectos <- read.csv("../00baseDatos/redestemproalesidactualizadomatricescuantitativass/79_1chacoff_2006 - redes temporales/11794503.v4 (Interacciones planta-polinizador (visitas a flores)/insect_names.csv")

# abejas <- read.csv("ml.abejas.csv")
# abejas$species_name <- gsub(pattern = " ", replacement = "_", x = abejas$sp.armonizado)
# abejas$species_name2 <- substr(abejas$species_name,  pattern = " ", replacement = "_", x = abejas$sp.armonizado)

library(rgbif)

# Buscar la familia (por ejemplo, Orchidaceae)
familia_info <- name_backbone(name = insectos$species_name, rank = "family", )

# Ver el ID de la familia
familia_key <- familia_info$familyKey
print(familia_key)


insectos <- subset(insectos, is.element(species_name, abejas$species_name))
setdiff( unique(abejas$species_name),unique(insectos$species_name))

head(chacof)
head(plantas)
head(insectos)

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

  
  m1g.abeja <- graph_from_data_frame(rt.i1[,c(6,5)])
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



par(mfrow=c(1,2))


png("TrayectoriaBetadiversidadChacoff.png")
plot(WN~periodo.n, tabla.beta, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta$periodo.n+0.2)),
     main="Trayectoría de Betadiversidad Chacoff")
     

axis(1, at = c(1, 2, 3,4,5), labels = unique(tabla.beta$periodo))


lines(x=tabla.beta$periodo.n, y=tabla.beta$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$OS, type=c("b"), ylim=c(0,1),
       col=3, lty=3, pch=16)

lines(x=tabla.beta$periodo.n, y=tabla.beta$ST, type=c("b"), ylim=c(0,1),
       col=4, lty=4, pch=16)

legend(col=c(1,2,3,4), legend=c("WN","S","OS","ST"), "topright", lty=c(1,2,3,4), pch=16, seg.len = 5)
dev.off()

tabla.beta

plot(WN~periodo.n, tabla.beta.abeja, type=c("b"), ylim=c(0,1),
     ylab="Beta diversidad", col=1, las=1, pch=16,xaxt = "n" , 
     xlab="Periodo", 
     xlim=c(0.8,max(tabla.beta.abeja$periodo.n+0.2)
            
     ))

axis(1, at = c(1, 2, 3,4,5), labels = unique(tabla.beta.abeja$periodo))


lines(x=tabla.beta.abeja$periodo.n, y=tabla.beta.abeja$S, type=c("b"), ylim=c(0,1),
      col=2, lty=2, pch=16)

lines(x=tabla.beta.abeja$periodo.n, y=tabla.beta.abeja$OS, type=c("b"), ylim=c(0,1),
      col=3, lty=3, pch=16)

lines(x=tabla.beta.abeja$periodo.n, y=tabla.beta.abeja$ST, type=c("b"), ylim=c(0,1),
      col=4, lty=4, pch=16)


head(chacof.f)

chacof06 <- readxl::read_excel("../00baseDatos/redestemproalesidactualizadomatricescuantitativass/79_1chacoff_2006 - redes temporales/79_1chacoff_2006.xlsx")
head(chacof06)

c06 <- pivot_longer(chacof06, cols = 2:136)
c06 <- subset(c06, value!=0)

chacof07 <- readxl::read_excel("../00baseDatos/redestemproalesidactualizadomatricescuantitativass/79_1chacoff_2006 - redes temporales/79_2Chacof_2007.xlsx")
head(chacof07)
c07 <- pivot_longer(chacof07, cols = 2:136)
c07 <- subset(c07, value!=0)




m1g <- graph_from_data_frame(c06[,c(1,2)])
m2g <- graph_from_data_frame(c07[,c(1,2)])

plot(m1g)
plot(m2g)


betalink(m1g, m2g, bf = B01)

library(gtools)



library(igraph)

# 1. Crear un data frame de aristas (relaciones Persona -> Empresa)
edges <- c07

# 2. Crear una lista de nodos únicos y definir su tipo (TRUE/FALSE)
# Identificamos qué nombres pertenecen a cada grupo
nodos_desde <- unique(edges$...1)
nodos_hasta <- unique(edges$name)

vertices <- data.frame(
  name = c(nodos_desde, nodos_hasta),
  type = c(rep(FALSE, length(nodos_desde)), rep(TRUE, length(nodos_hasta)))
)

# 3. Crear el grafo bipartito
g <- graph_from_data_frame(d = edges, directed = FALSE, vertices = vertices)

# 4. Verificar que es bipartito
is_bipartite(g) # Debe retornar TRUE

# 5. Graficar con una distribución bipartita
plot(g, 
     layout = layout_as_bipartite(g), 
     vertex.color = c("tomato", "gold")[V(g)$type + 1],
     vertex.size = 20,
     vertex.label.cex = 0.8)


