library(igraph)
library(tidyverse)
library(bipartite)
library(circlize)

base_datos <- readRDS("../00baseDatos/base_datos.RDS")

names(base_datos)
base_datos$
matricez.anchas <- names(base_datos)[grepl(x = names(base_datos), pattern = "_ancha" )]
matricez.anchas <- names(base_datos)[grepl(x = names(base_datos), pattern = "_ancha" )]

for(i in unique(matricez.anchas)){
  # i <- "matriz_ancha.004"
  # if(i == "M_076"){next}
  
  m.ancho.i <- base_datos[[i]] 
  rownames(m.ancho.i) <- m.ancho.i$plantas
  m.ancho.i$plantas <- NULL
  
  red <- as.matrix(m.ancho.i)
  
  #nota: es importante saber si las columnas están separadas por
  #comas o puntos y comas para definir el argumento sep
  head(red)
  class(red)
  rownames(red)
  colnames(red)
  
  #Graficos de redes bipartita
  #El paquete ofrece plotweb y visweb
  plotweb(red)
  plotweb(red, text.rot=90, arrow="down.center",
        col.interaction="wheat2")
  visweb(red)
  visweb(red, type="nested", text="interaction")
  
  ### otras opciones de grafica
  # install.packages("circlize")
  chordDiagram(red, annotationTrack="grid", big.gap=20)

# }




##Graficos de Redes unimodales
#gmode permite saber si el grafo es dirigido o no
# set.seed(123)
# gplot(rgraph(3), gmode="digraph")
# set.seed(123)
# gplot(rgraph(3), gmode="graph")

#Para crear una red unimodal (ej. de plantas) a partir de una red bipartita
# usar la función as.one.mode
# as.one.mode(red, project="lower")


## Podemos graficar las dos redes unimodales que se pueden obtener de una red bipartita de

  gplot(as.one.mode(red, project="lower"),
        label=rownames(red), gmode="graph",
        label.cex=0.6, vertex.col="lightgreen")
  
  gplot(as.one.mode(red, project="higher"),
      label=rownames(red), gmode="graph",
      label.cex=0.6, vertex.col="orange")


#----------------------------------------------------------
#Estructura de una red
#Atributos estructurales a nivel de toda la red: networklevel
# a nivel de grupo, como dispersones: grouplevel
# a nivel de especies: specieslevel
# a nivel de links: linklevel

#Metricas a nivel de toda la red - info general de la red:
#anidamiento, conectancia, modularidad y especialización a nivel de red

networklevel(red, index=c("connectance", "interaction evenness"))

# para calcular todas la metricas de red, excepto grado
networklevel(red, index="ALLBUTDD")

#Para estimar modularidad, determinar qué especies pertenecen a cada módulo y graficar los
#módulos:
mod_red <- computeModules(red, method="Beckett", deep=FALSE,
                          deleteOriginalFiles=FALSE, steps=1000, tolerance=1e-10,
                          experimental=FALSE, forceLPA=FALSE)
mod_red@likelihood
listModuleInformation(mod_red)
printoutModuleInformation(mod_red)
plotModuleWeb(mod_red)

#-----------------------------------------------------------------
#Metricas a nivel de grupo: para calcular el n° de especies y solapamiento de nicho de las
#especies de los diferentes grupos de la red.

networklevel(red, index=c("number of species", "niche overlap"),
             level="higher")
networklevel(red, index=c("number of species", "niche overlap"),
             level="lower")
networklevel(red, index=c("number of species", "niche overlap"),
             level="both")

?specieslevel

#------------------------------------------------------------
#Metricas a nivel de especie
specieslevel(red, index=c("degree","closeness"), level="higher")
specieslevel(red, index=c("degree","closeness"), level="lower")
specieslevel(red, index=c("degree","closeness"), level="both")
specieslevel(red, index="ALLBUTD", level="higher")
specieslevel(red, index="ALLBUTD", level="lower")

##Para calcular los roles estructurales de las especies, podemos utilizar los 
#coeficientes z (conectividad intra-modular) y c (coeficiente de participación) resultantes del análisis de
#modularidad (ver Olesen et al. 2007)

?specieslevel 

# Calcular z y c
cz <- czvalues(mod_red, weighted = TRUE, level = "higher")
cz_plantas <- czvalues(mod_red, weighted = TRUE, level = "lower")

# Convertir en data frame
df <- data.frame(
  c = cz[[1]],
  z = cz[[2]],
  species = names(cz[[1]])
)

# Convertir en data frame
df_plantas <- data.frame(
  c = cz_plantas[[1]],
  z = cz_plantas[[2]],
  species = names(cz_plantas[[1]])
)

# Graficar con ggplot2 y ggrepel
# library(ggplot2)
# library(ggrepel)

ggplot(df, aes(x = c, y = z, label = species)) +
   geom_point(size = 2.5, color = "black") +
  geom_vline(xintercept = 0.62, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray40") +
  geom_text_repel(size = 2.5, max.overlaps = Inf, segment.color = "gray70") +
  theme_minimal(base_size = 14) +
  xlim(0, 1) +
  labs(
    x = "Coeficiente de participación (c)",
    y = "Conectividad intra-modular (z)",
    title = paste("Roles estructurales de las especies de polinizadores",i,sep=" ")
  ) +
  theme(panel.grid.minor = element_blank())

ggplot(df_plantas, aes(x = c, y = z, label = species)) +
  geom_point(size = 2.5, color = "black") +
  geom_vline(xintercept = 0.62, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray40") +
  geom_text_repel(size = 2.5, max.overlaps = Inf, segment.color = "gray70") +
  theme_minimal(base_size = 14) +
  xlim(0, 1) +
  labs(
    x = "Coeficiente de participación (c)",
    y = "Conectividad intra-modular (z)",
    title = paste("Roles estructurales de las especies de plantas",i,sep=" ")
  ) +
  theme(panel.grid.minor = element_blank())


---------------
# library(ggplot2)
# library(ggrepel)

# Supongamos que ya tienes cz calculado
# cz <- czvalues(mod_red, weighted = TRUE, level = "higher")


ggplot(df, aes(x = c, y = z, label = species)) +
  # Puntos
  geom_point(size = 2.5, color = "black") +
  
  # Líneas de referencia
  geom_vline(xintercept = 0.62, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray40") +
  
  # Etiquetas
  geom_text_repel(
    size = 2.5,                  # etiquetas más pequeñas
    max.overlaps = Inf,
    segment.color = "gray70",
    box.padding = 0.3,
    point.padding = 0.2
  ) +

  # Límites del gráfico (ajustados para ver las 4 zonas)
  scale_x_continuous(limits = c(0, 1), expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(-2, 5), expand = c(0.02, 0.02)) +
  
  # Tema
  theme_minimal(base_size = 12) +
  labs(
    x = "Coeficiente de participación (c)",
    y = "Conectividad intra-modular (z)",
    title = "Roles estructurales de las especies"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )



#------------------------------------------------------------
#Redes unimodales gdeb siver para calcular la conectancia de la redunimodal
#lower es de plantas
red2 <- as.one.mode(red, project="lower")
gden(red2)

#para calcular el n° de enlaces dirigidos al nodo (cmode="indegree")
#n° de enlaces que salen del nodo (cmode="outegree")
sna::degree(red2, gmode="graph")
sna::degree(red2, cmode="indegree")
sna::degree(red2, cmode="outdegree")

#considerando los caminos que unen a las especies en una red
#Para calcular la centralidad de cercanía e intermediación:
closeness(red2, gmode="graph")
betweenness(red2, gmode="graph")


#---------------------------------------------------------
# Completitud de muestreo usando coverage-based rarefaction
#Se centra en las 3 medidas de los numeros de Hill
# library(iNEXT)
# 
# 
# web.chacoff <- as.matrix(read.csv2(file="M_079_chaco0611.csv", row.names=1))
# 
# head(web.chacoff)
# #Luego, calculamos los números de Hill utilizando nuestra matriz de interacciones
# out <- iNEXT(web.chacoff, q=c(0,1,2), datatype="abundance")
# 
# #grafico
# ggiNEXT(out, type=2)
# ggiNEXT(out, type = 2, facet.var = "Assemblage")
# 
# 

#--------------------------------------------------------------
#Robustez de una red



#--------------------------------------------------------------
#Modelos nulos
# La función nullmodel: r2dtable, vaznull para modelo nulo para comparar algo neutral
# funcion mgen
# Mosquin (1967) 
# Para generar 1000 matriceas aleatorizadas con nullmodel se usa la opción vaznull
#guardat en un objeto llamadio mos.null

library(bipartite) 
data(mosquin1967) 
mos.null <- nullmodel(red, N=1000, method="vaznull")
mos.null.nestedness <- array(0,1000)

for(i in 1:1000){ mos.null.nestedness[i] <- nested(mos.null[[i]],
                                                   method="NODF") }
quantile(mos.null.nestedness, prob=c(0.025,0.975))

#Se comparan los valores del modelo nulo con la matriz original
nested(red, method="NODF")

# alternativa a vaznull, la función mgen toma una matriz de probabilidades
#que indica la probabilidad de interacción de cada par de spp
#en la comunidad

I <- 10
J <- 6 
pmat <- matrix(1/(I*J),I,J)

#se usa mgen para construir matriz de interacciones imat
#función mgen puede ser utilizada para incluir info de las spp
# como abundancia, fenología, distribución espacial y caract fenotipicas

L=15 
imat <- mgen(pmat,L)
pla.ab <- rowSums(red)/sum(red)
pol.ab <- colSums(red)/sum(red)

#la matriz de probabilidades de interacción
pmat <- pla.ab %*% t(pol.ab)

i.tot <- sum(red) 
mgen(pmat, i.tot, keep.species=TRUE, rep.cell=TRUE)

mos.mgen <- nullmodel(red, N=1000, method="mgen")
mos.null.nestedness <- array(0,1000)

for(i in 1:1000){ mos.null.nestedness[i] <- nested(mos.mgen[[i]],
                                                   method="NODF") }

quantile(mos.null.nestedness, prob=c(0.025,0.975)) 
nested(red, method="NODF")


# Z-score para comparar redes
# z = (Xobs-Xnulo) / SDnulo



#Robustez
# La robustez de una red es una medida de la tolerancia que posee 
#la misma a la extinción secundaria de especies, 
#por lo que una red es más robusta cuando experimenta menor 
#cantidad de extinciones secundarias luego de perder una o más especies (Dunne et al. 2002
# extingue plantas
# data("memmott1999")
ex1 <- second.extinct(red, participant="lower", method="random", nrep=50, details=FALSE)
ex2 <- second.extinct(red, participant="lower", method="degree", nrep=50, details=FALSE)

slope.bipartite(ex1) 
robustness(ex1)
slope.bipartite(ex2)
robustness(ex2)

# para insectos
data("memmott1999")
ex1 <- second.extinct(red, participant="higher", method="random", nrep=50, details=FALSE)
ex2 <- second.extinct(red, participant="higher", method="degree", nrep=50, details=FALSE)

slope.bipartite(ex1) 
robustness(ex1)
slope.bipartite(ex2)
robustness(ex2)
gc()
}
