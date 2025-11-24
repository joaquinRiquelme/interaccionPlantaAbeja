library(igraph)
library(tidyverse)
base_datos <- readRDS("../00baseDatos/base_datos.RDS")

ml <- base_datos$matriz_comp_larga
ml <- subset(ml, interaccion > 0)
especie <- base_datos$especies

for(i in unique(ml$ID)){
  if(i == "M_076"){next}
  m.ancho.i <- ml[,c("especie","ID","plantas","interaccion")] |> 
    filter(ID == i) |> droplevels()
  
  m.ancha <- pivot_wider(m.ancho.i, 
                             id_cols = c(ID, plantas),
                             names_from = especie,
                             values_from = interaccion, values_fn = unique)
  str(m.ancha)
  m.ancha[is.na(m.ancha)] <- 0
  m.ancha[m.ancha==0] <- 0
  m.ancha$ID <- NULL
  plantas.i <- m.ancha$plantas
  m.ancha$plantas <- NULL
  G1 <- graph.incidence(as.data.frame(m.ancha), weighted = NULL)
  is_bipartite(G1)
  colrs <- c("blue", "red")[V(G1)$type + 1L]
  LO = layout_as_bipartite(G1)
  LO = LO[,c(2,1)]
  G2 <- G1
  V(G1)$name <- c(plantas.i,names(m.ancha))
  # Visualizar el gráfico con etiquetas
  png(filename = paste0("../03figuras/grafoBipartito/grafoBipartito",i,".png"), height =800, width = 800)
  plot(G1, vertex.color = colrs, layout = LO, axes = FALSE, main=i)
  dev.off()
  
  png(filename = paste0("../03figuras/grafoBipartito/grafoBipartito_numero",i,".png"), height =800, width = 800)
  plot(G2, vertex.color = colrs, layout = LO, axes = FALSE, main=i)
  dev.off()
}
