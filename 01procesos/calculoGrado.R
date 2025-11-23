library(tidyverse)

base_datos <- readRDS("../00baseDatos/base_datos.RDS")

ml <- base_datos$matriz_comp_larga
ml <- subset(ml, interaccion > 0)
especie <- base_datos$especies

# Grado 
Grado <- ml |> group_by(ID, especie) |>
  mutate(Grado=sum(interaccion>0)) |>
  select(c(-plantas,-interaccion)) |> 
  unique()

# Calculo de grado relativo y zscore
Grados <- Grado |> group_by(ID) |>
  mutate(Grado_max=max(Grado)) |>
  mutate(Grado_relativo=Grado/Grado_max) |>
  mutate(Z_Grado=scale(Grado)[,1])

write.csv(x = Grados, file = "../02salidas/Grados.csv", row.names = FALSE)


# Calculo de grado de especies de familias de abeja
ml_esp <- merge(ml, especie, all.x=TRUE)
head(ml_esp)
ml_abejas <- subset(ml_esp,  familia %in% c("Andrenidae",
                                            "Apidae",
                                            "Colletidae",
                                            "Halictidae",
                                            "Megachilidae",
                                            "Melittidae",
                                            "Strenitidae"))
# Grado abejas
GradoA <- ml_abejas |> group_by(ID, especie) |>
  mutate(Grado=sum(interaccion>0)) |>
  select(c(-plantas,-interaccion, -genero, -familia, -orden)) |> 
  unique()

# Calculo de grado relativo y zscore
Grados_A <- GradoA |> group_by(ID) |>
  mutate(Grado_max=max(Grado)) |>
  mutate(Grado_relativo=Grado/Grado_max) |>
  mutate(Z_Grado=scale(Grado)[,1])

write.csv(x = Grados_A, file = "../02salidas/Grados_Abejas.csv", row.names = FALSE)


# histograma de grado
for(i in unique(Grados$ID)){
  print(i)
  Grado_i <- subset(Grados, ID==i)
  
  # Grado
  png(filename = paste0("../03figuras/Grado/histograma_grado_",i,".png"))
  hist(Grado_i$Grado, main=paste("Histograma grado absoluto \n red",i),
       ylab="Frecuencia",
       xlab="Grado", las=1)
  dev.off()
  
  # Grado relativo
  png(filename = paste0("../03figuras/Grado/histograma_gradoRelativo_",i,".png"))
  hist(Grado_i$Grado_relativo, main=paste("Histograma grado relativo \n red",i),
       ylab="Frecuencia",
       xlab="Grado relativo", las=1)
  dev.off()
  
  # Zscore
  png(filename = paste0("../03figuras/Grado/histograma_Z_score_",i,".png"))
  hist(Grado_i$Z_Grado, main=paste("Histograma z score \n red",i),
       ylab="Frecuencia",
       xlab="Z score", las=1)
  dev.off()
}

Grados_A <- subset(Grados_A, !is.nan(Z_Grado))

# histograma de grados abejas
for(i in unique(Grados_A$ID)){
  print(i)
  # if(i=="M_060"){next}
  # if(i=="M_036"){next}
  # if(i=="M_013"){next}
  # if(i=="M_075"){next}
  # if(i=="M_009"){next}
  # if(i=="M_074"){next}
  # if(i=="M_038"){next}
  # if(i=="M_024"){next}
  # if(i=="M_010"){next}
  # if(i=="M_020"){next}
  # if(i=="M_026"){next}
  Grado_A_i <- subset(Grados_A, ID==i)
  
  # Grado
  png(filename = paste0("../03figuras/Grado/histograma_grado_Abejas_",i,".png"))
  hist(Grado_A_i$Grado, main=paste("Histograma grado absoluto Abejas\n red",i),
       ylab="Frecuencia",
       xlab="Grado", las=1)
  dev.off()
  
  # Grado relativo
  png(filename = paste0("../03figuras/Grado/histograma_gradoRelativo_Abejas_",i,".png"))
  hist(Grado_A_i$Grado_relativo, main=paste("Histograma grado relativo Abejas \n red",i),
       ylab="Frecuencia",
       xlab="Grado relativo", las=1)
  dev.off()
  
  # Zscore
  png(filename = paste0("../03figuras/Grado/histograma_Z_score_Abejas_",i,".png"))
  hist(Grado_A_i$Z_Grado, main=paste("Histograma z score Abejas \n red",i),
       ylab="Frecuencia",
       xlab="Z score", las=1)
  dev.off()
}

