library(tidyverse)

# base_datos <- readRDS("../00baseDatos/base_datos.RDS")

ml.arm <- read.csv("../01procesos/ml.arm.csv")


ml.arm2 <- ml.arm |> group_by(ID, sp.armonizado, plantas) |> 
  summarise(interaccion=as.numeric(sum(interaccion, na.rm = TRUE)>0))

# ml <- base_datos$matriz_comp_larga
head(ml.arm)
ml <- subset(ml.arm2, interaccion > 0)
# especie <- base_datos$especies

# Grado 
Grado <- ml |> group_by(ID, sp.armonizado) |>
  mutate(Grado=sum(interaccion==1)) |>
  select(c(-plantas,-interaccion)) |> 
  unique()
head(Grado)
Grado <- subset(Grado, Grado>0)

# Calculo de grado relativo y zscore
Grados <- Grado |> group_by(ID) |>
  mutate(Grado_max=max(Grado)) |>
  # mutat(Grado_potencial=)
  mutate(Grado_relativo=Grado/Grado_max) |>
  mutate(Z_Grado=scale(Grado)[,1])
head(Grados)
write.csv(x = Grados, file = "../02salidas/GradosV2.csv", row.names = FALSE)


# Calculo de grado de especies de familias de abeja

bdDIT <- read.csv("../02salidas/DITfull_porFuente.csv")
especie <- bdDIT
especie$DIT <- NULL
especie$FuenteDatos <- NULL
head(especie)

esp2 <- base_datos$especies
head(esp2)
egf <- unique(esp2[,c("familia","genero","especie")])
head(egf)
names(egf) <- names(especie)
head(egf)
egf$Genus_and_species <- gsub(pattern = "[.]",replacement = " ", x = egf$Genus_and_species)

aaa <- unique(rbind(egf, especie))

ml_esp <- merge(ml, aaa, by.x="sp.armonizado", by.y="Genus_and_species", all.x=TRUE)

head(ml_esp)


ml_abejas <- subset(ml_esp,  Family %in% c("Andrenidae",
                                            "Apidae",
                                            "Colletidae",
                                            "Halictidae",
                                            "Megachilidae",
                                            "Melittidae",
                                            "Strenitidae"))
sort(unique(ml_esp$sp.armonizado))
sort(unique(ml_abejas$sp.armonizado))
length(unique(ml_abejas$sp.armonizado))

ml_abejas_sinSP <- ml_abejas[!grepl(pattern = "\\ sp", x = ml_abejas$sp.armonizado, useBytes = TRUE)|
                               is.element(ml_abejas$sp.armonizado, c("Agapostemon splendens",
                                                               "Andrena spiraeana",
                                                               "Anthocopa spinigera",
                                                               "Colletes speciosus",
                                                               "Colletes spectabilis",
                                                               "Trigona spinipes",
                                                               "Osmia spinigera")),]
head(ml_abejas_sinSP)
sort(unique(ml_abejas_sinSP$sp.armonizado))

intersect(unique(ml_abejas$sp.armonizado), unique(ml_abejas_sinSP$sp.armonizado))
setdiff(unique(ml_abejas_sinSP$sp.armonizado),unique(ml_abejas$sp.armonizado))
setdiff(unique(ml_abejas$sp.armonizado),unique(ml_abejas_sinSP$sp.armonizado))

# Grado abejas
GradoA <- ml_abejas |> group_by(ID, sp.armonizado) |>
  mutate(Grado=sum(interaccion==1)) |>
  select(c(-plantas,-interaccion, -Genus, -Family)) |> 
  unique()
head(GradoA)
summary(GradoA)

# Calculo de grado relativo y zscore
Grados_A <- GradoA |> group_by(ID) |>
  mutate(Grado_max=max(Grado)) |>
  mutate(Grado_relativo=Grado/Grado_max) |>
  mutate(Z_Grado=scale(Grado)[,1])|>
  unique()
head(Grados_A)

write.csv(x = Grados_A, file = "../02salidas/Grados_Abejas.csv", row.names = FALSE)
library(lattice)
histogram(~Grado|ID, Grados_A, as.table=TRUE)
histogram(~Z_Grado|ID, Grados_A, as.table=TRUE)
histogram(~Grado_relativo|ID, Grados_A, as.table=TRUE)

# sin Apis mellifera
ml_abejas_sinAM <- subset(ml_abejas, !is.element(sp.armonizado, c ("Apis mellifera", "Apis mellifera Linnaeus")))

# Grado abejas
GradosinAM <- ml_abejas_sinAM |> group_by(ID, sp.armonizado) |>
  mutate(Grado=sum(interaccion==1)) |>
  select(c(-plantas,-interaccion, -Genus, -Family)) |> 
  unique()

# Calculo de grado relativo y zscore
Grados_sinAM <- GradosinAM |> group_by(ID) |>
  mutate(Grado_max=max(Grado)) |>
  mutate(Grado_relativo=Grado/Grado_max) |>
  mutate(Z_Grado=scale(Grado)[,1])

write.csv(x = Grados_sinAM, file = "../02salidas/Grados_Abejas_sinAM.csv", row.names = FALSE)




# sin sp
# Grado abejas
GradosinSP <- ml_abejas_sinSP |> group_by(ID, sp.armonizado) |>
  mutate(Grado=sum(interaccion==1)) |>
  select(c(-plantas,-interaccion, -Genus, -Family)) |> 
  unique()

# Calculo de grado relativo y zscore
Grados_sinSP <- GradosinSP |> group_by(ID) |>
  mutate(Grado_max=max(Grado)) |>
  mutate(Grado_relativo=Grado/Grado_max) |>
  mutate(Z_Grado=scale(Grado)[,1])

write.csv(x = Grados_sinSP, file = "../02salidas/Grados_Abejas_sinSP.csv", row.names = FALSE)



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


# sin Apis mellifera
Grados_sinAM <- subset(Grados_sinAM, !is.nan(Z_Grado))

# histograma de grados abejas
for(i in unique(Grados_sinAM$ID)){
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
  Grados_sinAM_i <- subset(Grados_sinAM, ID==i)
  
  # Grado
  png(filename = paste0("../03figuras/Grado/histograma_grado_Abejas_sinAM_",i,".png"))
  hist(Grados_sinAM_i$Grado, main=paste("Histograma grado absoluto Abejas sin A. mellifera\n red",i),
       ylab="Frecuencia",
       xlab="Grado", las=1)#, breaks = seq(1,5,max(Grados_sinAM_i$Grado)))
  dev.off()
  
  # Grado relativo
  png(filename = paste0("../03figuras/Grado/histograma_gradoRelativo_Abejas_sinAM_",i,".png"))
  hist(Grados_sinAM_i$Grado_relativo, main=paste("Histograma grado relativo Abejas sin A. mellifera\n red",i),
       ylab="Frecuencia",
       xlab="Grado relativo", las=1)
  dev.off()
  
  # Zscore
  png(filename = paste0("../03figuras/Grado/histograma_Z_score_Abejas_sinAM_",i,".png"))
  hist(Grados_sinAM_i$Z_Grado, main=paste("Histograma z score Abejas sin A. mellifera\n red",i),
       ylab="Frecuencia",
       xlab="Z score", las=1)
  dev.off()
}

# sin sp
  Grados_sinSP <- subset(Grados_sinSP, !is.nan(Z_Grado))
  Grados_sinSP <- subset(Grados_sinSP, !is.element(sp.armonizado,c ("Apis.mellifera", "Apis.mellifera.Linnaeus")))

# histograma de grados abejas
  for(i in unique(Grados_sinSP$ID)){
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
  Grados_sinSP_i <- subset(Grados_sinSP, ID==i)
  
  # Grado
  png(filename = paste0("../03figuras/Grado/histograma_grado_Abejas_sinSP_",i,".png"))
  hist(Grados_sinSP_i$Grado, main=paste("Histograma grado absoluto Abejas sin A. mellifera ni .sp\n red",i),
       ylab="Frecuencia",
       xlab="Grado", las=1)#, breaks = seq(1,5,max(Grados_sinAM_i$Grado)))
  dev.off()
  
  # Grado relativo
  png(filename = paste0("../03figuras/Grado/histograma_gradoRelativo_Abejas_sinSP_",i,".png"))
  hist(Grados_sinSP_i$Grado_relativo, main=paste("Histograma grado relativo Abejas sin A. mellifera ni .sp\n red",i),
       ylab="Frecuencia",
       xlab="Grado relativo", las=1)
  dev.off()
  
  # Zscore
  png(filename = paste0("../03figuras/Grado/histograma_Z_score_Abejas_sinSP_",i,".png"))
  hist(Grados_sinSP_i$Z_Grado, main=paste("Histograma z score Abejas sin A. mellifera ni .sp\n red",i),
       ylab="Frecuencia",
       xlab="Z score", las=1)
  dev.off()
}


