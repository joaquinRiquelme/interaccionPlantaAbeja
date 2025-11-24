# Grado y TC de abejas

base_datos <- readRDS("../00baseDatos/base_datos.RDS")
especie <- base_datos$especies
DIT <- base_datos$DIT 
head(DIT)
DIT$Abeja <- gsub(pattern = "_",replacement = ".",x = DIT$Abeja)
DIT$ID <- NULL
DIT <- unique(DIT)

Grados <- read.csv("../02salidas/Grados.csv")
head(Grados)
Grados_Abejas <- read.csv("../02salidas/Grados_Abejas.csv")
Grados_Abejas <- subset(Grados_Abejas, !is.nan(Z_Grado))

GyTC <- merge(Grados, DIT, by.x="especie", by.y="especies")
head(GyTC)

for(i in unique(GyTC$ID)){
  print(i)
  GyTC_i <- subset(GyTC, ID==i)
  png(filename = paste0("../03figuras/TCyGrado/dispersion_Grado_TC_",i,".png"))
  plot(Grado~DIT, data = GyTC_i, main=paste("DIT vs Grado \n matriz",i), las=1,
       ylab="Grado", 
       xlab="DIT (mm)",
       xlim=c(0,9),
       ylim=c(0,max(GyTC_i$Grado, na.rm = TRUE)*1.1))
  dev.off()
  
  png(filename = paste0("../03figuras/TCyGrado/dispersion_GradoRelativo_TC_",i,".png"))
  plot(Grado_relativo~DIT, data = GyTC_i, main=paste("DIT vs Grado relativo \n matriz",i), las=1,
       ylab="Grado relativo", 
       xlab="DIT (mm)",
       xlim=c(0,9),
       ylim=c(0,max(GyTC_i$Grado_relativo, na.rm = TRUE)*1.1))
  dev.off()
  
  png(filename = paste0("../03figuras/TCyGrado/dispersion_Zscore_TC_",i,".png"))
  plot(Z_Grado~DIT, data = GyTC_i, main=paste("DIT vs Z score \n matriz",i), las=1,
       ylab="Z score", 
       xlab="DIT (mm)",
       xlim=c(0,9),
       ylim=c(0,max(GyTC_i$Z_Grado, na.rm = TRUE)*1.1))
  dev.off()
  
}

# abejas
GyTC_A <- merge(Grados_Abejas, DIT, by.x="especie", by.y="especies")
head(GyTC_A)

for(i in unique(GyTC_A$ID)){
  print(i)
  if(i =="M_036"){next}
  if(i =="M_013"){next}
  if(i =="M_060"){next}
  if(i =="M_009"){next}
  if(i =="M_074"){next}
  if(i =="M_038"){next}
  GyTC_i <- subset(GyTC_A, ID==i)
  png(filename = paste0("../03figuras/TCyGrado/dispersion_Grado_TC_Abejas_",i,".png"))
  plot(Grado~DIT, data = GyTC_i, main=paste("DIT vs Grado \n matriz",i), las=1,
       ylab="Grado", 
       xlab="DIT (mm)",
       xlim=c(0,9),
       ylim=c(0,max(GyTC_i$Grado, na.rm = TRUE)*1.1))
  dev.off()
  
  png(filename = paste0("../03figuras/TCyGrado/dispersion_GradoRelativo_TC_Abejas_",i,".png"))
  plot(Grado_relativo~DIT, data = GyTC_i, main=paste("DIT vs Grado relativo \n matriz",i), las=1,
       ylab="Grado relativo", 
       xlab="DIT (mm)",
       xlim=c(0,9),
       ylim=c(0,max(GyTC_i$Grado_relativo, na.rm = TRUE)*1.1))
  dev.off()
  
  png(filename = paste0("../03figuras/TCyGrado/dispersion_Zscore_TC_Abejas_",i,".png"))
  plot(Z_Grado~DIT, data = GyTC_i, main=paste("DIT vs Z score \n matriz",i), las=1,
       ylab="Z score", 
       xlab="DIT (mm)",
       xlim=c(0,9),
       ylim=c(0,max(GyTC_i$Z_Grado, na.rm = TRUE)*1.1))
  dev.off()
  
}



# Global
png(filename = paste0("../03figuras/TCyGrado_global/dispersion_Grado_TC_global.png"))
plot(Grado~DIT, data = GyTC, main="DIT vs Grado todas las matrices", las=1,
     ylab="Grado", 
     xlab="DIT (mm)",
     xlim=c(0,9),
     ylim=c(0,max(GyTC$Grado, na.rm = TRUE)*1.1))
dev.off()

png(filename = paste0("../03figuras/TCyGrado_global/dispersion_GradoRelativo_TC_global.png"))
plot(Grado_relativo~DIT, data = GyTC, main="DIT vs Grado relativo global", las=1,
     ylab="Grado relativo", 
     xlab="DIT (mm)")
     # xlim=c(0,9),
     # ylim=c(0,max(GyTC_i$Grado_relativo, na.rm = TRUE)*1.1))
dev.off()

png(filename = paste0("../03figuras/TCyGrado_global/dispersion_Zscore_TC_global.png"))
plot(Z_Grado~DIT, data = GyTC, main="DIT vs Z score global", las=1,
     ylab="Z score", 
     xlab="DIT (mm)")
     # xlim=c(0,9),
     # ylim=c(0,max(GyTC_i$Z_Grado, na.rm = TRUE)*1.1))
dev.off()


# Global abejas
png(filename = paste0("../03figuras/TCyGrado_global/dispersion_Grado_TC_global_Abejas.png"))
plot(Grado~DIT, data = GyTC_A, main="DIT vs Grado todas las matrices", las=1,
     ylab="Grado", 
     xlab="DIT (mm)",
     xlim=c(0,9),
     ylim=c(0,max(GyTC_A$Grado, na.rm = TRUE)*1.1))
dev.off()

png(filename = paste0("../03figuras/TCyGrado_global/dispersion_GradoRelativo_TC_global_Abejas.png"))
plot(Grado_relativo~DIT, data = GyTC_A, main="DIT vs Grado relativo global", las=1,
     ylab="Grado relativo", 
     xlab="DIT (mm)")
# xlim=c(0,9),
# ylim=c(0,max(GyTC_i$Grado_relativo, na.rm = TRUE)*1.1))
dev.off()

png(filename = paste0("../03figuras/TCyGrado_global/dispersion_Zscore_TC_global_Abejas.png"))
plot(Z_Grado~DIT, data = GyTC_A, main="DIT vs Z score global", las=1,
     ylab="Z score", 
     xlab="DIT (mm)")
# xlim=c(0,9),
# ylim=c(0,max(GyTC_i$Z_Grado, na.rm = TRUE)*1.1))
dev.off()

