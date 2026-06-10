# Grado y TC de abejas

base_datos <- readRDS("../00baseDatos/base_datos.RDS")
# especie <- base_datos$especies
# DIT <- base_datos$DIT 
# head(DIT)
# DIT$Abeja <- gsub(pattern = "_",replacement = ".",x = DIT$Abeja)
# DIT$ID <- NULL
# DIT <- unique(DIT)

DIT <- read.csv("DIT.baseDatos.csv")
head(DIT)

# Grados <- read.csv("../02salidas/Grados.csv")
# head(Grados)
# Grados_Abejas <- read.csv("../02salidas/Grados_Abejas.csv")
Grados_Abejas <- read.csv("../02salidas/GFE.csv")

Grados_Abejas <- subset(Grados_Abejas, !is.nan(Z_Grado))

GyTC <- merge(Grados_Abejas, DIT, by="sp.armonizado", all.x = TRUE)
# GyTC_todas <- merge(Grados, DIT, by.x="especie", by.y="especies", all.x = TRUE)
# GyTC_todas_abejas <- subset(GyTC_todas, !is.na(Familia))

# table(is.na(GyTC_todas_abejas$DIT))

head(GyTC)
summary(GyTC$Z_Grado)
# head(GyTC_todas)
# table(GyTC_todas$Familia)
# head(Grados)


for(i in unique(GyTC$ID)){
  print(i)
  GyTC_i <- subset(GyTC, ID==i)
  png(filename = paste0("../03figuras/TCyGrado/dispersion_Grado_TC_",i,".png"))
  plot(Grado~DIT.final, data = GyTC_i, main=paste("DIT vs Grado \n matriz",i), las=1,
       ylab="Grado", 
       xlab="DIT (mm)",
       xlim=c(0,9),
       ylim=c(0,max(GyTC_i$Grado, na.rm = TRUE)*1.1))
  dev.off()
  
  png(filename = paste0("../03figuras/TCyGrado/dispersion_GradoRelativo_TC_",i,".png"))
  plot(Grado_relativo~DIT.final, data = GyTC_i, main=paste("DIT vs Grado relativo \n matriz",i), las=1,
       ylab="Grado relativo", 
       xlab="DIT (mm)",
       xlim=c(0,9),
       ylim=c(0,max(GyTC_i$Grado_relativo, na.rm = TRUE)*1.1))
  dev.off()
  
  png(filename = paste0("../03figuras/TCyGrado/dispersion_Zscore_TC_",i,".png"))
  plot(Z_Grado~DIT.final, data = GyTC_i, main=paste("DIT vs Z score \n matriz",i), las=1,
       ylab="Z score", 
       xlab="DIT (mm)",
       xlim=c(0,9),
       ylim=c(-1.5,max(GyTC_i$Z_Grado, na.rm = TRUE)*1.1))
  dev.off()
  
}


# Global


par(mfrow=c(3,1))
# png(filename = paste0("../03figuras/TCyGrado_global/dispersion_Grado_TC_global.png"))
plot(Grado~DIT.final, data = GyTC, main="DIT vs Grado todas las matrices", las=1,
     ylab="Grado", 
     xlab="DIT (mm)",
     xlim=c(0,9),
     ylim=c(0,max(GyTC$Grado, na.rm = TRUE)*1.1))
# dev.off()

# png(filename = paste0("../03figuras/TCyGrado_global/dispersion_GradoRelativo_TC_global.png"))
plot(Grado_relativo~DIT.final, data = GyTC, main="DIT vs Grado relativo global", las=1,
     ylab="Grado relativo", 
     xlab="DIT (mm)",
     xlim=c(0,9),
     ylim=c(0,max(GyTC$Grado_relativo, na.rm = TRUE)*1.1))
# dev.off()

# png(filename = paste0("../03figuras/TCyGrado_global/dispersion_Zscore_TC_global.png"))
plot(Z_Grado~DIT.final, data = GyTC, main="DIT vs Z score global", las=1,
     ylab="Z score", 
     xlab="DIT (mm)",
     xlim=c(0,9),
     ylim=c(-1.5,max(GyTC$Z_Grado, na.rm = TRUE)*1.1))
# dev.off()
# 
library(psych)
pairs.panels(unique(GyTC[,c("Grado","Grado_relativo","Z_Grado","DIT.final")]))


write.csv(GyTC, file = "GyTCV2.csv", row.names = FALSE)
