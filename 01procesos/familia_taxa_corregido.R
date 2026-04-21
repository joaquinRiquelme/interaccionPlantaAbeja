# ========================================================
# 1. Instalar y cargar librerías necesarias
# ========================================================
if (!requireNamespace("rgbif", quietly = TRUE)) install.packages("rgbif")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")

library(rgbif)
library(dplyr)
library(readr)
library(stringr)

# ========================================================
# 2. Cargar y preparar el dataframe (M_060)
# ========================================================
getwd()

base_datos <- readRDS("../00baseDatos/base_datos.RDS")


# chile
especies.i <- names(base_datos$matriz_ancha.001)[-1]
especies.i <- names(base_datos$matriz_ancha.002)[-1]
especies.i <- names(base_datos$matriz_ancha.003)[-1]
especies.i <- names(base_datos$matriz_ancha.034)[-1]

especies.i <- names(base_datos$matriz_ancha.026)[-1]
especies.i <- names(base_datos$matriz_ancha.042)[-1]
especies.i <- names(base_datos$matriz_ancha.059)[-1]
#brasil
especies.i <- names(base_datos$matriz_ancha.074)[-1]

especies.i <- names(base_datos$matriz_ancha.076)[-1]
especies.i <- names(base_datos$matriz_ancha.077)[-1]
#venezuela
especies.i <- names(base_datos$matriz_ancha.030)[-1]
especies.i <- names(base_datos$matriz_ancha.031)[-1]

#argentina
especies.i <- names(base_datos$matriz_ancha.022)[-1]
especies.i <- names(base_datos$matriz_ancha.023)[-1]
especies.i <- names(base_datos$matriz_ancha.051)[-1]
especies.i <- names(base_datos$matriz_ancha.072)[-1]
especies.i <- names(base_datos$matriz_ancha.079)[-1]
especies.i <- esp.chacof

#estados unidos
especies.i <- names(base_datos$matriz_ancha.005)[-1]


especies.i <- names(base_datos$matriz_ancha.006)[-1]
especies.i <- names(base_datos$matriz_ancha.007)[-1]
especies.i <- names(base_datos$matriz_ancha.008)[-1]
especies.i <- names(base_datos$matriz_ancha.009)[-1]
especies.i <- names(base_datos$matriz_ancha.010)[-1]
especies.i <- names(base_datos$matriz_ancha.012)[-1]
especies.i <- names(base_datos$matriz_ancha.015)[-1]
especies.i <- names(base_datos$matriz_ancha.016)[-1]
especies.i <- names(base_datos$matriz_ancha.017)[-1]
especies.i <- names(base_datos$matriz_ancha.018)[-1]
especies.i <- names(base_datos$matriz_ancha.036)[-1]
especies.i <- names(base_datos$matriz_ancha.037)[-1]
especies.i <- names(base_datos$matriz_ancha.038)[-1]
especies.i <- names(base_datos$matriz_ancha.039)[-1]
especies.i <- names(base_datos$matriz_ancha.043)[-1]
especies.i <- names(base_datos$matriz_ancha.046)[-1]
especies.i <- names(base_datos$matriz_ancha.047)[-1]
especies.i <- names(base_datos$matriz_ancha.049)[-1]
especies.i <- names(base_datos$matriz_ancha.050)[-1]
especies.i <- names(base_datos$matriz_ancha.058)[-1]


# especies <- base_datos$especies
head(especies.i)

df <- especies.i

head(df)
# Extraer los nombres de especies visitantes desde las columnas (exceptuando la primera columna)
especies_visitantes <- df 

# Formatear nombres: minúsculas y con espacio en lugar de punto
especies_formateadas <- tolower(especies_visitantes)
especies_formateadas <- str_replace_all(especies_formateadas, "\\_", "")
especies_formateadas <- str_replace_all(especies_formateadas, "\\.", " ")
especies_formateadas <- str_trim(especies_formateadas)
especies_formateadas <- str_replace_all(especies_formateadas, "0611", "")

# (Opcional) Eliminar términos como sp, cf, aff
especies_filtradas <- especies_formateadas[!str_detect(especies_formateadas, "\\b(sp|cf|aff)\\b")]

# Extraer el género (primera palabra)
especies_genero <- str_extract(especies_filtradas, "^\\w+")


# ========================================================
# 3. Función para obtener clasificación desde GBIF
# ========================================================
obtener_clasificacion <- function(especie, especie_genero) {
  tryCatch({
    info <- name_backbone(name = especie, strict = FALSE)
    
    if (info$matchType == "NONE") {
      info <- name_backbone(name = especie_genero, strict = FALSE)
    }
    
    if (info$matchType == "NONE") {
      return(data.frame(
        especie = especie,
        especie_genero = especie_genero,
        genero = NA,
        familia = NA,
        orden = NA,
        stringsAsFactors = FALSE
      ))
    } else {
      return(data.frame(
        especie = especie,
        especie_genero = especie_genero,
        genero = info$genus,
        familia = info$family,
        orden = info$order,
        stringsAsFactors = FALSE
      ))
    }
  }, error = function(e) {
    return(data.frame(
      especie = especie,
      especie_genero = especie_genero,
      genero = NA,
      familia = NA,
      orden = NA,
      stringsAsFactors = FALSE
    ))
  })
}

# ========================================================
# 4. Aplicar función a todas las especies visitantes
# ========================================================
clasificacion <- bind_rows(lapply(seq_along(especies_filtradas), function(i) {
  obtener_clasificacion(especies_filtradas[i], especies_genero[i])
}))


head(clasificacion)
table(clasificacion$familia, clasificacion$orden)
table(clasificacion$especie, clasificacion$familia)

head(clasificacion)
sort(unique(clasificacion$familia))
clasificacion.abejas <- subset(clasificacion, 
                               is.element(familia, c("Andrenidae",
                                                     "Halictidae",
                                                     "Apidae",
                                                     "Megachilidae",
                                                     "Melittidae",
                                                     "Colletidae",
                                                     "Stenotritidae")))
table(clasificacion.abejas$especie, clasificacion.abejas$familia)

DIT <- base_datos$DIT
head(DIT)
DIT.i <- subset(DIT, ID=="M_005")
DIT.i

# rasgos de Brasil
brasil <- read.csv("../00baseDatos/basededatosbrazil/Brazilian_bees_traits.csv")#, fileEncoding = "latin1")[-1,]
head(brasil)
summary(brasil)
brasil.i <- unique(brasil[,c("Family","Genus","Specific.epithet",
                      "ITDmeasured","Body.size.class")])
# png("DIT_brasil.png")
# hist(brasil.i$ITDmeasured)
# dev.off()

head(brasil.i)
brasil.i$Genus_and_species <- tolower(paste(brasil.i$Genus, brasil.i$Specific.epithet, sep=" "))
head(clasificacion.abejas)


cruce.b <- merge(clasificacion.abejas[,c("orden","especie")], brasil.i, by.x="especie", by.y="Genus_and_species", all==TRUE)
cruce.b
setdiff(unique(cruce.b$especie), unique(clasificacion.abejas$especie))
setdiff(unique(clasificacion.abejas$especie), unique(cruce.b$especie))

# rasgos de Europa
europa <- read.csv("../00baseDatos/baseeuropea/trait_data_input.csv", fileEncoding = "latin1")[-1,]
head(europa)
summary(europa)


europa.i <- europa[,c("Order","Family","Genus","Species","Genus_and_species",
                      "ITD_.inter.tegular_distance.","ITD_.inter.tegular_distance..2")]
head(europa.i)
europa.i$Genus_and_species <- tolower(europa.i$Genus_and_species)
head(clasificacion.abejas)

cruce <- merge(clasificacion.abejas[,c("orden","especie")], europa.i, by.x="especie", by.y="Genus_and_species", all==TRUE)
cruce
setdiff(unique(cruce$especie), unique(clasificacion.abejas$especie))
setdiff(unique(clasificacion.abejas$especie), unique(cruce$especie))







#

DIT$especie_genero <- str_replace_all(DIT$especies, "\\.", " ")

DIT_europa <- merge(DIT, europa, 
                    by.x="especie_genero", 
                    by.y="Genus_and_species",
                    all.x=TRUE,
                    all.y = FALSE)

head(DIT_europa)

head(brasil)
brasil$genero_especie <- paste(brasil$Genus, brasil$Specific.epithet)
DIT_europa_brasil <- merge(DIT_europa, brasil,
                           by.x="especie_genero",
                           by.y = "genero_especie",
                           all.x=TRUE,
                           all.y=FALSE)
head(DIT_europa_brasil)

png("../03figuras/DIT_porfuente.png", res = 150, width = 1040, height = 720)
par(mfrow=c(3,1))
hist(as.numeric(DIT_europa_brasil$DIT), 
     main="DIT KC",
     xlim=c(0,9),ylim=c(0,300),
     breaks = seq(from=0, to=10,by=0.5))
summary(DIT_europa_brasil$DIT)

hist(as.numeric(DIT_europa_brasil$ITD_.inter.tegular_distance.), 
     main="DIT Europa",
     xlim=c(0,9),ylim=c(0,300),
     breaks = seq(from=0, to=10,by=0.5))
summary(as.numeric(DIT_europa_brasil$ITD_.inter.tegular_distance.))

hist(as.numeric(DIT_europa_brasil$ITDmeasured), 
     main="DIT Brasil",
     xlim=c(0,9),ylim=c(0,300),
     breaks = seq(from=0, to=10,by=0.5))
summary(DIT_europa_brasil$ITDmeasured)
dev.off()

DIT_europa_brasil$delta.1 <- DIT_europa_brasil$DIT- as.numeric(DIT_europa_brasil$ITD_.inter.tegular_distance.)
DIT_europa_brasil$delta.2 <- DIT_europa_brasil$DIT- as.numeric(DIT_europa_brasil$ITDmeasured)


summary(DIT_europa_brasil$delta.1)
summary(DIT_europa_brasil$delta.2)

png("../03figuras/delta_DIT_porfuente.png", res = 150, width = 1040, height = 720)
par(mfrow=c(1,2))
boxplot(DIT_europa_brasil$delta.1, horizontal = FALSE, ylim=c(-1.5,4), main="KC-EUROPA", las=1)
boxplot(DIT_europa_brasil$delta.2, horizontal = FALSE, ylim=c(-1.5,4), main="KC-BRASIL", las=1)
dev.off()

DIT_europa_brasil[DIT_europa_brasil$especie_genero=="Centris tarsata",
                  c("DIT","ITD_.inter.tegular_distance.","ITDmeasured")]
europa.si <- subset(DIT_europa_brasil, !is.na(ITD_.inter.tegular_distance.))
brasil.si <- subset(DIT_europa_brasil, !is.na(ITDmeasured))


DIT_europa_brasil[abs(DIT_europa_brasil$delta.1)>1&!is.na(DIT_europa_brasil$delta.1),
                  c("especie_genero","DIT",
                    "ITD_.inter.tegular_distance.","ITDmeasured",
                    "delta.1")]

DIT_europa_brasil[abs(DIT_europa_brasil$delta.2)>1&!is.na(DIT_europa_brasil$delta.2),
                  c("especie_genero","DIT",
                    "ITD_.inter.tegular_distance.","ITDmeasured",
                    "delta.2")]




nrow(DIT_europa_brasil)

sum(str_detect(DIT$especie_genero, "\\b(sp|sp1|sp2|cf|aff)\\b"))

DIT2 <- subset(DIT_europa_brasil, 
               !str_detect(especie_genero, 
                           "\\b(sp|spp|spA|spB|spC|spD|spE|spF|sp1|sp2|sp3|sp4|sp5|sp6|sp7|sp8|sp9|sp10|cf|aff)\\b"))
nrow(DIT2)
head(DIT2)

DIT2$DIT.final <- as.numeric(DIT2$ITD_.inter.tegular_distance.)
summary(DIT2$DIT.final)
DIT2$DIT.final[is.na(DIT2$DIT.final)] <- DIT2$ITDmeasured[is.na(DIT2$DIT.final)]
DIT2$DIT.final[is.na(DIT2$DIT.final)] <- DIT2$DIT[is.na(DIT2$DIT.final)]

summary(DIT2$DIT.final)
hist(DIT2$DIT.final)

length(unique(DIT2$especie_genero))

ml <- base_datos$matriz_comp_larga
head(ml)
ml$especie_genero <- str_replace_all(ml$especie, "\\.", " ")

ml_europa <- merge(ml, europa[,c("Genus_and_species","ITD_.inter.tegular_distance.")], 
                    by.x="especie_genero", 
                    by.y="Genus_and_species",
                    all.x=TRUE,
                    all.y = FALSE)

ml_europa_brasil <- merge(ml_europa, brasil[,c("genero_especie","ITDmeasured")], 
                   by.x="especie_genero", 
                   by.y="genero_especie",
                   all.x=TRUE,
                   all.y = FALSE)
head(ml_europa_brasil)

ml3 <- merge(ml_europa_brasil, DIT2, all.x=TRUE, all.y=FALSE)
head(ml3)

ml3$DIT.final.final <- ml3$DIT.final
ml3$DIT.final.final[is.na(ml3$DIT.final.final)] <- as.numeric(ml3$ITD_.inter.tegular_distance.[is.na(ml3$DIT.final.final)])
ml3$DIT.final.final[is.na(ml3$DIT.final.final)] <- ml3$ITDmeasured[is.na(ml3$DIT.final.final)]
ml3$DIT.final.final[is.na(ml3$DIT.final.final)] <- ml3$DIT[is.na(ml3$DIT.final.final)]

summary(ml3$DIT.final.final)
matriz.aux <- unique(ml3[!is.na(ml3$DIT.final.final),
                         c("Familia","Family.x","Family.y","genero","especie_genero",
                           "DIT.final.final")])
nrow(matriz.aux)
png("histogramaDIT.final.final.png")
hist(matriz.aux$DIT.final.final)
dev.off()


write.csv(x = matriz.aux, file = "DIT.final.final.csv",row.names = FALSE)

library(lattice)

matriz.aux$body.size <- "01small"
matriz.aux$body.size[matriz.aux$DIT.final.final>2.2] <- "02medium"
matriz.aux$body.size[matriz.aux$DIT.final.final>3.9] <- "03large"

table(matriz.aux$body.size)

png("boxplot.body.size.png")
boxplot(DIT.final.final~body.size, matriz.aux)
dev.off()

png("tamanhos.familia.png")
histogram(~DIT.final.final|Familia,matriz.aux, layout=c(1,6), as.table=TRUE)
dev.off()

png("tamanhos.body.size.familia.png")
histogram(~DIT.final.final|body.size+Familia,matriz.aux, layout=c(3,6), as.table=TRUE)
dev.off()

histogram(~DIT.final.final|genero+Familia,matriz.aux, layout=c(7,6), as.table=TRUE, drop.unused.levels = TRUE)

matriz.aux$DIT.round <- round(matriz.aux$DIT.final.final*1,1)/1
png("barplot.tamanos.png")
barplot(table(matriz.aux$body.size, matriz.aux$DIT.round), col=c("green","turquoise","magenta"))
dev.off()



# Guardar resultados
write_csv(clasificacion, "clasificacion_resultados_M_044COR.csv")


# ========================================================
# 5. Filtrar Hymenoptera y familias de abejas
# ========================================================
# Leer archivo recién guardado (asegurando separación correcta)
resultado_arreglado <- read_csv("clasificacion_resultados_M_001.csv")

# Definir familias de abejas
familias_abejas <- c("Apidae", "Halictidae", "Megachilidae", "Andrenidae", 
                     "Colletidae", "Melittidae", "Stenotritidae")

# Filtrar por orden Hymenoptera y familia de abejas
resultados_abejas <- resultado_arreglado %>%
  filter(orden == "Hymenoptera", familia %in% familias_abejas)

# Ver primeras filas
print(head(resultados_abejas))


# Guardar los resultados en un archivo CSV
write_csv(resultado1, "listado_especies_M_001.csv")
getwd()
