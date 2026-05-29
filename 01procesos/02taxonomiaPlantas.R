library(rgbif)
library(dplyr)

# Codigo para taxonomia de plantas
ml.arm <- read.csv("ml.arm.csv")
summary(ml.arm$interaccion)
ml.arm <- subset(ml.arm, interaccion!=0)

plantas <- as.data.frame(x = unique(ml.arm$plantas))
names(plantas) <- "Especie.genero"
head(plantas)

plantas$Genero <- substr(x = plantas$Especie.genero, start = 1, stop = regexpr(pattern = "[.]", text = plantas$Especie.genero)-1)
sort(unique(plantas$Genero))

plantas <- subset(plantas, Genero !="")
sort(unique(plantas$Genero))

str(plantas)

prueba1 <- plantas[1:500,]


unique(plantas$Genero)
generos.plantas <- unique(plantas$Genero)
generos.plantas[generos.plantas=="L"] <- "Laretia"

sort(generos.plantas)

# Usamos kingdom = "Plantae" para evitar confusiones con animales homónimos
resultados <- name_backbone_checklist(
  # plantas$Especie.genero, 
  # plantas$Genero, 
  generos.plantas, 
  # genus  = "Genero",
  rank = "genus",
  kingdom = "Plantae", 
  strict = TRUE, 
  verbose = TRUE
)


resultados.genero <- subset(resultados, rank=="GENUS")
resultados.genero <- subset(resultados.genero, kingdom=="Plantae")
head(resultados.genero)

# 3. Limpiar el resultado
# La función devuelve un tibble con muchas columnas de GBIF
df_final <- resultados.genero |>
  select(kingdom, order,  family, genus, matchType, status, verbatim_name)

genero.aceptado <- unique(
  df_final[df_final$kingdom=="Plantae",# & is.element(df_final$status, c("ACCEPTED","SYNONIMUS")),
           c("order","family","genus","verbatim_name")])
head(genero.aceptado)
unique(genero.aceptado$genus)
sort(setdiff(unique(genero.aceptado$verbatim_name), unique(plantas$Genero)))
sort(setdiff(unique(plantas$Genero),unique(genero.aceptado$verbatim_name)))
sort(intersect(unique(plantas$Genero),unique(genero.aceptado$verbatim_name)))



unique(plantas$Genero)
head(plantas)

plantasFinal <- merge(plantas, genero.aceptado, by.x="Genero", by.y="genus", all.x = TRUE)
plantasFinal$genero.es.igual <- TRUE
plantasFinal$genero.es.igual[plantasFinal$Genero!=plantasFinal$verbatim_name] <- FALSE

# cantidad de generos que coinciden entre original y GBIF
table(unique(plantasFinal[,c("Genero","genero.es.igual")])$genero.es.igual)

names(plantasFinal) <- c("Genero.original","Genero.especie",
                         "Orden.GBIF","Familia.GBIF","Genero.GBIF","genero.es.igual")

head(plantasFinal)
write.csv(plantasFinal, file = "Plantas.armonizado.GBIF.csv",row.names = FALSE)


# print(df_final)