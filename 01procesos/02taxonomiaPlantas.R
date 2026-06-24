library(rgbif)
library(dplyr)

# Codigo para taxonomia de plantas
ml.arm <- read.csv("ml.arm.csv")
summary(ml.arm$interaccion)
ml.arm <- subset(ml.arm, interaccion!=0)
head(ml.arm)

plantas <- as.data.frame(x = unique(ml.arm$plantas))
names(plantas) <- "Especie.genero"
head(plantas)

plantas$Genero <- substr(x = plantas$Especie.genero, start = 1, stop = regexpr(pattern = "[.]", text = plantas$Especie.genero)-1)
sort(unique(plantas$Genero))

plantas <- subset(plantas, Genero !="")
sort(unique(plantas$Genero))

str(plantas)

# prueba1 <- plantas[1:500,]


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
head(plantasFinal)
# cantidad de generos que coinciden entre original y GBIF
table(unique(plantasFinal[,c("Genero","genero.es.igual")])$genero.es.igual)

names(plantasFinal) <- c("Genero.original","Genero.especie",
                         "Orden.GBIF","Familia.GBIF","Genero.GBIF","genero.es.igual")

head(plantasFinal)
write.csv(plantasFinal, file = "Plantas.armonizado.GBIF.csv",row.names = FALSE)
plantasFinal <- read.csv(file = "Plantas.armonizado.GBIF.csv")

plantas <- subset(plantasFinal, genero.es.igual==TRUE)
head(plantas)

plantas2 <- plantas |> group_by(Genero.especie) |> mutate(
  n.fam = length(unique(Familia.GBIF))
)
head(plantas2)

write.csv(plantas2, "plantas2.csv",row.names = FALSE)
plantas2 <- read.csv("plantas2.csv")
head(plantas2)

familias.cor <- readxl::read_xlsx("../00baseDatos/Plantas_Kathy_202060615.xlsx", sheet = 5)
plantas.cor <- readxl::read_xlsx("../00baseDatos/Plantas_Kathy_202060615.xlsx", sheet = 6)
plantas.cor <- plantas.cor[,c("Genero.especie","Género actual","Especie actual")]

head(plantas2)
head(plantas.cor)

plantas2$Genero.especie <- gsub(plantas2$Genero.especie, pattern = "\\.$", replacement = "")
plantas2$Especie <- gsub(plantas2$Genero.especie, pattern = "[.]", replacement = " ")

plantas.cor$Genero.especie <- gsub(plantas.cor$Genero.especie, pattern = "\\.$", replacement = "")

# aaa <- merge(plantas2, plantas.cor, by="Especie", all.x=TRUE, all.y=TRUE)
# aab <- merge(plantas2, plantas.cor, by="Especie", all.x=TRUE)
ccc <- unique(merge(plantas2, plantas.cor, by="Genero.especie", all.x=TRUE))

ccc2 <- subset(ccc, !is.na(`Especie actual`))

unique()



sum(table(aaa$Orden.GBIF))
sum(table(aaa$ORDEN))

plantas2$Familia.GBIF[plantas2$n.fam!=1]<- NA

barplot(table(plantas2$n.fam))
head(plantas2)

ml.arm.2 <- merge(plantas2, ml.arm, by.x = "Genero.especie", by.y = "plantas", all.x=FALSE, all.y=TRUE)
head(ml.arm.2)
ml.arm.2$planta <- ml.arm.2$Genero.especie 
ml.arm.2$Genero.especie <- NULL
ml.arm.2$genero.es.igual <- NULL
ml.arm.2$Bases.de.datos <- NULL
head(ml.arm.2)

ml.arm.2 <- unique(ml.arm.2[,c("ID","Orden.GBIF","Familia.GBIF","Genero.GBIF","Genero.original","planta",
                        "interaccion","especie","esp","Aceptado","sp.armonizado")])

head(ml.arm.2)

write.csv(ml.arm.2, "ml.armonizado.plantas.abejas.csv", row.names = FALSE)
ml.arm.2 <- read.csv("ml.armonizado.plantas.abejas.csv")

head(ml.arm.2)


aaaa

