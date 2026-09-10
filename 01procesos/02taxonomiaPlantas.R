# library(rgbif)
# library(dplyr)
library(readxl)
library(writexl)

# Codigo para taxonomia de plantas
ml.arm <- read.csv("../00baseDatos/tablas/matriz_comp_larga.csv")
summary(ml.arm$interaccion)
ml.arm <- subset(ml.arm, interaccion!=0)
head(ml.arm)

ml.arm$plantas[ml.arm$plantas=="Argyranthemum.frutescencs"] <- "Argyranthemum.frutescens" 
ml.arm$plantas[ml.arm$plantas=="Ampetopsis.brevipedunculata"] <- "Ampelopsis.brevipedunculata" 
# ml.arm$plantas[ml.arm$plantas=="AB"] <- "Acacia.bahiensis" 

# codigos de matriz 076
codigos076 <- read.csv("../00baseDatos/datosOriginales/076_Santos _2010/Anexo1_codigos_plantas.csv")

codigos076$genero <- 
  substr(codigos076$Plant.species.visited, 
         start = 1, 
         stop = regexpr(pattern = "[ ]", text = codigos076$Plant.species.visited)-1)

codigos076$especie.full <- 
  substr(codigos076$Plant.species.visited, 
         start = regexpr(pattern = "[ ]", text = codigos076$Plant.species.visited)+1, 
         stop = nchar(codigos076$Plant.species.visited))

codigos076$epiteto <- 
  substr(gsub(x = codigos076$especie.full, pattern = "[().&]",replacement = ""), 
         start = 1,
         stop = regexpr(pattern = "[ ]", text = codigos076$especie.full)-1)

codigos076$epiteto[codigos076$epiteto=="aﬀ "] <- "globosa"
codigos076$epiteto[codigos076$epiteto==""] <- "sp"

codigos076$genero.especie <- paste(codigos076$genero, codigos076$epiteto, sep=".")

ml.arm.sin76 <- subset(ml.arm, ID!="M_076")
ml.arm.76 <- subset(ml.arm, ID=="M_076")

m76 <- merge(ml.arm.76, codigos076, by.x="plantas", by.y = "Plant.species.codes", all.x = TRUE)
m76$plantas <- m76$genero.especie
m76 <- m76[,c("ID","plantas","especie","interaccion")]

ml.arm <- rbind(ml.arm.sin76, m76)
# ml.arm$plantas[ml.arm$plantas=="AO"] <- "Anacardium.occidentale" 
# ml.arm$plantas[ml.arm$plantas=="SA"] <- "Stigmaphyllon.auriculatum" 
# ml.arm$plantas[ml.arm$plantas=="AT"] <- "Alternanthera.brasiliana" 
# ml.arm$plantas[ml.arm$plantas=="BC"] <- "Boerhavia.coccinia" 
# ml.arm$plantas[ml.arm$plantas=="ZC"] <- "Ziziphus.cotinifolia" 





# AO =  L, Anacardiaceae
# SA = Stigmaphyllon auriculatum (Cav.) A. Juss., Malpighiaceae
# AT= Alternanthera brasiliana (L.) Kuntze, Amaranthaceae
# BC = Boerhavia coccinia Mill., Nyctaginaceae
# ZC=  Ziziphus cotinifolia Reiss., Rhamnaceae

ml.arm$plantas[ml.arm$plantas=="Epilobium.latifolium"] <- "Chamaenerium.angustifolium"
ml.arm$plantas[ml.arm$plantas=="Stachytapheta.jamaicencis"] <- "Stachytarpheta.jamaicensis"
ml.arm$plantas[ml.arm$plantas=="Psoralia.bituminosa"] <- "Bituminaria.bituminosa"
ml.arm$plantas[ml.arm$plantas=="Psoralea.bituminosa"] <- "Bituminaria.bituminosa"
ml.arm$plantas[ml.arm$plantas=="Ranumculus.sulphureus"] <- "Ranunculus.sulphureus"
ml.arm$plantas[ml.arm$plantas=="Anthriscus.aemula"] <- "Anthriscus.sylvestris"
ml.arm$plantas[ml.arm$plantas=="Azorella.monanthos"] <- "Azorella.monantha"
ml.arm$plantas[ml.arm$plantas=="Sangiunaria.canadensis"] <- "Sanguinaria.canadensis"
ml.arm$plantas[ml.arm$plantas=="Chicocca.alba"] <- "Chiococa.alba"
ml.arm$plantas[ml.arm$plantas=="Clerodendrum.molle.var."] <- "Clerodendrum.molle"
ml.arm$plantas[ml.arm$plantas=="Cassinia.vauvilliersi"] <- "Cassinia.vauvilliersii"
ml.arm$plantas[ml.arm$plantas=="Brachyome.sinclairii"] <- "Brachycome.sinlcairii"
ml.arm$plantas[ml.arm$plantas=="Ipomoea.acuminata"] <- "Ipomoea.indica"
ml.arm$plantas[ml.arm$plantas=="Reseada.luteola"] <- "Reseda.luteola"
ml.arm$plantas[ml.arm$plantas=="Ligustrum.joponicum"] <- "Ligustrum.japonicum"
ml.arm$plantas[ml.arm$plantas=="Epilobium.angustifolium"] <- "Chamaenerion.angustifolium"
ml.arm$plantas[ml.arm$plantas=="Clethra.barvinervis"] <- "Clethra.barbinervis"
ml.arm$plantas[ml.arm$plantas=="Desmodium.padocarpum"] <- "Desmodium.podocarpum"
ml.arm$plantas[ml.arm$plantas=="Mosta.dianthera"] <- "Mosla.dianthera"
ml.arm$plantas[ml.arm$plantas=="Clerodendron.trichotomum"] <- "Clerodendrum.trichotomum"
ml.arm$plantas[ml.arm$plantas=="Persicaria.comspicua"] <- "Persicaria.conspicua"
ml.arm$plantas[ml.arm$plantas=="Ptantago.asiatica"] <- "Plantago.asiatica"
ml.arm$plantas[ml.arm$plantas=="Sonchus.tenerrinus"] <- "Sonchus.tenerrimus"
ml.arm$plantas[ml.arm$plantas=="Thymelaea.hirusta"] <- "Thymelaea.hirsuta"
ml.arm$plantas[ml.arm$plantas=="Cerastium.vulgatum"] <- "Cerastium.glomeratum"
ml.arm$plantas[ml.arm$plantas=="Ilysanthes.dubia"] <- "Lindernia.dubia"
ml.arm$plantas[ml.arm$plantas=="Verbena.urticaefolia"] <- "Verbena.urticifolia"
ml.arm$plantas[ml.arm$plantas=="Roussea.simplex."] <- "Roussea.simplex"
ml.arm$plantas[ml.arm$plantas=="Castilleia.miniata"] <- "Castilleja.miniata"
 
ml.arm$plantas <- gsub(pattern = "+AF8-",replacement = ".", x = ml.arm$plantas)
ml.arm$plantas <- gsub(pattern = "[+]",replacement = "", x = ml.arm$plantas)

ml.arm$plantas <- paste0(toupper(substr(ml.arm$plantas, 1, 1)), substring(ml.arm$plantas, 2))

ml.arm$plantas <- gsub(pattern = "[.][.]",replacement = ".", x = ml.arm$plantas)
ml.arm$plantas[ml.arm$plantas=="Bakerella.hoyifolia.ssp.bojeri"] <- "Bakerella.hoyifolia"
ml.arm$plantas[ml.arm$plantas=="Doratoxylon.apetalum.var.apetalum"] <- "Doratoxylon.apetalum"
ml.arm$plantas[ml.arm$plantas=="Erythrospermum.monticolum.var.monticolum"] <- "Erythrospermum.monticolum"
ml.arm$plantas[ml.arm$plantas=="Euodia.obtusifolia.ssp.gigas.var.brachypoda"] <- "Euodia obtusifolia.ssp.gigas.var.gigas"
ml.arm$plantas[ml.arm$plantas=="Pandanus.barklyi.var.barklyi"] <- "Pandanus.barklyi"
ml.arm$plantas[ml.arm$plantas=="Pleurostylia.leucocarpa"] <- "Pleurostylia.leucocarpa."

ml.arm$Especie.planta <- gsub(pattern = "[.]",replacement = " ", x = ml.arm$plantas)


sort(unique(ml.arm$Especie.planta))
sort(unique(ml.arm$plantas))[1500:2000]




plantas_kathy2 <- readxl::read_excel("../00baseDatos/armonizacinplantaskc/Plantas_Kathy_final2.xlsx", sheet = 4, skip = 1)[,c(2,19,20,21)]
head(plantas_kathy2)
names(plantas_kathy2) <- c("Especie.original","Familia.planta","Genero.planta","Especie.planta.nuevo")
plantas_kathy2 <- rbind(plantas_kathy2, data.frame(Especie.original="Azorella.bolacina",
                                                   Familia.planta="Apiaceae",
                                                   Genero.planta="Azorella",
                                                   Especie.planta.nuevo="Azorella.madreporica"))

plantas_kathy2 <- rbind(plantas_kathy2, data.frame(Especie.original="Penstemon.unilateralis",
                                                   Familia.planta="Plantaginaceae",
                                                   Genero.planta="Penstemon",
                                                   Especie.planta.nuevo="Penstemon.unilateralis"))

plantas_kathy2 <- rbind(plantas_kathy2, data.frame(Especie.original="Lychnis.triflora",
                                                   Familia.planta="Caryophyllaceae",
                                                   Genero.planta="Silene",
                                                   Especie.planta.nuevo="Silene.sorensenis"))

plantas_kathy2 <- rbind(plantas_kathy2, data.frame(Especie.original="Lychnis.triflora",
                                                   Familia.planta="Caryophyllaceae",
                                                   Genero.planta="Silene",
                                                   Especie.planta.nuevo="Silene.sorensenis"))

plantas_kathy2 <- rbind(plantas_kathy2, data.frame(Especie.original="Chrysanthemum.leucanthemum",
                                                   Familia.planta="Asteraceae",
                                                   Genero.planta="Leucanthemum",
                                                   Especie.planta.nuevo="Leucanthemum.vulgare"))

plantas_kathy2 <- rbind(plantas_kathy2, data.frame(Especie.original="Benthamidia.japonica",
                                                   Familia.planta="Cornaceae",
                                                   Genero.planta="Cornus",
                                                   Especie.planta.nuevo="Cornus.kousa"))


plantas_kathy2 <- rbind(plantas_kathy2, data.frame(Especie.original="Helianthus.scaberrimus",
                                                   Familia.planta="Asteraceae",
                                                   Genero.planta="Helianthus",
                                                   Especie.planta.nuevo="Helianthus.bolanderi"))



plantas_kathy2$Especie.original[plantas_kathy2$Especie.planta.nuevo=="Chamaenerion angustifolium subsp. angustifolium"] <- "Chamaenerium.angustifolium"
plantas_kathy2$Especie.original[plantas_kathy2$Especie.original=="Sonchus.tenerrinus"] <- "Sonchus.tenerrimus"
plantas_kathy2$Especie.original[plantas_kathy2$Especie.original=="Sonchus.cf..ortunoi"] <- "Sonchus.cf.ortunoi"
plantas_kathy2$Especie.original[plantas_kathy2$Especie.original=="Myrceugenia.ovata.var..ovata"] <- "Myrceugenia.ovata.var.ovata"
plantas_kathy2$Especie.original[plantas_kathy2$Especie.original=="Roussea.simplex."] <- "Roussea.simplex"
plantas_kathy2$Especie.original[plantas_kathy2$Especie.original=="Castilleia.miniata"] <- "Castilleja.miniata"

plantas_kathy2$Especie.original <- gsub(pattern = "[.]",replacement = " ", x = plantas_kathy2$Especie.original)

write_xlsx(plantas_kathy2, "plantas_kathy3.xlsx")

ml.plantas.armonizado <- merge(ml.arm, plantas_kathy2, by.x="Especie.planta", by.y="Especie.original")
ml.plantas.armonizado2 <- merge(ml.arm, plantas_kathy2, by.x="Especie.planta", by.y="Especie.original", all.x=TRUE)


setdiff(unique(ml.plantas.armonizado$Especie.planta), unique(ml.plantas.armonizado2$Especie.planta))

plantas.sin.arm <- setdiff(unique(ml.plantas.armonizado2$Especie.planta), unique(ml.plantas.armonizado$Especie.planta))

data.sin.arm <- subset(ml.arm, is.element(Especie.planta, plantas.sin.arm))
print(table(unique(data.sin.arm[,c("ID","plantas")])$ID))



write.csv(data.sin.arm, "datos.no.armonizados.plantas.csv", row.names = FALSE)
write_xlsx(data.sin.arm, "datos.no.armonizados.plantas.xlsx")

print(head(subset(data.sin.arm, !(ID %in% c("M_040","M_041","M_046", "M_075.2","M_076","M_079")))))
stop(paste(length(unique(plantas.sin.arm))," especies faltantes"))

head(ml.plantas.armonizado)
ml.plantas.armonizado$Especie.planta.nuevo[is.na(ml.plantas.armonizado$Especie.planta.nuevo)] <- ml.plantas.armonizado$Especie.planta[is.na(ml.plantas.armonizado$Especie.planta.nuevo)]
# write.csv()

ml.plantas.armonizado$interaccion <- 1

ml.plantas.armonizado2 <- ml.plantas.armonizado |> 
  group_by(ID, especie, Familia.planta, Genero.planta, Especie.planta.nuevo) |>
  summarise(interaccionn=sum(interaccion, na.rm=TRUE))
unique(ml.plantas.armonizado2$interaccionn)
ml.plantas.armonizado2$interaccion <- 1
ml.plantas.armonizado2 <- unique(ml.plantas.armonizado2)

write.csv(ml.plantas.armonizado2, file.path(dir.salidas,"ml.plantas.armonizado2.csv"), row.names = FALSE)


######

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