library(rgbif)
library(dplyr)
library("taxize")

# base_datos <- readRDS("../00baseDatos/base_datos.RDS")
# ml <- base_datos$matriz_comp_larga
ml <- read.csv("../02salidas/ml.plantas.armonizado2.csv")
unique(ml$ID)
unique(ml$especie)
ml$esp <- gsub(ml$especie, replacement = " ", pattern="[.]")
sp.ml <- unique(ml$esp)


# DIT ----


DIT <- base_datos$DIT
DIT$esp <- gsub(DIT$especies, replacement = " ", pattern="[.]")

DIT$Abeja[DIT$Abeja=="Alloscirtetica.rufitarsis-002"] <- "Alloscirtetica.rufitarsis_002"
DIT$especies[DIT$Abeja=="Alloscirtetica.rufitarsis_002"] <- "Alloscirtetica.rufitarsis"
DIT$esp[DIT$Abeja=="Alloscirtetica.rufitarsis_002"] <- "Alloscirtetica rufitarsis"

DIT$Abeja[DIT$Abeja=="Agapostemon_virescens_073"] <- "Agapostemon.virescens_073"
DIT$esp[DIT$Abeja=="Agapostemon.virescens_073"] <- "Agapostemon virescens"
DIT$especies[DIT$Abeja=="Agapostemon.virescens_073"] <- "Agapostemon.virescens"
DIT$genero[DIT$Abeja=="Agapostemon.virescens_073"] <- "Agapostemon"

DIT$Abeja[DIT$Abeja=="Ashmeadiella bucconis_062"] <- "Ashmeadiella.bucconis_062"
DIT$genero[DIT$Abeja=="Ashmeadiella.bucconis_062"] <- "Ashmeadiella"

DIT$Abeja[DIT$Abeja=="Augochlora_pura_073"] <- "Augochlora.pura_073"
DIT$especies[DIT$Abeja=="Augochlora.pura_073"] <- "Augochlora.pura"
DIT$esp[DIT$Abeja=="Augochlora.pura_073"] <- "Augochlora pura"
DIT$genero[DIT$Abeja=="Augochlora.pura_073"] <- "Augochlora"

DIT$Abeja[DIT$Abeja=="Augochlorella_aurata_073"] <- "Augochlorella.aurata_073"
DIT$Abeja[DIT$Abeja=="Augochlorella aurata_1521"] <- "Augochlorella.aurata_073"
DIT$especies[DIT$Abeja=="Augochlorella.aurata_073"] <- "Augochlorella.aurata"
DIT$genero[DIT$Abeja=="Augochlorella.aurata_073"] <- "Augochlorella"
DIT$esp[DIT$Abeja=="Augochlorella.aurata_073"] <- "Augochlorella aurata"

DIT$esp[DIT$Abeja=="Bombus.terrestis_049"] <- "Bombus terrestris"
DIT$especies[DIT$Abeja=="Bombus.terrestis_049"] <- "Bombus.terrestris"
DIT$Abeja[DIT$Abeja=="Bombus.terrestis_049"] <- "Bombus.terrestris_049"

DIT$Abeja[DIT$Abeja=="Andrena.crategi_1521"] <- "Andrena.crataegi_1521"
DIT$especies[DIT$Abeja=="Andrena.crataegi_1521"] <- "Andrena.crataegi"
DIT$esp[DIT$Abeja=="Andrena.crataegi_1521"] <- "Andrena crataegi"

DIT$Abeja[DIT$Abeja=="Dasypoda cingulata_016"] <- "Dasypoda.cingulata_016"
DIT$genero[DIT$Abeja=="Dasypoda.cingulata_016"] <- "Dasypoda"

DIT$Abeja[DIT$Abeja=="Dufourea fimbriata_1521"] <- "Dufourea.fimbriata_1521"
DIT$genero[DIT$Abeja=="Dufourea.fimbriata_1521"] <- "Dufourea"

DIT$Abeja[DIT$Abeja=="Dufourea maura_1521"] <- "Dufourea.maura_1521"
DIT$genero[DIT$Abeja=="Dufourea.maura_1521"] <- "Dufourea"

DIT$Abeja[DIT$Abeja=="Halictus_confusus_073"] <- "Halictus.confusus_073"
DIT$especies[DIT$Abeja=="Halictus.confusus_073"] <- "Halictus.confusus"
DIT$genero[DIT$Abeja=="Halictus.confusus_073"] <- "Halictus"
DIT$esp[DIT$Abeja=="Halictus.confusus_073"] <- "Halictus confusus"

DIT$Abeja[DIT$esp=="Lassioglossum.fuscipenne_025"] <- "Lasioglossum.fuscipenne_025" 
DIT$especies[DIT$esp=="Lassioglossum fuscipenne"] <- "Lasioglossum.fuscipenne" 
DIT$genero[DIT$esp=="Lassioglossum fuscipenne"] <- "Lasioglossum" 
DIT$Familia[DIT$esp=="Lassioglossum fuscipenne"] <- "Halictidae" 

DIT$Abeja[DIT$Abeja=="Trigona_fuscipennis_s2010"] <- "Trigona.fuscipennis_s2010" 
DIT$especies[DIT$Abeja=="Trigona.fuscipennis_s2010"] <- "Trigona.fuscipennis" 
DIT$genero[DIT$Abeja=="Trigona.fuscipennis_s2010"] <- "Trigona"
DIT$esp[DIT$Abeja=="Trigona.fuscipennis_s2010"] <- "Trigona fuscipennis"

sp.dit <- unique(DIT$esp)

# tabla para la armonizacion de nombres cientificos ----
armonizacion <- read.csv("../00baseDatos/Armonizacion nombre-DIT-porfin.csv")
armonizacion <- subset(armonizacion, Nombre.en.matriz!="")
armonizacion <- subset(armonizacion, Nombre.en.matriz!=" ")
armonizacion <- subset(armonizacion, !is.na(Nombre.en.matriz))
armonizacion$Nombre.en.matriz <- gsub(pattern = "[/]", replacement = " ", x = armonizacion$Nombre.en.matriz)
armonizacion <- subset(armonizacion, Origen!="m_072")#&Nombre.en.matriz!="Bombus atratus")

head(armonizacion)
sp.m <- unique(armonizacion$Nombre.en.matriz)
acep.m <- unique(armonizacion$Aceptado)

sort(intersect(sp.m, sp.ml))
sort(setdiff(sp.m, sp.ml))

sort(intersect(sp.m, sp.dit))
sort(setdiff(sp.m, sp.dit))
sort(setdiff(sp.dit, sp.m))


# columna de nombre armonizado para ml ----
ml.arm <- merge(ml, armonizacion[,c("Nombre.en.matriz","Aceptado","Bases.de.datos")], 
                by.x = "esp", by.y="Nombre.en.matriz", all.x = TRUE, all.y=FALSE)
head(ml.arm)
sort(unique(ml.arm$Aceptado))

ml.arm$sp.armonizado <- ml.arm$Aceptado
ml.arm$sp.armonizado[is.na(ml.arm$sp.armonizado)] <- ml.arm$esp[is.na(ml.arm$sp.armonizado)]
head(ml.arm)
length(unique(ml.arm$sp.armonizado))
length(unique(ml$especie))

head(ml.arm)
summary(is.na(ml.arm$Aceptado)==TRUE)
summary(is.na(ml.arm$sp.armonizado)==TRUE)

write.csv(ml.arm, file = "ml.arm.csv", row.names = FALSE)

# DIT con nombres armonizados ----

DIT.arm <- merge(DIT, armonizacion[,c("Nombre.en.matriz","Aceptado","Bases.de.datos")], 
                 by.x = "esp", by.y="Nombre.en.matriz", all.x = TRUE, all.y=FALSE)
unique(DIT.arm$Aceptado)
head(DIT.arm)
sort(unique(DIT.arm$Aceptado))
sort(unique(DIT.arm$especies))

DIT.arm$sp.armonizado <- DIT.arm$Aceptado
DIT.arm$sp.armonizado[is.na(DIT.arm$sp.armonizado)] <- DIT.arm$esp[is.na(DIT.arm$sp.armonizado)]
head(DIT.arm)
length(unique(DIT.arm$sp.armonizado))
length(unique(DIT.arm$esp))

sort(setdiff(unique(DIT.arm$esp), unique(DIT.arm$sp.armonizado)))
sort(setdiff(unique(DIT.arm$sp.armonizado), unique(DIT.arm$esp)))
sort(intersect(unique(DIT.arm$sp.armonizado), unique(DIT.arm$esp)))

summary(is.na(DIT.arm$Aceptado)==TRUE)
summary(is.na(DIT.arm$sp.armonizado)==TRUE)


# Cruce de DIT

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

# rasgos de Europa
europa <- read.csv("../00baseDatos/baseeuropea/trait_data_input.csv", fileEncoding = "latin1")[-1,]
head(europa)
summary(europa)
europa$FuenteDatos <- "Europa"

europa.i <- europa[,c("Order","Family","Genus","Species","Genus_and_species",
                      "ITD_.inter.tegular_distance.","ITD_.inter.tegular_distance..2")]
head(europa.i)
europa.i <- unique(europa.i)
europa.i$Genus_and_species <- tolower(europa.i$Genus_and_species)


library(stringr)
DIT.arm$especie_genero <- str_replace_all(DIT.arm$sp.armonizado, "\\.", " ")
DIT.arm <- unique(DIT.arm)
head(DIT.arm)
DIT.arm$FuenteDatos <- "KC"

DIT.arm <- unique(DIT.arm[,c("Familia","genero","esp","DIT","FuenteDatos")])
head(DIT.arm)

europa.dit <- europa[c("Family","Genus","Genus_and_species","ITD_.inter.tegular_distance.","FuenteDatos")]
europa.dit <- unique(europa.dit)
europa.dit <- subset(europa.dit, !is.na(ITD_.inter.tegular_distance.))
head(europa.dit)

names(europa.dit)[4] <- "DIT"
names(DIT.arm) <- names(europa.dit)

europa_kc <- rbind(europa.dit, DIT.arm)
europa_kc$DIT <- as.numeric(europa_kc$DIT)
head(europa_kc)

brasil$genero_especie <- paste(brasil$Genus, brasil$Specific.epithet)
brasil$FuenteDatos <- "Brasil"
brasil.dit <- unique(brasil[,c("Family","Genus","genero_especie","ITDmeasured","FuenteDatos")])
brasil.dit <- subset(brasil.dit, !is.na(ITDmeasured))
names(brasil.dit) <- names(europa_kc)


europa_kc_brasil <- rbind(europa_kc, brasil.dit)
summary(europa_kc_brasil$Family)
table(europa_kc_brasil$FuenteDatos)

boxplot(DIT~FuenteDatos, europa_kc_brasil)

write.csv(europa_kc_brasil, file = "../02salidas/DITfull_porFuente.csv", row.names = FALSE)

# png("../03figuras/DIT_porfuente.png", res = 150, width = 1040, height = 720)
histogram(~DIT|FuenteDatos, europa_kc_brasil)
# dev.off()

daux <- pivot_wider(data = europa_kc_brasil, id_cols = c("Family","Genus","Genus_and_species"), 
            names_from = FuenteDatos, values_from = DIT, values_fn = {mean})
daux$delta.1 <- daux$KC- as.numeric(daux$Europa)
daux$delta.2 <- daux$KC- as.numeric(daux$Brasil)


summary(daux$delta.1)
summary(daux$delta.2)

# png("../03figuras/delta_DIT_porfuente.png", res = 150, width = 1040, height = 720)
par(mfrow=c(1,2))
boxplot(daux$delta.1, horizontal = FALSE, ylim=c(-1.5,4), main="KC-EUROPA", las=1)
boxplot(daux$delta.2, horizontal = FALSE, ylim=c(-1.5,4), main="KC-BRASIL", las=1)
dev.off()

sum(str_detect(daux$Genus_and_species, "\\b(sp|sp1|sp2|cf|aff)\\b"))

DIT2 <- subset(daux, 
               !str_detect(Genus_and_species, 
                           "\\b(sp|spp|spA|spB|spC|spD|spE|spF|sp1|sp2|sp3|sp4|sp5|sp6|sp7|sp8|sp9|sp10|cf|aff)\\b"))
nrow(DIT2)
head(DIT2)

summary(DIT2$KC)
summary(DIT2$Europa)
summary(DIT2$Brasil)

# Orden de asignacion del DIT
DIT2$DIT.final <- DIT2$Europa
DIT2$DIT.final[is.na(DIT2$DIT.final)] <- DIT2$Brasil[is.na(DIT2$DIT.final)]
DIT2$DIT.final[is.na(DIT2$DIT.final)] <- DIT2$KC[is.na(DIT2$DIT.final)]

summary(DIT2$DIT.final)

hist(unique(DIT2[,c("Genus_and_species","DIT.final")]$DIT.final))

DIT2 <- subset(DIT2, !is.na(DIT.final) )
DIT2 <- unique(DIT2)
dim(unique(DIT2[,c("Genus_and_species","DIT.final")]))


ml.dit.arm <- merge(ml.arm, DIT2, by.x="sp.armonizado", by.y="Genus_and_species", all.x=TRUE)
ml.dit.arm.dit <- subset(ml.dit.arm, !is.na(DIT.final))


DIT.baseDatos <- unique(ml.dit.arm.dit[,c("Family","Genus","sp.armonizado","DIT.final")])

hist(unique(DIT.baseDatos[,c("sp.armonizado","DIT.final")])$DIT.final, 
     main="Histograma de DIT por especie",
     xlab="DIT (mm)",
     ylab="Frecuencia", breaks = seq(from=0, to=9, by=0.5),
     las=1
     )

head(DIT.baseDatos)

write.csv(DIT.baseDatos, "DIT.baseDatos.csv", row.names = FALSE)
