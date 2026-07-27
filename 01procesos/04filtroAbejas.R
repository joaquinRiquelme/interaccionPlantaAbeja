# Filtro de matrices bipartitas para seleccionar solo las 
# especies de abejas correspondientes a las familias
# - Andrenidae
# - Apidae
# - Colletidae
# - Halictidae
# - Megachilidae
# - Melittidae
# - Strenitidae

# Cargar librerias
library(tidyverse)
library(writexl)
library(taxize)
library(beepr)

# Cargar base de datos
# base_datos <- readRDS("../00baseDatos/base_datos.RDS")

# tabla de interacciones en formato largo 
# ml <- base_datos$matriz_comp_larga
# ml <- read.csv(file = "ml.armonizado.plantas.abejas.csv")
# head(ml)

matriz_comp_larga <- read.csv("../02salidas/ml.arm.plantas.abejas.csv")
head(matriz_comp_larga)

matriz_comp_larga$genero <- substr(matriz_comp_larga$sp.armonizado, start = 1, stop = regexpr(pattern = " ", text = matriz_comp_larga$sp.armonizado, fixed = TRUE)-1)

especies <- unique(matriz_comp_larga[c("genero","sp.armonizado")])

head(especies)

# especies$especie = substr(especies$Abeja,start = 1, stop = nchar(as.character(especies$Abeja))-5)
sort(unique(especies$sp.armonizado))

# data.frame(aaa$`Apis mellifera`) |> pivot_wider()

vector.generos <- sort(unique(especies$genero))
vector.generos <- vector.generos[vector.generos!=""]

# vector.generos1 <- vector.generos[1:20]
# vector.generos500 <- vector.generos[1:500]
# vector.generos1000 <- vector.generos[501:1000]
# vector.generos1500 <- vector.generos[1001:1500]
# vector.generos2000 <- vector.generos[1501:2000]
# vector.generos2347 <- vector.generos[2001:2347]


ids.gbif <- get_gbifid_(vector.generos); beepr::beep(sound=8)
# ids.gbif500 <- get_gbifid_(vector.generos500); beepr::beep(sound=8)
# ids.gbif1000 <- get_gbifid_(vector.generos1000); beepr::beep(sound=8)
# ids.gbif1500 <- get_gbifid_(vector.generos1500); beepr::beep(sound=8)
# ids.gbif2000 <- get_gbifid_(vector.generos2000); beepr::beep(sound=8)
# ids.gbif2347 <- get_gbifid_(vector.generos2347); beepr::beep(sound=8)

# ids.gbif.todas <- list(ids.gbif500, ids.gbif1000, ids.gbif1500, ids.gbif2000, ids.gbif2347)


# registro sin familia, se elimina por ahora
# ids.gbif <- ids.gbif[-1574]
# ids.gbif
list.generos <- lapply(ids.gbif, function(x){
  x <- data.frame(x)
  if(nrow(x)>1){
    x2 <- x[ x$kingdom=="Animalia" &
               x$phylum=="Arthropoda" &
               # x$status =="ACCEPTED" &
               !is.na(x$genus) & !is.na(x$family) & 
               !is.na(x$order)& x$rank != "kingdom",]
    x2 <- x2[1,]
    
    if(is.element("order", names(x2)) &
       is.element("family", names(x2)) & 
       is.element("genus", names(x2))){
      
      x3 <- unique(x2[,c("order","family","genus")])
      x3
    }
    
  }
  
  #names(x)[1])
  
  
})


tabla.generos <- do.call(rbind, list.generos)

table(tabla.generos$order)
barplot(table(tabla.generos$order))

barplot(table(tabla.generos$family))
table(tabla.generos$family)

table(tabla.generos$genus)
tabla.generos <- unique(tabla.generos)

esp2 <- merge(especies, tabla.generos, by.x="genero", by.y="genus", all.x=TRUE)
table(esp2$order)
# table(esp2$family)
subset(esp2, is.na(family))

esp3 <- unique(esp2)
sum(is.na(esp3$family))
names(esp3) <- c("genero","especie","orden","familia")

# base_datos$especies <- rbind(base_datos$especies, esp3)

write.csv(esp3, "../02salidas/especies3.csv", row.names = FALSE)


#tabla de especies de polinizadores
especie <- esp3

# merge entre tabla de interacciones y de especies
ml.esp <- merge(matriz_comp_larga, especie, by.x="esp", by.y="especie", all.x=TRUE)

table(ml.esp$familia)

# filtro de especies de las familias listadas
ml.esp.abe <- ml.esp |> 
  filter(familia %in% c("Andrenidae",
                        "Apidae",
                        "Colletidae",
                        "Halictidae",
                        "Megachilidae",
                        "Melittidae",
                        "Strenitidae"))

head(ml.esp.abe)
unique(ml.esp.abe$especie)

ml.esp.abe <- unique(ml.esp.abe)

write.csv(ml.esp.abe, file.path(dir.salidas,"ml.abejas.csv"),row.names = FALSE)
# guardar tabla resultante en una lista de salida 

# matrices.abejas <- list(ml.esp.abe)

# creacion de matrices en formato ancho de abejas
# 
# for(m.i in unique(ml.esp.abe$ID)){
#   if(m.i == "M_076"){next }
#   m.ancho.i <- ml.esp.abe[,c("especie","ID","plantas","interaccion")] |> 
#     filter(ID == m.i) |> droplevels()
#   
#   m.ancho.abe <- pivot_wider(m.ancho.i, 
#                              id_cols = c(ID, plantas),
#                              names_from = especie,
#                              values_from = interaccion, values_fn = unique)
#   m.ancho.abe$ID <- NULL
#   write.csv(m.ancho.abe, file = file.path("../02salidas",
#                                           paste0("matriz_ancha_abejas.",m.i,".csv")), row.names = FALSE)
#   matrices.abejas[[paste0("matriz_ancha_abejas_",m.i)]] <- m.ancho.abe
# 
# }
# 
# 
# saveRDS(matrices.abejas, file = file.path("../02salidas","matrices_abejas.RDS"))
# 
# writexl::write_xlsx(matrices.abejas, path = file.path("../02salidas","matrices_abejas.xlsx"))
# 
# 
# 
