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

# Cargar base de datos
base_datos <- readRDS("../00baseDatos/base_datos.RDS")

# tabla de interacciones en formato largo 
ml <- base_datos$matriz_comp_larga
head(ml)

#tabla de especies de polinizadores
especie <- base_datos$especies

# merge entre tabla de interacciones y de especies
ml.esp <- merge(ml, especie, all.x=TRUE)

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

# guardar tabla resultante en una lista de salida 

matrices.abejas <- list(ml.esp.abe)

# creacion de matrices en formato ancho de abejas

for(m.i in unique(ml.esp.abe$ID)){
  if(m.i == "M_076"){next }
  m.ancho.i <- ml.esp.abe[,c("especie","ID","plantas","interaccion")] |> 
    filter(ID == m.i) |> droplevels()
  
  m.ancho.abe <- pivot_wider(m.ancho.i, 
                             id_cols = c(ID, plantas),
                             names_from = especie,
                             values_from = interaccion, values_fn = unique)
  m.ancho.abe$ID <- NULL
  write.csv(m.ancho.abe, file = file.path("../02salidas",
                                          paste0("matriz_ancha_abejas.",m.i,".csv")), row.names = FALSE)
  matrices.abejas[[paste0("matriz_ancha_abejas_",m.i)]] <- m.ancho.abe

}


saveRDS(matrices.abejas, file = file.path("../02salidas","matrices_abejas.RDS"))

writexl::write_xlsx(matrices.abejas, path = file.path("../02salidas","matrices_abejas.xlsx"))



