library(rgbif)
library(dplyr)
library("taxize")

ml <- base_datos$matriz_comp_larga
head(ml$ID)
ml.i <- subset(ml, ID=="M_018")
ml.i <- unique(ml.i[,c("ID","especie")])

# Simulamos tu base de datos original
bd_abejas <- data.frame(
  id = 1:nrow(ml.i),
  nombre_original = ml.i$especie
)
bd_abejas

# 1. Extraer nombres únicos para la consulta
nombres_unicos <- unique(bd_abejas$nombre_original)


ids.gbif <- get_gbifid_(nombres_unicos); beepr::beep(sound=8)
ids.gbif

# 2. Consultar GBIF usando la lista de nombres únicos
# Nota que el argumento se llama 'name_data' en lugar de 'name'
gbif_matches <- name_backbone_checklist(name_data = nombres_unicos, strict = TRUE, verbose = TRUE)



# Consultar nombre por nombre y guardar en una lista
lista_resultados <- lapply(nombres_unicos, function(nombre) {
  name_backbone(name = nombre)
})

# Combinar la lista en un solo data.frame usando dplyr
gbif_matches <- bind_rows(lista_resultados)

