library(rgbif)
library(dplyr)

# 1. Tu lista de géneros (incluyendo algunos que han cambiado de familia o son sinónimos)
generos <- c("Quercus", "Zea", "Monstera", "Althea") 

# 2. Consulta a GBIF fijando el reino vegetal
resultados12 <- name_backbone_checklist(generos, kingdom = "Plantae")

# 3. Filtrar para obtener la familia aceptada más reciente
familias_aceptadas <- resultados12 %>%
  # CASO 1: Si el nombre ya está ACEPTADO, nos quedamos con su familia.
  # CASO 2: Si es un SINÓNIMO (SYNONYM), GBIF a veces cambia la familia en 'acceptedUsageKey'
  # Para asegurar la máxima actualización, filtramos estados válidos:
  # filter(status %in% c("ACCEPTED", "SYNONYM")) %>%
  
  # Seleccionamos las columnas clave
  select(verbatim_name, genus, family, status, matchType, usageKey) %>%
  
  # Limpieza: Si un género no se encontró (matchType = NONE), la familia será NA
  filter(!is.na(family)) %>%
  
  # Si un género se repite por error, nos quedamos con el registro único
  distinct(verbatim_name, .keep_all = TRUE)

print(familias_aceptadas)
