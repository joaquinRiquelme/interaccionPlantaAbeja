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

# Usamos kingdom = "Plantae" para evitar confusiones con animales homónimos
resultados <- name_backbone_checklist(
  prueba1$Especie.genero, 
  genus  = "Genero",
  kingdom = "Plantae", 
  strict = TRUE, 
  verbose = TRUE
)

# 3. Limpiar el resultado
# La función devuelve un tibble con muchas columnas de GBIF
df_final <- resultados |>
  select(kingdom, order,  family, genus, matchType, status, verbatim_name)

genero.aceptado <- unique(
  df_final[df_final$kingdom=="Plantae",# & is.element(df_final$status, c("ACCEPTED","SYNONIMUS")),
           c("family","genus", "verbatim_name")])
head(genero.aceptado)
unique(genero.aceptado$genus)
setdiff(unique(genero.aceptado$genus), unique(plantas$Genero))
setdiff(unique(plantas$Genero),unique(genero.aceptado$genus))
intersect(unique(plantas$Genero),unique(genero.aceptado$genus))
intersect(unique(plantas$Genero),unique(genero.aceptado$verbatim_name))

unique(plantas$Genero)

aaa <- merge(plantas, genero.aceptado, by.x="Genero", by.y="genus", all.x = TRUE)

# print(df_final)