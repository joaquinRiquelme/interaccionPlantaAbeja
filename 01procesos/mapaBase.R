library("tidyverse")
library("sf")
library("basemaps")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggplot2")
library("ggspatial")
library("ggmap")

# base_datos <- readRDS("../00baseDatos/base_datos.RDS")
# ubi <- base_datos$ubicacion.geografica
ubi <- read.csv("../00baseDatos/tablas/ubicacion.geografica.csv")
ubi[ubi$ID== "M_060", "Longitud"] <- 57.3
ubi_sf <- st_as_sf(ubi, coords = c("Longitud", "Latitud"), crs="4326")



freq <- data.frame(table(ubi_sf$Localidad))
ubi_sf <- merge(ubi_sf, freq, by.x="Localidad", by.y="Var1")
st_crs(ubi_sf) <- 4326

aaa <- ubi_sf |> st_transform(crs = 3857)

plot(aaa)

#baja datos paises
world <- ne_countries(scale = "medium", returnclass = "sf")|>
  st_transform(crs = 3857)
plot(world$geometry)
st_as_sf(ubi, coords = c("Longitud", "Latitud"))
ubi|>st_transform(crs = 3857)

scalebar

aaa$Freq
mapa <- ggplot(data = world)+
  geom_sf(aes())+  #agrega geometría de objetos
  geom_sf(data=aaa, 
          col='red', 
          size=aaa$Freq+2)+
  # annotation_scale(
  #   data=world,
  #   pad_y = unit(2, "cm"),
  #   style="ticks",
  #   location = "bl",          # Ubicación: "bl" = bottom left (abajo a la izquierda)
  #   width_hint = 0.3          # Ancho relativo de la escala
  # ) +
  coord_sf(crs = 4326, 
           xlim = c(-180, 180),
           ylim = c(-90, 90),
           expand = FALSE
           )+
  scale_y_continuous(breaks = seq(-90,90,30))+
  scale_x_continuous(breaks = seq(-180,180,60))+
  annotation_north_arrow(
    location = "bl",          # Misma ubicación que la escala
    which_north = "true",     # "true" para norte verdadero
    pad_x = unit(2.5, "cm"),  # Espacio para separarla de la escala
    pad_y = unit(3, "cm"),  # Espacio para separarla de la escala
    style = north_arrow_fancy_orienteering # Estilo de la flecha
  ) +
  # scalebar(data = world, location = "bottomright", dist = 4,
             # dist_unit = "km", transform = TRUE,  model = "WGS84")+
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Ubicación de redes de interacción a nivel mundial")+
  theme_bw()
mapa
# +
  # scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
  # geom_point(data = aaa, 
           # aes(x = Longitud, y = Latitud), 
           # color = "red",  # Color de los puntos
           # size = 3) +  
  # scale_x_continuous(breaks=seq(-180,180,30))+
    # breaks = seq(-180, 180, by =30),  # Set breaks every 0.5 units
                     # labels = seq(-180, 180, by = 30)  # Set matching labels
                     # limits = c(0, 6),              # Set limits to 0 to 6
                     # expand = c(0.05, 0)            # Add 5% padding on the bottom
  # )+
  # Para el eje Y (Latitud), crea marcas cada 30 grados de -90 a 90.
  # scale_y_discrete(breaks = seq(-90, 90, 30)) +
  # label_axes = waiver()+
  annotation_scale(
    pad_y = unit(2, "cm"),
    style="ticks",
    location = "bl",          # Ubicación: "bl" = bottom left (abajo a la izquierda)
    width_hint = 0.3          # Ancho relativo de la escala
  ) +
  annotation_north_arrow(
    location = "bl",          # Misma ubicación que la escala
    which_north = "true",     # "true" para norte verdadero
    pad_x = unit(2.5, "cm"),  # Espacio para separarla de la escala
    pad_y = unit(3, "cm"),  # Espacio para separarla de la escala
    style = north_arrow_fancy_orienteering # Estilo de la flecha
  ) +
  # scalebar(data = world, location = "bottomright", dist = 4,
  #            dist_unit = "km", transform = TRUE,  model = "WGS84")+
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Ubicación de redes de interacción a nivel mundial")+
  theme_bw()

mapa

png("mapamundial.png", width = 1280, height = 720, units ="px")
mapa
dev.off()

png("mapamundial800.png", width = 800, height = 600, units ="px")
mapa
dev.off()

