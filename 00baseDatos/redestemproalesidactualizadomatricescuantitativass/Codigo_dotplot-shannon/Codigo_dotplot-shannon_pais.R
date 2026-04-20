library(ggplot2)

# Datos
pais <- c("Republic of Mauritius", "Seychelles", "Argentina", "USA", "Republic of Mauritius", "Republic of Mauritius",
          "South Africa", "Australia", "Japon", "USA", "USA", "Canada", "Japon", "Argentina", "Japon", "Japon",
          "Japon", "Japon", "Spain", "Brazil", "Republic of Mauritius", "Republic of Mauritius", "Brazil", "Argentina", "Argentina")

shannon <- c(6.11, 4.78, 6.76, 4.28, 2.99, 3.26, 3.37, 4.84, 7.30, 3.23, 3.27, 4.30, 6.57, 3.20,
             5.86, 5.60, 6.02, 6.89, 5.09, 3.97, 4.95, 5.03, 4.24, 4.98, 5.01)

# Crear data frame
datos <- data.frame(Pais = pais, Shannon = shannon)

# Asegurarse de que Shannon sea numérico
datos$Shannon <- as.numeric(datos$Shannon)

# Graficar
ggplot(datos, aes(x = reorder(Pais, -Shannon), y = Shannon, fill = Pais)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Índice de diversidad de Shannon por país",
       x = "País",
       y = "Índice de Shannon (H')") +
  scale_fill_viridis_d() +
  scale_y_continuous(limits = c(0, 8))   # ← aquí limitamos el eje Y a algo lógico

##dotplot
ggplot(datos, aes(x = reorder(Pais, Shannon), y = Shannon, color = Pais)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = Pais, y = 0, yend = Shannon)) +
  coord_flip() +
  theme_minimal()


library(ggplot2)

# Datos
pais <- c("Republic of Mauritius", "Seychelles", "Argentina", "USA", "Republic of Mauritius", "Republic of Mauritius",
          "South Africa", "Australia", "Japon", "USA", "USA", "Canada", "Japon", "Argentina", "Japon", "Japon",
          "Japon", "Japon", "Spain", "Brazil", "Republic of Mauritius", "Republic of Mauritius", "Brazil", "Argentina", "Argentina")

shannon <- c(6.11, 4.78, 6.76, 4.28, 2.99, 3.26, 3.37, 4.84, 7.30, 3.23, 3.27, 4.30, 6.57, 3.20,
             5.86, 5.60, 6.02, 6.89, 5.09, 3.97, 4.95, 5.03, 4.24, 4.98, 5.01)

# Crear data frame
datos <- data.frame(Pais = pais, Shannon = shannon)

# Gráfico tipo dotplot con ejes ajustados
ggplot(datos, aes(x = Shannon, y = reorder(Pais, Shannon), color = Pais)) +
  geom_point(size = 4) +
  geom_segment(aes(x = 0, xend = Shannon, y = Pais, yend = Pais), color = "grey") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, max(datos$Shannon) + 1)) +  # Ajusta el eje X
  labs(title = "Indice de Shannon por país",
       x = "Índice de Shannon (H')",
       y = "País") +
  theme(legend.position = "none")

