
############
# Cargar paquetes
library(dplyr)
library(tidyr)
library(proxy)
library(ggplot2)

# Leer datos
datos <- read.csv("Jaccard.csv")  # Asegúrate que este es tu archivo

# Definir columnas por especie
aca_cols <- c("S.acapulcoensis_size1", "S.acapulcoensis_size2", "S.acapulcoensis_size3")
fla_cols <- c("S.flavilatus_size1", "S.flavilatus_size2", "S.flavilatus_size3")
cols_comparar <- c(aca_cols, fla_cols)

# Inicializar resultados
resultados_heatmap <- data.frame()

# Loop por profundidad
for (zona in unique(datos$profundidad)) {
  datos_zona <- datos %>% filter(profundidad == zona)
  
  # Matriz binaria
  matriz <- datos_zona[, cols_comparar] %>%
    mutate_all(~ifelse(. > 0, 1, 0))
  
  matriz_t <- t(as.matrix(matriz))
  jaccard_dist <- proxy::dist(matriz_t, method = "Jaccard")
  jaccard_sim <- 1 - as.matrix(jaccard_dist)
  
  df <- as.data.frame(jaccard_sim)
  df$Talla_A <- rownames(df)
  df_largo <- pivot_longer(df, cols = -Talla_A, names_to = "Talla_B", values_to = "jaccard")
  df_largo$profundidad <- zona
  
  # Filtrar solo cruces con Talla_A = acapulcoensis y Talla_B = flavilatus
  df_filtrado <- df_largo %>%
    filter(Talla_A %in% aca_cols & Talla_B %in% fla_cols)
  
  resultados_heatmap <- bind_rows(resultados_heatmap, df_filtrado)
}

# Asegurar orden
resultados_heatmap$Talla_A <- factor(resultados_heatmap$Talla_A, levels = aca_cols)
resultados_heatmap$Talla_B <- factor(resultados_heatmap$Talla_B, levels = fla_cols)
resultados_heatmap$profundidad <- factor(resultados_heatmap$profundidad,
                                         levels = c("Shallow", "Intermediate", "Deep"))

# Graficar heatmap
Jaccard <-ggplot(resultados_heatmap, aes(x = Talla_B, y = Talla_A, fill = jaccard)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(jaccard, 2)), size = 3.5, color = "black") +
  scale_fill_gradient(low = "white", high = "#F08080", name = "Jaccard\n(similarity)") +
  # Etiquetas personalizadas para eje x (S. flavilatus)
  scale_x_discrete(
    labels = c(
      "S. flavilatus_size3" = "class 3",
      "S. flavilatus_size2" = "class 2",
      "S. flavilatus_size1" = "class 1"
    )
  ) +
  
  # Etiquetas personalizadas para eje y (S. acapulcoensis)
  scale_y_discrete(
    labels = c(
      "S. acapulcoensis_size3" = "class 3",
      "S. acapulcoensis_size2" = "class 2",
      "S. acapulcoensis_size1" = "class 1"
    )
  ) +
  
  facet_wrap(~ profundidad, nrow = 1) +
  theme_minimal(base_family = "Arial") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
    axis.text.y.left = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 11),
    strip.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ) +
  labs(
    x = expression("Size classes of"~italic("S. flavilatus")),
    y = expression("Size classes of"~italic("S. acapulcoensis"))
  )
print(Jaccard)

# Guardar la figura
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 1.- corregido/figuras/Jaccard.tiff",
  plot = Jaccard,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 8, height = 8, units = "in"
)

