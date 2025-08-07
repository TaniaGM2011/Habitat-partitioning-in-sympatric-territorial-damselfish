
# Cargar librerías
library(vegan)
library(ggplot2)
library(ggrepel)  # Para etiquetas sin sobreposición
library(dplyr)

# Leer datos
ABUN <- read.csv("ABUN.csv", row.names = 1)
SUS <- read.csv("SUS.csv", row.names = 1)

# Transformación de abundancias a Hellinger
fauna <- decostand(ABUN, method = "hellinger")

# Extraer variables categóricas
Profundidad <- SUS$Profundidad
Especie <- SUS$Especie

# Eliminar profundidad y especie del set de predictores
predictores <- SUS[, !colnames(SUS) %in% c("Profundidad", "Especie")]

# RDA
fauna.rda <- rda(fauna ~ ., data = predictores)

# 6. Revisar resultados
summary(fauna.rda)
RsquareAdj(fauna.rda)

# 7. Pruebas de significancia
anova.cca(fauna.rda, permutations = 9999)                     # Significancia global
anova.cca(fauna.rda, permutations = 9999, by = "term")        # Significancia de cada predictor
anova.cca(fauna.rda, permutations = 9999, by = "axis")        # Significancia por eje
vif.cca(fauna.rda)                                     # Colinealidad entre predictores

# Extraer coordenadas
sites_df <- as.data.frame(scores(fauna.rda, display = "sites", scaling = 2))
species_df <- as.data.frame(scores(fauna.rda, display = "species", scaling = 2))
env_df <- as.data.frame(scores(fauna.rda, display = "bp", scaling = 2))  # variables ambientales

# Agregar info a sitios
sites_df$Profundidad <- Profundidad
sites_df$Especie <- Especie

# Asignar color por profundidad
colores <- c("Shallow" = "#CD9B9B", "Intermediate" = "#EEB4B4", "Deep" = "#8B636C")

# Asignar forma por especie
formas <- c("S. acapulcoensis" = 16, "S. flavilatus" = 17)

# Agregar nombre de especie a species_df (para color)
species_df$Taxon <- rownames(species_df)
species_df$Grupo <- ifelse(grepl("S.acapulcoensis", species_df$Taxon), "S. acapulcoensis", 
                           ifelse(grepl("S.flavilatus", species_df$Taxon), "S. flavilatus", "Otro"))

col_taxon <- c("S. acapulcoensis" = "#C16540", "S. flavilatus" = "#00C1C8", "Otro" = "gray30")

# Crear gráfico con límites fijos
rda_plot <- ggplot() +
  # Sitios
  geom_point(data = sites_df, aes(x = RDA1, y = RDA2, color = Profundidad, shape = Especie), size = 3, alpha = 0.9) +
  
  # Flechas de especies
  geom_segment(data = species_df, aes(x = 0, y = 0, xend = RDA1, yend = RDA2, color = Grupo),
               arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.7) +
  geom_text_repel(data = species_df, aes(x = RDA1, y = RDA2, label = Taxon, color = Grupo),
                  size = 3, show.legend = FALSE, max.overlaps = 100) +
  
  # Flechas de variables ambientales
  geom_segment(data = env_df, aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.2, "cm")), color = "black", linewidth = 0.5) +
  geom_text_repel(data = env_df, aes(x = RDA1, y = RDA2, label = rownames(env_df)),
                  size = 3, color = "black", max.overlaps = 100) +
  
  # Líneas grises en el origen (0,0)
  geom_hline(yintercept = 0, color = "gray80", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "gray80", linewidth = 0.5) +
  
  # Escalas fijas
  xlim(-1, 1.2) +
  ylim(-1, 1.2) +
  
  # Estilo y leyendas
  scale_color_manual(values = c(colores, col_taxon)) +
  scale_shape_manual(values = formas) +
  labs(x = "RDA1 (27.84%)", y = "RDA2 (11.55%)", color = NULL, shape = "Especie") +
  theme_classic(base_family = "Arial") +
  theme(
    text = element_text(size = 11),
    legend.position = "top",
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10)
  )


# Guardar como TIFF
tiff("F:/Doctorado/TESIS version 3/capitulo 1.- corregido/figuras/rda_ggplot.tiff",
     width = 18, height = 15, units = "cm", res = 500, compression = "lzw")

print(rda_plot)

dev.off()
