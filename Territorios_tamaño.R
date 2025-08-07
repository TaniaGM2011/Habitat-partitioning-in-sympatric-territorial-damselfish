# Instalar paquetes si no los tienes
# install.packages(c("tidyverse", "car", "vegan", "ggpubr"))

library(tidyverse)
library(car)
library(vegan)
library(ggpubr)

# === 1. Cargar datos ===
# Pega tus datos en un archivo CSV (por ejemplo, "territorio.csv") con columnas:
# Especie, profundidad, tamanio

datos <- read.csv("territorio.csv")  # usa sep="," si es CSV con coma
colnames(datos) <- c("Especie", "Profundidad", "Area")  # aseguramos nombres uniformes
datos$Especie <- as.factor(datos$Especie)
datos$Profundidad <- as.factor(datos$Profundidad)

#######################
# Instalar si es necesario
# install.packages(c("dplyr", "ggplot2", "emmeans", "car"))

library(dplyr)
library(ggplot2)
library(car)
library(emmeans)


# Renombrar columnas si es necesario
colnames(datos) <- c("Especie", "Profundidad", "Area")

# Revisar estructura
str(datos)

# Asegurar que Especie y Profundidad sean factores
datos$Especie <- as.factor(datos$Especie)
datos$Profundidad <- as.factor(datos$Profundidad)

# Modelo GLM con distribución Gamma y enlace log
modelo_glm <- glm(Area ~ Especie * Profundidad, 
                  data = datos, 
                  family = Gamma(link = "log"))

# Resumen del modelo
summary(modelo_glm)

# ANOVA tipo II para evaluar efectos principales e interacción
Anova(modelo_glm)

# Calcular los emmeans
emmeans_glm <- emmeans(modelo_glm, ~ Especie * Profundidad)

# Comparaciones por pares por profundidad
contrast(emmeans_glm, method = "pairwise", by = "Profundidad", adjust = "tukey")

# Comparaciones por pares por especie
contrast(emmeans_glm, method = "pairwise", by = "Especie", adjust = "tukey")

# Comparaciones entre todas las combinaciones especie*profundidad
#pairs(emmeans_glm, adjust = "tukey")

# Visualización
plot(emmeans_glm)

library(DHARMa)
sim <- simulateResiduals(fittedModel = modelo_glm)
plot(sim)
#########################################
df_emmeans <- as.data.frame(emmeans(emmeans_glm, ~ Especie * Profundidad, type = "response"))

# Etiquetas en itálicas usando expression()
etiquetas_italicas <- c("S. acapulcoensis" = expression(italic("S. acapulcoensis")),
                        "S. flavilatus" = expression(italic("S. flavilatus")))

# Asegurar orden en profundidad
df_emmeans$Profundidad <- factor(df_emmeans$Profundidad,
                                 levels = c("Shallow", "Intermediate", "Deep"))


# Gráfico
ggplot(df_emmeans, aes(x = Profundidad, y = response, group = Especie)) +
  geom_point(aes(fill = Especie), shape = 21, size = 4, 
             position = position_dodge(0.5), color = "black") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, color = Especie), 
                position = position_dodge(0.5), width = 0.2, linewidth = 1) +
  scale_fill_manual(
    values = c("S. acapulcoensis" = "#C16540", "S. flavilatus" = "#00C1C8"),
    labels = etiquetas_italicas
  ) +
  scale_color_manual(
    values = c("S. acapulcoensis" = "#C16540", "S. flavilatus" = "#00C1C8"),
    labels = etiquetas_italicas
  ) +
  labs(x = "Depth",
       y = "Estimated area (m²)",
       fill = NULL,
       color = NULL) +
  scale_y_continuous(limits = c(0, 17), expand = expansion(mult = c(0, 0.05))) +
  theme_classic(base_family = "Arial", base_size = 11) +
  theme(
    text = element_text(size = 11, family = "Arial", color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 11, color = "black", family = "Arial"),
    legend.position = "top"
  )

ggsave("F:/Doctorado/TESIS version 3/capitulo 1.- corregido/figuras/territorio.tiff",
    dpi = 500,
   width = 6,
 height = 6,
units = "in",
compression = "lzw")





