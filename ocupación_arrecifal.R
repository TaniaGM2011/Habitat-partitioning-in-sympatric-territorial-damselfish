# === Cargar paquetes necesarios ===
library(dplyr)
library(ggplot2)
library(emmeans)
library(car)
library(DHARMa)

# === 1. Cargar y preparar datos ===
datos <- read.csv("ocu.csv")
colnames(datos) <- c("Especie", "Profundidad", "Ocu")

# Filtrar valores positivos y convertir a factores ordenados
datos_pos <- datos %>%
  filter(Ocu > 0) %>%
  mutate(
    Especie = factor(Especie),
    Profundidad = factor(Profundidad, levels = c("Somero", "Intermedio", "Profundo"))
  )

# === 2. Ajustar modelo GLM con distribución Gamma ===
modelo_glm <- glm(Ocu ~ Especie * Profundidad, data = datos_pos, family = Gamma(link = "log"))
summary(modelo_glm)

# ANOVA tipo II
Anova(modelo_glm)

# Diagnóstico de residuos
sim <- simulateResiduals(modelo_glm)
plot(sim)

# Histograma
hist(datos_pos$Ocu, main = "Distribución de área de ocupación", xlab = "Ocu")

# === 3. EMMEANS ===
modelo_emm <- emmeans(modelo_glm, ~ Especie * Profundidad)

# Comparaciones por profundidad
contrast(modelo_emm, method = "pairwise", by = "Profundidad", adjust = "tukey")

# Comparaciones por especie
contrast(modelo_emm, method = "pairwise", by = "Especie", adjust = "tukey")

# === 4. Gráfico de barras por especie/profundidad ===
df_emm <- as.data.frame(summary(modelo_emm, type = "response"))
df_emm$Profundidad <- factor(df_emm$Profundidad, levels = c("Somero", "Intermedio", "Profundo"))

etiquetas_italicas <- c("S. acapulcoensis" = expression(italic("Stegastes acapulcoensis")),
                        "S. flavilatus" = expression(italic("Stegastes flavilatus")))

ocupacion_predicha <- ggplot(df_emm, aes(x = Profundidad, y = response, fill = Especie)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = response, ymax = upper.CL),
                position = position_dodge(0.8), width = 0.25) +
  scale_fill_manual(values = c("S. acapulcoensis" = "#C16540",
                               "S. flavilatus" = "#00C1C8"),
                    labels = etiquetas_italicas) +
  labs(x = "Zona arrecifal", y = "Ocupación arrecifal estimada (m²)", fill = NULL) +
  theme_classic(base_family = "Arial") +
  theme(axis.text = element_text(color = "black", size = 11),
        axis.title = element_text(color = "black", size = 11),
        legend.text = element_text(size = 11),
        legend.position = "top")

print(ocupacion_predicha)

# === 5. Cálculo de área total y área libre ===
# --- Constantes declaradas arriba ---
area_por_profundidad <- 960   # m² totales por zona
factor_escala <- 12           # m² multiplicador (ajústalo a tu diseño)

pred_df <- df_emm |>
  dplyr::mutate(area_ocupada = response * factor_escala) |>
  dplyr::select(Especie, Profundidad, area_ocupada)


ocupacion_total <- pred_df |>
  dplyr::group_by(Profundidad) |>
  dplyr::summarise(area_ocupada_total = sum(area_ocupada), .groups = "drop") |>
  dplyr::mutate(
    Especie = "Área libre de damiselas",
    area_ocupada = pmax(area_por_profundidad - area_ocupada_total, 0)
  ) |>
  dplyr::select(Profundidad, Especie, area_ocupada)


df_final <- bind_rows(pred_df, ocupacion_total) |>
  mutate(Especie = factor(Especie,
                          levels = c("S. acapulcoensis", "S. flavilatus", "Área libre de damiselas")))

# === 6. Gráfico de barras apiladas con área libre ===
df_final$Profundidad <- factor(df_final$Profundidad, levels = c("Somero","Intermedio","Profundo"))

grafico_ocupacion <- ggplot(df_final, aes(x = Profundidad, y = area_ocupada, fill = Especie)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(
    values = c("S. acapulcoensis" = "#C16540",
               "S. flavilatus"    = "#00C1C8",
               "Área libre de damiselas" = "#CDBA96"),
    labels = c(expression(italic("Stegastes acapulcoensis")),
               expression(italic("Stegastes flavilatus")),
               "Área libre de damiselas")
  ) +
  labs(x = "Zona arrecifal", y = "Área arrecifal total  (m²)", fill = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.text  = element_text(color = "black", size = 11),
    axis.title = element_text(color = "black", size = 11),
    legend.text = element_text(size = 11),
    legend.position = "top"
  )

print(grafico_ocupacion)

# Guardar figura
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 1.- corregido/figuras/grafico_ocupacion.tiff",
  plot = grafico_ocupacion,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 8, height = 8, units = "in"
)

# === 7. Mostrar tabla de áreas ===
# Crear tabla de resumen con porcentajes
df_final %>%
  group_by(Especie) %>%
  summarise(area_total = sum(area_ocupada)) %>%
  mutate(porcentaje = round((area_total / 2880) * 100, 1)) %>%
  print()

# Calcular porcentaje por especie dentro de cada profundidad (base 960 m²)
df_final %>%
  mutate(area_ocupada = as.numeric(area_ocupada)) %>%
  group_by(Profundidad) %>%
  mutate(
    porcentaje = round((area_ocupada / 960) * 100, 1)
  ) %>%
  arrange(Profundidad, Especie) %>%
  print()





