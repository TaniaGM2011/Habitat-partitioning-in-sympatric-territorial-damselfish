
library(glmmTMB)
library(DHARMa)
library(dplyr)
library(ggplot2)
# Leer y preparar datos
datos <- read.csv("inter.csv")
view(datos)
names(datos)

datos <- datos %>%
  mutate(
    especie = factor(especie),
    interaccion = factor(interaccion),
    profundidad = factor(profundidad, levels = c("Shallow", "Intermediate", "Deep")),
    territorio = factor(territorio)
  )

summary_stats <- datos %>%
  summarise(across(
    c(frecuencia),
    list(mean = mean, sd = sd, min = min, max = max, median = median),
    na.rm = TRUE
  ))

print(summary_stats) ###

################ 
tabla_resumen <- datos %>%
  group_by(profundidad) %>%
  summarise(
    N = n(),
    Mean = round(mean(frecuencia, na.rm = TRUE), 2),
    Median = round(median(frecuencia, na.rm = TRUE), 2),
    Min = round(min(frecuencia, na.rm = TRUE), 2),
    Max = round(max(frecuencia, na.rm = TRUE), 2),
    SD = round(sd(frecuencia, na.rm = TRUE), 2))

tabla_resumen %>%
  kbl(caption = "frecuencia por especie, z, etc") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# 1. Verificar combinaciones sin presencia (frecuencia = 0)
combinaciones <- datos %>%
  group_by(especie, interaccion, profundidad) %>%
  summarise(suma_frecuencia = sum(frecuencia), .groups = "drop") %>%
  filter(suma_frecuencia == 0)

print(combinaciones)

# 2. Histograma de frecuencia por especie
ggplot(datos, aes(x = frecuencia)) +
  geom_histogram(binwidth = 1, fill = "gray70", color = "black") +
  #facet_wrap(~especie, scales = "free_y") +
  labs(x = "Frecuencia de interacción (int/h)", y = "Número de observaciones") +
  theme_classic(base_size = 11)


# 3. Calcular proporción de ceros
proporcion_ceros <- mean(datos$frecuencia == 0)
proporcion_ceros ### casi el 40% de los datos son ceros

##### MODELO ZINF
modelo_zinf_mejorado <- glmmTMB(
  frecuencia ~ especie + interaccion + profundidad +
    especie:interaccion + especie:profundidad + interaccion:profundidad + (1 | territorio),
  ziformula = ~ interaccion + especie + profundidad,
  family = nbinom2,
  data = datos
)

summary(modelo_zinf_mejorado) ##aunque la proporción de ceros no es tan alta como en abudancia, 
## y el summary muestra que la parte de Zero_in no es significativa, pero tamben que no hay sobreajuste
## es correcto emplear el modelo así. 

Anova(modelo_zinf_mejorado)

# Validación
simul_mejorado <- simulateResiduals(modelo_zinf_mejorado)
plot(simul_mejorado)

#install.packages("emmeans")  # si no lo tienes
library(emmeans)

# Medias estimadas para todas las combinaciones

emmeans(modelo_zinf_mejorado, pairwise ~ interaccion | especie * profundidad, type = "response")

emmeans(modelo_zinf_mejorado, pairwise ~ interaccion | especie, type = "response")

emmeans(modelo_zinf_mejorado, pairwise ~ interaccion | profundidad, type = "response")

#### graficar

## usando los datos del emmans

library(ggplot2)

# Datos
heat_data <- data.frame(
  especie = rep(c("S.acapulcoensis", "S.flavilatus"), each = 6),
  profundidad = rep(c("Shallow", "Intermediate", "Deep"), times = 2),
  interaccion = rep(c("Congener", "Conspecific"), times = 3),
  response = c(2.6, 11.8, 5.9, 11.9, 3.4, 9.0, 3.3, 5.4, 8.1, 6.5, 3.6, 5.4)
)


# Asegurar que los niveles estén en orden de arriba hacia abajo
heat_data$profundidad <- factor(heat_data$profundidad, levels = c("Deep", "Intermediate", "Shallow"))

# Asignar etiquetas en itálicas para las especies
heat_data$especie <- factor(heat_data$especie,
                            levels = c("S.acapulcoensis", "S.flavilatus"),
                            labels = c("italic(S.~acapulcoensis)", "italic(S.~flavilatus)"))

# Gráfico
interacciones <-ggplot(heat_data, aes(x = interaccion, y = profundidad, fill = response)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(response, 2)), color = "black", size = 4, family = "Arial") +
  facet_wrap(~especie, labeller = label_parsed) +
  scale_fill_gradient(low = "white", high = "#36648B", name = "Int/h") +
  theme_minimal(base_size = 11, base_family = "Arial") +
  labs(x = "Type of individuals", y = "Depth") +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 11),
    axis.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 11, color = "black"),
    legend.title = element_text(size = 11, color = "black"),
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  )
print(interacciones)
# Guardar figura
ggsave(
 filename = "F:/Doctorado/TESIS version 3/capitulo 1.- corregido/figuras/interacciones.tiff",
plot = interacciones,
device = "tiff",
dpi = 500,
compression = "lzw",
width = 6, height = 6, units = "in"
)



