#install.packages("Matrix", dependencies = TRUE)
#install.packages("cholmod_factor_ldetA", dependencies = TRUE)
#install.packages("lme4", dependencies = TRUE)


library(patchwork)
library(ggplot2)
library(tidyverse)     ## 
library(colorspace)    ## 
library(rcartocolor)   ## 
library(ggforce)       ## 
library(ggdist)        ## 
library(ggridges)      ## 
library(ggbeeswarm)    ## 
library(gghalves)      ## 
library(ggpubr)
library(systemfonts)   ## 
library(effects)
library(jtools)
library(sjPlot)
library(dotwhisker)
library(dplyr)
library(performance)
library(lme4)
library(lsmeans)
library(DHARMa)
library(ggeffects)
library(car)
library(sjmisc)
library(merTools)
library(glmmTMB)
library(MASS)
library(RColorBrewer)
library(lmerTest)
library(stargazer)
library(broom)
library(modelsummary)
library (arm)
library (rstanarm)
library(crplots)
library(ggpredict)
library(emmeans)
library(readr)
library(glmmTMB)

# Leer la base
datos <- read.csv("abundancia.csv")
names(datos)
view(datos)
# Convertir a factores y definir referencias correctas
datos <- datos %>%
  mutate(
    especie = factor(especie),
    talla = factor(talla, levels = c(1, 2, 3), ordered = FALSE),
    profundidad = factor(profundidad, levels = c("Shallow", "Intermediate", "Deep")),
    transecto = factor(transecto)
  )

# Verificar combinaciones sin presencia
datos %>%
  group_by(especie, talla, profundidad) %>%
  summarise(n = n(), suma = sum(abundancia)) %>%
  arrange(suma)

# Histograma general por especie
ggplot(datos, aes(x = abundancia)) +
  geom_histogram(binwidth = 1, fill = "gray70", color = "black") 

# Proporción de ceros
prop_ceros <- mean(datos$abundancia == 0)
prop_ceros ### 0.69 el 70% de los datos son ceros, apto para un modelo zero-inf

# Establecer referencias: talla3 y S. acapulcoensis
datos$talla <- relevel(datos$talla, ref = "3")
datos$especie <- relevel(datos$especie, ref = "S. acapulcoensis")

############## Modelo con inflación de ceros ###############}

modelo_zinf2 <- glmmTMB(
  abundancia ~ especie * (talla + profundidad) + (1 | transecto), #predice la abundancia esperada
  ziformula = ~ especie * (talla + profundidad), #modela qué variables influyen en la probabilidad de que ocurra un cero estructural
  family = nbinom2, ## esta familia es casi igual a de Possion, pero permite más varianza
  ## la varianza aumenta más rapido que la media. 
  data = datos
)

summary(modelo_zinf2)

Anova(modelo_zinf2)

simul2 <- simulateResiduals(modelo_zinf2) 
plot(simul2) ### buen modelo!! 

emmeans(modelo_zinf2, pairwise ~ especie | profundidad, type = "response") ## estima diferencias por profundidad

##   S. acapulcoensis muestra en todas las profundidades mayores abundancias esperadas que S. flavialatus.
##  diferencia más clara: en profundidad "Deep", S. acapulcoensis es claramente más abundante.
## En "Shallow" e "Intermediate", aunque hay diferencia, los intervalos de confianza se superponen casi totalmente (alta incertidumbre).
## En Deep: La abundancia de S. acapul es 6.7 veces mayor que la de S. flav, y es estadísticamente significativa (p = 0.02)!!

emmeans(modelo_zinf2, pairwise ~ especie | talla, type = "response")# abundancia esperada para cada especie, condicionada por talla.
## solo diferencias entre las tallas 3 (<.0OO1)
##  S. acapulcoensis tiene una abundancia esperada mucho mayor que S. flavilatus para talla 3.
## la abundancia esperada de S. acapulcoensis es 11.05 veces mayor que la de S. flavialatus.


###   Garfiquemos
#### diferencias entre especies por profundidad

em <- emmeans(modelo_zinf2, ~ especie | profundidad, type = "response")
em_df <- as.data.frame(em)

ggplot(em_df, aes(x = especie, y = response, color = especie)) +
  geom_point(size = 4, aes(fill = especie), shape = 21) + 
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0, size = 1) +
  facet_wrap(~ profundidad) +
  scale_color_manual(values = c("S. acapulcoensis" = "#C16540", "S. flavilatus" = "#00C1C8")) +
  scale_fill_manual(values = c("S. acapulcoensis" = "#C16540", "S. flavilatus" = "#00C1C8")) +
  labs(
    x = "Especie",
    y = "Abundancia esperada (± IC 95%)",
    title = "Abundancia estimada por especie en cada profundidad"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

#### diferencias entre especies talla/ profundidad
# Predicciones con corrección de sesgo para talla y profundidad
pred <- ggpredict(
  modelo_zinf2,
  terms = c("talla", "especie", "profundidad"),
  bias_correction = TRUE
)

###########################

pred$predicted <- pred$predicted 
pred$conf.low <- pred$conf.low 
pred$conf.high <- pred$conf.high 

# Asegurar orden correcto de tallas
pred$x <- factor(pred$x, levels = c("1", "2", "3"))

# Ajustar nombres de especie en la leyenda en itálicas
levels(pred$group) <- c("italic(S.~acapulcoensis)", "italic(S.~flavilatus)")

# se cambió a puntos

abundancia <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(fill = group), position = position_dodge(width = 0.5), 
             size = 4, shape = 21, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group),
                position = position_dodge(width = 0.5), linewidth = 1,
                width = 0) +
  facet_wrap(~facet, nrow = 1) +
  labs(
    x = "Size class",
    y = "Estimated abundance (ind/80m²)",
    fill = NULL,
    color = NULL
  ) +
  scale_fill_manual(
    values = c("#C16540", "#00C1C8"),
    labels = scales::label_parse()(levels(pred$group))
  ) +
  scale_color_manual(
    values = c("#C16540", "#00C1C8"),
    labels = scales::label_parse()(levels(pred$group))
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    legend.position = "top",
    text = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 11, color = "black")
  )

print(abundancia)

# Guardar figura
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 1.- corregido/figuras/abundancia.tiff",
  plot = abundancia,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 6, height = 6, units = "in"
)
