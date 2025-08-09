# --- Paquetes ---
library(tidyverse)
library(vegan)
library(ggplot2)
library(tidytext)
library(stringr)

# --- 1) Leer datos ---
dat <- read.csv("sustrato.csv", check.names = FALSE)

# Asegurar que 'zona' sea factor y en el orden deseado
dat$zona <- factor(dat$zona, levels = c("Shallow", "Intermediate", "Deep"))

# Matriz de variables (porcentajes) y metadatos
X_raw <- dat %>% select(-zona) %>% as.data.frame()
meta  <- dat %>% select(zona)

# --- 2) Pasar a proporciones si vienen en 0–100 ---
if (max(X_raw, na.rm = TRUE) > 1 && max(X_raw, na.rm = TRUE) <= 100) {
  X_raw <- X_raw / 100
}

# Eliminar filas con suma 0 (evita NaN en Hellinger)
rs <- rowSums(X_raw, na.rm = TRUE)
if (any(rs == 0)) {
  warning("Filas con suma 0 eliminadas antes de Hellinger.")
  X_raw <- X_raw[rs > 0, , drop = FALSE]
  meta  <- meta[rs > 0, , drop = FALSE]
}

# --- 3) Transformación de Hellinger ---
X_hell <- decostand(X_raw, method = "hellinger")

# --- 4) PERMANOVA ---
set.seed(123)
permanova <- adonis2(X_hell ~ zona,
                     data         = meta,
                     method       = "euclidean",
                     permutations = 9999,
                     by           = "terms")
print(permanova)

# --- 5) Comparaciones por pares ---
pairwise_adonis2 <- function(X, groups, method = "euclidean",
                             permutations = 9999, p.adjust.m = "BH") {
  stopifnot(nrow(X) == length(groups))
  gr  <- droplevels(as.factor(groups))
  cmb <- combn(levels(gr), 2, simplify = FALSE)
  out <- lapply(cmb, function(cc) {
    idx  <- gr %in% cc
    subX <- X[idx, , drop = FALSE]
    subG <- droplevels(gr[idx])
    mod  <- vegan::adonis2(subX ~ subG, method = method,
                           permutations = permutations)
    data.frame(group1 = cc[1],
               group2 = cc[2],
               F      = mod$F[1],
               R2     = mod$R2[1],
               p      = mod$`Pr(>F)`[1],
               stringsAsFactors = FALSE)
  })
  res <- bind_rows(out)
  res$p.adjust <- p.adjust(res$p, method = p.adjust.m)
  res[order(res$p.adjust), ]
}

pairwise_res <- pairwise_adonis2(X_hell, meta$zona,
                                 method = "euclidean",
                                 permutations = 9999,
                                 p.adjust.m = "BH")
print(pairwise_res)

# --- 6) SIMPER (una sola vez) ---
tidy_simper <- function(sim_obj, top = Inf, perc = TRUE) {
  comps <- names(sim_obj)
  out <- lapply(comps, function(comp) {
    df <- as.data.frame(sim_obj[[comp]])
    df$variable <- rownames(df)
    df <- df[order(-df$average), , drop = FALSE]
    total <- sum(df$average, na.rm = TRUE)
    df$contrib      <- df$average
    df$contrib_prop <- df$contrib / total
    df$cumu_prop    <- cumsum(df$contrib_prop)
    if (perc) {
      df$contrib_pct <- 100 * df$contrib_prop
      df$cumu_pct    <- 100 * df$cumu_prop
    }
    df$comparison <- comp
    rownames(df) <- NULL
    if (is.finite(top)) df <- head(df, top)
    df
  })
  bind_rows(out)
}

set.seed(123)
simper_res <- vegan::simper(X_hell, group = meta$zona, permutations = 9999, trace = FALSE)
simper_tbl <- tidy_simper(simper_res)

# --- 6.1) Tabla filtrada al 70% acumulado ---
simper_70 <- simper_tbl %>%
  group_by(comparison) %>%
  arrange(desc(contrib), .by_group = TRUE) %>%
  filter(cumu_pct <= 70) %>%
  ungroup()
print(simper_70)

# --- 6.2) Determinar zona con mayor promedio por variable ---
simper_70 <- simper_70 %>%
  separate(comparison, into = c("zona_A", "zona_B"), sep = "_", remove = FALSE) %>%
  mutate(zona_mayor = ifelse(ava >= avb, zona_A, zona_B))

simper_70$zona_mayor <- factor(simper_70$zona_mayor,
                               levels = c("Shallow", "Intermediate", "Deep"))

# --- 6.3) Gráfico de barras (colores por zona con mayor promedio) ---
colores_zona <- c("Shallow" = "#CD9B9B",
                  "Intermediate" = "#EEB4B4",
                  "Deep" = "#8B636C")

p_simper <- ggplot(simper_70,
                   aes(x = contrib_pct,
                       y = reorder_within(variable, contrib_pct, comparison),
                       fill = zona_mayor)) +
  geom_col() +
  scale_fill_manual(values = colores_zona, drop = FALSE) +
  facet_wrap(~comparison, scales = "free_y") +
  scale_y_reordered() +
  theme_classic(base_family = "Arial") +
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text  = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 11),
    legend.title = element_blank()
  ) +
  labs(x = "Contribución individual (%)", y = NULL,
       fill = "Zona con mayor promedio")
p_simper
# ggsave("SIMPER_contrib_70pct_coloresZona.png", p_simper, width = 7.5, height = 5.5, dpi = 300)
