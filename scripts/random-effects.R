# ---- Load Libraries ----
library(dplyr)
library(tidyverse)
library(lme4)
library(ggplot2)
library(forcats)
library(broom.mixed)
library(sjPlot)
library(viridis)
library(sandwich)


# ---- Load Data ----
df <- read_csv("participaciones-federales.csv") %>%
  mutate(
    year = as.factor(year),  # Convert year to factor for random effects
  )

# ---- Hierarchical Model ----

# -----------------------------------------------
# Modelo mixto con random slopes por Estado y Año
# -----------------------------------------------
# Este modelo permite que tanto el intercepto
# como la pendiente de lgdp_c varíen por Estado
# y por Año.
mixed2 <- lmer(
  lgov ~ lgdp_c +
    (1 + lgdp_c | Estado) +   # random intercept + slope por Estado
    (1 + lgdp_c | year),      # random intercept + slope por Año
  data = df
)

# ------------------------------------------------------
# Defino función de varianza‐covarianza para SE robustos
# ------------------------------------------------------
# Usamos HC3 para protegernos contra heterocedasticidad.
vcov_fun <- function(m) sandwich::vcovHC(m, type = "HC3")

# ------------------------------------------------------------
# 1) Forest‐plot de desviaciones de pendiente por Estado (HC3)
# ------------------------------------------------------------
p1 <- plot_model(
  mixed2,
  type       = "re",        # forest‐plot de random‐effects
  ri.nr      = 1,           # 1° agrupamiento = Estado
  terms      = "lgdp_c",    # solo la desviación de la pendiente
  sort.est   = "lgdp_c",    # ordena por magnitud de la pendiente
  vcov.fun   = vcov_fun,    # errores estándar robustos
  show.values= TRUE,
  value.offset=0.3,
  colors     = "bw",
  vline.color= "black",
  ci.style   = "whisker",
  dot.size   = 3,
  line.size  = 1
) +
  labs(
    title = "Slopes GDP-pc por Estado (HC3 SE)",
    x     = "Pendiente sobre GDP-pc (95% CI)",
    y     = "Estado"
  )

# Interpretación Estado:
# - Cada punto es cuánto difiere la elasticidad de gasto–GDP
#   de ese Estado respecto al promedio.
# - Rango: aprox. −0.61 (Colima) a +0.60 (Campeche).
# - Muchos IC no cruzan cero ⇒ desviaciones significativas.
# - Concluimos que la variación regional de la pendiente
#   es muy alta.

# ----------------------------------------------------------
# 2) Forest‐plot de desviaciones de pendiente por Año (HC3)
# ----------------------------------------------------------
p2 <- plot_model(
  mixed2,
  type       = "re",        # forest‐plot de random‐effects
  ri.nr      = 2,           # 2° agrupamiento = year
  terms      = "lgdp_c",    # solo la desviación de la pendiente
  sort.est   = "lgdp_c",
  vcov.fun   = vcov_fun,
  show.values= TRUE,
  value.offset=0.3,
  colors     = "bw",
  vline.color= "black",
  ci.style   = "whisker",
  dot.size   = 3,
  line.size  = 1
) +
  labs(
    title = "Slopes GDP-pc por Año (HC3 SE)",
    x     = "Pendiente sobre GDP-pc (95% CI)",
    y     = "Año"
  )

# Interpretación Año:
# - Cada punto es cuánto difiere la elasticidad de gasto–GDP
#   ese año respecto al promedio.
# - Rango: aprox. −0.20 (2013) a +0.11 (2023).
# - La mayoría de IC cruzan cero ⇒ pocas desviaciones
#   anuales significativas (solo 2015, 2018, 2020).
# - Concluimos que la variación temporal de la pendiente
#   es mucho menor que la regional.

# --------------------------------------------
# 3) Conclusiones globales (comentadas aquí)
# --------------------------------------------
# 1) Mayor variabilidad espacial que temporal:
#    - Estados: pendientes desde −0.61 hasta +0.60.
#    - Años: desde −0.20 hasta +0.11.
# 2) Estados “clave”:
#    - Alta elasticidad: CDMX, Guanajuato, México, Puebla.
#    - Elasticidad negativa: Colima, Campeche, Sonora.
# 3) Años “clave”:
#    - Positivos y significativos: 2015, 2018, 2020.
#    - 2008 no mostró desviaciones fuertes.
# 4) Robustez:
#    - SE HC3 confirman la solidez, sobre todo a nivel
#      estatal.

# -------------------------------------------------------
# 4) Combino ambos forest‐plots en una misma figura final
# -------------------------------------------------------
(combined <- plot_grid(p1, p2, ncol = 1))

save_plot(
  filename    = "random effects combined.png",
  plot        = combined,
  base_width  = 8,
  base_height = 12,
  dpi         = 300
)



# Random effects con predicciones marginales
# -------------------------------------------------------

# -----------------------------------------------
# 1) Predicción marginal por Estado
# -----------------------------------------------
# Cada línea es la predicción "total" de lgov vs lgdp_c
# (efecto fijo + efecto aleatorio) para un Estado.
p1_pred <- plot_model(
  mixed2,
  type      = "pred",               # marginal effects / predicted values
  terms     = c("lgdp_c", "Estado"),# continuo x factor Estado
  pred.type = "re",                 # incluye efectos aleatorios
  ci.lvl    = NA,                   # sin sombreado de IC
  vcov.fun  = vcov_fun              # errores estándar robustos HC3
) +
  labs(
    title = "Predicción de lgov vs GDP-pc por Estado",
    x     = "GDP per cápita (centrado)",
    y     = "Predicted lgov"
  ) +
  scale_color_viridis_d(option = "turbo")

# Interpretación por Estado:
# - En x = 0 (GDP per cápita en media), los distintos interceptos
#   muestran niveles base de gasto más altos (p.ej. CDMX) o más bajos.
# - La inclinación de cada línea es la elasticidad Estado-específica:
#     • Muy empinadas → respuesta fuerte (p.ej. CDMX, Guanajuato).
#     • Planas/descendentes → respuesta débil o negativa (p.ej. Colima, Campeche).
# - El abanico de pendientes revela gran heterogeneidad regional.

# -----------------------------------------------
# 2) Predicción marginal por Año
# -----------------------------------------------
# Cada línea es la predicción "total" de lgov vs lgdp_c
# para un Año (incluye aleatorios).
p2_pred <- plot_model(
  mixed2,
  type      = "pred",
  terms     = c("lgdp_c", "year [all]"), # todos los niveles de year
  pred.type = "re",
  ci.lvl    = NA,
  vcov.fun  = vcov_fun
) +
  labs(
    title = "Predicción de lgov vs GDP-pc por Año",
    x     = "GDP per cápita (centrado)",
    y     = "Predicted lgov"
  ) +
  scale_color_viridis_d(option = "turbo")

# Interpretación por Año:
# - En x = 0, los interceptos anuales apenas varían: gasto medio similar.
# - Las pendientes están casi paralelas, lo que indica poca variación
#   interanual en la elasticidad.
# - Solo años puntuales (p.ej. 2015, 2020) se desvían levemente.

# -----------------------------------------------
# 3) Combino ambos paneles en una figura final
# -----------------------------------------------
(combined_pred <- plot_grid(p1_pred, p2_pred, ncol = 1))

save_plot(
  filename    = "random effects combined - predict.png",
  plot        = combined_pred,
  base_width  = 8,
  base_height = 12,
  dpi         = 300
)
