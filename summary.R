# ──────────────────────────────────────────────────────────────────────────────
# Script de Análisis Exploratorio y Modelos para Participaciones Federales
# ──────────────────────────────────────────────────────────────────────────────

# 1. CARGAR PAQUETES Y DATOS ---------------------------------------------
# instala si es necesario:
# install.packages(c(
#   "tidyverse","lubridate","lme4","performance",
#   "car","lmtest","GGally","sjPlot","spdep","plm","gridExtra"))

library(tidyverse)
library(readxl)
library(skimr)
library(psych)
library(lubridate)
library(lme4)
library(performance)
library(lmtest)
library(GGally)
library(sjPlot)
library(plm)
library(gridExtra)
# library(spdep)

# Leer datos desde Excel y quitar primera fila de encabezados duplicados
gdp      <- read_xlsx("PIB real 2005-2023.xlsx", sheet = 1, range = "A2:T35")[-1, ]
gdp_mp   <- read_xlsx("PIB real 2005-2023.xlsx", sheet = 3, range = "A2:T35")[-1, ]
gov      <- read_xlsx("Participaciones federales 2005-2023.xlsx", sheet = 1, range = "A3:T36")[-1, ]
deflator <- read_xlsx("Participaciones federales 2005-2023.xlsx", sheet = 2, range = "A5:T38")[-1, ]
pop      <- read_xlsx("Participaciones federales 2005-2023.xlsx", sheet = 4, range = "A3:T36")[-1, ]

# Lista nombrada de data.frames
df_list <- list(
  gdp      = gdp,
  gdp_mp   = gdp_mp,
  gov      = gov,
  deflator = deflator,
  pop      = pop
)

# Función: renombra primera columna a "Estado" y pivotea a formato long
long <- function(df, value_name) {
  old_first <- names(df)[1]
  if (old_first != "Estado") {
    df <- df %>% rename(Estado = !!sym(old_first))
  }
  df %>%
    pivot_longer(
      cols      = -Estado,
      names_to  = "year",
      values_to = value_name
    ) %>%
    mutate(year = as.integer(year)) %>% 
    mutate(
    Estado = str_squish(Estado),                              # quita espacios dobles y bordes
    Estado = str_replace(Estado, "Ciudad de México 2_/",      # busca la etiqueta con “2_/”
                            "Ciudad de México"),              # y la reemplaza con la correcta
    Estado = str_replace(Estado, "Baja Californa",            # busca la etiqueta con "Baja Californa"
                            "Baja California"),               # y la reemplaza con la correcta
    Estado = str_replace(Estado, "Michoacan",                 # busca la etiqueta con "Michoacan"
                            "Michoacán")                      # y la reemplaza con la correcta
  )
}

# Aplicar pivot para cada data.frame usando su nombre como columna de valores
long_list <- Map(long, df_list, names(df_list))

# Combinar con full_join por "Estado" y "year"
df <- reduce(long_list, full_join, by = c("Estado", "year")) %>% 
    mutate(
        gov_real = gov / deflator,
        govpc = gov_real / pop * 1000000,
        gdppc = gdp / pop * 1000000,
        gdppc_mp = gdp_mp / pop * 1000000
    )

write.csv(df, "participaciones-federales.csv", row.names = FALSE)

# Revisar resultado
head(df)

# Estadísticas descriptivas de los datos
skim(df)
describe(df)

# Ajusta la ruta a tu CSV
df <- read_csv("participaciones-federales.csv")

# 2. ESTRUCTURA Y COBERTURA DEL PANEL -----------------------------------
df <- df %>%
  mutate(
    Estado = as.factor(Estado),
    year     = as.integer(year)
  )


df %>% count(Estado) %>% arrange(n) %>% print(n = Inf)
df %>% map_int(~ sum(is.na(.)))
df %>% map_int(~ sum(. == 0))
df %>% group_by(Estado) %>% reframe(A_min = min(year), A_max = max(year)) %>% print(n = Inf)

# 3. LIMPIEZA Y TRANSFORMACIONES -----------------------------------------
df <- df %>%
  mutate(
    lgov   = log(govpc),
    lgdp   = log(gdppc),
    lgdp_c = lgdp - mean(lgdp, na.rm = TRUE)
  )

# 4. ESTADÍSTICOS DESCRIPTIVOS Y DISTRIBUCIONES -------------------------
describe(df %>% select(gdppc, govpc, lgov, lgdp))

p1 <- ggplot(df, aes(lgdp)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = 0.3) +
  labs(title = "Log PIB per cápita")

p2 <- ggplot(df, aes(lgov)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = 0.3) +
  labs(title = "Log Participaciones per cápita")

grid.arrange(p1, p2, nrow = 1)
ggsave("distribución.png", width = 8, height = 6, dpi = 300)

ggplot(df, aes(Estado, govpc)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Participaciones per cápita por estado")
ggsave("boxplot participaciones.png", width = 8, height = 6, dpi = 300)

# 5. RELACIÓN PARTICIPACIONES ↔ PIB --------------------------------------
ggplot(df, aes(lgdp, lgov)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  labs(x = "log(PIB per cápita)", y = "log(Participaciones per cápita)")
ggsave("lgdp vs lgov.png", width = 8, height = 6, dpi = 300)

ggplot(df, aes(lgdp, lgov)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  facet_wrap(~ Estado, scales = "free") +
  theme(strip.text = element_text(size = 6))
ggsave("lgdp vs lgov por Estado.png", width = 8, height = 6, dpi = 300)

corr_anual <- df %>%
  group_by(year) %>%
  summarise(cor = cor(lgdp, lgov, use = "pairwise.complete.obs"))

ggplot(corr_anual, aes(year, cor)) + geom_line() + geom_point() +
  labs(title = "Correlación anual (elasticidades)")
ggsave("cor anual.png", width = 8, height = 6, dpi = 300)

# 6. REGRESIÖN -----------------------------------------------------------
df %>% select(gdp, pop, gov, gov_real, gdppc, govpc, lgov, lgdp) %>%
  GGally::ggpairs()
  ggsave("descriptivo.png", width = 8, height = 6, dpi = 300)

lm_global <- lm(govpc ~ gdppc, data = df)
summary(lm_global)

png(filename = "regresión simple.png", width = 8, height = 6, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(lm_global)
dev.off()

res_sd   <- sd(residuals(lm_global))
outliers <- df[abs(residuals(lm_global)) > 3 * res_sd, ]
print(outliers, n = Inf)

# 7. MODELO JERÁRQUICO Y ICC --------------------------------------------
# Intento inicial: random slope con predictor centrado
mixed <- lmer(
  lgov ~ lgdp_c + (lgdp_c | Estado),
  data    = df,
)

# Resumen del modelo
summary(mixed)$coefficients
lme4::ranef(mixed)

# Visualizar efectos aleatorios
plot_model(
  mixed,
  type        = "re",
  show.values = TRUE,
  colors      = c("red", "black"),      # all points & whiskers in black :contentReference[oaicite:2]{index=2}
  vline.color = "#00000084"      # draws the zero‐line in black
) +
  labs(title = "Efectos aleatorios por estado")
  ggsave("modelo jerárquico.png", width = 8, height = 8, dpi = 300)


# 8. DIAGNÓSTICOS -------------------------------------------------------
par(mfrow = c(1,1))
qqnorm(residuals(mixed)); qqline(residuals(mixed))
qqnorm(ranef(mixed)$Estado[,"(Intercept)"]); qqline(ranef(mixed)$Estado[,"(Intercept)"])

# 9. HAUSMAN TEST (FE vs RE) -------------------------------------------
df_plm <- pdata.frame(df, index = c("Estado", "year"))
fe_mod <- plm(lgov ~ lgdp, data = df_plm, model = "within")
re_mod <- plm(lgov ~ lgdp, data = df_plm, model = "random")

phtest(fe_mod, re_mod)

