# ──────────────────────────────────────────────────────────────────────────────
# Script de Análisis Exploratorio y Modelos para Participaciones Federales
# ──────────────────────────────────────────────────────────────────────────────

# 1. CARGAR PAQUETES Y DATOS ---------------------------------------------
# instala si es necesario:
# install.packages(c(
#   "tidyverse","lubridate","lme4","performance",
#   "car","lmtest","GGally","sjPlot","spdep","plm","gridExtra"))

library(tidyverse)
library(lubridate)
library(lme4)
library(performance)
library(car)
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
    Estado = str_replace(Estado, "Michoacan",                 # busca la etiqueta con "Baja Californa"
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
    Año     = as.integer(year)
  )

obs_por_estado <- df %>% count(Estado) %>% arrange(n)
print(obs_por_estado, n = Inf)

df %>% summarise(Año_min = min(Año), Año_max = max(Año))
df %>% group_by(Estado) %>% summarise(A_min = min(Año), A_max = max(Año))

# 3. LIMPIEZA Y TRANSFORMACIONES -----------------------------------------
df <- df %>% filter(govpc > 0, gdppc > 0)

df <- df %>%
  mutate(
    lgov   = log(govpc),
    lgdp   = log(gdppc),
    lgdp_c = lgdp - mean(lgdp, na.rm = TRUE)
  )

# 4. ESTADÍSTICOS DESCRIPTIVOS Y DISTRIBUCIONES -------------------------
summary(df %>% select(gdppc, govpc, lgov, lgdp))

p1 <- ggplot(df, aes(lgdp)) +
  geom_histogram(bins = 30) +
  geom_density(aes(y = ..count..), alpha = 0.3) +
  labs(title = "Log PIB per cápita")

p2 <- ggplot(df, aes(lgov)) +
  geom_histogram(bins = 30) +
  geom_density(aes(y = ..count..), alpha = 0.3) +
  labs(title = "Log Participaciones per cápita")

print(grid.arrange(p1, p2, nrow = 1))

ggplot(df, aes(Estado, govpc)) +
  geom_boxplot(na.rm = TRUE) +
  coord_flip() +
  labs(title = "Participaciones per cápita por estado")

# 5. RELACIÓN PARTICIPACIONES ↔ PIB --------------------------------------
ggplot(df, aes(lgdp, lgov)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess") +
  labs(x = "log(PIB per cápita)", y = "log(Participaciones per cápita)")

ggplot(df, aes(lgdp, lgov)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  facet_wrap(~ Estado, scales = "free") +
  theme(strip.text = element_text(size = 6))

corr_anual <- df %>%
  group_by(Año) %>%
  summarise(cor = cor(lgdp, lgov, use = "pairwise.complete.obs"))

ggplot(corr_anual, aes(Año, cor)) + geom_line() + geom_point() +
  labs(title = "Correlación anual (elasticidades)")

# 6. CORRELACIONES Y MULTICOLINEALIDAD ----------------------------------
df %>% select(gdp, pop, gov, gov_real, gdppc, govpc, lgov, lgdp) %>%
  GGally::ggpairs()

lm_global <- lm(govpc ~ gdppc, data = df)
# car::vif(lm_global)

# 7. OUTLIERS E INFLUENCIA ----------------------------------------------
infl <- influence.measures(lm_global)
summary(infl)
plot(lm_global, which = 5)

res_sd   <- sd(residuals(lm_global))
outliers <- df[abs(residuals(lm_global)) > 3 * res_sd, ]
print(outliers, n = Inf)

# 8. HETEROCEDASTICIDAD --------------------------------------------------
plot(lm_global$fitted.values, lm_global$residuals,
     xlab = "Valores Ajustados", ylab = "Residuos")
abline(h = 0, lty = 2)

lmtest::bptest(lm_global)

# 9. MODELO JERÁRQUICO Y ICC --------------------------------------------
# Intento inicial: random slope con predictor centrado
mixed <- lmer(
  lgov ~ lgdp_c + (lgdp_c | Estado),
  data    = df,
)

# Resumen del modelo
summary(mixed)$coefficients
lme4::ranef(mixed)

# ICC del modelo
dprint <- performance::icc(mixed)
print(dprint)

# Visualizar efectos aleatorios
sjPlot::plot_model(mixed, type = "re", show.values = TRUE) +
  labs(title = "Efectos aleatorios por estado")

# 10. DIAGNÓSTICOS -------------------------------------------------------
qqnorm(residuals(mixed)); qqline(residuals(mixed))
qqnorm(ranef(mixed)$Estado[,"(Intercept)"]); qqline(ranef(mixed)$Estado[,"(Intercept)"])

lmtest::dwtest(lm_global)

# 11. HAUSMAN TEST (FE vs RE) -------------------------------------------
df_plm <- pdata.frame(df, index = c("Estado", "Año"))
fe_mod <- plm(lgov ~ lgdp, data = df_plm, model = "within")
re_mod <- plm(lgov ~ lgdp, data = df_plm, model = "random")
ht <- phtest(fe_mod, re_mod)
print(ht)

# FIN – Interpreta resultados y elige FE o RE según Hausman y diagnóstico de singularidad.

