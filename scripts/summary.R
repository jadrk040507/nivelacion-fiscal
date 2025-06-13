# ---- Exploratory Analysis and Models ----

# Load libraries
library(tidyverse)
library(readxl)
library(skimr)
library(psych)
library(lme4)
library(broom.mixed)
library(sandwich)
library(lmtest)
library(GGally)
library(sjPlot)
library(plm)
library(gridExtra)

# Load cleaned data
df <- read_csv("participaciones-federales.csv")

# ---- Descriptive Statistics ----
describe(df %>% select(gdppc, govpc, lgov, lgdp))

p1 <- ggplot(df, aes(lgdp)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = 0.3) +
  labs(title = "Log PIB per cápita")

p2 <- ggplot(df, aes(lgov)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = 0.3) +
  labs(title = "Log Participaciones per cápita")

png(
  filename = "distribucion.png",
  width    = 8,
  height   = 6,
  units    = "in",
  res      = 300
)
grid.arrange(p1, p2, nrow = 1)
dev.off()

# Boxplot by state
ggplot(df, aes(Estado, govpc)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Participaciones per cápita por estado")

ggsave("boxplot participaciones.png", width = 8, height = 8, dpi = 300)

# ---- Relationship PIB vs Participaciones ----

# Scatter plot and smooth line
ggplot(df, aes(lgdp, lgov)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  labs(x = "log(PIB per cápita)", y = "log(Participaciones per cápita)")

ggsave("lgdp vs lgov.png", width = 8, height = 6, dpi = 300)

# Faceted by state
ggplot(df, aes(lgdp, lgov)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "steelblue") +
  facet_wrap(~ Estado, scales = "free") +
  theme(strip.text = element_text(size = 6))

ggsave("lgdp vs lgov por Estado.png", width = 8, height = 6, dpi = 300)

# Annual correlation between variables
corr_anual <- df %>%
  group_by(year) %>%
  summarise(cor = cor(lgdp, lgov, use = "pairwise.complete.obs"))

ggplot(corr_anual, aes(year, cor)) +
  geom_line() +
  geom_point() +
  labs(title = "Correlación anual (elasticidades)")

ggsave("cor anual.png", width = 8, height = 6, dpi = 300)

# ---- Simple Regression Diagnostics ----

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

# ---- Hierarchical Model ----

mixed <- lmer(
  lgov ~ lgdp_c + (lgdp_c | Estado),
  data = df
)

summary(mixed)$coefficients
lme4::ranef(mixed)

vcov_fun <- function(m) sandwich::vcovHC(m, type = "HC3")

plot_model(
  mixed,
  type        = "re",
  sort.est    = "lgdp_c",
  show.values = TRUE,
  colors      = "bw",
  vline.color = "black",
  ci.style    = "whisker",
  dot.size    = 3,
  line.size   = 1,
  vcov.fun    = vcov_fun,
  value.offset = 0.3
) +
  labs(
    title = "Estimated gdppc-slopes by Estado",
    x     = "Slope on gdppc (with 95% CI)",
    y     = "Estado"
  )

ggsave("modelo jerárquico - ambos.png", width = 8, height = 8, dpi = 300)

# Robust mixed model
model_robust <- lmer(
  lgov ~ lgdp_c + (lgdp_c | Estado),
  data = df
)

re_df <- tidy(
  model_robust,
  effects  = "ran_vals",
  conf.int = FALSE
) %>%
  filter(term == "lgdp_c") %>%
  rename(se_bj = std.error, bj = estimate)

beta1   <- fixef(model_robust)["lgdp_c"]
se_beta <- sqrt(
  diag(vcov_fun(model_robust))  # matriz de covariancias robusta
)["lgdp_c"]

plot_df <- re_df %>%
  mutate(
    slope    = beta1 + bj,
    se_slope = sqrt(se_beta^2 + se_bj^2),
    ci_low   = slope - 1.96 * se_slope,
    ci_high  = slope + 1.96 * se_slope
  )

# Plot total effects
ggplot(plot_df, aes(x = slope, y = fct_reorder(level, slope))) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2) +
  geom_vline(xintercept = 0, colour = "black", alpha = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Efecto total (fijo + aleatorio) por Estado",
    x     = "Pendiente (coef fijo + aleatorio)"
  )

ggsave("modelo jerárquico.png", width = 8, height = 8, dpi = 300)

# ---- Diagnostics ----
par(mfrow = c(1, 1))
qqnorm(residuals(mixed)); qqline(residuals(mixed))
qqnorm(ranef(mixed)$Estado[, "(Intercept)"]); qqline(ranef(mixed)$Estado[, "(Intercept)"])

# ---- Hausman Test ----
df_plm <- pdata.frame(df, index = c("Estado", "year"))
fe_mod <- plm(lgov ~ lgdp, data = df_plm, model = "within")
re_mod <- plm(lgov ~ lgdp, data = df_plm, model = "random")

phtest(fe_mod, re_mod)

