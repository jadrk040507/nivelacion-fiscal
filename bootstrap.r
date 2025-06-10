library(lme4)
library(dplyr)
library(ggplot2)
library(forcats)

df <- read_csv("participaciones-federales.csv")

# 1) fit a robust mixed‐model
model_robust <- lmer(
  lgov   ~ lgdp_c + (lgdp_c | Estado),
  data   = df
)

# 1) extraigo BLUPs y sus SEs
re_df <- tidy(model_robust,
              effects  = "ran_vals",
              conf.int = FALSE) %>%   # no necesito CI por ahora
  filter(term == "lgdp_c") %>%
  rename(se_bj = std.error,
         bj     = estimate)

# 2) valores fijos
beta1   <- fixef(model_robust)["lgdp_c"]
se_beta <- sqrt(vcov(model_robust)["lgdp_c", "lgdp_c"])

# 3) construyo data.frame con slope total y su se
plot_df <- re_df %>%
  mutate(
    slope    = beta1 + bj,
    se_slope = sqrt(se_beta^2 + se_bj^2),
    ci_low   = slope - 1.96*se_slope,
    ci_high  = slope + 1.96*se_slope
  )

# 4) gráfico
ggplot(plot_df, aes(x = slope, y = fct_reorder(level, slope))) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2) +
  geom_vline(xintercept = 0, color = "black", alpha = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "Efecto total (fijo + aleatorio) por Estado")
ggsave("modelo jerárquico.png", width = 8, height = 8, dpi = 300)










set.seed(2025)
boot_fun_full <- function(mod) {
  # coef fijo
  β1    <- fixef(mod)["lgdp_c"]
  # BLUPs de pendiente
  bj    <- ranef(mod)$Estado[ , "lgdp_c"]
  # devolvemos vector de longitud nEstados
  β1 + bj
}

boot_out <- bootMer(
  model_robust,
  FUN    = boot_fun_full,
  nsim   = 1000,           # ≥ 1,000 réplicas para estabilidad
  use.u = TRUE,           # usar BLUPs
  type   = "parametric"
)

# Matriz boot_out$t de dim (nsim × nEstados)
# SE bootstrap de cada columna = sd de las réplicas
se_boot <- apply(boot_out$t, 2, sd)

# CIs percentiles (opcional)
ci_boot <- apply(boot_out$t, 2,
                 function(x) quantile(x, c(0.025, 0.5, 0.975))
               )

# Montas tu data.frame final como antes:
plot_df <- tibble(
  Estado  = rownames(ranef(model_robust)$Estado),
  slope   = fixef(model_robust)["lgdp_c"] +
            ranef(model_robust)$Estado[ , "lgdp_c"],
  se_boot = se_boot,
  median = ci_boot[2,],
  ci_low  = ci_boot[1, ],
  ci_high = ci_boot[3, ]
    # ci_low  = slope - 1.96 * se_boot,
    # ci_high = slope + 1.96 * se_boot
)

print(plot_df)


ggplot(plot_df, aes(x = median, y = fct_reorder(Estado, slope))) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2) +
  geom_vline(xintercept = 0, color = "black", alpha = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Pendientes totales por Estado (IC bootstrap 95%)",
    x     = "Pendiente (coef fijo + aleatorio)",
    y     = "Estado"
  ) 
ggsave("modelo jerárquico - bootstrapped.png", width = 8, height = 8, dpi = 300)
