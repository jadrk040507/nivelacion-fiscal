library(lme4)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forcats)
library(broom.mixed)

df <- read_csv("participaciones-federales.csv")

# 1) fit a robust mixed‐model
model_robust <- lmer(
  lgov   ~ lgdp_c + (lgdp_c | Estado),
  data   = df
)

set.seed(2025)
boot_fun_full <- function(mod) {
  # coef fijo
  f    <- fixef(mod)["lgdp_c"]
  # BLUPs de pendiente
  r    <- ranef(mod)$Estado[ , "lgdp_c"]
  # devolvemos vector de longitud nEstados
  f + r
}

boot_out <- bootMer(
  model_robust,
  FUN    = boot_fun_full,
  nsim   = 1000,           # ≥ 1,000 réplicas para estabilidad
)

# Matriz boot_out$t de dim (nsim × nEstados)
# SE bootstrap de cada columna = sd de las réplicas
se_boot <- apply(boot_out$t, 2, sd)

# CIs percentiles (opcional)
# ci_boot <- apply(boot_out$t, 2,
#                  function(x) quantile(x, c(0.025, 0.5, 0.975))
#                )

# Montas tu data.frame final como antes:
plot_df <- tibble(
  Estado  = rownames(ranef(model_robust)$Estado),
  slope   = fixef(model_robust)["lgdp_c"] +
            ranef(model_robust)$Estado[ , "lgdp_c"],
  se_boot = se_boot,
#   median = ci_boot[2,],
  # ci_low  = ci_boot[1, ],
  # ci_high = ci_boot[3, ]
  ci_low  = slope - 1.96 * se_boot,
  ci_high = slope + 1.96 * se_boot
)

print(plot_df)


ggplot(plot_df, aes(x = slope, y = fct_reorder(Estado, slope))) +
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
