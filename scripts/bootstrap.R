# ---- Load Libraries ----
library(dplyr)
library(tidyverse)
library(lme4)
library(ggplot2)
library(forcats)
library(broom.mixed)

# ---- Load Data ----
df <- read_csv("participaciones-federales.csv")

# ---- Fit Mixed Model ----
model_robust <- lmer(
  lgov ~ lgdp_c + (lgdp_c | Estado),
  data = df
)

# ---- Bootstrap Slopes ----
set.seed(2025)
boot_fun_full <- function(mod) {
  f <- fixef(mod)["lgdp_c"]       # fixed effect
  r <- ranef(mod)$Estado[ , "lgdp_c"] # random slopes
  f + r                            # vector of slopes
}

boot_out <- bootMer(
  model_robust,
  FUN  = boot_fun_full,
  nsim = 1000                     # ≥ 1,000 replicates
)

# Standard errors from bootstrap samples
se_boot <- apply(boot_out$t, 2, sd)

# ---- Prepare Results ----
plot_df <- tibble(
  Estado  = rownames(ranef(model_robust)$Estado),
  slope   = fixef(model_robust)["lgdp_c"] +
    ranef(model_robust)$Estado[ , "lgdp_c"],
  se_boot = se_boot,
  ci_low  = slope - 1.96 * se_boot,
  ci_high = slope + 1.96 * se_boot
)

print(plot_df)

# ---- Plot Bootstrap Estimates ----
ggplot(plot_df, aes(x = slope, y = fct_reorder(Estado, slope))) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2) +
  geom_vline(xintercept = 0, colour = "black", alpha = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Pendientes totales por Estado (IC bootstrap 95%)",
    x     = "Pendiente (coef fijo + aleatorio)",
    y     = "Estado"
  )

ggsave("modelo jerárquico - bootstrapped.png", width = 8, height = 8, dpi = 300)
