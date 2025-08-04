# ---- Load Libraries ----
library(tidyverse)
library(readxl)
library(skimr)
library(psych)

# ---- Read Excel Files ----
gdp      <- read_xlsx("PIB real 2005-2023.xlsx", sheet = 1, range = "A2:T35")[-1, ]
gdp_mp   <- read_xlsx("PIB real 2005-2023.xlsx", sheet = 3, range = "A2:T35")[-1, ]
gdp_smp   <- read_xlsx("PIB real 2005-2023.xlsx", sheet = 4, range = "A2:T35")[-1, ]
gov      <- read_xlsx("Participaciones federales 2005-2023.xlsx", sheet = 1, range = "A3:T36")[-1, ]
deflator <- read_xlsx("Participaciones federales 2005-2023.xlsx", sheet = 2, range = "A5:T38")[-1, ]
pop      <- read_xlsx("Participaciones federales 2005-2023.xlsx", sheet = 4, range = "A3:T36")[-1, ]

# ---- Build Long Tables ----
df_list <- list(
  gdp      = gdp,
  gdp_mp   = gdp_mp,
  gdp_smp   = gdp_smp,
  gov      = gov,
  deflator = deflator,
  pop      = pop
)

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
      Estado = str_squish(Estado),                         # remove extra spaces
      # remove footnote marks in some state names
      Estado = str_replace(Estado, "Ciudad de México 2_/", "Ciudad de México"),
      Estado = str_replace(Estado, "Baja Californa", "Baja California"),
      Estado = str_replace(Estado, "Michoacan", "Michoacán")
    )
}

long_list <- Map(long, df_list, names(df_list))

# ---- Combine Sources ----
df <- reduce(long_list, full_join, by = c("Estado", "year")) %>%
  mutate(
    gov_real  = gov / deflator * 100,
    govpc     = gov_real / pop * 1e6,
    gdppc     = gdp / pop * 1e6,
    gdppc_smp  = gdp_smp / pop * 1e6
  )

# ---- Quick Checks ----
head(df)
skim(df)
describe(df)

# ---- Panel Structure ----
df <- df %>%
  mutate(
    Estado = as.factor(Estado),
    year   = as.integer(year)
  )

df %>% count(Estado) %>% arrange(n) %>% print(n = Inf)
df %>% map_int(~ sum(is.na(.)))
df %>% map_int(~ sum(. == 0))
df %>% group_by(Estado) %>% reframe(A_min = min(year), A_max = max(year)) %>% print(n = Inf)

# ---- Transformations ----
# df <- df %>%
#   mutate(
#     lgov   = log(govpc),
#     lgdp   = log(gdppc),
#     lgdp_c = lgdp - mean(lgdp, na.rm = TRUE)
#   )

# ---- Save Clean Data ----
write.csv(df, "participaciones-federales.csv", row.names = FALSE)
