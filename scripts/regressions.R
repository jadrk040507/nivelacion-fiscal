# ---- Load Libraries ----
library(tidyverse)
library(fixest)
library(modelsummary)

# ---- Load Data ----
df <- read_csv("participaciones-federales.csv") %>%
  mutate(d2008 = if_else(year < 2008, 0, 1),
         d2015 = if_else(year < 2015, 0, 1),
         d2018 = if_else(year < 2018, 0, 1),
         d2020 = if_else(year < 2020, 0, 1))

head(df)

# ---- Simple Models ----
model <- feols(govpc ~ gdppc, data = df)
summary(model)

model_log <- feols(log(govpc) ~ log(gdppc), data = df)
summary(model_log)

model_log_state <- feols(log(govpc) ~ log(gdppc) | Estado, data = df)
summary(model_log_state)

model_log_year <- feols(log(govpc) ~ log(gdppc) | year, data = df)
summary(model_log_year)

model_log_fixed <- feols(log(govpc) ~ log(gdppc) | Estado + year, data = df)
summary(model_log_fixed)

# ---- Fixed Effects ----
model_state <- feols(govpc ~ gdppc | Estado, data = df)
summary(model_state)

model_year <- feols(govpc ~ gdppc | year, data = df)
summary(model_year)

model_fixed <- feols(govpc ~ gdppc | Estado + year, data = df)
summary(model_fixed)

# ---- Dummy Example ----
model_d2008_state <- feols(govpc ~ gdppc + d2008*gdppc | Estado, data = df)
summary(model_d2008_state)

model_d2015_state <- feols(govpc ~ gdppc + d2015*gdppc | Estado, data = df)
summary(model_d2015_state)

model_d2018_state <- feols(govpc ~ gdppc + d2018*gdppc | Estado, data = df)
summary(model_d2018_state)

model_d2020_state <- feols(govpc ~ gdppc + d2020*gdppc | Estado, data = df)
summary(model_d2020_state)

model_d2008_2015_2018_state <- feols(govpc ~ gdppc + d2008*gdppc + d2015*gdppc + d2018*gdppc | Estado, data = df)
summary(model_d2008_2015_2018_state)

model_d2008_2015_2020_state <- feols(govpc ~ gdppc + d2008*gdppc + d2015*gdppc + d2020*gdppc | Estado, data = df)
summary(model_d2008_2015_2020_state)


# ---- Model Summary Table ----
modelsummary(
  list(
    "Modelo lineal"                              = model,
    "Modelo logarítmico"                         = model_log,
    "Modelo logarítmico con efectos de estado"   = model_log_state,
    "Modelo logarítmico con efectos de año"      = model_log_year,
    "Modelo logarítmico con efectos fijos"       = model_log_fixed,
    "Modelo con efectos de estado"               = model_state,
    "Modelo con efectos de año"                  = model_year,
    "Modelo con efectos fijos"                   = model_fixed,
    "Modelo con d2008 y efectos fijos"           = model_d2008_state,
    "Modelo con d2015 y efectos fijos"           = model_d2015_state,
    "Modelo con d2018 y efectos fijos"           = model_d2018_state,
    "Modelo con d2020 y efectos fijos"           = model_d2020_state,
    "Modelo con d2008, d2015 y d2018"            = model_d2008_2015_2018_state,
    "Modelo con d2008, d2015 y d2020"            = model_d2008_2015_2020_state
  )
)




