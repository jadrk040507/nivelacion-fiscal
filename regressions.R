library(tidyverse)
library(fixest)
library(modelsummary)

df <- read_csv("participaciones-federales.csv") %>% 
    mutate(d2008 = ifelse(year < 2008, 0, 1))

head(df)

model <- feols(govpc ~ gdppc, data = df)
summary(model)

model_log <- feols(log(govpc) ~ log(gdppc), data = df)
summary(model_log)

model_state <- feols(govpc ~ gdppc | Estado, data = df)
summary(model_state)

model_year <- feols(govpc ~ gdppc | year, data = df)
summary(model_year)

model_fixed <- feols(govpc ~ gdppc | Estado + year, data = df)
summary(model_fixed)

model_d2008 <- feols(govpc ~ gdppc + d2008, data = df)
summary(model_d2008)

model_d2008_state <- feols(govpc ~ gdppc + d2008 | Estado, data = df)
summary(model_d2008_state)

modelsummary(list(
    "Modelo lineal" = model,
    "Modelo logarítmico" = model_log,
    "Modelo con efectos de estado" = model_state,
    "Modelo con efectos de año" = model_year,
    "Modelo con efectos fijos" = model_fixed,
    "Modelo con d2008" = model_d2008,
    "Modelo con d2008 y efectos fijos" = model_d2008_state
),)

