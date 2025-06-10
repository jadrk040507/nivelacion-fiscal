# 1. CARGAR PAQUETES Y DATOS ---------------------------------------------
library(tidyverse)
library(readxl)
library(skimr)
library(psych)

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
        gov_real = gov / deflator * 100,
        govpc = gov_real / pop * 1000000,
        gdppc = gdp / pop * 1000000,
        gdppc_mp = gdp_mp / pop * 1000000
    )


# Revisar resultado
head(df)

# Estadísticas descriptivas de los datos
skim(df)
describe(df)

# Ajusta la ruta a tu CSV

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

write.csv(df, "participaciones-federales.csv", row.names = FALSE)
