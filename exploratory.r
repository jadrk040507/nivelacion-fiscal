library(tidyverse)
library(scales)
library(readxl)
library(lmerTest)
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

# Evolución del PIB Real por Estado (líneas)
ggplot(df, aes(x = year, y = gdp, color = Estado)) +
  geom_line(size = 0.8) +
  labs(
    title = "Evolución del PIB Real por Estado (2005–2023)",
    x     = "Año",
    y     = "PIB Real (millones MXN)"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(prefix = "$", suffix = " MXN"),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  theme_minimal() +
  theme(
    legend.position   = "bottom",
    legend.title      = element_blank(),
    axis.text.x       = element_text(angle = 0, vjust = 0.5),
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5)
  )
ggsave("gdp.png", width = 8, height = 6, dpi = 300)

# Boxplot de distribución anual de PIB Real,
# con etiquetas de los 5 estados con mayor PIB cada año
bs_gdp <- df %>%
  group_by(year) %>%
  slice_max(gdp, n = 5) %>%
  ungroup()

ggplot(df, aes(x = factor(year), y = gdp, group = year)) +
  geom_boxplot(fill = "steelblue", alpha = 0.3) +
  geom_text(
    data     = bs_gdp,
    aes(label = Estado),
    position = position_jitter(width = 0.2, height = 0),
    size     = 3,
    color    = "black"
  ) +
  labs(
    title = "Distribución Anual de PIB Real con Top 5 Estados Etiquetados",
    x     = "Año",
    y     = "PIB Real (millones MXN)"
  ) +
  scale_y_continuous(labels = scales::label_comma(prefix = "$", suffix = " MXN")) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "none"
  )
ggsave("box-gdp.png", width = 8, height = 6, dpi = 300)


# Evolución de Participaciones Federales por Estado (líneas)
ggplot(df, aes(x = year, y = gov, color = Estado)) +
  geom_line(size = 0.8) +
  labs(
    title = "Evolución de Participaciones Federales por Estado (2005–2023)",
    x     = "Año",
    y     = "Participaciones Federales (millones MXN)"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(prefix = "$", suffix = " MXN"),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  theme_minimal() +
  theme(
    legend.position   = "bottom",
    legend.title      = element_blank(),
    axis.text.x       = element_text(angle = 0, vjust = 0.5),
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5)
  )
ggsave("gov.png", width = 8, height = 6, dpi = 300)


# Boxplot de distribución anual de Participaciones Federales,
# con etiquetas de los 5 estados con mayor valor cada año
bs_gov <- df %>%
  group_by(year) %>%
  slice_max(order_by = gov, n = 5) %>%
  ungroup()

ggplot(df, aes(x = factor(year), y = gov, group = year)) +
  geom_boxplot(fill = "forestgreen", alpha = 0.3) +
  geom_text(
    data     = bs_gov,
    aes(label = Estado),
    position = position_jitter(width = 0.2, height = 0),
    size     = 3,
    color    = "black"
  ) +
  labs(
    title = "Distribución Anual de Participaciones Federales con Top 5 Estados Etiquetados",
    x     = "Año",
    y     = "Participaciones Federales (millones MXN)"
  ) +
  scale_y_continuous(labels = scales::label_comma(prefix = "$", suffix = " MXN")) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "none"
  )
ggsave("box-gov.png", width = 8, height = 6, dpi = 300)


# Evolución de Participaciones Federales Reales (líneas)
ggplot(df, aes(x = year, y = gov_real, color = Estado)) +
  geom_line(size = 0.8) +
  labs(
    title = "Evolución de Participaciones Federales Reales por Estado (2005–2023)",
    x     = "Año",
    y     = "Participaciones Federales Reales (millones MXN)"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(prefix = "$", suffix = " MXN"),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  theme_minimal() +
  theme(
    legend.position   = "bottom",
    legend.title      = element_blank(),
    axis.text.x       = element_text(angle = 0, vjust = 0.5),
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5)
  )
ggsave("gov-real.png", width = 8, height = 6, dpi = 300)


# Boxplot de distribución anual de Participaciones Federales Reales,
# con etiquetas de los 5 estados con mayor valor cada año
bs_gov_real <- df %>%
  group_by(year) %>%
  slice_max(order_by = gov_real, n = 5) %>%
  ungroup()

ggplot(df, aes(x = factor(year), y = gov_real, group = year)) +
  geom_boxplot(fill = "darkorange", alpha = 0.3) +
  geom_text(
    data     = bs_gov_real,
    aes(label = Estado),
    position = position_jitter(width = 0.2, height = 0),
    size     = 3,
    color    = "black"
  ) +
  labs(
    title = "Distribución Anual de Participaciones Federales Reales con Top 5 Estados Etiquetados",
    x     = "Año",
    y     = "Participaciones Federales Reales (millones MXN)"
  ) +
  scale_y_continuous(labels = scales::label_comma(prefix = "$", suffix = " MXN")) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "none"
  )
ggsave("box-gov-real.png", width = 8, height = 6, dpi = 300)


# Evolución de Participaciones Federales Reales per cápita (líneas)
ggplot(df, aes(x = year, y = govpc, color = Estado)) +
  geom_line(size = 0.8) +
  labs(
    title = "Evolución de Participaciones Federales Reales per cápita por Estado (2005–2023)",
    x     = "Año",
    y     = "Participaciones Federales Reales (millones MXN)"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(prefix = "$", suffix = " MXN"),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  theme_minimal() +
  theme(
    legend.position   = "bottom",
    legend.title      = element_blank(),
    axis.text.x       = element_text(angle = 0, vjust = 0.5),
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5)
  )
ggsave("govpc.png", width = 8, height = 6, dpi = 300)


# Boxplot de distribución anual de Participaciones Federales Reales per cápita,
# con etiquetas de los 5 estados con mayor valor cada año
bs_govpc <- df %>%
  group_by(year) %>%
  slice_max(order_by = govpc, n = 5) %>%
  ungroup()

ggplot(df, aes(x = factor(year), y = govpc, group = year)) +
  geom_boxplot(fill = "darkorange", alpha = 0.3) +
  geom_text(
    data     = bs_govpc,
    aes(label = Estado),
    position = position_jitter(width = 0.2, height = 0),
    size     = 3,
    color    = "black"
  ) +
  labs(
    title = "Distribución Anual de Participaciones Federales Reales per cápita con Top 5 Estados Etiquetados",
    x     = "Año",
    y     = "Participaciones Federales Reales"
  ) +
  scale_y_continuous(labels = scales::label_comma(prefix = "$", suffix = " MXN")) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "none"
  )
ggsave("box-govpc.png", width = 8, height = 6, dpi = 300)

# Gráfico de la evolución de la población por Estado (2005–2023)
ggplot(df, aes(year, pop, color = Estado, label = Estado)) +
  geom_line(size = 0.8) +         # líneas de población a lo largo del tiempo
  geom_text() +                   # etiquetas con el nombre del Estado
  labs(
    title = "Evolución de la Población por Estado (2005–2023)",
    x     = "Año",
    y     = "Población"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(),    # formato con separador de miles
    breaks = scales::pretty_breaks(n = 5)
  ) +
  theme_minimal() +
  theme(
    legend.position   = "bottom",      # leyenda abajo
    legend.title      = element_blank(),
    axis.text.x       = element_text(angle = 0, vjust = 0.5),
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5)
  )
ggsave("pop.png", width = 8, height = 6, dpi = 300)


# Cálculo de cambios logarítmicos anuales y acumulados de población
delta_pop <- df %>%
  group_by(Estado) %>%
  reframe(
    year,
    delta_pop = c(0, diff(log(pop))),   # diferencia logarítmica de población
    cum_pop   = cumsum(delta_pop)       # suma acumulada de los cambios
  )

# Gráfico de los cambios logarítmicos anuales por Estado
ggplot(delta_pop, aes(year, y = delta_pop, color = Estado, label = Estado)) +
  geom_line() +        # líneas de delta_pop a lo largo del tiempo
  geom_text() +        # etiquetas con el nombre del Estado
  theme_minimal()
ggsave("pop-growth.png", width = 8, height = 6, dpi = 300)

# Gráfico de los cambios logarítmicos acumulados por Estado
ggplot(delta_pop, aes(year, y = cum_pop, color = Estado, label = Estado)) +
  geom_line() +        # líneas de cum_pop a lo largo del tiempo
  geom_text() +        # etiquetas con el nombre del Estado
  theme_minimal()
ggsave("pop-growth-acc.png", width = 8, height = 6, dpi = 300)

# Modelo lineal simple de govpc en función de gdppc
lm(govpc ~ gdppc, data = df) %>%
  summary()            # resumen de los coeficientes y ajustes

# Modelo mixto con pendiente aleatoria de gdppc por Estado
re <- lmer(govpc ~ gdppc + (gdppc | Estado), data = df)
summary(re)            # resumen del modelo mixto

# Extracción de efectos aleatorios incluyendo varianza condicional
re_ranef <- ranef(re, condVar = TRUE)

# Recuperar el array de varianzas para el término gdppc
postVar_array <- attr(re_ranef$Estado, "postVar")

# Preparar data.frame con estimaciones y errores estándar por Estado
slope_df <- re_ranef$Estado %>%
  select(gdppc) %>%                      # solo la desviación de la pendiente
  rownames_to_column("area") %>%         # pasar fila a columna
  mutate(
    estimate  = gdppc + fixef(re)["gdppc"],  # pendiente total (BLUP + fijo)
    se        = sqrt(postVar_array[1,1, ]),  # error estándar = raíz de la varianza
    area_plot = fct_reorder(area, estimate)  # reordenar factores para la gráfica
  )

# Gráfico de las pendientes con sus intervalos de confianza al 95%
ggplot(slope_df, aes(x = estimate, y = area_plot)) +
  geom_point(size = 2) +                         # puntos de las estimaciones
  geom_errorbarh(                                # barras horizontales ±1.96·SE
    aes(xmin = estimate - 1.96*se,
        xmax = estimate + 1.96*se),
    height = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +  # línea vertical en cero
  labs(
    x        = "Slope on gdppc (with 95% CI)",
    y        = "Estado",
    title    = "Estimated gdppc-slopes by Estado",
    subtitle = "Points = BLUPs; bars = ±1.96×SE"
  ) +
  scale_x_continuous(
    labels = scales::label_comma(),            # formato con separador de miles
    breaks = scales::breaks_pretty()
  ) +
  theme_minimal()
ggsave("slopes-states.png", width = 8, height = 6, dpi = 300)
