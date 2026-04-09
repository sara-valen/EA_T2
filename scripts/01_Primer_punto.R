# =============================================================================
# Script_01: Ejercicio 1
# =============================================================================
# -----------------------------------------------------------------------------
# 0. Cargar bases de datos 
# -----------------------------------------------------------------------------

# Cargar configuración base

source(here::here("scripts", "00_Config.R"))

cuadrantes_geo   <- import(here::here("data", "punto1", "Cuadrantes_Geografica.dta"))
distrito_t1572   <- import(here::here("data", "punto1", "Distrito_Tributo1572.dta"))
hogares_consumo  <- import(here::here("data", "punto1", "Hogares_Consumo.dta"))
ninos_salud      <- import(here::here("data", "punto1", "Ninos_Salud.dta"))

# -----------------------------------------------------------------------------
# 1. Exploración inicial de los datos
# -----------------------------------------------------------------------------

# Examinar la estructura de las bases de datos

glimpse(cuadrantes_geo)
glimpse(distrito_t1572)
glimpse(hogares_consumo)
glimpse(ninos_salud)

skim(hogares_consumo)
skim(ninos_salud)
skim(cuadrantes_geo)
skim(distrito_t1572)

# Estadísticas descriptivas

hogares_consumo %>%
  group_by(pothuan_mita) %>%
  summarise(
    n           = n(),
    consumo_med = mean(lhhequiv),
    consumo_sd  = sd(lhhequiv),
    quechua     = mean(QUE),
    d_bnd_med   = mean(d_bnd)
  )

ninos_salud %>%
  group_by(pothuan_mita) %>%
  summarise(
    n          = n(),
    desnu_tasa = mean(desnu),
    desnu_sd   = sd(desnu),
    elev_med   = mean(elv_sh),
    slope_med  = mean(slope),
    d_bnd_med  = mean(d_bnd)
  )

# Tabla descriptiva

tabla_desc <- bind_rows(
  hogares_consumo %>%
    summarise(
      Variable = "Log consumo per cápita",
      N    = n(),
      Media = mean(lhhequiv),
      SD   = sd(lhhequiv),
      Min  = min(lhhequiv),
      Max  = max(lhhequiv)
    ),
  hogares_consumo %>%
    summarise(
      Variable = "Distancia a frontera (km)",
      N    = n(),
      Media = mean(d_bnd),
      SD   = sd(d_bnd),
      Min  = min(d_bnd),
      Max  = max(d_bnd)
    ),
  hogares_consumo %>%
    summarise(
      Variable = "Indicador mita",
      N    = n(),
      Media = mean(pothuan_mita),
      SD   = sd(pothuan_mita),
      Min  = min(pothuan_mita),
      Max  = max(pothuan_mita)
    ),
  ninos_salud %>%
    summarise(
      Variable = "Desnutrición infantil",
      N    = n(),
      Media = mean(desnu),
      SD   = sd(desnu),
      Min  = min(desnu),
      Max  = max(desnu)
    )
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

tabla_desc

tabla_desc <- tabla_desc %>% 
  mutate(N = as.integer(N))

print(xtable(tabla_desc,
             caption = "Estadísticas descriptivas — Variables principales",
             label   = "tab:desc",
             digits  = c(0, 0, 0, 3, 3, 3, 3)),
      include.rownames = FALSE,
      booktabs         = TRUE)

# Gráfica distribución de la distancia

ggplot(hogares_consumo, aes(x = d_bnd, fill = factor(pothuan_mita))) +
  geom_histogram(bins = 40, alpha = 0.65, position = "identity",
                 color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", 
             color = "black", linewidth = 0.5) +
  scale_fill_manual(
    values = c("0" = "#3B5B7A", "1" = "gray70"),
    labels = c("0" = "No mita", "1" = "Mita")
  ) +
  labs(
    title    = "Distribución de la distancia ajustada a la frontera",
    x        = "Distancia ajustada a la frontera (km)",
    y        = "Número de hogares",
    caption  = "La línea vertical discontinua indica el punto de corte (0 km).",
    fill     = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    plot.caption  = element_text(color = "gray50", size = 9),
    legend.position = "bottom"
  )

ggsave(here::here("stores", "hist_dbnd.png"), 
       width = 9, height = 5, dpi = 180)


