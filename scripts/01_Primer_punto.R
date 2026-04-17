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


# Consumo

consumo_total <- hogares_consumo %>%
  summarise(
    Variable = "Log consumo per cápita",
    Grupo    = "Total",
    N        = n(),
    Media    = mean(lhhequiv),
    SD       = sd(lhhequiv)
  )

consumo_grupo <- hogares_consumo %>%
  group_by(pothuan_mita) %>%
  summarise(
    N     = n(),
    Media = mean(lhhequiv),
    SD    = sd(lhhequiv)
  ) %>%
  mutate(
    Variable = "Log consumo per cápita",
    Grupo    = ifelse(pothuan_mita == 1,
                      "Dentro de la mita",
                      "Fuera de la mita")
  ) %>%
  select(Variable, Grupo, N, Media, SD)

# Desnutrición
desnu_total <- ninos_salud %>%
  summarise(
    Variable = "Desnutrición infantil",
    Grupo    = "Total",
    N        = n(),
    Media    = mean(desnu),
    SD       = sd(desnu)
  )

desnu_grupo <- ninos_salud %>%
  group_by(pothuan_mita) %>%
  summarise(
    N     = n(),
    Media = mean(desnu),
    SD    = sd(desnu)
  ) %>%
  mutate(
    Variable = "Desnutrición infantil",
    Grupo    = ifelse(pothuan_mita == 1,
                      "Dentro de la mita",
                      "Fuera de la mita")
  ) %>%
  select(Variable, Grupo, N, Media, SD)

# Merge

tabla_desc_unica <- bind_rows(
  consumo_total,
  consumo_grupo,
  desnu_total,
  desnu_grupo
) %>%
  mutate(across(c(Media, SD), ~ round(.x, 3)))

tabla_desc_unica

# Exportar con el formato 

print(xtable(tabla_desc_unica,
             caption = "Estadísticas descriptivas — Variables principales",
             label   = "tab:desc_unica"),
      include.rownames = FALSE,
      booktabs         = TRUE,
      sanitize.text.function = identity)



tabla_design <- bind_rows(
  
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
    )
  
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

tabla_design

# Exportar con el formato
print(xtable(tabla_design,
             caption = "Variables de diseño",
             label   = "tab:design"),
      include.rownames = FALSE,
      booktabs         = TRUE,
      sanitize.text.function = identity)


# Gráfica distribución de la distancia

grafica_dbnd <- ggplot(cuadrantes_geo, aes(x = d_bnd)) +
  
  geom_histogram(
    binwidth  = 5,
    fill      = "#B0C4DE",
    color     = "white",
    linewidth = 0.2
  ) +
  
  stat_function(
    fun = function(x) dnorm(x, mean = mean(cuadrantes_geo$d_bnd),
                            sd   = sd(cuadrantes_geo$d_bnd)) *
      nrow(cuadrantes_geo) * 5,
    color       = "black",
    linewidth   = 0.6,
    linetype    = "dashed",
    inherit.aes = FALSE
  ) +
  
  geom_vline(
    xintercept = 0,
    color      = "gray30",
    linetype   = "dashed",
    linewidth  = 0.6
  ) +
  
  coord_cartesian(xlim = c(-100, 100)) +
  scale_x_continuous(breaks = seq(-100, 100, 50)) +
  
  labs(
    x = "Distancia a la frontera",
    y = "Frecuencia"
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    axis.title  = element_text(face = "bold"),
    axis.line   = element_line(linewidth = 0.4),
    plot.margin = margin(20, 25, 15, 25)
  )

ggsave(
  filename = store_file("grafica_dbnd.png"),
  plot     = grafica_dbnd,
  width    = 10,
  height   = 4,
  dpi      = 300,
  bg       = "white"
)


# -----------------------------------------------------------------------------
# 3. Visualización de la discontinuidad en el tratamiento
# -----------------------------------------------------------------------------

grafica_discontinuidad <- hogares_consumo %>%
  # Creamos bins de 5km y calculamos la proporción de mita en cada bin
  # Esto suaviza la gráfica y la hace más legible
  mutate(bin = cut(d_bnd, breaks = seq(-100, 100, by = 5),
                   include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(
    mita_media = mean(pothuan_mita),
    d_bnd_med  = mean(d_bnd),
    n          = n()
  ) %>%
  filter(!is.na(bin)) %>%
  ggplot(aes(x = d_bnd_med, y = mita_media)) +
  geom_point(size = 1.5, color = "gray20") +
  # Línea flexible a cada lado del punto de corte
  geom_smooth(
    data = . %>% filter(d_bnd_med < 0),
    method = "loess", se = FALSE,
    color = "gray10", linewidth = 0.7
  ) +
  geom_smooth(
    data = . %>% filter(d_bnd_med >= 0),
    method = "loess", se = FALSE,
    color = "gray10", linewidth = 0.7
  ) +
  geom_vline(
    xintercept = 0,
    color      = "gray40",
    linewidth  = 0.4,
    linetype   = "dashed"
  ) +
  coord_cartesian(xlim = c(-100, 100)) +
  scale_x_continuous(breaks = seq(-100, 100, 50)) +
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     limits = c(-0.1, 1.1)) +
  labs(
    title    = "Discontinuidad en la probabilidad de tratamiento",
    subtitle = "Negativo: fuera de la mita   |   Positivo: dentro de la mita",
    x        = "Distancia ajustada a la frontera (km)",
    y        = "Probabilidad de mita"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
    axis.title    = element_text(face = "bold"),
    axis.line     = element_line(linewidth = 0.4),
    axis.ticks    = element_line(linewidth = 0.4),
    plot.margin   = margin(20, 25, 15, 25),
    aspect.ratio  = 0.4
  )

ggsave(
  filename = store_file("grafica_discontinuidad.png"),
  plot     = grafica_discontinuidad,
  width    = 10,
  height   = 4,
  dpi      = 300,
  bg       = "white"
)

# -----------------------------------------------------------------------------
# 5. Continuidad local de covariables
# -----------------------------------------------------------------------------

# b.Graficas de covariables

# Filtramos a ventana ±25 km para cada base
cuad_25    <- cuadrantes_geo  %>% filter(abs(d_bnd) <= 25)
hog_25     <- hogares_consumo %>% filter(abs(d_bnd) <= 25)
dist_25    <- distrito_t1572  %>% filter(abs(d_bnd) <= 25)

estilizar_rdplot <- function(rdplot_obj, titulo) {
  rdplot_obj +
    geom_vline(
      xintercept = 0,
      linetype   = "dashed",
      color      = "gray40",
      linewidth  = 0.4
    ) +
    labs(title = titulo) +
    theme_classic(base_size = 11) +
    theme(
      plot.title  = element_text(face = "bold", hjust = 0.5, size = 12),
      axis.title  = element_text(face = "bold"),
      axis.line   = element_line(linewidth = 0.4),
      axis.ticks  = element_line(linewidth = 0.4),
      plot.margin = margin(10, 15, 10, 15)
    )
}

# Elevación
p1_raw <- rdrobust::rdplot(
  y = cuad_25$elev,
  x = cuad_25$d_bnd,
  c = 0,
  p = 1,
  col.lines = "steelblue",
  col.dots  = "black",
  x.label = "Distancia a la frontera (km)",
  y.label = "Elevación (m)",
  title   = ""
)$rdplot


# Pendiente
p2_raw <- rdrobust::rdplot(
  y = cuad_25$slope,
  x = cuad_25$d_bnd,
  c = 0,
  p = 1,
  col.lines = "steelblue",
  col.dots  = "black",
  x.label = "Distancia a la frontera (km)",
  y.label = "Pendiente (°)",
  title   = ""
)$rdplot


# Quechua
p3_raw <- rdrobust::rdplot(
  y = hog_25$QUE,
  x = hog_25$d_bnd,
  c = 0,
  p = 1,
  col.lines = "steelblue",
  col.dots  = "black",
  x.label = "Distancia a la frontera (km)",
  y.label = "P(Quechua)",
  title   = ""
)$rdplot


# Tributación
p4_raw <- rdrobust::rdplot(
  y = dist_25$ltrib_rate,
  x = dist_25$d_bnd,
  c = 0,
  p = 1,
  col.lines = "steelblue",
  col.dots  = "black",
  x.label = "Distancia a la frontera (km)",
  y.label = "Log tarifa tributaria",
  title   = ""
)$rdplot

p1 <- estilizar_rdplot(p1_raw, "Elevación promedio")
p2 <- estilizar_rdplot(p2_raw, "Pendiente del terreno")
p3 <- estilizar_rdplot(p3_raw, "Probabilidad de hablar quechua")
p4 <- estilizar_rdplot(p4_raw, "Tarifa tributaria en 1572")

panel_covariables <- (p1 + p2) / (p3 + p4)
panel_covariables

ggsave(
  filename = store_file("panel_covariables.png"),
  plot     = panel_covariables,
  width    = 12,
  height   = 8,
  dpi      = 300,
  bg       = "white"
)

# c.

estimar_rd_cl <- function(data, y_var, ventana, cluster_var) {
  df <- data %>%
    filter(abs(d_bnd) <= ventana) %>%
    mutate(mita_x_dbnd = pothuan_mita * d_bnd)
  
  modelo <- lm(
    as.formula(paste(y_var, "~ pothuan_mita + d_bnd + mita_x_dbnd")),
    data = df
  )
  
  # Clustering
  ct <- coeftest(modelo,
                 vcov = vcovCL(modelo,
                               cluster = as.formula(paste0("~", cluster_var)),
                               data    = df))
  
  tibble(
    ventana = ventana,
    coef    = round(ct["pothuan_mita", "Estimate"], 3),
    se      = round(ct["pothuan_mita", "Std. Error"], 3),
    pval    = ct["pothuan_mita", "Pr(>|t|)"],
    sig     = case_when(
      pval < 0.01 ~ "***",
      pval < 0.05 ~ "**",
      pval < 0.10 ~ "*",
      TRUE        ~ ""
    ),
    n = nrow(df)
  )
}

ventanas <- c(100, 50, 25)

# Elevación — cluster por grid_id
res_elev <- map_dfr(ventanas, ~ estimar_rd_cl(
  cuadrantes_geo, "elev", .x, "grid_id")) %>%
  mutate(covariable = "Elevación promedio")

# Pendiente — cluster por grid_id
res_slope <- map_dfr(ventanas, ~ estimar_rd_cl(
  cuadrantes_geo, "slope", .x, "grid_id")) %>%
  mutate(covariable = "Pendiente del terreno")

# Quechua — cluster por ubigeo
res_que <- map_dfr(ventanas, ~ estimar_rd_cl(
  hogares_consumo, "QUE", .x, "ubigeo")) %>%
  mutate(covariable = "P(Quechua)")

# Tarifa tributaria — cluster por ccdd
res_trib <- map_dfr(ventanas, ~ estimar_rd_cl(
  distrito_t1572, "ltrib_rate", .x, "ccdd")) %>%
  mutate(covariable = "Tarifa tributaria 1572")

# Tabla
tabla_balance_cl <- bind_rows(res_elev, res_slope, res_que, res_trib) %>%
  mutate(coef_se = paste0(coef, sig, " (", se, ")")) %>%
  select(covariable, ventana, coef_se) %>%
  pivot_wider(
    names_from  = ventana,
    values_from = coef_se,
    names_prefix = "Ventana "
  ) %>%
  rename(
    "Covariable"    = covariable,
    "$\\pm$100 km"  = "Ventana 100",
    "$\\pm$50 km"   = "Ventana 50",
    "$\\pm$25 km"   = "Ventana 25"
  )

print(xtable(tabla_balance_cl,
             caption = "Continuidad local de covariables — Coeficiente $\\hat{\\gamma}$ y error estándar clusterizado",
             label   = "tab:balance",
             digits  = c(0, 0, 0, 0, 0)),
      include.rownames       = FALSE,
      booktabs               = TRUE,
      sanitize.text.function = identity)

# -----------------------------------------------------------------------------
# 6. Estimación del efecto causal
# -----------------------------------------------------------------------------

# Filtrar ventana ±25 km y crear interacciones
hog_25 <- hogares_consumo %>%
  filter(abs(d_bnd) <= 25) %>%
  mutate(
    mita_x_dbnd  = pothuan_mita * d_bnd,
    d_bnd2       = d_bnd^2,
    mita_x_dbnd2 = pothuan_mita * d_bnd^2
  )

nin_25 <- ninos_salud %>%
  filter(abs(d_bnd) <= 25) %>%
  mutate(
    mita_x_dbnd  = pothuan_mita * d_bnd,
    d_bnd2       = d_bnd^2,
    mita_x_dbnd2 = pothuan_mita * d_bnd^2
  )


# 6a. Polinomio grado 1

m_consumo_p1 <- lm(lhhequiv ~ pothuan_mita + d_bnd + mita_x_dbnd,
                   data = hog_25)
m_desnu_p1   <- lm(desnu ~ pothuan_mita + d_bnd + mita_x_dbnd,
                   data = nin_25)

ct_consumo_p1 <- coeftest(m_consumo_p1,
                          vcov = vcovCL(m_consumo_p1,
                                        cluster = ~ubigeo,
                                        data    = hog_25))
ct_desnu_p1   <- coeftest(m_desnu_p1,
                          vcov = vcovCL(m_desnu_p1,
                                        cluster = ~ubigeo,
                                        data    = nin_25))

# 6b. Polinomio grado 2

m_consumo_p2 <- lm(lhhequiv ~ pothuan_mita + d_bnd + d_bnd2 +
                     mita_x_dbnd + mita_x_dbnd2,
                   data = hog_25)
m_desnu_p2   <- lm(desnu ~ pothuan_mita + d_bnd + d_bnd2 +
                     mita_x_dbnd + mita_x_dbnd2,
                   data = nin_25)

ct_consumo_p2 <- coeftest(m_consumo_p2,
                          vcov = vcovCL(m_consumo_p2,
                                        cluster = ~ubigeo,
                                        data    = hog_25))
ct_desnu_p2   <- coeftest(m_desnu_p2,
                          vcov = vcovCL(m_desnu_p2,
                                        cluster = ~ubigeo,
                                        data    = nin_25))


# 6c. Polinomio bidimensional con feols

m_consumo_xy <- feols(
  lhhequiv ~ pothuan_mita * (x + y),
  subset  = ~ d_bnd >= -25 & d_bnd <= 25,
  data    = hogares_consumo,
  cluster = ~ubigeo
)

m_desnu_xy <- feols(
  desnu ~ pothuan_mita * (x + y),
  subset  = ~ d_bnd >= -25 & d_bnd <= 25,
  data    = ninos_salud,
  cluster = ~ubigeo
)


# 6d. Tabla comparativa


# Función para extraer de coeftest (grado 1 y 2)
extraer_cl <- function(ct, n_obs, nombre) {
  est  <- round(ct["pothuan_mita", "Estimate"], 3)
  se   <- round(ct["pothuan_mita", "Std. Error"], 3)
  pval <- ct["pothuan_mita", "Pr(>|t|)"]
  sig  <- case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.10 ~ "*",
    TRUE        ~ ""
  )
  tibble(
    Especificación = nombre,
    `Coef (SE)`    = paste0(est, sig, " (", se, ")"),
    N              = n_obs
  )
}

# Función para extraer de feols (bidimensional)
extraer_feols <- function(modelo, n_obs, nombre) {
  ct   <- coeftable(modelo)
  est  <- round(ct["pothuan_mita", "Estimate"], 3)
  se   <- round(ct["pothuan_mita", "Std. Error"], 3)
  pval <- ct["pothuan_mita", "Pr(>|t|)"]
  sig  <- case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.10 ~ "*",
    TRUE        ~ ""
  )
  tibble(
    Especificación = nombre,
    `Coef (SE)`    = paste0(est, sig, " (", se, ")"),
    N              = n_obs
  )
}

# Consumo
tabla_consumo_cl <- bind_rows(
  extraer_cl(ct_consumo_p1, nrow(hog_25), "Polinomio grado 1"),
  extraer_cl(ct_consumo_p2, nrow(hog_25), "Polinomio grado 2"),
  extraer_feols(m_consumo_xy, 580,        "Polinomio bidimensional")
) %>% rename(`Log consumo` = `Coef (SE)`, N_consumo = N)

# Desnutrición
tabla_desnu_cl <- bind_rows(
  extraer_cl(ct_desnu_p1, nrow(nin_25),   "Polinomio grado 1"),
  extraer_cl(ct_desnu_p2, nrow(nin_25),   "Polinomio grado 2"),
  extraer_feols(m_desnu_xy, 53693,        "Polinomio bidimensional")
) %>% rename(`Desnutrición` = `Coef (SE)`, N_desnu = N)

# Tabla combinada
tabla_efectos_cl <- tabla_consumo_cl %>%
  left_join(tabla_desnu_cl, by = "Especificación")

print(xtable(tabla_efectos_cl,
             caption = "Efecto causal de la mita — Errores estándar clusterizados por distrito ($\\pm$25 km)",
             label   = "tab:efectos",
             digits  = c(0,0,0,0,0,0)),
      include.rownames       = FALSE,
      booktabs               = TRUE,
      sanitize.text.function = identity)

# -----------------------------------------------------------------------------
# 7. Robustez al ancho de banda 
# -----------------------------------------------------------------------------

estimar_ventana_completa <- function(data_hog, data_nin, ventana) {
  
  hog_v <- data_hog %>%
    filter(abs(d_bnd) <= ventana) %>%
    mutate(
      mita_x_dbnd  = pothuan_mita * d_bnd,
      d_bnd2       = d_bnd^2,
      mita_x_dbnd2 = pothuan_mita * d_bnd^2
    )
  
  nin_v <- data_nin %>%
    filter(abs(d_bnd) <= ventana) %>%
    mutate(
      mita_x_dbnd  = pothuan_mita * d_bnd,
      d_bnd2       = d_bnd^2,
      mita_x_dbnd2 = pothuan_mita * d_bnd^2
    )
  
  # Grado 1
  m_con_p1 <- lm(lhhequiv ~ pothuan_mita + d_bnd + mita_x_dbnd, data = hog_v)
  m_des_p1 <- lm(desnu    ~ pothuan_mita + d_bnd + mita_x_dbnd, data = nin_v)
  
  ct_con_p1 <- coeftest(m_con_p1,
                        vcov = vcovCL(m_con_p1, cluster = ~ubigeo, data = hog_v))
  ct_des_p1 <- coeftest(m_des_p1,
                        vcov = vcovCL(m_des_p1, cluster = ~ubigeo, data = nin_v))
  
  # Grado 2
  m_con_p2 <- lm(lhhequiv ~ pothuan_mita + d_bnd + d_bnd2 +
                   mita_x_dbnd + mita_x_dbnd2, data = hog_v)
  m_des_p2 <- lm(desnu    ~ pothuan_mita + d_bnd + d_bnd2 +
                   mita_x_dbnd + mita_x_dbnd2, data = nin_v)
  
  ct_con_p2 <- coeftest(m_con_p2,
                        vcov = vcovCL(m_con_p2, cluster = ~ubigeo, data = hog_v))
  ct_des_p2 <- coeftest(m_des_p2,
                        vcov = vcovCL(m_des_p2, cluster = ~ubigeo, data = nin_v))
  
  # Bidimensional con feols
  m_con_xy <- feols(
    lhhequiv ~ pothuan_mita * (x + y),
    subset  = as.formula(paste0("~ d_bnd >= -", ventana, " & d_bnd <= ", ventana)),
    data    = data_hog,
    cluster = ~ubigeo
  )
  
  m_des_xy <- feols(
    desnu ~ pothuan_mita * (x + y),
    subset  = as.formula(paste0("~ d_bnd >= -", ventana, " & d_bnd <= ", ventana)),
    data    = data_nin,
    cluster = ~ubigeo
  )
  
  # Función auxiliar para coeftest
  fmt_cl <- function(ct) {
    est  <- round(ct["pothuan_mita", "Estimate"], 3)
    se   <- round(ct["pothuan_mita", "Std. Error"], 3)
    pval <- ct["pothuan_mita", "Pr(>|t|)"]
    sig  <- case_when(
      pval < 0.01 ~ "***",
      pval < 0.05 ~ "**",
      pval < 0.10 ~ "*",
      TRUE        ~ ""
    )
    paste0(est, sig, " (", se, ")")
  }
  
  # Función auxiliar para feols
  fmt_feols <- function(modelo) {
    ct   <- coeftable(modelo)
    est  <- round(ct["pothuan_mita", "Estimate"], 3)
    se   <- round(ct["pothuan_mita", "Std. Error"], 3)
    pval <- ct["pothuan_mita", "Pr(>|t|)"]
    sig  <- case_when(
      pval < 0.01 ~ "***",
      pval < 0.05 ~ "**",
      pval < 0.10 ~ "*",
      TRUE        ~ ""
    )
    paste0(est, sig, " (", se, ")")
  }
  
  tibble(
    Ventana           = paste0("$\\pm$", ventana, " km"),
    `Consumo p1`      = fmt_cl(ct_con_p1),
    `Consumo p2`      = fmt_cl(ct_con_p2),
    `Consumo xy`      = fmt_feols(m_con_xy),
    `Desnutrición p1` = fmt_cl(ct_des_p1),
    `Desnutrición p2` = fmt_cl(ct_des_p2),
    `Desnutrición xy` = fmt_feols(m_des_xy),
    N_hog             = nrow(hog_v),
    N_nin             = nrow(nin_v)
  )
}

# Estimar para las tres ventanas
resultados_ventanas <- bind_rows(
  estimar_ventana_completa(hogares_consumo, ninos_salud, 25),
  estimar_ventana_completa(hogares_consumo, ninos_salud, 50),
  estimar_ventana_completa(hogares_consumo, ninos_salud, 100)
)

print(xtable(resultados_ventanas,
             caption = "Robustez al ancho de banda — Errores clusterizados por distrito",
             label   = "tab:robustez",
             digits  = c(0,0,0,0,0,0,0,0,0,0)),
      include.rownames       = FALSE,
      booktabs               = TRUE,
      sanitize.text.function = identity)