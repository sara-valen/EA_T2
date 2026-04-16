# =============================================================================
# Script_03: Ejercicio 3
# =============================================================================
# -----------------------------------------------------------------------------
# 0. Cargar bases de datos 
# -----------------------------------------------------------------------------

# Cargar configuración base

source(here::here("scripts", "00_Config.R"))

valmoria   <- import(here::here("data", "Valmoria1989.dta"))

# -----------------------------------------------------------------------------
# 1. Exploración inicial de los datos y estadísticas descriptivas
# -----------------------------------------------------------------------------

# Examinar la estructura de las bases de datos

glimpse(valmoria)
skim(valmoria)

# Estadísticas descriptivas

# -----------------------------------------------------------------------------
# 1. Estadísticas descriptivas
# -----------------------------------------------------------------------------

desc_general <- valmoria %>%
  select(Y, D, Z, age, female, educ, urban) %>%
  skim()

desc_general

# Descriptivas por acceso a la señal
valmoria %>%
  group_by(Z) %>%
  summarise(
    n            = n(),
    apoyo_regimen = mean(Y),
    ve_tv        = mean(D),
    edad_media   = mean(age),
    pct_mujer    = mean(female),
    educ_media   = mean(educ),
    pct_urbano   = mean(urban)
  ) %>%
  mutate(Z = ifelse(Z == 1, "Con acceso (fuera del valle)", "Sin acceso (valle)"))

# Descriptivas por ver TV
valmoria %>%
  group_by(D) %>%
  summarise(
    n             = n(),
    apoyo_regimen = mean(Y),
    acceso_senal  = mean(Z),
    edad_media    = mean(age),
    pct_mujer     = mean(female),
    educ_media    = mean(educ),
    pct_urbano    = mean(urban)
  ) %>%
  mutate(D = ifelse(D == 1, "Ve TV extranjera", "No ve TV extranjera"))

# Tabla descriptiva para LaTeX
tabla_desc <- bind_rows(
  valmoria %>% summarise(
    Variable = "Apoyo al régimen (Y)",
    N = n(), Media = mean(Y), SD = sd(Y),
    Min = min(Y), Max = max(Y)
  ),
  valmoria %>% summarise(
    Variable = "Ve TV extranjera (D)",
    N = n(), Media = mean(D), SD = sd(D),
    Min = min(D), Max = max(D)
  ),
  valmoria %>% summarise(
    Variable = "Acceso a la señal (Z)",
    N = n(), Media = mean(Z), SD = sd(Z),
    Min = min(Z), Max = max(Z)
  ),
  valmoria %>% summarise(
    Variable = "Edad",
    N = n(), Media = mean(age), SD = sd(age),
    Min = min(age), Max = max(age)
  ),
  valmoria %>% summarise(
    Variable = "Mujer",
    N = n(), Media = mean(female), SD = sd(female),
    Min = min(female), Max = max(female)
  ),
  valmoria %>% summarise(
    Variable = "Educación (años)",
    N = n(), Media = mean(educ), SD = sd(educ),
    Min = min(educ), Max = max(educ)
  ),
  valmoria %>% summarise(
    Variable = "Urbano",
    N = n(), Media = mean(urban), SD = sd(urban),
    Min = min(urban), Max = max(urban)
  )
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

print(xtable(tabla_desc,
             caption = "Estadísticas descriptivas — Valmoria 1989",
             label   = "tab:desc_val"),
      include.rownames       = FALSE,
      booktabs               = TRUE,
      sanitize.text.function = identity)