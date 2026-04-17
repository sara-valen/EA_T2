# =============================================================================
# Script_03: Ejercicio 3 — Valmoria
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Cargar bases de datos 
# -----------------------------------------------------------------------------

# Cargar configuración base

source(here::here("scripts", "00_Config.R"))

valmoria   <- import(here::here("data", "Valmoria1989.dta"))

# -----------------------------------------------------------------------------
# 1. Estadísticas descriptivas
# -----------------------------------------------------------------------------

# Estructura general
glimpse(valmoria)

# Descriptivas generales
desc_general <- valmoria %>%
  select(Y, D, Z, age, female, educ, urban) %>%
  skim()

desc_general

# Descriptivas por acceso a la señal (Z)
valmoria %>%
  group_by(Z) %>%
  summarise(
    n             = n(),
    apoyo_regimen = round(mean(Y), 3),
    ve_tv         = round(mean(D), 3),
    edad_media    = round(mean(age), 3),
    pct_mujer     = round(mean(female), 3),
    educ_media    = round(mean(educ), 3),
    pct_urbano    = round(mean(urban), 3)
  ) %>%
  mutate(Z = ifelse(Z == 1,
                    "Con acceso (fuera del valle)",
                    "Sin acceso (valle)"))

# Descriptivas por ver TV (D)
valmoria %>%
  group_by(D) %>%
  summarise(
    n             = n(),
    apoyo_regimen = round(mean(Y), 3),
    acceso_senal  = round(mean(Z), 3),
    edad_media    = round(mean(age), 3),
    pct_mujer     = round(mean(female), 3),
    educ_media    = round(mean(educ), 3),
    pct_urbano    = round(mean(urban), 3)
  ) %>%
  mutate(D = ifelse(D == 1,
                    "Ve TV extranjera",
                    "No ve TV extranjera"))

# Tabla descriptiva general para LaTeX
tabla_desc_val <- bind_rows(
  valmoria %>% summarise(Variable = "Apoyo al régimen (Y)",
                         N = n(), Media = mean(Y), SD = sd(Y), Min = min(Y), Max = max(Y)),
  valmoria %>% summarise(Variable = "Ve TV extranjera (D)",
                         N = n(), Media = mean(D), SD = sd(D), Min = min(D), Max = max(D)),
  valmoria %>% summarise(Variable = "Acceso a la señal (Z)",
                         N = n(), Media = mean(Z), SD = sd(Z), Min = min(Z), Max = max(Z)),
  valmoria %>% summarise(Variable = "Edad",
                         N = n(), Media = mean(age), SD = sd(age), Min = min(age), Max = max(age)),
  valmoria %>% summarise(Variable = "Mujer",
                         N = n(), Media = mean(female), SD = sd(female), Min = min(female), Max = max(female)),
  valmoria %>% summarise(Variable = "Educación (años)",
                         N = n(), Media = mean(educ), SD = sd(educ), Min = min(educ), Max = max(educ)),
  valmoria %>% summarise(Variable = "Urbano",
                         N = n(), Media = mean(urban), SD = sd(urban), Min = min(urban), Max = max(urban))
) %>%
  mutate(
    N    = as.integer(N),
    across(where(is.numeric), ~ round(.x, 3))
  )

print(xtable(tabla_desc_val,
             caption = "Estadísticas descriptivas — Valmoria 1989",
             label   = "tab:desc_val",
             digits  = c(0, 0, 0, 3, 3, 3, 3)),
      include.rownames       = FALSE,
      booktabs               = TRUE,
      sanitize.text.function = identity)

# Tabla por grupo (con vs sin acceso)
tabla_por_grupo_val <- valmoria %>%
  mutate(Grupo = ifelse(Z == 1, "Con acceso", "Sin acceso")) %>%
  group_by(Grupo) %>%
  summarise(
    N              = n(),
    `Apoyo (media)`= round(mean(Y), 3),
    `Ve TV (media)`= round(mean(D), 3),
    `Edad (media)` = round(mean(age), 3),
    `Educ (media)` = round(mean(educ), 3),
    `Urbano (%)`   = round(mean(urban), 3)
  )

print(xtable(tabla_por_grupo_val,
             caption = "Estadísticas descriptivas por acceso a la señal",
             label   = "tab:desc_grupo_val"),
      include.rownames       = FALSE,
      booktabs               = TRUE,
      sanitize.text.function = identity)

# -----------------------------------------------------------------------------
# 2. Estimación por MCO
# -----------------------------------------------------------------------------

m_mco <- lm(Y ~ D + age + female + educ + urban,
            data = valmoria)

# Errores clusterizados por región
ct_mco <- coeftest(m_mco,
                   vcov = vcovCL(m_mco,
                                 cluster = ~region,
                                 data    = valmoria))
ct_mco

# -----------------------------------------------------------------------------
# 3. Estrategia de Variables Instrumentales
# -----------------------------------------------------------------------------

# 3a. Primera etapa
# Z (acceso a la señal) como instrumento de D (ver TV extranjera)
m_primera_etapa <- lm(D ~ Z + age + female + educ + urban,
                      data = valmoria)

ct_primera_etapa <- coeftest(m_primera_etapa,
                             vcov = vcovCL(m_primera_etapa,
                                           cluster = ~region,
                                           data    = valmoria))
ct_primera_etapa

# Estadístico F de la primera etapa — relevancia del instrumento
summary(m_primera_etapa)$fstatistic

# 3b. Estimación por Variables Instrumentales (2SLS)
m_iv <- ivreg(
  Y ~ D + age + female + educ + urban |
    Z + age + female + educ + urban,
  data = valmoria
)

ct_iv <- coeftest(m_iv,
                  vcov = vcovCL(m_iv,
                                cluster = ~region,
                                data    = valmoria))
ct_iv

# -----------------------------------------------------------------------------
# Tabla comparativa MCO vs IV
# -----------------------------------------------------------------------------

extraer_coef <- function(ct, nombre) {
  est  <- round(ct["D", "Estimate"], 3)
  se   <- round(ct["D", "Std. Error"], 3)
  pval <- ct["D", "Pr(>|t|)"]
  sig  <- case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.10 ~ "*",
    TRUE        ~ ""
  )
  tibble(
    Estimador    = nombre,
    `Coef (SE)`  = paste0(est, sig, " (", se, ")")
  )
}

# Primera etapa
est_z  <- round(ct_primera_etapa["Z", "Estimate"], 3)
se_z   <- round(ct_primera_etapa["Z", "Std. Error"], 3)
pval_z <- ct_primera_etapa["Z", "Pr(>|t|)"]
sig_z  <- "***"

tabla_resultados <- bind_rows(
  tibble(
    Estimador   = "Primera etapa (D ~ Z)",
    `Coef (SE)` = paste0(est_z, sig_z, " (", se_z, ")")
  ),
  extraer_coef(ct_mco, "MCO"),
  extraer_coef(ct_iv,  "IV (2SLS)")
)

print(xtable(tabla_resultados,
             caption = "Resultados — Primera etapa, MCO e IV. Variable dependiente: apoyo al régimen (Y)",
             label   = "tab:resultados_iv"),
      include.rownames       = FALSE,
      booktabs               = TRUE,
      sanitize.text.function = identity)


# Estimador de Wald
E_Y_Z1 <- mean(valmoria$Y[valmoria$Z == 1])
E_Y_Z0 <- mean(valmoria$Y[valmoria$Z == 0])
E_D_Z1 <- mean(valmoria$D[valmoria$Z == 1])
E_D_Z0 <- mean(valmoria$D[valmoria$Z == 0])

wald <- (E_Y_Z1 - E_Y_Z0) / (E_D_Z1 - E_D_Z0)

cat("Numerador (efecto reducido):", round(E_Y_Z1 - E_Y_Z0, 3), "\n")
cat("Denominador (primera etapa):", round(E_D_Z1 - E_D_Z0, 3), "\n")
cat("Estimador de Wald:", round(wald, 3), "\n")
