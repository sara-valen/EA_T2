# =============================================================================
# Script_02: Ejercicio 2
# =============================================================================
# -----------------------------------------------------------------------------
# 0. Cargar bases de datos 
# -----------------------------------------------------------------------------

# Cargar configuración base

source(here::here("scripts", "00_Config.R"))

female_politics <- import(here::here("data", "female_politics.dta"))

# -----------------------------------------------------------------------------
# 1. Exploración inicial de los datos y graficar variable de focalización
# -----------------------------------------------------------------------------

# Examinar la estructura de la base de datos

