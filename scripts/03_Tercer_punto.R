# =============================================================================
# Script_03: Ejercicio 3
# =============================================================================
# -----------------------------------------------------------------------------
# 0. Cargar bases de datos 
# -----------------------------------------------------------------------------

# Cargar configuración base

source(here("scripts", "00_Config.R"))

valmoria   <- import(here::here("data", "Valmoria1989.dta"))

# -----------------------------------------------------------------------------
# 1. Exploración inicial de los datos y estadísticas descriptivas
# -----------------------------------------------------------------------------

# Examinar la estructura de la base de datos

# Estadísticas descriptivas
