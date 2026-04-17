# Taller 2 — Econometría Avanzada 2026-1

Replicación y análisis de tres ejercicios empíricos usando regresión discontinua y variables instrumentales.

## Estructura del proyecto

```
├── Taller2.Rproj
├── scripts/
│   ├── 00_Config.R        # Configuración base y paquetes
│   ├── Script_01.R        # Ejercicio 1: RD — Mita colonial en Perú
│   ├── Script_02.R        # Ejercicio 2: RD — Elecciones cerradas en EEUU
│   └── Script_03.R        # Ejercicio 3: IV — Televisión y apoyo al régimen
├── data/
│   ├── punto1/            # Bases de datos Ejercicio 1
│   ├── punto2/            # Bases de datos Ejercicio 2
│   └── punto3/            # Bases de datos Ejercicio 3
├── stores/                # Outputs: gráficas y tablas
└── README.md
```

## Ejercicios

**Ejercicio 1:** Estimación del efecto causal de la mita colonial sobre el consumo de hogares y desnutrición infantil en Perú usando regresión discontinua (Dell, 2010).

**Ejercicio 2:** Análisis del impacto de nominar candidatas mujeres sobre resultados electorales usando RD en elecciones cerradas (Bucchianeri, 2018; Marshall, 2024).

**Ejercicio 3:** Estimación del efecto de la exposición a televisión extranjera sobre el apoyo a un régimen autoritario usando variables instrumentales.

## Requerimientos

R 4.x con los siguientes paquetes: `tidyverse`, `rdrobust`, `fixest`, `AER`, `sandwich`, `lmtest`, `xtable`, `skimr`, `patchwork`, `rio`, `here`.

## Reproducibilidad

Abrir `Taller2.Rproj` en RStudio y ejecutar los scripts en orden comenzando por `00_Config.R`. Todos los outputs se guardan automáticamente en `stores/`.
