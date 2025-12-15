#Análisis de homicidios México
Microdatos de defunciones INEGI 2024
# Homicidios y migración interna en México

**Descripción:**  
Exploración preliminar de datos de mortalidad para analizar la representación de migrantes internos entre las víctimas de homicidio y otras causas de muerte en México.

## Estructura del repositorio

- **data/**: datos utilizados en el análisis (o instrucciones para obtenerlos).
- **scripts/**: código en R para limpieza, análisis y visualización.
- **figures/**: gráficos generados a partir del análisis.
- **docs/**: notas, borradores o materiales de apoyo (si aplica).

## Qué se hizo

- **Exploración inicial** de la representación de migrantes internos entre las víctimas de homicidio por estado.
- **Comparación por sexo** de la proporción de migrantes internos entre víctimas de homicidio en distintos estados.
- **Análisis preliminar de causas de muerte** (homicidio, accidente, enfermedad, etc.) entre migrantes internos y no migrantes internos, incluyendo pruebas estadísticas básicas.

## Hallazgos preliminares

- La representación de migrantes internos entre las víctimas de homicidio varía notablemente entre estados.
- En varios estados, los hombres migrantes internos concentran una proporción mayor entre las víctimas de homicidio que las mujeres migrantes.
- Las causas de muerte muestran diferencias de composición entre migrantes internos y no migrantes internos, respaldadas por pruebas estadísticas (chi-cuadrada).

## Alcances y límites

Este análisis es **exploratorio y preliminar**.  
Se identificaron vetas analíticas relevantes, pero:

- No se han agotado todas las dimensiones posibles de análisis.
- Falta profundizar en la revisión metodológica y en la validación de resultados.
- No se formulan conclusiones causales ni se consideran estos resultados como definitivos o publicables sin una revisión adicional.

## Reproducibilidad

1. Clonar este repositorio.
2. Instalar los paquetes de R utilizados (dplyr, ggplot2, etc.).
3. Ejecutar los scripts en `scripts/` en el orden indicado
