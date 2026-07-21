# Fase 1 — Análisis Exploratorio: Dinámica Estacional de Influenza en México

## Objetivo

Esta fase tiene como propósito generar un entendimiento sólido de la dinámica de transmisión de influenza estacional en México, mediante el análisis de series temporales históricas de vigilancia epidemiológica. Los resultados de esta fase — datos procesados, patrones estacionales, y parámetros estimados — son el insumo directo para la Fase 2 (modelado dinámico).

## Contexto

La influenza es un patógeno respiratorio con alto potencial pandémico debido a su capacidad de cambio antigénico. Tras la pandemia de H1N1 en 2009, el virus H5N1 ha generado preocupación reciente por infecciones humanas en América del Norte, subrayando la importancia de la preparación y respuesta pandémica ante el riesgo de variantes con transmisión sostenida.

Aunque México implementa anualmente una campaña de vacunación gratuita dirigida principalmente a grupos prioritarios (menores de 5 años y adultos de 65 años y más), la transmisión de influenza se sostiene año con año, con ondas epidémicas bien marcadas.

## Qué se analiza en esta fase

- **Series temporales de vigilancia (SISVER)** para las temporadas 2024–2025 y 2025–2026: casos sintomáticos, hospitalizados y defunciones, estratificados por grupo de edad y subtipo viral.
- **Coberturas de vacunación** disponibles para 2022–2024.
- **Parámetros epidemiológicos** reportados en la literatura para influenza estacional.
- **Matrices de contacto** relevantes para la población mexicana.

## Por qué esto importa para la Fase 2

Este análisis busca generar el conjunto de datos y el entendimiento de patrones (estacionalidad, magnitud de ondas epidémicas, heterogeneidad por edad/subtipo) que informará las decisiones de estructura y calibración del modelo de [Fase 2 — Modelado Dinámico](../phase2_dynamical_modeling), usado para explorar escenarios de influenza estacional bajo distintas estrategias de vacunación.

## Estructura de esta fase

​```
phase1_exploratory/
├── data/
│   ├── raw/          # datos SISVER y coberturas, sin procesar, que se pueden obtenes y descargar libremente (ver documentación)
│   └── processed/    # datos limpios, listos para análisis
├── codes/            # funciones de limpieza y procesamiento reutilizables, exploración y visualización 
└── figures/          # gráficas de estacionalidad y patrones por edad/subtipo
​```
