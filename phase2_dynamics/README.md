# Fase 2 — Modelado Dinámico: Influenza Estacional en México

## Objetivo

Esta fase tiene como propósito desarrollar y documentar un **modelo mecanístico determinista** para estudiar la dinámica de transmisión de influenza estacional en México.

El modelo integra la información epidemiológica obtenida en la Fase 1 con una estructura dinámica que incorpora la **heterogeneidad por edad, los patrones de contacto y la vacunación**, permitiendo analizar la evolución de la infección y evaluar diferentes escenarios epidemiológicos.

## Contexto

El modelo busca representar los principales mecanismos que determinan la dinámica de influenza estacional en México, considerando la estructura de la población y los patrones de transmisión entre grupos de edad.

La formulación se basa en un modelo compartimental determinista y utiliza información epidemiológica, demográfica y de vacunación disponible para México.

## Qué se analiza en esta fase

* **Modelo mecanístico determinista** para la transmisión de influenza estacional.
* **Estructura por grupos de edad** para representar diferencias epidemiológicas y patrones de contacto.
* **Matrices de contacto** para describir la interacción entre grupos de edad.
* **Vacunación** mediante la incorporación de cobertura y eficacia de la vacuna.
* **Parámetros epidemiológicos** obtenidos de la literatura y de los análisis realizados en la Fase 1.
* **Simulaciones de escenarios** para estudiar la dinámica de influenza bajo diferentes condiciones de transmisión y vacunación.

## Por qué esto importa

El modelo desarrollado en esta fase constituye la base para estudiar el impacto potencial de diferentes estrategias de vacunación y escenarios epidemiológicos de influenza en México.

Los resultados de esta fase permitirán avanzar hacia la evaluación de intervenciones y escenarios de preparación frente a influenza.

## Relación con la Fase 1

La Fase 2 utiliza los resultados del análisis exploratorio desarrollado en la **[Fase 1 — Análisis Exploratorio](https://github.com/BioSDyn/PEE-influenza/tree/main/phase1_exploratory)**, incluyendo los patrones estacionales, la distribución por edad y los parámetros epidemiológicos identificados en los datos de vigilancia.

## Estructura de esta fase

```text
phase2_dynamics/

├── code/             # implementación y análisis del modelo

├── data/             # datos e insumos utilizados por el modelo

└── figures/          # resultados y visualizaciones
```

## Proyecto

Esta fase forma parte del proyecto **PEE-influenza — Evaluación del impacto potencial de nuevas variantes de influenza mediante modelos matemáticos**.

[Repositorio principal](https://github.com/BioSDyn/PEE-influenza)

