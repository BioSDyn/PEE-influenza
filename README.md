# PEE-influenza

Código y datos limpios para el desarrollo de modelos epidemiológicos compartimentales de influenza estacional en México.

Entregable del proyecto de investigación **PEE-2025-G-293** — *Evaluación del impacto potencial de nuevas variantes de influenza mediante modelos matemáticos: implicaciones en la toma de decisiones para la preparación y respuesta pandémica*.

## Estructura del proyecto

El trabajo está dividido en dos fases secuenciales:

### [Fase 1 — Análisis Exploratorio](./phase1_exploratory)
Análisis de series temporales históricas de vigilancia epidemiológica (SISVER), coberturas de vacunación, parámetros epidemiológicos y matrices de contacto. El objetivo es generar un entendimiento sólido de la dinámica estacional de influenza en México — insumo directo para el diseño del modelo de la Fase 2.

### [Fase 2 — Modelado Dinámico](./phase2_dynamical_modeling)
Desarrollo de un modelo mecanístico determinista, calibrado con los datos y patrones identificados en la Fase 1, para explorar escenarios de influenza estacional bajo diferentes estrategias de vacunación en la población mexicana.

## Datos

Los datos utilizados provienen del sistema de vigilancia **SISVER** para las temporadas 2024–2025 y 2025–2026 (casos sintomáticos, hospitalizados y defunciones, estratificados por grupo de edad y subtipo viral), coberturas de vacunación 2022–2024, y parámetros epidemiológicos y matrices de contacto reportados en la literatura.

## Cómo navegar este repositorio

- Empieza por el README de cada fase (`phase1_exploratory/README.md`, `phase2_dynamical_modeling/README.md`) para el detalle de objetivos, datos y estructura interna.
- Los notebooks dentro de cada fase están numerados en el orden sugerido de ejecución.
- Las funciones reutilizables (limpieza, procesamiento, modelado) viven en `src/` de cada fase, separadas de los notebooks exploratorios.

## Instalación

​```bash
git clone https://github.com/BioSDyn/PEE-influenza.git
cd PEE-influenza
# instrucciones de entorno (pendiente: environment.yml / requirements.txt)
​```

## Citación / Reconocimiento

Este trabajo es un entregable del proyecto **PEE-2025-G-293**. Si utilizas estos datos o código, por favor referencia el proyecto correspondiente.
