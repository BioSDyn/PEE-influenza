# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 03: Build analytic variables — influenza dataset
# Project "Secihti" 2025
# Created: June 2026 by Imelda Trejo, CCM-UNAM
# Last update: July 21, 2026 by Imelda Trejo
#
# INPUT:
#   data/processed/flu_clean.csv
#   data/processed/population_age_group.csv
#
# OUTPUT:
#   data/processed/calendar.rds
#   data/processed/population_denominator.rds
#   data/processed/population_denominator.csv
#   data/processed/flu_analytic.rds
#   data/processed/flu_analytic.csv
#
# Description:
#   Construye el calendario maestro, el denominador poblacional, y agrega al
#   line-list las variables derivadas necesarias para las series de tiempo:
#     1. epi_week     — semana epidemiológica CDC (de FECHA_SINTOMAS)
#     2. epi_year     — año epidemiológico CDC (de FECHA_SINTOMAS)
#     3. season_label — etiqueta de semana absoluta, ej. "2025-W03"
#     4. flu_season   — etiqueta de temporada, ej. "2024-2025"
#     5. t_season      — semana ordinal dentro de la temporada (1 = SE40)
#     6. t_global      — semana ordinal continua en todo el horizonte
#     7. age_group    — grupo de edad (0-4, 5-19, 20-64, 65+ años)
#
#   NOTA: epi_week/season_label/flu_season/t_season/t_global aquí están
#   anclados a FECHA_SINTOMAS. Los scripts de hospitalización y defunciones
#   deben RECALCULAR estas mismas columnas con fecha_ref = "FECHA_INGRESO" /
#   "FECHA_DEF" respectivamente — no reusar las de aquí directamente.
#
# Season definition (esta versión):
#   SE 40 (inicio oct) → SE 22 (~1 jun) del año siguiente.
#   Registros fuera de esta ventana quedan en NA (fuera de temporada).
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

# ── Libraries ──────────────────────────────────────────────────────────────────
library(dplyr)
library(lubridate)
library(readr)

source("codes/functions_sisver.R")

# ── Config ─────────────────────────────────────────────────────────────────────

input_path <- "data/processed/"
out_path   <- paste0(input_path, "flu_analytic.rds")

season_start_week <- 40
season_end_week   <- 22
min_year          <- 2024
max_year          <- 2026

seasons <- c("2024-2025", "2025-2026")

pop_years_map <- list(
  "2024-2025" = c(2024, 2025),
  "2025-2026" = c(2025, 2026)
)

# ══════════════════════════════════════════════════════════════════════════════
# 1. LOAD
# ══════════════════════════════════════════════════════════════════════════════

data    <- read_csv(paste0(input_path, "flu_clean.csv"), show_col_types = FALSE)
pop_age <- read_csv(paste0(input_path, "population_age_group.csv"), show_col_types = FALSE)

cat("Records loaded:", nrow(data), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 2. CALENDARIO Y DENOMINADOR POBLACIONAL
# ══════════════════════════════════════════════════════════════════════════════

pop_denom <- build_pop_denom(pop_age, pop_years_map)

min_date <- epiweek_to_date(2024, season_start_week)
max_date <- epiweek_to_date(2026, season_end_week)

calendar <- build_epi_calendar(
  min_date, max_date,
  season_start_week, season_end_week,
  min_year, max_year
)

# Verificación de límites de temporada
season_bounds <- calendar |>
  arrange(t_season) |>
  group_by(flu_season) |>
  slice(1, n()) |>
  ungroup() |>
  select(-t_global)

print(season_bounds)

# ══════════════════════════════════════════════════════════════════════════════
# 3. ASIGNAR SEMANA/TEMPORADA (por FECHA_SINTOMAS) Y FILTRAR AL HORIZONTE
# ══════════════════════════════════════════════════════════════════════════════

data <- build_analytic_epi_vars(data, calendar, fecha_ref = "FECHA_SINTOMAS")

cat("\nFlu seasons (antes de filtrar):\n")
print(table(data$flu_season, useNA = "ifany"))
cat("Fuera de temporada (NA):", sum(is.na(data$flu_season)), "de", nrow(data), "\n")

data <- data |>
  filter(flu_season %in% seasons)

cat("Registros dentro de temporada:", nrow(data), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 4. GRUPO DE EDAD
# ══════════════════════════════════════════════════════════════════════════════

data <- data |>
  mutate(age_group = make_age_group(age)) |>
  select(-age)

# ══════════════════════════════════════════════════════════════════════════════
# 5. GUARDAR
# ══════════════════════════════════════════════════════════════════════════════

saveRDS(calendar,  paste0(input_path, "calendar.rds"))
saveRDS(pop_denom, paste0(input_path, "population_denominator.rds"))
saveRDS(data,      out_path)

write_csv(pop_denom, paste0(input_path, "population_denominator.csv"))
write_csv(data,      paste0(input_path, "flu_analytic.csv"))

