# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 01: Population data — CONAPO projections
# Project supported by "Secihti", 2025
# Last update: July 21 2026 by Imelda Trejo
#
# Data Source: CONAPO Proyecciones de Población 2025
#   https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico
#
# INPUT:
#   data/raw/0_Pob_Inicio_1950_2070.xlsx
#
# OUTPUT:
#   data/processed/population_age_group.csv
#
# Description:
#   Reads CONAPO national population projections (CVE_GEO == 0),
#   filters analysis years, and aggregates by age_group (sex-collapsed).
#
# CONAPO file structure:
#   CVE_GEO  — geographic code (0 = national)
#   AÑO      — year
#   EDAD     — single-year age (0–130)
#   SEXO     — sex code (1 = Hombres, 2 = Mujeres, 3 = Total)
#   POBLACION— population count
#
# Conventions:
#   - Original variables: SPANISH (CONAPO dictionary)
#   - Derived variables:  ENGLISH
#
# Note: sex is not needed for this analysis, so SEXO is used only to exclude
#   the "Total" rows (avoiding double-counting) and then dropped.
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

library(readxl)
library(dplyr)
library(readr)
source("codes/functions_sisver.R")


config <- list(
  input_path  = "data/raw/0_Pob_Inicio_1950_2070 (proyeccion conapo para 2025).xlsx",
  output_csv  = "data/processed/population_age_group.csv",
  years       = 2024:2026,
  cve_geo     = 0L
)

# ══════════════════════════════════════════════════════════════════════════════
# 1. READ & FILTER
# ══════════════════════════════════════════════════════════════════════════════

pop_raw <- read_excel(config$input_path) |>
  filter(CVE_GEO == config$cve_geo, AÑO %in% config$years)

stopifnot("No population data found — check years or CVE_GEO." = nrow(pop_raw) > 0)
cat("Years found:", paste(sort(unique(pop_raw$AÑO)), collapse = ", "), "\n")
cat("SEXO codes  :", paste(sort(unique(pop_raw$SEXO)), collapse = ", "), "\n")

# Sex itself is not retained — Male/Female are summed together below.
pop_raw <- pop_raw |>
  filter(SEXO %in% c("Hombres", "Mujeres", "Male", "Female", 0, 1, "0", "1"))

# ══════════════════════════════════════════════════════════════════════════════
# 2. AGGREGATE — by year × age_group (sex-collapsed)
# ══════════════════════════════════════════════════════════════════════════════
age_labels <- c("0-4 years", "5-19 years", "20-64 years", "65+ years")

population <- pop_raw |>
  mutate(age_group = make_age_group(EDAD)) |>
  group_by(year = AÑO, age_group) |>
  summarise(population = sum(POBLACION, na.rm = TRUE), .groups = "drop") |>
  mutate(age_group = factor(age_group, levels = age_labels))

cat("\nPopulation summary:\n")
print(population |> group_by(year) |> summarise(total = sum(population)))

# ══════════════════════════════════════════════════════════════════════════════
# 3. SAVE
# ══════════════════════════════════════════════════════════════════════════════

write_csv(population, config$output_csv)
cat("\nSaved:", config$output_csv, "\n")
cat("Structure:\n")
str(population)

