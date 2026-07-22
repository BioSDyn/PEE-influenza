# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 04: Weekly series — casos sintomáticos de influenza (FECHA_SINTOMAS)
# Project "Secihti" 2025
# Created: June 2026 by Imelda Trejo, CCM-UNAM
#
# INPUT:  data/processed/calendar.rds
#         data/processed/population_denominator.rds
#         data/processed/flu_analytic.rds
# OUTPUT: data/processed/weekly_symp_rates.rds
#         figures/time_series/flu_symp_counts_by_age.png
#         figures/time_series/flu_symp_rates_by_age.png
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

# ── Libraries ──────────────────────────────────────────────────────────────────
library(dplyr)
library(tidyr)
library(ggplot2)

source("codes/functions_sisver.R")

# ── Config ────────────────────────────────────────────────────────────────────
input_path <- "data/processed/"
fig_path   <- "figures/"

rate_scale <- 100000
rate_label <- "100,000"


metric_title  <- "Influenza symptomatic"
output_prefix <- "flu_symp"

# ══════════════════════════════════════════════════════════════════════════════
# 1. LEER: calendario, denominador y dataset analítico (script 03) + filtrar positividad
# ══════════════════════════════════════════════════════════════════════════════

calendar  <- readRDS(paste0(input_path, "calendar.rds"))
pop_denom <- readRDS(paste0(input_path, "population_denominator.rds"))
flu_data  <- readRDS(paste0(input_path, "flu_analytic.rds")) |>
  filter(influenza_pcr == "Positive")

cat("Casos sintomáticos positivos:", nrow(flu_data), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 2. CONTEO SEMANAL POR GRUPO DE EDAD (síntomas)
# ══════════════════════════════════════════════════════════════════════════════

weekly_symp_cases <- flu_data |>
  count(season_label, flu_season, epi_week, age_group, name = "n_obs")

calendar_age_grid <- calendar |>
  crossing(age_group = unique(flu_data$age_group))

weekly_symp_cases_full <- calendar_age_grid |>
  left_join(weekly_symp_cases, by = c("season_label", "flu_season", "epi_week", "age_group"))

# ══════════════════════════════════════════════════════════════════════════════
# 3. IMPUTACIÓN ESTRATIFICADA
# ══════════════════════════════════════════════════════════════════════════════
# Ventana del suavizador: h_init = 1, h_iter = 2, n_iter = 3

weekly_symp_imp <- weekly_symp_cases_full |>
  group_by(flu_season, age_group) |>
  group_modify(~ impute_flu_series(.x, h_init = 1, h_iter = 2, n_iter = 3)) |>
  ungroup()

cat("\nSemanas imputadas por grupo y temporada:\n")
weekly_symp_imp |>
  filter(is_imputed) |>
  count(flu_season, age_group) |>
  print()

# ══════════════════════════════════════════════════════════════════════════════
# 4. TASAS SEMANALES
# ══════════════════════════════════════════════════════════════════════════════

weekly_symp_rates <- weekly_symp_imp |>
  left_join(pop_denom, by = c("flu_season", "age_group")) |>
  mutate(
    rate_obs = (n_obs / pop_denom) * rate_scale,
    rate_imp = (n_imp / pop_denom) * rate_scale
  )

cat("\nNAs en pop_denom tras el join:", sum(is.na(weekly_symp_rates$pop_denom)), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 5. PLOTS — conteos y tasas
# ══════════════════════════════════════════════════════════════════════════════

p_counts <- plot_age_series(
  weekly_symp_rates, calendar,
  y_obs = "n_obs", y_imp = "n_imp",
  title   = paste(metric_title, "cases by age group — detected and imputed"),
  y_label = "Number of cases"
)
p_counts

#ggsave(paste0(fig_path, output_prefix, "_counts_by_age.png"),
#       p_counts, width = 12, height = 8, dpi = 300)

p_rates <- plot_age_series(
  weekly_symp_rates, calendar,
  y_obs = "rate_obs", y_imp = "rate_imp",
  title    = paste(metric_title, "rate by age group"),
  subtitle = paste("per", rate_label),
  y_label  = paste("Rate (per", rate_label, ")")
)
p_rates

ggsave(paste0(fig_path, output_prefix, "_rates_by_age.png"),
       p_rates, width = 12, height = 8, dpi = 300)

# ══════════════════════════════════════════════════════════════════════════════
# 6. GUARDAR OUTPUTS
# ══════════════════════════════════════════════════════════════════════════════

saveRDS(weekly_symp_rates, paste0(input_path, "weekly_symp_rates.rds"))

