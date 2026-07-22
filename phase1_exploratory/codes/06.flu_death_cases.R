# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 06: Weekly series — defunciones por influenza (FECHA_DEF)
# Project supported by "Secihti", 2025
# Created: June 2026 by Imelda Trejo, CCM-UNAM
# Last uodated: July 21, 2026 by Imelda Trejo
#
# INPUT:  data/processed/calendar.rds
#         data/processed/population_denominator.rds
#         data/processed/flu_analytic.rds
# OUTPUT: data/processed/weekly_death_rates.rds
#         figures/flu_death_counts_by_age.png
#         figures/flu_death_rates_by_age.png
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

source("codes/functions_sisver.R")

# ── Config ────────────────────────────────────────────────────────────────────
input_path <- "data/processed/"
fig_path   <- "figures/"

rate_scale <- 100000
rate_label <- "100,000"


metric_title  <- "Influenza mortality"
output_prefix <- "flu_death"


# AJUSTAR si hay un indicador de defunción distinto (ej. DEF == "SI")
event_filter <- quote(influenza_pcr == "Positive" & !is.na(FECHA_DEF))

# ══════════════════════════════════════════════════════════════════════════════
# 1. LEER: calendario, denominador y dataset analítico (script 03)
# ══════════════════════════════════════════════════════════════════════════════

calendar  <- readRDS(paste0(input_path, "calendar.rds"))
pop_denom <- readRDS(paste0(input_path, "population_denominator.rds"))
data      <- readRDS(paste0(input_path, "flu_analytic.rds"))

# ══════════════════════════════════════════════════════════════════════════════
# 2. FILTRAR: positivos Y defunciones
# ══════════════════════════════════════════════════════════════════════════════

death_raw <- data |>
  filter(!!event_filter) |>
  select(-any_of(c("FECHA_ACTUALIZACION", "FECHA_SINTOMAS", "FECHA_INGRESO")))

cat("Registros que cumplen el filtro:", nrow(death_raw), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 3. RECALCULAR semana/temporada por FECHA_DEF, descartar fuera de temporada
# ══════════════════════════════════════════════════════════════════════════════
# flu_analytic.rds trae epi_week/season_label/flu_season anclados a FECHA_SINTOMAS
# (script 03) — aquí se sobrescriben con la versión anclada a FECHA_DEF,
# que es la fecha correcta para ubicar temporalmente una defunción.

death_data <- build_analytic_epi_vars(death_raw, calendar, fecha_ref = "FECHA_DEF") |>
  filter(!is.na(flu_season))

cat("Registros dentro de temporada:", nrow(death_data), "de", nrow(death_raw), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 4. CONTEO SEMANAL POR GRUPO DE EDAD (defunciones)
# ══════════════════════════════════════════════════════════════════════════════

weekly_death_cases <- death_data |>
  count(season_label, flu_season, epi_week, age_group, name = "n_obs")

calendar_age_grid <- calendar |>
  crossing(age_group = unique(death_data$age_group))

weekly_death_cases_full <- calendar_age_grid |>
  left_join(weekly_death_cases, by = c("season_label", "flu_season", "epi_week", "age_group"))

# ══════════════════════════════════════════════════════════════════════════════
# 5. IMPUTACIÓN ESTRATIFICADA
# ══════════════════════════════════════════════════════════════════════════════
# Ventana del suavizador: h_init = 1, h_iter = 2, n_iter = 3

weekly_death_imp <- weekly_death_cases_full |>
  group_by(flu_season, age_group) |>
  group_modify(~ impute_flu_series(.x, h_init = 1, h_iter = 2, n_iter = 3)) |>
  ungroup()

cat("\nSemanas imputadas (defunciones) por grupo y temporada:\n")
weekly_death_imp |>
  filter(is_imputed) |>
  count(flu_season, age_group) |>
  print()

# ══════════════════════════════════════════════════════════════════════════════
# 6. TASAS SEMANALES
# ══════════════════════════════════════════════════════════════════════════════

weekly_death_rates <- weekly_death_imp |>
  left_join(pop_denom, by = c("flu_season", "age_group")) |>
  mutate(
    rate_obs = (n_obs / pop_denom) * rate_scale,
    rate_imp = (n_imp / pop_denom) * rate_scale
  )

cat("\nNAs en pop_denom tras el join:", sum(is.na(weekly_death_rates$pop_denom)), "\n")


weekly_death_rates |>
  group_by(flu_season, age_group) |>
  summarise(pct_imputed = mean(is_imputed) * 100, .groups = "drop") |>
  arrange(desc(pct_imputed))

# ══════════════════════════════════════════════════════════════════════════════
# 7. PLOTS — conteos y tasas
# ══════════════════════════════════════════════════════════════════════════════

p_counts <- plot_age_series(
  weekly_death_rates, calendar,
  y_obs = "n_obs", y_imp = "n_imp",
  title   = paste(metric_title, "by age group — detected and imputed"),
  y_label = "Number of deaths"
)
p_counts

#ggsave(paste0(fig_path, output_prefix, "_counts_by_age.png"), p_counts, width = 12, height = 8, dpi = 300)

p_rates <- plot_age_series(
  weekly_death_rates, calendar,
  y_obs = "rate_obs", y_imp = "rate_imp",
  title    = paste(metric_title, "rate by age group"),
  subtitle = paste("per", rate_label),
  y_label  = paste("Rate (per", rate_label, ")")
)
p_rates

ggsave(paste0(fig_path, output_prefix, "_rates_by_age.png"),
       p_rates, width = 12, height = 8, dpi = 300)




# ══════════════════════════════════════════════════════════════════════════════
# 8. GUARDAR OUTPUTS
# ══════════════════════════════════════════════════════════════════════════════

saveRDS(weekly_death_rates, paste0(input_path, "weekly_death_rates.rds"))


