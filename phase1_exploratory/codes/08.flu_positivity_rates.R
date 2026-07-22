# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 07: Weekly test positivity rate by age group
# Project supported by "Secihti", 2025
# Created: June 2026 by Imelda Trejo, CCM-UNAM
#
# INPUT:  data/processed/calendar.rds
#         data/processed/flu_analytic.rds
# OUTPUT: data/processed/weekly_positivity.rds
#         figures/flu_positivity_by_age.png
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

source("codes/functions_sisver.R")

# ── Config ────────────────────────────────────────────────────────────────────
input_path <- "data/processed/"
fig_path   <- "figures/"

min_tested <- 5   # debajo de este número de pruebas, la positividad es poco confiable

# ══════════════════════════════════════════════════════════════════════════════
# 1. LEER: calendario y dataset analítico (script 03) — sin filtrar por positividad
# ══════════════════════════════════════════════════════════════════════════════

calendar <- readRDS(paste0(input_path, "calendar.rds"))
data     <- readRDS(paste0(input_path, "flu_analytic.rds"))

cat("Registros totales (positivos + negativos) en el horizonte:", nrow(data), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 2. CONTEO SEMANAL: total probado y total positivo, por grupo de edad
# ══════════════════════════════════════════════════════════════════════════════

weekly_tested <- data |>
  count(season_label, flu_season, epi_week, age_group, name = "n_tested")

weekly_positive <- data |>
  filter(influenza_pcr == "Positive") |>
  count(season_label, flu_season, epi_week, age_group, name = "n_positive")

calendar_age_grid <- calendar |>
  crossing(age_group = unique(data$age_group))

weekly_positivity <- calendar_age_grid |>
  left_join(weekly_tested,  by = c("season_label", "flu_season", "epi_week", "age_group")) |>
  left_join(weekly_positive, by = c("season_label", "flu_season", "epi_week", "age_group")) |>
  mutate(
    n_positive     = tidyr::replace_na(n_positive, 0),   # 0 genuino si hubo pruebas pero ninguna positiva
    positivity_pct = (n_positive / n_tested) * 100,       # NA si n_tested es NA (sin pruebas esa semana)
    low_n          = n_tested < min_tested
  )

cat("\nSemanas sin ninguna prueba registrada (n_tested = NA):",
    sum(is.na(weekly_positivity$n_tested)), "\n")
cat("Semanas con menos de", min_tested, "pruebas:",
    sum(weekly_positivity$low_n, na.rm = TRUE), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 3. PLOT: positividad semanal por grupo de edad
# ══════════════════════════════════════════════════════════════════════════════

plot_positivity_series <- function(data, calendar, min_tested = 5) {
  
  longest_season <- calendar |> count(flu_season) |> slice_max(n, n = 1) |> pull(flu_season)
  ticks_ref   <- calendar |> filter(flu_season == longest_season) |> arrange(t_season)
  breaks_show <- ticks_ref$t_season[seq(1, nrow(ticks_ref), by = 4)]
  labels_show <- ticks_ref$epi_week[seq(1, nrow(ticks_ref), by = 4)]
  
  ggplot(data, aes(x = t_season, y = positivity_pct, color = flu_season, group = flu_season)) +
    
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    
    geom_point(aes(shape = low_n), size = 2, na.rm = TRUE) +
    
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
    
    facet_wrap(~ age_group, scales = "free_y", ncol = 2) +
    
    scale_x_continuous(breaks = breaks_show, labels = labels_show, expand = c(0.02, 0)) +
    
    scale_color_manual(
      name   = "Season",
      values = c("2024-2025" = "#F8766D", "2025-2026" = "#00BFC4")
    ) +
    
    labs(
      title   = "Influenza test positivity rate by age group",
      x       = "Epidemic week (October–May)",
      y       = "Positivity (%)",
      caption = paste0("○ semana con menos de ", min_tested, " pruebas — interpretar con cautela")
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
      strip.text       = element_text(face = "bold")
    )
}

p_positivity <- plot_positivity_series(weekly_positivity, calendar, min_tested = min_tested)
p_positivity

ggsave(paste0(fig_path, "flu_positivity_by_age.png"),
       p_positivity, width = 12, height = 8, dpi = 300)

# ══════════════════════════════════════════════════════════════════════════════
# 4. GUARDAR OUTPUTS
# ══════════════════════════════════════════════════════════════════════════════

saveRDS(weekly_positivity, paste0(input_path, "weekly_positivity.rds"))

cat("\n── Outputs guardados ───────────────────────────────────────────────────\n")
cat("  Positividad semanal:", paste0(input_path, "weekly_positivity.rds\n"))
cat("  Figura              :", paste0(fig_path, "flu_positivity_by_age.png\n"))
cat("──────────────────────────────────────────────────────────────────────────\n")

