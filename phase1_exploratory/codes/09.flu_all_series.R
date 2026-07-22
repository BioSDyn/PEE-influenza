# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 09: Epidemic front comparison — symptomatic, hospitalized, deaths
# Project "PEE-Secihti" 2025
# Created: June 2026 by Imelda Trejo, CCM-UNAM
# Last update: July 21, 2026 by Imelda Trejo
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

input_path <- "data/processed/"
fig_path   <- "figures/"

rate_scale <- 100000
rate_label <- "100,000"


calendar           <- readRDS(paste0(input_path, "calendar.rds"))
weekly_symp_rates  <- readRDS(paste0(input_path, "weekly_symp_rates.rds"))
weekly_hosp_rates  <- readRDS(paste0(input_path, "weekly_hosp_rates.rds"))
weekly_death_rates <- readRDS(paste0(input_path, "weekly_death_rates.rds"))

# ══════════════════════════════════════════════════════════════════════════════
# 1. COMBINE: wide format, one row per week x age_group, no duplicated metadata
#    (calendar provides t_season/t_global/epi_week ONCE; each series only
#    contributes its own count columns)
# ══════════════════════════════════════════════════════════════════════════════

symp_only <- weekly_symp_rates |>
  select(season_label, age_group, n_obs, n_imp) |>
  rename(symp_obs = n_obs, symp_imp = n_imp)

hosp_only <- weekly_hosp_rates |>
  select(season_label, age_group, n_obs, n_imp) |>
  rename(hosp_obs = n_obs, hosp_imp = n_imp)

death_only <- weekly_death_rates |>
  select(season_label, age_group, n_obs, n_imp) |>
  rename(death_obs = n_obs, death_imp = n_imp)

combined_wide <- calendar |>
  crossing(age_group = unique(weekly_symp_rates$age_group)) |>
  left_join(symp_only,  by = c("season_label", "age_group")) |>
  left_join(hosp_only,  by = c("season_label", "age_group")) |>
  left_join(death_only, by = c("season_label", "age_group"))

# Sanity check: row count should match exactly (no accidental row duplication)
expected_rows <- nrow(calendar) * length(unique(weekly_symp_rates$age_group))
cat("Expected rows:", expected_rows, " | Actual rows:", nrow(combined_wide), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 2. LONG FORMAT FOR PLOTTING: stack the three series, then sum across age groups
# ══════════════════════════════════════════════════════════════════════════════

combined_long <- bind_rows(
  weekly_symp_rates  |> select(season_label, flu_season, epi_week, age_group,
                               n_obs, n_imp, is_imputed) |> mutate(series = "Symptomatic"),
  weekly_hosp_rates  |> select(season_label, flu_season, epi_week, age_group,
                               n_obs, n_imp, is_imputed) |> mutate(series = "Hospitalized"),
  weekly_death_rates |> select(season_label, flu_season, epi_week, age_group,
                               n_obs, n_imp, is_imputed) |> mutate(series = "Deaths")
) |>
  mutate(series = factor(series, levels = c("Symptomatic", "Hospitalized", "Deaths")))

# Total across age groups — one weekly value per series
combined_total <- combined_long |>
  group_by(series, season_label, flu_season, epi_week) |>
  summarise(
    n_obs      = sum(n_obs, na.rm = TRUE),
    n_imp      = sum(n_imp, na.rm = TRUE),
    is_imputed = any(is_imputed),   # TRUE if at least one age group was imputed that week
    .groups = "drop"
  ) |>
  left_join(calendar |> select(season_label, t_season, t_global), by = "season_label")

# ══════════════════════════════════════════════════════════════════════════════
# 3. PLOT: three stacked facets, continuous x-axis (t_global)
# ══════════════════════════════════════════════════════════════════════════════

ticks_ref   <- calendar |> arrange(t_global)
breaks_show <- ticks_ref$t_global[seq(1, nrow(ticks_ref), by = 4)]
labels_show <- ticks_ref$season_label[seq(1, nrow(ticks_ref), by = 4)]

season_cut <- calendar |>
  filter(flu_season == "2025-2026") |>
  summarise(t_global = min(t_global)) |>
  pull(t_global)

p_epidemic_fronts <- ggplot(combined_total, aes(x = t_global, color = series)) +
  
  geom_vline(xintercept = season_cut, linetype = "dashed", color = "grey50") +
  
  geom_line(aes(y = n_imp), linewidth = 0.6, alpha = 0.5) +
  
  geom_point(
    data = filter(combined_total, !is_imputed),
    aes(y = n_obs), shape = 16, size = 2.2
  ) +
  
  geom_point(
    data = filter(combined_total, is_imputed),
    aes(y = n_imp), shape = 4, size = 2.8, stroke = 1.3, color = "black"
  ) +
  
  facet_wrap(~ series, ncol = 1, scales = "free_y") +
  
  scale_x_continuous(breaks = breaks_show, labels = labels_show, expand = c(0.01, 0)) +
  
  scale_color_manual(values = c(
    "Symptomatic"  = "#2C7FB8",
    "Hospitalized" = "#D95F02",
    "Deaths"       = "#7B3294"
  ), guide = "none") +
  
  labs(
    title    = "Seasonal influenza epidemic waves: detected and imputed",
    subtitle = "Total (all age groups), SE40 2024 - SE22 2026",
    x        = "Epidemiological week",
    y        = "Weekly count",
    caption  = "\u25cf observed  |  \u2715 imputed  |  dashed line: start of 2025-2026 season"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    strip.text       = element_text(face = "bold")
  )+
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title        = element_text(size = 18, face = "bold"),
    plot.subtitle     = element_text(size = 14),
    plot.caption      = element_text(size = 11),
    axis.title        = element_text(size = 14),
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y       = element_text(size = 12),
    strip.text        = element_text(face = "bold", size = 13)
  )

p_epidemic_fronts

ggsave(paste0(fig_path, "flu_epidemic_fronts.png"),
       p_epidemic_fronts, width = 12, height = 9, dpi = 300)


#stratyfied plot by age groups

# Traer t_global desde el calendario (combined_long no lo tiene todavía)
combined_long_t <- combined_long |>
  left_join(calendar |> select(season_label, t_season, t_global), by = "season_label")

ticks_ref   <- calendar |> arrange(t_global)
breaks_show <- ticks_ref$t_global[seq(1, nrow(ticks_ref), by = 12)]   # cada 8 para que no se amontonen con 4x3 paneles
labels_show <- ticks_ref$season_label[seq(1, nrow(ticks_ref), by = 12)]

season_cut <- calendar |>
  filter(flu_season == "2025-2026") |>
  summarise(t_global = min(t_global)) |>
  pull(t_global)

age_colors <- c(
  "0-4 years"   = "#1B9E77",
  "5-19 years"  = "#D95F02",
  "20-64 years" = "#7570B3",
  "65+ years"   = "#E7298A"
)

p_fronts_by_age <- ggplot(combined_long_t, aes(x = t_global, color = age_group)) +
  
  geom_vline(xintercept = season_cut, linetype = "dashed", color = "grey50") +
  
  geom_line(aes(y = n_imp), linewidth = 0.6, alpha = 0.6) +
  
  geom_point(
    data = filter(combined_long_t, !is_imputed),
    aes(y = n_obs), shape = 16, size = 1.6
  ) +
  
  geom_point(
    data = filter(combined_long_t, is_imputed),
    aes(y = n_imp), shape = 4, size = 2, stroke = 1.1, color = "black"
  ) +
  
  facet_grid(series ~ age_group, scales = "free_y") +
  
  scale_x_continuous(breaks = breaks_show, labels = labels_show, expand = c(0.01, 0)) +
  
  scale_color_manual(values = age_colors, guide = "none") +
  
  labs(
    title    = "Seasonal influenza epidemic waves by age group",
    subtitle = "SE40 2024 - SE22 2026",
    x        = "Epidemiological week",
    y        = "Weekly count",
    caption  = "\u25cf observed  |  \u2715 imputed  |  dashed line: start of 2025-2026 season"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
    strip.text        = element_text(face = "bold", size = 10)
  )+
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title        = element_text(size = 18, face = "bold"),
    plot.subtitle     = element_text(size = 14),
    plot.caption      = element_text(size = 11),
    axis.title        = element_text(size = 14),
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y       = element_text(size = 12),
    strip.text        = element_text(face = "bold", size = 13)
  )

p_fronts_by_age


ggsave(paste0(fig_path, "flu_fronts_by_age.png"),
       p_fronts_by_age, width = 16, height = 9, dpi = 300)

# ══════════════════════════════════════════════════════════════════════════════
# Similar plot (rate_obs / rate_imp) 
# ══════════════════════════════════════════════════════════════════════════════

combined_long_rates <- bind_rows(
  weekly_symp_rates  |> select(season_label, flu_season, epi_week, age_group,
                               rate_obs, rate_imp, is_imputed) |> mutate(series = "Symptomatic"),
  weekly_hosp_rates  |> select(season_label, flu_season, epi_week, age_group,
                               rate_obs, rate_imp, is_imputed) |> mutate(series = "Hospitalized"),
  weekly_death_rates |> select(season_label, flu_season, epi_week, age_group,
                               rate_obs, rate_imp, is_imputed) |> mutate(series = "Deaths")
) |>
  mutate(series = factor(series, levels = c("Symptomatic", "Hospitalized", "Deaths"))) |>
  left_join(calendar |> select(season_label, t_season, t_global), by = "season_label")

age_colors <- c(
  "0-4 years"   = "#1B9E77",
  "5-19 years"  = "#D95F02",
  "20-64 years" = "#7570B3",
  "65+ years"   = "#E7298A"
)

p_fronts_by_age_rates <- ggplot(combined_long_rates, aes(x = t_global, color = age_group)) +
  
  geom_vline(xintercept = season_cut, linetype = "dashed", color = "grey50") +
  
  geom_line(aes(y = rate_imp), linewidth = 0.6, alpha = 0.6) +
  
  geom_point(
    data = filter(combined_long_rates, !is_imputed),
    aes(y = rate_obs), shape = 16, size = 1.6
  ) +
  
  geom_point(
    data = filter(combined_long_rates, is_imputed),
    aes(y = rate_imp), shape = 4, size = 2, stroke = 1.1, color = "black"
  ) +
  
  facet_grid(series ~ age_group, scales = "free_y") +
  
  scale_x_continuous(breaks = breaks_show, labels = labels_show, expand = c(0.01, 0)) +
  
  scale_color_manual(values = age_colors, guide = "none") +
  
  labs(
    title    = "Seasonal influenza epidemic waves by age group — rates",
    subtitle = paste("Per", rate_label, "population, SE40 2024 - SE22 2026"),
    x        = "Epidemiological week",
    y        = paste0("Rate (per ", rate_label, ")"),
    caption  = "\u25cf observed  |  \u2715 imputed  |  dashed line: start of 2025-2026 season"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
    strip.text        = element_text(face = "bold", size = 10)
  )

p_fronts_by_age_rates

ggsave(paste0(fig_path, "flu_fronts_by_age_rates.png"),
       p_fronts_by_age_rates, width = 16, height = 9, dpi = 300)

# ══════════════════════════════════════════════════════════════════════════════
# 4. SAVE OUTPUTS
# ══════════════════════════════════════════════════════════════════════════════

#saveRDS(combined_wide, paste0(input_path, "flu_combined_series_wide.rds"))
#saveRDS(combined_long, paste0(input_path, "flu_combined_series_long.rds"))

cat("\n--- Outputs saved ---------------------------------------------------\n")
cat("  Wide format :", paste0(input_path, "flu_combined_series_wide.rds\n"))
cat("  Long format :", paste0(input_path, "flu_combined_series_long.rds\n"))
cat("  Figure      :", paste0(fig_path, "flu_epidemic_fronts.png\n"))
cat("-----------------------------------------------------------------------\n")

