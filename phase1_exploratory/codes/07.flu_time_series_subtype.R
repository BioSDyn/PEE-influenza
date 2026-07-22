# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 03c: Visualización de subtipos de influenza
# Proyecto apoyado por la "Secihti" en el año 2025
# Last update: July 2026 by Imelda Trejo
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(knitr)
library(kableExtra)
library(scales)
library(readr)
source("codes/functions_sisver.R")

# ── Config ────────────────────────────────────────────────────────────────────
input_path <- "data/processed/"
fig_path   <- "figures/"
table_path <- "figures/"

# ══════════════════════════════════════════════════════════════════════════════
# 1. CATÁLOGO DE SUBTIPOS
# ══════════════════════════════════════════════════════════════════════════════

catalogo_subtipos <- tibble(
  clave = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 17,
            20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 32,
            33, 34, 35, 36, 37, 41, 997, 998, 999),
  descripcion = c(
    "INFLUENZA AH1N1 PMD", "INFLUENZA A H1", "INFLUENZA A H3",
    "INFLUENZA B", "NEGATIVO", "MUESTRA NO ADECUADA",
    "ADENOVIRUS", "PARAINFLUENZA 1", "PARAINFLUENZA 2",
    "PARAINFLUENZA 3", "VIRUS SINCICIAL RESPIRATORIO",
    "INFLUENZA A NO SUBTIPIFICADA", "INFLUENZA A H5",
    "MUESTRA RECHAZADA", "VIRUS SINCICIAL RESPIRATORIO A",
    "VIRUS SINCICIAL RESPIRATORIO B", "CORONA 229E", "CORONA OC43",
    "CORONA SARS", "CORONA NL63", "CORONA HKU1",
    "MUESTRA QUE NO AMPLIFICO", "ENTEROV/RHINOVIRUS",
    "METAPNEUMOVIRUS", "MUESTRA SIN AISLAMIENTO", "PARAINFLUENZA 4",
    "MUESTRA SIN CELULAS", "SARS-CoV-2", "MERS-CoV", "SARS-CoV",
    "BOCAVIRUS", "MUESTRA NO RECIBIDA",
    "NO APLICA (CASO SIN MUESTRA)", "SIN COINFECCIÓN", "PENDIENTE"
  ),
  categoria = c(
    "Influenza A", "Influenza A", "Influenza A",
    "Influenza B", "No influenza", "No válida",
    "Otro virus", "Parainfluenza", "Parainfluenza",
    "Parainfluenza", "VSR", "Influenza A",
    "Influenza A", "No válida", "VSR", "VSR",
    "Coronavirus", "Coronavirus", "Coronavirus",
    "Coronavirus", "Coronavirus", "No válida",
    "Otro virus", "Otro virus", "No válida",
    "Parainfluenza", "No válida", "Coronavirus",
    "Coronavirus", "Coronavirus", "Otro virus",
    "No válida", "No válida", "No válida", "Pendiente"
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# 2. LECTURA Y PREPARACIÓN DE DATOS
# ══════════════════════════════════════════════════════════════════════════════

data_all <- readRDS(paste0(input_path, "flu_analytic.rds"))
#data_all  <- read_csv("data/processed/flu_clean.csv")


data_flu <- data_all |>
  filter(
    influenza_pcr == "Positive",
    flu_season %in% c("2024-2025", "2025-2026"),
    !is.na(flu_season),
    !is.na(age_group)
  ) |>
  mutate(
    RESULTADO_PCR = as.character(RESULTADO_PCR),
    RESULTADO_PCR = recode(RESULTADO_PCR,
                           "ENTEROV//RHINOVIRUS" = "ENTEROV/RHINOVIRUS"
    )
  ) |>
  left_join(
    catalogo_subtipos |> mutate(descripcion = as.character(descripcion)),
    by = c("RESULTADO_PCR" = "descripcion")
  ) |>
  mutate(
    subtipo_label = case_when(
      clave == 1  ~ "AH1N1pdm",
      clave == 2  ~ "A H1",
      clave == 3  ~ "A H3",
      clave == 4  ~ "B",
      clave == 13 ~ "A no subtip.",
      clave == 14 ~ "A H5",
      TRUE        ~ "Otro/No det."
    ),
    subtipo_label = factor(
      subtipo_label,
      levels = c("AH1N1pdm", "A H1", "A H3", "B", "A no subtip.", "A H5")
    )
  ) |>
  filter(subtipo_label != "Otro/No det.")

cat("Registros totales de influenza confirmada:", nrow(data_flu), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 3. PALETA DE COLORES
# ══════════════════════════════════════════════════════════════════════════════

paleta_subtipos <- c(
  "AH1N1pdm"    = "#E63946",
  "A H1"        = "#FF6B6B",
  "A H3"        = "#F4A261",
  "B"           = "#2A9D8F",
  "A no subtip."= "#A8DADC",
  "A H5"        = "#6A0572"
)

# ══════════════════════════════════════════════════════════════════════════════
# 4. AGREGACIÓN SEMANAL
# ══════════════════════════════════════════════════════════════════════════════

weekly_sub <- data_flu |>
  group_by(flu_season, epi_week, epi_year, subtipo_label) |>
  summarise(casos = n(), .groups = "drop") |>
  mutate(
    epi_week_ord = ifelse(epi_week >= 40, epi_week - 39, epi_week + 13)
  ) |>
  arrange(flu_season, epi_week_ord)

# Eje x: secuencia completa de semanas de la temporada
flu_wks_completo     <- get_flu_weeks(unique(weekly_sub$epi_week))
epi_week_ord_completo <- seq_along(flu_wks_completo)

# ══════════════════════════════════════════════════════════════════════════════
# 5. PLOT: Barras apiladas por subtipo y temporada
# ══════════════════════════════════════════════════════════════════════════════

p_sub_stack <- ggplot(
  weekly_sub,
  aes(x = epi_week_ord, y = casos, fill = subtipo_label)
) +
  geom_col(width = 0.8) +
  facet_wrap(~ flu_season, ncol = 1) +
  scale_x_continuous(
    limits = range(epi_week_ord_completo),
    breaks = epi_week_ord_completo,
    labels = flu_wks_completo,
    expand = c(0.02, 0)
  ) +
  scale_fill_manual(name = "Subtipo", values = paleta_subtipos) +
  scale_y_continuous(
    labels = comma_format(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title   = "Casos confirmados de influenza por subtipo",
    x       = "Semana epidemiológica",
    y       = "Casos confirmados",
    caption = "Fuente: SISVER — temporadas 2024-2025 y 2025-2026"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    strip.text         = element_text(face = "bold", size = 12),
    legend.position    = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 1))

p_sub_stack

ggsave(paste0(fig_path, "ts_flu_subtipos_stack.png"),
       p_sub_stack, width = 12, height = 9, dpi = 300)
