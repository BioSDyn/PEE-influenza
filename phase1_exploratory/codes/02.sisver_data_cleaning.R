# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 02: Data cleaning — SISVER (Sistema de Vigilancia de Influenza)
# Project supported by "Secihti", 2025
# Last update: July 21 2026 by Imelda Trejo
#
# Data Source: https://www.gob.mx/salud/en/documentos/datos-abiertos-152127
#   Last downloaded: July 21, 2026 (latest release)
#
# INPUT:
#   data/raw/COVID19MEXICO{year}.csv  (years: 2024–2026)
#   data/catalogos/240708 Catalogos.xlsx
#   codigo/functions_sisver.R
#
# OUTPUT:
#   data/processed/flu_clean.rds
#   data/processed/flu_clean.csv
#
# Pipeline:
#   1. Read    → concatenate yearly CSV files
#   2. Filter  → flu surveillance only (CLASIFICACION_FINAL_FLU %in% c(3, 7))
#   3. Dates   → character → Date; FECHA_DEF "9999-99-99" → NA
#   4. Age    → character → integer; 999 → NA
#   5. Dedup   → keep most recent record per ID_REGISTRO
#   6. Remove  → epidemiologically inconsistent date records
#   7. Recode  → apply SISVER catalogues to categorical variables
#   8. Save
#
# Cleaning decisions:
#   1. Surveillance scope: flu-confirmed (code 3) and flu-negative (code 7) only.
#      All other CLASIFICACION_FINAL_FLU codes excluded.
#      SISVER total: 507949 | Vigilancia flu: 303,900
#
#   2. Deduplication: SISVER republishes full historical data each year.
#      Same patient appears across yearly files with different FECHA_ACTUALIZACION.
#      Strategy: keep most recent update per ID_REGISTRO.
#      Result: 69,047 records removed (23%).
#
#   3. Date inconsistencies (all patients — FECHA_INGRESO is clinically meaningful
#      for both hospitalized and ambulatory patients, confirmed by diagnostic):
#        - FECHA_DEF     < FECHA_SINTOMAS  → biologically impossible
#        - FECHA_DEF     < FECHA_INGRESO   → impossible
#        - FECHA_INGRESO < FECHA_SINTOMAS  → not plausible for our modeling purpose
#
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

# ── Libraries ──────────────────────────────────────────────────────────────────
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(readxl)

source("codes/functions_sisver.R")

# ── Config ─────────────────────────────────────────────────────────────────────
config <- list(
  path_raw      = "data/raw/",
  patron        = "COVID19MEXICO",
  years         = 2024:2026,
  mode          = "full",
  catalogo_path = "data/catalogos/240708 Catalogos.xlsx",
  output_csv    = "data/processed/flu_clean.csv",  # set NULL to skip
  save_csv      = TRUE
)

# ── Variables of interest ──────────────────────────────────────────────────────
VARS_INTEREST <- c(
  "FECHA_ACTUALIZACION", "FECHA_INGRESO", "FECHA_SINTOMAS", "FECHA_DEF",
  "EDAD", "SEXO", "TIPO_PACIENTE", "ID_REGISTRO",
  "RESULTADO_PCR", "CLASIFICACION_FINAL_FLU"
)

# ══════════════════════════════════════════════════════════════════════════════
# 1. READ — concatenate yearly files
# ══════════════════════════════════════════════════════════════════════════════

files <- list.files(
  path       = config$path_raw,
  pattern    = paste0(config$patron, ".*\\.csv$"),
  full.names = TRUE
) |>
  (\(x) x[str_extract(x, "\\d{4}") %in% as.character(config$years)])()

stopifnot("No input files found." = length(files) > 0)
cat("Files found:", length(files), "\n")
print(basename(files))

data_raw <- map_dfr(files, ~ read_sisver_one_by_one_csv_file(.x, config$mode))
cat("Records read:", nrow(data_raw), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 2. FILTER & SELECT — flu surveillance scope
# ══════════════════════════════════════════════════════════════════════════════

data_flu <- data_raw |>
  filter(CLASIFICACION_FINAL_FLU %in% c(3, 7)) |>
  select(all_of(VARS_INTEREST))

cat("flu records:", nrow(data_flu), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 3. DATES — character → Date; "9999-99-99" → NA (patient alive)
# ══════════════════════════════════════════════════════════════════════════════

date_vars <- names(data_flu)[str_starts(names(data_flu), "FECHA")]

data_flu <- data_flu |>
  convert_date_vars(date_vars = date_vars, sisver_vars = "FECHA_DEF")

# ══════════════════════════════════════════════════════════════════════════════
# 4. AGE — character → integer; 999 (unknown) → NA
# ══════════════════════════════════════════════════════════════════════════════

data_flu <- data_flu |>
  mutate(age = na_if(as.integer(EDAD), 999L))

# ══════════════════════════════════════════════════════════════════════════════
# 5. DEDUPLICATION — keep most recent record per ID_REGISTRO
# ══════════════════════════════════════════════════════════════════════════════

data_dedup <- dedup_registros_sisver(data_flu)

# ══════════════════════════════════════════════════════════════════════════════
# 6. DATE CONSISTENCY — remove epidemiological inconsistent records
# ══════════════════════════════════════════════════════════════════════════════

flag_def_sintomas <- !is.na(data_dedup$FECHA_DEF) & 
  data_dedup$FECHA_DEF < data_dedup$FECHA_SINTOMAS

# flags bien definidos con NA controlado
flag_def_ingreso <- !is.na(data_dedup$FECHA_DEF) & 
  !is.na(data_dedup$FECHA_INGRESO) &
  data_dedup$FECHA_DEF < data_dedup$FECHA_INGRESO

flag_ingreso_sintomas <- data_dedup$TIPO_PACIENTE == 2 &
  !is.na(data_dedup$FECHA_INGRESO) &
  !is.na(data_dedup$FECHA_SINTOMAS) &
  data_dedup$FECHA_INGRESO < data_dedup$FECHA_SINTOMAS

table(flag_def_sintomas)
table(flag_def_ingreso)
table(flag_ingreso_sintomas)


data_clean <- data_dedup |>
  remove_inconsistencias_fechas() |>
  select(-ID_REGISTRO)


cat("\n── Cleaning summary ──────────────────────────\n")
cat("  data_raw       :", nrow(data_raw),   "records\n")
cat("  data_flu:", nrow(data_flu), "records\n")
cat("  data_dedup     :", nrow(data_dedup), "records\n")
cat("  data_clean     :", nrow(data_clean), "records\n")
cat("──────────────────────────────────────────────\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# 7. RECODE — apply SISVER catalog to categorical variables
# ══════════════════════════════════════════════════════════════════════════════

catalogo_list <- excel_sheets(config$catalogo_path) |>
  set_names() |>
  map(~ read_excel(config$catalogo_path, sheet = .x))

data_clean <- data_clean |>
  mutate(
    TIPO_PACIENTE = factor(
      TIPO_PACIENTE,
      levels = as.character(catalogo_list$`Catálogo TIPO_PACIENTE`$CLAVE),
      labels = catalogo_list$`Catálogo TIPO_PACIENTE`$DESCRIPCIÓN
    ),
    SEXO = factor(
      SEXO,
      levels  = catalogo_list$`Catálogo SEXO`$CLAVE,
      labels  = catalogo_list$`Catálogo SEXO`$DESCRIPCIÓN,
      exclude = NULL
    ),
    RESULTADO_PCR = factor(
      RESULTADO_PCR,
      levels  = catalogo_list$`Catálogo RESULTADO_PCR`$CLAVE,
      labels  = catalogo_list$`Catálogo RESULTADO_PCR`$DESCRIPCIÓN,
      exclude = NULL
    ),
    influenza_pcr = case_when(
      CLASIFICACION_FINAL_FLU == 3 ~ "Positive",
      CLASIFICACION_FINAL_FLU == 7 ~ "Negative",
      TRUE ~ NA_character_
    ),
    influenza_pcr = factor(influenza_pcr, levels = c("Negative", "Positive"))
) |>
  select(-c(EDAD,CLASIFICACION_FINAL_FLU,FECHA_ACTUALIZACION))


table(data_clean$influenza_pcr,data_clean$TIPO_PACIENTE)
# ══════════════════════════════════════════════════════════════════════════════
# 8. SAVE
# ══════════════════════════════════════════════════════════════════════════════

#saveRDS(data_clean, config$output_rds)
#cat("Saved:", config$output_rds, "\n")

if (isTRUE(config$save_csv) && !is.null(config$output_csv)) {
  write_csv(data_clean, config$output_csv)
  cat("Saved:", config$output_csv, "\n")
}

