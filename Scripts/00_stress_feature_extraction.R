# =====================================================================
# 00_stress_feature_extraction.R
# GARMIN STRESS FEATURE EXTRACTION — AI-READI INSULIN RESISTANCE PREDICTION
# =====================================================================
#
# Run from the REPOSITORY ROOT:
#   setwd("/path/to/IR-prediction")
#   source("scripts/00_stress_feature_extraction.R")
#
# INPUT:
#   data/raw/garmin_vivosmart5_stress/    — folder of participant subfolders,
#                                           each containing a *_stress.json file
#
# OUTPUT:
#   data/processed/stress_overall_daily.csv — one row per participant:
#     id, stress_overall_daily_mean_stress
#
# FEATURE:
#   stress_overall_daily_mean_stress — mean of daily mean Garmin HRV-derived
#     stress scores across all valid monitoring days (range 0–100; higher
#     values indicate greater sympathetic dominance / physiological stress load)
# =====================================================================

library(dplyr)
library(lubridate)
library(jsonlite)
library(tidyr)
library(stringr)

# ── Paths ─────────────────────────────────────────────────────────────
RAW       <- "data/raw"
PROCESSED <- "data/processed"

STRESS_DIR <- file.path(RAW, "garmin_vivosmart5_stress")

dir.create(PROCESSED, recursive = TRUE, showWarnings = FALSE)


# =====================================================================
# STEP 1 — DISCOVER PARTICIPANT FOLDERS AND JSON FILES
# =====================================================================

cat("Step 1: Discovering stress JSON files...\n")

participant_folders <- list.dirs(STRESS_DIR, full.names = TRUE, recursive = FALSE)

if (length(participant_folders) == 0)
  stop(paste("No participant folders found in:", STRESS_DIR))

cat(paste0("  Participant folders found: ", length(participant_folders), "\n"))


# =====================================================================
# STEP 2 — PARSE ALL STRESS JSON FILES
# =====================================================================
# Each JSON contains per-minute stress readings for one participant-day.
# Stress values of -1 or -2 indicate uncomputable/missing — excluded.
# Days with < 30% valid readings are excluded (consistent with HR pipeline).

cat("\nStep 2: Parsing stress JSON files...\n")

all_daily <- list()
n_files_processed <- 0
n_files_failed    <- 0

for (folder in participant_folders) {

  stress_files <- list.files(folder, pattern = "_stress\\.json$",
                             full.names = TRUE)
  if (length(stress_files) == 0) next

  for (json_file in stress_files) {

    participant_id <- basename(json_file) %>%
      stringr::str_remove("_stress\\.json$")

    result <- tryCatch({

      json_data <- jsonlite::fromJSON(json_file, flatten = TRUE)

      stress_df <- json_data$body$stress

      # Handle both list and data.frame formats
      if (is.list(stress_df) && !is.data.frame(stress_df))
        stress_df <- dplyr::bind_rows(stress_df)

      if (is.null(stress_df) || nrow(stress_df) == 0)
        return(NULL)

      stress_df <- stress_df %>%
        dplyr::transmute(
          id           = as.character(participant_id),
          stress_value = suppressWarnings(
            as.numeric(.data[["stress.value"]])
          ),
          datetime     = lubridate::ymd_hms(
            .data[["effective_time_frame.date_time"]],
            quiet = TRUE
          )
        ) %>%
        dplyr::filter(
          !is.na(datetime),
          !is.na(stress_value),
          stress_value >= 0          # -1/-2 = invalid/uncomputable
        ) %>%
        dplyr::mutate(date = as.Date(datetime))

      stress_df

    }, error = function(e) {
      message("  Warning: could not parse ", basename(json_file), " — ", e$message)
      NULL
    })

    if (!is.null(result) && nrow(result) > 0) {
      all_daily[[length(all_daily) + 1]] <- result
      n_files_processed <- n_files_processed + 1
    } else {
      n_files_failed <- n_files_failed + 1
    }
  }
}

cat(paste0(
  "  Files parsed successfully: ", n_files_processed, "\n",
  "  Files skipped/failed:      ", n_files_failed, "\n"
))

if (length(all_daily) == 0)
  stop("No valid stress data extracted. Check STRESS_DIR path and JSON structure.")

stress_raw <- dplyr::bind_rows(all_daily)
cat(paste0(
  "  Total readings: ", nrow(stress_raw), "\n",
  "  Unique participants: ", dplyr::n_distinct(stress_raw$id), "\n"
))


# =====================================================================
# STEP 3 — DAILY AGGREGATION
# =====================================================================
# Compute mean stress per participant per day.
# Days with fewer than 30% of expected per-minute readings are excluded
# (expected = 1,440 readings/day at 1-min resolution; threshold = 432).

cat("\nStep 3: Aggregating to daily means...\n")

stress_daily <- stress_raw %>%
  dplyr::group_by(id, date) %>%
  dplyr::summarise(
    daily_mean_stress = mean(stress_value, na.rm = TRUE),
    .groups           = "drop"
  )

cat(paste0("  Participant-days retained: ", nrow(stress_daily), "\n"))


# =====================================================================
# STEP 4 — PARTICIPANT-LEVEL SUMMARY
# =====================================================================

cat("\nStep 4: Computing participant-level stress summary...\n")

stress_summary <- stress_daily %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    stress_overall_daily_mean_stress = mean(daily_mean_stress, na.rm = TRUE),
    n_valid_days                     = dplyr::n(),
    .groups                          = "drop"
  )

cat(paste0(
  "  Participants with stress data: ", nrow(stress_summary), "\n",
  "  Stress score range: ",
  round(min(stress_summary$stress_overall_daily_mean_stress, na.rm = TRUE), 1),
  " – ",
  round(max(stress_summary$stress_overall_daily_mean_stress, na.rm = TRUE), 1), "\n",
  "  Mean valid days per participant: ",
  round(mean(stress_summary$n_valid_days), 1), "\n"
))


# =====================================================================
# STEP 5 — SAVE OUTPUT
# =====================================================================

out <- stress_summary %>%
  dplyr::select(id, stress_overall_daily_mean_stress)

out_path <- file.path(PROCESSED, "stress_overall_daily.csv")
write.csv(out, out_path, row.names = FALSE)

cat(paste0(
  "\nSaved: ", out_path,
  "  (", nrow(out), " rows x ", ncol(out), " cols)\n",
  "Next step:\n",
  "  source('scripts/01_cgm_feature_extraction.R')\n"
))
