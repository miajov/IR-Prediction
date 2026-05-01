# =====================================================================
# 01_cgm_feature_extraction.R
# CGM FEATURE EXTRACTION — AI-READI INSULIN RESISTANCE PREDICTION
# =====================================================================
#
# Run from the REPOSITORY ROOT:
#   setwd("/path/to/IR-prediction")
#   source("scripts/01_cgm_feature_extraction.R")
#
# INPUT FILES:
#   data/raw/cgm_data_all.csv              — raw CGM readings (all participants)
#   data/raw/sleep_all_participants.csv    — Garmin sleep/wake timestamps + timezones
#   data/processed/participant_ids_study1.csv — verified Study 1 IDs (n=97)
#   data/processed/participant_ids_study2.csv — verified Study 2 IDs (n=61)
#
# OUTPUT FILES:
#   data/processed/cgm_features.csv       — one row per participant, 6 columns:
#     participant_id, mean_fasting_glucose, sd_fasting_glucose,
#     excursions_per_day, overall_max_glucose, rec_time_50pct_median
#
# FEATURE DEFINITIONS:
#   mean_fasting_glucose   — mean of daily mean wake-to-first-meal glucose (mg/dL)
#   sd_fasting_glucose     — SD of daily mean fasting glucose across monitoring days
#   excursions_per_day     — mean confirmed daytime glucose excursions per day
#   overall_max_glucose    — maximum interstitial glucose across monitoring period
#   rec_time_50pct_median  — median time (min) from excursion peak to 50% recovery
# =====================================================================

library(dplyr)
library(tidyr)

# ── Paths ─────────────────────────────────────────────────────────────
RAW       <- "data/raw"
PROCESSED <- "data/processed"


# =====================================================================
# STEP 1 — LOAD FILES
# =====================================================================

cat("Step 1: Loading files...\n")

cgm_raw   <- read.csv(file.path(RAW,       "cgm_data_all.csv"),
                      stringsAsFactors = FALSE)
sleep_raw <- read.csv(file.path(RAW,       "sleep_all_participants.csv"),
                      stringsAsFactors = FALSE)
ids_s1    <- read.csv(file.path(PROCESSED, "participant_ids_study1.csv"),
                      stringsAsFactors = FALSE)
ids_s2    <- read.csv(file.path(PROCESSED, "participant_ids_study2.csv"),
                      stringsAsFactors = FALSE)

study1_ids <- as.character(ids_s1$participant_id)   # n = 97
study2_ids <- as.character(ids_s2$participant_id)   # n = 61
all_ids    <- unique(c(study1_ids, study2_ids))      # n = 158

cat(paste0(
  "  cgm_data_all.csv:             ", nrow(cgm_raw),   " rows\n",
  "  sleep_all_participants.csv:   ", nrow(sleep_raw), " rows\n",
  "  Study 1 IDs: ", length(study1_ids),
  "  Study 2 IDs: ", length(study2_ids), "\n"
))

# Prepare cgm_obs
cgm_obs <- cgm_raw %>%
  dplyr::filter(as.character(id) %in% all_ids) %>%
  dplyr::transmute(
    folder_id              = as.character(id),
    time_utc               = as.POSIXct(time, tz = "UTC"),
    excusions_num_blood_gl = as.numeric(gl),
    local_date             = as.Date(as.POSIXct(time, tz = "UTC"))
  )

# Prepare sleep_selected
sleep_selected <- sleep_raw %>%
  dplyr::filter(as.character(participant_id) %in% all_ids) %>%
  dplyr::mutate(participant_id = as.character(participant_id))

cat(paste0(
  "  CGM rows for analysis participants: ", nrow(cgm_obs), "\n",
  "  Sleep records for analysis participants: ", nrow(sleep_selected), "\n\n"
))


# =====================================================================
# SHARED HELPERS
# =====================================================================

local_date_hms_to_utc <- function(date_value, hms_value, tz_value) {
  local_ts <- as.POSIXct(
    paste(as.character(date_value), hms_value),
    format = "%Y-%m-%d %H:%M:%S",
    tz     = tz_value
  )
  as.POSIXct(as.numeric(local_ts), origin = "1970-01-01", tz = "UTC")
}

tz_map <- sleep_selected %>%
  dplyr::mutate(folder_id = as.character(participant_id)) %>%
  dplyr::group_by(folder_id) %>%
  dplyr::summarise(timezone = dplyr::first(timezone), .groups = "drop")

cgm_obs <- cgm_obs %>%
  dplyr::mutate(folder_id = as.character(folder_id)) %>%
  dplyr::left_join(tz_map, by = "folder_id")

cgm_obs$local_hour <- vapply(
  seq_len(nrow(cgm_obs)),
  function(i) {
    tz <- cgm_obs$timezone[i]
    if (is.na(tz) || !nzchar(tz)) return(NA_integer_)
    tryCatch(
      as.integer(format(cgm_obs$time_utc[i], tz = tz, format = "%H")),
      error = function(e) NA_integer_
    )
  },
  FUN.VALUE = integer(1)
)


# =====================================================================
# FEATURES 1 & 2 — FASTING MEAN AND SD GLUCOSE
# =====================================================================
# Fasting window: wake time → first sustained glucose rise (>= 15 mg/dL
# above wake baseline, confirmed in two consecutive readings <= 15 min apart).
# Late-riser fix: participants waking at or after 09:00 local have the
# ceiling extended to wake + 3 hours rather than a fixed 09:00 ceiling.

cat("Step 2: Computing fasting glucose features...\n")

sleep_end_no_colon <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2",
                           sleep_selected$sleep_end)
sleep_selected$wake_time_utc <- as.POSIXct(
  sleep_end_no_colon,
  format = "%Y-%m-%dT%H:%M:%S%z",
  tz     = "UTC"
)
sleep_selected <- sleep_selected %>%
  dplyr::filter(!is.na(wake_time_utc), !is.na(timezone), nzchar(timezone))

sleep_selected$wake_local_date <- as.Date(vapply(
  seq_len(nrow(sleep_selected)),
  function(i) format(
    sleep_selected$wake_time_utc[i],
    tz     = sleep_selected$timezone[i],
    format = "%Y-%m-%d"
  ),
  FUN.VALUE = character(1)
))

sleep_selected$morning_start_utc <- as.POSIXct(vapply(
  seq_len(nrow(sleep_selected)),
  function(i) as.numeric(local_date_hms_to_utc(
    sleep_selected$wake_local_date[i], "06:00:00", sleep_selected$timezone[i]
  )),
  FUN.VALUE = numeric(1)
), origin = "1970-01-01", tz = "UTC")

sleep_selected$morning_end_utc <- as.POSIXct(vapply(
  seq_len(nrow(sleep_selected)),
  function(i) {
    fixed_end <- as.numeric(local_date_hms_to_utc(
      sleep_selected$wake_local_date[i], "09:00:00", sleep_selected$timezone[i]
    ))
    wake <- as.numeric(sleep_selected$wake_time_utc[i])
    if (!is.na(wake) && wake >= fixed_end) wake + 3 * 3600 else fixed_end
  },
  FUN.VALUE = numeric(1)
), origin = "1970-01-01", tz = "UTC")

window_base <- sleep_selected %>%
  dplyr::transmute(
    folder_id       = as.character(participant_id),
    local_date      = as.Date(wake_local_date),
    timezone        = timezone,
    wake_time_utc   = wake_time_utc,
    morning_end_utc = morning_end_utc
  ) %>%
  dplyr::group_by(folder_id, local_date, timezone) %>%
  dplyr::summarise(
    wake_time_utc   = min(wake_time_utc,   na.rm = TRUE),
    morning_end_utc = min(morning_end_utc, na.rm = TRUE),
    .groups = "drop"
  )

first_elevated_time_fn <- function(time_vec, gl_vec) {
  ok       <- !is.na(time_vec) & !is.na(gl_vec)
  time_vec <- time_vec[ok]; gl_vec <- gl_vec[ok]
  if (length(gl_vec) < 2) return(NA_real_)
  baseline  <- gl_vec[1]
  threshold <- baseline + 15
  idx       <- which(gl_vec >= threshold)
  if (length(idx) == 0) return(NA_real_)
  for (j in idx) {
    if (j < length(gl_vec) &&
        gl_vec[j + 1] >= threshold &&
        as.numeric(difftime(time_vec[j + 1], time_vec[j], units = "mins")) <= 15)
      return(as.numeric(time_vec[j]))
  }
  NA_real_
}

morning_candidates <- cgm_obs %>%
  dplyr::inner_join(window_base, by = c("folder_id", "local_date")) %>%
  dplyr::filter(time_utc >= wake_time_utc, time_utc <= morning_end_utc) %>%
  dplyr::arrange(folder_id, local_date, time_utc)

elevated_end <- morning_candidates %>%
  dplyr::group_by(folder_id, local_date) %>%
  dplyr::summarise(
    first_elevated_num = first_elevated_time_fn(time_utc, excusions_num_blood_gl),
    morning_end_num    = as.numeric(min(morning_end_utc, na.rm = TRUE)),
    wake_num           = as.numeric(min(wake_time_utc,   na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    end_num = ifelse(is.na(first_elevated_num), morning_end_num, first_elevated_num),
    end_num = pmax(end_num, wake_num)
  ) %>%
  dplyr::transmute(
    folder_id,
    local_date,
    fasting_start_utc = as.POSIXct(wake_num, origin = "1970-01-01", tz = "UTC"),
    fasting_end_utc   = as.POSIXct(end_num,  origin = "1970-01-01", tz = "UTC")
  )

fasting_daily <- cgm_obs %>%
  dplyr::inner_join(elevated_end, by = c("folder_id", "local_date")) %>%
  dplyr::filter(
    time_utc >= fasting_start_utc,
    time_utc <= fasting_end_utc
  ) %>%
  dplyr::group_by(folder_id, local_date) %>%
  dplyr::summarise(
    mean_fasting_glucose_daily = mean(excusions_num_blood_gl, na.rm = TRUE),
    n_fasting_points           = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_fasting_points >= 3)

fasting_summary <- fasting_daily %>%
  dplyr::group_by(folder_id) %>%
  dplyr::summarise(
    mean_fasting_glucose = mean(mean_fasting_glucose_daily, na.rm = TRUE),
    sd_fasting_glucose   = sd(mean_fasting_glucose_daily,   na.rm = TRUE),
    n_fasting_days       = dplyr::n(),
    .groups = "drop"
  )

cat(paste0("  Fasting summary computed for: ", nrow(fasting_summary), " participants\n"))


# =====================================================================
# FEATURE 3 — EXCURSIONS PER DAY
# =====================================================================

cat("Step 3: Computing excursions per day...\n")

detect_excursion_count <- function(time_vec, gl_vec, min_rise = 15) {
  n <- length(gl_vec); if (n < 4) return(0L)
  count <- 0L; i <- 1L
  while (i <= n - 3L) {
    baseline  <- median(gl_vec[max(1L, i - 2L):i], na.rm = TRUE)
    threshold <- baseline + min_rise
    peak_idx  <- i
    for (j in seq(i + 1L, min(i + 48L, n))) {
      if (!is.na(gl_vec[j]) && gl_vec[j] > gl_vec[peak_idx]) peak_idx <- j
    }
    if (peak_idx == i || gl_vec[peak_idx] < threshold) { i <- i + 1L; next }
    confirmed <- FALSE
    for (j in seq(i + 1L, min(peak_idx, n - 1L))) {
      if (!is.na(gl_vec[j]) && !is.na(gl_vec[j + 1L]) &&
          gl_vec[j] >= threshold && gl_vec[j + 1L] >= threshold &&
          as.numeric(difftime(time_vec[j + 1L], time_vec[j], units = "mins")) <= 15) {
        confirmed <- TRUE; break
      }
    }
    if (confirmed) {
      count     <- count + 1L
      rec_thresh <- baseline + 0.2 * (gl_vec[peak_idx] - baseline)
      advanced  <- FALSE
      for (k in seq(peak_idx + 1L, min(peak_idx + 36L, n))) {
        if (!is.na(gl_vec[k]) && gl_vec[k] <= rec_thresh) {
          i <- k; advanced <- TRUE; break
        }
      }
      if (!advanced) i <- min(peak_idx + 36L, n) + 1L
    } else {
      i <- i + 1L
    }
  }
  count
}

cgm_daytime <- cgm_obs %>%
  dplyr::filter(!is.na(local_hour), local_hour >= 6, local_hour < 22) %>%
  dplyr::arrange(folder_id, local_date, time_utc)

excursions_daily <- cgm_daytime %>%
  dplyr::group_by(folder_id, local_date) %>%
  dplyr::filter(dplyr::n() >= 48) %>%
  dplyr::summarise(
    n_excursions       = detect_excursion_count(time_utc, excusions_num_blood_gl),
    n_daytime_readings = dplyr::n(),
    .groups = "drop"
  )

excursions_summary <- excursions_daily %>%
  dplyr::group_by(folder_id) %>%
  dplyr::summarise(
    excursions_per_day = mean(n_excursions, na.rm = TRUE),
    .groups = "drop"
  )

cat(paste0("  Excursions computed for: ", nrow(excursions_summary), " participants\n"))


# =====================================================================
# FEATURE 4 — OVERALL MAX GLUCOSE
# =====================================================================

cat("Step 4: Computing overall max glucose...\n")

overall_max_summary <- cgm_obs %>%
  dplyr::group_by(folder_id) %>%
  dplyr::summarise(
    overall_max_glucose = max(excusions_num_blood_gl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    overall_max_glucose = ifelse(
      is.infinite(overall_max_glucose), NA_real_, overall_max_glucose
    )
  )


# =====================================================================
# FEATURE 5 — 50% RECOVERY TIME
# =====================================================================

cat("Step 5: Computing 50% recovery time...\n")

compute_recovery_50pct <- function(time_vec, gl_vec, min_rise = 15) {
  n <- length(gl_vec); if (n < 4) return(numeric(0))
  rec_times <- numeric(0); i <- 1L
  while (i <= n - 3L) {
    baseline  <- median(gl_vec[max(1L, i - 2L):i], na.rm = TRUE)
    threshold <- baseline + min_rise
    peak_idx  <- i; peak_val <- gl_vec[i]
    for (j in seq(i + 1L, min(i + 48L, n))) {
      if (!is.na(gl_vec[j]) && gl_vec[j] > peak_val) {
        peak_val <- gl_vec[j]; peak_idx <- j
      }
    }
    if (peak_idx == i || peak_val < threshold) { i <- i + 1L; next }
    half_threshold <- baseline + 0.5 * (peak_val - baseline)
    found <- FALSE
    for (k in seq(peak_idx + 1L, min(peak_idx + 36L, n))) {
      if (!is.na(gl_vec[k]) && gl_vec[k] <= half_threshold) {
        t_50      <- as.numeric(difftime(time_vec[k], time_vec[peak_idx], units = "mins"))
        rec_times <- c(rec_times, t_50)
        found     <- TRUE; i <- k; break
      }
    }
    if (!found) i <- peak_idx + 1L
  }
  rec_times
}

rec_50pct_daily <- cgm_daytime %>%
  dplyr::group_by(folder_id, local_date) %>%
  dplyr::filter(dplyr::n() >= 48) %>%
  dplyr::summarise(
    daily_rec_times = list(
      compute_recovery_50pct(time_utc, excusions_num_blood_gl)
    ),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    mean_rec_50pct_daily = sapply(
      daily_rec_times,
      function(x) if (length(x) == 0) NA_real_ else mean(x, na.rm = TRUE)
    )
  )

rec_50pct_summary <- rec_50pct_daily %>%
  dplyr::group_by(folder_id) %>%
  dplyr::summarise(
    rec_time_50pct_median = median(mean_rec_50pct_daily, na.rm = TRUE),
    .groups = "drop"
  )


# =====================================================================
# STEP 6 — MERGE ALL FEATURES
# =====================================================================

cat("\nStep 6: Merging all features...\n")

cgm_features <- fasting_summary %>%
  dplyr::select(folder_id, mean_fasting_glucose, sd_fasting_glucose) %>%
  dplyr::full_join(excursions_summary,  by = "folder_id") %>%
  dplyr::full_join(overall_max_summary, by = "folder_id") %>%
  dplyr::full_join(
    rec_50pct_summary %>% dplyr::select(folder_id, rec_time_50pct_median),
    by = "folder_id"
  ) %>%
  dplyr::rename(participant_id = folder_id) %>%
  dplyr::filter(participant_id %in% all_ids)

cat(paste0("  Total participants in output: ", nrow(cgm_features), " (expected 158)\n"))


# =====================================================================
# STEP 7 — MISSINGNESS REPORT
# =====================================================================

cat("\nStep 7: Missingness report\n")

feat_cols <- c("mean_fasting_glucose", "sd_fasting_glucose",
               "excursions_per_day", "overall_max_glucose", "rec_time_50pct_median")

cat(sprintf("  %-38s %14s %14s\n", "Feature", "Study1 NA (%)", "Study2 NA (%)"))
cat(paste(rep("-", 70), collapse = ""), "\n")

for (col in feat_cols) {
  s1 <- cgm_features[cgm_features$participant_id %in% study1_ids, ]
  s2 <- cgm_features[cgm_features$participant_id %in% study2_ids, ]
  na1 <- sum(is.na(s1[[col]])); na2 <- sum(is.na(s2[[col]]))
  p1  <- round(na1 / nrow(s1) * 100, 1)
  p2  <- round(na2 / nrow(s2) * 100, 1)
  cat(sprintf("  %-38s %5d (%4.1f%%)  %5d (%4.1f%%)\n", col, na1, p1, na2, p2))
}


# =====================================================================
# STEP 8 — SAVE
# =====================================================================

out_path <- file.path(PROCESSED, "cgm_features.csv")
write.csv(cgm_features, out_path, row.names = FALSE)
cat(paste0("\nSaved: ", out_path, "  (", nrow(cgm_features), " rows x ",
           ncol(cgm_features), " cols)\n"))
cat("Next step:\n  source('scripts/02_data_preparation.R')\n")
