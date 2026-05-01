# =====================================================================
# 02_data_preparation.R
# DATA PREPARATION PIPELINE — AI-READI INSULIN RESISTANCE PREDICTION
# =====================================================================
#
# Run from the REPOSITORY ROOT:
#   setwd("/path/to/IR-prediction-")
#   source("scripts/02_data_preparation.R")
#
# INPUT FILES:
#   data/raw/biomarker_data.csv                  — clinical biomarkers
#   data/raw/participants.tsv                    — site, study group, device flags
#   data/raw/observation_in_.csv                 — observation data (export from
#                                                  observation_in_.numbers via
#                                                  File > Export To > CSV)
#   data/processed/stress_overall_daily.csv            — Garmin daily stress score
#   data/processed/hr_participant_summary_daynight.csv — Garmin HR day/night summary
#   data/processed/cgm_features.csv             — CGM-derived features
#   data/processed/participant_ids_study1.csv   — sanity check IDs (not used for filtering)
#   data/processed/participant_ids_study2.csv   — sanity check IDs (not used for filtering)
#
# OUTPUT FILES:
#   data/analysis_ready/df_study1_modelC.csv    — Study 1 analysis-ready (n=97)
#   data/analysis_ready/df_study2_modelC.csv    — Study 2 analysis-ready (n=61)
#
# PRISMA FLOW (Figure S1):
#   Start                          n = 1,067
#   -insulin_dependent             n =   130 excluded → 937
#   -fasting < 8 hours             n =   680 excluded → 257
#   -HbA1c >= 6.5%                 n =    62 excluded → 195
#   -site split + wearable filter  Study1: → 99; Study2: → 63
#   -HOMA-IR missing               Study1: → 99; Study2: → 62
#   -HOMA-IR outliers (med+3SD)    Study1: → 97; Study2: → 61
# =====================================================================

library(dplyr)
library(tidyr)

# ── Paths ─────────────────────────────────────────────────────────────
RAW       <- "data/raw"
PROCESSED <- "data/processed"
OUT       <- "data/analysis_ready"

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)


# =====================================================================
# STEP 1 — LOAD FILES
# =====================================================================

cat("Step 1: Loading files...\n")

biomark  <- read.csv(file.path(RAW, "biomarker_data.csv"),
                     stringsAsFactors = FALSE)
parts    <- read.delim(file.path(RAW, "participants.tsv"),
                       stringsAsFactors = FALSE)
ob_raw   <- read.csv(file.path(RAW, "observation_in_.csv"),
                     stringsAsFactors = FALSE, na.strings = c("", "NA"))
stress   <- read.csv(file.path(RAW, "stress_overall_daily.csv"),
                     stringsAsFactors = FALSE)
hr       <- read.csv(file.path(RAW, "hr_participant_summary_daynight.csv"),
                     stringsAsFactors = FALSE)
cgm      <- read.csv(file.path(PROCESSED, "cgm_features.csv"),
                     stringsAsFactors = FALSE)

# Sanity check ID lists — loaded for end-of-pipeline verification only
ids_s1   <- read.csv(file.path(PROCESSED, "participant_ids_study1.csv"),
                     stringsAsFactors = FALSE)
ids_s2   <- read.csv(file.path(PROCESSED, "participant_ids_study2.csv"),
                     stringsAsFactors = FALSE)

cat(paste0(
  "  biomarker_data.csv:                  ", nrow(biomark), " rows\n",
  "  participants.tsv:                    ", nrow(parts),   " rows\n",
  "  observation_in_.csv:                 ", nrow(ob_raw),  " rows\n",
  "  cgm_features.csv:                    ", nrow(cgm),     " rows\n",
  "  stress_overall_daily.csv:            ", nrow(stress),  " rows\n",
  "  hr_participant_summary_daynight.csv: ", nrow(hr),      " rows\n"
))


# =====================================================================
# STEP 2 — EXTRACT FASTING HOURS FROM OBSERVATION FILE
# =====================================================================
# The fasting hours question is tagged "paate" in observation_source_value.
# value_as_string holds the numeric response (hours since last meal).
#
# Two possible CSV export layouts:
#   Layout A: columns already named (observation_source_value present)
#   Layout B: all data in column 1, comma-separated — requires splitting

cat("\nStep 2: Extracting fasting hours from observation file...\n")

has_source_col <- "observation_source_value" %in% names(ob_raw)
has_value_col  <- "value_as_string"          %in% names(ob_raw)

if (has_source_col && has_value_col) {
  cat("  Layout A: columns already parsed.\n")
  ob <- ob_raw
} else {
  cat("  Layout B: splitting column 1 by comma.\n")
  ob <- ob_raw %>%
    tidyr::separate(
      col  = 1,
      into = c(
        "observation_id", "person_id", "observation_concept_id",
        "observation_date", "observation_datetime",
        "observation_type_concept_id", "value_as_number",
        "value_as_string", "value_as_concept_id",
        "qualifier_concept_id", "unit_concept_id", "provider_id",
        "visit_occurrence_id", "visit_detail_id",
        "observation_source_value", "observation_source_concept_id",
        "unit_source_value", "value_source_value",
        "value_source_concept_id", "extra1", "extra2"
      ),
      sep   = ",",
      fill  = "right",
      extra = "merge"
    )
}

fasting <- ob %>%
  dplyr::filter(grepl("paate", observation_source_value, ignore.case = TRUE)) %>%
  dplyr::transmute(
    participant_id = as.integer(trimws(person_id)),
    fasting_hours  = as.numeric(trimws(value_as_string))
  ) %>%
  dplyr::filter(!is.na(participant_id), !is.na(fasting_hours)) %>%
  dplyr::group_by(participant_id) %>%
  dplyr::summarise(fasting_hours = max(fasting_hours, na.rm = TRUE),
                   .groups = "drop")

cat(paste0(
  "  Participants with fasting data: ", nrow(fasting), "\n",
  "  Fasting hours range: ",  min(fasting$fasting_hours),
  " – ", max(fasting$fasting_hours), " hours\n",
  "  Fasting >= 8h: ", sum(fasting$fasting_hours >= 8),
  "  Fasting < 8h:  ", sum(fasting$fasting_hours < 8), "\n"
))


# =====================================================================
# STEP 3 — MERGE BIOMARKER + PARTICIPANTS + FASTING HOURS
# =====================================================================

cat("\nStep 3: Merging core data...\n")

df <- biomark %>%
  dplyr::distinct(person_id, .keep_all = TRUE) %>%
  dplyr::inner_join(
    parts %>% dplyr::distinct(participant_id, .keep_all = TRUE),
    by = c("person_id" = "participant_id")
  ) %>%
  dplyr::inner_join(
    fasting %>% dplyr::select(participant_id, fasting_hours),
    by = c("person_id" = "participant_id")
  ) %>%
  dplyr::rename(participant_id = person_id)

cat(paste0("  After merge: n=", nrow(df), " (expected 1,067)\n"))


# =====================================================================
# STEP 4 — PARTICIPANT EXCLUSION CRITERIA (PRISMA steps a–c)
# =====================================================================

cat("\nStep 4: Applying exclusion criteria...\n")

# (a) Insulin-dependent: HOMA-IR uninterpretable under exogenous insulin
n_before <- nrow(df)
df <- df %>% dplyr::filter(study_group != "insulin_dependent")
cat(paste0("  (a) -insulin_dependent: n=", n_before - nrow(df),
           " excluded → ", nrow(df), " (expected: excl=130, n=937)\n"))

# (b) Fasting < 8h: valid HOMA-IR requires >= 8h fast
n_before <- nrow(df)
df <- df %>% dplyr::filter(fasting_hours >= 8)
cat(paste0("  (b) -fasting < 8h: n=", n_before - nrow(df),
           " excluded → ", nrow(df), " (expected: excl=680, n=257)\n"))

# (c) HbA1c >= 6.5%: T2D diagnosis; exceeds AI-READI eligibility threshold
#     Participants with missing HbA1c are retained at this stage
n_before <- nrow(df)
df <- df %>% dplyr::filter(is.na(`HbA1c....`) | `HbA1c....` < 6.5)
cat(paste0("  (c) -HbA1c >= 6.5%: n=", n_before - nrow(df),
           " excluded → ", nrow(df), " (expected: excl=62, n=195)\n"))


# =====================================================================
# STEP 5 — SPLIT BY SITE
# =====================================================================

cat("\nStep 5: Splitting by site...\n")

df_s1 <- df %>% dplyr::filter(clinical_site %in% c("UW", "UCSD"))
df_s2 <- df %>% dplyr::filter(clinical_site == "UAB")

cat(paste0(
  "  Study 1 (UW+UCSD): n=", nrow(df_s1), " (expected 123, excl=72)\n",
  "  Study 2 (UAB):     n=", nrow(df_s2), " (expected 72,  excl=123)\n"
))


# =====================================================================
# STEP 6 — EXCLUDE PARTICIPANTS WITHOUT WEARABLE DATA
# =====================================================================

cat("\nStep 6: Applying wearable data filter...\n")

hr_ids     <- unique(hr$participant_id)
stress_ids <- unique(stress$id)

has_wearable <- function(df) {
  df %>%
    dplyr::filter(
      wearable_blood_glucose == TRUE,
      participant_id %in% hr_ids,
      participant_id %in% stress_ids
    )
}

n_before <- nrow(df_s1); df_s1 <- has_wearable(df_s1)
cat(paste0("  Study 1: -", n_before - nrow(df_s1),
           " → ", nrow(df_s1), " (expected: excl=24, n=99)\n"))

n_before <- nrow(df_s2); df_s2 <- has_wearable(df_s2)
cat(paste0("  Study 2: -", n_before - nrow(df_s2),
           " → ", nrow(df_s2), " (expected: excl=9, n=63)\n"))


# =====================================================================
# STEP 7 — COMPUTE HOMA-IR AND EXCLUDE MISSING
# =====================================================================
# HOMA-IR = (fasting glucose [mmol/L] x fasting insulin [mU/L]) / 22.5
# glucose: mg/dL x 0.0555 = mmol/L
# insulin: ng/mL x 6.945  = mU/L

cat("\nStep 7: Computing HOMA-IR and excluding missing values...\n")

add_homa <- function(df) {
  df %>% dplyr::mutate(
    HOMA_IR = (`Glucose..mg.dL.` * 0.0555 * `INSULIN..ng.mL.` * 6.945) / 22.5
  )
}

df_s1 <- add_homa(df_s1); n <- nrow(df_s1)
df_s1 <- df_s1 %>% dplyr::filter(!is.na(HOMA_IR))
cat(paste0("  Study 1: -", n - nrow(df_s1),
           " → ", nrow(df_s1), " (expected: excl=0, n=99)\n"))

df_s2 <- add_homa(df_s2); n <- nrow(df_s2)
df_s2 <- df_s2 %>% dplyr::filter(!is.na(HOMA_IR))
cat(paste0("  Study 2: -", n - nrow(df_s2),
           " → ", nrow(df_s2), " (expected: excl=1, n=62)\n"))


# =====================================================================
# STEP 8 — REMOVE HOMA-IR OUTLIERS (median + 3 SD, per study)
# =====================================================================

cat("\nStep 8: Removing HOMA-IR outliers...\n")

rm_outliers <- function(df, label, n_expected) {
  cutoff   <- median(df$HOMA_IR, na.rm = TRUE) + 3 * sd(df$HOMA_IR, na.rm = TRUE)
  n_before <- nrow(df)
  df       <- df %>% dplyr::filter(HOMA_IR <= cutoff)
  cat(paste0("  ", label, ": cutoff=", round(cutoff, 2),
             " → -", n_before - nrow(df), " outliers → n=", nrow(df),
             " (expected n=", n_expected, ")\n"))
  df
}

df_s1 <- rm_outliers(df_s1, "Study 1", 97)
df_s2 <- rm_outliers(df_s2, "Study 2", 61)


# =====================================================================
# STEP 9 — BINARISE HOMA-IR → IR_LABEL
# =====================================================================

cat("\nStep 9: Binarising HOMA-IR at cutoff 2.9...\n")

df_s1 <- df_s1 %>%
  dplyr::mutate(IR_label = dplyr::if_else(HOMA_IR >= 2.9, "IR", "Non-IR"))
df_s2 <- df_s2 %>%
  dplyr::mutate(IR_label = dplyr::if_else(HOMA_IR >= 2.9, "IR", "Non-IR"))

cat(paste0(
  "  Study 1: IR=", sum(df_s1$IR_label == "IR"),
  " Non-IR=", sum(df_s1$IR_label == "Non-IR"),
  " (expected IR=32, Non-IR=65)\n",
  "  Study 2: IR=", sum(df_s2$IR_label == "IR"),
  " Non-IR=", sum(df_s2$IR_label == "Non-IR"),
  " (expected IR=19, Non-IR=42)\n"
))


# =====================================================================
# STEP 10 — MERGE CGM FEATURES
# =====================================================================
# Source: data/processed/cgm_features.csv
# Features: mean_fasting_glucose, sd_fasting_glucose, excursions_per_day,
#           overall_max_glucose, rec_time_50pct_median

cat("\nStep 10: Merging CGM features...\n")

cgm_clean <- cgm %>% dplyr::distinct(participant_id, .keep_all = TRUE)
df_s1 <- df_s1 %>% dplyr::left_join(cgm_clean, by = "participant_id")
df_s2 <- df_s2 %>% dplyr::left_join(cgm_clean, by = "participant_id")

cat(paste0(
  "  Study 1: ", sum(!is.na(df_s1$mean_fasting_glucose)), "/97 with CGM data\n",
  "  Study 2: ", sum(!is.na(df_s2$mean_fasting_glucose)), "/61 with CGM data\n"
))


# =====================================================================
# STEP 11 — MERGE GARMIN STRESS + HR DAY-NIGHT VARIABILITY
# =====================================================================
# stress_overall_daily_mean_stress: mean daily HRV stress (0-100)
#   Source: data/raw/stress_overall_daily.csv (id -> participant_id)
#   Expected missingness: Study1 1/97, Study2 0/61
#
# sd_hr_day_night_diff: SD of (daytime HR - nocturnal HR) across days
#   Source: data/raw/hr_participant_summary_daynight.csv
#   Sole missingness cause: absent Garmin sleep window
#   Expected: Study1 7/97, Study2 2/61

cat("\nStep 11: Merging Garmin stress and HR day-night variability...\n")

stress_clean <- stress %>%
  dplyr::rename(participant_id = id) %>%
  dplyr::distinct(participant_id, .keep_all = TRUE) %>%
  dplyr::select(participant_id, stress_overall_daily_mean_stress)

hr_clean <- hr %>%
  dplyr::distinct(participant_id, .keep_all = TRUE) %>%
  dplyr::select(participant_id, sd_hr_day_night_diff)

df_s1 <- df_s1 %>%
  dplyr::left_join(stress_clean, by = "participant_id") %>%
  dplyr::left_join(hr_clean,     by = "participant_id")

df_s2 <- df_s2 %>%
  dplyr::left_join(stress_clean, by = "participant_id") %>%
  dplyr::left_join(hr_clean,     by = "participant_id")

cat(paste0(
  "  stress NA: Study1=", sum(is.na(df_s1$stress_overall_daily_mean_stress)),
  " (exp 1)  Study2=", sum(is.na(df_s2$stress_overall_daily_mean_stress)), " (exp 0)\n",
  "  sd_hr  NA: Study1=", sum(is.na(df_s1$sd_hr_day_night_diff)),
  " (exp 7)  Study2=", sum(is.na(df_s2$sd_hr_day_night_diff)), " (exp 2)\n"
))


# =====================================================================
# STEP 12 — SELECT FINAL COLUMNS
# =====================================================================

cat("\nStep 12: Selecting final columns...\n")

select_final <- function(df) {
  df %>%
    dplyr::transmute(
      participant_id,
      IR_label,
      HOMA_IR,
      bmi  = bmi_vsorres..BMI,
      age,
      whr  = whr_vsorres..Waist.to.Hip.Ratio..WHR.,
      mean_fasting_glucose,
      sd_fasting_glucose,
      excursions_per_day,
      overall_max_glucose,
      rec_time_50pct_median,
      stress_overall_daily_mean_stress,
      sd_hr_day_night_diff
    )
}

df_study1 <- select_final(df_s1)
df_study2 <- select_final(df_s2)


# =====================================================================
# STEP 13 — MISSINGNESS VERIFICATION
# =====================================================================

cat("\nStep 13: Missingness verification\n")

expected_na <- list(
  mean_fasting_glucose             = c(6,  2),
  sd_fasting_glucose               = c(10, 4),
  excursions_per_day               = c(3,  0),
  overall_max_glucose              = c(1,  0),
  rec_time_50pct_median            = c(3,  0),
  stress_overall_daily_mean_stress = c(1,  0),
  sd_hr_day_night_diff             = c(7,  2),
  bmi                              = c(0,  0),
  age                              = c(0,  0),
  whr                              = c(0,  0),
  HOMA_IR                          = c(0,  0)
)

cat(sprintf("  %-38s %12s %12s  Status\n", "Feature", "S1 Exp|Act", "S2 Exp|Act"))
cat(paste(rep("-", 78), collapse = ""), "\n")
all_ok <- TRUE
for (col in names(expected_na)) {
  e1 <- expected_na[[col]][1]; e2 <- expected_na[[col]][2]
  a1 <- sum(is.na(df_study1[[col]])); a2 <- sum(is.na(df_study2[[col]]))
  ok <- (a1 == e1) && (a2 == e2)
  if (!ok) all_ok <- FALSE
  cat(sprintf("  %-38s %5d|%-5d %5d|%-5d  %s\n",
              col, e1, a1, e2, a2, if (ok) "OK" else "*** MISMATCH ***"))
}
cat(if (all_ok) "\n  All missingness counts match.\n"
    else "\n  *** Missingness mismatches — check above. ***\n")


# =====================================================================
# STEP 14 — SANITY CHECK AGAINST VERIFIED PARTICIPANT ID LISTS
# =====================================================================
# participant_ids_study1/2.csv are NOT used for filtering.
# They serve only to confirm the pipeline recovers the correct samples.

cat("\nStep 14: Sanity check against verified participant ID lists\n")

check_ids <- function(derived, verified, label) {
  d <- sort(derived$participant_id)
  v <- sort(verified$participant_id)
  n_match   <- sum(d %in% v)
  n_extra   <- sum(!d %in% v)
  n_missing <- sum(!v %in% d)
  cat(paste0("  ", label, ":  derived n=", length(d),
             "  verified n=", length(v), "\n",
             "    Matching: ", n_match,
             "  Extra: ", n_extra,
             "  Missing: ", n_missing, "\n"))
  if (n_extra == 0 && n_missing == 0) {
    cat("    \u2713 Perfect match.\n")
  } else {
    cat("    \u26a0 Mismatch — likely due to observation_in_.csv export.\n")
  }
}

check_ids(df_study1, ids_s1, "Study 1")
check_ids(df_study2, ids_s2, "Study 2")


# =====================================================================
# STEP 15 — SAVE OUTPUTS
# =====================================================================

cat("\nStep 15: Saving outputs...\n")

write.csv(df_study1,
          file.path(OUT, "df_study1_modelC.csv"),
          row.names = FALSE)
write.csv(df_study2,
          file.path(OUT, "df_study2_modelC.csv"),
          row.names = FALSE)

cat(paste0(
  "  Saved: ", file.path(OUT, "df_study1_modelC.csv"),
  "  (n=", nrow(df_study1), ")\n",
  "  Saved: ", file.path(OUT, "df_study2_modelC.csv"),
  "  (n=", nrow(df_study2), ")\n",
  "\nNext step:\n",
  "  source('scripts/03_rf_modeling.R')\n"
))
