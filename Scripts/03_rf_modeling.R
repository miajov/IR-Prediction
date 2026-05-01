# =====================================================================
# 03_rf_modeling.R
# RANDOM FOREST MODELING — AI-READI INSULIN RESISTANCE PREDICTION
# =====================================================================
#
# Run from the REPOSITORY ROOT:
#   setwd("/path/to/IR-prediction")
#   source("scripts/03_rf_modeling.R")
#
# INPUT FILES:
#   data/analysis_ready/df_study1_modelC.csv  — Study 1 (n=97, internal)
#   data/analysis_ready/df_study2_modelC.csv  — Study 2 (n=61, external)
#
# OUTPUT FILES:
#   results/tables/Table2_main_results.csv    — all metrics with 95% CIs
#
# MODELS:
#   CGM Model        — 8 features: bmi, age, whr + 5 CGM-derived features
#   Smartwatch Model — 5 features: bmi, age, whr + stress + HR day-night
#   Baseline Model   — 3 features: bmi, age, whr
#
# HYPERPARAMETERS (fixed a priori):
#   mtry = 1  |  ntree = 300  |  preProcess = medianImpute
#   Training: 70/30 stratified split, repeated 5-fold CV (20 repeats)
#   Class imbalance: random upsampling within each training fold
#
# CI METHODS:
#   AUC-ROC: DeLong (ci.auc, pROC); bootstrap fallback if DeLong fails
#   All other metrics: 2,000-iteration non-parametric bootstrap
# =====================================================================

library(dplyr)
library(caret)
library(pROC)
library(PRROC)

# ── Paths ─────────────────────────────────────────────────────────────
ANALYSIS_READY <- "data/analysis_ready"
RESULTS        <- "results/tables"

dir.create(RESULTS, recursive = TRUE, showWarnings = FALSE)


# =====================================================================
# STEP 1 — READ DATA
# =====================================================================

cat("Step 1: Loading analysis-ready data...\n")

df_test     <- read.csv(file.path(ANALYSIS_READY, "df_study1_modelC.csv"),
                        stringsAsFactors = FALSE)
df_external <- read.csv(file.path(ANALYSIS_READY, "df_study2_modelC.csv"),
                        stringsAsFactors = FALSE)

cat(paste0(
  "  Study 1 (internal): n=", nrow(df_test),
  "  IR=", sum(df_test$IR_label == "IR"),
  "  Non-IR=", sum(df_test$IR_label == "Non-IR"), "\n",
  "  Study 2 (external): n=", nrow(df_external),
  "  IR=", sum(df_external$IR_label == "IR"),
  "  Non-IR=", sum(df_external$IR_label == "Non-IR"), "\n"
))


# =====================================================================
# STEP 2 — FEATURE SETS
# =====================================================================

feature_sets <- list(
  CGM_model = c(
    "bmi", "mean_fasting_glucose", "sd_fasting_glucose",
    "excursions_per_day", "rec_time_50pct_median",
    "overall_max_glucose", "age", "whr"
  ),
  Smartwatch_model = c(
    "stress_overall_daily_mean_stress", "sd_hr_day_night_diff",
    "age", "bmi", "whr"
  ),
  Baseline_model = c(
    "bmi", "age", "whr"
  )
)


# =====================================================================
# STEP 3 — METRICS FUNCTION
# =====================================================================
# AUC-ROC 95% CI: DeLong method (ci.auc, pROC).
# Bootstrap fallback only if DeLong fails (< 2 positives or negatives).
# All other metric CIs: 2,000-iteration non-parametric bootstrap.

compute_metrics <- function(y_true, y_pred, y_prob,
                             dataset = "Unknown",
                             n_boot  = 2000,
                             seed    = 42) {

  # Point estimates
  cm     <- confusionMatrix(y_pred, y_true)
  acc    <- as.numeric(cm$overall["Accuracy"])
  sens   <- as.numeric(cm$byClass["Sensitivity"])
  spec   <- as.numeric(cm$byClass["Specificity"])
  balacc <- (sens + spec) / 2
  prec   <- as.numeric(cm$byClass["Pos Pred Value"])
  f1     <- ifelse(is.na(prec) | prec + sens == 0, NA_real_,
                   2 * prec * sens / (prec + sens))

  roc_obj <- roc(y_true, y_prob, levels = c("Non.IR", "IR"), quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))

  pr_obj <- pr.curve(
    scores.class0  = y_prob,
    weights.class0 = ifelse(y_true == "IR", 1, 0),
    curve          = FALSE
  )
  prauc <- pr_obj$auc.integral

  # DeLong 95% CI for AUC-ROC
  auc_lo <- NA_real_; auc_hi <- NA_real_; auc_method <- "DeLong"
  tryCatch({
    dl     <- as.numeric(ci.auc(roc_obj, method = "delong", conf.level = 0.95))
    auc_lo <- dl[1]; auc_hi <- dl[3]
  }, error = function(e) {
    auc_method <<- "Bootstrap (DeLong failed)"
  })

  # Bootstrap 95% CIs for all other metrics (and AUC fallback)
  set.seed(seed)
  n   <- length(y_true)
  mat <- matrix(NA_real_, nrow = n_boot, ncol = 8,
                dimnames = list(NULL,
                  c("acc","sens","spec","balacc","prec","f1","auc","prauc")))

  for (i in seq_len(n_boot)) {
    idx_b  <- sample(n, n, replace = TRUE)
    yb     <- y_true[idx_b]
    pb_cls <- y_pred[idx_b]
    pb_pr  <- y_prob[idx_b]
    if (length(unique(yb)) < 2) next

    cm_b  <- confusionMatrix(pb_cls, yb)
    s_b   <- as.numeric(cm_b$byClass["Sensitivity"])
    sp_b  <- as.numeric(cm_b$byClass["Specificity"])
    pr_b  <- as.numeric(cm_b$byClass["Pos Pred Value"])
    f1_b  <- ifelse(is.na(pr_b) | pr_b + s_b == 0, NA_real_,
                    2 * pr_b * s_b / (pr_b + s_b))
    auc_b <- tryCatch(
      as.numeric(auc(roc(yb, pb_pr, levels = c("Non.IR", "IR"), quiet = TRUE))),
      error = function(e) NA_real_
    )
    prauc_b <- pr.curve(scores.class0  = pb_pr,
                         weights.class0 = ifelse(yb == "IR", 1, 0),
                         curve          = FALSE)$auc.integral
    mat[i, ] <- c(as.numeric(cm_b$overall["Accuracy"]),
                  s_b, sp_b, (s_b + sp_b) / 2, pr_b, f1_b, auc_b, prauc_b)
  }

  ci <- apply(mat, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)

  # Use bootstrap AUC CI only if DeLong failed
  if (is.na(auc_lo)) {
    auc_lo <- ci["2.5%",  "auc"]
    auc_hi <- ci["97.5%", "auc"]
  }

  data.frame(
    Dataset           = dataset,
    Accuracy          = round(acc,    3),
    Accuracy_lo       = round(ci["2.5%",  "acc"],    3),
    Accuracy_hi       = round(ci["97.5%", "acc"],    3),
    Sensitivity       = round(sens,   3),
    Sensitivity_lo    = round(ci["2.5%",  "sens"],   3),
    Sensitivity_hi    = round(ci["97.5%", "sens"],   3),
    Specificity       = round(spec,   3),
    Specificity_lo    = round(ci["2.5%",  "spec"],   3),
    Specificity_hi    = round(ci["97.5%", "spec"],   3),
    Balanced_Accuracy = round(balacc, 3),
    BalAcc_lo         = round(ci["2.5%",  "balacc"], 3),
    BalAcc_hi         = round(ci["97.5%", "balacc"], 3),
    Precision         = round(prec,   3),
    Precision_lo      = round(ci["2.5%",  "prec"],   3),
    Precision_hi      = round(ci["97.5%", "prec"],   3),
    F1                = round(f1,     3),
    F1_lo             = round(ci["2.5%",  "f1"],     3),
    F1_hi             = round(ci["97.5%", "f1"],     3),
    AUC_ROC           = round(auc_val, 3),
    AUC_lo            = round(auc_lo,  3),
    AUC_hi            = round(auc_hi,  3),
    AUC_CI_method     = auc_method,
    AUC_PR            = round(prauc,  3),
    PRAUC_lo          = round(ci["2.5%",  "prauc"],  3),
    PRAUC_hi          = round(ci["97.5%", "prauc"],  3),
    stringsAsFactors  = FALSE
  )
}


# =====================================================================
# STEP 4 — TRAIN MODEL
# =====================================================================
# Fixed hyperparameters: mtry=1, ntree=300, medianImpute.
# Imputation parameters are estimated from the training fold only
# and applied automatically by caret inside predict() — do NOT call
# predict(model$preProcess, X) manually before predict(model, X),
# as this would apply imputation twice to the external data.

train_model <- function(features, df, model_name, n_boot = 2000) {

  cat(paste0("\n  Training: ", model_name, " (", length(features), " features)\n"))

  missing_cols <- setdiff(features, names(df))
  if (length(missing_cols) > 0)
    stop(paste("Missing columns in training data:", paste(missing_cols, collapse = ", ")))

  X <- df %>%
    dplyr::select(all_of(features)) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.double))

  y <- factor(trimws(df$IR_label))
  levels(y) <- make.names(levels(y))

  # 70/30 stratified split
  set.seed(123)
  idx     <- createDataPartition(y, p = 0.7, list = FALSE)
  X_train <- X[idx, ];  X_test <- X[-idx, ]
  y_train <- y[idx];    y_test  <- y[-idx]

  ctrl <- trainControl(
    method          = "repeatedcv",
    number          = 5,
    repeats         = 20,
    sampling        = "up",
    classProbs      = TRUE,
    summaryFunction = twoClassSummary
  )

  model <- train(
    x          = X_train,
    y          = y_train,
    method     = "rf",
    trControl  = ctrl,
    preProcess = "medianImpute",
    tuneGrid   = expand.grid(mtry = 1),
    ntree      = 300,
    metric     = "ROC"
  )

  # Internal holdout evaluation
  prob_test <- predict(model, X_test, type = "prob")[, "IR"]
  pred_test <- predict(model, X_test)

  metrics <- compute_metrics(y_test, pred_test, prob_test,
                              dataset = "Internal Holdout",
                              n_boot  = n_boot)

  cat(paste0("    Internal holdout AUC-ROC: ", metrics$AUC_ROC,
             " [", metrics$AUC_lo, ", ", metrics$AUC_hi, "]\n"))

  list(model    = model,
       features = features,
       name     = model_name,
       internal_holdout_metrics = metrics)
}


# =====================================================================
# STEP 5 — EVALUATE ON EXTERNAL DATA
# =====================================================================

evaluate_model <- function(model_obj, df, n_boot = 2000) {

  features <- model_obj$features
  model    <- model_obj$model

  missing_cols <- setdiff(features, names(df))
  if (length(missing_cols) > 0)
    stop(paste("Missing columns in external data:", paste(missing_cols, collapse = ", ")))

  X <- df %>%
    dplyr::select(all_of(features)) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.double))

  y <- factor(trimws(df$IR_label))
  levels(y) <- levels(model$trainingData$.outcome)

  prob <- predict(model, X, type = "prob")[, "IR"]
  pred <- predict(model, X)

  metrics <- compute_metrics(y, pred, prob,
                              dataset = "External Validation",
                              n_boot  = n_boot)

  cat(paste0("    External validation AUC-ROC: ", metrics$AUC_ROC,
             " [", metrics$AUC_lo, ", ", metrics$AUC_hi, "]\n"))

  metrics
}


# =====================================================================
# STEP 6 — TRAIN ALL THREE MODELS
# =====================================================================

cat("\nStep 6: Training models...\n")

model_CGM        <- train_model(feature_sets$CGM_model,
                                df_test, "CGM Model")
model_Smartwatch <- train_model(feature_sets$Smartwatch_model,
                                df_test, "Smartwatch Model")
model_Baseline   <- train_model(feature_sets$Baseline_model,
                                df_test, "Baseline Model")


# =====================================================================
# STEP 7 — EVALUATE ON EXTERNAL VALIDATION SET
# =====================================================================

cat("\nStep 7: Evaluating on external validation set (Study 2)...\n")

ext_CGM        <- evaluate_model(model_CGM,        df_external)
ext_Smartwatch <- evaluate_model(model_Smartwatch, df_external)
ext_Baseline   <- evaluate_model(model_Baseline,   df_external)


# =====================================================================
# STEP 8 — COLLECT AND SAVE RESULTS
# =====================================================================

cat("\nStep 8: Saving results...\n")

add_model_col <- function(metrics, model_name) {
  df <- as.data.frame(metrics)
  df$Model <- model_name
  df
}

results_all <- dplyr::bind_rows(
  add_model_col(model_CGM$internal_holdout_metrics,        "CGM Model"),
  add_model_col(model_Smartwatch$internal_holdout_metrics, "Smartwatch Model"),
  add_model_col(model_Baseline$internal_holdout_metrics,   "Baseline Model"),
  add_model_col(ext_CGM,                                   "CGM Model"),
  add_model_col(ext_Smartwatch,                            "Smartwatch Model"),
  add_model_col(ext_Baseline,                              "Baseline Model")
)

col_order <- c(
  "Model", "Dataset",
  "Accuracy",          "Accuracy_lo",    "Accuracy_hi",
  "Sensitivity",       "Sensitivity_lo", "Sensitivity_hi",
  "Specificity",       "Specificity_lo", "Specificity_hi",
  "Balanced_Accuracy", "BalAcc_lo",      "BalAcc_hi",
  "Precision",         "Precision_lo",   "Precision_hi",
  "F1",                "F1_lo",          "F1_hi",
  "AUC_ROC",           "AUC_lo",         "AUC_hi",  "AUC_CI_method",
  "AUC_PR",            "PRAUC_lo",       "PRAUC_hi"
)
results_all <- results_all[, col_order]

out_path <- file.path(RESULTS, "Table2_main_results.csv")
write.csv(results_all, out_path, row.names = FALSE)

cat(paste0("  Saved: ", out_path, "\n\n"))
print(results_all[, c("Model","Dataset","AUC_ROC","AUC_lo","AUC_hi",
                       "Sensitivity","Specificity","Balanced_Accuracy")])
