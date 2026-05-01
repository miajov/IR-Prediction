# Predicting Insulin Resistance from Free-Living CGM and Smartwatch Data

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Data: AI-READI](https://img.shields.io/badge/Data-AI--READI-blue)](https://aireadi.org)

This repository contains the analysis code accompanying the paper:

> ****  
> *[Authors]*  
> [DOI link]

We train and externally validate three random forest classifiers — a CGM-based model, a Smartwatch-based model, and an anthropometric Baseline model — to predict HOMA-IR-defined insulin resistance (cut-off ≥ 2.9) from 10-day free-living wearable data collected as part of the [AI-READI dataset](https://aireadi.org) (v2.0.0).

---

## Repository Structure

```
IR-prediction/
│
├── README.md
├── LICENSE
├── .gitignore
│
├── data/
│   ├── raw/                           # AI-READI source files 
│   │   ├── biomarker_data.csv         # place here after AI-READI data access
│   │   ├── participants.tsv
│   │   ├── observation_in_.numbers
│   │
│   ├── processed/                     # derived intermediate files
│   │   ├── cgm_features.csv           # CGM features extracted from raw CGM data
|   |   ├── stress_overall_daily.csv   # Stress score/HRV feature extracted from raw CGM data
│   │   └── hr_participant_summary_daynight.csv  # HR SD score extracted from sleep/HR data
│   │   ├── participant_ids_study1.csv # verified Study 1 participant IDs (sanity check)
│   │   └── participant_ids_study2.csv # verified Study 2 participant IDs (sanity check)
│   │
│   └── analysis_ready/                # final modeling inputs
│       ├── df_study1_modelC.csv       # Study 1: n=97 (UW + UCSD sites)
│       └── df_study2_modelC.csv       # Study 2: n=61 (UAB site, external validation)
│
├── scripts/
│   ├── 01_cgm_feature_extraction.R    # extract CGM features from raw CGM time series
│   ├── 02_data_preparation.R          # full cleaning pipeline (PRISMA flow)
│   └── 03_rf_modeling.R               # random forest modeling + evaluation
│
└── results/
    ├── tables/
    │   ├── results_main.csv           # main AUC-ROC results with DeLong 95% CIs
    │   ├── imputation_sensitivity.csv # sensitivity analysis across imputation strategies
    │   ├── algorithm_comparison.csv   # comparison across ML algorithms
    │   └── glycemic_subset_results.csv
    └── supplementary/
        ├── supplementary_imputation_table.docx
        └── table_pa_sleep_comparison.docx
```

---

## Data Access

The raw data used in this study are from the **AI-READI dataset v2.0.0**, which requires registration and a data access agreement.

1. Register at [https://aireadi.org](https://aireadi.org)
2. Request access to the dataset and accept the data use agreement
3. Download the following files and place them in `data/raw/`:

| File | Description |
|------|-------------|
| `biomarker_data.csv` | Fasting blood biomarkers and anthropometrics |
| `participants.tsv` | Site, study group, device availability flags |
| `observation_in_.numbers` | Observation data including fasting hours (`paate`) |
| `stress_overall_daily.csv` | Garmin daily HRV-based stress score |
| `hr_participant_summary_daynight.csv` | Garmin HR day/night summary statistics |

The CGM raw data (`cgm_data_all.csv`) and Garmin sleep file (`sleep_all_participants.csv`) are required to run script `01_cgm_feature_extraction.R`. If you wish to skip CGM re-extraction, the pre-computed `data/processed/cgm_features.csv` is provided.

> **Note:** The analysis-ready files in `data/analysis_ready/` are derived outputs that do not contain raw biomarker values. Their inclusion in this repository is subject to AI-READI data governance terms — please verify compliance before redistributing.

---

## Reproduction Instructions

Scripts must be run in order. Set your working directory to the repository root before running each script.

### Step 1 — CGM Feature Extraction (optional)

```r
# Extracts five CGM-derived features from raw CGM time series.
# Skip this step if using the pre-computed data/processed/cgm_features.csv.
source("scripts/01_cgm_feature_extraction.R")
```

**Output:** `data/processed/cgm_features.csv`

**Required inputs:** `data/raw/cgm_data_all.csv`, `data/raw/sleep_all_participants.csv`

### Step 2 — Data Preparation

```r
# Applies the full PRISMA exclusion sequence, computes HOMA-IR,
# merges all features, and saves analysis-ready datasets.
# Requires observation_in_.numbers exported as observation_in_.csv
# (File > Export To > CSV in Apple Numbers).
source("scripts/02_data_preparation.R")
```

**Output:** `data/analysis_ready/df_study1_modelC.csv`, `data/analysis_ready/df_study2_modelC.csv`

**Required inputs:** all files in `data/raw/` plus `data/processed/cgm_features.csv`

**PRISMA flow reproduced:**

```
1,067 participants
  − 130  insulin-dependent
  − 680  fasting < 8 hours  (from observation_in_.csv, paate variable)
  −  62  HbA1c ≥ 6.5%
  = 195  eligible
     → Study 1 (UW + UCSD): n = 97  [IR = 32, Non-IR = 65]
     → Study 2 (UAB):       n = 61  [IR = 19, Non-IR = 42]
```

A sanity check at the end of the script compares derived participant IDs against `data/processed/participant_ids_study1.csv` and `data/processed/participant_ids_study2.csv`. A perfect match confirms full reproducibility.

### Step 3 — Modeling

```r
# Trains three random forest classifiers (CGM, Smartwatch, Baseline),
# evaluates on internal holdout and external validation set,
# computes DeLong CIs, and outputs all results tables.
source("scripts/03_rf_modeling.R")
```

**Output:** all files in `results/tables/`

**Required inputs:** `data/analysis_ready/df_study1_modelC.csv`, `data/analysis_ready/df_study2_modelC.csv`

---

## Models

Three random forest classifiers are trained on Study 1 (70/30 stratified split) and evaluated on the held-out 30% and externally on Study 2:

| Model | Features | n features |
|-------|----------|-----------|
| **CGM Model** | BMI, age, WHR, mean fasting glucose, SD fasting glucose, excursions/day, 50% recovery time, overall peak glucose | 8 |
| **Smartwatch Model** | BMI, age, WHR, mean daily stress (HRV), HR day–night variability | 5 |
| **Baseline Model** | BMI, age, WHR | 3 |


### TBD not final!  (we may change: mtry set to floor(√p) per model for the main one TBD)
**Fixed hyperparameters:** `ntree = 300`, `mtry = 1`, `max_depth = 5`. Class imbalance addressed by random upsampling of the minority class within each training fold. Missing values imputed by median, with imputation parameters estimated on training data only.

---

## Main Results (Placeholder; will change)

External validation AUC-ROC (95% CI, DeLong method):

| Model | External AUC-ROC | 95% CI |
|-------|-----------------|--------|
| CGM Model | **0.895** | [0.795, 0.995] |
| Smartwatch Model | 0.841 | [0.721, 0.960] |
| Baseline Model | 0.738 | [0.595, 0.882] |

Full results including sensitivity, specificity, balanced accuracy, and AUC-PR are in `results/tables/results_main.csv`.

---

## Software Requirements

```r
R version ≥ 4.2.0

# Required packages
install.packages(c(
  "dplyr",    # >= 1.1.0
  "tidyr",    # >= 1.3.0
  "caret",    # >= 6.0.94
  "pROC",     # >= 1.18.0
  "PRROC",    # >= 1.3.1
  "randomForest"  # >= 4.7.1
))
```

---

## Citation

If you use this code or data in your work, please cite:

```bibtex
@article{[citekey],
  title   = {Predicting Insulin Resistance from Free-Living Continuous Glucose
             Monitoring and Smartwatch Data: A Multisite External Validation Study},
  author  = {[Authors]},
  journal = {npj Digital Medicine},
  year    = {[Year]},
  doi     = {[DOI]}
}
```

Please also cite the AI-READI dataset:

```bibtex
@dataset{aireadi2024,
  title     = {AI-READI Dataset v2.0.0},
  author    = {{AI-READI Consortium}},
  year      = {2024},
  publisher = {AI-READI},
  url       = {https://aireadi.org}
}
```

---

## License

Code in this repository is released under the [MIT License](LICENSE). Data files derived from the AI-READI dataset are subject to the [AI-READI Data Use Agreement](https://aireadi.org).
