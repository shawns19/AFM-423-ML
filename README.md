# Machine Learning for Market Regime Detection in Dynamic Factor Allocation

R pipeline for the project: pull factor and macro data, build regime labels, train classifiers, backtest strategies, and produce figures and summary tables.

## Setup

R 4.x and packages: `quantmod`, `zoo`, `depmixS4`, `xgboost`, `pROC`, `ggplot2`, `scales`, `tidyr`, `dplyr` (scripts install missing packages when possible).

## Run

From the folder that contains `src/`:

```bash
Rscript src/01_pull_data.R
Rscript src/02_preprocess_data.R
Rscript src/03_build_labels.R
Rscript src/04_train_models.R
Rscript src/05_backtest.R
Rscript src/06_make_figures.R
```

Scripts write under `data_raw/`, `data_processed/`, and `output/`. Variable definitions are in `docs/DATA_DICTIONARY.md`.

## Scripts

| File | Purpose |
|------|---------|
| `01_pull_data.R` | Download French factor and FRED macro series; write dated raw snapshots |
| `02_preprocess_data.R` | Construct the clean monthly modeling dataset |
| `03_build_labels.R` | Regime labels (HMM) and feature matrix |
| `04_train_models.R` | Expanding-window training; logistic regression and XGBoost; OOS predictions |
| `05_backtest.R` | Static benchmark and dynamic strategies; transaction costs |
| `06_make_figures.R` | Export figures to `output/figures/` |
