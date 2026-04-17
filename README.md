# Machine Learning for Market Regime Detection in Dynamic Factor Allocation

R code and outputs for the report above: data ingestion (Kenneth French and FRED), preprocessing, regime labeling, out-of-sample model training, backtesting, and figure generation.

## Repository layout

| Path | Version control |
|------|------------------|
| `src/` | Tracked — R scripts |
| `output/figures/`, `output/tables/` | Tracked — report figures and summary tables |
| `docs/` | Tracked — `DATA_DICTIONARY.md` (variable definitions) |
| `data_raw/`, `data_processed/` | **Not** tracked (large / regenerable; see `.gitignore`) |
| `report_draft/` | **Not** tracked (working drafts) |

After cloning, the repo root is the project root: `README.md`, `src/`, `output/`, `docs/`, etc. Run the pipeline locally to recreate `data_raw/` and `data_processed/` (ignored by Git), or keep a local copy.

## Requirements

- R 4.x
- R packages: `quantmod`, `zoo`, `depmixS4`, `xgboost`, `pROC`, `ggplot2`, `scales`, `tidyr`, `dplyr`  
  Missing packages are installed automatically on first run when possible.

## Execution

From the repository root (where `src/` lives):

```bash
Rscript src/01_pull_data.R
Rscript src/02_preprocess_data.R
Rscript src/03_build_labels.R
Rscript src/04_train_models.R
Rscript src/05_backtest.R
Rscript src/06_make_figures.R
```

`resolve_project_root()` in each script must resolve to this same directory. By default it accepts a working directory whose folder name is `gpr2`; if your clone has another name, rename that folder or edit `resolve_project_root()` to match your path.

## Scripts

| File | Purpose |
|------|---------|
| `01_pull_data.R` | Download French factor and FRED macro series; write dated raw snapshots |
| `02_preprocess_data.R` | Construct the clean monthly modeling dataset |
| `03_build_labels.R` | Regime labels (HMM) and feature matrix |
| `04_train_models.R` | Expanding-window training; logistic regression and XGBoost; OOS predictions |
| `05_backtest.R` | Static benchmark and dynamic strategies; transaction costs |
| `06_make_figures.R` | Export figures to `output/figures/` |

## Reproducibility

Random seeds are set where required for stochastic components. Methodology, definitions, and reported results are documented in the written report.
