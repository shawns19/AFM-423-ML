# DATA_DICTIONARY

**Universe:** Monthly frequency; 4 factors (`MKT-RF`, `SMB`, `HML`, `MOM`) plus `RF`; macro series `VIXCLS`, `T10Y2Y`, `FEDFUNDS`, `UNRATE`.

## Data Sources

- Kenneth French Data Library
  - `F-F_Research_Data_Factors_CSV.zip`
  - `F-F_Momentum_Factor_CSV.zip`
- FRED (St. Louis Fed)
  - `VIXCLS`, `T10Y2Y`, `FEDFUNDS`, `UNRATE`

## Variable Definitions

| Variable | Source | Raw Code | Native Frequency | Units | Transform in Pipeline | Lag Policy for Modeling | Missing Handling |
|---|---|---|---|---|---|---|---|
| `date` | Derived | month-end date | Monthly | Date | Converted to month-end `Date` | N/A | N/A |
| `MKT_RF` | Kenneth French | `Mkt-RF` | Monthly | Percent return | Converted to decimal (`/100`) | `MKT_RF_L1` used as predictor; contemporaneous used as realized outcome | Rows without required factors removed in final clean set |
| `SMB` | Kenneth French | `SMB` | Monthly | Percent return | Converted to decimal (`/100`) | `SMB_L1` used as predictor | Same as above |
| `HML` | Kenneth French | `HML` | Monthly | Percent return | Converted to decimal (`/100`) | `HML_L1` used as predictor | Same as above |
| `MOM` | Kenneth French | `Mom` | Monthly | Percent return | Converted to decimal (`/100`) | `MOM_L1` used as predictor | Same as above |
| `RF` | Kenneth French | `RF` | Monthly | Percent return | Converted to decimal (`/100`) | `RF_L1` used as predictor | Same as above |
| `MKT` | Derived | `MKT_RF + RF` | Monthly | Decimal return | Arithmetic sum | Not primary predictor (diagnostic/useful derived return) | Inherits parent missingness |
| `VIXCLS` | FRED | `VIXCLS` | Daily | Index level | Month-end value selected as last available daily observation in each month | `VIXCLS_L1` used as predictor | Monthly value then LOCF on monthly calendar |
| `T10Y2Y` | FRED | `T10Y2Y` | Daily | Percentage points | Month-end value = last available daily observation in month | `T10Y2Y_L1` | LOCF on monthly calendar |
| `FEDFUNDS` | FRED | `FEDFUNDS` | Monthly | Percent | Carried through monthly harmonization; last available value in month retained | `FEDFUNDS_L1` | LOCF on monthly calendar |
| `UNRATE` | FRED | `UNRATE` | Monthly | Percent | Carried through monthly harmonization; last available value in month retained | `UNRATE_L1` | LOCF on monthly calendar |
| `DLOG_VIX` | Derived | `diff(log(VIXCLS))` | Monthly | Log difference | First difference of log level | `DLOG_VIX_L1` | First diff introduces NA at start; dropped later if required |
| `DT10Y2Y` | Derived | `diff(T10Y2Y)` | Monthly | Change in pp | First difference | `DT10Y2Y_L1` | First diff NA handling as above |
| `DFEDFUNDS` | Derived | `diff(FEDFUNDS)` | Monthly | Change in pp | First difference | `DFEDFUNDS_L1` | First diff NA handling as above |
| `DUNRATE` | Derived | `diff(UNRATE)` | Monthly | Change in pp | First difference | `DUNRATE_L1` | First diff NA handling as above |
| `regime_label` | Derived (Phase 3) | calm/turbulent | Monthly | Categorical | 2-state Gaussian HMM fitted via depmixS4 on monthly `MKT_RF` with expanding-window re-estimation (fit on rows `1:t` for month `t`); month-`t` state assigned from filtered/current posterior (no future smoothing). calm = low-variance state, turbulent = high-variance state for each fit. | contemporaneous state (no-future assignment) | Undefined for very early rows before minimum history; removed before final modeling matrix |
| `regime_next` | Derived (Phase 3) | lead(`regime_label`) | Monthly | Categorical | One-step-ahead target label derived from expanding-window, no-future `regime_label` sequence | target at `t+1` predicted using features at `t` | last row NA (and early-history label gaps) removed in final complete-case matrix |

## Feature Engineering Variables (Phase 3)

Additional lagged factor features written to `feature_matrix_phase3.csv`:
- `*_VOL3_L1`: 3-month rolling SD (lagged 1 month).
- `*_MOM3_L1`: 3-month rolling cumulative return proxy (lagged 1 month).

These are constructed for `MKT_RF`, `SMB`, `HML`, and `MOM`.

## File Outputs by Phase

- Phase 1:
  - `data_raw/<run_id>/factors_monthly_raw.csv`
  - `data_raw/<run_id>/fred_daily_raw.csv`
  - `data_processed/initial_monthly_panel_draft.csv`
- Phase 2:
  - `data_processed/monthly_panel_pre_dropna.csv`
  - `data_processed/clean_monthly_model_base.csv`
- Phase 3:
  - `data_processed/regime_labeled_dataset.csv`
  - `data_processed/feature_matrix_phase3.csv`
  - `output/tables/phase3_state_balance.csv`
  - `output/tables/phase3_transition_matrix.csv`
  - `output/tables/phase3_state_persistence.csv`
