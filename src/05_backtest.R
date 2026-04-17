#!/usr/bin/env Rscript
# Phase 5: Backtest and performance attribution

options(stringsAsFactors = FALSE, warn = 1)

resolve_project_root <- function() {
  cwd <- normalizePath(getwd(), mustWork = TRUE)
  candidate <- normalizePath(file.path(cwd, "afm 423/gpr/gpr2"), mustWork = FALSE)
  if (dir.exists(candidate)) return(candidate)
  if (basename(cwd) == "gpr2") return(cwd)
  stop("Could not resolve project root. Run from repository root or from gpr2/.")
}

assert_required_columns <- function(df, cols, name) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in ", name, ": ", paste(missing_cols, collapse = ", "))
  }
}

calc_drawdown_stats <- function(ret_vec) {
  equity <- cumprod(1 + ret_vec)
  running_max <- cummax(equity)
  drawdown <- equity / running_max - 1
  max_dd <- min(drawdown, na.rm = TRUE)
  list(drawdown = drawdown, max_drawdown = max_dd)
}

calc_performance_metrics <- function(ret_vec, rf_vec, turnover_vec, tc_vec) {
  n <- length(ret_vec)
  ann_return <- (prod(1 + ret_vec)^(12 / n)) - 1
  ann_vol <- sd(ret_vec) * sqrt(12)
  excess <- ret_vec - rf_vec
  sharpe <- ifelse(sd(excess) > 0, mean(excess) / sd(excess) * sqrt(12), NA_real_)
  dd_stats <- calc_drawdown_stats(ret_vec)
  max_dd <- dd_stats$max_drawdown
  calmar <- ifelse(is.finite(max_dd) && max_dd < 0, ann_return / abs(max_dd), NA_real_)

  data.frame(
    annualized_return = ann_return,
    annualized_volatility = ann_vol,
    sharpe_ratio = sharpe,
    max_drawdown = max_dd,
    calmar_ratio = calmar,
    avg_monthly_turnover = mean(turnover_vec),
    annual_turnover = mean(turnover_vec) * 12,
    avg_monthly_transaction_cost_drag = mean(tc_vec),
    n_months = n,
    stringsAsFactors = FALSE
  )
}

project_root <- resolve_project_root()
dir_processed <- file.path(project_root, "data_processed")
dir_tables <- file.path(project_root, "output", "tables")
dir_src <- file.path(project_root, "src")

if (!dir.exists(dir_processed)) stop("Missing data_processed directory.")
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_src, recursive = TRUE, showWarnings = FALSE)

path_oos <- file.path(dir_processed, "oos_predictions.csv")
path_regime <- file.path(dir_processed, "regime_labeled_dataset.csv")
path_feature <- file.path(dir_processed, "feature_matrix_phase3.csv")
path_metrics <- file.path(dir_tables, "model_metrics_full.csv")

required_inputs <- c(path_oos, path_regime, path_feature, path_metrics)
for (p in required_inputs) {
  if (!file.exists(p)) stop("Missing required input file: ", p)
}

oos <- read.csv(path_oos)
regime_df <- read.csv(path_regime)

assert_required_columns(
  oos,
  c("date", "logit_pred", "xgb_pred"),
  "oos_predictions.csv"
)
assert_required_columns(
  regime_df,
  c("date", "MKT_RF", "SMB", "HML", "MOM", "RF"),
  "regime_labeled_dataset.csv"
)

oos$date <- as.Date(oos$date)
regime_df$date <- as.Date(regime_df$date)

if (any(is.na(oos$date))) stop("Found invalid date values in oos_predictions.csv.")
if (any(is.na(regime_df$date))) stop("Found invalid date values in regime_labeled_dataset.csv.")
if (any(duplicated(oos$date))) stop("Duplicate OOS dates found in oos_predictions.csv.")
if (any(duplicated(regime_df$date))) stop("Duplicate dates found in regime_labeled_dataset.csv.")

oos <- oos[order(oos$date), ]
regime_df <- regime_df[order(regime_df$date), ]

returns_df <- regime_df[, c("date", "MKT_RF", "SMB", "HML", "MOM", "RF")]
joined <- merge(oos, returns_df, by = "date", all.x = TRUE, all.y = FALSE, sort = TRUE)
if (nrow(joined) != nrow(oos)) {
  stop("Date join mismatch: expected ", nrow(oos), " OOS rows, got ", nrow(joined), ".")
}

if (any(!complete.cases(joined[, c("MKT_RF", "SMB", "HML", "MOM", "RF")]))) {
  bad_dates <- joined$date[!complete.cases(joined[, c("MKT_RF", "SMB", "HML", "MOM", "RF")])]
  stop("Missing factor returns after join for dates: ", paste(as.character(bad_dates), collapse = ", "))
}

valid_states <- c("calm", "turbulent")
if (any(!joined$logit_pred %in% valid_states)) stop("Unexpected states in logit_pred.")
if (any(!joined$xgb_pred %in% valid_states)) stop("Unexpected states in xgb_pred.")

weights_map <- list(
  calm = c(MKT_RF = 0.30, SMB = 0.25, HML = 0.15, MOM = 0.30),
  turbulent = c(MKT_RF = 0.35, SMB = 0.15, HML = 0.35, MOM = 0.15)
)

factor_total <- cbind(
  MKT_RF = joined$MKT_RF + joined$RF,
  SMB = joined$SMB + joined$RF,
  HML = joined$HML + joined$RF,
  MOM = joined$MOM + joined$RF
)

get_weights_from_pred <- function(pred_vec, map) {
  out <- t(vapply(pred_vec, function(s) map[[s]], numeric(4)))
  colnames(out) <- c("MKT_RF", "SMB", "HML", "MOM")
  out
}

weights_benchmark <- matrix(rep(c(0.25, 0.25, 0.25, 0.25), nrow(joined)), ncol = 4, byrow = TRUE)
colnames(weights_benchmark) <- c("MKT_RF", "SMB", "HML", "MOM")
weights_logit <- get_weights_from_pred(joined$logit_pred, weights_map)
weights_xgb <- get_weights_from_pred(joined$xgb_pred, weights_map)

compute_turnover <- function(w_mat) {
  prev <- rbind(rep(0, ncol(w_mat)), w_mat[-nrow(w_mat), , drop = FALSE])
  rowSums(abs(w_mat - prev))
}

turn_benchmark <- compute_turnover(weights_benchmark)
turn_logit <- compute_turnover(weights_logit)
turn_xgb <- compute_turnover(weights_xgb)

gross_benchmark <- rowSums(weights_benchmark * factor_total)
gross_logit <- rowSums(weights_logit * factor_total)
gross_xgb <- rowSums(weights_xgb * factor_total)

bps_levels <- c(5, 10, 20)
cost_rate <- function(bps) bps / 10000

net_return <- function(gross, turnover, bps) {
  gross - cost_rate(bps) * turnover
}

ts_df <- data.frame(
  date = joined$date,
  RF = joined$RF,
  factor_MKT_RF_total = factor_total[, "MKT_RF"],
  factor_SMB_total = factor_total[, "SMB"],
  factor_HML_total = factor_total[, "HML"],
  factor_MOM_total = factor_total[, "MOM"],
  benchmark_w_MKT_RF = weights_benchmark[, "MKT_RF"],
  benchmark_w_SMB = weights_benchmark[, "SMB"],
  benchmark_w_HML = weights_benchmark[, "HML"],
  benchmark_w_MOM = weights_benchmark[, "MOM"],
  dynamic_logit_w_MKT_RF = weights_logit[, "MKT_RF"],
  dynamic_logit_w_SMB = weights_logit[, "SMB"],
  dynamic_logit_w_HML = weights_logit[, "HML"],
  dynamic_logit_w_MOM = weights_logit[, "MOM"],
  dynamic_xgboost_w_MKT_RF = weights_xgb[, "MKT_RF"],
  dynamic_xgboost_w_SMB = weights_xgb[, "SMB"],
  dynamic_xgboost_w_HML = weights_xgb[, "HML"],
  dynamic_xgboost_w_MOM = weights_xgb[, "MOM"],
  benchmark_gross_return = gross_benchmark,
  dynamic_logit_gross_return = gross_logit,
  dynamic_xgboost_gross_return = gross_xgb,
  benchmark_turnover = turn_benchmark,
  dynamic_logit_turnover = turn_logit,
  dynamic_xgboost_turnover = turn_xgb,
  benchmark_tc_10bps = cost_rate(10) * turn_benchmark,
  dynamic_logit_tc_10bps = cost_rate(10) * turn_logit,
  dynamic_xgboost_tc_10bps = cost_rate(10) * turn_xgb,
  benchmark_net_return_10bps = net_return(gross_benchmark, turn_benchmark, 10),
  dynamic_logit_net_return_10bps = net_return(gross_logit, turn_logit, 10),
  dynamic_xgboost_net_return_10bps = net_return(gross_xgb, turn_xgb, 10),
  stringsAsFactors = FALSE
)

# Validation checks
if (any(duplicated(ts_df$date))) stop("backtest_timeseries has duplicate dates.")
key_ret_cols <- c("benchmark_net_return_10bps", "dynamic_logit_net_return_10bps", "dynamic_xgboost_net_return_10bps")
if (any(!complete.cases(ts_df[, key_ret_cols]))) stop("NA detected in key net return columns.")

weight_sum_chk <- function(df, cols) rowSums(df[, cols])
if (any(abs(weight_sum_chk(ts_df, c("benchmark_w_MKT_RF", "benchmark_w_SMB", "benchmark_w_HML", "benchmark_w_MOM")) - 1) > 1e-10)) {
  stop("Benchmark weights do not sum to 1 for all months.")
}
if (any(abs(weight_sum_chk(ts_df, c("dynamic_logit_w_MKT_RF", "dynamic_logit_w_SMB", "dynamic_logit_w_HML", "dynamic_logit_w_MOM")) - 1) > 1e-10)) {
  stop("Logit dynamic weights do not sum to 1 for all months.")
}
if (any(abs(weight_sum_chk(ts_df, c("dynamic_xgboost_w_MKT_RF", "dynamic_xgboost_w_SMB", "dynamic_xgboost_w_HML", "dynamic_xgboost_w_MOM")) - 1) > 1e-10)) {
  stop("XGBoost dynamic weights do not sum to 1 for all months.")
}

if (any(ts_df$benchmark_turnover < -1e-12) || any(ts_df$dynamic_logit_turnover < -1e-12) || any(ts_df$dynamic_xgboost_turnover < -1e-12)) {
  stop("Turnover contains negative values.")
}
if (any(ts_df$benchmark_tc_10bps < -1e-12) || any(ts_df$dynamic_logit_tc_10bps < -1e-12) || any(ts_df$dynamic_xgboost_tc_10bps < -1e-12)) {
  stop("Transaction cost contains negative values.")
}

perf_benchmark <- calc_performance_metrics(
  ts_df$benchmark_net_return_10bps, ts_df$RF, ts_df$benchmark_turnover, ts_df$benchmark_tc_10bps
)
perf_logit <- calc_performance_metrics(
  ts_df$dynamic_logit_net_return_10bps, ts_df$RF, ts_df$dynamic_logit_turnover, ts_df$dynamic_logit_tc_10bps
)
perf_xgb <- calc_performance_metrics(
  ts_df$dynamic_xgboost_net_return_10bps, ts_df$RF, ts_df$dynamic_xgboost_turnover, ts_df$dynamic_xgboost_tc_10bps
)

performance_summary <- rbind(
  cbind(strategy = "benchmark", perf_benchmark),
  cbind(strategy = "dynamic_logit", perf_logit),
  cbind(strategy = "dynamic_xgboost", perf_xgb)
)

for (i in seq_len(nrow(performance_summary))) {
  ann_ret_recalc <- (prod(1 + switch(
    performance_summary$strategy[i],
    benchmark = ts_df$benchmark_net_return_10bps,
    dynamic_logit = ts_df$dynamic_logit_net_return_10bps,
    dynamic_xgboost = ts_df$dynamic_xgboost_net_return_10bps
  ))^(12 / performance_summary$n_months[i])) - 1
  if (abs(ann_ret_recalc - performance_summary$annualized_return[i]) > 1e-10) {
    stop("Annualized return consistency check failed for ", performance_summary$strategy[i], ".")
  }
}

turnover_cost_summary <- data.frame(
  strategy = c("benchmark", "dynamic_logit", "dynamic_xgboost"),
  avg_monthly_turnover = c(mean(turn_benchmark), mean(turn_logit), mean(turn_xgb)),
  annual_turnover = c(mean(turn_benchmark), mean(turn_logit), mean(turn_xgb)) * 12,
  avg_monthly_tc_drag_10bps = c(mean(cost_rate(10) * turn_benchmark), mean(cost_rate(10) * turn_logit), mean(cost_rate(10) * turn_xgb)),
  annual_tc_drag_10bps = c(mean(cost_rate(10) * turn_benchmark), mean(cost_rate(10) * turn_logit), mean(cost_rate(10) * turn_xgb)) * 12,
  stringsAsFactors = FALSE
)

sensitivity_rows <- list()
for (bps in bps_levels) {
  bench_net <- net_return(gross_benchmark, turn_benchmark, bps)
  logit_net <- net_return(gross_logit, turn_logit, bps)
  xgb_net <- net_return(gross_xgb, turn_xgb, bps)

  m_bench <- calc_performance_metrics(bench_net, ts_df$RF, turn_benchmark, cost_rate(bps) * turn_benchmark)
  m_logit <- calc_performance_metrics(logit_net, ts_df$RF, turn_logit, cost_rate(bps) * turn_logit)
  m_xgb <- calc_performance_metrics(xgb_net, ts_df$RF, turn_xgb, cost_rate(bps) * turn_xgb)

  sens <- rbind(
    cbind(strategy = "benchmark", tc_bps = bps, m_bench),
    cbind(strategy = "dynamic_logit", tc_bps = bps, m_logit),
    cbind(strategy = "dynamic_xgboost", tc_bps = bps, m_xgb)
  )

  bench_ann <- sens$annualized_return[sens$strategy == "benchmark"]
  bench_sharpe <- sens$sharpe_ratio[sens$strategy == "benchmark"]
  sens$delta_ann_return_vs_benchmark <- sens$annualized_return - bench_ann
  sens$delta_sharpe_vs_benchmark <- sens$sharpe_ratio - bench_sharpe
  sens$beats_benchmark_ann_return <- sens$annualized_return > bench_ann

  sensitivity_rows[[as.character(bps)]] <- sens
}
cost_sensitivity <- do.call(rbind, sensitivity_rows)
rownames(cost_sensitivity) <- NULL

bench_row <- performance_summary[performance_summary$strategy == "benchmark", ]
logit_row <- performance_summary[performance_summary$strategy == "dynamic_logit", ]
xgb_row <- performance_summary[performance_summary$strategy == "dynamic_xgboost", ]

model_comparison <- rbind(
  data.frame(
    comparison = "dynamic_logit_vs_benchmark",
    delta_annualized_return = logit_row$annualized_return - bench_row$annualized_return,
    delta_sharpe_ratio = logit_row$sharpe_ratio - bench_row$sharpe_ratio,
    delta_max_drawdown = logit_row$max_drawdown - bench_row$max_drawdown,
    delta_calmar_ratio = logit_row$calmar_ratio - bench_row$calmar_ratio,
    delta_annual_turnover = logit_row$annual_turnover - bench_row$annual_turnover,
    stringsAsFactors = FALSE
  ),
  data.frame(
    comparison = "dynamic_xgboost_vs_benchmark",
    delta_annualized_return = xgb_row$annualized_return - bench_row$annualized_return,
    delta_sharpe_ratio = xgb_row$sharpe_ratio - bench_row$sharpe_ratio,
    delta_max_drawdown = xgb_row$max_drawdown - bench_row$max_drawdown,
    delta_calmar_ratio = xgb_row$calmar_ratio - bench_row$calmar_ratio,
    delta_annual_turnover = xgb_row$annual_turnover - bench_row$annual_turnover,
    stringsAsFactors = FALSE
  ),
  data.frame(
    comparison = "dynamic_xgboost_vs_dynamic_logit",
    delta_annualized_return = xgb_row$annualized_return - logit_row$annualized_return,
    delta_sharpe_ratio = xgb_row$sharpe_ratio - logit_row$sharpe_ratio,
    delta_max_drawdown = xgb_row$max_drawdown - logit_row$max_drawdown,
    delta_calmar_ratio = xgb_row$calmar_ratio - logit_row$calmar_ratio,
    delta_annual_turnover = xgb_row$annual_turnover - logit_row$annual_turnover,
    stringsAsFactors = FALSE
  )
)

monthly_attribution <- data.frame(
  date = ts_df$date,
  benchmark_MKT_RF_contrib = ts_df$benchmark_w_MKT_RF * ts_df$factor_MKT_RF_total,
  benchmark_SMB_contrib = ts_df$benchmark_w_SMB * ts_df$factor_SMB_total,
  benchmark_HML_contrib = ts_df$benchmark_w_HML * ts_df$factor_HML_total,
  benchmark_MOM_contrib = ts_df$benchmark_w_MOM * ts_df$factor_MOM_total,
  dynamic_logit_MKT_RF_contrib = ts_df$dynamic_logit_w_MKT_RF * ts_df$factor_MKT_RF_total,
  dynamic_logit_SMB_contrib = ts_df$dynamic_logit_w_SMB * ts_df$factor_SMB_total,
  dynamic_logit_HML_contrib = ts_df$dynamic_logit_w_HML * ts_df$factor_HML_total,
  dynamic_logit_MOM_contrib = ts_df$dynamic_logit_w_MOM * ts_df$factor_MOM_total,
  dynamic_xgboost_MKT_RF_contrib = ts_df$dynamic_xgboost_w_MKT_RF * ts_df$factor_MKT_RF_total,
  dynamic_xgboost_SMB_contrib = ts_df$dynamic_xgboost_w_SMB * ts_df$factor_SMB_total,
  dynamic_xgboost_HML_contrib = ts_df$dynamic_xgboost_w_HML * ts_df$factor_HML_total,
  dynamic_xgboost_MOM_contrib = ts_df$dynamic_xgboost_w_MOM * ts_df$factor_MOM_total,
  stringsAsFactors = FALSE
)

write.csv(ts_df, file.path(dir_processed, "backtest_timeseries.csv"), row.names = FALSE)
write.csv(performance_summary, file.path(dir_tables, "backtest_performance_summary.csv"), row.names = FALSE)
write.csv(turnover_cost_summary, file.path(dir_tables, "backtest_turnover_cost_summary.csv"), row.names = FALSE)
write.csv(cost_sensitivity, file.path(dir_tables, "backtest_cost_sensitivity.csv"), row.names = FALSE)
write.csv(model_comparison, file.path(dir_tables, "backtest_model_comparison.csv"), row.names = FALSE)
write.csv(monthly_attribution, file.path(dir_tables, "backtest_monthly_attribution.csv"), row.names = FALSE)

message("Phase 5 backtest complete.")
