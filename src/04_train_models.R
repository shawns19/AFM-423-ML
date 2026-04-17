#!/usr/bin/env Rscript
# Phase 4: Walk-forward model training

options(stringsAsFactors = FALSE, warn = 1)

suppressPackageStartupMessages({
  if (!require(xgboost)) install.packages("xgboost", repos = "https://cloud.r-project.org")
  if (!require(pROC)) install.packages("pROC", repos = "https://cloud.r-project.org")
  library(xgboost)
  library(pROC)
})

resolve_project_root <- function() {
  cwd <- normalizePath(getwd(), mustWork = TRUE)
  candidate <- normalizePath(file.path(cwd, "afm 423/gpr/gpr2"), mustWork = FALSE)
  if (dir.exists(candidate)) return(candidate)
  if (basename(cwd) == "gpr2") return(cwd)
  stop("Could not resolve project root. Run from repository root or from gpr2/.")
}

safe_auc <- function(y_true, prob_turbulent) {
  y_bin <- ifelse(y_true == "turbulent", 1, 0)
  if (length(unique(y_bin)) < 2) return(NA_real_)
  as.numeric(pROC::auc(y_bin, prob_turbulent, quiet = TRUE))
}

compute_metrics <- function(df, prob_col, pred_col) {
  if (nrow(df) == 0) {
    return(data.frame(
      n_obs = 0,
      accuracy = NA_real_,
      f1_turbulent = NA_real_,
      auc_roc = NA_real_,
      tp = NA_integer_,
      fp = NA_integer_,
      tn = NA_integer_,
      fn = NA_integer_,
      stringsAsFactors = FALSE
    ))
  }

  y_true <- as.character(df$regime_next)
  y_pred <- as.character(df[[pred_col]])
  y_prob <- as.numeric(df[[prob_col]])

  tp <- sum(y_true == "turbulent" & y_pred == "turbulent")
  fp <- sum(y_true == "calm" & y_pred == "turbulent")
  tn <- sum(y_true == "calm" & y_pred == "calm")
  fn <- sum(y_true == "turbulent" & y_pred == "calm")

  accuracy <- (tp + tn) / (tp + fp + tn + fn)
  precision <- ifelse((tp + fp) > 0, tp / (tp + fp), NA_real_)
  recall <- ifelse((tp + fn) > 0, tp / (tp + fn), NA_real_)
  f1 <- ifelse(
    !is.na(precision) && !is.na(recall) && (precision + recall) > 0,
    2 * precision * recall / (precision + recall),
    NA_real_
  )
  auc <- safe_auc(y_true, y_prob)

  data.frame(
    n_obs = nrow(df),
    accuracy = accuracy,
    f1_turbulent = f1,
    auc_roc = auc,
    tp = as.integer(tp),
    fp = as.integer(fp),
    tn = as.integer(tn),
    fn = as.integer(fn),
    stringsAsFactors = FALSE
  )
}

project_root <- resolve_project_root()
dir_processed <- file.path(project_root, "data_processed")
dir_tables <- file.path(project_root, "output", "tables")
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)

input_path <- file.path(dir_processed, "feature_matrix_phase3.csv")
if (!file.exists(input_path)) {
  stop("Missing feature_matrix_phase3.csv. Run src/03_build_labels.R first.")
}

df <- read.csv(input_path)
df$date <- as.Date(df$date)
df <- df[order(df$date), ]

feature_cols <- c(
  "MKT_RF_L1", "SMB_L1", "HML_L1", "MOM_L1", "RF_L1",
  "MKT_RF_VOL3_L1", "SMB_VOL3_L1", "HML_VOL3_L1", "MOM_VOL3_L1",
  "MKT_RF_MOM3_L1", "SMB_MOM3_L1", "HML_MOM3_L1", "MOM_MOM3_L1",
  "VIXCLS_L1", "T10Y2Y_L1", "FEDFUNDS_L1", "UNRATE_L1",
  "DLOG_VIX_L1", "DT10Y2Y_L1", "DFEDFUNDS_L1", "DUNRATE_L1"
)

required_cols <- c("date", feature_cols, "regime_next")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Missing required columns in feature matrix: ", paste(missing_cols, collapse = ", "))
}

model_df <- df[, required_cols]
model_df <- model_df[complete.cases(model_df), ]
model_df$regime_next <- as.character(model_df$regime_next)
model_df <- model_df[model_df$regime_next %in% c("calm", "turbulent"), ]
model_df <- model_df[order(model_df$date), ]

oos_start <- as.Date("2007-01-01")
oos_dates <- sort(unique(model_df$date[model_df$date >= oos_start]))
if (length(oos_dates) == 0) stop("No OOS rows found on/after 2007-01-01.")

pred_rows <- vector("list", length(oos_dates))
retrain_count <- 0L
logit_model <- NULL
xgb_model <- NULL

for (i in seq_along(oos_dates)) {
  current_date <- oos_dates[i]
  train_idx <- which(model_df$date < current_date)
  test_idx <- which(model_df$date == current_date)
  if (length(test_idx) == 0) next

  if (length(train_idx) == 0) stop("No training rows before OOS date: ", as.character(current_date))

  should_retrain <- is.null(logit_model) || ((i - 1) %% 6 == 0)
  if (should_retrain) {
    train_df <- model_df[train_idx, c(feature_cols, "regime_next")]
    if (length(unique(train_df$regime_next)) < 2) {
      stop("Training labels do not contain both classes by date: ", as.character(current_date))
    }

    train_df$regime_next <- factor(train_df$regime_next, levels = c("calm", "turbulent"))

    set.seed(42)
    logit_model <- glm(regime_next ~ ., data = train_df, family = binomial)

    x_train <- as.matrix(train_df[, feature_cols])
    y_train <- ifelse(train_df$regime_next == "turbulent", 1, 0)
    xgb_train <- xgboost::xgb.DMatrix(data = x_train, label = y_train)

    set.seed(42)
    xgb_model <- xgboost::xgb.train(
      params = list(
        objective = "binary:logistic",
        max_depth = 3,
        eta = 0.1,
        nthread = 1
      ),
      data = xgb_train,
      nrounds = 100,
      verbose = 0
    )
    retrain_count <- retrain_count + 1L
  }

  test_df <- model_df[test_idx, c("date", feature_cols, "regime_next")]
  logit_prob <- as.numeric(predict(logit_model, newdata = test_df[, feature_cols, drop = FALSE], type = "response"))
  logit_pred <- ifelse(logit_prob >= 0.5, "turbulent", "calm")

  x_test <- as.matrix(test_df[, feature_cols])
  xgb_prob <- as.numeric(predict(xgb_model, newdata = x_test))
  xgb_pred <- ifelse(xgb_prob >= 0.5, "turbulent", "calm")

  pred_rows[[i]] <- data.frame(
    date = test_df$date,
    regime_next = as.character(test_df$regime_next),
    logit_prob = logit_prob,
    logit_pred = logit_pred,
    xgb_prob = xgb_prob,
    xgb_pred = xgb_pred,
    stringsAsFactors = FALSE
  )
}

oos_predictions <- do.call(rbind, pred_rows)
if (is.null(oos_predictions) || nrow(oos_predictions) == 0) {
  stop("No OOS predictions were generated.")
}
oos_predictions <- oos_predictions[order(oos_predictions$date), ]

write.csv(oos_predictions, file.path(dir_processed, "oos_predictions.csv"), row.names = FALSE)

logit_full <- compute_metrics(oos_predictions, "logit_prob", "logit_pred")
logit_full$model <- "logit"
xgb_full <- compute_metrics(oos_predictions, "xgb_prob", "xgb_pred")
xgb_full$model <- "xgboost"

metrics_full <- rbind(logit_full, xgb_full)
metrics_full <- metrics_full[, c("model", "n_obs", "accuracy", "f1_turbulent", "auc_roc", "tp", "fp", "tn", "fn")]
write.csv(metrics_full, file.path(dir_tables, "model_metrics_full.csv"), row.names = FALSE)

subperiods <- data.frame(
  subperiod = c("2007-2009", "2010-2019", "2020-2024"),
  start = as.Date(c("2007-01-01", "2010-01-01", "2020-01-01")),
  end = as.Date(c("2009-12-31", "2019-12-31", "2024-12-31")),
  stringsAsFactors = FALSE
)

subperiod_metrics_rows <- vector("list", nrow(subperiods) * 2)
row_ctr <- 1L
for (j in seq_len(nrow(subperiods))) {
  sp <- subperiods[j, ]
  sp_df <- oos_predictions[oos_predictions$date >= sp$start & oos_predictions$date <= sp$end, ]

  m_logit <- compute_metrics(sp_df, "logit_prob", "logit_pred")
  m_logit$model <- "logit"
  m_logit$subperiod <- sp$subperiod
  subperiod_metrics_rows[[row_ctr]] <- m_logit
  row_ctr <- row_ctr + 1L

  m_xgb <- compute_metrics(sp_df, "xgb_prob", "xgb_pred")
  m_xgb$model <- "xgboost"
  m_xgb$subperiod <- sp$subperiod
  subperiod_metrics_rows[[row_ctr]] <- m_xgb
  row_ctr <- row_ctr + 1L
}

metrics_subperiod <- do.call(rbind, subperiod_metrics_rows)
metrics_subperiod <- metrics_subperiod[, c("subperiod", "model", "n_obs", "accuracy", "f1_turbulent", "auc_roc", "tp", "fp", "tn", "fn")]
write.csv(metrics_subperiod, file.path(dir_tables, "model_metrics_subperiod.csv"), row.names = FALSE)

train_oos_summary <- data.frame(
  metric = c(
    "total_rows_feature_matrix",
    "training_only_rows_before_2007",
    "oos_rows_2007_onward",
    "retraining_events",
    "first_oos_prediction_date",
    "last_oos_prediction_date"
  ),
  value = c(
    nrow(model_df),
    sum(model_df$date < oos_start),
    sum(model_df$date >= oos_start),
    retrain_count,
    as.character(min(oos_predictions$date)),
    as.character(max(oos_predictions$date))
  ),
  stringsAsFactors = FALSE
)
write.csv(train_oos_summary, file.path(dir_tables, "train_oos_split_summary.csv"), row.names = FALSE)

message("Phase 4 model training and walk-forward validation complete.")
