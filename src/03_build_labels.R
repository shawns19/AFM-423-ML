#!/usr/bin/env Rscript
# Phase 3: Regime labels and feature engineering

options(stringsAsFactors = FALSE, warn = 1)

suppressPackageStartupMessages({
  if (!require(zoo)) install.packages("zoo", repos = "https://cloud.r-project.org")
  if (!require(depmixS4)) install.packages("depmixS4", repos = "https://cloud.r-project.org")
  library(zoo)
  library(depmixS4)
})

resolve_project_root <- function() {
  cwd <- normalizePath(getwd(), mustWork = TRUE)
  candidate <- normalizePath(file.path(cwd, "afm 423/gpr/gpr2"), mustWork = FALSE)
  if (dir.exists(candidate)) return(candidate)
  if (basename(cwd) == "gpr2") return(cwd)
  stop("Could not resolve project root. Run from repository root or from gpr2/.")
}

project_root <- resolve_project_root()
dir_processed <- file.path(project_root, "data_processed")
dir_tables <- file.path(project_root, "output", "tables")
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)

input_path <- file.path(dir_processed, "clean_monthly_model_base.csv")
if (!file.exists(input_path)) {
  stop("Missing clean_monthly_model_base.csv. Run src/02_preprocess_data.R first.")
}

df <- read.csv(input_path)
df$date <- as.Date(df$date)
df <- df[order(df$date), ]

lag1 <- function(x) c(NA, x[-length(x)])
lead1 <- function(x) c(x[-1], NA)

# expanding-window filtered HMM: fit on 1:t, assign state at t (no future look-ahead)
extract_last_state <- function(post_obj) {
  if (is.matrix(post_obj)) {
    return(max.col(post_obj[nrow(post_obj), , drop = FALSE], ties.method = "first"))
  }

  if (is.data.frame(post_obj)) {
    prob_cols <- grep("^S[0-9]+$", names(post_obj), value = TRUE)
    if (length(prob_cols) >= 2) {
      last_probs <- as.numeric(post_obj[nrow(post_obj), prob_cols, drop = TRUE])
      return(which.max(last_probs))
    }
    if ("state" %in% names(post_obj)) {
      return(as.integer(post_obj$state[nrow(post_obj)]))
    }
  }

  stop("Could not extract filtered HMM state.")
}

extract_state_sequence <- function(post_obj) {
  if (is.matrix(post_obj)) {
    return(apply(post_obj, 1, which.max))
  }

  if (is.data.frame(post_obj)) {
    if ("state" %in% names(post_obj)) {
      return(as.integer(post_obj$state))
    }
    prob_cols <- grep("^S[0-9]+$", names(post_obj), value = TRUE)
    if (length(prob_cols) >= 2) {
      probs <- as.matrix(post_obj[, prob_cols, drop = FALSE])
      return(apply(probs, 1, which.max))
    }
  }

  stop("Could not extract filtered HMM state sequence.")
}

min_history <- 24L
df$hmm_state <- NA_integer_

for (t in seq_len(nrow(df))) {
  if (t < min_history) next

  window_df <- df[seq_len(t), c("MKT_RF"), drop = FALSE]
  set.seed(42)
  hmm_spec_t <- depmixS4::depmix(MKT_RF ~ 1, data = window_df, nstates = 2, family = gaussian())
  hmm_fit_t <- depmixS4::fit(hmm_spec_t, verbose = FALSE)
  hmm_post_t <- depmixS4::posterior(hmm_fit_t, type = "filtering")

  # Explicit stable mapping each fit: low-variance state -> calm, high-variance -> turbulent.
  state_seq_t <- extract_state_sequence(hmm_post_t)
  state_vars_t <- tapply(window_df$MKT_RF, state_seq_t, var, na.rm = TRUE)
  if (length(state_vars_t) != 2 || any(!is.finite(state_vars_t))) {
    stop("HMM state variance mapping failed at index ", t, ".")
  }
  calm_state_t <- as.integer(names(state_vars_t)[which.min(state_vars_t)])
  turbulent_state_t <- as.integer(names(state_vars_t)[which.max(state_vars_t)])

  state_t <- extract_last_state(hmm_post_t)
  df$hmm_state[t] <- ifelse(state_t == turbulent_state_t, 2L, 1L)
}

df$regime_label <- ifelse(df$hmm_state == 2L, "turbulent", ifelse(df$hmm_state == 1L, "calm", NA))
df$regime_label <- as.character(df$regime_label)
df$regime_next <- lead1(df$regime_label)
df$regime_next <- as.character(df$regime_next)

factor_cols <- c("MKT_RF", "SMB", "HML", "MOM", "RF")
for (nm in factor_cols) {
  df[[paste0(nm, "_L1")]] <- lag1(df[[nm]])
}

for (nm in c("MKT_RF", "SMB", "HML", "MOM")) {
  df[[paste0(nm, "_VOL3_L1")]] <- lag1(zoo::rollapply(df[[nm]], width = 3, FUN = sd, align = "right", fill = NA))
  df[[paste0(nm, "_MOM3_L1")]] <- lag1(zoo::rollapply(df[[nm]], width = 3, FUN = sum, align = "right", fill = NA))
}

feature_cols <- c(
  "MKT_RF_L1", "SMB_L1", "HML_L1", "MOM_L1", "RF_L1",
  "MKT_RF_VOL3_L1", "SMB_VOL3_L1", "HML_VOL3_L1", "MOM_VOL3_L1",
  "MKT_RF_MOM3_L1", "SMB_MOM3_L1", "HML_MOM3_L1", "MOM_MOM3_L1",
  "VIXCLS_L1", "T10Y2Y_L1", "FEDFUNDS_L1", "UNRATE_L1",
  "DLOG_VIX_L1", "DT10Y2Y_L1", "DFEDFUNDS_L1", "DUNRATE_L1"
)

feature_matrix <- df[, c("date", feature_cols, "regime_label", "regime_next")]
feature_matrix <- feature_matrix[complete.cases(feature_matrix[, c(feature_cols, "regime_next")]), ]
feature_matrix <- feature_matrix[order(feature_matrix$date), ]

state_counts <- as.data.frame(table(feature_matrix$regime_next))
names(state_counts) <- c("regime_next", "count")
state_counts$share <- round(state_counts$count / sum(state_counts$count), 4)

regime_label_factor <- factor(feature_matrix$regime_label, levels = c("calm", "turbulent"))
regime_next_factor <- factor(feature_matrix$regime_next, levels = c("calm", "turbulent"))

transition_raw <- table(regime_label_factor, regime_next_factor)
transition_tbl <- as.data.frame.matrix(transition_raw)
transition_tbl$from_state <- rownames(transition_tbl)
rownames(transition_tbl) <- NULL
transition_tbl <- transition_tbl[, c("from_state", "calm", "turbulent")]

persistence <- data.frame(
  metric = c("calm_to_calm", "turbulent_to_turbulent"),
  value = c(
    ifelse(sum(transition_raw["calm", ]) > 0, transition_raw["calm", "calm"] / sum(transition_raw["calm", ]), NA),
    ifelse(sum(transition_raw["turbulent", ]) > 0, transition_raw["turbulent", "turbulent"] / sum(transition_raw["turbulent", ]), NA)
  )
)

run_id <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")

write.csv(df, file.path(dir_processed, "regime_labeled_dataset.csv"), row.names = FALSE)
write.csv(feature_matrix, file.path(dir_processed, "feature_matrix_phase3.csv"), row.names = FALSE)
write.csv(feature_matrix, file.path(dir_processed, paste0("feature_matrix_phase3_", run_id, ".csv")), row.names = FALSE)

write.csv(state_counts, file.path(dir_tables, "phase3_state_balance.csv"), row.names = FALSE)
write.csv(transition_tbl, file.path(dir_tables, "phase3_transition_matrix.csv"), row.names = FALSE)
write.csv(persistence, file.path(dir_tables, "phase3_state_persistence.csv"), row.names = FALSE)

message("Phase 3 labels/features complete.")
