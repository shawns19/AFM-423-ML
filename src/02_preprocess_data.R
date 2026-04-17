#!/usr/bin/env Rscript
# Phase 2: Preprocessing

options(stringsAsFactors = FALSE, warn = 1)

suppressPackageStartupMessages({
  if (!require(zoo)) install.packages("zoo", repos = "https://cloud.r-project.org")
  library(zoo)
})

resolve_project_root <- function() {
  cwd <- normalizePath(getwd(), mustWork = TRUE)
  candidate <- normalizePath(file.path(cwd, "afm 423/gpr/gpr2"), mustWork = FALSE)
  if (dir.exists(candidate)) return(candidate)
  if (basename(cwd) == "gpr2") return(cwd)
  stop("Could not resolve project root. Run from repository root or from gpr2/.")
}

project_root <- resolve_project_root()
dir_raw <- file.path(project_root, "data_raw")
dir_processed <- file.path(project_root, "data_processed")
dir.create(dir_processed, recursive = TRUE, showWarnings = FALSE)

latest_snapshot_file <- file.path(dir_raw, "latest_snapshot.txt")
if (!file.exists(latest_snapshot_file)) {
  stop("Missing data_raw/latest_snapshot.txt. Run src/01_pull_data.R first.")
}
snapshot_dir <- readLines(latest_snapshot_file, warn = FALSE)[1]
if (!dir.exists(snapshot_dir)) stop("Snapshot dir does not exist: ", snapshot_dir)

factors <- read.csv(file.path(snapshot_dir, "factors_monthly_raw.csv"))
fred_daily <- read.csv(file.path(snapshot_dir, "fred_daily_raw.csv"))
factors$date <- as.Date(factors$date)
fred_daily$date <- as.Date(fred_daily$date)

# convert percent returns to decimal
return_cols <- c("MKT_RF", "SMB", "HML", "MOM", "RF")
for (nm in return_cols) factors[[nm]] <- as.numeric(factors[[nm]]) / 100
factors$MKT <- factors$MKT_RF + factors$RF
factors <- factors[order(factors$date), ]

to_monthly_last <- function(df, value_col) {
  ym <- as.yearmon(df$date)
  split_idx <- split(seq_len(nrow(df)), ym)
  rows <- vapply(split_idx, function(idx) {
    valid <- idx[!is.na(df[[value_col]][idx])]
    if (length(valid) == 0) return(NA_integer_)
    valid[length(valid)]
  }, integer(1))
  rows <- rows[!is.na(rows)]
  out <- data.frame(
    date = as.Date(as.yearmon(names(rows)), frac = 1),
    value = as.numeric(df[[value_col]][rows]),
    stringsAsFactors = FALSE
  )
  names(out)[2] <- value_col
  out
}

fred_codes <- c("VIXCLS", "T10Y2Y", "FEDFUNDS", "UNRATE")
fred_monthly_parts <- lapply(fred_codes, function(code) to_monthly_last(fred_daily, code))
fred_monthly <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), fred_monthly_parts)
fred_monthly <- fred_monthly[order(fred_monthly$date), ]

# yearmon sequence avoids duplicate month-ends from R calendar arithmetic on seq(Date)
ym_seq <- seq(as.yearmon(min(factors$date)), as.yearmon(max(factors$date)), by = 1/12)
monthly_calendar <- data.frame(date = as.Date(ym_seq, frac = 1))

panel <- merge(monthly_calendar, factors, by = "date", all.x = TRUE)
panel <- merge(panel, fred_monthly, by = "date", all.x = TRUE)
panel <- panel[order(panel$date), ]

# Missing policy for macro levels: use last available observation at month-end.
for (code in fred_codes) {
  panel[[code]] <- zoo::na.locf(panel[[code]], na.rm = FALSE)
}

lag1 <- function(x) c(NA, x[-length(x)])

panel$DLOG_VIX <- c(NA, diff(log(panel$VIXCLS)))
panel$DT10Y2Y <- c(NA, diff(panel$T10Y2Y))
panel$DFEDFUNDS <- c(NA, diff(panel$FEDFUNDS))
panel$DUNRATE <- c(NA, diff(panel$UNRATE))

macro_features <- c("VIXCLS", "T10Y2Y", "FEDFUNDS", "UNRATE", "DLOG_VIX", "DT10Y2Y", "DFEDFUNDS", "DUNRATE")
for (nm in macro_features) {
  panel[[paste0(nm, "_L1")]] <- lag1(panel[[nm]])
}

required_cols <- c("MKT_RF", "SMB", "HML", "MOM", "RF", "VIXCLS_L1", "T10Y2Y_L1", "FEDFUNDS_L1", "UNRATE_L1")
is_complete <- complete.cases(panel[, required_cols])
clean_monthly_model_base <- panel[is_complete, ]
clean_monthly_model_base <- clean_monthly_model_base[order(clean_monthly_model_base$date), ]

run_id <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")

write.csv(panel, file.path(dir_processed, "monthly_panel_pre_dropna.csv"), row.names = FALSE)
write.csv(clean_monthly_model_base, file.path(dir_processed, "clean_monthly_model_base.csv"), row.names = FALSE)
write.csv(clean_monthly_model_base, file.path(dir_processed, paste0("clean_monthly_model_base_", run_id, ".csv")), row.names = FALSE)

preprocess_manifest <- data.frame(
  run_id_utc = run_id,
  run_timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  snapshot_dir = snapshot_dir,
  rows_before_dropna = nrow(panel),
  rows_after_dropna = nrow(clean_monthly_model_base),
  stringsAsFactors = FALSE
)
write.csv(preprocess_manifest, file.path(dir_processed, "preprocess_manifest.csv"), row.names = FALSE)

message("Phase 2 preprocessing complete.")
