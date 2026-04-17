#!/usr/bin/env Rscript
# Phase 1: Data ingestion

options(stringsAsFactors = FALSE, warn = 1)

suppressPackageStartupMessages({
  if (!require(quantmod)) install.packages("quantmod", repos = "https://cloud.r-project.org")
  if (!require(zoo)) install.packages("zoo", repos = "https://cloud.r-project.org")
  library(quantmod)
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
dir_data_raw <- file.path(project_root, "data_raw")
dir_data_processed <- file.path(project_root, "data_processed")
dir.create(dir_data_raw, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_data_processed, recursive = TRUE, showWarnings = FALSE)

run_id <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
snapshot_dir <- file.path(dir_data_raw, run_id)
dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)

message("Snapshot directory: ", snapshot_dir)

parse_french_monthly <- function(zip_url, value_columns, dataset_name) {
  zip_path <- file.path(snapshot_dir, paste0(dataset_name, ".zip"))
  utils::download.file(zip_url, zip_path, mode = "wb", quiet = TRUE)

  file_listing <- unzip(zip_path, list = TRUE)
  csv_name <- file_listing$Name[1]
  unzip(zip_path, files = csv_name, exdir = snapshot_dir, overwrite = TRUE)
  csv_path <- file.path(snapshot_dir, csv_name)
  raw_lines <- readLines(csv_path, warn = FALSE)

  start_idx <- grep("^[0-9]{6},", raw_lines)[1]
  if (is.na(start_idx)) stop("Could not locate monthly data start for ", dataset_name)

  annual_idx <- grep("^[0-9]{4},", raw_lines)
  annual_idx <- annual_idx[annual_idx > start_idx]
  blank_idx <- which(trimws(raw_lines) == "")
  blank_idx <- blank_idx[blank_idx > start_idx]
  end_candidates <- c(annual_idx, blank_idx)
  end_idx <- if (length(end_candidates) > 0) min(end_candidates) else length(raw_lines) + 1

  header_idx <- start_idx - 1
  monthly_lines <- raw_lines[header_idx:(end_idx - 1)]
  monthly_df <- read.csv(text = paste(monthly_lines, collapse = "\n"), check.names = FALSE)
  names(monthly_df)[1] <- "date_yyyymm"

  keep_cols <- c("date_yyyymm", value_columns)
  missing_cols <- setdiff(keep_cols, names(monthly_df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in ", dataset_name, ": ", paste(missing_cols, collapse = ", "))
  }
  monthly_df <- monthly_df[, keep_cols]
  monthly_df$date <- as.Date(as.yearmon(as.character(monthly_df$date_yyyymm), "%Y%m"), frac = 1)
  monthly_df$date_yyyymm <- NULL

  for (nm in names(monthly_df)) {
    if (nm != "date") monthly_df[[nm]] <- as.numeric(monthly_df[[nm]])
  }

  monthly_df <- monthly_df[order(monthly_df$date), ]
  write.csv(monthly_df, file.path(snapshot_dir, paste0(dataset_name, "_monthly_raw.csv")), row.names = FALSE)
  monthly_df
}

message("Downloading Kenneth French datasets...")
ff3 <- parse_french_monthly(
  zip_url = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip",
  value_columns = c("Mkt-RF", "SMB", "HML", "RF"),
  dataset_name = "ff3"
)

mom <- parse_french_monthly(
  zip_url = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip",
  value_columns = c("Mom"),
  dataset_name = "mom"
)

factors_monthly <- merge(ff3, mom, by = "date", all = FALSE)
names(factors_monthly) <- c("date", "MKT_RF", "SMB", "HML", "RF", "MOM")
factors_monthly <- factors_monthly[order(factors_monthly$date), ]
write.csv(factors_monthly, file.path(snapshot_dir, "factors_monthly_raw.csv"), row.names = FALSE)

message("Downloading FRED series...")
fred_codes <- c("VIXCLS", "T10Y2Y", "FEDFUNDS", "UNRATE")
fred_list <- lapply(fred_codes, function(code) {
  out <- NULL
  xt <- tryCatch(
    suppressWarnings(getSymbols(code, src = "FRED", auto.assign = FALSE)),
    error = function(e) NULL
  )

  if (!is.null(xt)) {
    out <- data.frame(date = as.Date(index(xt)), value = as.numeric(xt[, 1]), stringsAsFactors = FALSE)
  } else {
    fred_url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", code)
    raw <- read.csv(fred_url, na.strings = c(".", ""))
    names(raw) <- c("date", "value")
    raw$date <- as.Date(raw$date)
    out <- raw
  }

  names(out)[2] <- code
  out <- out[order(out$date), ]
  write.csv(out, file.path(snapshot_dir, paste0("fred_", code, "_daily_raw.csv")), row.names = FALSE)
  out
})
names(fred_list) <- fred_codes

fred_daily <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), fred_list)
fred_daily <- fred_daily[order(fred_daily$date), ]
write.csv(fred_daily, file.path(snapshot_dir, "fred_daily_raw.csv"), row.names = FALSE)

to_monthly_last <- function(df, value_col) {
  ym <- as.yearmon(df$date)
  split_idx <- split(seq_len(nrow(df)), ym)
  rows <- vapply(split_idx, function(idx) {
    valid <- idx[!is.na(df[[value_col]][idx])]
    if (length(valid) == 0) return(NA_integer_)
    valid[length(valid)]
  }, integer(1))
  rows <- rows[!is.na(rows)]
  out <- data.frame(date = as.Date(as.yearmon(names(rows)), frac = 1), value = df[[value_col]][rows], stringsAsFactors = FALSE)
  names(out)[2] <- value_col
  out
}

fred_monthly_list <- lapply(fred_codes, function(code) to_monthly_last(fred_daily, code))
fred_monthly_draft <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), fred_monthly_list)
fred_monthly_draft <- fred_monthly_draft[order(fred_monthly_draft$date), ]

initial_panel <- merge(factors_monthly, fred_monthly_draft, by = "date", all = FALSE)
initial_panel <- initial_panel[order(initial_panel$date), ]

write.csv(fred_monthly_draft, file.path(snapshot_dir, "fred_monthly_draft.csv"), row.names = FALSE)
write.csv(initial_panel, file.path(dir_data_processed, "initial_monthly_panel_draft.csv"), row.names = FALSE)
write.csv(initial_panel, file.path(dir_data_processed, paste0("initial_monthly_panel_draft_", run_id, ".csv")), row.names = FALSE)

manifest <- data.frame(
  run_id_utc = run_id,
  run_timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  snapshot_dir = snapshot_dir,
  ff3_url = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip",
  mom_url = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip",
  fred_codes = paste(fred_codes, collapse = ";"),
  factors_rows = nrow(factors_monthly),
  fred_daily_rows = nrow(fred_daily),
  initial_panel_rows = nrow(initial_panel),
  stringsAsFactors = FALSE
)
write.csv(manifest, file.path(snapshot_dir, "snapshot_manifest.csv"), row.names = FALSE)
writeLines(snapshot_dir, con = file.path(dir_data_raw, "latest_snapshot.txt"))

message("Phase 1 ingestion complete.")
