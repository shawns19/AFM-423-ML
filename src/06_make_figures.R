# Phase 6: Figure generation

# ── 0. Dependencies ──────────────────────────────────────────────────────────
for (pkg in c("ggplot2", "scales", "tidyr", "dplyr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# ── 1. Resolve project root ──────────────────────────────────────────────────
resolve_project_root <- function() {
  cwd <- normalizePath(getwd(), mustWork = TRUE)
  candidate <- normalizePath(file.path(cwd, "afm 423/gpr/gpr2"), mustWork = FALSE)
  if (dir.exists(candidate)) return(candidate)
  if (basename(cwd) == "gpr2") return(cwd)
  stop("Could not resolve project root. Run from workspace root or gpr2/.")
}

ROOT <- resolve_project_root()
message("Project root: ", ROOT)

# ── 2. Paths ─────────────────────────────────────────────────────────────────
PATH_TS    <- file.path(ROOT, "data_processed", "backtest_timeseries.csv")
PATH_PRED  <- file.path(ROOT, "data_processed", "oos_predictions.csv")
PATH_PERF  <- file.path(ROOT, "output", "tables", "backtest_performance_summary.csv")
PATH_FIGS  <- file.path(ROOT, "output", "figures")

for (f in c(PATH_TS, PATH_PRED, PATH_PERF)) {
  if (!file.exists(f)) stop("Missing input file: ", f)
}

dir.create(PATH_FIGS, showWarnings = FALSE, recursive = TRUE)

# ── 3. Load data ──────────────────────────────────────────────────────────────
ts   <- read.csv(PATH_TS,   stringsAsFactors = FALSE)
pred <- read.csv(PATH_PRED, stringsAsFactors = FALSE)
perf <- read.csv(PATH_PERF, stringsAsFactors = FALSE)

ts$date   <- as.Date(ts$date)
pred$date <- as.Date(pred$date)

# ── 4. Shared theme ──────────────────────────────────────────────────────────
theme_report <- function() {
  theme_bw(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 12),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title       = element_text(size = 10)
    )
}

COLORS <- c(
  benchmark        = "#555555",
  dynamic_logit    = "#2166AC",
  dynamic_xgboost  = "#D6604D"
)

## Figure 1: Cumulative Net Return Curves

cum_ret <- ts |>
  arrange(date) |>
  mutate(
    benchmark       = cumprod(1 + benchmark_net_return_10bps),
    dynamic_logit   = cumprod(1 + dynamic_logit_net_return_10bps),
    dynamic_xgboost = cumprod(1 + dynamic_xgboost_net_return_10bps)
  ) |>
  select(date, benchmark, dynamic_logit, dynamic_xgboost) |>
  pivot_longer(-date, names_to = "strategy", values_to = "cum_return")

# Sanity check — no NAs
if (anyNA(cum_ret$cum_return)) stop("NA values found in cumulative return series.")

cum_ret$strategy <- factor(cum_ret$strategy,
                           levels = c("benchmark", "dynamic_logit", "dynamic_xgboost"),
                           labels = c("Benchmark (EW)", "Dynamic Logit", "Dynamic XGBoost"))

p1 <- ggplot(cum_ret, aes(x = date, y = cum_return, colour = strategy)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = unname(COLORS)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Figure 1: Cumulative Net Returns (2007–2026)",
    x     = NULL,
    y     = "Cumulative Return (indexed to 1.0)"
  ) +
  theme_report()

out1 <- file.path(PATH_FIGS, "fig1_cumulative_returns.png")
ggsave(out1, p1, width = 9, height = 5, dpi = 150)
message("Saved: ", out1)

## Figure 2: Drawdown Curves

drawdown <- ts |>
  arrange(date) |>
  mutate(
    cum_bench = cumprod(1 + benchmark_net_return_10bps),
    cum_logit = cumprod(1 + dynamic_logit_net_return_10bps),
    cum_xgb   = cumprod(1 + dynamic_xgboost_net_return_10bps),
    dd_bench  = cum_bench / cummax(cum_bench) - 1,
    dd_logit  = cum_logit / cummax(cum_logit) - 1,
    dd_xgb    = cum_xgb   / cummax(cum_xgb)   - 1
  ) |>
  select(date, benchmark = dd_bench, dynamic_logit = dd_logit, dynamic_xgboost = dd_xgb) |>
  pivot_longer(-date, names_to = "strategy", values_to = "drawdown")

if (anyNA(drawdown$drawdown)) stop("NA values found in drawdown series.")
drawdown$strategy <- factor(drawdown$strategy,
                            levels = c("benchmark", "dynamic_logit", "dynamic_xgboost"),
                            labels = c("Benchmark (EW)", "Dynamic Logit", "Dynamic XGBoost"))

p2 <- ggplot(drawdown, aes(x = date, y = drawdown, colour = strategy)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = unname(COLORS)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title = "Figure 2: Rolling Drawdown from Peak (2007–2026)",
    x     = NULL,
    y     = "Drawdown"
  ) +
  theme_report()

out2 <- file.path(PATH_FIGS, "fig2_drawdown.png")
ggsave(out2, p2, width = 9, height = 5, dpi = 150)
message("Saved: ", out2)

## Figure 3: Regime Timeline Overlay

regime_long <- pred |>
  select(date, logit_pred, xgb_pred) |>
  pivot_longer(
    cols      = c(logit_pred, xgb_pred),
    names_to  = "model",
    values_to = "regime"
  ) |>
  mutate(
    model  = recode(model,
                    logit_pred = "Logistic",
                    xgb_pred   = "XGBoost"),
    regime = factor(regime, levels = c("calm", "turbulent"))
  )

p3 <- ggplot(regime_long, aes(x = date, y = model, fill = regime)) +
  geom_tile(height = 0.85) +
  scale_fill_manual(
    values = c(calm = "#AEC6E8", turbulent = "#8B1A1A"),
    labels = c("Calm", "Turbulent")
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Figure 3: Regime Predictions — Logistic vs XGBoost (2007–2026)",
    x     = NULL,
    y     = NULL,
    fill  = "Regime"
  ) +
  theme_report() +
  theme(
    legend.position = "bottom",
    axis.text.y     = element_text(size = 11, face = "bold"),
    panel.grid      = element_blank()
  )

out3 <- file.path(PATH_FIGS, "fig3_regime_timeline.png")
ggsave(out3, p3, width = 9, height = 5, dpi = 150)
message("Saved: ", out3)

## Figure 4: Monthly Turnover Over Time

turnover_long <- ts |>
  arrange(date) |>
  select(date, dynamic_logit = dynamic_logit_turnover, dynamic_xgboost = dynamic_xgboost_turnover) |>
  pivot_longer(-date, names_to = "strategy", values_to = "turnover")

turnover_long$strategy <- factor(turnover_long$strategy,
                                 levels = c("dynamic_logit", "dynamic_xgboost"),
                                 labels = c("Dynamic Logit", "Dynamic XGBoost"))

# 6-month rolling average helper for visual clarity
roll6 <- function(x) {
  stats::filter(x, rep(1 / 6, 6), sides = 1)
}

turnover_long <- turnover_long |>
  group_by(strategy) |>
  arrange(date) |>
  mutate(turnover_smooth = as.numeric(roll6(turnover))) |>
  ungroup()

p4 <- ggplot(turnover_long, aes(x = date)) +
  geom_line(aes(y = turnover, colour = strategy), linewidth = 0.35, alpha = 0.35) +
  geom_line(aes(y = turnover_smooth, colour = strategy), linewidth = 0.9, na.rm = TRUE) +
  scale_colour_manual(values = c("Dynamic Logit" = "#2166AC", "Dynamic XGBoost" = "#D6604D")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    limits = c(0, NA)
  ) +
  labs(
    title    = "Figure 4: Monthly Portfolio Turnover (2007–2026)",
    subtitle = "Faint lines = raw monthly; bold lines = 6-month rolling average",
    x        = NULL,
    y        = "Turnover (sum |Δw|)"
  ) +
  theme_report()

out4 <- file.path(PATH_FIGS, "fig4_turnover.png")
ggsave(out4, p4, width = 9, height = 5, dpi = 150)
message("Saved: ", out4)

# ── 5. Final confirmation ─────────────────────────────────────────────────────
expected <- c("fig1_cumulative_returns.png", "fig2_drawdown.png",
              "fig3_regime_timeline.png",    "fig4_turnover.png")
missing <- expected[!file.exists(file.path(PATH_FIGS, expected))]
if (length(missing) > 0) {
  stop("The following figures were NOT saved: ", paste(missing, collapse = ", "))
}
message("\n=== All 4 figures saved to: ", PATH_FIGS, " ===")
