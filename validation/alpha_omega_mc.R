#!/usr/bin/env Rscript
# Monte Carlo validation of FiabilityLab's Alpha-vs-Omega discordance rule.
#
# Rule under test (R/internalconsistency.b.R:898):
#   flag "coefficients disagree" iff |alpha - omega| > .04
#
# This script does NOT reimplement alpha/omega. It calls FiabilityLab's own
# exported internalConsistency() on simulated single-factor data and reads
# whether the shipped discordance panel actually fires, so the validation
# exercises the real code path (same approach as AssumptionsLab's AJS
# Monte Carlo work on its own decision rules).
#
# Ground truth is fully controlled by the simulation: items are generated
# from a single common factor with a known vector of loadings. Loading
# heterogeneity (spread of the loading vector) is the manipulated violation
# of tau-equivalence; loadings all equal is the null case (loading_sd = 0).
#
# Run from the package root:
#   Rscript validation/alpha_omega_mc.R [n_reps]
# n_reps defaults to 300 per cell; pass a smaller number for a quick check.

suppressMessages(library(pkgload))
pkgload::load_all(".", quiet = TRUE)

# NOTE ON RUNTIME: each internalConsistency() call costs ~4.4 sec (psych::omega
# runs a parallel-analysis factor-count estimation internally on every call,
# with bootstrapCi=FALSE not skipping it) -- this is the real shipped code
# path, not something this script can speed up. n_reps=60 below (default)
# means 3 n-levels x 5 loading_sd-levels x 60 reps = 900 calls, ~66 min.
# Raise n_reps for a final, more precise run once the design is confirmed.
args <- commandArgs(trailingOnly = TRUE)
n_reps <- if (length(args) >= 1) as.integer(args[[1]]) else 60L

set.seed(20260909)

k_items   <- 6L
sample_ns <- c(50L, 150L, 400L)
# mean loading held at .65; loading_sd is the manipulated heterogeneity
# (spread across the k items' loadings), clamped to a plausible range.
loading_sd_levels <- c(0.00, 0.05, 0.15, 0.25, 0.35)

make_loadings <- function(k, mean_l, sd_l) {
  if (sd_l == 0) return(rep(mean_l, k))
  spread <- seq(-1, 1, length.out = k) * sd_l * sqrt(3)
  pmin(pmax(mean_l + spread, 0.15), 0.95)
}

simulate_items <- function(n, loadings) {
  k <- length(loadings)
  f <- rnorm(n)
  df <- as.data.frame(lapply(loadings, function(l) l * f + sqrt(1 - l^2) * rnorm(n)))
  names(df) <- paste0("i", seq_len(k))
  df
}

run_one <- function(n, loading_sd) {
  loadings <- make_loadings(k_items, mean_l = 0.65, sd_l = loading_sd)
  df <- simulate_items(n, loadings)
  res <- suppressWarnings(suppressMessages(
    internalConsistency(
      data = df, items = names(df),
      alpha = TRUE, omega = TRUE, bootstrapCi = FALSE, showPlots = FALSE,
      itemAnalysis = FALSE, normality = FALSE,
      checkDimensionality = FALSE, checkReliabilityAssumptions = FALSE)
  ))
  mt <- res$mainTable$asDF
  alpha_v <- mt$value[mt$coefficient == "Cronbach's α"]
  omega_v <- mt$value[grepl("^McDonald", mt$coefficient)]
  alpha_v <- if (length(alpha_v)) alpha_v[1] else NA_real_
  omega_v <- if (length(omega_v)) omega_v[1] else NA_real_
  fired <- isTRUE(res$discordanceNote$visible) &&
    grepl("disagree|no coinciden", res$discordanceNote$content)
  data.frame(
    n = n, loading_sd = loading_sd,
    true_loading_range = diff(range(loadings)),
    alpha = alpha_v, omega = omega_v,
    abs_gap = abs(alpha_v - omega_v),
    panel_fired = fired)
}

grid <- expand.grid(n = sample_ns, loading_sd = loading_sd_levels, rep = seq_len(n_reps))
cat(sprintf("Running %d cells (%d n-levels x %d loading_sd-levels x %d reps)...\n",
            nrow(grid), length(sample_ns), length(loading_sd_levels), n_reps))

results <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  if (i %% 200 == 0) cat(sprintf("  %d / %d\n", i, nrow(grid)))
  run_one(grid$n[i], grid$loading_sd[i])
}))

out_path <- file.path("validation", "results_alpha_omega_mc.csv")
write.csv(results, out_path, row.names = FALSE)

# Clopper-Pearson exact CI on each cell's firing rate, to accompany the
# point estimate: a rate from n_reps=60 carries substantial Monte Carlo
# error on its own (see README/paper discussion).
cells <- unique(results[, c("n", "loading_sd")])
summary_tbl <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
  sub <- results$panel_fired[results$n == cells$n[i] & results$loading_sd == cells$loading_sd[i]]
  k <- sum(sub); ntot <- length(sub)
  ci <- binom.test(k, ntot)$conf.int
  data.frame(n = cells$n[i], loading_sd = cells$loading_sd[i], n_reps = ntot,
             panel_fired = k / ntot, ci_lower = ci[1], ci_upper = ci[2])
}))
summary_tbl <- summary_tbl[order(summary_tbl$n, summary_tbl$loading_sd), ]
summary_path <- file.path("validation", "results_alpha_omega_mc_summary.csv")
write.csv(summary_tbl, summary_path, row.names = FALSE)
cat("\nEmpirical panel-firing rate with 95% Clopper-Pearson CI:\n")
print(summary_tbl, row.names = FALSE)
cat(sprintf("\nFull per-replicate results written to: %s\n", out_path))
cat(sprintf("Summary with CIs written to: %s\n", summary_path))
