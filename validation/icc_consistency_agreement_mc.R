#!/usr/bin/env Rscript
# Monte Carlo validation of FiabilityLab's ICC-forms discordance rule.
#
# Rule under test (R/interrater.b.R:640-651), continuous data only:
#   flag "ICC forms disagree" iff (ICC_consistency - ICC_absolute) > .05
#   NOTE: this is SIGNED, not abs() -- only one direction is ever flagged.
#
# This script does NOT reimplement the ICC. It calls FiabilityLab's own
# exported interRater() on simulated continuous rating data and reads
# whether the shipped discordance panel actually fires -- exercising the
# real code path (same approach as the other two validation scripts).
#
# Ground truth is fully controlled: k raters score n subjects from a common
# true score with rater-specific additive bias (shift) and independent
# noise. Rank order across subjects is preserved for every rater (only a
# constant is added per rater), so consistency-type reliability should stay
# high regardless of the shift, while absolute-agreement-type reliability
# should degrade as the shift grows -- the textbook condition the rule is
# meant to catch (Shrout & Fleiss, 1979). shift = 0 is the null case.
#
# A second condition checks the rule's DIRECTIONALITY claim: since the
# check is signed (not abs()), it should never fire under pure random noise
# with NO systematic shift, even at higher noise levels that lower both ICC
# forms roughly equally (that shouldn't create a consistency > absolute gap
# in either direction on average).
#
# Run from the package root:
#   Rscript validation/icc_consistency_agreement_mc.R [n_reps]
# n_reps defaults to 150 per cell; pass a smaller number for a quick check.

suppressMessages(pkgload::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
n_reps <- if (length(args) >= 1) as.integer(args[[1]]) else 150L

set.seed(20260909)

k_raters   <- 3L
sample_ns  <- c(20L, 50L, 150L)
noise_sd   <- 0.5
# shift_levels: magnitude of a systematic additive offset applied to one of
# the k raters (the others stay at 0); shift = 0 is the null case.
shift_levels <- c(0.0, 0.3, 0.6, 1.0, 1.5, 2.0)

simulate_ratings <- function(n, shift, noise = noise_sd) {
  true_score <- rnorm(n)
  bias <- c(0, 0, shift)[seq_len(k_raters)]
  df <- as.data.frame(lapply(bias, function(b) true_score + b + rnorm(n, sd = noise)))
  names(df) <- paste0("r", seq_len(k_raters))
  df
}

run_one <- function(n, shift) {
  df <- simulate_ratings(n, shift)
  res <- suppressWarnings(suppressMessages(
    interRater(data = df, ratings = names(df), dataType = "continuous",
               kappa = FALSE, gwet = FALSE, krippendorff = FALSE, icc = TRUE,
               checkIccAssumptions = FALSE, bootstrapCi = FALSE)
  ))
  mt <- res$mainTable$asDF
  icc_c <- mt$value[grepl("consistency|consistencia", mt$coefficient, ignore.case = TRUE)]
  icc_a <- mt$value[grepl("absolute|absoluto", mt$coefficient, ignore.case = TRUE)]
  icc_c <- if (length(icc_c)) icc_c[1] else NA_real_
  icc_a <- if (length(icc_a)) icc_a[1] else NA_real_
  fired <- isTRUE(res$discordanceNote$visible) &&
    grepl("disagree|no coinciden", res$discordanceNote$content)
  data.frame(n = n, shift = shift, icc_consistency = icc_c, icc_absolute = icc_a,
             signed_gap = icc_c - icc_a, panel_fired = fired)
}

grid <- expand.grid(n = sample_ns, shift = shift_levels, rep = seq_len(n_reps))
cat(sprintf("Running %d cells (%d n-levels x %d shift-levels x %d reps)...\n",
            nrow(grid), length(sample_ns), length(shift_levels), n_reps))

results <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  if (i %% 300 == 0) cat(sprintf("  %d / %d\n", i, nrow(grid)))
  run_one(grid$n[i], grid$shift[i])
}))

out_path <- file.path("validation", "results_icc_consistency_agreement_mc.csv")
write.csv(results, out_path, row.names = FALSE)

summary_tbl <- aggregate(panel_fired ~ n + shift, data = results, FUN = function(x) mean(x, na.rm = TRUE))
cat("\nEmpirical panel-firing rate (fraction of replicates flagged as discordant):\n")
print(summary_tbl[order(summary_tbl$n, summary_tbl$shift), ], row.names = FALSE)
cat(sprintf("\nFull per-replicate results written to: %s\n", out_path))

# ── Directionality check ──────────────────────────────────────────────────
# The rule is signed (icc_c - icc_a > .05), not abs(). Increasing pure noise
# with NO systematic shift should NOT create a one-directional gap, so the
# panel should stay silent here regardless of how much noise degrades both
# ICC forms together -- unlike shift, which should be the only thing this
# rule reacts to. A false-positive rate above chance here would mean the
# panel is (wrongly) reacting to reliability degradation in general, not
# specifically to rater bias.
noise_levels <- c(0.5, 1.0, 1.5, 2.5)
n_fixed <- 150L
dir_grid <- expand.grid(noise = noise_levels, rep = seq_len(n_reps))
cat(sprintf("\nRunning %d directionality-check cells (shift=0, n=%d, %d noise-levels x %d reps)...\n",
            nrow(dir_grid), n_fixed, length(noise_levels), n_reps))

run_noise_only <- function(n, noise) {
  df <- simulate_ratings(n, shift = 0, noise = noise)
  res <- suppressWarnings(suppressMessages(
    interRater(data = df, ratings = names(df), dataType = "continuous",
               kappa = FALSE, gwet = FALSE, krippendorff = FALSE, icc = TRUE,
               checkIccAssumptions = FALSE, bootstrapCi = FALSE)
  ))
  mt <- res$mainTable$asDF
  icc_c <- mt$value[grepl("consistency|consistencia", mt$coefficient, ignore.case = TRUE)]
  icc_a <- mt$value[grepl("absolute|absoluto", mt$coefficient, ignore.case = TRUE)]
  icc_c <- if (length(icc_c)) icc_c[1] else NA_real_
  icc_a <- if (length(icc_a)) icc_a[1] else NA_real_
  fired <- isTRUE(res$discordanceNote$visible) &&
    grepl("disagree|no coinciden", res$discordanceNote$content)
  data.frame(n = n, noise = noise, icc_consistency = icc_c, icc_absolute = icc_a,
             signed_gap = icc_c - icc_a, panel_fired = fired)
}

dir_results <- do.call(rbind, lapply(seq_len(nrow(dir_grid)), function(i) {
  run_noise_only(n_fixed, dir_grid$noise[i])
}))

dir_out_path <- file.path("validation", "results_icc_directionality_mc.csv")
write.csv(dir_results, dir_out_path, row.names = FALSE)

dir_summary <- aggregate(panel_fired ~ noise, data = dir_results, FUN = function(x) mean(x, na.rm = TRUE))
cat("\nDirectionality check -- false-positive rate under pure noise, no shift (should stay near 0):\n")
print(dir_summary[order(dir_summary$noise), ], row.names = FALSE)
cat(sprintf("\nFull per-replicate results written to: %s\n", dir_out_path))
