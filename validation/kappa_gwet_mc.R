#!/usr/bin/env Rscript
# Monte Carlo validation of FiabilityLab's Kappa-vs-Gwet discordance rule
# (the "Kappa paradox").
#
# Rule under test (R/interrater.b.R:603-616), nominal/ordinal only:
#   flag "coefficients disagree" iff |Kappa - Gwet's AC1/AC2| > .05
#
# This script does NOT reimplement Kappa/Gwet. It calls FiabilityLab's own
# exported interRater() on simulated 2-rater binary-classification data and
# reads whether the shipped discordance panel actually fires -- exercising
# the real code path (same approach as alpha_omega_mc.R and as
# AssumptionsLab's AJS Monte Carlo work on its own decision rules).
#
# Ground truth is fully controlled: two raters independently misclassify a
# true binary state with a FIXED per-rater error rate (held constant across
# conditions, so raw observed agreement stays roughly constant too). The
# manipulated variable is category prevalence -- how skewed the true
# state's distribution is. Balanced prevalence (.50) is the null case;
# increasing skew is the classic condition under which Kappa becomes
# conservative relative to Gwet's AC1 even though rater accuracy hasn't
# changed (Gwet, 2014).
#
# Run from the package root:
#   Rscript validation/kappa_gwet_mc.R [n_reps]
# n_reps defaults to 150 per cell; pass a smaller number for a quick check.

suppressMessages(pkgload::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
n_reps <- if (length(args) >= 1) as.integer(args[[1]]) else 150L

set.seed(20260909)

sample_ns    <- c(50L, 150L, 400L)
# prevalence = P(true state = 1); .50 is balanced (null case), higher values
# are increasingly skewed toward category "1".
prevalence_levels <- c(0.50, 0.55, 0.60, 0.65, 0.70, 0.80, 0.90, 0.95)
rater_error  <- 0.08  # fixed per-rater misclassification rate, held constant
                      # across conditions so raw observed agreement doesn't
                      # confound the prevalence manipulation.

simulate_raters <- function(n, prevalence, error) {
  true_state <- rbinom(n, 1, prevalence)
  flip <- function(x) ifelse(runif(length(x)) < error, 1L - x, x)
  data.frame(r1 = factor(flip(true_state)), r2 = factor(flip(true_state)))
}

run_one <- function(n, prevalence) {
  df <- simulate_raters(n, prevalence, rater_error)
  # Degenerate draws (a rater using only one category) make Kappa
  # undefined; redraw up to 10 times before giving up on this replicate.
  attempt <- 0L
  res <- NULL
  repeat {
    attempt <- attempt + 1L
    ok <- nlevels(factor(df$r1)) > 1L && nlevels(factor(df$r2)) > 1L
    if (ok) {
      res <- tryCatch(
        suppressWarnings(suppressMessages(
          interRater(data = df, ratings = names(df), dataType = "nominal",
                     kappa = TRUE, gwet = TRUE, krippendorff = FALSE, icc = FALSE,
                     checkIccAssumptions = FALSE, bootstrapCi = FALSE)
        )),
        error = function(e) NULL)
    }
    if (!is.null(res) || attempt >= 10L) break
    df <- simulate_raters(n, prevalence, rater_error)
  }
  if (is.null(res)) {
    return(data.frame(n = n, prevalence = prevalence, kappa = NA_real_,
                       gwet = NA_real_, abs_gap = NA_real_, panel_fired = NA,
                       observed_agreement = NA_real_))
  }
  mt <- res$mainTable$asDF
  kappa_v <- mt$value[grepl("Kappa", mt$coefficient)]
  gwet_v  <- mt$value[grepl("Gwet|AC1|AC2", mt$coefficient)]
  kappa_v <- if (length(kappa_v)) kappa_v[1] else NA_real_
  gwet_v  <- if (length(gwet_v)) gwet_v[1] else NA_real_
  fired <- isTRUE(res$discordanceNote$visible) &&
    grepl("disagree|no coinciden", res$discordanceNote$content)
  obs_agree <- mean(df$r1 == df$r2)
  data.frame(n = n, prevalence = prevalence, kappa = kappa_v, gwet = gwet_v,
             abs_gap = abs(kappa_v - gwet_v), panel_fired = fired,
             observed_agreement = obs_agree)
}

grid <- expand.grid(n = sample_ns, prevalence = prevalence_levels, rep = seq_len(n_reps))
cat(sprintf("Running %d cells (%d n-levels x %d prevalence-levels x %d reps)...\n",
            nrow(grid), length(sample_ns), length(prevalence_levels), n_reps))

results <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  if (i %% 300 == 0) cat(sprintf("  %d / %d\n", i, nrow(grid)))
  run_one(grid$n[i], grid$prevalence[i])
}))

out_path <- file.path("validation", "results_kappa_gwet_mc.csv")
write.csv(results, out_path, row.names = FALSE)

# Clopper-Pearson exact CI per cell, on the non-degenerate replicates only
# (NA rows are draws where a rater used a single category in all 10 retry
# attempts and Kappa is undefined -- excluded from both numerator and
# denominator, not counted as non-firing).
cells <- unique(results[, c("n", "prevalence")])
summary_tbl <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
  sub <- results$panel_fired[results$n == cells$n[i] & results$prevalence == cells$prevalence[i]]
  sub <- sub[!is.na(sub)]
  k <- sum(sub); ntot <- length(sub)
  ci <- if (ntot > 0) binom.test(k, ntot)$conf.int else c(NA_real_, NA_real_)
  data.frame(n = cells$n[i], prevalence = cells$prevalence[i], n_reps = ntot,
             panel_fired = if (ntot > 0) k / ntot else NA_real_,
             ci_lower = ci[1], ci_upper = ci[2])
}))
summary_tbl <- summary_tbl[order(summary_tbl$n, summary_tbl$prevalence), ]
summary_path <- file.path("validation", "results_kappa_gwet_mc_summary.csv")
write.csv(summary_tbl, summary_path, row.names = FALSE)
cat("\nEmpirical panel-firing rate with 95% Clopper-Pearson CI:\n")
print(summary_tbl, row.names = FALSE)
cat(sprintf("\nFull per-replicate results written to: %s\n", out_path))
cat(sprintf("Summary with CIs written to: %s\n", summary_path))
