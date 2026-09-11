#!/usr/bin/env Rscript
# Bootstrap CI for Omega/CR by sex on the bfi/SAPA 5-factor model, to
# support the ChJS applied-example claim (paper.tex Section 5.2) that the
# pooled Omega table is a reasonable estimate for both sexes given
# metric/scalar invariance. Reconstructs the exact model already reported
# in the paper (verified to reproduce its pooled CFI robust=.751, RMSEA
# robust=.094, and all five Omega/AVE values before this script was
# written) and adds case-resampling bootstrap CIs per sex, which
# FiabilityLab's advancedReliability() does not expose on its own. Not
# part of the package's own validation suite -- lives here only because
# this repo already has a working GitHub Actions R environment, and the
# fit is too CPU-heavy (WLSMV CFA per bootstrap replicate) to run
# comfortably on a contended desktop.
#
# Run from this directory:
#   Rscript omega_by_sex_bootstrap.R [n_boot] [group: 1|2|both]
# group 1 = men, 2 = women, both = sequential (slow). Defaults: 150, both.

suppressMessages(library(lavaan))
suppressMessages(library(semTools))

args <- commandArgs(trailingOnly = TRUE)
n_boot <- if (length(args) >= 1) as.integer(args[[1]]) else 150L
which_group <- if (length(args) >= 2) args[[2]] else "both"

set.seed(20260911)

bfi <- read.csv("bfi_sapa.csv", row.names = 1)
rev_items <- c("A1", "C4", "C5", "E1", "E2", "O2", "O5")
for (it in rev_items) bfi[[it]] <- 7 - bfi[[it]]

items <- c(paste0("A", 1:5), paste0("C", 1:5), paste0("E", 1:5), paste0("N", 1:5), paste0("O", 1:5))
cc <- complete.cases(bfi[, c(items, "gender")])
d <- bfi[cc, c(items, "gender")]

model <- "
Agree =~ A1+A2+A3+A4+A5
Consc =~ C1+C2+C3+C4+C5
Extra =~ E1+E2+E3+E4+E5
Neuro =~ N1+N2+N3+N4+N5
Open  =~ O1+O2+O3+O4+O5
"

fit_omega <- function(dat) {
  fit <- tryCatch(
    cfa(model, data = dat, ordered = items, estimator = "WLSMV", std.lv = TRUE),
    error = function(e) NULL)
  if (is.null(fit) || !lavInspect(fit, "converged")) return(rep(NA_real_, 5))
  cr <- tryCatch(suppressMessages(compRelSEM(fit)), error = function(e) rep(NA_real_, 5))
  # semTools::compRelSEM() returns a plain named numeric vector for a
  # single-factor model, but a *list* of "lavaan.vector"-classed scalars
  # for a multi-factor model (semTools 0.5.9) -- same shape quirk already
  # fixed in R/advancedreliability.b.R. [[ + as.numeric() unwraps either
  # shape to a bare scalar; vapply over that gives a clean numeric vector
  # that assigns cleanly into a matrix row.
  vapply(c("Agree", "Consc", "Extra", "Neuro", "Open"),
         function(nm) tryCatch(as.numeric(cr[[nm]]), error = function(e) NA_real_),
         numeric(1))
}

run_group <- function(dat, label) {
  point <- fit_omega(dat)
  n <- nrow(dat)
  boot_mat <- matrix(NA_real_, nrow = n_boot, ncol = 5)
  for (b in seq_len(n_boot)) {
    idx <- sample(seq_len(n), size = n, replace = TRUE)
    boot_mat[b, ] <- fit_omega(dat[idx, ])
    if (b %% 10 == 0) cat(sprintf("[%s] boot %d/%d done\n", label, b, n_boot))
  }
  ci <- apply(boot_mat, 2, function(x) quantile(x, c(.025, .975), na.rm = TRUE))
  boot_se <- apply(boot_mat, 2, sd, na.rm = TRUE)
  out <- data.frame(
    factor = c("Agree", "Consc", "Extra", "Neuro", "Open"),
    omega = point,
    ci_lower = ci[1, ], ci_upper = ci[2, ], boot_se = boot_se,
    n_boot_valid = colSums(!is.na(boot_mat)))
  out_path <- sprintf("results_omega_by_sex_%s.csv", label)
  write.csv(out, out_path, row.names = FALSE)
  # Raw per-replicate bootstrap values, for a Wald test comparing groups
  # (needs the bootstrap SE, not just the percentile CI reported above).
  raw <- as.data.frame(boot_mat)
  names(raw) <- c("Agree", "Consc", "Extra", "Neuro", "Open")
  raw_path <- sprintf("results_omega_by_sex_%s_raw_boot.csv", label)
  write.csv(raw, raw_path, row.names = FALSE)
  cat(sprintf("\n=== %s (n=%d) ===\n", label, n))
  print(out, row.names = FALSE)
  cat(sprintf("Written to: %s and %s\n", out_path, raw_path))
  out
}

if (which_group %in% c("1", "both")) run_group(d[d$gender == 1, ], "men")
if (which_group %in% c("2", "both")) run_group(d[d$gender == 2, ], "women")
