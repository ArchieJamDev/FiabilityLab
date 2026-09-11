#!/usr/bin/env Rscript
# Formal test of reliability (Omega) equality between sexes on the
# bfi/SAPA 5-factor model, following Raykov's approach of defining
# composite reliability as a nonlinear function of freely-estimated
# multigroup loadings and testing an equality constraint via LRT --
# requested independently by two external reviews (Perplexity, Mistral)
# as a stronger complement to the case-resampling bootstrap in
# omega_by_sex_bootstrap.R, which only gives descriptive/overlapping CIs,
# not a formal hypothesis test.
#
# Under WLSMV's default delta parameterization, each ordinal item's
# underlying continuous response is scaled to total variance 1, so for a
# single congeneric factor with loadings lambda_i:
#   Omega = (sum(lambda_i))^2 / [(sum(lambda_i))^2 + sum(1 - lambda_i^2)]
# This is written as a lavaan ":=" defined parameter directly from the
# (group-labeled, freely-estimated) loading parameters -- no separate
# residual-variance parameters needed. For each of the 5 facets, two
# models are compared: the fully free multigroup model (configural,
# already fit elsewhere) vs. the same model with one added nonlinear
# constraint forcing that facet's two group-specific Omegas to be equal.
# lavTestLRT() gives a formal chi-square-difference test (df=1) per
# facet -- the actual test both reviews asked for, not the bootstrap
# heuristic.
#
# Run from this directory:
#   Rscript omega_equality_lrt.R

suppressMessages(library(lavaan))

bfi <- read.csv("bfi_sapa.csv", row.names = 1)
rev_items <- c("A1", "C4", "C5", "E1", "E2", "O2", "O5")
for (it in rev_items) bfi[[it]] <- 7 - bfi[[it]]

items <- c(paste0("A", 1:5), paste0("C", 1:5), paste0("E", 1:5), paste0("N", 1:5), paste0("O", 1:5))
cc <- complete.cases(bfi[, c(items, "gender")])
d <- bfi[cc, c(items, "gender")]

facets <- list(
  Agree = paste0("A", 1:5),
  Consc = paste0("C", 1:5),
  Extra = paste0("E", 1:5),
  Neuro = paste0("N", 1:5),
  Open  = paste0("O", 1:5)
)

# Build multigroup model syntax with group-specific loading labels
# (c(<g1>,<g2>)*item) and one Omega ":=" parameter per facet per group.
build_loading_line <- function(factor_name, its) {
  labs_g1 <- paste0(tolower(factor_name), seq_along(its), "g1")
  labs_g2 <- paste0(tolower(factor_name), seq_along(its), "g2")
  terms <- sprintf("c(%s,%s)*%s", labs_g1, labs_g2, its)
  list(line = sprintf("%s =~ %s", factor_name, paste(terms, collapse = " + ")),
       labs_g1 = labs_g1, labs_g2 = labs_g2)
}

built <- lapply(names(facets), function(fn) build_loading_line(fn, facets[[fn]]))
names(built) <- names(facets)

omega_defs <- unlist(lapply(names(built), function(fn) {
  g1 <- built[[fn]]$labs_g1; g2 <- built[[fn]]$labs_g2
  sum_g1 <- paste(g1, collapse = "+"); sum_g2 <- paste(g2, collapse = "+")
  sq_g1 <- paste(sprintf("(1-%s^2)", g1), collapse = "+")
  sq_g2 <- paste(sprintf("(1-%s^2)", g2), collapse = "+")
  c(
    sprintf("omega_%s_g1 := (%s)^2 / ((%s)^2 + %s)", fn, sum_g1, sum_g1, sq_g1),
    sprintf("omega_%s_g2 := (%s)^2 / ((%s)^2 + %s)", fn, sum_g2, sum_g2, sq_g2)
  )
}))

model_free <- paste(c(
  sapply(built, function(b) b$line),
  omega_defs
), collapse = "\n")

cat("=== Fitting free (unconstrained-Omega) multigroup model ===\n")
fit_free <- cfa(model_free, data = d, group = "gender", ordered = items,
                 estimator = "WLSMV", std.lv = TRUE)
cat("Converged:", lavInspect(fit_free, "converged"), "\n")

pe <- parameterEstimates(fit_free)
omega_rows <- pe[grepl("^omega_", pe$lhs), c("lhs", "est", "se")]
cat("\nFree-model Omega point estimates (should match omega_by_sex_bootstrap.R point estimates):\n")
print(omega_rows, row.names = FALSE)

# One free-vs-constrained LRT per facet.
results <- lapply(names(facets), function(fn) {
  constraint <- sprintf("omega_%s_g1 == omega_%s_g2", fn, fn)
  model_constrained <- paste(model_free, constraint, sep = "\n")
  fit_c <- tryCatch(
    cfa(model_constrained, data = d, group = "gender", ordered = items,
        estimator = "WLSMV", std.lv = TRUE),
    error = function(e) NULL)
  if (is.null(fit_c) || !lavInspect(fit_c, "converged")) {
    return(data.frame(factor = fn, chisq_diff = NA, df_diff = NA, p_value = NA,
                       note = "constrained model did not converge"))
  }
  lrt <- tryCatch(lavTestLRT(fit_free, fit_c, method = "satorra.bentler.2010"),
                   error = function(e) NULL)
  if (is.null(lrt)) {
    return(data.frame(factor = fn, chisq_diff = NA, df_diff = NA, p_value = NA,
                       note = "LRT failed"))
  }
  data.frame(factor = fn,
             chisq_diff = lrt[["Chisq diff"]][2],
             df_diff = lrt[["Df diff"]][2],
             p_value = lrt[["Pr(>Chisq)"]][2],
             note = "")
})

out <- do.call(rbind, results)
cat("\n=== Formal test of Omega equality between sexes (scaled LRT, df=1 each) ===\n")
print(out, row.names = FALSE)
write.csv(out, "results_omega_equality_lrt.csv", row.names = FALSE)
cat("\nWritten to: results_omega_equality_lrt.csv\n")
