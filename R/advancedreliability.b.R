# -----------------------------------------------------------------------------
# FiabilityLab - Advanced Reliability (SEM).
#
# Phase 3 from ARCHITECTURE.md, as its own menu analysis (not a tab nested
# inside Internal Consistency): once the user knows -- from theory or a prior
# validation -- which items belong to which subscale, a confirmatory factor
# model (fit via lavaan) gives structure-specific reliability and validity
# evidence that the classical, exploratory-detection-only coefficients in
# Internal Consistency cannot: Composite Reliability/omega and AVE per
# subscale (Fornell & Larcker, 1981), Hancock & Mueller's (2001) H
# coefficient, HTMT discriminant validity between subscales (Henseler,
# Ringle & Sarstedt, 2015), omega hierarchical generalized to a confirmatory
# second-order structure with a formal likelihood-ratio comparison against
# the freely-correlated model (McDonald, 1999), and -- when fit is not
# adequate -- modification indices as a diagnostic to interpret through
# theory, never as an automatic edit (MacCallum, Roznowski & Necowitz,
# 1992). By far the most methodologically involved analysis in the suite,
# so its report is deliberately the most extensive: every table and every
# plot gets its own dedicated, data-grounded explanatory note directly
# beneath it -- the same "pair a result with its own note" convention
# interRater already uses for iccAssumptionsTable/iccAssumptionsNote and
# discordanceNote -- rather than leaving all interpretation to one summary
# panel at the end. Same admission contract as every other coefficient in
# this suite: Library entry + Bibliography citation before this code.
#
# ES: Fase 3 de ARCHITECTURE.md, como su propio análisis de menú (no una
# pestaña anidada dentro de Internal Consistency): una vez que el usuario
# sabe -- por teoría o una validación previa -- qué ítems pertenecen a qué
# subescala, un modelo factorial confirmatorio (ajustado con lavaan) da
# evidencia de confiabilidad y validez específica a la estructura que los
# coeficientes clásicos de detección solo exploratoria de Internal
# Consistency no pueden dar: Confiabilidad Compuesta/omega y AVE por
# subescala (Fornell & Larcker, 1981), el coeficiente H de Hancock & Mueller
# (2001), validez discriminante HTMT entre subescalas (Henseler, Ringle &
# Sarstedt, 2015), omega jerárquico generalizado a una estructura
# confirmatoria de segundo orden con una comparación formal de razón de
# verosimilitud contra el modelo libremente correlacionado (McDonald,
# 1999), y -- cuando el ajuste no es adecuado -- índices de modificación
# como diagnóstico a interpretar a través de la teoría, nunca como una
# edición automática (MacCallum, Roznowski & Necowitz, 1992). Por mucho el
# análisis metodológicamente más involucrado de la suite, así que su
# reporte es deliberadamente el más extenso: cada tabla y cada gráfico
# tiene su propia nota explicativa dedicada y basada en los datos
# directamente debajo -- la misma convención de "emparejar un resultado con
# su propia nota" que interRater ya usa para iccAssumptionsTable/
# iccAssumptionsNote y discordanceNote -- en vez de dejar toda la
# interpretación a un solo panel resumen al final. Mismo contrato de
# admisión que cualquier otro coeficiente de esta suite: entrada en la
# Library + cita en Bibliography antes de este código.
# -----------------------------------------------------------------------------

advancedReliabilityClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "advancedReliabilityClass",
    inherit = advancedReliabilityBase,
    private = list(

        .plot_rows     = NULL,   # data.frame: factor, value (CR/omega per factor)
        .loadings_data = NULL,   # data.frame: factor, item, loading
        .fitcmp_data   = NULL,   # data.frame: model, index, value (for CFI/TLI comparison plot)

        .tr = function(en, es) .fl_tr(en, es, self$options$reportLang),
        .plot_theme = function() .fl_plot_theme(self$options$plotStyle),
        .plot_colors = function() .fl_plot_colors(self$options$plotStyle),
        .interp_rel = function(val) .fl_interp_rel(val, private$.tr),
        .reset_table = function(table, n_rows) .fl_reset_table(table, n_rows),

        .esc = function(x) {
            x <- gsub("&", "&amp;", x, fixed = TRUE)
            x <- gsub("<", "&lt;", x, fixed = TRUE)
            gsub(">", "&gt;", x, fixed = TRUE)
        },

        # ── Fit a CFA with the estimator/missing-data strategy resolved
        # once for the whole run (ordinal items always use WLSMV, which
        # requires listwise deletion; continuous items use ML/MLR, with
        # FIML available for plausibly-MAR missingness). Shared by the
        # correlated-factors and second-order models so both are estimated
        # consistently. ──────────────────────────────────────────────────
        .fit_cfa = function(model_syntax, data, ordered_items, estimator, missing_eff) {
            fit_args <- list(model = model_syntax, data = data, std.lv = TRUE)
            if (!is.null(ordered_items)) {
                fit_args$ordered <- ordered_items
                fit_args$estimator <- "WLSMV"
            } else {
                fit_args$estimator <- estimator
                if (identical(missing_eff, "fiml")) fit_args$missing <- "fiml"
            }
            tryCatch(do.call(lavaan::cfa, fit_args), error = function(e) NULL)
        },

        # ── Fit measures extended with AIC/BIC (only meaningful under full
        # ML/MLR, not WLSMV's limited-information estimation) and robust
        # variants (available whenever the estimator is MLR or WLSMV) --
        # missing measures come back as NA rather than erroring. ──────────
        .fit_measures_ext = function(fit, estimator_eff) {
            is_robust <- estimator_eff %in% c("MLR", "WLSMV")
            # Under a robust/categorical estimator, the plain chisq/df/p are
            # not the recommended test (WLSMV's plain CFI/TLI/RMSEA can even
            # fall outside [0,1]) -- the scaled chi-square/df/p is, so we
            # read those into the same "chisq"/"df"/"pvalue" names the rest
            # of the code already expects, rather than adding parallel
            # columns nothing downstream would know to prefer.
            chisq_names <- if (is_robust) c("chisq.scaled", "df.scaled", "pvalue.scaled") else c("chisq", "df", "pvalue")
            fit_names <- c("cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
            robust_names <- if (is_robust) c("cfi.robust", "tli.robust", "rmsea.robust") else character(0)
            ic_names <- if (estimator_eff %in% c("ML", "MLR")) c("aic", "bic") else character(0)
            all_names <- c(chisq_names, fit_names, robust_names, ic_names)
            fm <- tryCatch(lavaan::fitMeasures(fit, all_names), error = function(e) NULL)
            out <- stats::setNames(rep(NA_real_, length(all_names)), all_names)
            if (!is.null(fm)) out[names(fm)] <- fm
            if (is_robust) {
                names(out)[names(out) == "chisq.scaled"] <- "chisq"
                names(out)[names(out) == "df.scaled"] <- "df"
                names(out)[names(out) == "pvalue.scaled"] <- "pvalue"
            }
            out
        },

        .fit_verdict = function(cfi, rmsea, srmr) {
            tr <- private$.tr
            if (any(is.na(c(cfi, rmsea, srmr)))) return(tr("N/A", "N/D"))
            if (cfi >= .95 && rmsea <= .06 && srmr <= .08) tr("Good", "Bueno")
            else if (cfi >= .90 && rmsea <= .08 && srmr <= .10) tr("Acceptable", "Aceptable")
            else tr("Poor", "Pobre")
        },

        # ── Solution admissibility: convergence only means the optimizer
        # stopped, not that the estimates are usable. lavInspect(...,
        # "post.check") flags Heywood cases and non-positive-definite
        # matrices directly; we additionally count negative residual
        # variances and out-of-range standardized loadings by hand (both
        # symptoms post.check can also catch, surfaced here per-item so the
        # note can name them) and the largest latent correlation (near/at 1
        # signals two factors may not be empirically distinct regardless of
        # what HTMT says about their indicators). ──────────────────────────
        .solution_diag = function(fit, items, factor_ids, n_analyzed, n_available, item_is_ordinal = FALSE, ordinal_data = NULL) {
            tr <- private$.tr
            post_ok <- tryCatch(isTRUE(lavaan::lavInspect(fit, "post.check")), error = function(e) NA)
            ss <- tryCatch(lavaan::standardizedSolution(fit), error = function(e) NULL)
            neg_var <- 0L
            out_of_bounds <- 0L
            max_corr <- NA_real_
            if (!is.null(ss)) {
                resid_rows <- ss[ss$op == "~~" & ss$lhs == ss$rhs & ss$lhs %in% items, ]
                neg_var <- sum(resid_rows$est.std < 0, na.rm = TRUE)
                load_rows <- ss[ss$op == "=~", ]
                out_of_bounds <- sum(abs(load_rows$est.std) >= 1, na.rm = TRUE)
                if (length(factor_ids) >= 2L) {
                    corr_rows <- ss[ss$op == "~~" & ss$lhs != ss$rhs & ss$lhs %in% factor_ids & ss$rhs %in% factor_ids, ]
                    if (nrow(corr_rows) > 0L) max_corr <- max(abs(corr_rows$est.std), na.rm = TRUE)
                }
            }
            # For ordinal items (polychoric/tetrachoric correlations under
            # WLSMV), an empty cell in a pair's contingency table makes that
            # pair's correlation unstable or inestimable -- worth flagging
            # directly rather than only via a downstream convergence
            # failure that gives no hint of the cause.
            empty_cells <- NA_integer_
            if (isTRUE(item_is_ordinal) && !is.null(ordinal_data) && length(items) >= 2L) {
                pairs <- utils::combn(items, 2, simplify = FALSE)
                empty_cells <- as.integer(sum(vapply(pairs, function(p) {
                    tb <- table(ordinal_data[[p[1]]], ordinal_data[[p[2]]])
                    any(tb == 0L)
                }, logical(1))))
            }
            admissible <- if (is.na(post_ok)) tr("N/A", "N/D") else if (post_ok) tr("Yes", "Sí") else tr("No", "No")
            list(converged = tr("Yes", "Sí"), admissible = admissible,
                 negVariances = as.integer(neg_var), loadingsOutOfBounds = as.integer(out_of_bounds),
                 maxLatentCorr = .fl_clean_na(max_corr), emptyCellPairs = .fl_clean_na(empty_cells),
                 nAnalyzed = as.integer(n_analyzed), nAvailable = as.integer(n_available),
                 flagged = isTRUE(!post_ok) || neg_var > 0L || out_of_bounds > 0L || isTRUE(empty_cells > 0L))
        },

        .run = function() {
            tr  <- private$.tr
            opt <- self$options
            res <- self$results
            esc <- private$.esc
            fit_verdict <- private$.fit_verdict

            all_result_names <- c("fitTable", "fitNote", "solutionDiagnosticsTable", "solutionDiagnosticsNote",
                                   "modelComparisonTable", "modelComparisonNote",
                                   "plotFitComparison", "plotFitComparisonNote",
                                   "reliabilityTable", "reliabilityNote", "plotComparison", "plotComparisonNote",
                                   "plotLoadings", "plotLoadingsNote", "htmtTable", "htmtNote", "omegaHNote",
                                   "modificationIndicesTable", "modificationIndicesNote", "interpretation")
            hide_all <- function() for (nm in all_result_names) res[[nm]]$setVisible(FALSE)
            bail <- function(msg_en, msg_es) {
                hide_all()
                res$interpretation$setVisible(TRUE)
                res$interpretation$setContent(.fl_prose("<p>&#9888; ", tr(msg_en, msg_es), "</p>"))
            }

            if (!requireNamespace("lavaan", quietly = TRUE) || !requireNamespace("semTools", quietly = TRUE)) {
                bail("This analysis requires the lavaan and semTools packages, which are not both installed here.",
                     "Este análisis requiere los paquetes lavaan y semTools, que no están ambos instalados aquí.")
                return()
            }

            # ── Collect factor definitions from the dynamic "Add New
            # Factor" list (safe internal ids for lavaan syntax; user-
            # entered labels kept only for display) -- same Array/Group
            # option + ListBox-with-addButton UI pattern as jamovi's own
            # built-in Confirmatory Factor Analysis, so any number of
            # factors is supported instead of a fixed handful of slots. ──
            factors <- list()
            factors_opt <- opt$factors
            for (i in seq_along(factors_opt)) {
                grp <- factors_opt[[i]]
                fitems <- grp$vars
                if (length(fitems) >= 3L) {
                    fname <- grp$label
                    if (is.null(fname) || !nzchar(trimws(fname))) fname <- paste(tr("Factor", "Factor"), i)
                    factors[[length(factors) + 1L]] <- list(id = paste0("F", i), name = fname, items = fitems)
                }
            }
            if (length(factors) == 0L) {
                bail("Assign at least 3 items to at least one factor to run the confirmatory analysis.",
                     "Asigne al menos 3 ítems a al menos un factor para correr el análisis confirmatorio.")
                return()
            }

            all_items <- unique(unlist(lapply(factors, function(f) f$items)))
            df_raw <- self$data[, all_items, drop = FALSE]
            n_available <- nrow(df_raw)
            for (col in names(df_raw)) df_raw[[col]] <- suppressWarnings(as.numeric(df_raw[[col]]))

            # ── Resolve estimator/item-type/missing-data strategy once for
            # the whole run. Ordinal items always take WLSMV (which needs
            # listwise deletion -- FIML has no ordinal/WLSMV equivalent
            # here); otherwise ML or MLR, optionally with FIML. ────────────
            item_is_ordinal <- identical(opt$itemType, "ordinal") || identical(opt$estimator, "wlsmv") ||
                (identical(opt$itemType, "auto") && .fl_is_low_cardinality(df_raw))
            if (item_is_ordinal) {
                estimator_eff <- "WLSMV"
                missing_eff <- "listwise"
            } else {
                estimator_eff <- switch(opt$estimator, mlr = "MLR", ml = "ML", "ML")
                missing_eff <- if (identical(opt$missingData, "fiml")) "fiml" else "listwise"
            }
            ordered_items <- if (item_is_ordinal) all_items else NULL
            df_adv <- if (identical(missing_eff, "fiml")) df_raw else na.omit(df_raw)
            n_adv <- nrow(df_adv)
            if (n_adv < 20L || length(all_items) < 3L) {
                bail("Not enough complete cases to fit a confirmatory factor model (minimum 20 required).",
                     "No hay suficientes casos completos para ajustar un modelo factorial confirmatorio (mínimo 20 requeridos).")
                return()
            }

            model_cf <- paste(vapply(factors, function(f)
                paste0(f$id, " =~ ", paste(f$items, collapse = " + ")), character(1)), collapse = "\n")
            fit_cf <- private$.fit_cfa(model_cf, df_adv, ordered_items, estimator_eff, missing_eff)
            cf_ok  <- !is.null(fit_cf) && isTRUE(tryCatch(lavaan::lavInspect(fit_cf, "converged"), error = function(e) FALSE))
            if (cf_ok && identical(missing_eff, "fiml"))
                n_adv <- tryCatch(lavaan::lavInspect(fit_cf, "ntotal"), error = function(e) n_adv)
            if (!cf_ok) {
                bail("The confirmatory factor model did not converge on this data/structure -- try fewer factors, more items per factor, or check the item assignments.",
                     "El modelo factorial confirmatorio no convergió con estos datos/estructura -- intente con menos factores, más ítems por factor, o revise las asignaciones.")
                return()
            }

            fnames_txt <- paste(vapply(factors, function(f) esc(f$name), character(1)), collapse = ", ")
            factor_ids <- vapply(factors, function(f) f$id, character(1))

            diag_rows <- list()
            diag_cf <- private$.solution_diag(fit_cf, all_items, factor_ids, n_adv, n_available, item_is_ordinal, df_adv)
            diag_rows[[length(diag_rows) + 1L]] <- c(list(model = tr("Correlated factors", "Factores correlacionados")), diag_cf[names(diag_cf) != "flagged"])

            fm_cf <- private$.fit_measures_ext(fit_cf, estimator_eff)
            pick <- function(fm, robust_name, plain_name) if (!is.na(fm[robust_name])) unname(fm[robust_name]) else unname(fm[plain_name])
            cf_verdict <- fit_verdict(pick(fm_cf, "cfi.robust", "cfi"), pick(fm_cf, "rmsea.robust", "rmsea"), fm_cf["srmr"])
            fit_rows <- list(list(
                model = tr("Correlated factors", "Factores correlacionados"),
                chisq = .fl_clean_na(unname(fm_cf["chisq"])), df = unname(fm_cf["df"]),
                pvalue = .fl_clean_na(unname(fm_cf["pvalue"])),
                cfi = .fl_clean_na(unname(fm_cf["cfi"])), tli = .fl_clean_na(unname(fm_cf["tli"])),
                rmsea = .fl_clean_na(unname(fm_cf["rmsea"])),
                rmsea_ci = if (is.na(fm_cf["rmsea.ci.lower"])) "" else paste0("[", round(fm_cf["rmsea.ci.lower"], 3), ", ", round(fm_cf["rmsea.ci.upper"], 3), "]"),
                srmr = .fl_clean_na(unname(fm_cf["srmr"])),
                aic = .fl_clean_na(unname(fm_cf["aic"])), bic = .fl_clean_na(unname(fm_cf["bic"])),
                cfi_robust = .fl_clean_na(unname(fm_cf["cfi.robust"])), tli_robust = .fl_clean_na(unname(fm_cf["tli.robust"])),
                rmsea_robust = .fl_clean_na(unname(fm_cf["rmsea.robust"])),
                verdict = cf_verdict))
            fitcmp_rows <- list(
                list(model = tr("Correlated factors", "Factores correlacionados"), index = "CFI", value = unname(fm_cf["cfi"])),
                list(model = tr("Correlated factors", "Factores correlacionados"), index = "TLI", value = unname(fm_cf["tli"])))

            # ── Per-factor loadings, CR/omega, AVE, Hancock & Mueller's H
            # (from the freely-correlated model -- each factor's OWN
            # reliability, independent of any second-order structure) ────
            invisible(capture.output(cr_vals <- suppressWarnings(suppressMessages(semTools::compRelSEM(fit_cf)))))
            ave_vals <- suppressWarnings(tryCatch(semTools::AVE(fit_cf), error = function(e) NULL))
            ss <- lavaan::standardizedSolution(fit_cf)

            loadings_rows <- list()
            rel_rows <- lapply(factors, function(f) {
                lam_rows <- ss[ss$op == "=~" & ss$lhs == f$id, c("rhs", "est.std")]
                for (r in seq_len(nrow(lam_rows)))
                    loadings_rows[[length(loadings_rows) + 1L]] <<- list(
                        factor = f$name, item = lam_rows$rhs[r], loading = lam_rows$est.std[r])
                lam_finite <- lam_rows$est.std[is.finite(lam_rows$est.std)]
                heywood_n <- sum(abs(lam_finite) >= 1)
                lam <- lam_finite[abs(lam_finite) < 1]
                h_terms <- (lam^2) / (1 - lam^2)
                h_val <- if (length(h_terms) > 0L) sum(h_terms) / (1 + sum(h_terms)) else NA_real_
                cr_val  <- if (!is.null(cr_vals) && f$id %in% names(cr_vals)) unname(cr_vals[f$id]) else NA_real_
                ave_val <- if (!is.null(ave_vals) && f$id %in% names(ave_vals)) unname(ave_vals[f$id]) else NA_real_
                interp <- paste0(private$.interp_rel(cr_val),
                    if (!is.na(ave_val) && ave_val < .50) paste0("; ", tr("AVE &lt; .50", "AVE &lt; .50")) else "",
                    if (heywood_n > 0L) paste0("; ", tr("Heywood case", "Caso Heywood")) else "")
                list(factor = esc(f$name), items = length(f$items),
                     cr = .fl_clean_na(cr_val), ave = .fl_clean_na(ave_val), h = .fl_clean_na(h_val),
                     rel_g = NA_real_, interpretation = interp)
            })

            private$.loadings_data <- if (length(loadings_rows) > 0L) data.frame(
                factor  = vapply(loadings_rows, function(r) r$factor, character(1)),
                item    = vapply(loadings_rows, function(r) r$item, character(1)),
                loading = vapply(loadings_rows, function(r) r$loading, numeric(1)),
                stringsAsFactors = FALSE) else NULL
            if (is.null(private$.loadings_data)) {
                res$plotLoadings$setVisible(FALSE)
                res$plotLoadingsNote$setVisible(FALSE)
            }
            # EN: A negative loading is a different problem from a merely
            # weak (low-positive) one -- it usually means the item needs
            # reverse-scoring (its raw values run opposite the rest of the
            # factor), not that it should be revised or dropped.
            # ES: Una carga negativa es un problema distinto a una
            # simplemente débil (positiva baja) -- usualmente significa que
            # el ítem necesita recodificación inversa (sus valores brutos
            # corren en sentido opuesto al resto del factor), no que deba
            # revisarse o eliminarse.
            neg_loadings <- NULL
            low_loadings <- NULL
            if (!is.null(private$.loadings_data)) {
                ld <- private$.loadings_data[!is.na(private$.loadings_data$loading), ]
                neg_loadings <- ld[ld$loading < 0, ]
                low_loadings <- ld[ld$loading >= 0 & ld$loading < .50, ]
            }
            weak_loadings <- if (!is.null(private$.loadings_data))
                private$.loadings_data[!is.na(private$.loadings_data$loading) & private$.loadings_data$loading < .50, ]
            else NULL

            # Data for the Reliability Comparison plot -- same forest-plot
            # convention as interRater's/Internal Consistency's own
            # .plotComparison, applied here to CR/omega per factor.
            private$.plot_rows <- data.frame(
                factor = vapply(rel_rows, function(r) r$factor, character(1)),
                value  = vapply(rel_rows, function(r) if (is.na(r$cr)) NA_real_ else r$cr, numeric(1)),
                stringsAsFactors = FALSE)
            if (all(is.na(private$.plot_rows$value))) {
                res$plotComparison$setVisible(FALSE)
                res$plotComparisonNote$setVisible(FALSE)
            }

            # ── HTMT (discriminant validity between factor pairs) ─────────
            htmt_rows <- list()
            if (length(factors) >= 2L) {
                hm <- tryCatch(semTools::htmt(model_cf, data = df_adv), error = function(e) NULL)
                if (!is.null(hm)) {
                    pairs <- combn(length(factors), 2)
                    for (p in seq_len(ncol(pairs))) {
                        fa <- factors[[pairs[1, p]]]; fb <- factors[[pairs[2, p]]]
                        val <- tryCatch(hm[fa$id, fb$id], error = function(e) NA_real_)
                        htmt_rows[[length(htmt_rows) + 1L]] <- list(
                            factor_a = esc(fa$name), factor_b = esc(fb$name),
                            htmt = .fl_clean_na(val),
                            verdict = if (is.na(val)) tr("N/A", "N/D") else if (val > .85) tr("Concern", "Preocupante") else tr("OK", "OK"))
                    }
                }
            }

            # ── Second-order model: fit, formal comparison against the
            # freely-correlated model (likelihood-ratio test), omega
            # hierarchical, and each factor's reliability specifically due
            # to G (requires >=3 first-order factors to be identified) ────
            omega_h <- NA_real_
            lrt <- NULL   # list(dchisq, ddf, p) once both models are fit
            ho_ok <- FALSE
            need_more_factors <- isTRUE(opt$secondOrder) && length(factors) < 3L
            if (isTRUE(opt$secondOrder) && length(factors) >= 3L) {
                model_2nd <- paste0(model_cf, "\nG =~ ", paste(vapply(factors, function(f) f$id, character(1)), collapse = " + "))
                fit_2nd <- private$.fit_cfa(model_2nd, df_adv, ordered_items, estimator_eff, missing_eff)
                ho_ok <- !is.null(fit_2nd) && isTRUE(tryCatch(lavaan::lavInspect(fit_2nd, "converged"), error = function(e) FALSE))
                if (ho_ok) {
                    diag_2nd <- private$.solution_diag(fit_2nd, all_items, c(factor_ids, "G"), n_adv, n_available, item_is_ordinal, df_adv)
                    diag_rows[[length(diag_rows) + 1L]] <- c(list(model = tr("Second-order (general factor)", "Segundo orden (factor general)")), diag_2nd[names(diag_2nd) != "flagged"])

                    fm_2nd <- private$.fit_measures_ext(fit_2nd, estimator_eff)
                    ho_verdict <- fit_verdict(pick(fm_2nd, "cfi.robust", "cfi"), pick(fm_2nd, "rmsea.robust", "rmsea"), fm_2nd["srmr"])
                    fit_rows[[length(fit_rows) + 1L]] <- list(
                        model = tr("Second-order (general factor)", "Segundo orden (factor general)"),
                        chisq = .fl_clean_na(unname(fm_2nd["chisq"])), df = unname(fm_2nd["df"]),
                        pvalue = .fl_clean_na(unname(fm_2nd["pvalue"])),
                        cfi = .fl_clean_na(unname(fm_2nd["cfi"])), tli = .fl_clean_na(unname(fm_2nd["tli"])),
                        rmsea = .fl_clean_na(unname(fm_2nd["rmsea"])),
                        rmsea_ci = if (is.na(fm_2nd["rmsea.ci.lower"])) "" else paste0("[", round(fm_2nd["rmsea.ci.lower"], 3), ", ", round(fm_2nd["rmsea.ci.upper"], 3), "]"),
                        srmr = .fl_clean_na(unname(fm_2nd["srmr"])),
                        aic = .fl_clean_na(unname(fm_2nd["aic"])), bic = .fl_clean_na(unname(fm_2nd["bic"])),
                        cfi_robust = .fl_clean_na(unname(fm_2nd["cfi.robust"])), tli_robust = .fl_clean_na(unname(fm_2nd["tli.robust"])),
                        rmsea_robust = .fl_clean_na(unname(fm_2nd["rmsea.robust"])),
                        verdict = ho_verdict)
                    fitcmp_rows[[length(fitcmp_rows) + 1L]] <- list(model = tr("Second-order", "Segundo orden"), index = "CFI", value = unname(fm_2nd["cfi"]))
                    fitcmp_rows[[length(fitcmp_rows) + 1L]] <- list(model = tr("Second-order", "Segundo orden"), index = "TLI", value = unname(fm_2nd["tli"]))

                    # Formal likelihood-ratio comparison: the second-order
                    # model is nested within (a constrained version of) the
                    # freely-correlated one, so lavTestLRT() gives the right
                    # chi-square-difference test (and, unlike a hand-computed
                    # chisq/df difference, automatically applies the correct
                    # scaled-difference correction if a robust estimator is
                    # ever used here). We read the returned anova/data.frame
                    # object's named columns directly rather than its printed
                    # table, so this does not depend on print formatting.
                    lrt_out <- tryCatch(lavaan::lavTestLRT(fit_cf, fit_2nd), error = function(e) NULL)
                    if (!is.null(lrt_out) && nrow(lrt_out) >= 2L && all(c("Chisq diff", "Df diff", "Pr(>Chisq)") %in% names(lrt_out))) {
                        d_chisq <- lrt_out[["Chisq diff"]][2]
                        d_df    <- lrt_out[["Df diff"]][2]
                        p_val   <- lrt_out[["Pr(>Chisq)"]][2]
                        lrt <- list(dchisq = d_chisq, ddf = d_df,
                                    p = if (is.finite(d_df) && d_df > 0) p_val else NA_real_)
                    } else {
                        d_chisq <- unname(fm_2nd["chisq"] - fm_cf["chisq"])
                        d_df    <- unname(fm_2nd["df"]    - fm_cf["df"])
                        lrt <- if (is.finite(d_chisq) && is.finite(d_df) && d_df > 0)
                            list(dchisq = d_chisq, ddf = d_df, p = stats::pchisq(d_chisq, d_df, lower.tail = FALSE))
                        else list(dchisq = d_chisq, ddf = d_df, p = NA_real_)
                    }

                    # Omega hierarchical for the TOTAL scale: proportion of
                    # the model-implied total-score variance attributable to
                    # G, via the loading chain item -> first-order factor ->
                    # G and lavaan's own model-implied covariance matrix
                    # (McDonald, 1999).
                    ss2 <- lavaan::standardizedSolution(fit_2nd)
                    g_load <- ss2[ss2$op == "=~" & ss2$lhs == "G", c("rhs", "est.std")]
                    f_load <- ss2[ss2$op == "=~" & ss2$lhs %in% vapply(factors, function(f) f$id, character(1)),
                                   c("lhs", "rhs", "est.std")]
                    f_load <- merge(f_load, g_load, by.x = "lhs", by.y = "rhs")
                    f_load$lambda_g <- f_load$est.std.x * f_load$est.std.y
                    lambda_g <- setNames(f_load$lambda_g, f_load$rhs)
                    implied <- tryCatch(lavaan::fitted(fit_2nd)$cov, error = function(e) NULL)
                    if (!is.null(implied)) {
                        lambda_g <- lambda_g[rownames(implied)]
                        omega_h <- sum(lambda_g)^2 / sum(implied)
                    }

                    # Each factor's OWN reliability due specifically to G --
                    # semTools' compRelSEM() on a model with a detected
                    # higher-order factor treats G, not the first-order
                    # factor, as the "true score" source, so this is a
                    # genuinely different (and typically smaller) number
                    # than the factor's own CR/omega above: the share of
                    # THAT reliable variance that is general rather than
                    # specific to the subscale.
                    rel_g_vals <- tryCatch({
                        out <- character(0)
                        invisible(capture.output(out <- suppressWarnings(suppressMessages(semTools::compRelSEM(fit_2nd)))))
                        out
                    }, error = function(e) NULL)
                    for (i in seq_along(rel_rows))
                        if (!is.null(rel_g_vals) && factors[[i]]$id %in% names(rel_g_vals))
                            rel_rows[[i]]$rel_g <- .fl_clean_na(unname(rel_g_vals[factors[[i]]$id]))
                }
            }

            # ── Write tables/plots-data now that rel_g may have been added ──
            ft <- res$fitTable
            private$.reset_table(ft, length(fit_rows))
            for (i in seq_along(fit_rows)) ft$setRow(rowNo = i, values = fit_rows[[i]])

            sdt <- res$solutionDiagnosticsTable
            private$.reset_table(sdt, length(diag_rows))
            for (i in seq_along(diag_rows)) sdt$setRow(rowNo = i, values = diag_rows[[i]])
            any_flagged <- isTRUE(diag_cf$flagged) || (isTRUE(opt$secondOrder) && ho_ok && isTRUE(diag_2nd$flagged))
            res$solutionDiagnosticsNote$setContent(.fl_prose(
                "<p>", tr(
                    "Convergence only means the optimizer stopped at some solution; it does not mean that solution is admissible. \"Admissible\" (from lavaan's own post-estimation check) means no negative residual variances, no non-positive-definite covariance matrix, and no other Heywood-case symptom -- a prerequisite for trusting CR, AVE, H, HTMT or omega hierarchical from that model. Negative residual variances and standardized loadings at or beyond 1 are two common, visible symptoms of an inadmissible solution; the largest latent correlation flags whether two factors may not be empirically distinct. For ordinal items, an item pair with an empty cell in its contingency table (some combination of categories that no one in the sample chose) makes that pair's underlying polychoric/tetrachoric correlation unstable or inestimable, which can silently degrade everything downstream even when the model still converges.",
                    "La convergencia solo significa que el optimizador se detuvo en alguna solución; no significa que esa solución sea admisible. \"Admisible\" (de la propia verificación posterior a la estimación de lavaan) significa sin varianzas residuales negativas, sin matriz de covarianza no definida positiva, y sin otro síntoma de caso Heywood -- un requisito previo para confiar en el CR, AVE, H, HTMT u omega jerárquico de ese modelo. Las varianzas residuales negativas y las cargas estandarizadas iguales o mayores a 1 son dos síntomas comunes y visibles de una solución inadmisible; la correlación latente máxima señala si dos factores podrían no ser empíricamente distintos. Para ítems ordinales, un par de ítems con una celda vacía en su tabla de contingencia (alguna combinación de categorías que nadie en la muestra eligió) hace que la correlación policórica/tetracórica subyacente de ese par sea inestable o inestimable, lo cual puede degradar silenciosamente todo lo que depende de ella aunque el modelo aún converja."),
                "</p>",
                if (any_flagged) paste0("<p>&#9888; ", tr(
                        "At least one model above shows a symptom of an inadmissible or borderline solution. Do not interpret CR, AVE, H, HTMT or omega hierarchical from that model until this is resolved -- common causes are too few cases for the number of parameters, near-perfectly correlated items, or a factor defined by too few (or too similar) indicators.",
                        "Al menos un modelo de arriba muestra un síntoma de una solución inadmisible o límite. No interprete el CR, AVE, H, HTMT u omega jerárquico de ese modelo hasta resolver esto -- las causas comunes son muy pocos casos para el número de parámetros, ítems casi perfectamente correlacionados, o un factor definido por muy pocos indicadores (o indicadores demasiado similares)."), "</p>")
                    else paste0("<p>&#10003; ", tr("No admissibility symptoms detected in the model(s) above.", "No se detectaron síntomas de inadmisibilidad en el/los modelo(s) de arriba."), "</p>")))

            rat <- res$reliabilityTable
            private$.reset_table(rat, length(rel_rows))
            for (i in seq_along(rel_rows)) rat$setRow(rowNo = i, values = rel_rows[[i]])

            hmt_tab <- res$htmtTable
            private$.reset_table(hmt_tab, length(htmt_rows))
            if (length(htmt_rows) > 0L) {
                for (i in seq_along(htmt_rows)) hmt_tab$setRow(rowNo = i, values = htmt_rows[[i]])
            } else {
                hmt_tab$setVisible(FALSE)
                res$htmtNote$setVisible(FALSE)
            }

            if (!isTRUE(opt$secondOrder)) {
                res$modelComparisonTable$setVisible(FALSE)
                res$modelComparisonNote$setVisible(FALSE)
                res$omegaHNote$setVisible(FALSE)
                res$plotFitComparison$setVisible(FALSE)
                res$plotFitComparisonNote$setVisible(FALSE)
            } else if (need_more_factors) {
                res$modelComparisonTable$setVisible(FALSE)
                res$plotFitComparison$setVisible(FALSE)
                res$plotFitComparisonNote$setVisible(FALSE)
                res$modelComparisonNote$setContent(.fl_prose("<p>&#9888; ", tr(
                    "A second-order general factor needs at least 3 first-order factors to be statistically identified; only ",
                    "Un factor general de segundo orden necesita al menos 3 factores de primer orden para estar estadísticamente identificado; solo se definieron "),
                    length(factors), tr(" were defined. Add another factor to enable this.", " . Agregue otro factor para habilitar esto."), "</p>"))
                res$omegaHNote$setVisible(FALSE)
            } else if (!ho_ok) {
                res$modelComparisonTable$setVisible(FALSE)
                res$plotFitComparison$setVisible(FALSE)
                res$plotFitComparisonNote$setVisible(FALSE)
                res$modelComparisonNote$setContent(.fl_prose("<p>&#9888; ", tr(
                    "The second-order model did not converge -- the general-factor structure may not fit this data even though the correlated-factors model above does.",
                    "El modelo de segundo orden no convergió -- la estructura de factor general puede no ajustar a estos datos aunque el modelo de factores correlacionados de arriba sí lo haga."), "</p>"))
                res$omegaHNote$setVisible(FALSE)
            } else {
                mct <- res$modelComparisonTable
                private$.reset_table(mct, 1L)
                mct$setRow(rowNo = 1, values = list(
                    comparison = tr("Second-order vs. correlated factors", "Segundo orden vs. factores correlacionados"),
                    dchisq = .fl_clean_na(lrt$dchisq), ddf = lrt$ddf, pvalue = .fl_clean_na(lrt$p),
                    verdict = if (is.na(lrt$p)) tr("Not testable (0 df)", "No comprobable (0 gl)")
                              else if (lrt$p < .05) tr("Second-order fits worse", "Segundo orden ajusta peor")
                              else tr("Fits comparably", "Ajusta de forma comparable")))

                res$modelComparisonNote$setContent(.fl_prose("<p>", if (is.na(lrt$p))
                        tr("With exactly 3 first-order factors, the second-order model is mathematically equivalent to the correlated-factors model above (identical fit, 0 degrees of freedom difference, shown as Δdf = 0 in the table above) — a genuinely testable comparison needs 4 or more first-order factors.",
                           "Con exactamente 3 factores de primer orden, el modelo de segundo orden es matemáticamente equivalente al de factores correlacionados de arriba (ajuste idéntico, 0 grados de libertad de diferencia, mostrado como Δgl = 0 en la tabla de arriba) — una comparación genuinamente comprobable necesita 4 o más factores de primer orden.")
                    else if (lrt$p < .05)
                        paste0(tr("Significant: imposing a single general factor over the ", "Significativo: imponer un único factor general sobre los "),
                               length(factors), tr(" subscales fits significantly worse than letting them correlate freely (see the table above). Treat the general-factor structure — and the Omega Hierarchical value below — with real skepticism; report per-factor reliability instead of a single general-factor score unless there is a strong theoretical reason to insist on one.",
                                  " subescalas ajusta significativamente peor que dejarlas correlacionar libremente (vea la tabla de arriba). Trate la estructura de factor general — y el valor de Omega Jerárquico de abajo — con verdadero escepticismo; reporte la confiabilidad por factor en vez de un único puntaje de factor general a menos que haya una razón teórica fuerte para insistir en uno."))
                    else
                        tr("Not significant: the general factor accounts for the correlations among subscales about as well as letting them correlate freely (see the table above), supporting the second-order structure statistically — though this is not the same as it being the theoretically correct structure; that judgment still rests on the instrument's conceptual design.",
                           "No significativo: el factor general explica las correlaciones entre subescalas casi tan bien como dejarlas correlacionar libremente (vea la tabla de arriba), respaldando estadísticamente la estructura de segundo orden — aunque esto no es lo mismo que sea la estructura teóricamente correcta; ese juicio sigue dependiendo del diseño conceptual del instrumento."),
                    "</p>"))

                res$omegaHNote$setContent(.fl_prose("<p>", tr(
                        paste0("Omega hierarchical for the total scale is <b>", round(omega_h, 3), "</b> (", private$.interp_rel(omega_h), ") — the proportion of total-score variance attributable specifically to the general factor G, net of each subscale's own specific variance (McDonald, 1999)."),
                        paste0("El omega jerárquico para la escala total es <b>", round(omega_h, 3), "</b> (", private$.interp_rel(omega_h), ") — la proporción de varianza del puntaje total atribuible específicamente al factor general G, descontando la varianza propia de cada subescala (McDonald, 1999).")),
                    "</p>",
                    "<p>", tr(
                        "This number is only as trustworthy as the model comparison above: if the likelihood-ratio test found the second-order structure fits significantly worse, treat this value as conditional on a structure the data do not support, not as independent evidence of a general dimension. Each factor's Reliability Table row below also lists its reliability specifically due to G — the share of that factor's own reliable variance that reflects the general trait rather than something specific to the subscale; a factor with high CR/&omega; but low reliability-due-to-G is measuring its own specific construct well, but a total score built by summing all items would discard most of that factor's reliable variance as if it were noise.",
                        "Este número solo es confiable en la medida en que lo sea la comparación de modelos de arriba: si la prueba de razón de verosimilitud encontró que la estructura de segundo orden ajusta significativamente peor, trate este valor como condicional a una estructura que los datos no respaldan, no como evidencia independiente de una dimensión general. La fila de cada factor en la Tabla de Confiabilidad de abajo también lista su confiabilidad debida específicamente a G — la parte de la varianza confiable propia de ese factor que refleja el rasgo general en vez de algo específico de la subescala; un factor con CR/&omega; alto pero confiabilidad-debida-a-G baja está midiendo bien su propio constructo específico, pero un puntaje total construido sumando todos los ítems descartaría la mayor parte de la varianza confiable de ese factor como si fuera ruido."),
                    "</p><p>", tr(
                        "This omega hierarchical is derived from a second-order model, which forces the general factor's relationship to each subscale to run entirely through that subscale's overall factor (a restricted, nested special case). It is a different quantity from the omega hierarchical of an exploratory or confirmatory bifactor model, which lets each item load on the general factor directly as well as its own subscale; the two need not agree, and disagreement between them is itself informative about which structure fits the construct better.",
                        "Este omega jerárquico se deriva de un modelo de segundo orden, que obliga a que la relación del factor general con cada subescala pase enteramente por el factor global de esa subescala (un caso especial restringido y anidado). Es una cantidad distinta del omega jerárquico de un modelo bifactor exploratorio o confirmatorio, que permite que cada ítem cargue directamente sobre el factor general además de su propia subescala; ambos no tienen por qué coincidir, y su desacuerdo es en sí mismo informativo sobre qué estructura ajusta mejor al constructo."),
                    "</p>"))

                private$.fitcmp_data <- data.frame(
                    model = vapply(fitcmp_rows, function(r) r$model, character(1)),
                    index = vapply(fitcmp_rows, function(r) r$index, character(1)),
                    value = vapply(fitcmp_rows, function(r) r$value, numeric(1)),
                    stringsAsFactors = FALSE)
                res$plotFitComparisonNote$setContent(.fl_prose("<p>", tr(
                    "Bars compare CFI and TLI (both 0-1, higher is better) between the correlated-factors and second-order models; the dashed line marks the .95 conventional good-fit threshold (Hu &amp; Bentler, 1999). Visibly shorter bars for the second-order model are the same information as a significant likelihood-ratio test above, shown graphically.",
                    "Las barras comparan CFI y TLI (ambos 0-1, mayor es mejor) entre los modelos de factores correlacionados y de segundo orden; la línea punteada marca el umbral convencional de buen ajuste de .95 (Hu &amp; Bentler, 1999). Barras visiblemente más cortas para el modelo de segundo orden son la misma información que una prueba de razón de verosimilitud significativa de arriba, mostrada gráficamente."), "</p>"))
            }

            # ── Modification indices (only surfaced when fit isn't Good --
            # a diagnostic to interpret through the model's theoretical
            # meaning, never an automatic edit) ────────────────────────────
            mi_rows <- list()
            if (!identical(cf_verdict, tr("Good", "Bueno"))) {
                mi <- tryCatch(lavaan::modindices(fit_cf, sort. = TRUE, maximum.number = 5L), error = function(e) NULL)
                if (!is.null(mi) && nrow(mi) > 0L) {
                    # Exclude self-pairs (lhs == rhs): under std.lv = TRUE,
                    # factor variances are fixed to 1 for identification, so
                    # lavaan flags "freeing" them as a large modification
                    # index -- not a substantive respecification like a
                    # cross-loading or a correlated residual between two
                    # different items.
                    mi <- mi[mi$mi > 3.84 & mi$lhs != mi$rhs, , drop = FALSE]
                    for (i in seq_len(min(nrow(mi), 5L))) {
                        r <- mi[i, ]
                        kind <- if (r$op == "=~") tr("Possible cross-loading", "Posible carga cruzada")
                                else if (r$op == "~~") tr("Possible correlated residual", "Posible residuo correlacionado")
                                else r$op
                        mi_rows[[length(mi_rows) + 1L]] <- list(
                            suggestion = paste0(esc(r$lhs), " ", r$op, " ", esc(r$rhs)),
                            type = kind, mi = .fl_clean_na(r$mi), epc = .fl_clean_na(r$epc))
                    }
                }
            }
            mit <- res$modificationIndicesTable
            private$.reset_table(mit, length(mi_rows))
            if (length(mi_rows) > 0L) {
                for (i in seq_along(mi_rows)) mit$setRow(rowNo = i, values = mi_rows[[i]])
                res$modificationIndicesNote$setContent(.fl_prose(
                    "<p>", tr(
                        "Model fit is not Good, so the largest candidate respecifications are listed above -- as diagnostics, never as automatic edits. Purely data-driven respecification capitalizes on chance features of this specific sample and may not replicate in a new one (MacCallum, Roznowski &amp; Necowitz, 1992).",
                        "El ajuste del modelo no es Bueno, así que arriba se listan las mayores reespecificaciones candidatas -- como diagnósticos, nunca como ediciones automáticas. La reespecificación puramente guiada por los datos capitaliza sobre características azarosas de esta muestra específica y puede no replicarse en una nueva (MacCallum, Roznowski &amp; Necowitz, 1992)."),
                    "</p><p>", tr(
                        "For each row: a \"Possible cross-loading\" (=~) suggests an item may reflect more than one factor -- only add it if the item's content plausibly does so conceptually. A \"Possible correlated residual\" (~~) between two items suggests they share variance beyond their common factor -- often explainable by shared wording or format (e.g. two reverse-worded items, or two items sharing a stem), not a sign either item is bad. Removing an item is only one of several possible responses to poor fit, not the default one.",
                        "Para cada fila: una \"Posible carga cruzada\" (=~) sugiere que un ítem podría reflejar más de un factor -- agréguela solo si el contenido del ítem lo hace plausible conceptualmente. Un \"Posible residuo correlacionado\" (~~) entre dos ítems sugiere que comparten varianza más allá de su factor común -- a menudo explicable por redacción o formato compartido (p. ej. dos ítems redactados en reversa, o dos ítems que comparten un enunciado), no una señal de que alguno de los dos ítems sea malo. Eliminar un ítem es solo una de varias respuestas posibles a un ajuste pobre, no la predeterminada."),
                    "</p>"))
            } else {
                mit$setVisible(FALSE)
                res$modificationIndicesNote$setVisible(FALSE)
            }

            # ── Fit note (always shown) ────────────────────────────────────
            estim_desc <- if (item_is_ordinal)
                tr("Items were treated as ordinal (WLSMV estimator on polychoric/tetrachoric correlations), which requires complete cases. &chi;&sup2;, CFI, TLI and RMSEA below are the scaled/robust versions WLSMV recommends, not the plain ones.",
                   "Los ítems se trataron como ordinales (estimador WLSMV sobre correlaciones policóricas/tetracóricas), lo cual requiere casos completos. El &chi;&sup2;, CFI, TLI y RMSEA de abajo son las versiones escaladas/robustas que WLSMV recomienda, no las simples.")
                else if (identical(estimator_eff, "MLR"))
                tr("Items were treated as continuous, fit with MLR (robust to non-normality). &chi;&sup2;, CFI, TLI and RMSEA below are the scaled/robust versions MLR recommends, not the plain ones.",
                   "Los ítems se trataron como continuos, ajustados con MLR (robusto a la no normalidad). El &chi;&sup2;, CFI, TLI y RMSEA de abajo son las versiones escaladas/robustas que MLR recomienda, no las simples.")
                else
                tr("Items were treated as continuous, fit with ML.", "Los ítems se trataron como continuos, ajustados con ML.")
            missing_desc <- if (identical(missing_eff, "fiml"))
                paste0(" ", tr(
                    "Missing values were handled with Full Information Maximum Likelihood (FIML), which uses all available information per case under the assumption that data are missing at random conditional on the model's variables, rather than discarding any case outright.",
                    "Los valores faltantes se manejaron con Máxima Verosimilitud de Información Completa (FIML), que usa toda la información disponible por caso bajo el supuesto de que los datos faltan al azar condicional a las variables del modelo, en vez de descartar cualquier caso por completo."))
                else ""
            n_desc <- if (n_available > n_adv)
                paste0(" ", tr(paste0(n_adv, " of ", n_available, " available cases were used."),
                               paste0("Se usaron ", n_adv, " de ", n_available, " casos disponibles.")))
                else ""
            res$fitNote$setContent(.fl_prose(
                "<p>", estim_desc, missing_desc, n_desc, "</p>",
                "<p>", tr(
                    "CFI and TLI (both 0-1) reward explaining more covariance than a null (no-correlation) model; RMSEA and SRMR (both 0-1, lower is better) penalize model complexity and average residual correlation, respectively. Hu &amp; Bentler (1999): CFI/TLI &ge; .95, RMSEA &le; .06, SRMR &le; .08 for Good; CFI/TLI &ge; .90, RMSEA &le; .08, SRMR &le; .10 for Acceptable. When the estimator is robust (MLR or WLSMV), the robust CFI/TLI/RMSEA columns — not the plain ones — are what these cutoffs and the Fit verdict use.",
                    "El CFI y el TLI (ambos 0-1) premian explicar más covarianza que un modelo nulo (sin correlación); el RMSEA y el SRMR (ambos 0-1, menor es mejor) penalizan la complejidad del modelo y la correlación residual promedio, respectivamente. Hu &amp; Bentler (1999): CFI/TLI &ge; .95, RMSEA &le; .06, SRMR &le; .08 para Bueno; CFI/TLI &ge; .90, RMSEA &le; .08, SRMR &le; .10 para Aceptable. Cuando el estimador es robusto (MLR o WLSMV), las columnas CFI/TLI/RMSEA robustas — no las simples — son las que usan estos umbrales y el veredicto de Ajuste."),
                "</p><p>", tr(
                    paste0("The correlated-factors model above is ", tolower(cf_verdict), "."),
                    paste0("El modelo de factores correlacionados de arriba es ", tolower(cf_verdict), ".")),
                if (!identical(cf_verdict, tr("Good", "Bueno"))) paste0(" ", tr(
                    "Every number downstream (CR, AVE, H, HTMT) inherits this model, so treat them with corresponding caution — see the Modification Indices below.",
                    "Todo número más abajo (CR, AVE, H, HTMT) hereda este modelo, así que trátelos con la cautela correspondiente — vea los Índices de Modificación abajo.")) else "",
                "</p>"))

            # ── Reliability note (always shown) ────────────────────────────
            worst_ave <- Filter(function(r) !is.na(r$ave) && r$ave < .50, rel_rows)
            low_relg <- list()
            if (isTRUE(opt$secondOrder) && ho_ok)
                low_relg <- Filter(function(r) !is.na(r$rel_g) && !is.na(r$cr) && r$cr > 0 && (r$rel_g / r$cr) < .40, rel_rows)

            rel_intro_p <- tr(
                "CR/&omega; (Composite Reliability, Fornell &amp; Larcker, 1981; under this model — one common factor per subscale, no correlated residuals — numerically the same as McDonald's, 1999, total &omega;) is the proportion of each factor's own composite-score variance attributable to its common factor, from this model's standardized loadings — unlike Cronbach's &alpha;, it does not assume equal (tau-equivalent) loadings. AVE (Fornell &amp; Larcker, 1981) is the average squared loading — convergent validity, not reliability; &ge; .50 is the field-standard minimum. H (Hancock &amp; Mueller, 2001) is an alternative construct-reliability index computed from the same loadings, less sensitive than CR/&omega; to a single weak indicator.",
                "El CR/&omega; (Confiabilidad Compuesta, Fornell &amp; Larcker, 1981; bajo este modelo — un factor común por subescala, sin residuos correlacionados — numéricamente igual al &omega; total de McDonald, 1999) es la proporción de la varianza propia del puntaje compuesto de cada factor atribuible a su factor común, a partir de las cargas estandarizadas de este modelo — a diferencia del Alfa de Cronbach, no asume cargas iguales (tau-equivalencia). El AVE (Fornell &amp; Larcker, 1981) es la carga al cuadrado promedio — validez convergente, no confiabilidad; &ge; .50 es el mínimo estándar del campo. El H (Hancock &amp; Mueller, 2001) es un índice de confiabilidad de constructo alternativo calculado a partir de las mismas cargas, menos sensible que el CR/&omega; a un único indicador débil.")

            ave_names <- paste(vapply(worst_ave, function(r) r$factor, character(1)), collapse = ", ")
            if (length(worst_ave) > 0L) {
                ave_p <- paste0("<p>&#9888; ", tr(
                    paste0(length(worst_ave), " of ", length(rel_rows), " factor(s) have AVE &lt; .50 (", ave_names, ") — their items explain less than half the variance in their own factor."),
                    paste0(length(worst_ave), " de ", length(rel_rows), " factor(es) tienen AVE &lt; .50 (", ave_names, ") — sus ítems explican menos de la mitad de la varianza de su propio factor.")), "</p>")
            } else {
                ave_p <- paste0("<p>&#10003; ", tr("All factors reach AVE &ge; .50.", "Todos los factores alcanzan AVE &ge; .50."), "</p>")
            }

            relg_p <- ""
            lrt_significant <- !is.null(lrt) && !is.na(lrt$p) && lrt$p < .05
            if (isTRUE(opt$secondOrder) && ho_ok) {
                relg_names <- paste(vapply(low_relg, function(r) r$factor, character(1)), collapse = ", ")
                if (lrt_significant) {
                    relg_txt <- tr(
                        "The second-order model fits significantly worse than free correlations (see the Model Comparison table above), so these reliability-due-to-G values overstate how much of each factor's reliable variance is really shared with a general trait -- treat them, like Omega Hierarchical, with real skepticism.",
                        "El modelo de segundo orden ajusta significativamente peor que el de correlaciones libres (vea la tabla de Comparación de Modelos arriba), por lo que estos valores de confiabilidad-debida-a-G sobreestiman cuánto de la varianza confiable de cada factor realmente se comparte con un rasgo general -- trátelos, al igual que el Omega Jerárquico, con escepticismo real.")
                    relg_p <- paste0("<p>&#9888; ", relg_txt, "</p>")
                } else if (length(low_relg) > 0L) {
                    relg_txt <- tr(
                        paste0(length(low_relg), " factor(s) (", relg_names, ") have most of their reliable variance specific to the subscale rather than the general factor G (reliability-due-to-G under 40% of their own CR/&omega;) — a total-score interpretation would discard most of what makes these subscales reliable."),
                        paste0(length(low_relg), " factor(es) (", relg_names, ") tienen la mayor parte de su varianza confiable específica a la subescala en vez del factor general G (confiabilidad-debida-a-G bajo 40% de su propio CR/&omega;) — una interpretación de puntaje total descartaría la mayor parte de lo que hace confiables a estas subescalas."))
                    relg_p <- paste0("<p>&#9888; ", relg_txt, "</p>")
                } else {
                    relg_txt <- tr("Reliability-due-to-G is a substantial share of each factor's own CR/&omega; -- the general-factor structure captures most of what each subscale reliably measures.",
                                   "La confiabilidad-debida-a-G es una parte sustancial del propio CR/&omega; de cada factor -- la estructura de factor general captura la mayor parte de lo que cada subescala mide de forma confiable.")
                    relg_p <- paste0("<p>&#10003; ", relg_txt, "</p>")
                }
            }

            neg_p <- ""
            if (!is.null(neg_loadings) && nrow(neg_loadings) > 0L) {
                neg_txt <- paste(paste0(neg_loadings$item, " (", round(neg_loadings$loading, 2), ")"), collapse = ", ")
                neg_p <- paste0("<p>&#9888; ", tr(
                    paste0(nrow(neg_loadings), " item(s) load negatively on their own factor: ", neg_txt, ". This usually means the item needs reverse-scoring (recoding it so higher raw values mean more of the construct) before re-running this analysis, not that it should be dropped."),
                    paste0(nrow(neg_loadings), " ítem(s) cargan negativamente sobre su propio factor: ", neg_txt, ". Esto usualmente significa que el ítem necesita recodificación inversa (para que valores brutos más altos signifiquen más del constructo) antes de volver a correr este análisis, no que deba eliminarse.")), "</p>")
            }

            loadings_p <- ""
            if (!is.null(low_loadings) && nrow(low_loadings) > 0L) {
                low_txt <- paste(paste0(low_loadings$item, " (", round(low_loadings$loading, 2), ")"), collapse = ", ")
                loadings_p <- paste0("<p>&#9888; ", tr(
                    paste0(nrow(low_loadings), " item(s) load below .50 (see the Standardized Loadings plot below): ", low_txt, "."),
                    paste0(nrow(low_loadings), " ítem(s) cargan por debajo de .50 (vea el gráfico de Cargas Estandarizadas abajo): ", low_txt, ".")), "</p>")
            }

            res$reliabilityNote$setContent(.fl_prose("<p>", rel_intro_p, "</p>", ave_p, relg_p, neg_p, loadings_p))

            # ── Plot captions (always shown when the plot is) ──────────────
            res$plotComparisonNote$setContent(.fl_prose("<p>", tr(
                "Each point is one factor's CR/&omega; on the 0-1 reliability scale — the same forest-plot convention used elsewhere in FiabilityLab, applied here to compare subscales at a glance instead of coefficients.",
                "Cada punto es el CR/&omega; de un factor en la escala de confiabilidad 0-1 — la misma convención de forest-plot usada en otras partes de FiabilityLab, aplicada aquí para comparar subescalas de un vistazo en vez de coeficientes."), "</p>"))

            res$plotLoadingsNote$setContent(.fl_prose("<p>", tr(
                "One bar per item, grouped by factor, showing its standardized loading; the dashed line marks .50, a common (not universal) minimum for an item to be considered a reasonably strong indicator of its factor.",
                "Una barra por ítem, agrupadas por factor, mostrando su carga estandarizada; la línea punteada marca .50, un mínimo común (no universal) para que un ítem se considere un indicador razonablemente fuerte de su factor."),
                if (!is.null(weak_loadings) && nrow(weak_loadings) > 0L) paste0(" ", tr("The item(s) below this line are listed in the note above the Reliability by Factor table.", "El/los ítem(s) bajo esta línea se listan en la nota arriba de la tabla Confiabilidad por Factor.")) else "",
                "</p>"))

            # ── HTMT note (only when >=2 factors) ──────────────────────────
            worst_htmt <- Filter(function(r) !is.na(r$htmt) && r$htmt > .85, htmt_rows)
            if (length(htmt_rows) > 0L) {
                res$htmtNote$setContent(.fl_prose(
                    "<p>", tr(
                        "HTMT (Henseler, Ringle &amp; Sarstedt, 2015) is the ratio of average between-factor correlations to average within-factor correlations — a discriminant-validity check for whether two subscales are empirically distinct rather than measuring the same thing twice. The classical Fornell-Larcker criterion and cross-loading inspection were shown to miss this in common research situations that HTMT reliably detects; &gt; .85 signals a concern.",
                        "El HTMT (Henseler, Ringle &amp; Sarstedt, 2015) es la razón entre las correlaciones promedio entre factores y las correlaciones promedio dentro de cada factor — una verificación de validez discriminante de si dos subescalas son empíricamente distintas en vez de medir dos veces lo mismo. Se demostró que el criterio clásico de Fornell-Larcker y la inspección de cargas cruzadas no detectan esto en situaciones de investigación comunes que el HTMT sí detecta de forma confiable; &gt; .85 señala una preocupación."),
                    "</p>",
                    if (length(worst_htmt) > 0L) paste0("<p>&#9888; ", tr(
                            paste0(length(worst_htmt), " pair(s) exceed .85: ", paste(vapply(worst_htmt, function(r) paste0(r$factor_a, "–", r$factor_b), character(1)), collapse = "; "), " — consider whether these are genuinely distinct constructs."),
                            paste0(length(worst_htmt), " par(es) exceden .85: ", paste(vapply(worst_htmt, function(r) paste0(r$factor_a, "–", r$factor_b), character(1)), collapse = "; "), " — considere si estos son constructos genuinamente distintos.")), "</p>")
                    else paste0("<p>&#10003; ", tr("All factor pairs are below .85 — no discriminant-validity concern detected.", "Todos los pares de factores están bajo .85 — no se detectó preocupación de validez discriminante."), "</p>")))
            }

            # ── Interpretation (top-level synthesis; detail lives in the
            # notes above, next to what each is about) ─────────────────────
            adv_html <- paste0(.fl_prose_open(),
                "<h4>", tr("What happened", "Qué pasó"), "</h4>",
                "<p>", tr(paste0("A confirmatory factor model with ", length(factors), " factor(s) (", fnames_txt,
                                 ") was fit on n = ", n_adv, " complete cases",
                                 if (n_available > n_adv) paste0(" (", n_available - n_adv, " of ", n_available, " available cases excluded listwise for missing values on at least one item)") else "",
                                 ". Model fit: ", cf_verdict,
                                 if (isTRUE(opt$secondOrder) && ho_ok) "; a second-order general factor was additionally fit and formally compared against it." else "."),
                          paste0("Se ajustó un modelo factorial confirmatorio con ", length(factors), " factor(es) (", fnames_txt,
                                 ") sobre n = ", n_adv, " casos completos",
                                 if (n_available > n_adv) paste0(" (se excluyeron ", n_available - n_adv, " de ", n_available, " casos disponibles por eliminación por lista, con valores faltantes en al menos un ítem)") else "",
                                 ". Ajuste del modelo: ", cf_verdict,
                                 if (isTRUE(opt$secondOrder) && ho_ok) "; adicionalmente se ajustó un factor general de segundo orden y se comparó formalmente contra él." else ".")), "</p>",
                "<h4>", tr("Why", "Por qué"), "</h4>",
                "<p>", tr(
                    "Every table and plot below has its own note explaining what it shows and how to read this run's specific numbers — this panel only ties them together.",
                    "Cada tabla y gráfico de abajo tiene su propia nota explicando qué muestra y cómo leer los números específicos de esta corrida — este panel solo los conecta entre sí."), "</p>",
                "<h4>", tr("What it means", "Qué implica"), "</h4>",
                "<p>", tr(
                    "See the Fiability Library → Advanced Reliability (SEM) section for full definitions, formulas, and assumptions of CR, AVE, H, HTMT, second-order omega, the model-comparison likelihood-ratio test, and modification indices, and Bibliography → Advanced Reliability (SEM) for the underlying citations.",
                    "Vea la sección Biblioteca de Confiabilidad → Confiabilidad Avanzada (SEM) para definiciones, fórmulas y supuestos completos de CR, AVE, H, HTMT, omega de segundo orden, la prueba de razón de verosimilitud de comparación de modelos, e índices de modificación, y Bibliografía → Confiabilidad Avanzada (SEM) para las citas correspondientes."), "</p>",
                "<h4>", tr("What to do now", "Qué hacer ahora"), "</h4>",
                "<ul style='line-height:1;'>",
                if (any_flagged)
                    paste0("<li>", tr("The Solution Admissibility table above flags at least one symptom of an inadmissible or borderline solution -- resolve this first; every other number in this report inherits whichever model is affected.",
                                      "La tabla de Admisibilidad de la Solución de arriba señala al menos un síntoma de una solución inadmisible o límite -- resuelva esto primero; cualquier otro número de este informe hereda el modelo que esté afectado."), "</li>") else "",
                if (!is.null(lrt) && !is.na(lrt$p) && lrt$p < .05)
                    paste0("<li>", tr("The second-order structure fits significantly worse than free correlations — report per-factor reliability (CR/ω/H above) rather than a single general-factor score, unless there is a strong theoretical reason to insist on one.",
                                      "La estructura de segundo orden ajusta significativamente peor que las correlaciones libres — reporte la confiabilidad por factor (CR/ω/H de arriba) en vez de un único puntaje de factor general, a menos que haya una razón teórica fuerte para insistir en uno."), "</li>") else "",
                if (length(mi_rows) > 0L)
                    paste0("<li>", tr(
                        "For each Modification Indices suggestion, ask first whether it makes theoretical sense before adding it — purely statistical respecification may not replicate (MacCallum, Roznowski &amp; Necowitz, 1992).",
                        "Para cada sugerencia de Índices de Modificación, pregunte primero si tiene sentido teórico antes de agregarla — la reespecificación puramente estadística puede no replicarse (MacCallum, Roznowski &amp; Necowitz, 1992)."), "</li>")
                else if (!identical(cf_verdict, tr("Good","Bueno")))
                    paste0("<li>", tr("Model fit is not fully adequate — reconsider whether the factor assignments match the instrument's theoretical structure before trusting CR/AVE/H from this model.",
                                      "El ajuste del modelo no es plenamente adecuado — reconsidere si las asignaciones de factores corresponden a la estructura teórica del instrumento antes de confiar en el CR/AVE/H de este modelo."), "</li>") else "",
                if (length(worst_ave) > 0L)
                    paste0("<li>", tr("Only after ruling out a theoretically defensible respecification: consider revising or replacing items in the low-AVE factor(s) above.",
                                      "Solo tras descartar una reespecificación teóricamente defendible: considere revisar o reemplazar ítems en el/los factor(es) con AVE bajo de arriba."), "</li>") else "",
                if (length(worst_htmt) > 0L)
                    paste0("<li>", tr("Consider whether the flagged factor pair(s) are conceptually distinct constructs at all before merging them or revising cross-loading items.",
                                      "Considere si el/los par(es) de factores marcados son en absoluto constructos conceptualmente distintos antes de fusionarlos o revisar ítems con cargas cruzadas."), "</li>") else "",
                if (length(low_relg) > 0L)
                    paste0("<li>", tr("For the flagged factor(s), report and interpret that subscale's own score rather than folding it into a total/general-factor score.",
                                      "Para el/los factor(es) marcados, reporte e interprete el puntaje propio de esa subescala en vez de incorporarlo a un puntaje total/de factor general."), "</li>") else "",
                if (!any_flagged && (is.null(lrt) || is.na(lrt$p) || lrt$p >= .05) && length(mi_rows) == 0L && identical(cf_verdict, tr("Good","Bueno")) &&
                    length(worst_ave) == 0L && length(worst_htmt) == 0L && length(low_relg) == 0L)
                    paste0("<li>", tr("No specific corrective action indicated from the advanced analysis.",
                                      "No se indica ninguna acción correctiva específica desde el análisis avanzado."), "</li>") else "",
                "</ul>",
                .fl_footnote(tr(
                    "CFA fit cutoffs follow Hu &amp; Bentler (1999); HTMT threshold follows Henseler, Ringle &amp; Sarstedt (2015); modification-index caution follows MacCallum, Roznowski &amp; Necowitz (1992); omega hierarchical follows McDonald (1999).",
                    "Los criterios de ajuste del AFC siguen a Hu &amp; Bentler (1999); el umbral de HTMT sigue a Henseler, Ringle &amp; Sarstedt (2015); la cautela sobre índices de modificación sigue a MacCallum, Roznowski &amp; Necowitz (1992); el omega jerárquico sigue a McDonald (1999).")),
                .fl_prose_close())
            res$interpretation$setContent(adv_html)
        },

        # ── Plot: reliability comparison (forest-plot style, one point per
        # factor) -- same construction as interRater's/Internal
        # Consistency's own .plotComparison. ──────────────────────────────
        .plotComparison = function(image, ggtheme, theme, ...) {
            d <- private$.plot_rows
            if (is.null(d) || nrow(d) == 0L || all(is.na(d$value))) return(FALSE)
            cols <- private$.plot_colors()
            d$factor <- factor(d$factor, levels = rev(d$factor))
            p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = factor)) +
                ggplot2::geom_point(size = 3.2, colour = cols$primary) +
                ggplot2::coord_cartesian(xlim = c(0, 1)) +
                ggplot2::labs(x = private$.tr("Composite Reliability / ω", "Confiabilidad Compuesta / ω"), y = NULL,
                              title = private$.tr("Reliability by Factor", "Confiabilidad por Factor")) +
                private$.plot_theme()
            print(p)
            TRUE
        },

        # ── Plot: standardized loadings, faceted by factor ────────────────
        .plotLoadings = function(image, ggtheme, theme, ...) {
            d <- private$.loadings_data
            if (is.null(d) || nrow(d) == 0L) return(FALSE)
            cols <- private$.plot_colors()
            d$item <- factor(d$item, levels = rev(unique(d$item)))
            p <- ggplot2::ggplot(d, ggplot2::aes(x = item, y = loading)) +
                ggplot2::geom_bar(stat = "identity", fill = cols$primary, alpha = .85, width = .6) +
                ggplot2::geom_hline(yintercept = .50, linetype = "dashed", colour = cols$secondary, linewidth = .5) +
                ggplot2::coord_flip(ylim = c(0, 1)) +
                ggplot2::facet_wrap(~factor, scales = "free_y") +
                ggplot2::labs(x = NULL, y = private$.tr("Standardized loading", "Carga estandarizada"),
                              title = private$.tr("Standardized Loadings by Factor (dashed = .50)",
                                                   "Cargas Estandarizadas por Factor (punteada = .50)")) +
                private$.plot_theme()
            print(p)
            TRUE
        },

        # ── Plot: fit comparison between correlated-factors and
        # second-order models (CFI/TLI, grouped bars) ─────────────────────
        .plotFitComparison = function(image, ggtheme, theme, ...) {
            d <- private$.fitcmp_data
            if (is.null(d) || nrow(d) == 0L) return(FALSE)
            cols <- private$.plot_colors()
            d$model <- factor(d$model, levels = unique(d$model))
            p <- ggplot2::ggplot(d, ggplot2::aes(x = index, y = value, fill = model)) +
                ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = .6), width = .55, alpha = .9) +
                ggplot2::geom_hline(yintercept = .95, linetype = "dashed", colour = "grey40", linewidth = .4) +
                ggplot2::scale_fill_manual(values = stats::setNames(c(cols$primary, cols$secondary), levels(d$model)), name = NULL) +
                ggplot2::coord_cartesian(ylim = c(0, 1)) +
                ggplot2::labs(x = NULL, y = private$.tr("Value (dashed = .95 good-fit reference)", "Valor (punteada = referencia de buen ajuste .95)"),
                              title = private$.tr("Model Fit Comparison", "Comparación de Ajuste entre Modelos")) +
                private$.plot_theme()
            print(p)
            TRUE
        }
    )
)
