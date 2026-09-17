# -----------------------------------------------------------------------------------
# FiabilityLab
# A Jamovi module for reliability and inter-rater agreement analysis and
# methodological decision support.
#
# Copyright (C) 2026 Arquímedes De León Chacón Chacón
#
# This file is part of FiabilityLab.
#
# FiabilityLab is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# FiabilityLab is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with FiabilityLab.
# If not, see https://www.gnu.org/licenses/.
# -----------------------------------------------------------------------------

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

# Workflow / Flujo de trabajo:
# Validate (lavaan/semTools available, at least one factor with >=3 items) ->
# prepare (collect factor definitions from the dynamic "Add New Factor" list,
# resolve item-type/estimator/missing-data strategy) -> describe missingness
# (n analyzed vs. n available) -> diagnose design (exploratory parallel
# analysis against the specified structure, solution admissibility beyond
# mere convergence) -> compute supported estimands (CFA fit, CR/omega, AVE,
# Hancock & Mueller's H, HTMT, second-order omega hierarchical) -> quantify
# uncertainty (formal model-comparison likelihood-ratio test) -> interpret
# (per-table/per-plot notes, modification indices as theory-guided
# diagnostics, never automatic edits) -> assemble report.
# ES: Validar (lavaan/semTools disponibles, al menos un factor con >=3 ítems)
# -> preparar (recolectar las definiciones de factor de la lista dinámica
# "Add New Factor", resolver la estrategia de tipo de ítem/estimador/datos
# faltantes) -> describir datos faltantes (n analizado vs. n disponible) ->
# diagnosticar diseño (análisis paralelo exploratorio contra la estructura
# especificada, admisibilidad de la solución más allá de la mera
# convergencia) -> calcular los estimandos soportados (ajuste AFC, CR/omega,
# AVE, H de Hancock & Mueller, HTMT, omega jerárquico de segundo orden) ->
# cuantificar incertidumbre (prueba formal de razón de verosimilitud de
# comparación de modelos) -> interpretar (notas por tabla/gráfico, índices de
# modificación como diagnósticos guiados por teoría, nunca ediciones
# automáticas) -> ensamblar el informe.
advancedReliabilityClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "advancedReliabilityClass",
    inherit = advancedReliabilityBase,
    private = list(

        .plot_rows     = NULL,   # data.frame: factor, value (CR/omega per factor)
        .loadings_data = NULL,   # data.frame: factor, item, loading
        .fitcmp_data   = NULL,   # data.frame: model, index, value (for CFI/TLI comparison plot)
        .parallel_data = NULL,   # data.frame: component, series (observed/simulated), value

        # Plot colours derived from jamovi's own theme -- see
        # .fl_plot_colors()' own definition in shared-helpers.R for why
        # (jamovi's official module review, 2026-09-16). .plot_theme() and
        # the plotStyle option it used to read are removed entirely; every
        # render function below uses the ggtheme jamovi already passes in.
        .plot_colors = function(theme) .fl_plot_colors(theme),
        .interp_rel = function(val) {
            if (is.na(val) || !is.finite(val)) return(.("N/A"))
            if (val >= .95) return(.("Excellent"))
            if (val >= .90) return(.("Good"))
            if (val >= .80) return(.("Acceptable"))
            if (val >= .70) return(.("Questionable"))
            if (val >= .60) return(.("Poor"))
            .("Unacceptable")
        },
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
            if (any(is.na(c(cfi, rmsea, srmr)))) return(.("N/A"))
            if (cfi >= .95 && rmsea <= .06 && srmr <= .08) .("Good")
            else if (cfi >= .90 && rmsea <= .08 && srmr <= .10) .("Acceptable")
            else .("Poor")
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
            admissible <- if (is.na(post_ok)) .("N/A") else if (post_ok) .("Yes") else .("No")
            list(converged = .("Yes"), admissible = admissible,
                 negVariances = as.integer(neg_var), loadingsOutOfBounds = as.integer(out_of_bounds),
                 maxLatentCorr = .fl_clean_na(max_corr), emptyCellPairs = .fl_clean_na(empty_cells),
                 nAnalyzed = as.integer(n_analyzed), nAvailable = as.integer(n_available),
                 flagged = isTRUE(!post_ok) || neg_var > 0L || out_of_bounds > 0L || isTRUE(empty_cells > 0L))
        },

        .run = function() {
            opt <- self$options
            res <- self$results
            esc <- private$.esc
            fit_verdict <- private$.fit_verdict

            # jamovi's official module review (2026-09-16) found this file
            # hiding every result and leaving one Html message on conditions
            # the user can fix -- jamovi already has a presentation for a
            # failed analysis (a stable, greyed pane with an error message);
            # that only works if the results stay in place instead of
            # collapsing/re-expanding as the user drags items into factors
            # one at a time. jmvcore::reject() throws so jamovi shows that
            # standard presentation instead of a custom hide-everything one.
            # ES: La revisión oficial de módulos de jamovi (2026-09-16)
            # encontró que este archivo ocultaba todo resultado y dejaba un
            # solo mensaje Html en condiciones que el usuario puede corregir
            # -- jamovi ya tiene una presentación para un análisis fallido
            # (un panel gris estable con un mensaje de error); eso solo
            # funciona si los resultados quedan en su lugar en vez de
            # colapsar/reexpandirse mientras el usuario arrastra ítems a los
            # factores uno por uno. jmvcore::reject() lanza una excepción
            # para que jamovi muestre esa presentación estándar en vez de
            # una propia que oculta todo.

            if (!requireNamespace("lavaan", quietly = TRUE) || !requireNamespace("semTools", quietly = TRUE))
                jmvcore::reject(.("This analysis requires the lavaan and semTools packages, which are not both installed here."))

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
                    if (is.null(fname) || !nzchar(trimws(fname))) fname <- paste(.("Factor"), i)
                    factors[[length(factors) + 1L]] <- list(id = paste0("F", i), name = fname, items = fitems)
                }
            }
            if (length(factors) == 0L)
                jmvcore::reject(.("Assign at least 3 items to at least one factor to run the confirmatory analysis."))

            all_items <- unique(unlist(lapply(factors, function(f) f$items)))
            df_raw <- self$data[, all_items, drop = FALSE]
            n_available <- nrow(df_raw)
            for (col in names(df_raw)) df_raw[[col]] <- suppressWarnings(as.numeric(df_raw[[col]]))

            # See the note at .fl_safe_names()' own definition in
            # shared-helpers.R for why -- item names go into lavaan model
            # syntax below, so everything downstream (df_raw's own column
            # names, each factor's items, all_items/ordered_items) switches
            # to the safe encoding here; safe_names$to_raw translates back
            # only at the specific points where an item name is shown to
            # the user (the loadings table/plot, modification indices).
            # ES: Ver la nota en la propia definición de .fl_safe_names()
            # en shared-helpers.R sobre por qué -- los nombres de ítem van a
            # sintaxis de modelo de lavaan más abajo, así que todo lo que
            # sigue (los propios nombres de columna de df_raw, los ítems de
            # cada factor, all_items/ordered_items) cambia a la
            # codificación segura aquí; safe_names$to_raw traduce de vuelta
            # solo en los puntos específicos donde se muestra un nombre de
            # ítem al usuario (la tabla/gráfico de cargas, los índices de
            # modificación).
            safe_names <- .fl_safe_names(all_items)
            names(df_raw) <- unname(safe_names$to_safe[names(df_raw)])
            factors <- lapply(factors, function(f) {
                f$items <- unname(safe_names$to_safe[f$items])
                f
            })
            all_items <- unname(safe_names$to_safe[all_items])

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
            if (n_adv < 20L || length(all_items) < 3L)
                jmvcore::reject(.("Not enough complete cases to fit a confirmatory factor model (minimum 20 required)."))

            # ── Exploratory dimensionality (parallel analysis, Horn 1965,
            # via psych::fa.parallel): independent of the confirmatory
            # structure the user assigned above, this asks how many
            # factors the items suggest on their own -- a real disagreement
            # with the number of factors specified is worth investigating
            # before trusting the confirmatory results, though it is not
            # by itself proof the specified structure is wrong (parallel
            # analysis is exploratory and sample-dependent too). Only run
            # when psych is available and the option is on; failures here
            # never block the confirmatory analysis below. ─────────────────
            if (isTRUE(opt$showParallelAnalysis) && requireNamespace("psych", quietly = TRUE)) {
                pa <- tryCatch({
                    cor_type <- if (item_is_ordinal) "poly" else "cor"
                    invisible(capture.output(out <- psych::fa.parallel(df_adv, fa = "fa", plot = FALSE, cor = cor_type)))
                    out
                }, error = function(e) NULL)
                if (!is.null(pa) && !is.null(pa$nfact) && is.finite(pa$nfact)) {
                    n_suggested <- as.integer(round(pa$nfact))
                    n_specified <- length(factors)
                    pa_verdict <- if (n_suggested == n_specified)
                        .("Matches the specified structure")
                    else if (n_suggested < n_specified)
                        .("Suggests fewer factors than specified")
                    else .("Suggests more factors than specified")
                    pat <- res$parallelAnalysisTable
                    private$.reset_table(pat, 1L)
                    pat$setRow(rowNo = 1, values = list(suggested = n_suggested, specified = n_specified, verdict = pa_verdict))
                    res$parallelAnalysisNote$setContent(.fl_prose(
                        "<p>", .("Parallel analysis (Horn, 1965) compares the eigenvalues from an exploratory factor analysis of these items against the eigenvalues expected from random data of the same size -- the suggested number of factors is how many real eigenvalues exceed their random counterpart (see the scree plot below). This is exploratory and does not know about the factor structure assigned above; it is a sanity check on that structure, not a replacement for it."),
                        "</p>",
                        if (n_suggested != n_specified) paste0("<p>⚠ ", .("The suggested and specified number of factors disagree. This does not automatically mean the specified structure is wrong -- theory-driven confirmatory structures legitimately group items in ways a purely data-driven exploratory method may not recover, especially with correlated factors or a small number of items per factor. But it is worth understanding why they disagree before treating the confirmatory results as the final word."), "</p>")
                            else paste0("<p>✓ ", .("The exploratory and confirmatory factor counts agree."), "</p>")))
                    n_show <- min(length(pa$fa.values), 15L)
                    private$.parallel_data <- data.frame(
                        component = rep(seq_len(n_show), 2),
                        series = c(rep(.("Observed data"), n_show), rep(.("Simulated random data"), n_show)),
                        value = c(pa$fa.values[seq_len(n_show)], pa$fa.sim[seq_len(n_show)]),
                        stringsAsFactors = FALSE)
                    # jamovi's official module review (2026-09-16) found every
                    # plot in this module rendered blank on export: the
                    # render function only ever runs live, straight after
                    # .run(), OR later on its own, in a separate re-created
                    # analysis instance that restores saved results but never
                    # calls .run() again -- so a private$ field read there is
                    # always NULL on that second path. setState() is the only
                    # channel that survives into that second instance.
                    # ES: La revisión oficial de módulos de jamovi
                    # (2026-09-16) encontró que todo gráfico de este módulo
                    # se exportaba en blanco: la función de render solo corre
                    # en vivo justo después de .run(), O más tarde por su
                    # cuenta, en una instancia de análisis separada que
                    # restaura los resultados guardados pero nunca vuelve a
                    # llamar a .run() -- así que un campo private$ leído ahí
                    # siempre es NULL en ese segundo camino. setState() es el
                    # único canal que sobrevive a esa segunda instancia.
                    res$plotParallelAnalysis$setState(private$.parallel_data)
                    res$plotParallelAnalysisNote$setContent(.fl_prose("<p>", .("Eigenvalues from the actual items (observed) against the average eigenvalues from simulated random data of the same size and number of variables; the suggested number of factors is where the observed line still sits above the simulated one."), "</p>"))
                } else {
                    res$parallelAnalysisTable$setVisible(FALSE)
                    res$parallelAnalysisNote$setVisible(FALSE)
                    res$plotParallelAnalysis$setVisible(FALSE)
                    res$plotParallelAnalysisNote$setVisible(FALSE)
                }
            } else {
                res$parallelAnalysisTable$setVisible(FALSE)
                res$parallelAnalysisNote$setVisible(FALSE)
                res$plotParallelAnalysis$setVisible(FALSE)
                res$plotParallelAnalysisNote$setVisible(FALSE)
            }

            model_cf <- paste(vapply(factors, function(f)
                paste0(f$id, " =~ ", paste(f$items, collapse = " + ")), character(1)), collapse = "\n")
            fit_cf <- private$.fit_cfa(model_cf, df_adv, ordered_items, estimator_eff, missing_eff)
            cf_ok  <- !is.null(fit_cf) && isTRUE(tryCatch(lavaan::lavInspect(fit_cf, "converged"), error = function(e) FALSE))
            if (cf_ok && identical(missing_eff, "fiml"))
                n_adv <- tryCatch(lavaan::lavInspect(fit_cf, "ntotal"), error = function(e) n_adv)
            if (!cf_ok)
                jmvcore::reject(.("The confirmatory factor model did not converge on this data/structure -- try fewer factors, more items per factor, or check the item assignments."))

            fnames_txt <- paste(vapply(factors, function(f) esc(f$name), character(1)), collapse = ", ")
            factor_ids <- vapply(factors, function(f) f$id, character(1))

            diag_rows <- list()
            diag_cf <- private$.solution_diag(fit_cf, all_items, factor_ids, n_adv, n_available, item_is_ordinal, df_adv)
            diag_rows[[length(diag_rows) + 1L]] <- c(list(model = .("Correlated factors")), diag_cf[names(diag_cf) != "flagged"])

            fm_cf <- private$.fit_measures_ext(fit_cf, estimator_eff)
            pick <- function(fm, robust_name, plain_name) if (!is.na(fm[robust_name])) unname(fm[robust_name]) else unname(fm[plain_name])
            cf_verdict <- fit_verdict(pick(fm_cf, "cfi.robust", "cfi"), pick(fm_cf, "rmsea.robust", "rmsea"), fm_cf["srmr"])
            fit_rows <- list(list(
                model = .("Correlated factors"),
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
                list(model = .("Correlated factors"), index = "CFI", value = unname(fm_cf["cfi"])),
                list(model = .("Correlated factors"), index = "TLI", value = unname(fm_cf["tli"])))

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
                        factor = f$name,
                        item = .fl_unsafe_name(lam_rows$rhs[r], safe_names$to_raw),
                        loading = lam_rows$est.std[r])
                lam_finite <- lam_rows$est.std[is.finite(lam_rows$est.std)]
                heywood_n <- sum(abs(lam_finite) >= 1)
                lam <- lam_finite[abs(lam_finite) < 1]
                h_terms <- (lam^2) / (1 - lam^2)
                h_val <- if (length(h_terms) > 0L) sum(h_terms) / (1 + sum(h_terms)) else NA_real_
                # semTools::compRelSEM() returns a plain named numeric vector for
                # a single-factor model, but a *list* of "lavaan.vector"-classed
                # scalars (each carrying a pretty-print "header" attribute) for a
                # multi-factor model (semTools 0.5.9) -- unname(cr_vals[f$id])
                # single-bracket-indexes that list into a length-1 list, which
                # is.finite() (called downstream in .interp_rel()) has no method
                # for. [[ + as.numeric() unwraps either shape to a bare scalar.
                cr_val  <- if (!is.null(cr_vals) && f$id %in% names(cr_vals)) as.numeric(cr_vals[[f$id]]) else NA_real_
                ave_val <- if (!is.null(ave_vals) && f$id %in% names(ave_vals)) as.numeric(ave_vals[[f$id]]) else NA_real_
                # jamovi's official module review (2026-09-16) found HTML
                # entities and esc()'s HTML-escaping applied to text bound
                # for a native Table's cells here -- jamovi already escapes
                # table/output text itself, so a factor named "A & B" would
                # render literally as "A &amp; B", and "&lt;" would show as
                # the four characters "&lt;" instead of "<". Both are
                # dropped below; esc() stays only where text goes into an
                # Html result (e.g. fnames_txt above, used in the
                # interpretation block).
                # ES: La revisión oficial de módulos de jamovi (2026-09-16)
                # encontró entidades HTML y el escapado HTML de esc()
                # aplicados a texto destinado a celdas de una Table nativa
                # aquí -- jamovi ya escapa el propio texto de tabla/salida,
                # así que un factor llamado "A & B" se mostraría
                # literalmente como "A &amp; B", y "&lt;" se vería como los
                # cuatro caracteres "&lt;" en vez de "<". Ambos se quitan
                # abajo; esc() se mantiene solo donde el texto va a un
                # resultado Html (p. ej. fnames_txt arriba, usado en el
                # bloque de interpretación).
                interp <- paste0(private$.interp_rel(cr_val),
                    if (!is.na(ave_val) && ave_val < .50) paste0("; ", .("AVE < .50")) else "",
                    if (heywood_n > 0L) paste0("; ", .("Heywood case")) else "")
                list(factor = f$name, items = length(f$items),
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
            } else {
                # State for .plotLoadings -- see the note at
                # .plotParallelAnalysis' own state assignment above for why.
                # ES: Estado para .plotLoadings -- ver la nota en la propia
                # asignación de estado de .plotParallelAnalysis arriba.
                res$plotLoadings$setState(private$.loadings_data)
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
            } else {
                # State for .plotComparison -- see the note at
                # .plotParallelAnalysis' own state assignment above for why.
                # ES: Estado para .plotComparison -- ver la nota en la propia
                # asignación de estado de .plotParallelAnalysis arriba.
                res$plotComparison$setState(private$.plot_rows)
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
                            factor_a = fa$name, factor_b = fb$name,
                            htmt = .fl_clean_na(val),
                            verdict = if (is.na(val)) .("N/A") else if (val > .85) .("Concern") else .("OK"))
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
                    diag_rows[[length(diag_rows) + 1L]] <- c(list(model = .("Second-order (general factor)")), diag_2nd[names(diag_2nd) != "flagged"])

                    fm_2nd <- private$.fit_measures_ext(fit_2nd, estimator_eff)
                    ho_verdict <- fit_verdict(pick(fm_2nd, "cfi.robust", "cfi"), pick(fm_2nd, "rmsea.robust", "rmsea"), fm_2nd["srmr"])
                    fit_rows[[length(fit_rows) + 1L]] <- list(
                        model = .("Second-order (general factor)"),
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
                    fitcmp_rows[[length(fitcmp_rows) + 1L]] <- list(model = .("Second-order"), index = "CFI", value = unname(fm_2nd["cfi"]))
                    fitcmp_rows[[length(fitcmp_rows) + 1L]] <- list(model = .("Second-order"), index = "TLI", value = unname(fm_2nd["tli"]))

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
                            # Same list-vs-scalar unwrap as cr_val above (line ~420):
                            # compRelSEM() returns a list for a multi-factor model, and
                            # this branch always has >=3 factors (main model's own
                            # factors plus G), so single-bracket indexing here left
                            # rel_g as a length-1 list -- is.na(list(x)) is FALSE, so
                            # .fl_clean_na() passed it through unchanged, and the
                            # rel_g / cr division below then failed with "non-numeric
                            # argument to binary operator". [[ + as.numeric() unwraps
                            # either shape to a bare scalar, same as cr_val/ave_val.
                            rel_rows[[i]]$rel_g <- .fl_clean_na(as.numeric(rel_g_vals[[factors[[i]]$id]]))
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
                "<p>", .("Convergence only means the optimizer stopped at some solution; it does not mean that solution is admissible. \"Admissible\" (from lavaan's own post-estimation check) means no negative residual variances, no non-positive-definite covariance matrix, and no other Heywood-case symptom -- a prerequisite for trusting CR, AVE, H, HTMT or omega hierarchical from that model. Negative residual variances and standardized loadings at or beyond 1 are two common, visible symptoms of an inadmissible solution; the largest latent correlation flags whether two factors may not be empirically distinct. For ordinal items, an item pair with an empty cell in its contingency table (some combination of categories that no one in the sample chose) makes that pair's underlying polychoric/tetrachoric correlation unstable or inestimable, which can silently degrade everything downstream even when the model still converges."),
                "</p>",
                if (any_flagged) paste0("<p>⚠ ", .("At least one model above shows a symptom of an inadmissible or borderline solution. Do not interpret CR, AVE, H, HTMT or omega hierarchical from that model until this is resolved -- common causes are too few cases for the number of parameters, near-perfectly correlated items, or a factor defined by too few (or too similar) indicators."), "</p>")
                    else paste0("<p>✓ ", .("No admissibility symptoms detected in the model(s) above."), "</p>")))

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
                res$modelComparisonNote$setContent(.fl_prose("<p>⚠ ", .("A second-order general factor needs at least 3 first-order factors to be statistically identified; only "),
                    length(factors), .(" were defined. Add another factor to enable this."), "</p>"))
                res$omegaHNote$setVisible(FALSE)
            } else if (!ho_ok) {
                res$modelComparisonTable$setVisible(FALSE)
                res$plotFitComparison$setVisible(FALSE)
                res$plotFitComparisonNote$setVisible(FALSE)
                res$modelComparisonNote$setContent(.fl_prose("<p>⚠ ", .("The second-order model did not converge -- the general-factor structure may not fit this data even though the correlated-factors model above does."), "</p>"))
                res$omegaHNote$setVisible(FALSE)
            } else {
                mct <- res$modelComparisonTable
                private$.reset_table(mct, 1L)
                mct$setRow(rowNo = 1, values = list(
                    comparison = .("Second-order vs. correlated factors"),
                    dchisq = .fl_clean_na(lrt$dchisq), ddf = lrt$ddf, pvalue = .fl_clean_na(lrt$p),
                    verdict = if (is.na(lrt$p)) .("Not testable (0 df)")
                              else if (lrt$p < .05) .("Second-order fits worse")
                              else .("Fits comparably")))

                res$modelComparisonNote$setContent(.fl_prose("<p>", if (is.na(lrt$p))
                        .("With exactly 3 first-order factors, the second-order model is mathematically equivalent to the correlated-factors model above (identical fit, 0 degrees of freedom difference, shown as Δdf = 0 in the table above) — a genuinely testable comparison needs 4 or more first-order factors.")
                    else if (lrt$p < .05)
                        paste0(.("Significant: imposing a single general factor over the "),
                               length(factors), .(" subscales fits significantly worse than letting them correlate freely (see the table above). Treat the general-factor structure — and the Omega Hierarchical value below — with real skepticism; report per-factor reliability instead of a single general-factor score unless there is a strong theoretical reason to insist on one."))
                    else
                        .("Not significant: the general factor accounts for the correlations among subscales about as well as letting them correlate freely (see the table above), supporting the second-order structure statistically — though this is not the same as it being the theoretically correct structure; that judgment still rests on the instrument's conceptual design."),
                    "</p>"))

                res$omegaHNote$setContent(.fl_prose("<p>", jmvcore::format(
                        .("Omega hierarchical for the total scale is <b>{val}</b> ({lbl}) — the proportion of total-score variance attributable specifically to the general factor G, net of each subscale's own specific variance (McDonald, 1999)."),
                        val = round(omega_h, 3), lbl = private$.interp_rel(omega_h)),
                    "</p>",
                    "<p>", .("This number is only as trustworthy as the model comparison above: if the likelihood-ratio test found the second-order structure fits significantly worse, treat this value as conditional on a structure the data do not support, not as independent evidence of a general dimension. Each factor's Reliability Table row below also lists its reliability specifically due to G — the share of that factor's own reliable variance that reflects the general trait rather than something specific to the subscale; a factor with high CR/ω but low reliability-due-to-G is measuring its own specific construct well, but a total score built by summing all items would discard most of that factor's reliable variance as if it were noise."),
                    "</p><p>", .("This omega hierarchical is derived from a second-order model, which forces the general factor's relationship to each subscale to run entirely through that subscale's overall factor (a restricted, nested special case). It is a different quantity from the omega hierarchical of an exploratory or confirmatory bifactor model, which lets each item load on the general factor directly as well as its own subscale; the two need not agree, and disagreement between them is itself informative about which structure fits the construct better."),
                    "</p>"))

                private$.fitcmp_data <- data.frame(
                    model = vapply(fitcmp_rows, function(r) r$model, character(1)),
                    index = vapply(fitcmp_rows, function(r) r$index, character(1)),
                    value = vapply(fitcmp_rows, function(r) r$value, numeric(1)),
                    stringsAsFactors = FALSE)
                # State for .plotFitComparison -- see the note at
                # .plotParallelAnalysis' own state assignment above for why.
                # ES: Estado para .plotFitComparison -- ver la nota en la
                # propia asignación de estado de .plotParallelAnalysis arriba.
                res$plotFitComparison$setState(private$.fitcmp_data)
                res$plotFitComparisonNote$setContent(.fl_prose("<p>", .("Bars compare CFI and TLI (both 0-1, higher is better) between the correlated-factors and second-order models; the dashed line marks the .95 conventional good-fit threshold (Hu &amp; Bentler, 1999). Visibly shorter bars for the second-order model are the same information as a significant likelihood-ratio test above, shown graphically."), "</p>"))
            }

            # ── Modification indices (only surfaced when fit isn't Good --
            # a diagnostic to interpret through the model's theoretical
            # meaning, never an automatic edit) ────────────────────────────
            mi_rows <- list()
            if (!identical(cf_verdict, .("Good"))) {
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
                        kind <- if (r$op == "=~") .("Possible cross-loading")
                                else if (r$op == "~~") .("Possible correlated residual")
                                else r$op
                        mi_rows[[length(mi_rows) + 1L]] <- list(
                            suggestion = paste0(
                                .fl_unsafe_name(r$lhs, safe_names$to_raw), " ", r$op, " ",
                                .fl_unsafe_name(r$rhs, safe_names$to_raw)),
                            type = kind, mi = .fl_clean_na(r$mi), epc = .fl_clean_na(r$epc))
                    }
                }
            }
            mit <- res$modificationIndicesTable
            private$.reset_table(mit, length(mi_rows))
            if (length(mi_rows) > 0L) {
                for (i in seq_along(mi_rows)) mit$setRow(rowNo = i, values = mi_rows[[i]])
                res$modificationIndicesNote$setContent(.fl_prose(
                    "<p>", .("Model fit is not Good, so the largest candidate respecifications are listed above -- as diagnostics, never as automatic edits. Purely data-driven respecification capitalizes on chance features of this specific sample and may not replicate in a new one (MacCallum, Roznowski &amp; Necowitz, 1992)."),
                    "</p><p>", .("For each row: a \"Possible cross-loading\" (=~) suggests an item may reflect more than one factor -- only add it if the item's content plausibly does so conceptually. A \"Possible correlated residual\" (~~) between two items suggests they share variance beyond their common factor -- often explainable by shared wording or format (e.g. two reverse-worded items, or two items sharing a stem), not a sign either item is bad. Removing an item is only one of several possible responses to poor fit, not the default one."),
                    "</p>"))
            } else {
                mit$setVisible(FALSE)
                res$modificationIndicesNote$setVisible(FALSE)
            }

            # ── Fit note (always shown) ────────────────────────────────────
            estim_desc <- if (item_is_ordinal)
                .("Items were treated as ordinal (WLSMV estimator on polychoric/tetrachoric correlations), which requires complete cases. χ², CFI, TLI and RMSEA below are the scaled/robust versions WLSMV recommends, not the plain ones.")
                else if (identical(estimator_eff, "MLR"))
                .("Items were treated as continuous, fit with MLR (robust to non-normality). χ², CFI, TLI and RMSEA below are the scaled/robust versions MLR recommends, not the plain ones.")
                else
                .("Items were treated as continuous, fit with ML.")
            # EN: Both the estimator and the missing-data options are
            # silently overridden when items are ordinal (WLSMV always
            # wins, and FIML has no ordinal/WLSMV equivalent) -- surfacing
            # that here so a user who explicitly picked ML/MLR or FIML
            # does not assume their choice was honored.
            # ES: Tanto el estimador como la opción de datos faltantes se
            # sobrescriben silenciosamente cuando los ítems son ordinales
            # (WLSMV siempre gana, y FIML no tiene equivalente ordinal/
            # WLSMV) -- se hace visible aquí para que un usuario que eligió
            # explícitamente ML/MLR o FIML no asuma que su elección se
            # respetó.
            override_desc <- if (item_is_ordinal && opt$estimator %in% c("ml", "mlr"))
                paste0(" ", jmvcore::format(
                    .("You selected {estimator}, but WLSMV was used instead because the items are ordinal."),
                    estimator = toupper(opt$estimator)))
                else if (item_is_ordinal && identical(opt$missingData, "fiml"))
                paste0(" ", .("You selected FIML, but listwise deletion was used instead because FIML has no equivalent for ordinal/WLSMV items."))
                else ""
            missing_desc <- if (identical(missing_eff, "fiml"))
                paste0(" ", .("Missing values were handled with Full Information Maximum Likelihood (FIML), which uses all available information per case under the assumption that data are missing at random conditional on the model's variables, rather than discarding any case outright."))
                else ""
            n_desc <- if (n_available > n_adv)
                paste0(" ", jmvcore::format(.("{n_adv} of {n_avail} available cases were used."), n_adv = n_adv, n_avail = n_available))
                else ""
            res$fitNote$setContent(.fl_prose(
                "<p>", estim_desc, override_desc, missing_desc, n_desc, "</p>",
                "<p>", .("CFI and TLI (both 0-1) reward explaining more covariance than a null (no-correlation) model; RMSEA and SRMR (both 0-1, lower is better) penalize model complexity and average residual correlation, respectively. Hu &amp; Bentler (1999): CFI/TLI ≥ .95, RMSEA ≤ .06, SRMR ≤ .08 for Good; CFI/TLI ≥ .90, RMSEA ≤ .08, SRMR ≤ .10 for Acceptable. These are conventional descriptive benchmarks from simulation studies under specific conditions, not universal pass/fail laws -- treat the Good/Acceptable/Poor verdict as a starting point for judgment, not a substitute for it, and weigh it alongside the loadings, residuals, and theoretical plausibility of the model. When the estimator is robust (MLR or WLSMV), the robust CFI/TLI/RMSEA columns — not the plain ones — are what these cutoffs and the Fit verdict use."),
                "</p><p>", jmvcore::format(
                    .("The correlated-factors model above is {verdict}."),
                    verdict = tolower(cf_verdict)),
                if (!identical(cf_verdict, .("Good"))) paste0(" ", .("Every number downstream (CR, AVE, H, HTMT) inherits this model, so treat them with corresponding caution — see the Modification Indices below.")) else "",
                "</p>"))

            # ── Reliability note (always shown) ────────────────────────────
            worst_ave <- Filter(function(r) !is.na(r$ave) && r$ave < .50, rel_rows)
            low_relg <- list()
            if (isTRUE(opt$secondOrder) && ho_ok)
                low_relg <- Filter(function(r) !is.na(r$rel_g) && !is.na(r$cr) && r$cr > 0 && (r$rel_g / r$cr) < .40, rel_rows)

            rel_intro_p <- .("CR/ω (Composite Reliability, Fornell &amp; Larcker, 1981; under this model — one common factor per subscale, no correlated residuals — numerically the same as McDonald's, 1999, total ω) is the proportion of each factor's own composite-score variance attributable to its common factor, from this model's standardized loadings — unlike Cronbach's α, it does not assume equal (tau-equivalent) loadings. AVE (Fornell &amp; Larcker, 1981) is the average squared loading — convergent validity, not reliability; ≥ .50 is the field-standard minimum. H (Hancock &amp; Mueller, 2001) is an alternative construct-reliability index computed from the same loadings, less sensitive than CR/ω to a single weak indicator.")

            ave_names <- paste(vapply(worst_ave, function(r) r$factor, character(1)), collapse = ", ")
            if (length(worst_ave) > 0L) {
                ave_p <- paste0("<p>⚠ ", jmvcore::format(
                    .("{worst} of {total} factor(s) have AVE &lt; .50 ({names}) — their items explain less than half the variance in their own factor."),
                    worst = length(worst_ave), total = length(rel_rows), names = ave_names), "</p>")
            } else {
                ave_p <- paste0("<p>✓ ", .("All factors reach AVE ≥ .50."), "</p>")
            }

            relg_p <- ""
            lrt_significant <- !is.null(lrt) && !is.na(lrt$p) && lrt$p < .05
            if (isTRUE(opt$secondOrder) && ho_ok) {
                relg_names <- paste(vapply(low_relg, function(r) r$factor, character(1)), collapse = ", ")
                if (lrt_significant) {
                    relg_txt <- .("The second-order model fits significantly worse than free correlations (see the Model Comparison table above), so these reliability-due-to-G values overstate how much of each factor's reliable variance is really shared with a general trait -- treat them, like Omega Hierarchical, with real skepticism.")
                    relg_p <- paste0("<p>⚠ ", relg_txt, "</p>")
                } else if (length(low_relg) > 0L) {
                    relg_txt <- jmvcore::format(
                        .("{n} factor(s) ({names}) have most of their reliable variance specific to the subscale rather than the general factor G (reliability-due-to-G under 40% of their own CR/ω) — a total-score interpretation would discard most of what makes these subscales reliable."),
                        n = length(low_relg), names = relg_names)
                    relg_p <- paste0("<p>⚠ ", relg_txt, "</p>")
                } else {
                    relg_txt <- .("Reliability-due-to-G is a substantial share of each factor's own CR/ω -- the general-factor structure captures most of what each subscale reliably measures.")
                    relg_p <- paste0("<p>✓ ", relg_txt, "</p>")
                }
            }

            neg_p <- ""
            if (!is.null(neg_loadings) && nrow(neg_loadings) > 0L) {
                neg_txt <- paste(paste0(neg_loadings$item, " (", round(neg_loadings$loading, 2), ")"), collapse = ", ")
                neg_p <- paste0("<p>⚠ ", jmvcore::format(
                    .("{n} item(s) load negatively on their own factor: {items}. This usually means the item needs reverse-scoring (recoding it so higher raw values mean more of the construct) before re-running this analysis, not that it should be dropped."),
                    n = nrow(neg_loadings), items = neg_txt), "</p>")
            }

            loadings_p <- ""
            if (!is.null(low_loadings) && nrow(low_loadings) > 0L) {
                low_txt <- paste(paste0(low_loadings$item, " (", round(low_loadings$loading, 2), ")"), collapse = ", ")
                loadings_p <- paste0("<p>⚠ ", jmvcore::format(
                    .("{n} item(s) load below .50 (see the Standardized Loadings plot below): {items}."),
                    n = nrow(low_loadings), items = low_txt), "</p>")
            }

            res$reliabilityNote$setContent(.fl_prose("<p>", rel_intro_p, "</p>", ave_p, relg_p, neg_p, loadings_p))

            # ── Plot captions (always shown when the plot is) ──────────────
            res$plotComparisonNote$setContent(.fl_prose("<p>", .("Each point is one factor's CR/ω on the 0-1 reliability scale — the same forest-plot convention used elsewhere in FiabilityLab, applied here to compare subscales at a glance instead of coefficients."), "</p>"))

            res$plotLoadingsNote$setContent(.fl_prose("<p>", .("One bar per item, grouped by factor, showing its standardized loading; the dashed line marks .50, a common (not universal) minimum for an item to be considered a reasonably strong indicator of its factor."),
                if (!is.null(weak_loadings) && nrow(weak_loadings) > 0L) paste0(" ", .("The item(s) below this line are listed in the note above the Reliability by Factor table.")) else "",
                "</p>"))

            # ── HTMT note (only when >=2 factors) ──────────────────────────
            worst_htmt <- Filter(function(r) !is.na(r$htmt) && r$htmt > .85, htmt_rows)
            if (length(htmt_rows) > 0L) {
                res$htmtNote$setContent(.fl_prose(
                    "<p>", .("HTMT (Henseler, Ringle &amp; Sarstedt, 2015) is the ratio of average between-factor correlations to average within-factor correlations — a discriminant-validity check for whether two subscales are empirically distinct rather than measuring the same thing twice. The classical Fornell-Larcker criterion and cross-loading inspection were shown to miss this in common research situations that HTMT reliably detects; &gt; .85 signals a concern."),
                    "</p>",
                    if (length(worst_htmt) > 0L) paste0("<p>⚠ ", jmvcore::format(
                            .("{n} pair(s) exceed .85: {pairs} — consider whether these are genuinely distinct constructs."),
                            n = length(worst_htmt),
                            pairs = paste(vapply(worst_htmt, function(r) paste0(r$factor_a, "–", r$factor_b), character(1)), collapse = "; ")), "</p>")
                    else paste0("<p>✓ ", .("All factor pairs are below .85 — no discriminant-validity concern detected."), "</p>")))
            }

            # ── Interpretation (top-level synthesis; detail lives in the
            # notes above, next to what each is about) ─────────────────────
            adv_html <- paste0(.fl_prose_open(),
                "<h4>", .("What happened"), "</h4>",
                "<p>", jmvcore::format(
                    .("A confirmatory factor model with {nf} factor(s) ({fnames}) was fit on n = {n} complete cases{excluded}. Model fit: {verdict}{secondorder}"),
                    nf = length(factors), fnames = fnames_txt, n = n_adv,
                    excluded = if (n_available > n_adv)
                        jmvcore::format(.(" ({excl} of {avail} available cases excluded listwise for missing values on at least one item)"),
                                        excl = n_available - n_adv, avail = n_available)
                        else "",
                    verdict = cf_verdict,
                    secondorder = if (isTRUE(opt$secondOrder) && ho_ok)
                        .("; a second-order general factor was additionally fit and formally compared against it.")
                        else "."
                ), "</p>",
                "<h4>", .("Why"), "</h4>",
                "<p>", .("Every table and plot below has its own note explaining what it shows and how to read this run's specific numbers — this panel only ties them together."), "</p>",
                "<h4>", .("What it means"), "</h4>",
                "<p>", .("See the Fiability Library → Advanced Reliability (SEM) section for full definitions, formulas, and assumptions of CR, AVE, H, HTMT, second-order omega, the model-comparison likelihood-ratio test, and modification indices, and Bibliography → Advanced Reliability (SEM) for the underlying citations."), "</p>",
                "<h4>", .("What to do now"), "</h4>",
                "<ul style='line-height:1;'>",
                if (any_flagged)
                    paste0("<li>", .("The Solution Admissibility table above flags at least one symptom of an inadmissible or borderline solution -- resolve this first; every other number in this report inherits whichever model is affected."), "</li>") else "",
                if (!is.null(lrt) && !is.na(lrt$p) && lrt$p < .05)
                    paste0("<li>", .("The second-order structure fits significantly worse than free correlations — report per-factor reliability (CR/ω/H above) rather than a single general-factor score, unless there is a strong theoretical reason to insist on one."), "</li>") else "",
                if (length(mi_rows) > 0L)
                    paste0("<li>", .("For each Modification Indices suggestion, ask first whether it makes theoretical sense before adding it — purely statistical respecification may not replicate (MacCallum, Roznowski &amp; Necowitz, 1992)."), "</li>")
                else if (!identical(cf_verdict, .("Good")))
                    paste0("<li>", .("Model fit is not fully adequate — reconsider whether the factor assignments match the instrument's theoretical structure before trusting CR/AVE/H from this model."), "</li>") else "",
                if (length(worst_ave) > 0L)
                    paste0("<li>", .("Only after ruling out a theoretically defensible respecification: consider revising or replacing items in the low-AVE factor(s) above."), "</li>") else "",
                if (length(worst_htmt) > 0L)
                    paste0("<li>", .("Consider whether the flagged factor pair(s) are conceptually distinct constructs at all before merging them or revising cross-loading items."), "</li>") else "",
                if (length(low_relg) > 0L)
                    paste0("<li>", .("For the flagged factor(s), report and interpret that subscale's own score rather than folding it into a total/general-factor score."), "</li>") else "",
                if (!any_flagged && (is.null(lrt) || is.na(lrt$p) || lrt$p >= .05) && length(mi_rows) == 0L && identical(cf_verdict, .("Good")) &&
                    length(worst_ave) == 0L && length(worst_htmt) == 0L && length(low_relg) == 0L)
                    paste0("<li>", .("No specific corrective action indicated from the advanced analysis."), "</li>") else "",
                "</ul>",
                .fl_footnote(.("CFA fit cutoffs follow Hu &amp; Bentler (1999); HTMT threshold follows Henseler, Ringle &amp; Sarstedt (2015); modification-index caution follows MacCallum, Roznowski &amp; Necowitz (1992); omega hierarchical follows McDonald (1999).")),
                .fl_prose_close())
            res$interpretation$setContent(adv_html)
        },

        # ── Plot: parallel-analysis scree plot (observed vs. simulated
        # eigenvalues) ─────────────────────────────────────────────────────
        # All four render functions below read image$state, not a private$
        # field -- see the note at each state assignment in .run() above for
        # why (jamovi's official module review, 2026-09-16).
        .plotParallelAnalysis = function(image, ggtheme, theme, ...) {
            d <- image$state
            if (is.null(d) || nrow(d) == 0L) return(FALSE)
            cols <- private$.plot_colors(theme)
            d$series <- factor(d$series, levels = unique(d$series))
            # Fixed Observed/Simulated colour distinction, not a
            # user-configurable one -- see internalconsistency.b.R's
            # .plotItemTotal for why this always-on manual scale goes
            # AFTER `+ ggtheme`.
            # ES: Distinción de color fija Observado/Simulado, no una
            # configurable por el usuario -- ver .plotItemTotal de
            # internalconsistency.b.R sobre por qué esta escala manual
            # siempre activa va DESPUÉS de `+ ggtheme`.
            p <- ggplot2::ggplot(d, ggplot2::aes(x = component, y = value, colour = series, shape = series)) +
                ggplot2::geom_line(linewidth = .6) +
                ggplot2::geom_point(size = 2.2) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50", linewidth = .4) +
                ggplot2::scale_x_continuous(breaks = unique(d$component)) +
                ggplot2::labs(x = .("Factor"), y = .("Eigenvalue"),
                              title = .("Parallel Analysis Scree Plot")) +
                ggtheme +
                ggplot2::scale_colour_manual(values = stats::setNames(c(cols$primary, cols$secondary), levels(d$series)), name = NULL) +
                ggplot2::scale_shape_manual(values = c(16, 17), name = NULL)
            print(p)
            TRUE
        },

        # ── Plot: reliability comparison (forest-plot style, one point per
        # factor) -- same construction as interRater's/Internal
        # Consistency's own .plotComparison. ──────────────────────────────
        .plotComparison = function(image, ggtheme, theme, ...) {
            d <- image$state
            if (is.null(d) || nrow(d) == 0L || all(is.na(d$value))) return(FALSE)
            cols <- private$.plot_colors(theme)
            d$factor <- factor(d$factor, levels = rev(d$factor))
            p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = factor)) +
                ggplot2::geom_point(size = 3.2, colour = cols$primary) +
                ggplot2::coord_cartesian(xlim = c(0, 1)) +
                ggplot2::labs(x = .("Composite Reliability / ω"), y = NULL,
                              title = .("Reliability by Factor")) +
                ggtheme
            print(p)
            TRUE
        },

        # ── Plot: standardized loadings, faceted by factor ────────────────
        .plotLoadings = function(image, ggtheme, theme, ...) {
            d <- image$state
            if (is.null(d) || nrow(d) == 0L) return(FALSE)
            cols <- private$.plot_colors(theme)
            d$item <- factor(d$item, levels = rev(unique(d$item)))
            # jamovi's official module review (2026-09-16) found this plot
            # clipped its axis at 0, hiding the negative loadings the
            # Reliability note above specifically warns about (a sign an
            # item needs reverse-scoring) -- they showed up as empty rows
            # instead. Extending the lower bound to include any genuinely
            # negative loading (with a small margin) keeps them visible.
            # ES: La revisión oficial de módulos de jamovi (2026-09-16)
            # encontró que este gráfico recortaba su eje en 0, ocultando
            # las cargas negativas que la nota de Confiabilidad de arriba
            # advierte específicamente (una señal de que un ítem necesita
            # recodificación inversa) -- aparecían como filas vacías en su
            # lugar. Extender el límite inferior para incluir cualquier
            # carga genuinamente negativa (con un margen pequeño) las
            # mantiene visibles.
            ylo <- min(-0.2, min(d$loading, na.rm = TRUE))
            p <- ggplot2::ggplot(d, ggplot2::aes(x = item, y = loading)) +
                ggplot2::geom_bar(stat = "identity", fill = cols$primary, alpha = .85, width = .6) +
                ggplot2::geom_hline(yintercept = .50, linetype = "dashed", colour = cols$secondary, linewidth = .5) +
                ggplot2::coord_flip(ylim = c(ylo, 1)) +
                ggplot2::facet_wrap(~factor, scales = "free_y") +
                ggplot2::labs(x = NULL, y = .("Standardized loading"),
                              title = .("Standardized Loadings by Factor (dashed = .50)")) +
                ggtheme
            print(p)
            TRUE
        },

        # ── Plot: fit comparison between correlated-factors and
        # second-order models (CFI/TLI, grouped bars) ─────────────────────
        .plotFitComparison = function(image, ggtheme, theme, ...) {
            d <- image$state
            if (is.null(d) || nrow(d) == 0L) return(FALSE)
            cols <- private$.plot_colors(theme)
            d$model <- factor(d$model, levels = unique(d$model))
            # Fixed two-model colour distinction, not a user-configurable
            # one -- see internalconsistency.b.R's .plotItemTotal for why
            # this always-on manual scale goes AFTER `+ ggtheme`.
            # ES: Distinción de color fija entre dos modelos, no una
            # configurable por el usuario -- ver .plotItemTotal de
            # internalconsistency.b.R sobre por qué esta escala manual
            # siempre activa va DESPUÉS de `+ ggtheme`.
            p <- ggplot2::ggplot(d, ggplot2::aes(x = index, y = value, fill = model)) +
                ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = .6), width = .55, alpha = .9) +
                ggplot2::geom_hline(yintercept = .95, linetype = "dashed", colour = "grey40", linewidth = .4) +
                ggplot2::coord_cartesian(ylim = c(0, 1)) +
                ggplot2::labs(x = NULL, y = .("Value (dashed = .95 good-fit reference)"),
                              title = .("Model Fit Comparison")) +
                ggtheme +
                ggplot2::scale_fill_manual(values = stats::setNames(c(cols$primary, cols$secondary), levels(d$model)), name = NULL)
            print(p)
            TRUE
        }
    )
)
