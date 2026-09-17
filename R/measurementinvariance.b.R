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
# FiabilityLab - Measurement Invariance.
#
# Before comparing group means or subscale scores on a scale (by sex,
# country, time point, or any other grouping variable), the same
# measurement model has to hold across those groups -- otherwise an
# observed difference may reflect how the items function differently per
# group rather than a real difference on the construct (a "same score,
# different meaning" problem no amount of within-group reliability can
# catch). This analysis fits the standard nested sequence of increasingly
# restrictive multi-group CFA models -- configural (same structure),
# metric/weak (equal loadings), scalar/strong (equal loadings and
# intercepts/thresholds), strict (also equal residual variances) -- and
# tests each one against the model just before it via a formal
# likelihood-ratio test, alongside the sample-size-robust ΔCFI criterion
# (Cheung & Rensvold, 2002), since the LRT alone gets hypersensitive to
# trivial misfit in large samples. A companion, not a replacement, for
# Advanced Reliability (SEM): that analysis assumes one population: this
# one checks whether that assumption is safe to make across groups before
# any cross-group comparison is drawn from it.
#
# ES: Antes de comparar medias de grupo o puntajes de subescala en una
# escala (por sexo, país, momento temporal, o cualquier otra variable de
# agrupación), el mismo modelo de medición tiene que sostenerse a través
# de esos grupos -- de lo contrario una diferencia observada puede
# reflejar que los ítems funcionan distinto por grupo en vez de una
# diferencia real en el constructo (un problema de "mismo puntaje,
# distinto significado" que ninguna cantidad de confiabilidad dentro de
# grupo puede detectar). Este análisis ajusta la secuencia estándar de
# modelos AFC multigrupo cada vez más restrictivos -- configural (misma
# estructura), métrica/débil (cargas iguales), escalar/fuerte (cargas e
# interceptos/umbrales iguales), estricta (también varianzas residuales
# iguales) -- y prueba cada uno contra el modelo justo anterior mediante
# una prueba formal de razón de verosimilitud, junto con el criterio
# ΔCFI robusto al tamaño muestral (Cheung & Rensvold, 2002), ya que el
# LRT por sí solo se vuelve hipersensible a desajustes triviales en
# muestras grandes. Un complemento, no un reemplazo, de Confiabilidad
# Avanzada (SEM): ese análisis asume una sola población; este verifica
# si ese supuesto es seguro de hacer a través de grupos antes de sacar
# cualquier comparación entre grupos de él.
# -----------------------------------------------------------------------------

# Workflow / Flujo de trabajo:
# Validate (lavaan available, a grouping variable, at least one factor with
# >=3 items, >=2 group levels, enough cases per group) -> prepare (collect
# factor definitions, resolve item-type/estimator strategy) -> describe
# missingness (n analyzed) -> diagnose design (group sizes, convergence at
# each level of the sequence) -> compute supported estimands (the nested
# configural/metric/scalar/strict multi-group CFA sequence) -> quantify
# uncertainty (likelihood-ratio test and ΔCFI at each level, against the
# level just before it) -> interpret (a five-way verdict per level --
# supported by both/one/neither criterion, or undetermined -- plus which
# comparisons the highest fully-supported level actually licenses) ->
# assemble report.
# ES: Validar (lavaan disponible, una variable de agrupación, al menos un
# factor con >=3 ítems, >=2 niveles de grupo, suficientes casos por grupo)
# -> preparar (recolectar las definiciones de factor, resolver la estrategia
# de tipo de ítem/estimador) -> describir datos faltantes (n analizado) ->
# diagnosticar diseño (tamaños de grupo, convergencia en cada nivel de la
# secuencia) -> calcular los estimandos soportados (la secuencia anidada de
# AFC multigrupo configural/métrica/escalar/estricta) -> cuantificar
# incertidumbre (prueba de razón de verosimilitud y ΔCFI en cada nivel,
# contra el nivel justo anterior) -> interpretar (un veredicto de cinco
# categorías por nivel -- respaldado por ambos/uno/ningún criterio, o
# indeterminado -- más qué comparaciones habilita realmente el nivel máximo
# plenamente respaldado) -> ensamblar el informe.
measurementInvarianceClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "measurementInvarianceClass",
    inherit = measurementInvarianceBase,
    private = list(

        .fitcmp_data = NULL,   # data.frame: level, index, value (for the fit-across-levels plot)

        # Plot colours derived from jamovi's own theme -- see
        # .fl_plot_colors()' own definition in shared-helpers.R for why
        # (jamovi's official module review, 2026-09-16). .plot_theme() and
        # the plotStyle option it used to read are removed entirely; the
        # render function below uses the ggtheme jamovi already passes in.
        .plot_colors = function(theme) .fl_plot_colors(theme),
        .reset_table = function(table, n_rows) .fl_reset_table(table, n_rows),

        .esc = function(x) {
            x <- gsub("&", "&amp;", x, fixed = TRUE)
            x <- gsub("<", "&lt;", x, fixed = TRUE)
            gsub(">", "&gt;", x, fixed = TRUE)
        },

        # ── Classify one invariance-sequence step from its LRT p-value and
        # its ΔCFI, both against the model just before it. EN: previously
        # this was a plain OR ("either criterion passing counts as
        # holding"), which silently treated LRT-non-significant-but-large-
        # ΔCFI-drop the same as LRT-significant-but-small-ΔCFI-drop --
        # collapsing two criteria with different sensitivities (the LRT is
        # hypersensitive in large samples, which is WHY Cheung & Rensvold's
        # (2002) ΔCFI is used as a complement; but a real drop the ΔCFI
        # flags should not be waved away just because the LRT (which can be
        # underpowered in a small sample) happened not to reach
        # significance). This now reports which criterion(a) actually
        # support the restriction, so a genuine disagreement between them
        # is visible rather than silently resolved in the restriction's
        # favor.
        # ES: antes esto era un simple OR ("basta con que un criterio se
        # cumpla"), que trataba por igual el caso LRT-no-significativo-
        # pero-con-caída-grande-de-ΔCFI y el caso LRT-significativo-pero-
        # con-caída-pequeña-de-ΔCFI -- colapsando dos criterios con
        # sensibilidades distintas (el LRT es hipersensible en muestras
        # grandes, que es justamente POR QUÉ se usa el ΔCFI de Cheung &
        # Rensvold (2002) como complemento; pero una caída real que el
        # ΔCFI señala no debería descartarse solo porque el LRT -- que
        # puede tener poca potencia en una muestra pequeña -- no haya
        # resultado significativo). Ahora esto reporta qué criterio(s)
        # realmente respaldan la restricción, de modo que un desacuerdo
        # genuino entre ellos sea visible en vez de resolverse
        # silenciosamente a favor de la restricción.
        .invariance_verdict = function(p_val, dcfi) {
            lrt_ok <- !is.na(p_val); cfi_ok <- !is.na(dcfi)
            lrt_holds <- lrt_ok && p_val >= .05
            cfi_holds <- cfi_ok && dcfi >= -.01
            if (!lrt_ok && !cfi_ok) return(.("Unable to determine"))
            if (lrt_holds && cfi_holds) return(.("Supported by both criteria"))
            if (lrt_holds) return(.("Supported by LRT only"))
            if (cfi_holds) return(.("Supported by ΔCFI only"))
            .("Not supported")
        },

        # ── Fit measures that prefer the scaled/robust chi-square/df/p and
        # robust CFI/RMSEA under a robust estimator (MLR or WLSMV) --
        # matching advancedreliability.b.R's own .fit_measures_ext(). WLSMV's
        # *plain* chi-square/CFI/RMSEA are not the recommended numbers and
        # can even fall outside [0,1]; a ΔCFI computed from the plain CFI
        # would inherit that unreliability, which previously went
        # unaddressed here even though Advanced Reliability already solved
        # it. ──────────────────────────────────────────────────────────────
        .fit_measures_ext = function(fit, estimator_eff) {
            is_robust <- estimator_eff %in% c("MLR", "WLSMV")
            chisq_names <- if (is_robust) c("chisq.scaled", "df.scaled", "pvalue.scaled") else c("chisq", "df", "pvalue")
            fit_names <- c("cfi", "rmsea")
            robust_names <- if (is_robust) c("cfi.robust", "rmsea.robust") else character(0)
            all_names <- c(chisq_names, fit_names, robust_names)
            fm <- tryCatch(lavaan::fitMeasures(fit, all_names), error = function(e) NULL)
            out <- stats::setNames(rep(NA_real_, length(all_names)), all_names)
            if (!is.null(fm)) out[names(fm)] <- fm
            if (is_robust) {
                names(out)[names(out) == "chisq.scaled"] <- "chisq"
                names(out)[names(out) == "df.scaled"] <- "df"
                names(out)[names(out) == "pvalue.scaled"] <- "pvalue"
                if (!is.na(out["cfi.robust"])) out["cfi"] <- out["cfi.robust"]
                if (!is.na(out["rmsea.robust"])) out["rmsea"] <- out["rmsea.robust"]
            }
            out
        },

        .fit_cfa_group = function(model_syntax, data, group_var, ordered_items, estimator, group_equal) {
            fit_args <- list(model = model_syntax, data = data, group = group_var, std.lv = TRUE, group.equal = group_equal)
            if (!is.null(ordered_items)) {
                fit_args$ordered <- ordered_items
                fit_args$estimator <- "WLSMV"
                # lavaan's default "delta" parameterization for categorical
                # indicators fixes residual variances for scale
                # identification, so group.equal="residuals" (the Strict
                # level) is a silent no-op under it -- the fit comes out
                # byte-for-byte identical to the Scalar model (confirmed:
                # df=625 for both under delta vs. the correct df=650 under
                # theta, on FiabilityLab's own bfi/gender invariance
                # example). "theta" parameterization frees the residual
                # variances so the equality constraint actually binds.
                # Configural/Metric/Scalar are left on delta (the standard
                # choice, and what they were already tested against) --
                # only Strict, the one level that constrains residuals,
                # needs theta.
                if ("residuals" %in% group_equal) fit_args$parameterization <- "theta"
            } else {
                fit_args$estimator <- estimator
            }
            tryCatch(do.call(lavaan::cfa, fit_args), error = function(e) NULL)
        },

        .run = function() {
            opt <- self$options
            res <- self$results
            esc <- private$.esc

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

            if (!requireNamespace("lavaan", quietly = TRUE))
                jmvcore::reject(.("This analysis requires the lavaan package, which is not installed here."))

            group_name <- opt$group
            if (is.null(group_name) || !nzchar(group_name))
                jmvcore::reject(.("Assign a grouping variable to test invariance across."))

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
                jmvcore::reject(.("Assign at least 3 items to at least one factor to run the invariance sequence."))

            all_items <- unique(unlist(lapply(factors, function(f) f$items)))
            df_raw <- self$data[, c(all_items, group_name), drop = FALSE]
            for (col in all_items) df_raw[[col]] <- suppressWarnings(as.numeric(df_raw[[col]]))
            df_raw[[group_name]] <- factor(df_raw[[group_name]])

            # See the note at .fl_safe_names()' own definition in
            # shared-helpers.R for why -- item names go into lavaan model
            # syntax below, so the item columns (not group_name, which
            # lavaan takes as a plain data-frame column reference, never
            # parsed as syntax text) switch to the safe encoding here.
            # This file never shows an item name back to the user (only
            # group_name/its levels, via esc() elsewhere), so no reverse
            # mapping is needed, unlike advancedreliability.b.R's own use
            # of the same helper.
            # ES: Ver la nota en la propia definición de .fl_safe_names()
            # en shared-helpers.R sobre por qué -- los nombres de ítem van
            # a sintaxis de modelo de lavaan más abajo, así que las
            # columnas de ítems (no group_name, que lavaan toma como una
            # referencia de columna de data frame normal, nunca analizada
            # como texto de sintaxis) cambian a la codificación segura
            # aquí. Este archivo nunca muestra un nombre de ítem de vuelta
            # al usuario (solo group_name/sus niveles, vía esc() en otras
            # partes), así que no se necesita mapeo inverso, a diferencia
            # del propio uso del mismo ayudante en advancedreliability.b.R.
            safe_names <- .fl_safe_names(all_items)
            for (col in all_items) names(df_raw)[names(df_raw) == col] <- unname(safe_names$to_safe[col])
            factors <- lapply(factors, function(f) {
                f$items <- unname(safe_names$to_safe[f$items])
                f
            })
            all_items <- unname(safe_names$to_safe[all_items])
            df_adv <- na.omit(df_raw)
            n_adv <- nrow(df_adv)
            group_levels <- levels(droplevels(df_adv[[group_name]]))
            if (length(group_levels) < 2L)
                jmvcore::reject(.("The grouping variable needs at least 2 levels (after removing missing values) to test invariance."))
            if (n_adv < 20L * length(group_levels))
                jmvcore::reject(.("Not enough complete cases per group to fit a multi-group confirmatory factor model (roughly 20+ per group recommended)."))

            item_is_ordinal <- identical(opt$itemType, "ordinal") || identical(opt$estimator, "wlsmv") ||
                (identical(opt$itemType, "auto") && .fl_is_low_cardinality(df_raw[, all_items, drop = FALSE]))
            estimator_eff <- if (item_is_ordinal) "WLSMV" else switch(opt$estimator, mlr = "MLR", ml = "ML", "ML")
            ordered_items <- if (item_is_ordinal) all_items else NULL
            threshold_or_intercept <- if (item_is_ordinal) "thresholds" else "intercepts"

            model_cf <- paste(vapply(factors, function(f)
                paste0(f$id, " =~ ", paste(f$items, collapse = " + ")), character(1)), collapse = "\n")

            gst <- res$groupSummaryTable
            grp_counts <- table(df_adv[[group_name]])
            private$.reset_table(gst, length(grp_counts))
            # jamovi's official module review (2026-09-16) found esc()'s
            # HTML-escaping applied to text bound for a native Table's cell
            # here -- jamovi already escapes table/output text itself, so a
            # group level named "A & B" would render literally as
            # "A &amp; B". esc() stays on group_name/its levels elsewhere in
            # this file, where they go into an Html result instead.
            # ES: La revisión oficial de módulos de jamovi (2026-09-16)
            # encontró el escapado HTML de esc() aplicado a texto destinado
            # a la celda de una Table nativa aquí -- jamovi ya escapa el
            # propio texto de tabla/salida, así que un nivel de grupo
            # llamado "A & B" se mostraría literalmente como "A &amp; B".
            # esc() se mantiene sobre group_name/sus niveles en otras
            # partes de este archivo, donde van a un resultado Html.
            for (i in seq_along(grp_counts)) gst$setRow(rowNo = i, values = list(level = names(grp_counts)[i], n = as.integer(grp_counts[i])))

            levels_seq <- list(
                list(name = .("Configural"), equal = character(0)),
                list(name = .("Metric (weak)"), equal = "loadings"),
                list(name = .("Scalar (strong)"), equal = c("loadings", threshold_or_intercept)),
                list(name = .("Strict"), equal = c("loadings", threshold_or_intercept, "residuals")))

            fits <- lapply(levels_seq, function(lvl)
                private$.fit_cfa_group(model_cf, df_adv, group_name, ordered_items, estimator_eff, lvl$equal))
            ok <- vapply(fits, function(f) !is.null(f) && isTRUE(tryCatch(lavaan::lavInspect(f, "converged"), error = function(e) FALSE)), logical(1))

            if (!ok[1])
                jmvcore::reject(.("The configural (baseline) model did not converge -- try fewer factors, more items per factor, or check the item/group assignments before testing invariance."))

            fm_list <- lapply(fits, function(f) if (is.null(f)) NULL else private$.fit_measures_ext(f, estimator_eff))

            inv_rows <- list()
            prev_cfi <- NA_real_
            last_ok_idx <- 0L
            for (i in seq_along(levels_seq)) {
                if (!ok[i] || is.null(fm_list[[i]])) {
                    inv_rows[[length(inv_rows) + 1L]] <- list(
                        model = levels_seq[[i]]$name, chisq = NA_real_, df = NA_integer_, cfi = NA_real_, rmsea = NA_real_,
                        dchisq = NA_real_, ddf = NA_integer_, pvalue = NA_real_, dcfi = NA_real_,
                        verdict = .("Did not converge"))
                    next
                }
                fm <- fm_list[[i]]
                dchisq <- NA_real_; ddf <- NA_integer_; p_val <- NA_real_; dcfi <- NA_real_; verdict <- .("Baseline")
                if (last_ok_idx > 0L) {
                    lrt_out <- tryCatch(lavaan::lavTestLRT(fits[[last_ok_idx]], fits[[i]]), error = function(e) NULL)
                    if (!is.null(lrt_out) && nrow(lrt_out) >= 2L && all(c("Chisq diff", "Df diff", "Pr(>Chisq)") %in% names(lrt_out))) {
                        dchisq <- lrt_out[["Chisq diff"]][2]
                        ddf    <- lrt_out[["Df diff"]][2]
                        p_val  <- lrt_out[["Pr(>Chisq)"]][2]
                    }
                    dcfi <- unname(fm["cfi"]) - prev_cfi
                    verdict <- private$.invariance_verdict(p_val, dcfi)
                }
                inv_rows[[length(inv_rows) + 1L]] <- list(
                    model = levels_seq[[i]]$name,
                    chisq = .fl_clean_na(unname(fm["chisq"])), df = unname(fm["df"]),
                    cfi = .fl_clean_na(unname(fm["cfi"])), rmsea = .fl_clean_na(unname(fm["rmsea"])),
                    dchisq = .fl_clean_na(dchisq), ddf = ddf, pvalue = .fl_clean_na(p_val), dcfi = .fl_clean_na(dcfi),
                    verdict = verdict)
                prev_cfi <- unname(fm["cfi"])
                last_ok_idx <- i
            }

            it <- res$invarianceTable
            private$.reset_table(it, length(inv_rows))
            for (i in seq_along(inv_rows)) it$setRow(rowNo = i, values = inv_rows[[i]])

            not_supported <- Filter(function(r) identical(r$verdict, .("Not supported")), inv_rows)
            discordant <- Filter(function(r) identical(r$verdict, .("Supported by LRT only")) ||
                                              identical(r$verdict, .("Supported by ΔCFI only")), inv_rows)
            failed_to_converge <- Filter(function(r) identical(r$verdict, .("Did not converge")), inv_rows)

            # EN: "Full invariance holds" doesn't by itself say WHICH
            # comparisons that actually licenses, and a partial failure
            # doesn't say what remains usable below it. max_level_name is
            # the last level in the sequence where every step up to and
            # including it was fully supported by both criteria (a
            # consecutive run from Configural) -- used to state explicitly
            # what that highest confirmed level does and does not permit,
            # rather than leaving "full invariance" / "does not hold" to
            # imply more or less than the data actually support.
            # ES: "La invariancia completa se sostiene" no dice por sí solo
            # QUÉ comparaciones habilita eso, y un fallo parcial no dice qué
            # sigue siendo utilizable por debajo de él. max_level_name es el
            # último nivel de la secuencia donde cada paso hasta él inclusive
            # estuvo plenamente respaldado por ambos criterios (una racha
            # consecutiva desde Configural) -- se usa para decir explícita-
            # mente qué permite y qué no permite ese nivel máximo confirmado,
            # en vez de dejar que "invariancia completa" / "no se sostiene"
            # implique más o menos de lo que los datos realmente respaldan.
            both_ok <- vapply(inv_rows, function(r)
                identical(r$verdict, .("Supported by both criteria")) ||
                identical(r$verdict, .("Baseline")), logical(1))
            max_ok_idx <- 0L
            for (i in seq_along(both_ok)) { if (both_ok[i]) max_ok_idx <- i else break }
            max_level_name <- if (max_ok_idx > 0L) inv_rows[[max_ok_idx]]$model else NULL

            usage_desc <- if (is.null(max_level_name))
                .("Not even configural invariance is fully confirmed -- this construct's structure is not yet established as comparable across groups at all.")
                else if (identical(max_level_name, .("Configural")))
                .("Configural invariance is the highest level fully supported: the factor structure itself is comparable, but no numeric comparison (correlations, regression coefficients, group means) across these groups is yet justified -- this includes composite reliability (Omega/CR, Jak &amp; Jorgensen, 2017): since metric invariance is not confirmed, the loadings Omega/CR is built from may genuinely differ by group, so a single pooled Omega/CR risks averaging over a real difference rather than describing either group correctly. Fitting one pooled model when loadings actually differ by group also forces that between-group difference into the model's error/residual variance, inflating it and deflating Omega/CR in a way that looks like a measurement problem but is really a violated-invariance problem. Suspect that reliability itself may differ by group here, and run Advanced Reliability separately within each group (rather than once on the pooled sample) to check.")
                else if (identical(max_level_name, .("Metric (weak)")))
                .("Metric (weak) invariance is the highest level fully supported: correlations/regression coefficients involving the factor can be compared across these groups, and so can composite reliability (Omega/CR from Advanced Reliability) -- it is computed from the same loadings this level confirms are equal. Group means/observed scores should not be compared -- that requires scalar invariance, which is not confirmed here.")
                else if (identical(max_level_name, .("Scalar (strong)")))
                .("Scalar (strong) invariance is the highest level fully supported -- this is exactly the level group mean/score comparisons require, so those comparisons are on solid footing, even though strict invariance (a stronger, less commonly needed condition) is not confirmed.")
                else
                .("Strict invariance is fully supported through every level tested. Group mean/score comparisons only require the scalar level reached before it, so this is more than sufficient for that purpose.")

            estim_desc <- if (item_is_ordinal)
                .("Items were treated as ordinal (WLSMV estimator on polychoric/tetrachoric correlations). χ², CFI and RMSEA below are the scaled/robust versions WLSMV recommends, not the plain ones.")
                else if (identical(estimator_eff, "MLR"))
                .("Items were treated as continuous, fit with MLR (robust to non-normality). χ², CFI and RMSEA below are the scaled/robust versions MLR recommends, not the plain ones.")
                else .("Items were treated as continuous, fit with ML.")
            override_desc <- if (item_is_ordinal && opt$estimator %in% c("ml", "mlr"))
                paste0(" ", jmvcore::format(
                    .("You selected {estimator}, but WLSMV was used instead because the items are ordinal."),
                    estimator = toupper(opt$estimator)))
                else ""

            res$invarianceNote$setContent(.fl_prose(
                "<p>", estim_desc, override_desc, " ", .("Each row after Configural is tested against the row just before it (not against Configural directly), on two criteria: a likelihood-ratio test (LRT; significant at p &lt; .05 means the added restriction costs a real amount of fit) and the change in CFI (ΔCFI &lt; -.01 is Cheung &amp; Rensvold's, 2002, sample-size-robust threshold for the same question). The LRT alone gets hypersensitive to trivial misfit in large samples -- part of why ΔCFI is reported alongside it -- but the two do not always agree: this report shows \"Supported by both criteria\" only when they do, and names which single criterion supports the restriction when they disagree, rather than treating either criterion passing as sufficient on its own."),
                "</p>",
                "<p>", .("Configural invariance means the same items load on the same factors in every group, with everything else free -- the minimum requirement for the construct to even be comparable across groups. Metric (weak) invariance -- equal loadings -- is required before comparing regression/correlation coefficients involving the factor across groups, and this includes composite reliability (Omega/CR, from Advanced Reliability): both are computed from the loadings, so a difference in Omega/CR between groups is not interpretable as a real reliability difference unless the loadings it is built from are already confirmed equal here. Scalar (strong) invariance -- also equal intercepts/thresholds -- is required before comparing group means or observed scores; without it, an observed mean difference may reflect item functioning differences, not a real difference on the construct. Strict invariance -- also equal residual variances -- is a stronger, less commonly required condition."),
                "</p>",
                if (length(failed_to_converge) > 0L) paste0("<p>⚠ ", .("One or more models in the sequence did not converge -- the invariance question cannot be answered past that point with this data/structure."), "</p>",
                        "<p>", usage_desc, "</p>")
                    else if (length(not_supported) > 0L) paste0("<p>⚠ ", jmvcore::format(
                        .("{model} invariance is not supported by either criterion -- do not compare whatever that level of invariance is required for (see above) across {group} groups without first identifying which specific parameters differ (partial invariance) via semTools::partialInvariance()/partialInvarianceCat()."),
                        model = not_supported[[1]]$model, group = esc(group_name)), "</p>",
                        "<p>", usage_desc, "</p>")
                    else if (length(discordant) > 0L) paste0("<p>⚠ ", jmvcore::format(
                        .("{model} invariance is supported by only one of the two criteria -- treat this level with real caution rather than as settled. Investigate partial invariance to see whether the discordance traces to specific items before relying on comparisons that require this level."),
                        model = discordant[[1]]$model), "</p>",
                        "<p>", usage_desc, "</p>")
                    else paste0("<p>✓ ", .("Full invariance is supported by both criteria through every level tested."), "</p>",
                        "<p>", usage_desc, "</p>")))

            private$.fitcmp_data <- data.frame(
                level = vapply(inv_rows, function(r) r$model, character(1)),
                cfi = vapply(inv_rows, function(r) if (is.na(r$cfi)) NA_real_ else r$cfi, numeric(1)),
                stringsAsFactors = FALSE)
            # jamovi's official module review (2026-09-16) found this plot
            # rendered blank on export: the render function only ever runs
            # live, straight after .run(), OR later on its own, in a
            # separate re-created analysis instance that restores saved
            # results but never calls .run() again -- so a private$ field
            # read there is always NULL on that second path. setState() is
            # the only channel that survives into that second instance.
            # ES: La revisión oficial de módulos de jamovi (2026-09-16)
            # encontró que este gráfico se exportaba en blanco: la función
            # de render solo corre en vivo justo después de .run(), O más
            # tarde por su cuenta, en una instancia de análisis separada que
            # restaura los resultados guardados pero nunca vuelve a llamar a
            # .run() -- así que un campo private$ leído ahí siempre es NULL
            # en ese segundo camino. setState() es el único canal que
            # sobrevive a esa segunda instancia.
            res$plotInvariance$setState(private$.fitcmp_data)
            res$plotInvarianceNote$setContent(.fl_prose("<p>", .("CFI (0-1, higher is better) at each level of the invariance sequence; a level's bar noticeably shorter than the one before it is the same signal as a significant likelihood-ratio test or a large ΔCFI in the table above, shown graphically."), "</p>"))

            adv_html <- paste0(.fl_prose_open(),
                "<h4>", .("What happened"), "</h4>",
                "<p>", jmvcore::format(
                    .("A {nf}-factor measurement model was tested for invariance across {ng} levels of {group} ({levels}) on n = {n} complete cases."),
                    nf = length(factors), ng = length(group_levels), group = esc(group_name),
                    levels = paste(esc(group_levels), collapse = ", "), n = n_adv), "</p>",
                "<h4>", .("What to do now"), "</h4>",
                "<ul style='line-height:1;'>",
                if (length(failed_to_converge) > 0L)
                    paste0("<li>", .("Resolve the convergence failure first -- fewer groups/factors, more items per factor, or more cases per group may help."), "</li>")
                else if (length(not_supported) > 0L)
                    paste0("<li>", .("Before comparing group means or scores, investigate partial invariance to find which specific items break the restriction, rather than abandoning the comparison or forcing full invariance."), "</li>")
                else if (length(discordant) > 0L)
                    paste0("<li>", .("At least one level is supported by only one criterion (LRT or ΔCFI, not both) -- treat comparisons requiring that level with caution and consider investigating partial invariance before relying on them."), "</li>")
                else
                    paste0("<li>", .("Group comparisons on this construct's means/scores are supported by this invariance sequence."), "</li>"),
                "</ul>",
                .fl_footnote(.("The ΔCFI criterion follows Cheung &amp; Rensvold (2002); the invariance level requirements for mean/score comparisons follow standard SEM practice (e.g. Vandenberg &amp; Lance, 2000).")),
                .fl_prose_close())
            res$interpretation$setContent(adv_html)
        },

        .plotInvariance = function(image, ggtheme, theme, ...) {
            d <- image$state
            if (is.null(d) || nrow(d) == 0L || all(is.na(d$cfi))) return(FALSE)
            cols <- private$.plot_colors(theme)
            d$level <- factor(d$level, levels = unique(d$level))
            p <- ggplot2::ggplot(d, ggplot2::aes(x = level, y = cfi)) +
                ggplot2::geom_bar(stat = "identity", fill = cols$primary, alpha = .85, width = .55) +
                ggplot2::geom_hline(yintercept = .95, linetype = "dashed", colour = cols$secondary, linewidth = .5) +
                ggplot2::coord_cartesian(ylim = c(0, 1)) +
                ggplot2::labs(x = NULL, y = "CFI",
                              title = .("Fit Across Invariance Levels")) +
                ggtheme
            print(p)
            TRUE
        }
    )
)
