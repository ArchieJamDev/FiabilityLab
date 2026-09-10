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

        .tr = function(en, es) .fl_tr(en, es, self$options$reportLang),
        .plot_theme = function() .fl_plot_theme(self$options$plotStyle),
        .plot_colors = function() .fl_plot_colors(self$options$plotStyle),
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
            tr <- private$.tr
            lrt_ok <- !is.na(p_val); cfi_ok <- !is.na(dcfi)
            lrt_holds <- lrt_ok && p_val >= .05
            cfi_holds <- cfi_ok && dcfi >= -.01
            if (!lrt_ok && !cfi_ok) return(tr("Unable to determine", "No se pudo determinar"))
            if (lrt_holds && cfi_holds) return(tr("Supported by both criteria", "Respaldado por ambos criterios"))
            if (lrt_holds) return(tr("Supported by LRT only", "Respaldado solo por LRT"))
            if (cfi_holds) return(tr("Supported by ΔCFI only", "Respaldado solo por ΔCFI"))
            tr("Not supported", "No respaldado")
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
            } else {
                fit_args$estimator <- estimator
            }
            tryCatch(do.call(lavaan::cfa, fit_args), error = function(e) NULL)
        },

        .run = function() {
            tr  <- private$.tr
            opt <- self$options
            res <- self$results
            esc <- private$.esc

            all_result_names <- c("groupSummaryTable", "invarianceTable", "invarianceNote",
                                   "plotInvariance", "plotInvarianceNote", "interpretation")
            hide_all <- function() for (nm in all_result_names) res[[nm]]$setVisible(FALSE)
            bail <- function(msg_en, msg_es) {
                hide_all()
                res$interpretation$setVisible(TRUE)
                res$interpretation$setContent(.fl_prose("<p>&#9888; ", tr(msg_en, msg_es), "</p>"))
            }

            if (!requireNamespace("lavaan", quietly = TRUE)) {
                bail("This analysis requires the lavaan package, which is not installed here.",
                     "Este análisis requiere el paquete lavaan, que no está instalado aquí.")
                return()
            }

            group_name <- opt$group
            if (is.null(group_name) || !nzchar(group_name)) {
                bail("Assign a grouping variable to test invariance across.",
                     "Asigne una variable de agrupación para comprobar la invariancia a través de ella.")
                return()
            }

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
                bail("Assign at least 3 items to at least one factor to run the invariance sequence.",
                     "Asigne al menos 3 ítems a al menos un factor para correr la secuencia de invariancia.")
                return()
            }

            all_items <- unique(unlist(lapply(factors, function(f) f$items)))
            df_raw <- self$data[, c(all_items, group_name), drop = FALSE]
            for (col in all_items) df_raw[[col]] <- suppressWarnings(as.numeric(df_raw[[col]]))
            df_raw[[group_name]] <- factor(df_raw[[group_name]])
            df_adv <- na.omit(df_raw)
            n_adv <- nrow(df_adv)
            group_levels <- levels(droplevels(df_adv[[group_name]]))
            if (length(group_levels) < 2L) {
                bail("The grouping variable needs at least 2 levels (after removing missing values) to test invariance.",
                     "La variable de agrupación necesita al menos 2 niveles (tras remover valores faltantes) para probar invariancia.")
                return()
            }
            if (n_adv < 20L * length(group_levels)) {
                bail("Not enough complete cases per group to fit a multi-group confirmatory factor model (roughly 20+ per group recommended).",
                     "No hay suficientes casos completos por grupo para ajustar un modelo factorial confirmatorio multigrupo (se recomiendan aproximadamente 20+ por grupo).")
                return()
            }

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
            for (i in seq_along(grp_counts)) gst$setRow(rowNo = i, values = list(level = esc(names(grp_counts)[i]), n = as.integer(grp_counts[i])))

            levels_seq <- list(
                list(name = tr("Configural", "Configural"), equal = character(0)),
                list(name = tr("Metric (weak)", "Métrica (débil)"), equal = "loadings"),
                list(name = tr("Scalar (strong)", "Escalar (fuerte)"), equal = c("loadings", threshold_or_intercept)),
                list(name = tr("Strict", "Estricta"), equal = c("loadings", threshold_or_intercept, "residuals")))

            fits <- lapply(levels_seq, function(lvl)
                private$.fit_cfa_group(model_cf, df_adv, group_name, ordered_items, estimator_eff, lvl$equal))
            ok <- vapply(fits, function(f) !is.null(f) && isTRUE(tryCatch(lavaan::lavInspect(f, "converged"), error = function(e) FALSE)), logical(1))

            if (!ok[1]) {
                bail("The configural (baseline) model did not converge -- try fewer factors, more items per factor, or check the item/group assignments before testing invariance.",
                     "El modelo configural (base) no convergió -- intente con menos factores, más ítems por factor, o revise las asignaciones de ítems/grupo antes de probar invariancia.")
                return()
            }

            fm_list <- lapply(fits, function(f) if (is.null(f)) NULL else private$.fit_measures_ext(f, estimator_eff))

            inv_rows <- list()
            prev_cfi <- NA_real_
            last_ok_idx <- 0L
            for (i in seq_along(levels_seq)) {
                if (!ok[i] || is.null(fm_list[[i]])) {
                    inv_rows[[length(inv_rows) + 1L]] <- list(
                        model = levels_seq[[i]]$name, chisq = NA_real_, df = NA_integer_, cfi = NA_real_, rmsea = NA_real_,
                        dchisq = NA_real_, ddf = NA_integer_, pvalue = NA_real_, dcfi = NA_real_,
                        verdict = tr("Did not converge", "No convergió"))
                    next
                }
                fm <- fm_list[[i]]
                dchisq <- NA_real_; ddf <- NA_integer_; p_val <- NA_real_; dcfi <- NA_real_; verdict <- tr("Baseline", "Línea base")
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

            not_supported <- Filter(function(r) identical(r$verdict, tr("Not supported", "No respaldado")), inv_rows)
            discordant <- Filter(function(r) identical(r$verdict, tr("Supported by LRT only", "Respaldado solo por LRT")) ||
                                              identical(r$verdict, tr("Supported by ΔCFI only", "Respaldado solo por ΔCFI")), inv_rows)
            failed_to_converge <- Filter(function(r) identical(r$verdict, tr("Did not converge", "No convergió")), inv_rows)

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
                identical(r$verdict, tr("Supported by both criteria", "Respaldado por ambos criterios")) ||
                identical(r$verdict, tr("Baseline", "Línea base")), logical(1))
            max_ok_idx <- 0L
            for (i in seq_along(both_ok)) { if (both_ok[i]) max_ok_idx <- i else break }
            max_level_name <- if (max_ok_idx > 0L) inv_rows[[max_ok_idx]]$model else NULL

            usage_desc <- if (is.null(max_level_name))
                tr("Not even configural invariance is fully confirmed -- this construct's structure is not yet established as comparable across groups at all.",
                   "Ni siquiera la invariancia configural está plenamente confirmada -- la estructura de este constructo aún no está establecida como comparable entre grupos en absoluto.")
                else if (identical(max_level_name, tr("Configural", "Configural")))
                tr("Configural invariance is the highest level fully supported: the factor structure itself is comparable, but no numeric comparison (correlations, regression coefficients, group means) across these groups is yet justified -- this includes composite reliability (Omega/CR, Jak &amp; Jorgensen, 2017): since metric invariance is not confirmed, the loadings Omega/CR is built from may genuinely differ by group, so a single pooled Omega/CR risks averaging over a real difference rather than describing either group correctly. Fitting one pooled model when loadings actually differ by group also forces that between-group difference into the model's error/residual variance, inflating it and deflating Omega/CR in a way that looks like a measurement problem but is really a violated-invariance problem. Suspect that reliability itself may differ by group here, and run Advanced Reliability separately within each group (rather than once on the pooled sample) to check.",
                   "La invariancia configural es el nivel máximo plenamente respaldado: la estructura factorial en sí es comparable, pero ninguna comparación numérica (correlaciones, coeficientes de regresión, medias de grupo) entre estos grupos está aún justificada -- esto incluye la confiabilidad compuesta (Omega/CR, Jak y Jorgensen, 2017): como la invariancia métrica no está confirmada, las cargas de las que se construye Omega/CR pueden diferir genuinamente por grupo, así que un Omega/CR único agrupado corre el riesgo de promediar sobre una diferencia real en vez de describir correctamente a ninguno de los dos grupos. Ajustar un solo modelo agrupado cuando las cargas en realidad difieren por grupo también fuerza esa diferencia entre grupos hacia la varianza de error/residual del modelo, inflándola y desinflando el Omega/CR de una forma que parece un problema de medición pero en realidad es un problema de invariancia violada. Sospeche que la confiabilidad misma puede diferir por grupo aquí, y ejecute Advanced Reliability por separado dentro de cada grupo (en vez de una sola vez sobre la muestra agrupada) para revisarlo.")
                else if (identical(max_level_name, tr("Metric (weak)", "Métrica (débil)")))
                tr("Metric (weak) invariance is the highest level fully supported: correlations/regression coefficients involving the factor can be compared across these groups, and so can composite reliability (Omega/CR from Advanced Reliability) -- it is computed from the same loadings this level confirms are equal. Group means/observed scores should not be compared -- that requires scalar invariance, which is not confirmed here.",
                   "La invariancia métrica (débil) es el nivel máximo plenamente respaldado: las correlaciones/coeficientes de regresión que involucren al factor pueden compararse entre estos grupos, y también la confiabilidad compuesta (Omega/CR de Advanced Reliability) -- se calcula a partir de las mismas cargas que este nivel confirma que son iguales. Las medias de grupo/puntajes observados no deberían compararse -- eso requiere invariancia escalar, que no está confirmada aquí.")
                else if (identical(max_level_name, tr("Scalar (strong)", "Escalar (fuerte)")))
                tr("Scalar (strong) invariance is the highest level fully supported -- this is exactly the level group mean/score comparisons require, so those comparisons are on solid footing, even though strict invariance (a stronger, less commonly needed condition) is not confirmed.",
                   "La invariancia escalar (fuerte) es el nivel máximo plenamente respaldado -- este es exactamente el nivel que requieren las comparaciones de medias/puntajes de grupo, así que esas comparaciones están en una base sólida, aunque la invariancia estricta (una condición más fuerte, requerida con menos frecuencia) no esté confirmada.")
                else
                tr("Strict invariance is fully supported through every level tested. Group mean/score comparisons only require the scalar level reached before it, so this is more than sufficient for that purpose.",
                   "La invariancia estricta está plenamente respaldada en todos los niveles probados. Las comparaciones de medias/puntajes de grupo solo requieren el nivel escalar alcanzado antes de ella, así que esto es más que suficiente para ese propósito.")

            estim_desc <- if (item_is_ordinal)
                tr("Items were treated as ordinal (WLSMV estimator on polychoric/tetrachoric correlations). χ², CFI and RMSEA below are the scaled/robust versions WLSMV recommends, not the plain ones.",
                   "Los ítems se trataron como ordinales (estimador WLSMV sobre correlaciones policóricas/tetracóricas). El χ², CFI y RMSEA de abajo son las versiones escaladas/robustas que WLSMV recomienda, no las simples.")
                else if (identical(estimator_eff, "MLR"))
                tr("Items were treated as continuous, fit with MLR (robust to non-normality). χ², CFI and RMSEA below are the scaled/robust versions MLR recommends, not the plain ones.",
                   "Los ítems se trataron como continuos, ajustados con MLR (robusto a la no normalidad). El χ², CFI y RMSEA de abajo son las versiones escaladas/robustas que MLR recomienda, no las simples.")
                else tr("Items were treated as continuous, fit with ML.", "Los ítems se trataron como continuos, ajustados con ML.")
            override_desc <- if (item_is_ordinal && opt$estimator %in% c("ml", "mlr"))
                paste0(" ", tr(
                    paste0("You selected ", toupper(opt$estimator), ", but WLSMV was used instead because the items are ordinal."),
                    paste0("Usted seleccionó ", toupper(opt$estimator), ", pero se usó WLSMV en su lugar porque los ítems son ordinales.")))
                else ""

            res$invarianceNote$setContent(.fl_prose(
                "<p>", estim_desc, override_desc, " ", tr(
                    paste0("Each row after Configural is tested against the row just before it (not against Configural directly), on two criteria: a likelihood-ratio test (LRT; significant at p &lt; .05 means the added restriction costs a real amount of fit) and the change in CFI (&Delta;CFI &lt; -.01 is Cheung &amp; Rensvold's, 2002, sample-size-robust threshold for the same question). The LRT alone gets hypersensitive to trivial misfit in large samples -- part of why &Delta;CFI is reported alongside it -- but the two do not always agree: this report shows \"Supported by both criteria\" only when they do, and names which single criterion supports the restriction when they disagree, rather than treating either criterion passing as sufficient on its own."),
                    paste0("Cada fila después de Configural se prueba contra la fila justo anterior (no contra Configural directamente), con dos criterios: una prueba de razón de verosimilitud (LRT; significativa en p &lt; .05 significa que la restricción agregada cuesta una cantidad real de ajuste) y el cambio en CFI (&Delta;CFI &lt; -.01 es el umbral robusto al tamaño muestral de Cheung &amp; Rensvold, 2002, para la misma pregunta). El LRT por sí solo se vuelve hipersensible a desajustes triviales en muestras grandes -- parte de por qué se reporta el &Delta;CFI junto a él -- pero ambos no siempre coinciden: este informe muestra \"Respaldado por ambos criterios\" solo cuando coinciden, y nombra qué criterio único respalda la restricción cuando discrepan, en vez de tratar que cualquiera de los dos se cumpla como suficiente por sí solo.")),
                "</p>",
                "<p>", tr(
                    "Configural invariance means the same items load on the same factors in every group, with everything else free -- the minimum requirement for the construct to even be comparable across groups. Metric (weak) invariance -- equal loadings -- is required before comparing regression/correlation coefficients involving the factor across groups, and this includes composite reliability (Omega/CR, from Advanced Reliability): both are computed from the loadings, so a difference in Omega/CR between groups is not interpretable as a real reliability difference unless the loadings it is built from are already confirmed equal here. Scalar (strong) invariance -- also equal intercepts/thresholds -- is required before comparing group means or observed scores; without it, an observed mean difference may reflect item functioning differences, not a real difference on the construct. Strict invariance -- also equal residual variances -- is a stronger, less commonly required condition.",
                    "La invariancia configural significa que los mismos ítems cargan sobre los mismos factores en cada grupo, con todo lo demás libre -- el requisito mínimo para que el constructo sea siquiera comparable entre grupos. La invariancia métrica (débil) -- cargas iguales -- se requiere antes de comparar coeficientes de regresión/correlación que involucren al factor entre grupos, y esto incluye la confiabilidad compuesta (Omega/CR, de Advanced Reliability): ambas se calculan a partir de las cargas, así que una diferencia en Omega/CR entre grupos no es interpretable como una diferencia real de confiabilidad a menos que las cargas de las que se construye ya estén confirmadas como iguales aquí. La invariancia escalar (fuerte) -- también interceptos/umbrales iguales -- se requiere antes de comparar medias de grupo o puntajes observados; sin ella, una diferencia de medias observada puede reflejar diferencias en el funcionamiento de los ítems, no una diferencia real en el constructo. La invariancia estricta -- también varianzas residuales iguales -- es una condición más fuerte, requerida con menos frecuencia."),
                "</p>",
                if (length(failed_to_converge) > 0L) paste0("<p>&#9888; ", tr(
                        "One or more models in the sequence did not converge -- the invariance question cannot be answered past that point with this data/structure.",
                        "Uno o más modelos de la secuencia no convergieron -- la pregunta de invariancia no puede responderse más allá de ese punto con estos datos/estructura."), "</p>",
                        "<p>", usage_desc, "</p>")
                    else if (length(not_supported) > 0L) paste0("<p>&#9888; ", tr(
                        paste0(not_supported[[1]]$model, " invariance is not supported by either criterion -- do not compare whatever that level of invariance is required for (see above) across ", esc(group_name), " groups without first identifying which specific parameters differ (partial invariance) via semTools::partialInvariance()/partialInvarianceCat()."),
                        paste0("La invariancia ", not_supported[[1]]$model, " no está respaldada por ningún criterio -- no compare aquello para lo que se requiere ese nivel de invariancia (vea arriba) entre grupos de ", esc(group_name), " sin antes identificar qué parámetros específicos difieren (invariancia parcial) vía semTools::partialInvariance()/partialInvarianceCat().")), "</p>",
                        "<p>", usage_desc, "</p>")
                    else if (length(discordant) > 0L) paste0("<p>&#9888; ", tr(
                        paste0(discordant[[1]]$model, " invariance is supported by only one of the two criteria -- treat this level with real caution rather than as settled. Investigate partial invariance to see whether the discordance traces to specific items before relying on comparisons that require this level."),
                        paste0("La invariancia ", discordant[[1]]$model, " está respaldada por solo uno de los dos criterios -- trate este nivel con verdadera cautela en vez de darlo por resuelto. Investigue la invariancia parcial para ver si la discordancia se debe a ítems específicos antes de confiar en comparaciones que requieran este nivel.")), "</p>",
                        "<p>", usage_desc, "</p>")
                    else paste0("<p>&#10003; ", tr("Full invariance is supported by both criteria through every level tested.",
                                                     "La invariancia completa está respaldada por ambos criterios en todos los niveles probados."), "</p>",
                        "<p>", usage_desc, "</p>")))

            private$.fitcmp_data <- data.frame(
                level = vapply(inv_rows, function(r) r$model, character(1)),
                cfi = vapply(inv_rows, function(r) if (is.na(r$cfi)) NA_real_ else r$cfi, numeric(1)),
                stringsAsFactors = FALSE)
            res$plotInvarianceNote$setContent(.fl_prose("<p>", tr(
                "CFI (0-1, higher is better) at each level of the invariance sequence; a level's bar noticeably shorter than the one before it is the same signal as a significant likelihood-ratio test or a large &Delta;CFI in the table above, shown graphically.",
                "CFI (0-1, mayor es mejor) en cada nivel de la secuencia de invariancia; una barra de un nivel notablemente más corta que la anterior es la misma señal que una prueba de razón de verosimilitud significativa o un &Delta;CFI grande en la tabla de arriba, mostrada gráficamente."), "</p>"))

            adv_html <- paste0(.fl_prose_open(),
                "<h4>", tr("What happened", "Qué pasó"), "</h4>",
                "<p>", tr(
                    paste0("A ", length(factors), "-factor measurement model was tested for invariance across ", length(group_levels), " levels of ", esc(group_name), " (", paste(esc(group_levels), collapse = ", "), ") on n = ", n_adv, " complete cases."),
                    paste0("Se probó un modelo de medición de ", length(factors), " factor(es) para invariancia a través de ", length(group_levels), " niveles de ", esc(group_name), " (", paste(esc(group_levels), collapse = ", "), ") sobre n = ", n_adv, " casos completos.")), "</p>",
                "<h4>", tr("What to do now", "Qué hacer ahora"), "</h4>",
                "<ul style='line-height:1;'>",
                if (length(failed_to_converge) > 0L)
                    paste0("<li>", tr("Resolve the convergence failure first -- fewer groups/factors, more items per factor, or more cases per group may help.",
                                      "Resuelva primero la falla de convergencia -- menos grupos/factores, más ítems por factor, o más casos por grupo pueden ayudar."), "</li>")
                else if (length(not_supported) > 0L)
                    paste0("<li>", tr("Before comparing group means or scores, investigate partial invariance to find which specific items break the restriction, rather than abandoning the comparison or forcing full invariance.",
                                      "Antes de comparar medias o puntajes de grupo, investigue la invariancia parcial para encontrar qué ítems específicos rompen la restricción, en vez de abandonar la comparación o forzar la invariancia completa."), "</li>")
                else if (length(discordant) > 0L)
                    paste0("<li>", tr("At least one level is supported by only one criterion (LRT or ΔCFI, not both) -- treat comparisons requiring that level with caution and consider investigating partial invariance before relying on them.",
                                      "Al menos un nivel está respaldado por solo un criterio (LRT o ΔCFI, no ambos) -- trate con cautela las comparaciones que requieran ese nivel y considere investigar invariancia parcial antes de confiar en ellas."), "</li>")
                else
                    paste0("<li>", tr("Group comparisons on this construct's means/scores are supported by this invariance sequence.",
                                      "Las comparaciones de grupo sobre las medias/puntajes de este constructo están respaldadas por esta secuencia de invariancia."), "</li>"),
                "</ul>",
                .fl_footnote(tr(
                    "The ΔCFI criterion follows Cheung &amp; Rensvold (2002); the invariance level requirements for mean/score comparisons follow standard SEM practice (e.g. Vandenberg &amp; Lance, 2000).",
                    "El criterio ΔCFI sigue a Cheung &amp; Rensvold (2002); los requisitos de nivel de invariancia para comparaciones de medias/puntajes siguen la práctica estándar en SEM (p. ej. Vandenberg &amp; Lance, 2000).")),
                .fl_prose_close())
            res$interpretation$setContent(adv_html)
        },

        .plotInvariance = function(image, ggtheme, theme, ...) {
            d <- private$.fitcmp_data
            if (is.null(d) || nrow(d) == 0L || all(is.na(d$cfi))) return(FALSE)
            cols <- private$.plot_colors()
            d$level <- factor(d$level, levels = unique(d$level))
            p <- ggplot2::ggplot(d, ggplot2::aes(x = level, y = cfi)) +
                ggplot2::geom_bar(stat = "identity", fill = cols$primary, alpha = .85, width = .55) +
                ggplot2::geom_hline(yintercept = .95, linetype = "dashed", colour = cols$secondary, linewidth = .5) +
                ggplot2::coord_cartesian(ylim = c(0, 1)) +
                ggplot2::labs(x = NULL, y = "CFI",
                              title = private$.tr("Fit Across Invariance Levels", "Ajuste a Través de los Niveles de Invariancia")) +
                private$.plot_theme()
            print(p)
            TRUE
        }
    )
)
