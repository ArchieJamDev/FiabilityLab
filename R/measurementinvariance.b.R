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

        # Report-text translation dispatch -- see shared-helpers.R's own
        # note on .fl_tr() for why this stays separate from jamovi's
        # native .() catalog (which still drives every YAML-derived
        # title/label/checkbox).
        .tr = function(en, es) .fl_tr(en, es, self$options$reportLang),

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
            if (!lrt_ok && !cfi_ok) return(private$.tr("Unable to determine", "No se pudo determinar"))
            if (lrt_holds && cfi_holds) return(private$.tr("Supported by both criteria", "Respaldado por ambos criterios"))
            if (lrt_holds) return(private$.tr("Supported by LRT only", "Respaldado solo por LRT"))
            if (cfi_holds) return(private$.tr("Supported by \u0394CFI only", "Respaldado solo por \u0394CFI"))
            private$.tr("Not supported", "No respaldado")
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
                jmvcore::reject(private$.tr("This analysis requires the lavaan package, which is not installed here.", "Este an\u00E1lisis requiere el paquete lavaan, que no est\u00E1 instalado aqu\u00ED."))

            group_name <- opt$group
            if (is.null(group_name) || !nzchar(group_name))
                jmvcore::reject(private$.tr("Assign a grouping variable to test invariance across.", "Asigne una variable de agrupaci\u00F3n para comprobar la invariancia a trav\u00E9s de ella."))

            factors <- list()
            factors_opt <- opt$factors
            for (i in seq_along(factors_opt)) {
                grp <- factors_opt[[i]]
                fitems <- grp$vars
                if (length(fitems) >= 3L) {
                    fname <- grp$label
                    if (is.null(fname) || !nzchar(trimws(fname))) fname <- paste(private$.tr("Factor", "Factor"), i)
                    factors[[length(factors) + 1L]] <- list(id = paste0("F", i), name = fname, items = fitems)
                }
            }
            if (length(factors) == 0L)
                jmvcore::reject(private$.tr("Assign at least 3 items to at least one factor to run the invariance sequence.", "Asigne al menos 3 \u00EDtems a al menos un factor para correr la secuencia de invariancia."))

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
                jmvcore::reject(private$.tr("The grouping variable needs at least 2 levels (after removing missing values) to test invariance.", "La variable de agrupaci\u00F3n necesita al menos 2 niveles (tras remover valores faltantes) para probar invariancia."))
            if (n_adv < 20L * length(group_levels))
                jmvcore::reject(private$.tr("Not enough complete cases per group to fit a multi-group confirmatory factor model (roughly 20+ per group recommended).", "No hay suficientes casos completos por grupo para ajustar un modelo factorial confirmatorio multigrupo (se recomiendan aproximadamente 20+ por grupo)."))

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
                list(name = private$.tr("Configural", "Configural"), equal = character(0)),
                list(name = private$.tr("Metric (weak)", "M\u00E9trica (d\u00E9bil)"), equal = "loadings"),
                list(name = private$.tr("Scalar (strong)", "Escalar (fuerte)"), equal = c("loadings", threshold_or_intercept)),
                list(name = private$.tr("Strict", "Estricta"), equal = c("loadings", threshold_or_intercept, "residuals")))

            fits <- lapply(levels_seq, function(lvl)
                private$.fit_cfa_group(model_cf, df_adv, group_name, ordered_items, estimator_eff, lvl$equal))
            ok <- vapply(fits, function(f) !is.null(f) && isTRUE(tryCatch(lavaan::lavInspect(f, "converged"), error = function(e) FALSE)), logical(1))

            if (!ok[1])
                jmvcore::reject(private$.tr("The configural (baseline) model did not converge -- try fewer factors, more items per factor, or check the item/group assignments before testing invariance.", "El modelo configural (base) no convergi\u00F3 -- intente con menos factores, m\u00E1s \u00EDtems por factor, o revise las asignaciones de \u00EDtems/grupo antes de probar invariancia."))

            fm_list <- lapply(fits, function(f) if (is.null(f)) NULL else private$.fit_measures_ext(f, estimator_eff))

            inv_rows <- list()
            prev_cfi <- NA_real_
            last_ok_idx <- 0L
            for (i in seq_along(levels_seq)) {
                if (!ok[i] || is.null(fm_list[[i]])) {
                    inv_rows[[length(inv_rows) + 1L]] <- list(
                        model = levels_seq[[i]]$name, chisq = NA_real_, df = NA_integer_, cfi = NA_real_, rmsea = NA_real_,
                        dchisq = NA_real_, ddf = NA_integer_, pvalue = NA_real_, dcfi = NA_real_,
                        verdict = private$.tr("Did not converge", "No convergi\u00F3"))
                    next
                }
                fm <- fm_list[[i]]
                dchisq <- NA_real_; ddf <- NA_integer_; p_val <- NA_real_; dcfi <- NA_real_; verdict <- private$.tr("Baseline", "L\u00EDnea base")
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

            not_supported <- Filter(function(r) identical(r$verdict, private$.tr("Not supported", "No respaldado")), inv_rows)
            discordant <- Filter(function(r) identical(r$verdict, private$.tr("Supported by LRT only", "Respaldado solo por LRT")) ||
                                              identical(r$verdict, private$.tr("Supported by \u0394CFI only", "Respaldado solo por \u0394CFI")), inv_rows)
            failed_to_converge <- Filter(function(r) identical(r$verdict, private$.tr("Did not converge", "No convergi\u00F3")), inv_rows)

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
                identical(r$verdict, private$.tr("Supported by both criteria", "Respaldado por ambos criterios")) ||
                identical(r$verdict, private$.tr("Baseline", "L\u00EDnea base")), logical(1))
            max_ok_idx <- 0L
            for (i in seq_along(both_ok)) { if (both_ok[i]) max_ok_idx <- i else break }
            max_level_name <- if (max_ok_idx > 0L) inv_rows[[max_ok_idx]]$model else NULL

            usage_desc <- if (is.null(max_level_name))
                private$.tr("Not even configural invariance is fully confirmed -- this construct's structure is not yet established as comparable across groups at all.", "Ni siquiera la invariancia configural est\u00E1 plenamente confirmada -- la estructura de este constructo a\u00FAn no est\u00E1 establecida como comparable entre grupos en absoluto.")
                else if (identical(max_level_name, private$.tr("Configural", "Configural")))
                private$.tr("Configural invariance is the highest level fully supported: the factor structure itself is comparable, but no numeric comparison (correlations, regression coefficients, group means) across these groups is yet justified -- this includes composite reliability (Omega/CR, Jak &amp; Jorgensen, 2017): since metric invariance is not confirmed, the loadings Omega/CR is built from may genuinely differ by group, so a single pooled Omega/CR risks averaging over a real difference rather than describing either group correctly. Fitting one pooled model when loadings actually differ by group also forces that between-group difference into the model's error/residual variance, inflating it and deflating Omega/CR in a way that looks like a measurement problem but is really a violated-invariance problem. Suspect that reliability itself may differ by group here, and run Advanced Reliability separately within each group (rather than once on the pooled sample) to check.", "La invariancia configural es el nivel m\u00E1ximo plenamente respaldado: la estructura factorial en s\u00ED es comparable, pero ninguna comparaci\u00F3n num\u00E9rica (correlaciones, coeficientes de regresi\u00F3n, medias de grupo) entre estos grupos est\u00E1 a\u00FAn justificada -- esto incluye la confiabilidad compuesta (Omega/CR, Jak y Jorgensen, 2017): como la invariancia m\u00E9trica no est\u00E1 confirmada, las cargas de las que se construye Omega/CR pueden diferir genuinamente por grupo, as\u00ED que un Omega/CR \u00FAnico agrupado corre el riesgo de promediar sobre una diferencia real en vez de describir correctamente a ninguno de los dos grupos. Ajustar un solo modelo agrupado cuando las cargas en realidad difieren por grupo tambi\u00E9n fuerza esa diferencia entre grupos hacia la varianza de error/residual del modelo, infl\u00E1ndola y desinflando el Omega/CR de una forma que parece un problema de medici\u00F3n pero en realidad es un problema de invariancia violada. Sospeche que la confiabilidad misma puede diferir por grupo aqu\u00ED, y ejecute Advanced Reliability por separado dentro de cada grupo (en vez de una sola vez sobre la muestra agrupada) para revisarlo.")
                else if (identical(max_level_name, private$.tr("Metric (weak)", "M\u00E9trica (d\u00E9bil)")))
                private$.tr("Metric (weak) invariance is the highest level fully supported: correlations/regression coefficients involving the factor can be compared across these groups, and so can composite reliability (Omega/CR from Advanced Reliability) -- it is computed from the same loadings this level confirms are equal. Group means/observed scores should not be compared -- that requires scalar invariance, which is not confirmed here.", "La invariancia m\u00E9trica (d\u00E9bil) es el nivel m\u00E1ximo plenamente respaldado: las correlaciones/coeficientes de regresi\u00F3n que involucren al factor pueden compararse entre estos grupos, y tambi\u00E9n la confiabilidad compuesta (Omega/CR de Advanced Reliability) -- se calcula a partir de las mismas cargas que este nivel confirma que son iguales. Las medias de grupo/puntajes observados no deber\u00EDan compararse -- eso requiere invariancia escalar, que no est\u00E1 confirmada aqu\u00ED.")
                else if (identical(max_level_name, private$.tr("Scalar (strong)", "Escalar (fuerte)")))
                private$.tr("Scalar (strong) invariance is the highest level fully supported -- this is exactly the level group mean/score comparisons require, so those comparisons are on solid footing, even though strict invariance (a stronger, less commonly needed condition) is not confirmed.", "La invariancia escalar (fuerte) es el nivel m\u00E1ximo plenamente respaldado -- este es exactamente el nivel que requieren las comparaciones de medias/puntajes de grupo, as\u00ED que esas comparaciones est\u00E1n en una base s\u00F3lida, aunque la invariancia estricta (una condici\u00F3n m\u00E1s fuerte, requerida con menos frecuencia) no est\u00E9 confirmada.")
                else
                private$.tr("Strict invariance is fully supported through every level tested. Group mean/score comparisons only require the scalar level reached before it, so this is more than sufficient for that purpose.", "La invariancia estricta est\u00E1 plenamente respaldada en todos los niveles probados. Las comparaciones de medias/puntajes de grupo solo requieren el nivel escalar alcanzado antes de ella, as\u00ED que esto es m\u00E1s que suficiente para ese prop\u00F3sito.")

            estim_desc <- if (item_is_ordinal)
                private$.tr("Items were treated as ordinal (WLSMV estimator on polychoric/tetrachoric correlations). \u03C7\u00B2, CFI and RMSEA below are the scaled/robust versions WLSMV recommends, not the plain ones.", "Los \u00EDtems se trataron como ordinales (estimador WLSMV sobre correlaciones polic\u00F3ricas/tetrac\u00F3ricas). El \u03C7\u00B2, CFI y RMSEA de abajo son las versiones escaladas/robustas que WLSMV recomienda, no las simples.")
                else if (identical(estimator_eff, "MLR"))
                private$.tr("Items were treated as continuous, fit with MLR (robust to non-normality). \u03C7\u00B2, CFI and RMSEA below are the scaled/robust versions MLR recommends, not the plain ones.", "Los \u00EDtems se trataron como continuos, ajustados con MLR (robusto a la no normalidad). El \u03C7\u00B2, CFI y RMSEA de abajo son las versiones escaladas/robustas que MLR recomienda, no las simples.")
                else private$.tr("Items were treated as continuous, fit with ML.", "Los \u00EDtems se trataron como continuos, ajustados con ML.")
            override_desc <- if (item_is_ordinal && opt$estimator %in% c("ml", "mlr"))
                paste0(" ", jmvcore::format(
                    private$.tr("You selected {estimator}, but WLSMV was used instead because the items are ordinal.", "Usted seleccion\u00F3 {estimator}, pero se us\u00F3 WLSMV en su lugar porque los \u00EDtems son ordinales."),
                    estimator = toupper(opt$estimator)))
                else ""

            res$invarianceNote$setContent(.fl_prose(
                "<p>", estim_desc, override_desc, " ", private$.tr("Each row after Configural is tested against the row just before it (not against Configural directly), on two criteria: a likelihood-ratio test (LRT; significant at p &lt; .05 means the added restriction costs a real amount of fit) and the change in CFI (\u0394CFI &lt; -.01 is Cheung &amp; Rensvold's, 2002, sample-size-robust threshold for the same question). The LRT alone gets hypersensitive to trivial misfit in large samples -- part of why \u0394CFI is reported alongside it -- but the two do not always agree: this report shows \"Supported by both criteria\" only when they do, and names which single criterion supports the restriction when they disagree, rather than treating either criterion passing as sufficient on its own.", "Cada fila despu\u00E9s de Configural se prueba contra la fila justo anterior (no contra Configural directamente), con dos criterios: una prueba de raz\u00F3n de verosimilitud (LRT; significativa en p &lt; .05 significa que la restricci\u00F3n agregada cuesta una cantidad real de ajuste) y el cambio en CFI (\u0394CFI &lt; -.01 es el umbral robusto al tama\u00F1o muestral de Cheung &amp; Rensvold, 2002, para la misma pregunta). El LRT por s\u00ED solo se vuelve hipersensible a desajustes triviales en muestras grandes -- parte de por qu\u00E9 se reporta el \u0394CFI junto a \u00E9l -- pero ambos no siempre coinciden: este informe muestra \"Respaldado por ambos criterios\" solo cuando coinciden, y nombra qu\u00E9 criterio \u00FAnico respalda la restricci\u00F3n cuando discrepan, en vez de tratar que cualquiera de los dos se cumpla como suficiente por s\u00ED solo."),
                "</p>",
                "<p>", private$.tr("Configural invariance means the same items load on the same factors in every group, with everything else free -- the minimum requirement for the construct to even be comparable across groups. Metric (weak) invariance -- equal loadings -- is required before comparing regression/correlation coefficients involving the factor across groups, and this includes composite reliability (Omega/CR, from Advanced Reliability): both are computed from the loadings, so a difference in Omega/CR between groups is not interpretable as a real reliability difference unless the loadings it is built from are already confirmed equal here. Scalar (strong) invariance -- also equal intercepts/thresholds -- is required before comparing group means or observed scores; without it, an observed mean difference may reflect item functioning differences, not a real difference on the construct. Strict invariance -- also equal residual variances -- is a stronger, less commonly required condition.", "La invariancia configural significa que los mismos \u00EDtems cargan sobre los mismos factores en cada grupo, con todo lo dem\u00E1s libre -- el requisito m\u00EDnimo para que el constructo sea siquiera comparable entre grupos. La invariancia m\u00E9trica (d\u00E9bil) -- cargas iguales -- se requiere antes de comparar coeficientes de regresi\u00F3n/correlaci\u00F3n que involucren al factor entre grupos, y esto incluye la confiabilidad compuesta (Omega/CR, de Advanced Reliability): ambas se calculan a partir de las cargas, as\u00ED que una diferencia en Omega/CR entre grupos no es interpretable como una diferencia real de confiabilidad a menos que las cargas de las que se construye ya est\u00E9n confirmadas como iguales aqu\u00ED. La invariancia escalar (fuerte) -- tambi\u00E9n interceptos/umbrales iguales -- se requiere antes de comparar medias de grupo o puntajes observados; sin ella, una diferencia de medias observada puede reflejar diferencias en el funcionamiento de los \u00EDtems, no una diferencia real en el constructo. La invariancia estricta -- tambi\u00E9n varianzas residuales iguales -- es una condici\u00F3n m\u00E1s fuerte, requerida con menos frecuencia."),
                "</p>",
                if (length(failed_to_converge) > 0L) paste0("<p>⚠ ", private$.tr("One or more models in the sequence did not converge -- the invariance question cannot be answered past that point with this data/structure.", "Uno o m\u00E1s modelos de la secuencia no convergieron -- la pregunta de invariancia no puede responderse m\u00E1s all\u00E1 de ese punto con estos datos/estructura."), "</p>",
                        "<p>", usage_desc, "</p>")
                    else if (length(not_supported) > 0L) paste0("<p>⚠ ", jmvcore::format(
                        private$.tr("{model} invariance is not supported by either criterion -- do not compare whatever that level of invariance is required for (see above) across {group} groups without first identifying which specific parameters differ (partial invariance) via semTools::partialInvariance()/partialInvarianceCat().", "La invariancia {model} no est\u00E1 respaldada por ning\u00FAn criterio -- no compare aquello para lo que se requiere ese nivel de invariancia (vea arriba) entre grupos de {group} sin antes identificar qu\u00E9 par\u00E1metros espec\u00EDficos difieren (invariancia parcial) v\u00EDa semTools::partialInvariance()/partialInvarianceCat()."),
                        model = not_supported[[1]]$model, group = esc(group_name)), "</p>",
                        "<p>", usage_desc, "</p>")
                    else if (length(discordant) > 0L) paste0("<p>⚠ ", jmvcore::format(
                        private$.tr("{model} invariance is supported by only one of the two criteria -- treat this level with real caution rather than as settled. Investigate partial invariance to see whether the discordance traces to specific items before relying on comparisons that require this level.", "La invariancia {model} est\u00E1 respaldada por solo uno de los dos criterios -- trate este nivel con verdadera cautela en vez de darlo por resuelto. Investigue la invariancia parcial para ver si la discordancia se debe a \u00EDtems espec\u00EDficos antes de confiar en comparaciones que requieran este nivel."),
                        model = discordant[[1]]$model), "</p>",
                        "<p>", usage_desc, "</p>")
                    else paste0("<p>✓ ", private$.tr("Full invariance is supported by both criteria through every level tested.", "La invariancia completa est\u00E1 respaldada por ambos criterios en todos los niveles probados."), "</p>",
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
            res$plotInvarianceNote$setContent(.fl_prose("<p>", private$.tr("CFI (0-1, higher is better) at each level of the invariance sequence; a level's bar noticeably shorter than the one before it is the same signal as a significant likelihood-ratio test or a large \u0394CFI in the table above, shown graphically.", "CFI (0-1, mayor es mejor) en cada nivel de la secuencia de invariancia; una barra de un nivel notablemente m\u00E1s corta que la anterior es la misma se\u00F1al que una prueba de raz\u00F3n de verosimilitud significativa o un \u0394CFI grande en la tabla de arriba, mostrada gr\u00E1ficamente."), "</p>"))

            adv_html <- paste0(.fl_prose_open(),
                "<h4>", private$.tr("What happened", "Qu\u00E9 pas\u00F3"), "</h4>",
                "<p>", jmvcore::format(
                    private$.tr("A {nf}-factor measurement model was tested for invariance across {ng} levels of {group} ({levels}) on n = {n} complete cases.", "Se prob\u00F3 un modelo de medici\u00F3n de {nf} factor(es) para invariancia a trav\u00E9s de {ng} niveles de {group} ({levels}) sobre n = {n} casos completos."),
                    nf = length(factors), ng = length(group_levels), group = esc(group_name),
                    levels = paste(esc(group_levels), collapse = ", "), n = n_adv), "</p>",
                "<h4>", private$.tr("What to do now", "Qu\u00E9 hacer ahora"), "</h4>",
                "<ul style='line-height:1;'>",
                if (length(failed_to_converge) > 0L)
                    paste0("<li>", private$.tr("Resolve the convergence failure first -- fewer groups/factors, more items per factor, or more cases per group may help.", "Resuelva primero la falla de convergencia -- menos grupos/factores, m\u00E1s \u00EDtems por factor, o m\u00E1s casos por grupo pueden ayudar."), "</li>")
                else if (length(not_supported) > 0L)
                    paste0("<li>", private$.tr("Before comparing group means or scores, investigate partial invariance to find which specific items break the restriction, rather than abandoning the comparison or forcing full invariance.", "Antes de comparar medias o puntajes de grupo, investigue la invariancia parcial para encontrar qu\u00E9 \u00EDtems espec\u00EDficos rompen la restricci\u00F3n, en vez de abandonar la comparaci\u00F3n o forzar la invariancia completa."), "</li>")
                else if (length(discordant) > 0L)
                    paste0("<li>", private$.tr("At least one level is supported by only one criterion (LRT or \u0394CFI, not both) -- treat comparisons requiring that level with caution and consider investigating partial invariance before relying on them.", "Al menos un nivel est\u00E1 respaldado por solo un criterio (LRT o \u0394CFI, no ambos) -- trate con cautela las comparaciones que requieran ese nivel y considere investigar invariancia parcial antes de confiar en ellas."), "</li>")
                else
                    paste0("<li>", private$.tr("Group comparisons on this construct's means/scores are supported by this invariance sequence.", "Las comparaciones de grupo sobre las medias/puntajes de este constructo est\u00E1n respaldadas por esta secuencia de invariancia."), "</li>"),
                "</ul>",
                .fl_footnote(private$.tr("The \u0394CFI criterion follows Cheung &amp; Rensvold (2002); the invariance level requirements for mean/score comparisons follow standard SEM practice (e.g. Vandenberg &amp; Lance, 2000).", "El criterio \u0394CFI sigue a Cheung &amp; Rensvold (2002); los requisitos de nivel de invariancia para comparaciones de medias/puntajes siguen la pr\u00E1ctica est\u00E1ndar en SEM (p. ej. Vandenberg &amp; Lance, 2000).")),
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
                              title = private$.tr("Fit Across Invariance Levels", "Ajuste a Trav\u00E9s de los Niveles de Invariancia")) +
                ggtheme
            print(p)
            TRUE
        }
    )
)
