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

# Workflow / Flujo de trabajo:
# Validate (options, k >= 2 items) -> prepare (detect dichotomous/polytomous/
# ordinal level, build the complete-case data frame) -> describe missingness
# (n available vs. removed listwise) -> diagnose design (sample-size/subject-
# to-item screening, level-aware normality, tau-equivalence/unidimensionality/
# normality assumptions behind alpha) -> compute supported estimands (alpha,
# ordinal alpha, omega/omega hierarchical, GLB, split-half, Guttman lambda,
# KR-20/21 -- whichever apply to the detected level) -> quantify uncertainty
# (bootstrap CIs) -> interpret (alpha-vs-omega discordance, weak item-total
# correlations as a diagnostic flag, dimensionality) -> assemble report
# (tables, plots, interpretation panel).
# ES: Validar (opciones, k >= 2 ítems) -> preparar (detectar nivel
# dicotómico/politómico/ordinal, construir el data frame de casos completos)
# -> describir datos faltantes (n disponible vs. eliminado por lista) ->
# diagnosticar diseño (cribado de tamaño muestral/razón sujeto-ítem,
# normalidad consciente del nivel de medida, supuestos de tau-equivalencia/
# unidimensionalidad/normalidad detrás del alfa) -> calcular los estimandos
# soportados (alfa, alfa ordinal, omega/omega jerárquico, GLB, mitades
# partidas, lambda de Guttman, KR-20/21 -- los que apliquen al nivel
# detectado) -> cuantificar incertidumbre (IC por bootstrap) -> interpretar
# (discordancia alfa-vs-omega, correlaciones ítem-total débiles como alerta
# diagnóstica, dimensionalidad) -> ensamblar el informe (tablas, gráficos,
# panel de interpretación).
internalConsistencyClass <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "internalConsistencyClass",
    inherit = internalConsistencyBase,
    private = list(

        # ── Stored state for plot renderers ───────────────────────────────────
        .df_clean  = NULL,
        .citc_vals = NULL,
        .alpha_drop= NULL,
        .fa_result = NULL,
        .plot_rows = NULL,   # data.frame: coefficient, value, ci_lower, ci_upper

        # ── Translation helper ────────────────────────────────────────────────

        # Plot colours derived from jamovi's own theme -- see
        # .fl_plot_colors()' own definition in shared-helpers.R for why
        # (jamovi's official module review, 2026-09-16). .plot_theme() and
        # the plotStyle option it used to read are removed entirely; every
        # render function below uses the ggtheme jamovi already passes in.
        .plot_colors = function(theme) .fl_plot_colors(theme),

        # ── Interpretation of reliability coefficient ─────────────────────────
        .interp_rel = function(val) {
            if (is.na(val) || !is.finite(val)) return(.("N/A"))
            if (val >= .95) return(.("Excellent"))
            if (val >= .90) return(.("Good"))
            if (val >= .80) return(.("Acceptable"))
            if (val >= .70) return(.("Questionable"))
            if (val >= .60) return(.("Poor"))
            .("Unacceptable")
        },

        # ── Significance stars ────────────────────────────────────────────────
        .sig = function(p) {
            if (is.na(p) || !is.finite(p)) return("")
            if (p < .001) return("***")
            if (p < .01)  return("**")
            if (p < .05)  return("*")
            return("")
        },

        # ── Tau-equivalence test (Cronbach's α's core assumption) ────────────
        # EN: α equals the true reliability only when items are tau-
        # equivalent -- equal true-score loadings on a single common factor
        # (Cronbach, 1951; Zumbo, Gadermann & Zeisser, 2007). Formally
        # testable as a likelihood-ratio comparison between a congeneric
        # single-factor CFA (loadings free) and a tau-equivalent one
        # (loadings constrained equal): a significant difference rejects
        # tau-equivalence. Computed from each model's own chi-square/df
        # (lavaan::fitMeasures(), stable field names across versions) rather
        # than parsing lavaan::lavTestLRT()'s printed comparison table,
        # whose column names have changed between lavaan releases.
        # ES: El α equivale a la confiabilidad verdadera solo cuando los
        # ítems son tau-equivalentes -- iguales cargas de puntaje verdadero
        # sobre un único factor común (Cronbach, 1951; Zumbo, Gadermann &
        # Zeisser, 2007). Formalmente comprobable como una comparación de
        # razón de verosimilitud entre un AFC congenérico de un factor
        # (cargas libres) y uno tau-equivalente (cargas restringidas a ser
        # iguales): una diferencia significativa rechaza la tau-
        # equivalencia. Se calcula a partir del chi-cuadrado/gl propios de
        # cada modelo (lavaan::fitMeasures(), nombres de campo estables
        # entre versiones) en vez de parsear la tabla de comparación
        # impresa de lavaan::lavTestLRT(), cuyos nombres de columna han
        # cambiado entre versiones de lavaan.
        # EN: item_is_ordinal picks the estimator, matching Advanced
        # Reliability's own approach: WLSMV on polychoric/tetrachoric
        # correlations for ordinal or dichotomous items (declared via
        # ordered=), plain ML for genuinely continuous ones. Earlier
        # versions of this test always used ML regardless of the level
        # this module had itself just detected -- a real inconsistency
        # for a module whose stated design principle is measurement-
        # level-aware checks (the same fix already applied to this
        # module's own normality check, and to Advanced Reliability's CFA).
        # ES: item_is_ordinal elige el estimador, igual que el propio
        # enfoque de Advanced Reliability: WLSMV sobre correlaciones
        # policóricas/tetracóricas para ítems ordinales o dicotómicos
        # (declarados vía ordered=), ML simple para los genuinamente
        # continuos. Versiones anteriores de esta prueba siempre usaban ML
        # sin importar el nivel que este mismo módulo acababa de detectar
        # -- una inconsistencia real para un módulo cuyo principio de
        # diseño declarado es verificaciones conscientes del nivel de
        # medida (el mismo arreglo ya aplicado a la propia prueba de
        # normalidad de este módulo, y al AFC de Advanced Reliability).
        .tau_equivalence_test = function(df, item_is_ordinal = FALSE) {
            if (!requireNamespace("lavaan", quietly = TRUE))
                return(list(stat = NA_real_, df = NA_integer_, p = NA_real_, available = FALSE))
            # jamovi's official module review (2026-09-16) found that item
            # names with spaces, hyphens or accented characters break
            # lavaan's model-syntax parser when pasted directly into a
            # formula string -- the parse error was swallowed by the
            # tryCatch below and reported as "did not converge". df is a
            # local copy (R's own copy-on-modify semantics), so renaming
            # its columns here to jmvcore::toB64()'s safe encoding is fully
            # contained to this function; nothing downstream returns or
            # displays an item name, so no reverse mapping is needed here,
            # unlike advancedreliability.b.R's/measurementinvariance.b.R's
            # own use of the same .fl_safe_names() helper (shared-helpers.R).
            # ES: La revisión oficial de módulos de jamovi (2026-09-16)
            # encontró que nombres de ítem con espacios, guiones o
            # caracteres acentuados rompen el analizador de sintaxis de
            # modelos de lavaan al pegarse directamente en una cadena de
            # fórmula -- el error de análisis quedaba absorbido por el
            # tryCatch de abajo y se reportaba como "no convergió". df es
            # una copia local (semántica de copiar-al-modificar de R), así
            # que renombrar sus columnas aquí a la codificación segura de
            # jmvcore::toB64() queda totalmente contenido en esta función;
            # nada más abajo devuelve ni muestra un nombre de ítem, así que
            # no se necesita mapeo inverso aquí, a diferencia del propio
            # uso del mismo ayudante .fl_safe_names() (shared-helpers.R) en
            # advancedreliability.b.R/measurementinvariance.b.R.
            names(df) <- unname(jmvcore::toB64(names(df)))
            items <- names(df)
            syn_c <- paste0("f =~ ", paste(items, collapse = " + "))
            syn_t <- paste0("f =~ ", paste0("a*", items, collapse = " + "))
            fit_args_c <- list(model = syn_c, data = df, std.lv = TRUE)
            fit_args_t <- list(model = syn_t, data = df, std.lv = TRUE)
            if (item_is_ordinal) {
                fit_args_c$ordered <- items; fit_args_c$estimator <- "WLSMV"
                fit_args_t$ordered <- items; fit_args_t$estimator <- "WLSMV"
            }
            fit_c <- tryCatch(do.call(lavaan::cfa, fit_args_c), error = function(e) NULL)
            fit_t <- tryCatch(do.call(lavaan::cfa, fit_args_t), error = function(e) NULL)
            ok <- function(f) !is.null(f) && isTRUE(tryCatch(lavaan::lavInspect(f, "converged"), error = function(e) FALSE))
            if (!ok(fit_c) || !ok(fit_t))
                return(list(stat = NA_real_, df = NA_integer_, p = NA_real_, available = TRUE))
            # lavTestLRT() automatically applies the correct scaled
            # chi-square-difference correction under WLSMV (or MLR),
            # rather than hand-differencing chisq/df -- see
            # advancedreliability.b.R's own model-comparison LRT for the
            # same reasoning.
            lrt_out <- tryCatch(lavaan::lavTestLRT(fit_c, fit_t), error = function(e) NULL)
            if (!is.null(lrt_out) && nrow(lrt_out) >= 2L && all(c("Chisq diff", "Df diff", "Pr(>Chisq)") %in% names(lrt_out))) {
                d_chisq <- lrt_out[["Chisq diff"]][2]
                d_df    <- lrt_out[["Df diff"]][2]
                p       <- lrt_out[["Pr(>Chisq)"]][2]
            } else {
                fm_c <- lavaan::fitMeasures(fit_c, c("chisq", "df"))
                fm_t <- lavaan::fitMeasures(fit_t, c("chisq", "df"))
                d_chisq <- unname(fm_t["chisq"] - fm_c["chisq"])
                d_df    <- unname(fm_t["df"]    - fm_c["df"])
                p <- if (is.finite(d_chisq) && is.finite(d_df) && d_df > 0) stats::pchisq(d_chisq, d_df, lower.tail = FALSE) else NA_real_
            }
            if (!is.finite(d_chisq) || !is.finite(d_df) || d_df <= 0)
                return(list(stat = NA_real_, df = NA_integer_, p = NA_real_, available = TRUE))
            list(stat = d_chisq, df = d_df, p = p, available = TRUE)
        },

        # ── Bootstrap a scalar stat ───────────────────────────────────────────
        .bootstrap = function(df, stat_fn, B = 1000L) .fl_bootstrap(df, stat_fn, B),

        # ── Reset a Jamovi table to a requested number of rows ───────────────
        .reset_table = function(table, n_rows) .fl_reset_table(table, n_rows),

        # ── Main run ─────────────────────────────────────────────────────────
        .run = function() {
            opt <- self$options

            # ── 1. Validate ───────────────────────────────────────────────────
            items <- opt$items
            if (length(items) < 2) {
                self$results$autoDetectNote$setContent(
                    paste0("<p><b>", .("Select at least 2 items to compute reliability coefficients."), "</b></p>"))
                return()
            }

            # ── 2. Prepare data ───────────────────────────────────────────────
            df_raw <- self$data[, items, drop = FALSE]
            for (col in names(df_raw))
                df_raw[[col]] <- suppressWarnings(as.numeric(df_raw[[col]]))
            df <- na.omit(df_raw)
            n_miss  <- nrow(df_raw) - nrow(df)
            n       <- nrow(df)
            k       <- ncol(df)

            if (n < 5L) {
                self$results$autoDetectNote$setContent(paste0("<p><b>", .("Not enough complete cases (minimum 5 required)."), "</b></p>"))
                return()
            }
            private$.df_clean <- df

            # jamovi's official module review (2026-09-16) found every plot in
            # this file rendered blank on export: a plot's render function only
            # ever runs live, straight after .run(), OR later on its own, in a
            # separate re-created analysis instance that restores saved results
            # but never calls .run() again -- so a private$ field read there is
            # always NULL on that second path. self$results$<image>$setState()
            # is the only channel that survives into that second instance.
            # .plotItemDist draws one small bar chart per item from a frequency
            # table, not from the full cleaned data frame, so that's what goes
            # into state -- smaller, and exactly what the render function needs.
            # ES: La revisión oficial de módulos de jamovi (2026-09-16) encontró
            # que todo gráfico de este archivo se exportaba en blanco: la
            # función de render de un gráfico solo corre en vivo justo después
            # de .run(), O más tarde por su cuenta, en una instancia de análisis
            # separada que restaura los resultados guardados pero nunca vuelve a
            # llamar a .run() -- así que un campo private$ leído ahí siempre es
            # NULL en ese segundo camino. self$results$<image>$setState() es el
            # único canal que sobrevive a esa segunda instancia. .plotItemDist
            # dibuja un pequeño gráfico de barras por ítem a partir de una tabla
            # de frecuencias, no del data frame completo, así que eso es lo que
            # va al estado -- más pequeño, y exactamente lo que necesita la
            # función de render.
            item_freq <- lapply(seq_len(ncol(df)), function(j) {
                x   <- df[[j]]
                tbl <- as.data.frame(table(x), stringsAsFactors = FALSE)
                colnames(tbl) <- c("val", "freq")
                tbl$val <- as.numeric(tbl$val)
                list(name = names(df)[j], tbl = tbl)
            })
            self$results$plotItemDist$setState(item_freq)

            # ── 3. Detect measurement level ───────────────────────────────────
            n_unique <- sapply(df, function(x) length(unique(x)))
            n_opts   <- max(n_unique)          # max response options across items
            is_dichot_data <- all(n_unique <= 2L) &&
                              all(unlist(df) %in% c(0, 1, NA))

            level <- switch(opt$measureLevel,
                auto        = if (is_dichot_data) "dichotomous" else "polytomous",
                dichotomous = "dichotomous",
                polytomous  = "polytomous")

            # EN: Whether the polytomous items are ordinal (discrete, <=7
            # whole-number categories -- a typical Likert scale) rather
            # than genuinely continuous. This gates which normality check
            # runs below: Shapiro-Wilk assumes a continuous variable, and
            # running it on a handful of discrete categories mostly detects
            # the item's own discreteness/ties rather than a real
            # distributional problem -- a module whose purpose is checking
            # assumptions should not itself apply a continuous-data test to
            # ordinal data.
            # ES: Si los ítems politómicos son ordinales (discretos, <=7
            # categorías de números enteros -- una escala Likert típica) en
            # vez de genuinamente continuos. Esto determina qué prueba de
            # normalidad corre abajo: Shapiro-Wilk asume una variable
            # continua, y aplicarla a un puñado de categorías discretas
            # detecta mayormente la propia discreción/empates del ítem en
            # vez de un problema distribucional real -- un módulo cuyo
            # propósito es verificar supuestos no debería él mismo aplicar
            # una prueba para datos continuos a datos ordinales.
            is_ordinal_scale <- level == "polytomous" && .fl_is_low_cardinality(df)

            # ── 4. Auto-detect note ───────────────────────────────────────────
            detect_icon <- if (level == "dichotomous") "&#9679;" else "&#9632;"
            level_lbl   <- if (level == "dichotomous")
                .("Dichotomous (0/1)")
            else
                jmvcore::format(.("Polytomous ({n} response options)"), n = n_opts)

            auto_html <- .fl_prose(
                "<table style='border-collapse:collapse;'>",
                "<tr><td style='padding:4px 10px;'><b>", .("Items"), "</b></td><td>", k, "</td></tr>",
                "<tr><td style='padding:4px 10px;'><b>", .("Complete cases"), "</b></td><td>", n,
                if (n_miss > 0) paste0(" <span style='color:orange;'>(", n_miss, " ", .("removed listwise"), ")</span>") else "", "</td></tr>",
                "<tr><td style='padding:4px 10px;'><b>", .("Max. response options"), "</b></td><td>", n_opts, "</td></tr>",
                "<tr><td style='padding:4px 10px;'><b>", .("Detected level"), "</b></td><td>", detect_icon, " ", level_lbl, "</td></tr>",
                "</table>")
            self$results$autoDetectNote$setContent(auto_html)

            # ── 5. Sample adequacy ────────────────────────────────────────────
            ratio <- n / k
            n_warn <- if (n < 50)  .("⚠ Very small sample (n < 50). Estimates likely unstable.")
                     else if (n < 100) .("⚠ Small sample (n < 100). Interpret with caution.")
                     else if (n < 200) .("✓ Adequate sample (n ≥ 100). Suitable for preliminary research.")
                     else .("✓ Good sample size (n ≥ 200).")
            ratio_warn <- if (ratio < 5)  .("⚠ Subject-to-item ratio very low (< 5:1). Reliability estimates unreliable.")
                          else if (ratio < 10) .("⚠ Subject-to-item ratio low (5–10:1). Acceptable for pilot studies.")
                          else .("✓ Subject-to-item ratio adequate (≥ 10:1).")
            norm_note <- if (level == "polytomous" && !is_dichot_data)
                .("<b>Normality note:</b> If items deviate markedly from normality (|skew| > 2 or |kurt| > 7), prefer Ordinal Alpha or McDonald’s Omega over Cronbach’s Alpha.")
            else ""
            options_note <- if (n_opts == 2L)
                .("<b>2-category items detected.</b> KR-20 and KR-21 are preferred over Cronbach’s Alpha for dichotomous data.")
            else if (n_opts <= 4L)
                .("<b>Few response options (≤ 4).</b> Consider Ordinal Alpha; standard Alpha may underestimate reliability.")
            else ""
            samp_html <- .fl_prose(
                "<p style='font-size:0.85em;color:#666;'>", .("The n and subject-to-item thresholds below are rule-of-thumb screening guidance, not universal statistical criteria -- treat them as prompts to interpret estimates more cautiously, not as pass/fail cutoffs."),
                "</p>",
                "<ul style='line-height:1;'>",
                "<li>", n_warn, "</li>",
                "<li>", ratio_warn, "</li>",
                if (nzchar(norm_note))    paste0("<li>", norm_note, "</li>")    else "",
                if (nzchar(options_note)) paste0("<li>", options_note, "</li>") else "",
                "</ul>")
            self$results$sampleAdequacy$setContent(samp_html)

            # ── 6. Normality check (Shapiro-Wilk; continuous data only) ────
            # EN: Normality is a continuous-distribution concept, so the
            # entire check -- table, and every downstream mention of it --
            # only runs when the scale was actually detected/set as
            # continuous. Dichotomous items get no normality check because
            # the concept doesn't apply to a two-point (Bernoulli)
            # variable; ordinal (discrete Likert-type) items get none
            # either, for the same reason a Shapiro-Wilk test isn't run on
            # them even when this was applicable in an earlier version of
            # this module: it would mostly detect the item's own
            # discreteness/ties rather than a real distributional problem.
            # Item-level skew/kurtosis for ordinal items is still screened
            # elsewhere (the sample-adequacy note above), just not framed
            # here as a normality verdict.
            # ES: La normalidad es un concepto de distribución continua, así
            # que toda la verificación -- tabla, y cada mención posterior de
            # ella -- solo corre cuando la escala fue realmente detectada/
            # fijada como continua. Los ítems dicotómicos no reciben
            # verificación de normalidad porque el concepto no aplica a una
            # variable de dos puntos (Bernoulli); los ítems ordinales
            # (tipo Likert discretos) tampoco reciben ninguna, por la misma
            # razón por la que no se corre una prueba de Shapiro-Wilk sobre
            # ellos ni siquiera cuando esto era aplicable en una versión
            # anterior de este módulo: detectaría mayormente la propia
            # discreción/empates del ítem en vez de un problema
            # distribucional real. La asimetría/curtosis a nivel de ítem
            # para ítems ordinales igual se examina en otra parte (la nota
            # de adecuación muestral de arriba), solo que no se presenta
            # aquí como un veredicto de normalidad.
            nonnormal_count <- 0L
            normality_applicable <- opt$normality && level != "dichotomous" && !is_ordinal_scale
            if (normality_applicable) {
                norm_tab <- self$results$normalityTable
                private$.reset_table(norm_tab, k)
                for (j in seq_len(k)) {
                    x  <- df[[j]]
                    sk <- if (length(x) >= 3) {
                        m  <- mean(x); s <- sd(x)
                        n_x <- length(x)
                        sum(((x - m)/s)^3) / n_x
                    } else NA_real_
                    ku <- if (length(x) >= 4) {
                        m  <- mean(x); s <- sd(x)
                        n_x <- length(x)
                        sum(((x - m)/s)^4) / n_x - 3
                    } else NA_real_

                    sw <- if (length(x) >= 3 && length(x) <= 5000)
                        tryCatch(shapiro.test(x), error=function(e) NULL)
                    else NULL
                    W_val <- if (!is.null(sw)) sw$statistic[["W"]] else NA_real_
                    p_val <- if (!is.null(sw)) sw$p.value            else NA_real_
                    dec <- if (is.na(p_val)) .("–")
                           else if (p_val < .05) .("Non-normal")
                           else .("Normal")
                    if (!is.na(p_val) && p_val < .05) nonnormal_count <- nonnormal_count + 1L
                    norm_tab$setRow(rowNo = j, values = list(
                        item     = names(df)[j],
                        W        = W_val,
                        p        = p_val,
                        sig      = private$.sig(p_val),
                        skewness = sk,
                        kurtosis = ku,
                        decision = dec))
                }
            } else if (opt$normality && (level == "dichotomous" || is_ordinal_scale)) {
                self$results$normalityTable$setVisible(FALSE)
            }

            # ── 7. Item analysis (via psych::alpha) ───────────────────────────
            alpha_obj <- tryCatch(
                psych::alpha(df, warnings = FALSE),
                error = function(e) NULL)

            if (!is.null(alpha_obj)) {
                citc_vals  <- alpha_obj$item.stats[, "r.cor"]
                alpha_drop <- alpha_obj$alpha.drop[, "raw_alpha"]
                loadings   <- tryCatch({
                    fa_1 <- psych::fa(df, nfactors = 1L, rotate = "none",
                                      warnings = FALSE, fm = "minres")
                    as.numeric(fa_1$loadings)
                }, error = function(e) rep(NA_real_, k))

                private$.citc_vals <- citc_vals
                private$.alpha_drop<- alpha_drop

                # State for .plotItemTotal -- see the note at private$.df_clean's
                # assignment (step 2) for why this can't be a private$ field
                # read alone. citc_vals is in the same order as df's columns
                # (psych::alpha()'s item.stats preserves it), but can come back
                # SHORTER than ncol(df) -- psych::alpha() silently drops a
                # zero-variance item internally -- so this pads with NA the
                # same bounds-safe way itemTable's own row-filling does just
                # below, instead of assuming the lengths always match.
                # ES: Estado para .plotItemTotal -- ver la nota en la
                # asignación de private$.df_clean (paso 2) sobre por qué esto
                # no puede depender solo de leer un campo private$. citc_vals
                # está en el mismo orden que las columnas de df (item.stats de
                # psych::alpha() lo preserva), pero puede volver MÁS CORTO que
                # ncol(df) -- psych::alpha() descarta en silencio un ítem de
                # varianza cero internamente -- así que esto rellena con NA de
                # la misma forma segura por límites que usa el llenado de
                # filas de itemTable justo abajo, en vez de asumir que las
                # longitudes siempre coinciden.
                self$results$plotItemTotal$setState(data.frame(
                    item = names(df),
                    citc = vapply(seq_along(df), function(j)
                        if (j <= length(citc_vals)) as.numeric(citc_vals[j]) else NA_real_,
                        numeric(1)),
                    stringsAsFactors = FALSE))

                if (opt$itemAnalysis) {
                    it_tab <- self$results$itemTable
                    private$.reset_table(it_tab, k)
                    for (j in seq_len(k)) {
                        x  <- df[[j]]
                        sk <- if (length(x) >= 3) {
                            m <- mean(x); s <- sd(x); n_x <- length(x)
                            sum(((x - m)/s)^3) / n_x
                        } else NA_real_
                        it_tab$setRow(rowNo = j, values = list(
                            item       = names(df)[j],
                            mean       = mean(x, na.rm = TRUE),
                            sd         = sd(x, na.rm = TRUE),
                            skewness   = sk,
                            n_unique   = as.integer(n_unique[j]),
                            citc       = if (j <= length(citc_vals))  citc_vals[j]  else NA_real_,
                            alpha_drop = if (j <= length(alpha_drop)) alpha_drop[j] else NA_real_,
                            loading    = if (j <= length(loadings))   loadings[j]   else NA_real_))
                    }
                }
            }

            # ── 8. Compute all statistics → accumulate rows ───────────────────
            # Safe defaults so the interpretation engine (step 12) can always
            # reference these regardless of which coefficients the user enabled.
            oa <- ot <- oh <- glb_val <- NA_real_
            rows      <- list()   # each element: list(coefficient, value, interpretation, applicability, fn)
            boot_rows <- list()   # only those with opt-in bootstrap

            # max_b caps bootstrap replicates independently of the user's
            # bootstrapSamples setting, for statistics that refit an entire
            # model per replicate (e.g. omega's factor analysis) and would
            # otherwise multiply a ~1-second refit by up to 10,000 -- slow
            # enough in the real jamovi engine (not just a raw Rscript call)
            # to hit its resource watchdog and crash the engine process.
            # ES: max_b limita las réplicas de bootstrap independientemente
            # de bootstrapSamples, para estadísticos que reajustan un modelo
            # completo por réplica (p. ej. el análisis factorial del omega) y
            # que de otro modo multiplicarían un reajuste de ~1 segundo hasta
            # por 10,000 -- suficientemente lento en el motor real de jamovi
            # (no solo en una llamada directa a Rscript) como para activar su
            # vigilante de recursos y hacer caer el motor.
            add_stat <- function(name, val, interp_val, cond, fn = NULL, max_b = NULL) {
                rows[[length(rows) + 1L]] <<- list(
                    coefficient   = name,
                    value         = val,
                    interpretation= private$.interp_rel(interp_val),
                    applicability = cond)
                if (!is.null(fn))
                    boot_rows[[length(boot_rows) + 1L]] <<- list(name = name, fn = fn, val = val, max_b = max_b)
            }

            # helper: Cronbach's alpha from matrix
            .calc_alpha <- function(d) {
                kk <- ncol(d); vi <- apply(d, 2, var, na.rm=TRUE)
                vt <- var(rowSums(d, na.rm=TRUE), na.rm=TRUE)
                (kk/(kk-1)) * (1 - sum(vi)/vt)
            }

            # ── 8a. Cronbach's Alpha ──────────────────────────────────────────
            if (opt$alpha) {
                a_val <- if (!is.null(alpha_obj)) alpha_obj$total$raw_alpha else .calc_alpha(df)
                alpha_applicability <- if (level == "dichotomous")
                    .("Binary (0/1) items; mathematically equivalent to KR-20 for this case")
                    else if (is_ordinal_scale)
                    .("Ordinal/Likert items; assumes τ-equivalence -- see the Reliability Assumptions Check below")
                    else
                    .("Polytomous, continuous items; assumes τ-equivalence & normality")
                add_stat(
                    .("Cronbach's α"),
                    a_val, a_val,
                    alpha_applicability,
                    function(d) .calc_alpha(d))
            }

            # ── 8b. Ordinal Alpha ─────────────────────────────────────────────
            if (opt$ordinalAlpha) {
                oa <- tryCatch({
                    pc <- psych::polychoric(df, correct = 0)
                    kk <- ncol(df)
                    (kk/(kk-1)) * (1 - kk/sum(pc$rho))
                }, error = function(e) NA_real_)
                add_stat(
                    .("Ordinal α (polychoric)"),
                    oa, oa,
                    .("Ordinal/Likert items; does not require normality"))
            }

            # ── 8c. Dimensionality pre-check for Omega / Omega-h ──────────────
            # Omega hierarchical is only meaningful relative to a bifactor
            # model with real group factors beneath the general factor; a
            # fixed nfactors=1 makes omega_h collapse onto omega_total for
            # every scale. Parallel analysis (reused below for the
            # dimensionality note/scree plot) runs once here so both omega
            # coefficients fit the data's own suggested factor structure.
            fa_par <- NULL
            if ((opt$omega || opt$omegaHierarchical || opt$checkDimensionality || opt$plotScree) && k >= 3L) {
                fa_par <- tryCatch(
                    suppressWarnings(psych::fa.parallel(df, plot = FALSE, fa = "both")),
                    error = function(e) NULL)
                private$.fa_result <- fa_par
            }
            n_omega_factors <- if (!is.null(fa_par))
                max(1L, min(fa_par$nfact, max(1L, floor(k / 3))))
            else 1L

            # ── 8d. McDonald's Omega (total & hierarchical) ───────────────────
            omega_obj <- NULL
            if ((opt$omega || opt$omegaHierarchical) && k >= 3L) {
                omega_obj <- tryCatch(
                    psych::omega(df, nfactors = n_omega_factors, plot = FALSE),
                    error = function(e) NULL)
            }

            if (opt$omega && !is.null(omega_obj)) {
                ot <- omega_obj$omega.tot
                add_stat(
                    .("McDonald's ω (total)"),
                    ot, ot,
                    .("Polytomous; robust to non-normality; preferred over α"),
                    function(d) {
                        o <- suppressWarnings(suppressMessages(tryCatch(
                            psych::omega(d, nfactors = n_omega_factors, plot = FALSE),
                            error = function(e) NULL)))
                        if (is.null(o)) NA_real_ else o$omega.tot
                    },
                    max_b = 200L)
            }

            if (opt$omegaHierarchical && !is.null(omega_obj)) {
                # EN: With a single underlying factor, omega hierarchical
                # isn't a distinct quantity from omega total -- it's not
                # conceptually meaningful (psych::omega() itself warns
                # "Omega_h ... not meaningful with one factor"). Showing a
                # number here would present it as if it were real evidence
                # of a general-factor structure, when there's none to speak
                # of; N/A is the honest value.
                # ES: Con un único factor subyacente, el omega jerárquico no
                # es una cantidad distinta del omega total -- no es
                # conceptualmente significativo (el propio psych::omega()
                # advierte "Omega_h ... not meaningful with one factor").
                # Mostrar un número aquí lo presentaría como si fuera
                # evidencia real de una estructura de factor general, cuando
                # no hay ninguna; N/D es el valor honesto.
                if (n_omega_factors >= 2L) {
                    oh <- omega_obj$omega_h
                    cond_h <- .("Multidimensional scales; proportion of variance due to g-factor")
                } else {
                    oh <- NA_real_
                    cond_h <- .("N/A -- parallel analysis suggests 1 factor, so this is not conceptually distinct from ω total")
                }
                add_stat(
                    .("McDonald's ω hierarchical"),
                    oh, oh, cond_h)
            }

            # ── 8e. GLB ───────────────────────────────────────────────────────
            # psych::omega()'s result object has no $GLB field -- GLB is
            # computed by its own dedicated function, independently of the
            # omega fit above.
            if (opt$glb) {
                glb_val <- tryCatch(psych::glb.fa(df)$glb, error = function(e) NA_real_)
                add_stat(
                    .("GLB (Greatest Lower Bound)"),
                    glb_val, glb_val,
                    .("Best achievable lower-bound estimate; no distributional assumptions"))
            }

            # ── 8e. Split-half & Guttman Lambdas ─────────────────────────────
            split_obj <- if (opt$splitHalf || opt$guttman)
                tryCatch(psych::splitHalf(df, raw = TRUE, brute = FALSE),
                         error = function(e) NULL)
            else NULL

            if (opt$splitHalf && !is.null(split_obj)) {
                sb <- split_obj$meanr    # Spearman-Brown corrected mean
                # Average SB across all splits
                sb_mean <- tryCatch({
                    r_raw  <- split_obj$raw
                    sb_all <- 2*r_raw / (1 + r_raw)
                    mean(sb_all, na.rm=TRUE)
                }, error=function(e) NA_real_)
                add_stat(
                    .("Split-half (Spearman-Brown)"),
                    sb_mean, sb_mean,
                    .("Continuous items; highly sensitive to how items are split"))
            }

            if (opt$guttman && !is.null(split_obj)) {
                for (lam in c("lambda2","lambda3","lambda4","lambda5","lambda6")) {
                    val <- tryCatch(split_obj[[lam]], error=function(e) NA_real_)
                    if (!is.null(val) && !is.na(val)) {
                        lbl <- sub("lambda","λ", lam)
                        add_stat(
                            paste0("Guttman ", lbl),
                            val, val,
                            .("Model-free lower bound; robust alternative to α"))
                    }
                }
            }

            # ── 8f. KR-20 / KR-21 (dichotomous) ─────────────────────────────
            # EN: The formulas below treat every item value as a 0/1
            # "correct" indicator (p_i is a proportion, q_i = 1 - p_i is
            # meaningful only in [0,1]). If the user forces
            # measureLevel = "dichotomous" on data that merely has 2
            # categories but isn't coded 0/1 (e.g. 1/2, or a recoded
            # Likert subset), p_i and q_i stop being valid proportions
            # (q_i can even go negative) and the formulas would silently
            # produce a number with no real meaning. Verified here
            # instead of assumed.
            # ES: Las fórmulas de abajo tratan cada valor de ítem como un
            # indicador 0/1 de "correcto" (p_i es una proporción, q_i =
            # 1 - p_i solo tiene sentido en [0,1]). Si el usuario fuerza
            # measureLevel = "dichotomous" sobre datos que solo tienen 2
            # categorías pero no están codificados 0/1 (p. ej. 1/2, o un
            # subconjunto Likert recodificado), p_i y q_i dejan de ser
            # proporciones válidas (q_i puede incluso volverse negativo) y
            # las fórmulas producirían en silencio un número sin
            # significado real. Verificado aquí en vez de asumido.
            if (level == "dichotomous" || opt$measureLevel == "dichotomous") {
                is_binary_01 <- all(unlist(df) %in% c(0, 1))
                p_i     <- colMeans(df, na.rm = TRUE)
                q_i     <- 1 - p_i
                vt      <- var(rowSums(df, na.rm = TRUE), na.rm = TRUE)
                M_score <- mean(rowSums(df, na.rm = TRUE), na.rm = TRUE)

                if (opt$kr20 && !is_binary_01) {
                    add_stat("KR-20 (Kuder-Richardson)", NA_real_, NA_real_,
                        .("⚠ Not computed: items are not coded 0/1 (KR-20 requires genuinely binary items, not merely 2 categories)"))
                } else if (opt$kr20 && is.finite(vt) && vt > 0) {
                    kr20_val <- (k/(k-1)) * (1 - sum(p_i*q_i)/vt)
                    add_stat(
                        "KR-20 (Kuder-Richardson)",
                        kr20_val, kr20_val,
                        .("Dichotomous (0/1) items only; equivalent to α for binary data"),
                        function(d) {
                            p <- colMeans(d); q <- 1-p; vv <- var(rowSums(d))
                            kk <- ncol(d); (kk/(kk-1))*(1 - sum(p*q)/vv)
                        })
                }
                if (opt$kr21 && !is_binary_01) {
                    add_stat("KR-21", NA_real_, NA_real_,
                        .("⚠ Not computed: items are not coded 0/1 (KR-21 requires genuinely binary items, not merely 2 categories)"))
                } else if (opt$kr21 && is.finite(vt) && vt > 0) {
                    kr21_val <- (k/(k-1)) * (1 - (M_score*(k - M_score))/(k * vt))
                    add_stat(
                        "KR-21",
                        kr21_val, kr21_val,
                        .("Dichotomous items; assumes equal item difficulty (conservative estimate)"),
                        function(d) {
                            kk <- ncol(d); M <- mean(rowSums(d)); vv <- var(rowSums(d))
                            (kk/(kk-1))*(1 - (M*(kk-M))/(kk*vv))
                        })
                }
            }

            # ── 8g. Reliability assumptions check (tau-equivalence,
            # unidimensionality, normality) -- same purpose as interRater's
            # ICC assumptions check: α is the most commonly reported
            # coefficient and the most assumption-laden one, so testing
            # these formally (not just inferring tau-equivalence from an
            # α-vs-ω gap, as the discordance panel above already does)
            # lets the report say definitively whether α is trustworthy
            # here.
            # ES: Verificación de supuestos de confiabilidad (tau-
            # equivalencia, unidimensionalidad, normalidad) -- mismo
            # propósito que la verificación de supuestos del ICC en
            # interRater: el α es el coeficiente más reportado y el más
            # cargado de supuestos, así que probarlos formalmente (no solo
            # inferir la tau-equivalencia de una brecha α-vs-ω, como ya
            # hace el panel de discordancia de arriba) permite que el
            # reporte diga con certeza si el α es confiable aquí.
            if (opt$alpha && isTRUE(opt$checkReliabilityAssumptions) && k >= 3L) {
                tau <- private$.tau_equivalence_test(df, item_is_ordinal = is_ordinal_scale || level == "dichotomous")

                verdict <- function(p) {
                    if (is.null(p) || is.na(p)) .("N/A")
                    else if (p < .05) .("Violated")
                    else .("Met")
                }

                n_fact_assump <- if (!is.null(fa_par)) fa_par$nfact else NA_integer_
                uni_verdict <- if (is.na(n_fact_assump)) .("N/A")
                               else if (n_fact_assump <= 1L) .("Met")
                               else .("Violated")

                norm_verdict <- if (!normality_applicable) .("N/A")
                                else if (nonnormal_count > 0L) .("Violated")
                                else .("Met")
                norm_test_lbl <- if (!normality_applicable)
                    .("Not applicable (dichotomous/ordinal)")
                else
                    .("Shapiro-Wilk (items failing)")

                assum_rows <- list(
                    list(assumption = .("Tau-equivalence"),
                         test = .("CFA likelihood-ratio test"),
                         statistic = .fl_clean_na(tau$stat),
                         df = if (!is.na(tau$df)) as.character(tau$df) else "",
                         p_value = .fl_clean_na(tau$p),
                         verdict = if (!tau$available) .("lavaan not installed") else verdict(tau$p)),
                    list(assumption = .("Unidimensionality"),
                         test = .("Parallel analysis (factors suggested)"),
                         statistic = .fl_clean_na(as.numeric(n_fact_assump)),
                         df = "",
                         p_value = NA,
                         verdict = uni_verdict),
                    list(assumption = .("Normality of items"),
                         test = norm_test_lbl,
                         statistic = .fl_clean_na(as.numeric(nonnormal_count)),
                         df = "",
                         p_value = NA,
                         verdict = norm_verdict))

                rat <- self$results$reliabilityAssumptionsTable
                private$.reset_table(rat, length(assum_rows))
                for (i in seq_along(assum_rows)) rat$setRow(rowNo = i, values = assum_rows[[i]])

                tau_bad  <- tau$available && !is.na(tau$p) && tau$p < .05
                uni_bad  <- !is.na(n_fact_assump) && n_fact_assump > 1L
                norm_bad <- normality_applicable && nonnormal_count > 0L

                tau_estim_p <- if (is_ordinal_scale || level == "dichotomous")
                    .("The congeneric/tau-equivalent models below were fit with WLSMV on polychoric/tetrachoric correlations, matching how these items were detected/set (ordinal or dichotomous). ")
                    else ""
                note_html <- paste0(.fl_prose_open(),
                    "<p>", tau_estim_p, .("Cronbach's α is the most commonly reported reliability coefficient and also the most assumption-laden: it equals the true reliability only when items are tau-equivalent (equal true-score loadings on a single common factor) (Cronbach, 1951; Zumbo, Gadermann &amp; Zeisser, 2007). Unlike McDonald's ω, GLB, or the Guttman λ family above (none of which require tau-equivalence), α is biased -- usually downward -- whenever this assumption fails."), "</p>",
                    if (!tau$available) paste0("<p>⚠ ", .("The tau-equivalence test requires the lavaan package, which is not installed here -- install it for a direct statistical test; in the meantime, the discordance panel above (α vs. ω) is an indirect signal of the same thing."), "</p>")
                    else if (is.na(tau$p)) paste0("<p>⚠ ", .("The tau-equivalence models did not converge on this data -- no formal verdict available; rely on the discordance panel above instead."), "</p>")
                    else if (tau_bad) paste0("<p>⚠ <b>", .("Tau-equivalence violated"), ":</b> ",
                        .("a congeneric model (free item loadings) fits significantly better than a tau-equivalent model (equal loadings) (p &lt; .05). Items load unequally on the underlying factor, so α is likely biased here -- report McDonald's ω instead, which does not require this assumption."), "</p>")
                    else paste0("<p>✓ ", .("No evidence against tau-equivalence -- the congeneric and tau-equivalent models fit comparably well, so α's assumption looks reasonable here."), "</p>"),
                    "<p>", if (uni_bad) paste0("⚠ <b>", .("Unidimensionality violated"), ":</b> ",
                        jmvcore::format(.("parallel analysis suggests {n} factors. A single overall α mixes distinct dimensions into one number; see the Dimensionality Check panel below for what to do about it."),
                           n = n_fact_assump))
                        else paste0("✓ ", .("Parallel analysis supports a single underlying factor -- consistent with what a single overall α is meant to measure.")), "</p>",
                    "<p>", if (norm_bad) paste0("⚠ <b>", .("Item normality violated"), ":</b> ",
                        jmvcore::format(.("{n} item(s) fail Shapiro-Wilk (see the Item Normality table below); α's standard-error formula assumes multivariate normality, so treat its confidence interval as approximate and prefer Ordinal α or the bootstrap CI above."),
                           n = nonnormal_count))
                        else if (normality_applicable) paste0("✓ ", .("All items pass the normality check -- α is not at a distributional disadvantage here."))
                        else if (opt$normality) .("Not applicable to dichotomous or ordinal items -- normality is a continuous-distribution concept.")
                        else .("Normality testing is off (enable it in Item Analysis to check this)."),
                    "</p>",
                    .fl_prose_close())
                self$results$reliabilityAssumptionsNote$setContent(note_html)
            } else {
                self$results$reliabilityAssumptionsTable$setVisible(FALSE)
                self$results$reliabilityAssumptionsNote$setVisible(FALSE)
            }

            # ── 9. Fill main table ────────────────────────────────────────────
            main_tab <- self$results$mainTable
            private$.reset_table(main_tab, length(rows))
            for (i in seq_along(rows)) {
                main_tab$setRow(rowNo = i, values = rows[[i]][
                    c("coefficient","value","interpretation","applicability")])
            }

            # ── 10. Bootstrap CIs ─────────────────────────────────────────────
            ci_by_name <- list()
            if (opt$bootstrapCi && length(boot_rows) > 0L) {
                B       <- as.integer(opt$bootstrapSamples)
                bt_tab  <- self$results$bootstrapTable
                private$.reset_table(bt_tab, length(boot_rows))
                for (i in seq_along(boot_rows)) {
                    r  <- boot_rows[[i]]
                    B_i <- if (!is.null(r$max_b)) min(B, r$max_b) else B
                    ci <- private$.bootstrap(df, r$fn, B_i)
                    bt_tab$setRow(rowNo = i, values = list(
                        coefficient = r$name,
                        estimate    = r$val,
                        n_boot      = B_i,
                        ci_lower    = ci$lo,
                        ci_upper    = ci$hi,
                        se_boot     = ci$se))
                    ci_by_name[[r$name]] <- ci
                }
            }

            # ── 10b. Coefficient Comparison plot data ─────────────────────────
            # EN: Same idea as interRater's .plotComparison -- one point +
            # CI whisker per computed coefficient on the same 0-1 scale, so
            # an α-vs-ω (or GLB-vs-α) gap that reads as a sentence in the
            # discordance panel is immediately visible as two dots sitting
            # apart. Coefficients without a bootstrap CI (or with
            # bootstrapCi off) plot as a point with no visible whisker,
            # matching interRater's own fallback.
            # ES: Misma idea que .plotComparison de interRater -- un punto +
            # barra de error por coeficiente calculado en la misma escala
            # 0-1, para que una brecha α-vs-ω (o GLB-vs-α) que se lee como
            # una oración en el panel de discordancia sea inmediatamente
            # visible como dos puntos separados. Los coeficientes sin IC por
            # bootstrap (o con bootstrapCi desactivado) se grafican como un
            # punto sin barra visible, igual que el respaldo de interRater.
            plot_rows <- Filter(function(r) !is.na(r$value), rows)
            if (length(plot_rows) > 0L) {
                private$.plot_rows <- data.frame(
                    coefficient = vapply(plot_rows, function(r) r$coefficient, character(1)),
                    value       = vapply(plot_rows, function(r) r$value, numeric(1)),
                    ci_lower    = vapply(plot_rows, function(r) {
                        ci <- ci_by_name[[r$coefficient]]
                        if (is.null(ci) || is.na(ci$lo)) r$value else ci$lo
                    }, numeric(1)),
                    ci_upper    = vapply(plot_rows, function(r) {
                        ci <- ci_by_name[[r$coefficient]]
                        if (is.null(ci) || is.na(ci$hi)) r$value else ci$hi
                    }, numeric(1)),
                    stringsAsFactors = FALSE)
                # State for .plotComparison -- see the note at private$.df_clean's
                # assignment (step 2) for why. This data frame is already exactly
                # what the render function draws.
                # ES: Estado para .plotComparison -- ver la nota en la asignación
                # de private$.df_clean (paso 2). Este data frame ya es
                # exactamente lo que dibuja la función de render.
                self$results$plotComparison$setState(private$.plot_rows)
            } else {
                self$results$plotComparison$setVisible(FALSE)
            }

            # ── 11. Dimensionality (parallel analysis) ────────────────────────
            # Reuses the fa_par already computed in step 8c above (whenever
            # omega/omegaHierarchical/checkDimensionality triggered it); only
            # recomputes here if none of those requested it yet.
            if (opt$checkDimensionality && k >= 3L && n >= 20L) {
                if (is.null(fa_par)) {
                    fa_par <- tryCatch(
                        suppressWarnings(psych::fa.parallel(df, plot = FALSE, fa = "both")),
                        error = function(e) NULL)
                    private$.fa_result <- fa_par
                }

                if (!is.null(fa_par)) {
                    n_f <- fa_par$nfact
                    n_c <- fa_par$ncomp
                    if (n_f <= 1L && n_c <= 1L) {
                        dim_html <- paste0(
                            "<p>✓ <b>", .("Unidimensional structure suggested."), "</b> ",
                            jmvcore::format(.("Parallel analysis recommends {nf} factor(s) and {nc} component(s). Cronbach\u2019s \u03B1 and McDonald\u2019s \u03C9 are appropriate."),
                               nf = n_f, nc = n_c), "</p>")
                    } else {
                        dim_html <- paste0(
                            "<p>⚠ <b>",
                            jmvcore::format(.("Multidimensional structure detected: {nf} factor(s), {nc} component(s)."),
                               nf = n_f, nc = n_c),
                            "</b></p><p>",
                            .("Recommendation: (1) Compute Cronbach’s α per subscale separately. (2) Report Omega Hierarchical (ωₕ) for the total scale. (3) Overall α across all items may be misleading."),
                            "</p>")
                    }
                    self$results$dimensionalityNote$setContent(dim_html)
                }
            }

            # State for .plotScree -- see the note at private$.df_clean's
            # assignment (step 2) for why. fa_par may have been computed in
            # step 8c above (whenever omega/omegaHierarchical/checkDimensionality/
            # plotScree requested it) or just above in this step; whichever it
            # is, only the first 10 observed and simulated eigenvalues actually
            # get drawn, so that's what goes into state -- not the fa.parallel()
            # result object itself, which is much larger than the numbers it
            # contains.
            # ES: Estado para .plotScree -- ver la nota en la asignación de
            # private$.df_clean (paso 2). fa_par puede haberse calculado en el
            # paso 8c arriba (cuando omega/omegaHierarchical/checkDimensionality/
            # plotScree lo pidieron) o justo arriba en este paso; sea cual sea,
            # solo los primeros 10 autovalores observados y simulados realmente
            # se dibujan, así que eso es lo que va al estado -- no el objeto
            # resultado de fa.parallel() en sí, que es mucho más grande que los
            # números que contiene.
            if (!is.null(fa_par)) {
                ev_fa  <- fa_par$fa.values
                ev_sim <- fa_par$fa.sim
                n_fact <- min(length(ev_fa), 10L)
                self$results$plotScree$setState(list(
                    actual = ev_fa[seq_len(n_fact)],
                    simulated = if (is.matrix(ev_sim) && nrow(ev_sim) >= 1L && ncol(ev_sim) >= n_fact)
                        colMeans(ev_sim)[seq_len(n_fact)]
                    else if (!is.matrix(ev_sim) && length(ev_sim) >= n_fact)
                        ev_sim[seq_len(n_fact)]
                    else rep(1, n_fact)))
            }

            # ── 12. Interpretation & recommendations ──────────────────────────
            # EN: Data-reactive interpretation engine. Always answers four
            # questions grounded in THIS run's numbers, not generic templated
            # boilerplate: What happened? Why? What does it imply? What
            # should the researcher do now? References live exclusively in
            # the Bibliography module (topic: Classical Test Theory) -- this
            # report never duplicates a citation list.
            # ES: Motor de interpretación reactivo a los datos. Responde
            # siempre cuatro preguntas ancladas en los números de ESTA
            # corrida: ¿Qué pasó? ¿Por qué? ¿Qué implica? ¿Qué debe hacer el
            # investigador ahora? Las referencias viven exclusivamente en el
            # módulo Bibliography (tema: Teoría Clásica de los Tests).
            best_val   <- if (!is.null(alpha_obj)) alpha_obj$total$raw_alpha else NA_real_
            interp_lbl <- private$.interp_rel(best_val)

            citc_v  <- private$.citc_vals
            drop_v  <- private$.alpha_drop
            weak_idx <- if (!is.null(citc_v)) which(citc_v < .30) else integer(0)

            weak_html <- if (length(weak_idx) > 0L) {
                items_txt <- vapply(weak_idx, function(j) {
                    paste0("<code>", names(df)[j], "</code> (r = ", round(citc_v[j], 3),
                           if (!is.null(drop_v) && j <= length(drop_v))
                               paste0(", α ", .("if removed"), " = ", round(drop_v[j], 3))
                           else "",
                           ")")
                }, character(1))
                paste0("<p>⚠ <b>", .("Weak item-total correlation(s)"), ":</b> ",
                       .("the following item(s) fall below the r(item-total) ≥ .30 screening threshold: "),
                       paste(items_txt, collapse = "; "), ". ",
                       .("This is a diagnostic flag, not a retention rule: a low item-total correlation can come from reverse-scoring that was never recoded, multidimensionality, deliberately heterogeneous content, or an item that genuinely doesn't function as intended. Inspect the item's content and scoring direction, and consider its theoretical role in the construct, before deciding to revise, recode, or remove it — the item-analysis table above shows α if each item were dropped only as one input to that judgment, not as the criterion itself."),
                       "</p>")
            } else {
                paste0("<p>✓ ", .("No items fall below the r(item-total) ≥ .30 threshold."), "</p>")
            }

            # ── Discordance panel (standalone result, matching interRater's
            # own discordanceNote) ────────────────────────────────────────────
            discord_html <- if (!is.na(best_val) && !is.na(ot) && abs(best_val - ot) > .04) {
                paste0("<p>⚠ <b>", .("Coefficients disagree"), ":</b> ",
                       jmvcore::format(.("Cronbach's α ({alpha}) and McDonald's ω ({omega}) differ by more than .04. α assumes equal item loadings (τ-equivalence); "),
                          alpha = round(best_val,3), omega = round(ot,3)),
                       .("this scale's items likely load unequally on the underlying factor, so α is probably biased here — trust ω instead."),
                       "</p>")
            } else if (!is.na(best_val) && !is.na(ot)) {
                paste0("<p>✓ ", .("Cronbach's α and McDonald's ω agree closely — the τ-equivalence assumption behind α looks reasonable here."), "</p>")
            } else ""
            self$results$discordanceNote$setContent(if (nzchar(discord_html)) .fl_prose(discord_html) else discord_html)
            if (!nzchar(discord_html)) self$results$discordanceNote$setVisible(FALSE)

            normality_html <- if (normality_applicable && k > 0L) {
                if (nonnormal_count > 0L) {
                    paste0("<p>⚠ ",
                           jmvcore::format(.("{n1} of {n2} item(s) fail the Shapiro-Wilk normality test (p < .05). "),
                              n1 = nonnormal_count, n2 = k),
                           jmvcore::format(.("Prefer {alt} over raw Cronbach's α for the primary estimate."),
                              alt = if (!is.na(oa)) .("Ordinal α") else .("McDonald's ω")),
                           "</p>")
                } else {
                    paste0("<p>✓ ", .("All items pass the normality check — Cronbach's α is not at a distributional disadvantage here."), "</p>")
                }
            } else ""

            dim_summary_html <- if (!is.null(fa_par)) {
                if (n_omega_factors >= 2L) {
                    paste0("<p>⚠ ",
                           jmvcore::format(.("Parallel analysis suggests {n} factor(s) — this scale is likely multidimensional. A single overall α/ω may blend distinct dimensions; consider reporting reliability per subscale."),
                              n = fa_par$nfact),
                           "</p>")
                } else {
                    paste0("<p>✓ ", .("Parallel analysis suggests a single (unidimensional) factor — a single overall reliability estimate is appropriate."), "</p>")
                }
            } else ""

            action_items <- character(0)
            if (length(weak_idx) > 0L)
                action_items <- c(action_items, jmvcore::format(
                    .("Inspect the content, scoring direction, and theoretical role of: {items} -- only revise or remove after that substantive review."),
                    items = paste(names(df)[weak_idx], collapse = ", ")))
            if (!is.na(best_val) && !is.na(ot) && abs(best_val - ot) > .04)
                action_items <- c(action_items, .("Report McDonald's ω as the primary coefficient, not α."))
            if (opt$normality && nonnormal_count > 0L && !is.na(oa))
                action_items <- c(action_items, .("Report Ordinal α alongside α given the non-normal items."))
            if (!is.null(fa_par) && n_omega_factors >= 2L)
                action_items <- c(action_items, .("Compute reliability per subscale in addition to the overall estimate."))
            if (n < 200L)
                action_items <- c(action_items, jmvcore::format(
                    .("n = {n} is below the n ≥ 200 rule of thumb for a stable estimate; treat the CI above as wide."),
                    n = n))
            action_html <- if (length(action_items) > 0L)
                paste0("<ul style='line-height:1;'>",
                       paste0("<li>", action_items, "</li>", collapse = ""), "</ul>")
            else
                paste0("<p>", .("No specific corrective action indicated — the primary estimate can be reported as-is."), "</p>")

            # .fl_prose_open()/.fl_prose_close() (shared-helpers.R) wrap this
            # whole panel in Bibliography's own typographic convention (no
            # font-size override -- inherits jamovi's default, same as
            # Bibliography and the Fiability Library -- plus line-height: 1
            # and text-align: justify) so all four Html-producing modules
            # render body text identically.
            # ES: .fl_prose_open()/.fl_prose_close() (shared-helpers.R)
            # envuelven todo este panel en la misma convención tipográfica
            # de Bibliography (sin sobreescribir font-size -- hereda el
            # predeterminado de jamovi, igual que Bibliography y la
            # Fiability Library -- más line-height: 1 y text-align: justify)
            # para que los cuatro módulos que producen Html rendericen el
            # texto corrido de forma idéntica.
            rec_html <- paste0(
                .fl_prose_open(),
                "<h4>", .("What happened"), "</h4>",
                "<p>", jmvcore::format(
                    .("The primary reliability estimate for this scale is <b>{val}</b> ({lbl})."),
                    val = round(best_val, 3), lbl = interp_lbl), "</p>",

                "<h4>", .("Why"), "</h4>",
                weak_html, normality_html, dim_summary_html,

                "<h4>", .("What it means"), "</h4>",
                "<p>", .("See the Reliability Coefficients and Item Analysis tables above for the exact numbers behind each point below; the Fiability Library (Coefficients section) documents each coefficient's assumptions and formula in full."),
                "</p>",
                "<p style='margin-top:0.8em;font-weight:700;'>", .("Interpretation Benchmarks"), "</p>",
                "<table style='border-collapse:collapse;'>",
                "<tr><th style='padding:3px 8px;border:1px solid #ccc;'>", .("Value"), "</th>",
                "<th style='padding:3px 8px;border:1px solid #ccc;'>", .("Interpretation"), "</th></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>≥ .95</td><td style='padding:3px 8px;border:1px solid #ccc;'>", .("Excellent"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>.90 – .94</td><td style='padding:3px 8px;border:1px solid #ccc;'>", .("Good"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>.80 – .89</td><td style='padding:3px 8px;border:1px solid #ccc;'>", .("Acceptable"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>.70 – .79</td><td style='padding:3px 8px;border:1px solid #ccc;'>", .("Questionable"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>.60 – .69</td><td style='padding:3px 8px;border:1px solid #ccc;'>", .("Poor"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>< .60</td><td style='padding:3px 8px;border:1px solid #ccc;'>", .("Unacceptable"), "</td></tr>",
                "</table>",

                "<h4>", .("What to do now"), "</h4>",
                action_html,

                "<p style='font-size:0.85em;color:#666;'>", .("See Fiability Library → Coefficients for full definitions, assumptions and references (Bibliography → Classical Test Theory)."),
                "</p>",
                "</div>")
            self$results$interpretation$setContent(rec_html)
        },

        # ── Plot: coefficient comparison (forest-plot style) ────────────────
        # Reads image$state, not a private$ field -- jamovi's image-export
        # path re-creates the analysis instance and calls this render
        # function directly, without calling .run() again first, so a
        # private$ field would always be NULL there. See the note at
        # private$.df_clean's assignment in .run() (step 2) for the full
        # explanation. Same construction as interRater's .plotComparison.
        .plotComparison = function(image, ggtheme, theme, ...) {
            d <- image$state
            if (is.null(d) || nrow(d) == 0L) return(FALSE)
            cols <- private$.plot_colors(theme)
            d$coefficient <- factor(d$coefficient, levels = rev(d$coefficient))
            lo <- min(0, d$ci_lower, na.rm = TRUE)
            hi <- max(1, d$ci_upper, na.rm = TRUE)
            p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = coefficient)) +
                ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                                       width = .15, orientation = "y", colour = cols$primary, linewidth = .6) +
                ggplot2::geom_point(size = 3.2, colour = cols$primary) +
                ggplot2::coord_cartesian(xlim = c(lo, hi)) +
                ggplot2::labs(x = .("Value (95% CI)"), y = NULL,
                              title = .("Coefficient Comparison")) +
                ggtheme
            print(p)
            TRUE
        },

        # ── Plot: item score distributions ────────────────────────────────────
        # Reads image$state (a list of per-item frequency tables), not
        # private$.df_clean -- see the note on .plotComparison above.
        .plotItemDist = function(image, ggtheme, theme, ...) {
            item_freq <- image$state
            if (is.null(item_freq)) return(FALSE)
            k   <- length(item_freq)
            nc  <- min(k, 4L)
            nr  <- ceiling(k / nc)
            cols <- private$.plot_colors(theme)

            plots <- lapply(item_freq, function(it) {
                ggplot2::ggplot(it$tbl, ggplot2::aes(x = val, y = freq)) +
                    ggplot2::geom_bar(stat = "identity", fill = cols$primary, alpha = .85, width = .6) +
                    ggplot2::labs(title = it$name, x = NULL, y = NULL) +
                    ggtheme +
                    ggplot2::theme(plot.title = ggplot2::element_text(size = 9, face = "bold"),
                                   axis.text  = ggplot2::element_text(size = 8))
            })
            if (requireNamespace("gridExtra", quietly = TRUE)) {
                # EN: arrangeGrob() returns a gtable, not a ggplot -- print()
                # on it just prints its structure as text (this was a real
                # bug: the item-distribution plot rendered as a blank panel
                # in jamovi, since nothing ever drew it). grid.draw() is the
                # correct way to render a gtable/grob onto the active
                # graphics device.
                # ES: arrangeGrob() retorna un gtable, no un ggplot -- print()
                # sobre él solo imprime su estructura como texto (esto era un
                # bug real: el gráfico de distribución de ítems se
                # renderizaba como un panel en blanco en jamovi, ya que nada
                # lo dibujaba nunca). grid.draw() es la forma correcta de
                # dibujar un gtable/grob en el dispositivo gráfico activo.
                p <- gridExtra::arrangeGrob(grobs = plots, ncol = nc)
                grid::grid.draw(p)
            } else {
                print(plots[[1]])
            }
            TRUE
        },

        # ── Plot: item–total correlations ─────────────────────────────────────
        # Reads image$state, not private$.citc_vals/.df_clean -- see the note
        # on .plotComparison above.
        .plotItemTotal = function(image, ggtheme, theme, ...) {
            dat <- image$state
            if (is.null(dat)) return(FALSE)

            cols <- private$.plot_colors(theme)
            dat$item <- factor(dat$item, levels = rev(dat$item))
            # A fixed TRUE/FALSE colour distinction (above/below the .30
            # threshold), not a user-configurable "colour by group" -- the
            # kind of always-on manual scale jamovi's own plot-theme guide
            # says to add AFTER `+ ggtheme`, since ggtheme's own discrete
            # scale would otherwise silently win if it came later in the
            # `+` chain (a sibling module, AssumptionsLab, had exactly this
            # bug slip through in its own plot-theme migration -- see
            # feedback-check-fiabilitylab-same-bugs in project memory).
            # ES: Una distinción de color VERDADERO/FALSO fija (por encima/
            # debajo del umbral .30), no un "colorear por grupo"
            # configurable por el usuario -- el tipo de escala manual
            # siempre activa que la propia guía de temas de gráficos de
            # jamovi dice agregar DESPUÉS de `+ ggtheme`, ya que la propia
            # escala discreta de ggtheme ganaría en silencio si apareciera
            # después en la cadena de `+` (un módulo hermano,
            # AssumptionsLab, tuvo exactamente este bug pasar
            # desapercibido en su propia migración de tema de gráficos --
            # ver feedback-check-fiabilitylab-same-bugs en la memoria del
            # proyecto).
            p <- ggplot2::ggplot(dat, ggplot2::aes(x = item, y = citc,
                                                    fill = citc >= .30)) +
                ggplot2::geom_bar(stat = "identity", width = .6) +
                ggplot2::geom_hline(yintercept = .30, linetype = "dashed",
                                    colour = cols$secondary, linewidth = .6) +
                ggplot2::coord_flip() +
                ggplot2::labs(x = NULL,
                              y = .("Corrected item-total r"),
                              title = .("Item–Total Correlations (threshold = .30)")) +
                ggtheme +
                ggplot2::scale_fill_manual(values = c("FALSE" = cols$secondary,
                                                       "TRUE"  = cols$primary),
                                           guide = "none")
            print(p)
            TRUE
        },

        # ── Plot: scree (parallel analysis) ──────────────────────────────────
        # Reads image$state (the first 10 observed/simulated eigenvalues,
        # already resolved for the matrix-vs-vector fa.sim shape below), not
        # private$.fa_result/.df_clean -- see the note on .plotComparison
        # above. No live re-run fallback is needed or possible here: on the
        # export path .run() never executes again, so there is no fresh data
        # to re-run fa.parallel() on even if this function tried to.
        .plotScree = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)

            n_fact <- length(st$actual)
            cols   <- private$.plot_colors(theme)
            dat <- data.frame(
                factor    = seq_len(n_fact),
                actual    = st$actual,
                simulated = st$simulated)

            # Fixed Actual/Simulated colour distinction, not a
            # user-configurable one -- see the note on .plotItemTotal
            # above for why this always-on manual scale goes AFTER
            # `+ ggtheme`.
            # ES: Distinción de color fija Actual/Simulado, no una
            # configurable por el usuario -- ver la nota en .plotItemTotal
            # arriba sobre por qué esta escala manual siempre activa va
            # DESPUÉS de `+ ggtheme`.
            p <- ggplot2::ggplot(dat, ggplot2::aes(x = factor)) +
                ggplot2::geom_line(ggplot2::aes(y = actual,    colour = "Actual"),    linewidth = 1) +
                ggplot2::geom_point(ggplot2::aes(y = actual,   colour = "Actual"),    size = 2.5) +
                ggplot2::geom_line(ggplot2::aes(y = simulated, colour = "Simulated"), linewidth = .7, linetype = "dashed") +
                ggplot2::labs(
                    x     = .("Factor"),
                    y     = .("Eigenvalue"),
                    title = .("Scree Plot — Parallel Analysis")) +
                ggtheme +
                ggplot2::scale_colour_manual(
                    name   = NULL,
                    values = c("Actual" = cols$primary, "Simulated" = cols$secondary))
            print(p)
            TRUE
        }
    )
)
