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
        .tr = function(en, es) .fl_tr(en, es, self$options$reportLang),

        # User-selectable plot style (independent of jamovi's own light/
        # dark theme, which is what the render functions' own `ggtheme`
        # argument adapts to) -- lets the report's plots match whatever
        # house style a manuscript/thesis needs. Same option/helper as
        # interRater's .plot_theme(), for consistency across the module.
        .plot_theme = function() .fl_plot_theme(self$options$plotStyle),
        .plot_colors = function() .fl_plot_colors(self$options$plotStyle),

        # ── Interpretation of reliability coefficient ─────────────────────────
        .interp_rel = function(val) .fl_interp_rel(val, private$.tr),

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
            tr  <- private$.tr
            opt <- self$options

            # ── 1. Validate ───────────────────────────────────────────────────
            items <- opt$items
            if (length(items) < 2) {
                self$results$autoDetectNote$setContent(
                    paste0("<p><b>", tr(
                        "Select at least 2 items to compute reliability coefficients.",
                        "Seleccione al menos 2 ítems para calcular coeficientes de confiabilidad."
                    ), "</b></p>"))
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
                self$results$autoDetectNote$setContent(paste0("<p><b>", tr(
                    "Not enough complete cases (minimum 5 required).",
                    "No hay suficientes casos completos (mínimo 5 requeridos)."
                ), "</b></p>"))
                return()
            }
            private$.df_clean <- df

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
                tr("Dichotomous (0/1)", "Dicotómico (0/1)")
            else
                tr(paste0("Polytomous (", n_opts, " response options)"),
                   paste0("Politómico (", n_opts, " opciones de respuesta)"))

            auto_html <- .fl_prose(
                "<table style='border-collapse:collapse;'>",
                "<tr><td style='padding:4px 10px;'><b>", tr("Items", "Ítems"), "</b></td><td>", k, "</td></tr>",
                "<tr><td style='padding:4px 10px;'><b>", tr("Complete cases", "Casos completos"), "</b></td><td>", n,
                if (n_miss > 0) paste0(" <span style='color:orange;'>(", n_miss, " ", tr("removed listwise","eliminados listwise"), ")</span>") else "", "</td></tr>",
                "<tr><td style='padding:4px 10px;'><b>", tr("Max. response options","Máx. opciones"), "</b></td><td>", n_opts, "</td></tr>",
                "<tr><td style='padding:4px 10px;'><b>", tr("Detected level","Nivel detectado"), "</b></td><td>", detect_icon, " ", level_lbl, "</td></tr>",
                "</table>")
            self$results$autoDetectNote$setContent(auto_html)

            # ── 5. Sample adequacy ────────────────────────────────────────────
            ratio <- n / k
            n_warn <- if (n < 50)  tr("&#9888; Very small sample (n < 50). Estimates likely unstable.",
                                      "&#9888; Muestra muy pequeña (n < 50). Estimaciones probablemente inestables.")
                     else if (n < 100) tr("&#9888; Small sample (n < 100). Interpret with caution.",
                                          "&#9888; Muestra pequeña (n < 100). Interprete con precaución.")
                     else if (n < 200) tr("&#10003; Adequate sample (n \u2265 100). Suitable for preliminary research.",
                                          "&#10003; Muestra adecuada (n \u2265 100). Apta para investigación preliminar.")
                     else tr("&#10003; Good sample size (n \u2265 200).",
                             "&#10003; Buen tamaño de muestra (n \u2265 200).")
            ratio_warn <- if (ratio < 5)  tr("&#9888; Subject-to-item ratio very low (< 5:1). Reliability estimates unreliable.",
                                              "&#9888; Razón sujetos/ítems muy baja (< 5:1). Estimaciones poco confiables.")
                          else if (ratio < 10) tr("&#9888; Subject-to-item ratio low (5\u201310:1). Acceptable for pilot studies.",
                                                   "&#9888; Razón sujetos/ítems baja (5\u201310:1). Aceptable para estudios piloto.")
                          else tr("&#10003; Subject-to-item ratio adequate (\u2265 10:1).",
                                  "&#10003; Razón sujetos/ítems adecuada (\u2265 10:1).")
            norm_note <- if (level == "polytomous" && !is_dichot_data)
                tr("<b>Normality note:</b> If items deviate markedly from normality (|skew| > 2 or |kurt| > 7), prefer Ordinal Alpha or McDonald\u2019s Omega over Cronbach\u2019s Alpha.",
                   "<b>Nota de normalidad:</b> Si los ítems se desvían notablemente de la normalidad (|asimetría| > 2 o |curtosis| > 7), prefiera el Alfa Ordinal o el Omega de McDonald en lugar del Alfa de Cronbach.")
            else ""
            options_note <- if (n_opts == 2L)
                tr("<b>2-category items detected.</b> KR-20 and KR-21 are preferred over Cronbach\u2019s Alpha for dichotomous data.",
                   "<b>Ítems con 2 categorías detectados.</b> KR-20 y KR-21 son preferibles al Alfa de Cronbach para datos dicotómicos.")
            else if (n_opts <= 4L)
                tr("<b>Few response options (\u2264 4).</b> Consider Ordinal Alpha; standard Alpha may underestimate reliability.",
                   "<b>Pocas opciones de respuesta (\u2264 4).</b> Considere Alfa Ordinal; el Alfa estándar puede subestimar la confiabilidad.")
            else ""
            samp_html <- .fl_prose(
                "<p style='font-size:0.85em;color:#666;'>", tr(
                    "The n and subject-to-item thresholds below are rule-of-thumb screening guidance, not universal statistical criteria -- treat them as prompts to interpret estimates more cautiously, not as pass/fail cutoffs.",
                    "Los umbrales de n y de razón sujetos/ítems de abajo son orientación de cribado basada en reglas empíricas, no criterios estadísticos universales -- trátelos como una señal para interpretar las estimaciones con más cautela, no como puntos de corte de aprobado/reprobado."),
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
                    dec <- if (is.na(p_val)) tr("–","–")
                           else if (p_val < .05) tr("Non-normal", "No normal")
                           else tr("Normal", "Normal")
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
                    tr("Binary (0/1) items; mathematically equivalent to KR-20 for this case",
                       "Ítems binarios (0/1); matemáticamente equivalente al KR-20 en este caso")
                    else if (is_ordinal_scale)
                    tr("Ordinal/Likert items; assumes τ-equivalence -- see the Reliability Assumptions Check below",
                       "Ítems ordinales/Likert; asume equivalencia-τ -- vea la Verificación de Supuestos de Confiabilidad abajo")
                    else
                    tr("Polytomous, continuous items; assumes τ-equivalence & normality",
                       "Ítems politómicos/continuos; asume equivalencia-τ y normalidad")
                add_stat(
                    tr("Cronbach's \u03B1",          "Alfa de Cronbach (\u03B1)"),
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
                    tr("Ordinal \u03B1 (polychoric)", "Alfa Ordinal (policórica)"),
                    oa, oa,
                    tr("Ordinal/Likert items; does not require normality",
                       "Ítems ordinales/Likert; no requiere normalidad"))
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
                    tr("McDonald's \u03C9 (total)", "Omega de McDonald (\u03C9 total)"),
                    ot, ot,
                    tr("Polytomous; robust to non-normality; preferred over \u03B1",
                       "Politómico; robusto a no normalidad; preferible al \u03B1"),
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
                    cond_h <- tr("Multidimensional scales; proportion of variance due to g-factor",
                                 "Escalas multidimensionales; proporción de varianza del factor g")
                } else {
                    oh <- NA_real_
                    cond_h <- tr("N/A -- parallel analysis suggests 1 factor, so this is not conceptually distinct from ω total",
                                 "N/D -- el análisis paralelo sugiere 1 factor, así que esto no es conceptualmente distinto del ω total")
                }
                add_stat(
                    tr("McDonald\'s ω hierarchical", "Omega Jerárquico (ωₕ)"),
                    oh, oh, cond_h)
            }

            # ── 8e. GLB ───────────────────────────────────────────────────────
            # psych::omega()'s result object has no $GLB field -- GLB is
            # computed by its own dedicated function, independently of the
            # omega fit above.
            if (opt$glb) {
                glb_val <- tryCatch(psych::glb.fa(df)$glb, error = function(e) NA_real_)
                add_stat(
                    tr("GLB (Greatest Lower Bound)", "Límite Inferior Máximo (GLB)"),
                    glb_val, glb_val,
                    tr("Best achievable lower-bound estimate; no distributional assumptions",
                       "Mejor cota inferior alcanzable; sin supuestos distribucionales"))
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
                    tr("Split-half (Spearman-Brown)", "Mitades partidas (Spearman-Brown)"),
                    sb_mean, sb_mean,
                    tr("Continuous items; highly sensitive to how items are split",
                       "Ítems continuos; muy sensible a cómo se dividen los ítems"))
            }

            if (opt$guttman && !is.null(split_obj)) {
                for (lam in c("lambda2","lambda3","lambda4","lambda5","lambda6")) {
                    val <- tryCatch(split_obj[[lam]], error=function(e) NA_real_)
                    if (!is.null(val) && !is.na(val)) {
                        lbl <- sub("lambda","λ", lam)
                        add_stat(
                            tr(paste0("Guttman ", lbl), paste0("Guttman ", lbl)),
                            val, val,
                            tr("Model-free lower bound; robust alternative to \u03B1",
                               "Cota inferior sin modelo; alternativa robusta al \u03B1"))
                    }
                }
            }

            # ── 8f. KR-20 / KR-21 (dichotomous) ─────────────────────────────
            if (level == "dichotomous" || opt$measureLevel == "dichotomous") {
                p_i     <- colMeans(df, na.rm = TRUE)
                q_i     <- 1 - p_i
                vt      <- var(rowSums(df, na.rm = TRUE), na.rm = TRUE)
                M_score <- mean(rowSums(df, na.rm = TRUE), na.rm = TRUE)

                if (opt$kr20 && is.finite(vt) && vt > 0) {
                    kr20_val <- (k/(k-1)) * (1 - sum(p_i*q_i)/vt)
                    add_stat(
                        "KR-20 (Kuder-Richardson)",
                        kr20_val, kr20_val,
                        tr("Dichotomous (0/1) items only; equivalent to \u03B1 for binary data",
                           "Solo ítems dicotómicos (0/1); equivalente al \u03B1 para datos binarios"),
                        function(d) {
                            p <- colMeans(d); q <- 1-p; vv <- var(rowSums(d))
                            kk <- ncol(d); (kk/(kk-1))*(1 - sum(p*q)/vv)
                        })
                }
                if (opt$kr21 && is.finite(vt) && vt > 0) {
                    kr21_val <- (k/(k-1)) * (1 - (M_score*(k - M_score))/(k * vt))
                    add_stat(
                        "KR-21",
                        kr21_val, kr21_val,
                        tr("Dichotomous items; assumes equal item difficulty (conservative estimate)",
                           "Ítems dicotómicos; asume dificultad igual en todos (estimación conservadora)"),
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
                    if (is.null(p) || is.na(p)) tr("N/A", "N/D")
                    else if (p < .05) tr("Violated", "Violado")
                    else tr("Met", "Cumplido")
                }

                n_fact_assump <- if (!is.null(fa_par)) fa_par$nfact else NA_integer_
                uni_verdict <- if (is.na(n_fact_assump)) tr("N/A", "N/D")
                               else if (n_fact_assump <= 1L) tr("Met", "Cumplido")
                               else tr("Violated", "Violado")

                norm_verdict <- if (!normality_applicable) tr("N/A", "N/D")
                                else if (nonnormal_count > 0L) tr("Violated", "Violado")
                                else tr("Met", "Cumplido")
                norm_test_lbl <- if (!normality_applicable)
                    tr("Not applicable (dichotomous/ordinal)", "No aplica (dicotómico/ordinal)")
                else
                    tr("Shapiro-Wilk (items failing)", "Shapiro-Wilk (ítems que fallan)")

                assum_rows <- list(
                    list(assumption = tr("Tau-equivalence", "Tau-equivalencia"),
                         test = tr("CFA likelihood-ratio test", "Prueba de razón de verosimilitud AFC"),
                         statistic = .fl_clean_na(tau$stat),
                         df = if (!is.na(tau$df)) as.character(tau$df) else "",
                         p_value = .fl_clean_na(tau$p),
                         verdict = if (!tau$available) tr("lavaan not installed", "lavaan no instalado") else verdict(tau$p)),
                    list(assumption = tr("Unidimensionality", "Unidimensionalidad"),
                         test = tr("Parallel analysis (factors suggested)", "Análisis paralelo (factores sugeridos)"),
                         statistic = .fl_clean_na(as.numeric(n_fact_assump)),
                         df = "",
                         p_value = NA,
                         verdict = uni_verdict),
                    list(assumption = tr("Normality of items", "Normalidad de ítems"),
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
                    tr("The congeneric/tau-equivalent models below were fit with WLSMV on polychoric/tetrachoric correlations, matching how these items were detected/set (ordinal or dichotomous). ",
                       "Los modelos congenérico/tau-equivalente de abajo se ajustaron con WLSMV sobre correlaciones policóricas/tetracóricas, en línea con cómo se detectaron/fijaron estos ítems (ordinales o dicotómicos). ")
                    else ""
                note_html <- paste0(.fl_prose_open(),
                    "<p>", tau_estim_p, tr(
                        "Cronbach's &alpha; is the most commonly reported reliability coefficient and also the most assumption-laden: it equals the true reliability only when items are tau-equivalent (equal true-score loadings on a single common factor) (Cronbach, 1951; Zumbo, Gadermann &amp; Zeisser, 2007). Unlike McDonald's &omega;, GLB, or the Guttman &lambda; family above (none of which require tau-equivalence), &alpha; is biased -- usually downward -- whenever this assumption fails.",
                        "El Alfa de Cronbach es el coeficiente de confiabilidad más reportado y también el más cargado de supuestos: equivale a la confiabilidad verdadera solo cuando los ítems son tau-equivalentes (iguales cargas de puntaje verdadero sobre un único factor común) (Cronbach, 1951; Zumbo, Gadermann &amp; Zeisser, 2007). A diferencia del Omega de McDonald, el GLB o la familia &lambda; de Guttman de arriba (ninguno de los cuales requiere tau-equivalencia), el &alpha; está sesgado -- usualmente a la baja -- cuando este supuesto falla."
                    ), "</p>",
                    if (!tau$available) paste0("<p>&#9888; ", tr(
                            "The tau-equivalence test requires the lavaan package, which is not installed here -- install it for a direct statistical test; in the meantime, the discordance panel above (&alpha; vs. &omega;) is an indirect signal of the same thing.",
                            "La prueba de tau-equivalencia requiere el paquete lavaan, que no está instalado aquí -- instálelo para obtener una prueba estadística directa; mientras tanto, el panel de discordancia de arriba (&alpha; vs. &omega;) es una señal indirecta de lo mismo."), "</p>")
                    else if (is.na(tau$p)) paste0("<p>&#9888; ", tr(
                            "The tau-equivalence models did not converge on this data -- no formal verdict available; rely on the discordance panel above instead.",
                            "Los modelos de tau-equivalencia no convergieron con estos datos -- no hay veredicto formal disponible; use el panel de discordancia de arriba en su lugar."), "</p>")
                    else if (tau_bad) paste0("<p>&#9888; <b>", tr("Tau-equivalence violated", "Tau-equivalencia violada"), ":</b> ",
                        tr("a congeneric model (free item loadings) fits significantly better than a tau-equivalent model (equal loadings) (p &lt; .05). Items load unequally on the underlying factor, so &alpha; is likely biased here -- report McDonald's &omega; instead, which does not require this assumption.",
                           "un modelo congenérico (cargas de ítem libres) ajusta significativamente mejor que un modelo tau-equivalente (cargas iguales) (p &lt; .05). Los ítems cargan de forma desigual sobre el factor subyacente, así que el &alpha; probablemente esté sesgado aquí -- reporte el Omega de McDonald en su lugar, que no requiere este supuesto."), "</p>")
                    else paste0("<p>&#10003; ", tr("No evidence against tau-equivalence -- the congeneric and tau-equivalent models fit comparably well, so &alpha;'s assumption looks reasonable here.",
                                                    "No hay evidencia contra la tau-equivalencia -- los modelos congenérico y tau-equivalente ajustan de forma comparable, así que el supuesto del &alpha; parece razonable aquí."), "</p>"),
                    "<p>", if (uni_bad) paste0("&#9888; <b>", tr("Unidimensionality violated", "Unidimensionalidad violada"), ":</b> ",
                        tr(paste0("parallel analysis suggests ", n_fact_assump, " factors. A single overall &alpha; mixes distinct dimensions into one number; see the Dimensionality Check panel below for what to do about it."),
                           paste0("el análisis paralelo sugiere ", n_fact_assump, " factores. Un &alpha; global único mezcla dimensiones distintas en un solo número; vea el panel de Verificación de Dimensionalidad abajo para saber qué hacer al respecto.")))
                        else paste0("&#10003; ", tr("Parallel analysis supports a single underlying factor -- consistent with what a single overall &alpha; is meant to measure.",
                                                     "El análisis paralelo respalda un solo factor subyacente -- consistente con lo que un &alpha; global único pretende medir.")), "</p>",
                    "<p>", if (norm_bad) paste0("&#9888; <b>", tr("Item normality violated", "Normalidad de ítems violada"), ":</b> ",
                        tr(paste0(nonnormal_count, " item(s) fail Shapiro-Wilk (see the Item Normality table below); &alpha;'s standard-error formula assumes multivariate normality, so treat its confidence interval as approximate and prefer Ordinal &alpha; or the bootstrap CI above."),
                           paste0(nonnormal_count, " ítem(s) fallan Shapiro-Wilk (vea la tabla de Normalidad de Ítems abajo); la fórmula del error estándar del &alpha; asume normalidad multivariada, así que trate su intervalo de confianza como aproximado y prefiera el Alfa Ordinal o el IC por bootstrap de arriba.")))
                        else if (normality_applicable) paste0("&#10003; ", tr("All items pass the normality check -- &alpha; is not at a distributional disadvantage here.", "Todos los ítems pasan la prueba de normalidad -- el &alpha; no está en desventaja distribucional aquí."))
                        else if (opt$normality) tr("Not applicable to dichotomous or ordinal items -- normality is a continuous-distribution concept.", "No aplica a ítems dicotómicos u ordinales -- la normalidad es un concepto de distribución continua.")
                        else tr("Normality testing is off (enable it in Item Analysis to check this).", "La prueba de normalidad está desactivada (actívela en Análisis de Ítems para revisar esto)."),
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
                            "<p>&#10003; <b>", tr("Unidimensional structure suggested.","Estructura unidimensional sugerida."), "</b> ",
                            tr(paste0("Parallel analysis recommends ", n_f, " factor(s) and ", n_c, " component(s). ",
                                      "Cronbach\u2019s \u03B1 and McDonald\u2019s \u03C9 are appropriate."),
                               paste0("El análisis paralelo recomienda ", n_f, " factor(es) y ", n_c, " componente(s). ",
                                      "El Alfa de Cronbach y el Omega de McDonald son apropiados.")), "</p>")
                    } else {
                        dim_html <- paste0(
                            "<p>&#9888; <b>",
                            tr(paste0("Multidimensional structure detected: ", n_f, " factor(s), ", n_c, " component(s)."),
                               paste0("Estructura multidimensional detectada: ", n_f, " factor(es), ", n_c, " componente(s).")),
                            "</b></p><p>",
                            tr(paste0("Recommendation: (1) Compute Cronbach\u2019s \u03B1 per subscale separately. ",
                                      "(2) Report Omega Hierarchical (\u03C9\u2095) for the total scale. ",
                                      "(3) Overall \u03B1 across all items may be misleading."),
                               paste0("Recomendación: (1) Calcule el Alfa de Cronbach por subescala por separado. ",
                                      "(2) Reporte el Omega Jerárquico (\u03C9\u2095) para la escala total. ",
                                      "(3) El \u03B1 global puede ser engañoso.")),
                            "</p>")
                    }
                    self$results$dimensionalityNote$setContent(dim_html)
                }
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
                               paste0(", α ", tr("if removed","si se elimina"), " = ", round(drop_v[j], 3))
                           else "",
                           ")")
                }, character(1))
                paste0("<p>&#9888; <b>", tr("Weak item-total correlation(s)", "Correlación(es) ítem-total débil(es)"), ":</b> ",
                       tr("the following item(s) fall below the r(item-total) &ge; .30 screening threshold: ",
                          "el/los siguiente(s) ítem(s) está(n) por debajo del umbral de cribado r(ítem-total) &ge; .30: "),
                       paste(items_txt, collapse = "; "), ". ",
                       tr("This is a diagnostic flag, not a retention rule: a low item-total correlation can come from reverse-scoring that was never recoded, multidimensionality, deliberately heterogeneous content, or an item that genuinely doesn't function as intended. Inspect the item's content and scoring direction, and consider its theoretical role in the construct, before deciding to revise, recode, or remove it — the item-analysis table above shows &alpha; if each item were dropped only as one input to that judgment, not as the criterion itself.",
                          "Esta es una alerta diagnóstica, no una regla de retención: una correlación ítem-total baja puede deberse a una recodificación inversa que nunca se aplicó, multidimensionalidad, contenido deliberadamente heterogéneo, o un ítem que genuinamente no funciona como se pretendía. Inspeccione el contenido y la dirección de puntuación del ítem, y considere su rol teórico en el constructo, antes de decidir revisarlo, recodificarlo o eliminarlo — el α si se elimina cada ítem en la tabla de análisis de ítems de arriba es solo un insumo para ese juicio, no el criterio en sí mismo."),
                       "</p>")
            } else {
                paste0("<p>&#10003; ", tr("No items fall below the r(item-total) ≥ .30 threshold.",
                                          "Ningún ítem está por debajo del umbral r(ítem-total) ≥ .30."), "</p>")
            }

            # ── Discordance panel (standalone result, matching interRater's
            # own discordanceNote) ────────────────────────────────────────────
            discord_html <- if (!is.na(best_val) && !is.na(ot) && abs(best_val - ot) > .04) {
                paste0("<p>&#9888; <b>", tr("Coefficients disagree", "Los coeficientes no coinciden"), ":</b> ",
                       tr(paste0("Cronbach's α (", round(best_val,3), ") and McDonald's ω (", round(ot,3),
                                 ") differ by more than .04. α assumes equal item loadings (τ-equivalence); "),
                          paste0("El Alfa de Cronbach (", round(best_val,3), ") y el Omega de McDonald (", round(ot,3),
                                 ") difieren en más de .04. El α asume cargas de ítem iguales (τ-equivalencia); ")),
                       tr("this scale's items likely load unequally on the underlying factor, so α is probably biased here — trust ω instead.",
                          "los ítems de esta escala probablemente cargan de forma desigual sobre el factor subyacente, así que el α probablemente esté sesgado aquí — confíe en ω en su lugar."),
                       "</p>")
            } else if (!is.na(best_val) && !is.na(ot)) {
                paste0("<p>&#10003; ", tr("Cronbach's α and McDonald's ω agree closely — the τ-equivalence assumption behind α looks reasonable here.",
                                          "El Alfa de Cronbach y el Omega de McDonald coinciden de cerca — el supuesto de τ-equivalencia detrás del α parece razonable aquí."), "</p>")
            } else ""
            self$results$discordanceNote$setContent(if (nzchar(discord_html)) .fl_prose(discord_html) else discord_html)
            if (!nzchar(discord_html)) self$results$discordanceNote$setVisible(FALSE)

            normality_html <- if (normality_applicable && k > 0L) {
                if (nonnormal_count > 0L) {
                    paste0("<p>&#9888; ",
                           tr(paste0(nonnormal_count, " of ", k, " item(s) fail the Shapiro-Wilk normality test (p < .05). "),
                              paste0(nonnormal_count, " de ", k, " ítem(s) fallan la prueba de normalidad de Shapiro-Wilk (p < .05). ")),
                           tr(paste0("Prefer ", if (!is.na(oa)) "Ordinal α" else "McDonald's ω",
                                     " over raw Cronbach's α for the primary estimate."),
                              paste0("Prefiera el ", if (!is.na(oa)) "Alfa Ordinal" else "Omega de McDonald",
                                     " sobre el Alfa de Cronbach bruto como estimación primaria.")),
                           "</p>")
                } else {
                    paste0("<p>&#10003; ", tr("All items pass the normality check — Cronbach's α is not at a distributional disadvantage here.",
                                              "Todos los ítems pasan la prueba de normalidad — el Alfa de Cronbach no está en desventaja distribucional aquí."), "</p>")
                }
            } else ""

            dim_summary_html <- if (!is.null(fa_par)) {
                if (n_omega_factors >= 2L) {
                    paste0("<p>&#9888; ",
                           tr(paste0("Parallel analysis suggests ", fa_par$nfact, " factor(s) — this scale is likely multidimensional. ",
                                     "A single overall α/ω may blend distinct dimensions; consider reporting reliability per subscale."),
                              paste0("El análisis paralelo sugiere ", fa_par$nfact, " factor(es) — esta escala probablemente es multidimensional. ",
                                     "Un α/ω global único puede mezclar dimensiones distintas; considere reportar la confiabilidad por subescala.")),
                           "</p>")
                } else {
                    paste0("<p>&#10003; ", tr("Parallel analysis suggests a single (unidimensional) factor — a single overall reliability estimate is appropriate.",
                                              "El análisis paralelo sugiere un solo factor (unidimensional) — una estimación de confiabilidad global única es apropiada."), "</p>")
                }
            } else ""

            action_items <- character(0)
            if (length(weak_idx) > 0L)
                action_items <- c(action_items, tr(
                    paste0("Inspect the content, scoring direction, and theoretical role of: ", paste(names(df)[weak_idx], collapse = ", "), " -- only revise or remove after that substantive review."),
                    paste0("Inspeccione el contenido, la dirección de puntuación y el rol teórico de: ", paste(names(df)[weak_idx], collapse = ", "), " -- revise o elimine solo después de esa revisión sustantiva.")))
            if (!is.na(best_val) && !is.na(ot) && abs(best_val - ot) > .04)
                action_items <- c(action_items, tr("Report McDonald's ω as the primary coefficient, not α.",
                                                    "Reporte el Omega de McDonald como coeficiente primario, no el α."))
            if (opt$normality && nonnormal_count > 0L && !is.na(oa))
                action_items <- c(action_items, tr("Report Ordinal α alongside α given the non-normal items.",
                                                    "Reporte el Alfa Ordinal junto al α dado los ítems no normales."))
            if (!is.null(fa_par) && n_omega_factors >= 2L)
                action_items <- c(action_items, tr("Compute reliability per subscale in addition to the overall estimate.",
                                                    "Calcule la confiabilidad por subescala además de la estimación global."))
            if (n < 200L)
                action_items <- c(action_items, tr(paste0("n = ", n, " is below the n ≥ 200 rule of thumb for a stable estimate; treat the CI above as wide."),
                                                    paste0("n = ", n, " está por debajo de la regla empírica n ≥ 200 para una estimación estable; trate el IC de arriba como amplio.")))
            action_html <- if (length(action_items) > 0L)
                paste0("<ul style='line-height:1;'>",
                       paste0("<li>", action_items, "</li>", collapse = ""), "</ul>")
            else
                paste0("<p>", tr("No specific corrective action indicated — the primary estimate can be reported as-is.",
                                  "No se indica ninguna acción correctiva específica — la estimación primaria puede reportarse tal cual."), "</p>")

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
                "<h4>", tr("What happened", "Qué pasó"), "</h4>",
                "<p>", tr(
                    paste0("The primary reliability estimate for this scale is <b>",
                           round(best_val, 3), "</b> (", interp_lbl, ")."),
                    paste0("La estimación primaria de confiabilidad para esta escala es <b>",
                           round(best_val, 3), "</b> (", interp_lbl, ").")), "</p>",

                "<h4>", tr("Why", "Por qué"), "</h4>",
                weak_html, normality_html, dim_summary_html,

                "<h4>", tr("What it means", "Qué implica"), "</h4>",
                "<p>", tr(
                    "See the Reliability Coefficients and Item Analysis tables above for the exact numbers behind each point below; the Fiability Library (Coefficients section) documents each coefficient's assumptions and formula in full.",
                    "Vea las tablas de Coeficientes de Confiabilidad y Análisis de Ítems arriba para las cifras exactas detrás de cada punto; la Biblioteca de Confiabilidad (sección Coeficientes) documenta el supuesto y la fórmula de cada coeficiente en detalle."),
                "</p>",
                "<p style='margin-top:0.8em;font-weight:700;'>", tr("Interpretation Benchmarks", "Criterios de interpretación"), "</p>",
                "<table style='border-collapse:collapse;'>",
                "<tr><th style='padding:3px 8px;border:1px solid #ccc;'>", tr("Value","Valor"), "</th>",
                "<th style='padding:3px 8px;border:1px solid #ccc;'>", tr("Interpretation","Interpretación"), "</th></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>≥ .95</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Excellent","Excelente"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>.90 – .94</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Good","Bueno"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>.80 – .89</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Acceptable","Aceptable"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>.70 – .79</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Questionable","Cuestionable"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>.60 – .69</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Poor","Pobre"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>< .60</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Unacceptable","Inaceptable"), "</td></tr>",
                "</table>",

                "<h4>", tr("What to do now", "Qué hacer ahora"), "</h4>",
                action_html,

                "<p style='font-size:0.85em;color:#666;'>", tr(
                    "See Fiability Library → Coefficients for full definitions, assumptions and references (Bibliography → Classical Test Theory).",
                    "Vea Biblioteca de Confiabilidad → Coeficientes para definiciones y supuestos completos, y referencias (Bibliografía → Teoría Clásica de los Tests)."),
                "</p>",
                "</div>")
            self$results$interpretation$setContent(rec_html)
        },

        # ── Plot: coefficient comparison (forest-plot style) ────────────────
        # Same construction as interRater's .plotComparison -- see the note
        # at private$.plot_rows' construction (step 10b) for why.
        .plotComparison = function(image, ggtheme, theme, ...) {
            d <- private$.plot_rows
            if (is.null(d) || nrow(d) == 0L) return(FALSE)
            cols <- private$.plot_colors()
            d$coefficient <- factor(d$coefficient, levels = rev(d$coefficient))
            lo <- min(0, d$ci_lower, na.rm = TRUE)
            hi <- max(1, d$ci_upper, na.rm = TRUE)
            p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = coefficient)) +
                ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                                       width = .15, orientation = "y", colour = cols$primary, linewidth = .6) +
                ggplot2::geom_point(size = 3.2, colour = cols$primary) +
                ggplot2::coord_cartesian(xlim = c(lo, hi)) +
                ggplot2::labs(x = private$.tr("Value (95% CI)", "Valor (IC 95%)"), y = NULL,
                              title = private$.tr("Coefficient Comparison", "Comparación de Coeficientes")) +
                private$.plot_theme()
            print(p)
            TRUE
        },

        # ── Plot: item score distributions ────────────────────────────────────
        .plotItemDist = function(image, ggtheme, theme, ...) {
            df <- private$.df_clean
            if (is.null(df)) return(FALSE)
            k   <- ncol(df)
            nc  <- min(k, 4L)
            nr  <- ceiling(k / nc)
            cols <- private$.plot_colors()

            plots <- lapply(seq_len(k), function(j) {
                x   <- df[[j]]
                tbl <- as.data.frame(table(x), stringsAsFactors = FALSE)
                colnames(tbl) <- c("val","freq")
                tbl$val <- as.numeric(tbl$val)
                ggplot2::ggplot(tbl, ggplot2::aes(x = val, y = freq)) +
                    ggplot2::geom_bar(stat = "identity", fill = cols$primary, alpha = .85, width = .6) +
                    ggplot2::labs(title = names(df)[j], x = NULL, y = NULL) +
                    private$.plot_theme() +
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
        .plotItemTotal = function(image, ggtheme, theme, ...) {
            citc <- private$.citc_vals
            df   <- private$.df_clean
            if (is.null(citc) || is.null(df)) return(FALSE)

            cols <- private$.plot_colors()
            dat <- data.frame(
                item = factor(names(df), levels = rev(names(df))),
                citc = as.numeric(citc))
            p <- ggplot2::ggplot(dat, ggplot2::aes(x = item, y = citc,
                                                    fill = citc >= .30)) +
                ggplot2::geom_bar(stat = "identity", width = .6) +
                ggplot2::geom_hline(yintercept = .30, linetype = "dashed",
                                    colour = cols$secondary, linewidth = .6) +
                ggplot2::scale_fill_manual(values = c("FALSE" = cols$secondary,
                                                       "TRUE"  = cols$primary),
                                           guide = "none") +
                ggplot2::coord_flip() +
                ggplot2::labs(x = NULL,
                              y = private$.tr("Corrected item-total r",
                                              "r ítem-total corregida"),
                              title = private$.tr("Item–Total Correlations (threshold = .30)",
                                                  "Correlaciones Ítem-Total (umbral = .30)")) +
                private$.plot_theme()
            print(p)
            TRUE
        },

        # ── Plot: scree (parallel analysis) ──────────────────────────────────
        .plotScree = function(image, ggtheme, theme, ...) {
            fa_par <- private$.fa_result
            df     <- private$.df_clean
            if (is.null(df)) return(FALSE)

            # Re-run if needed
            if (is.null(fa_par)) {
                fa_par <- tryCatch(
                    suppressWarnings(psych::fa.parallel(df, plot = FALSE, fa = "both")),
                    error = function(e) NULL)
            }
            if (is.null(fa_par)) return(FALSE)

            ev_fa   <- fa_par$fa.values
            # fa_par$fa.sim is a matrix (n.iter simulated eigenvalue sets x
            # n.factor) in the general case, but psych::fa.parallel()
            # sometimes returns it as a plain vector for a strongly
            # unidimensional structure -- rowMeans() on a bare vector
            # throws "'x' must be an array of at least two dimensions",
            # caught by this session's own render-function verification
            # pass (a bug the earlier asDF()-only testing never exercised).
            # ES: fa_par$fa.sim es una matriz (n.iter conjuntos de
            # autovalores simulados x n.factor) en el caso general, pero
            # psych::fa.parallel() a veces la retorna como vector simple
            # para una estructura fuertemente unidimensional -- rowMeans()
            # sobre un vector simple lanza "'x' must be an array of at
            # least two dimensions", detectado por la propia verificación
            # de funciones de render de esta sesión (un bug que las
            # pruebas anteriores basadas solo en asDF() nunca ejercitaron).
            ev_sim  <- fa_par$fa.sim
            n_fact  <- min(length(ev_fa), 10L)
            cols    <- private$.plot_colors()
            dat <- data.frame(
                factor  = seq_len(n_fact),
                actual  = ev_fa[seq_len(n_fact)],
                simulated = if (is.matrix(ev_sim) && nrow(ev_sim) >= 1L && ncol(ev_sim) >= n_fact)
                    colMeans(ev_sim)[seq_len(n_fact)]
                else if (!is.matrix(ev_sim) && length(ev_sim) >= n_fact)
                    ev_sim[seq_len(n_fact)]
                else rep(1, n_fact))

            p <- ggplot2::ggplot(dat, ggplot2::aes(x = factor)) +
                ggplot2::geom_line(ggplot2::aes(y = actual,    colour = "Actual"),    linewidth = 1) +
                ggplot2::geom_point(ggplot2::aes(y = actual,   colour = "Actual"),    size = 2.5) +
                ggplot2::geom_line(ggplot2::aes(y = simulated, colour = "Simulated"), linewidth = .7, linetype = "dashed") +
                ggplot2::scale_colour_manual(
                    name   = NULL,
                    values = c("Actual" = cols$primary, "Simulated" = cols$secondary)) +
                ggplot2::labs(
                    x     = private$.tr("Factor", "Factor"),
                    y     = private$.tr("Eigenvalue", "Autovalor"),
                    title = private$.tr("Scree Plot — Parallel Analysis",
                                        "Gráfico de sedimentación — Análisis paralelo")) +
                private$.plot_theme()
            print(p)
            TRUE
        }
    )
)
