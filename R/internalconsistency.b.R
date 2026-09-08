internalConsistencyClass <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "internalConsistencyClass",
    inherit = internalConsistencyBase,
    private = list(

        # ── Stored state for plot renderers ───────────────────────────────────
        .df_clean  = NULL,
        .citc_vals = NULL,
        .alpha_drop= NULL,
        .fa_result = NULL,

        # ── Translation helper ────────────────────────────────────────────────
        .tr = function(en, es) {
            if (identical(self$options$reportLang, "es")) es else en
        },

        # ── Interpretation of reliability coefficient ─────────────────────────
        .interp_rel = function(val) {
            tr <- private$.tr
            if (is.na(val) || !is.finite(val)) return(tr("N/A", "N/D"))
            if (val >= .95) return(tr("Excellent",    "Excelente"))
            if (val >= .90) return(tr("Good",         "Bueno"))
            if (val >= .80) return(tr("Acceptable",   "Aceptable"))
            if (val >= .70) return(tr("Questionable", "Cuestionable"))
            if (val >= .60) return(tr("Poor",         "Pobre"))
            return(tr("Unacceptable", "Inaceptable"))
        },

        # ── Significance stars ────────────────────────────────────────────────
        .sig = function(p) {
            if (is.na(p) || !is.finite(p)) return("")
            if (p < .001) return("***")
            if (p < .01)  return("**")
            if (p < .05)  return("*")
            return("")
        },

        # ── Bootstrap a scalar stat ───────────────────────────────────────────
        .bootstrap = function(df, stat_fn, B = 1000L) {
            n <- nrow(df)
            vals <- numeric(B)
            for (b in seq_len(B)) {
                idx     <- sample.int(n, n, replace = TRUE)
                vals[b] <- tryCatch(stat_fn(df[idx, , drop=FALSE]), error=function(e) NA_real_)
            }
            vals <- vals[is.finite(vals)]
            if (length(vals) < 10L) return(list(se=NA, lo=NA, hi=NA))
            list(se = sd(vals),
                 lo = quantile(vals, .025, names=FALSE),
                 hi = quantile(vals, .975, names=FALSE))
        },

        # ── Reset a Jamovi table to a requested number of rows ───────────────
        .reset_table = function(table, n_rows) {
            table$deleteRows()
            if (n_rows > 0L) {
                for (row_no in seq_len(n_rows))
                    table$addRow(rowKey = row_no)
            }
            invisible(table)
        },

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

            # ── 4. Auto-detect note ───────────────────────────────────────────
            detect_icon <- if (level == "dichotomous") "&#9679;" else "&#9632;"
            level_lbl   <- if (level == "dichotomous")
                tr("Dichotomous (0/1)", "Dicotómico (0/1)")
            else
                tr(paste0("Polytomous (", n_opts, " response options)"),
                   paste0("Politómico (", n_opts, " opciones de respuesta)"))

            auto_html <- paste0(
                "<table style='border-collapse:collapse;font-size:13px;'>",
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
            samp_html <- paste0(
                "<ul style='line-height:1.8;font-size:13px;'>",
                "<li>", n_warn, "</li>",
                "<li>", ratio_warn, "</li>",
                if (nzchar(norm_note))    paste0("<li>", norm_note, "</li>")    else "",
                if (nzchar(options_note)) paste0("<li>", options_note, "</li>") else "",
                "</ul>")
            self$results$sampleAdequacy$setContent(samp_html)

            # ── 6. Normality tests (Shapiro-Wilk per item) ────────────────────
            if (opt$normality) {
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
                    norm_tab$setRow(rowNo = j, values = list(
                        item     = names(df)[j],
                        W        = W_val,
                        p        = p_val,
                        sig      = private$.sig(p_val),
                        skewness = sk,
                        kurtosis = ku,
                        decision = dec))
                }
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
            rows      <- list()   # each element: list(coefficient, value, interpretation, applicability, fn)
            boot_rows <- list()   # only those with opt-in bootstrap

            add_stat <- function(name, val, interp_val, cond, fn = NULL) {
                rows[[length(rows) + 1L]] <<- list(
                    coefficient   = name,
                    value         = val,
                    interpretation= private$.interp_rel(interp_val),
                    applicability = cond)
                if (!is.null(fn))
                    boot_rows[[length(boot_rows) + 1L]] <<- list(name = name, fn = fn, val = val)
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
                add_stat(
                    tr("Cronbach's \u03B1",          "Alfa de Cronbach (\u03B1)"),
                    a_val, a_val,
                    tr("Polytomous, continuous items; assumes \u03C4-equivalence & normality",
                       "Ítems politómicos/continuos; asume equivalencia-\u03C4 y normalidad"),
                    function(d) .calc_alpha(d))
            }

            # ── 8b. Ordinal Alpha ─────────────────────────────────────────────
            if (opt$ordinalAlpha) {
                oa <- tryCatch({
                    pc  <- psych::polychoric(df, correct = 0)
                    eig <- eigen(pc$rho, only.values = TRUE)$values
                    kk  <- ncol(df)
                    (kk/(kk-1)) * (1 - kk/sum(eig))
                }, error = function(e) NA_real_)
                add_stat(
                    tr("Ordinal \u03B1 (polychoric)", "Alfa Ordinal (policórica)"),
                    oa, oa,
                    tr("Ordinal/Likert items; does not require normality",
                       "Ítems ordinales/Likert; no requiere normalidad"))
            }

            # ── 8c. McDonald's Omega ──────────────────────────────────────────
            omega_obj <- NULL
            if ((opt$omega || opt$omegaHierarchical || opt$glb) && k >= 3L) {
                omega_obj <- tryCatch(
                    psych::omega(df, nfactors = 1L, plot = FALSE, warnings = FALSE),
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
                        o <- tryCatch(psych::omega(d, nfactors=1, plot=FALSE, warnings=FALSE),
                                     error=function(e) NULL)
                        if (is.null(o)) NA_real_ else o$omega.tot
                    })
            }

            if (opt$omegaHierarchical && !is.null(omega_obj)) {
                oh <- omega_obj$omega_h
                add_stat(
                    tr("McDonald's \u03C9 hierarchical", "Omega Jerárquico (\u03C9\u2095)"),
                    oh, oh,
                    tr("Multidimensional scales; proportion of variance due to g-factor",
                       "Escalas multidimensionales; proporción de varianza del factor g"))
            }

            # ── 8d. GLB ───────────────────────────────────────────────────────
            if (opt$glb && !is.null(omega_obj)) {
                glb_val <- tryCatch(omega_obj$GLB, error=function(e) NA_real_)
                if (is.null(glb_val)) glb_val <- NA_real_
                add_stat(
                    tr("GLB (Greatest Lower Bound)", "Límite Inferior Máximo (GLB)"),
                    glb_val, glb_val,
                    tr("Upper bound for true reliability; no distributional assumptions",
                       "Cota superior de la confiabilidad real; sin supuestos distribucionales"))
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
                        lbl <- sub("lambda","\\u03BB", lam)
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

            # ── 9. Fill main table ────────────────────────────────────────────
            main_tab <- self$results$mainTable
            private$.reset_table(main_tab, length(rows))
            for (i in seq_along(rows)) {
                main_tab$setRow(rowNo = i, values = rows[[i]][
                    c("coefficient","value","interpretation","applicability")])
            }

            # ── 10. Bootstrap CIs ─────────────────────────────────────────────
            if (opt$bootstrapCi && length(boot_rows) > 0L) {
                B       <- as.integer(opt$bootstrapSamples)
                bt_tab  <- self$results$bootstrapTable
                private$.reset_table(bt_tab, length(boot_rows))
                for (i in seq_along(boot_rows)) {
                    r  <- boot_rows[[i]]
                    ci <- private$.bootstrap(df, r$fn, B)
                    bt_tab$setRow(rowNo = i, values = list(
                        coefficient = r$name,
                        estimate    = r$val,
                        ci_lower    = ci$lo,
                        ci_upper    = ci$hi,
                        se_boot     = ci$se))
                }
            }

            # ── 11. Dimensionality (parallel analysis) ────────────────────────
            if (opt$checkDimensionality && k >= 3L && n >= 20L) {
                fa_par <- tryCatch(
                    psych::fa.parallel(df, plot = FALSE, warnings = FALSE, fa = "both"),
                    error = function(e) NULL)
                private$.fa_result <- fa_par

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
            best_val <- if (!is.null(alpha_obj)) alpha_obj$total$raw_alpha else NA_real_
            interp_lbl <- private$.interp_rel(best_val)

            rec_html <- paste0(
                "<h4>", tr("Overall Assessment", "Evaluación general"), "</h4>",
                "<p>", tr(
                    paste0("The primary reliability estimate for this scale is <b>",
                           round(best_val, 3), "</b> (", interp_lbl, ")."),
                    paste0("La estimación primaria de confiabilidad para esta escala es <b>",
                           round(best_val, 3), "</b> (", interp_lbl, ").")),"</p>",
                "<h4>", tr("Interpretation Benchmarks (Kline, 2000; George & Mallery, 2003)", 
                            "Criterios de interpretación (Kline, 2000; George & Mallery, 2003)"), "</h4>",
                "<table style='border-collapse:collapse;font-size:13px;'>",
                "<tr><th style='padding:3px 8px;border:1px solid #ccc;'>", tr("Value","Valor"), "</th>",
                "<th style='padding:3px 8px;border:1px solid #ccc;'>", tr("Interpretation","Interpretación"), "</th></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>\u2265 .95</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Excellent","Excelente"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>.90 \u2013 .94</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Good","Bueno"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>.80 \u2013 .89</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Acceptable","Aceptable"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>.70 \u2013 .79</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Questionable","Cuestionable"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>.60 \u2013 .69</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Poor","Pobre"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>< .60</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Unacceptable","Inaceptable"), "</td></tr>",
                "</table>",
                "<h4>", tr("Effect of Key Factors on Reliability", "Efecto de factores clave en la confiabilidad"), "</h4>",
                "<ul style='font-size:13px;line-height:1.8;'>",
                "<li><b>", tr("Sample size","Tamaño de muestra"), ":</b> ",
                tr(paste0("n = ", n, ". Larger samples produce more stable estimates. Aim for n \u2265 200."),
                   paste0("n = ", n, ". Muestras más grandes producen estimaciones más estables. Objetivo: n \u2265 200.")), "</li>",
                "<li><b>", tr("Number of items","Número de ítems"), ":</b> ",
                tr(paste0("k = ", k, ". More items generally increase \u03B1 (Spearman-Brown prophecy). Minimum 6\u20138 items recommended."),
                   paste0("k = ", k, ". Más ítems generalmente aumentan \u03B1 (profecía de Spearman-Brown). Mínimo 6\u20138 ítems recomendado.")), "</li>",
                "<li><b>", tr("Response options","Opciones de respuesta"), ":</b> ",
                tr(paste0("Max = ", n_opts, ". Scales with 4\u20137 points typically yield higher reliability than binary items."),
                   paste0("Máx = ", n_opts, ". Escalas con 4\u20137 puntos producen mayor confiabilidad que ítems binarios.")), "</li>",
                "<li><b>", tr("Normality","Normalidad"), ":</b> ",
                tr("Non-normal item distributions inflate standard errors. Ordinal \u03B1 or \u03C9 are preferable when normality is violated.",
                   "Distribuciones no normales inflan los errores estándar. El Alfa Ordinal o el Omega son preferibles cuando se viola la normalidad."), "</li>",
                "<li><b>", tr("Dimensionality","Dimensionalidad"), ":</b> ",
                tr("Cronbach\u2019s \u03B1 assumes unidimensionality. If the scale is multidimensional, \u03B1 may be misleading. Use \u03C9\u2095 or compute \u03B1 per subscale.",
                   "El Alfa de Cronbach asume unidimensionalidad. Si la escala es multidimensional, el \u03B1 puede ser engañoso. Use \u03C9\u2095 o calcule \u03B1 por subescala."), "</li>",
                "</ul>")
            self$results$interpretation$setContent(rec_html)

            # ── 13. References ────────────────────────────────────────────────
            refs_html <- paste0(
                "<p style='font-size:12px;line-height:1.8;'>",
                "Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests. <i>Psychometrika, 16</i>(3), 297\u2013334. https://doi.org/10.1007/BF02310555<br>",
                "George, D., & Mallery, P. (2003). <i>SPSS for Windows step by step</i> (4th ed.). Allyn & Bacon.<br>",
                "Kline, P. (2000). <i>The handbook of psychological testing</i> (2nd ed.). Routledge.<br>",
                "Kuder, G. F., & Richardson, M. W. (1937). The theory of the estimation of test reliability. <i>Psychometrika, 2</i>(3), 151\u2013160. https://doi.org/10.1007/BF02288391<br>",
                "McDonald, R. P. (1999). <i>Test theory: A unified treatment</i>. Lawrence Erlbaum.<br>",
                "Revelle, W. (2024). <i>psych: Procedures for psychological, psychometric, and personality research</i> (R package). https://CRAN.R-project.org/package=psych<br>",
                "Zumbo, B. D., Gadermann, A. M., & Zeisser, C. (2007). Ordinal versions of coefficients alpha and theta for Likert rating scales. <i>Journal of Modern Applied Statistical Methods, 6</i>(1), 21\u201329. https://doi.org/10.22237/jmasm/1177992180",
                "</p>")
            self$results$references$setContent(refs_html)
        },

        # ── Plot: item score distributions ────────────────────────────────────
        .plotItemDist = function(image, ggtheme, theme, ...) {
            df <- private$.df_clean
            if (is.null(df)) return(FALSE)
            k   <- ncol(df)
            nc  <- min(k, 4L)
            nr  <- ceiling(k / nc)

            plots <- lapply(seq_len(k), function(j) {
                x   <- df[[j]]
                tbl <- as.data.frame(table(x), stringsAsFactors = FALSE)
                colnames(tbl) <- c("val","freq")
                tbl$val <- as.numeric(tbl$val)
                ggplot2::ggplot(tbl, ggplot2::aes(x = val, y = freq)) +
                    ggplot2::geom_bar(stat = "identity", fill = "#4E79A7", alpha = .85, width = .6) +
                    ggplot2::labs(title = names(df)[j], x = NULL, y = NULL) +
                    ggtheme +
                    ggplot2::theme(plot.title = ggplot2::element_text(size = 9, face = "bold"),
                                   axis.text  = ggplot2::element_text(size = 8))
            })
            if (requireNamespace("gridExtra", quietly = TRUE)) {
                p <- gridExtra::arrangeGrob(grobs = plots, ncol = nc)
                print(p)
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

            dat <- data.frame(
                item = factor(names(df), levels = rev(names(df))),
                citc = as.numeric(citc))
            p <- ggplot2::ggplot(dat, ggplot2::aes(x = item, y = citc,
                                                    fill = citc >= .30)) +
                ggplot2::geom_bar(stat = "identity", width = .6) +
                ggplot2::geom_hline(yintercept = .30, linetype = "dashed",
                                    colour = "#E15759", linewidth = .6) +
                ggplot2::scale_fill_manual(values = c("FALSE" = "#F28E2B",
                                                       "TRUE"  = "#4E79A7"),
                                           guide = "none") +
                ggplot2::coord_flip() +
                ggplot2::labs(x = NULL,
                              y = private$.tr("Corrected item-total r",
                                              "r ítem-total corregida"),
                              title = private$.tr("Item–Total Correlations (threshold = .30)",
                                                  "Correlaciones Ítem-Total (umbral = .30)")) +
                ggtheme
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
                    psych::fa.parallel(df, plot = FALSE, warnings = FALSE, fa = "both"),
                    error = function(e) NULL)
            }
            if (is.null(fa_par)) return(FALSE)

            ev_fa   <- fa_par$fa.values
            ev_sim  <- fa_par$fa.sim
            n_fact  <- min(length(ev_fa), 10L)
            dat <- data.frame(
                factor  = seq_len(n_fact),
                actual  = ev_fa[seq_len(n_fact)],
                simulated = if (length(ev_sim) >= n_fact)
                    rowMeans(ev_sim)[seq_len(n_fact)]
                else rep(1, n_fact))

            p <- ggplot2::ggplot(dat, ggplot2::aes(x = factor)) +
                ggplot2::geom_line(ggplot2::aes(y = actual,    colour = "Actual"),    linewidth = 1) +
                ggplot2::geom_point(ggplot2::aes(y = actual,   colour = "Actual"),    size = 2.5) +
                ggplot2::geom_line(ggplot2::aes(y = simulated, colour = "Simulated"), linewidth = .7, linetype = "dashed") +
                ggplot2::scale_colour_manual(
                    name   = NULL,
                    values = c("Actual" = "#4E79A7", "Simulated" = "#E15759")) +
                ggplot2::labs(
                    x     = private$.tr("Factor", "Factor"),
                    y     = private$.tr("Eigenvalue", "Autovalor"),
                    title = private$.tr("Scree Plot — Parallel Analysis",
                                        "Gráfico de sedimentación — Análisis paralelo")) +
                ggtheme
            print(p)
            TRUE
        }
    )
)
