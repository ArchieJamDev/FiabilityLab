# -----------------------------------------------------------------------------
# FiabilityLab - Inter-Rater Agreement.
#
# Fase 1: computes chance-corrected agreement / reliability among 2+
# independent raters scoring the same cases, at nominal, ordinal, or
# continuous measurement levels. Every coefficient here has its own entry in
# the Fiability Library (category: Inter-Rater Agreement) and citation in
# Bibliography (topic: Inter-Rater Reliability), written before this file per
# the Library/Bibliography admission contract (ARCHITECTURE.md).
#
# ES: Fase 1: calcula el acuerdo/confiabilidad corregido por azar entre 2+
# jueces independientes que califican los mismos casos, a nivel de medida
# nominal, ordinal o continuo. Cada coeficiente aquí tiene su propia entrada
# en la Fiability Library (categoría: Inter-Rater Agreement) y cita en
# Bibliography (tema: Inter-Rater Reliability), escritas antes que este
# archivo según el contrato de admisión Library/Bibliography.
# -----------------------------------------------------------------------------

interRaterClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "interRaterClass",
    inherit = interRaterBase,
    private = list(

        # ── Stored state for plot renderers ───────────────────────────────
        .plot_rows = NULL,   # data.frame: coefficient, value, ci_lower, ci_upper
        .diag_type = NULL,   # "prevalence" | "rater_mean" | NULL
        .diag_data = NULL,   # data.frame backing .plotDiagnostic
        .diag_extra = NULL,  # scalar backing .plotDiagnostic (e.g. grand mean)

        .tr = function(en, es) .fl_tr(en, es, self$options$reportLang),

        # User-selectable plot style (independent of jamovi's own light/
        # dark theme, which is what the render functions' own `ggtheme`
        # argument adapts to) -- lets the report's plots match whatever
        # house style a manuscript/thesis needs.
        # ES: Estilo de gráfico seleccionable por el usuario (independiente
        # del tema claro/oscuro propio de jamovi, al que se adapta el
        # argumento `ggtheme` de las funciones de renderizado) -- permite
        # que los gráficos del reporte coincidan con el estilo que exija
        # un manuscrito o tesis.
        .plot_theme = function() .fl_plot_theme(self$options$plotStyle),

        # EN: The three colour-scheme styles (green-red, purple-orange,
        # blue-green light) pick a primary/secondary hex pair used across
        # every geom in the module's plots, replacing the old fixed blue/
        # red. The three background-theme styles (light/gray/linedraw)
        # keep the original blue/red pair so their look is otherwise
        # unchanged.
        # ES: Los tres estilos de esquema de color (verde-rojo, morado-
        # naranja, azul-verde claro) eligen un par primario/secundario de
        # hex usado en cada geom de los gráficos del módulo, reemplazando
        # el azul/rojo fijo anterior. Los tres estilos de tema de fondo
        # (light/gray/linedraw) conservan el par azul/rojo original para
        # que su aspecto no cambie.
        .plot_colors = function() .fl_plot_colors(self$options$plotStyle),

        # ── ICC assumption diagnostics ─────────────────────────────────────
        # EN: ICC (two-way model) is a variance-partition of the same
        # subject x rater ANOVA that Shrout & Fleiss (1979) build the
        # coefficient on, so its point estimate and (F-distribution-based)
        # CI inherit that model's usual assumptions: normally-distributed
        # residuals, homogeneous error variance across raters, and an
        # additive (linear, non-interacting) subject x rater structure.
        # These three helpers compute the standard diagnostic for each.
        # ES: El ICC (modelo de dos vías) es una partición de varianza del
        # mismo ANOVA sujeto x juez sobre el que Shrout & Fleiss (1979)
        # construyen el coeficiente, así que su valor puntual y su IC
        # (basado en la distribución F) heredan los supuestos usuales de
        # ese modelo: residuos normalmente distribuidos, varianza de error
        # homogénea entre jueces, y una estructura sujeto x juez aditiva
        # (lineal, sin interacción). Estos tres ayudantes calculan el
        # diagnóstico estándar de cada uno.

        # Levene (1960): one-way ANOVA F-test on |y - group mean|, testing
        # equality of error variance across raters (the ICC model's
        # homoscedasticity assumption). Mean-centered (classic Levene, not
        # the median-centered Brown-Forsythe variant).
        .levene_manual = function(y, g) {
            g <- factor(g)
            means <- tapply(y, g, mean, na.rm = TRUE)
            z <- abs(y - means[g])
            fit <- tryCatch(stats::aov(z ~ g), error = function(e) NULL)
            if (is.null(fit)) return(list(stat = NA_real_, df1 = NA_integer_, df2 = NA_integer_, p = NA_real_))
            s <- summary(fit)[[1]]
            list(stat = s[["F value"]][1], df1 = s[["Df"]][1], df2 = s[["Df"]][2], p = s[["Pr(>F)"]][1])
        },

        # Tukey (1949): the classic one-degree-of-freedom test for
        # non-additivity in a two-way layout without replication -- exactly
        # the subject x rater structure ICC is computed on. Tests whether
        # the subject and rater effects combine additively (i.e. linearly);
        # a significant result means at least one rater's scores curve
        # relative to the others rather than following the same straight-
        # line relationship the ICC model assumes.
        .tukey_nonadditivity = function(df_num) {
            m <- as.matrix(df_num)
            r <- nrow(m); cc <- ncol(m)
            row_means <- rowMeans(m); col_means <- colMeans(m); grand <- mean(m)
            row_dev <- row_means - grand; col_dev <- col_means - grand
            num   <- sum(outer(row_dev, col_dev) * m)
            denom <- sum(row_dev^2) * sum(col_dev^2)
            df_e   <- (r - 1L) * (cc - 1L)
            df_rem <- df_e - 1L
            if (!is.finite(denom) || denom <= 0 || df_rem < 1L)
                return(list(stat = NA_real_, df1 = 1L, df2 = NA_integer_, p = NA_real_))
            ss_n  <- num^2 / denom
            resid <- sweep(sweep(m, 1, row_means, "-"), 2, col_means, "-") + grand
            ss_e   <- sum(resid^2)
            ss_rem <- ss_e - ss_n
            if (ss_rem <= 0) return(list(stat = NA_real_, df1 = 1L, df2 = df_rem, p = NA_real_))
            f_stat <- ss_n / (ss_rem / df_rem)
            p <- stats::pf(f_stat, 1, df_rem, lower.tail = FALSE)
            list(stat = f_stat, df1 = 1L, df2 = df_rem, p = p)
        },

        # Landis & Koch (1977) bands -- the field-standard interpretation
        # scale for chance-corrected categorical agreement (Kappa, Gwet,
        # Krippendorff on nominal/ordinal data). Distinct from the
        # Kline (2000)/George & Mallery (2003) bands internalConsistency
        # uses, which describe internal-consistency coefficients, not
        # inter-rater agreement.
        .interp_kappa = function(val) {
            tr <- private$.tr
            if (is.na(val) || !is.finite(val)) return(tr("N/A", "N/D"))
            if (val < 0)     return(tr("Poor",           "Pobre"))
            if (val <= .20)  return(tr("Slight",          "Leve"))
            if (val <= .40)  return(tr("Fair",             "Aceptable"))
            if (val <= .60)  return(tr("Moderate",         "Moderado"))
            if (val <= .80)  return(tr("Substantial",      "Sustancial"))
            return(tr("Almost perfect", "Casi perfecto"))
        },

        # ICC / Krippendorff on continuous-ish data: reuse the same general
        # reliability bands as internalConsistency (Kline, 2000; George &
        # Mallery, 2003) -- already curated in Bibliography, and this is
        # the same underlying question (proportion of variance/agreement),
        # not a chance-corrected categorical statistic.
        .interp_rel = function(val) .fl_interp_rel(val, private$.tr),

        .bootstrap = function(df, stat_fn, B = 1000L) .fl_bootstrap(df, stat_fn, B),

        .reset_table = function(table, n_rows) .fl_reset_table(table, n_rows),

        .run = function() {
            tr  <- private$.tr
            opt <- self$options

            # ── 1. Validate ──────────────────────────────────────────────────
            ratings <- opt$ratings
            if (length(ratings) < 2) {
                self$results$autoDetectNote$setContent(paste0("<p><i>", tr(
                    "Please select at least 2 rating variables to compute agreement coefficients.",
                    "Por favor seleccione al menos 2 variables de calificación para calcular los coeficientes de acuerdo."
                ), "</i></p>"))
                self$results$mainTable$setVisible(FALSE)
                self$results$plotComparison$setVisible(FALSE)
                self$results$discordanceNote$setVisible(FALSE)
                self$results$plotDiagnostic$setVisible(FALSE)
                self$results$interpretation$setVisible(FALSE)
                return()
            }

            # ── 2. Prepare data ──────────────────────────────────────────────
            df_raw <- self$data[, ratings, drop = FALSE]
            df <- na.omit(df_raw)
            n_miss <- nrow(df_raw) - nrow(df)
            n <- nrow(df)
            k <- ncol(df)

            if (n < 5L) {
                self$results$autoDetectNote$setContent(paste0("<p><b>", tr(
                    "Not enough complete cases (minimum 5 required).",
                    "No hay suficientes casos completos (mínimo 5 requeridos)."
                ), "</b></p>"))
                self$results$mainTable$setVisible(FALSE)
                self$results$plotComparison$setVisible(FALSE)
                self$results$discordanceNote$setVisible(FALSE)
                self$results$plotDiagnostic$setVisible(FALSE)
                self$results$interpretation$setVisible(FALSE)
                return()
            }

            # ── 3. Detect measurement level ──────────────────────────────────
            is_char <- vapply(df, function(x) is.character(x) || is.factor(x), logical(1))
            is_nominal_data <- any(is_char)

            n_unique <- vapply(df, function(x) length(unique(x)), integer(1))
            is_whole <- vapply(df, function(x) {
                xn <- suppressWarnings(as.numeric(x))
                !anyNA(xn) && all(abs(xn - round(xn)) < 1e-8)
            }, logical(1))
            is_ordinal_data <- !is_nominal_data && all(is_whole) && max(n_unique) <= 7L

            level <- switch(opt$dataType,
                auto       = if (is_nominal_data) "nominal" else if (is_ordinal_data) "ordinal" else "continuous",
                nominal    = "nominal",
                ordinal    = "ordinal",
                continuous = "continuous")

            df_num <- if (level != "nominal") {
                df_n <- df
                for (col in names(df_n)) df_n[[col]] <- suppressWarnings(as.numeric(df_n[[col]]))
                df_n
            } else df

            level_lbl <- switch(level,
                nominal    = tr("Nominal", "Nominal"),
                ordinal    = tr(paste0("Ordinal (", max(n_unique), " categories)"), paste0("Ordinal (", max(n_unique), " categorías)")),
                continuous = tr("Continuous", "Continuo"))

            auto_html <- .fl_prose(
                "<table style='border-collapse:collapse;'>",
                "<tr><td style='padding:4px 10px;'><b>", tr("Raters", "Jueces"), "</b></td><td>", k, "</td></tr>",
                "<tr><td style='padding:4px 10px;'><b>", tr("Complete cases", "Casos completos"), "</b></td><td>", n,
                if (n_miss > 0) paste0(" <span style='color:orange;'>(", n_miss, " ", tr("removed listwise", "eliminados listwise"), ")</span>") else "",
                "</td></tr>",
                "<tr><td style='padding:4px 10px;'><b>", tr("Detected level", "Nivel detectado"), "</b></td><td>", level_lbl, "</td></tr>",
                "</table>")
            self$results$autoDetectNote$setContent(auto_html)

            # ── 4. Accumulate rows (value, interpretation, ci, p) ─────────────
            rows <- list()
            # Sanitize NA_real_/NaN to plain NA before they reach the Table:
            # a numeric NA_real_/NaN that survives to the real jamovi engine
            # (not just a direct-R asDF() check, which shows it correctly as
            # NA) can render as the literal text "NaN" in the jamovi Desktop
            # UI once it crosses the engine's serialization boundary. Plain
            # NA is the pattern already used elsewhere in the Lab suite for
            # "not computable" numeric table cells (e.g. AssumptionsLab's
            # multcheck.b.R).
            .clean_na <- .fl_clean_na
            add_row <- function(name, val, interp, ci = c(NA_real_, NA_real_), p = NA_real_) {
                rows[[length(rows) + 1L]] <<- list(
                    coefficient = name, value = .clean_na(val), interpretation = interp,
                    ci_lower = .clean_na(ci[1]), ci_upper = .clean_na(ci[2]), p_value = .clean_na(p))
            }

            # max_b caps replicate count independently of the user's
            # bootstrapSamples for statistics too slow per-call to bootstrap
            # at the full default (1000) without risking the real jamovi
            # engine's resource watchdog -- kappam.fleiss/kripp.alpha each
            # measured ~0.12 sec/replicate on a 450-row x 4-rater nominal
            # table, so 1000 replicates of EACH (run sequentially) is ~4
            # minutes just for these two CIs. Mirrors the same fix already
            # applied to internalConsistency's Omega bootstrap.
            # ES: max_b limita las réplicas independientemente de
            # bootstrapSamples para estadísticos demasiado lentos por
            # llamada como para hacer bootstrap con el valor completo por
            # defecto (1000) sin arriesgar el vigilante de recursos del
            # motor real de jamovi -- kappam.fleiss/kripp.alpha midieron
            # ~0.12 seg/réplica en una tabla nominal de 450 filas x 4
            # jueces, así que 1000 réplicas de CADA UNO (secuencial) son
            # ~4 minutos solo para estos dos IC. Replica el mismo arreglo
            # ya aplicado al bootstrap del Omega en internalConsistency.
            B <- if (isTRUE(opt$bootstrapCi)) opt$bootstrapSamples else 0L
            boot_ci <- function(stat_fn, max_b = NULL) {
                if (B <= 0L) return(c(NA_real_, NA_real_))
                B_i <- if (!is.null(max_b)) min(B, max_b) else B
                bt <- private$.bootstrap(df, stat_fn, B_i)
                c(bt$lo, bt$hi)
            }

            kappa_val <- gwet_val <- krip_val <- icc_c_val <- icc_a_val <- NA_real_
            krip_row_idx <- NULL

            # ── 5a. Kappa (nominal: Cohen/Fleiss; ordinal: weighted Cohen/Fleiss) ─
            if (opt$kappa && k >= 2L) {
                if (level == "continuous") {
                    add_row(tr("Kappa", "Kappa"), NA_real_, tr("Not applicable to continuous data; see ICC below",
                                                                "No aplica a datos continuos; ver ICC abajo"))
                } else {
                    weight_arg <- if (level == "ordinal") "squared" else "unweighted"
                    if (k == 2L) {
                        kres <- tryCatch(irr::kappa2(df, weight = weight_arg), error = function(e) NULL)
                        kname <- if (level == "ordinal") tr("Weighted Cohen's Kappa", "Kappa Ponderado de Cohen")
                                 else tr("Cohen's Kappa", "Kappa de Cohen")
                        kfn <- function(d) tryCatch(irr::kappa2(d, weight = weight_arg)$value, error = function(e) NA_real_)
                    } else {
                        kres <- tryCatch(irr::kappam.fleiss(df), error = function(e) NULL)
                        kname <- if (level == "ordinal") tr("Fleiss' Kappa (unweighted)", "Kappa de Fleiss (sin ponderar)")
                                 else tr("Fleiss' Kappa", "Kappa de Fleiss")
                        kfn <- function(d) tryCatch(irr::kappam.fleiss(d)$value, error = function(e) NA_real_)
                    }
                    if (!is.null(kres)) {
                        kappa_val <- kres$value
                        add_row(kname, kappa_val, private$.interp_kappa(kappa_val), boot_ci(kfn, max_b = 200L), kres$p.value)
                    }
                }
            }

            # ── 5b. Gwet's AC1 (nominal) / AC2 (ordinal) ──────────────────────
            if (opt$gwet && k >= 2L && level != "continuous") {
                w <- if (level == "ordinal") "quadratic" else "unweighted"
                gres <- tryCatch(irrCAC::gwet.ac1.raw(df, weights = w), error = function(e) NULL)
                if (!is.null(gres)) {
                    est <- gres$est
                    gwet_val <- est$coeff.val
                    ci_txt <- gsub("[()]", "", est$conf.int)
                    ci_parts <- suppressWarnings(as.numeric(strsplit(ci_txt, ",")[[1]]))
                    gname <- if (level == "ordinal") "Gwet's AC2" else "Gwet's AC1"
                    add_row(gname, gwet_val, private$.interp_kappa(gwet_val),
                            if (length(ci_parts) == 2) ci_parts else c(NA_real_, NA_real_), est$p.value)
                }
            }

            # ── 5c. Krippendorff's Alpha (universal; bootstrapped, no native CI/p) ─
            # EN: kripp.alpha()'s interval method (used for continuous data)
            # builds a coincidence matrix over every DISTINCT observed value
            # -- 1257 of them on a 423-row/3-rater continuous test case, vs.
            # a handful of categories for nominal/ordinal data -- and calls
            # lpSolve for an actual linear-programming step. Measured cost:
            # ~0.12 sec/replicate for nominal/ordinal (max_b=200 is fine,
            # ~20-30 sec total) vs. ~5 sec/replicate for continuous (200
            # replicates would be ~17 minutes -- exactly what hung the real
            # jamovi engine with all five coefficients + bootstrap enabled
            # together). Bootstrapping this one specifically is skipped for
            # continuous data instead of just shrinking max_b, since even a
            # handful of replicates costs real time while .bootstrap()'s own
            # <10-valid-replicate floor means a small cap could never
            # produce a usable CI anyway.
            # ES: El método interval de kripp.alpha() (usado para datos
            # continuos) construye una matriz de coincidencia sobre cada
            # valor observado DISTINTO -- 1257 en un caso de prueba continuo
            # de 423 filas/3 jueces, frente a un puñado de categorías en
            # datos nominales/ordinales -- y llama a lpSolve para un paso
            # real de programación lineal. Costo medido: ~0.12 seg/réplica
            # para nominal/ordinal (max_b=200 está bien, ~20-30 seg en
            # total) frente a ~5 seg/réplica para continuo (200 réplicas
            # serían ~17 minutos -- justo lo que colgó el motor real de
            # jamovi con los cinco coeficientes + bootstrap activados a la
            # vez). El bootstrap de este coeficiente se omite para datos
            # continuos en vez de solo reducir max_b, ya que incluso pocas
            # réplicas cuestan tiempo real y el piso de <10 réplicas válidas
            # de .bootstrap() nunca produciría un IC utilizable con un
            # límite pequeño de todos modos.
            if (opt$krippendorff && k >= 2L) {
                method <- switch(level, nominal = "nominal", ordinal = "ordinal", continuous = "interval")
                dsrc <- if (level == "nominal") df else df_num
                kfn <- function(d) {
                    m <- tryCatch(t(as.matrix(d)), error = function(e) NULL)
                    if (is.null(m)) return(NA_real_)
                    suppressWarnings(tryCatch(irr::kripp.alpha(m, method = method)$value, error = function(e) NA_real_))
                }
                ka_val <- kfn(dsrc)
                interp <- if (level == "nominal" || level == "ordinal") private$.interp_kappa(ka_val) else private$.interp_rel(ka_val)
                # Keep the table/plot label short -- a full explanatory
                # sentence here once made the table render very wide and
                # clipped the forest plot's title/axis, since this same
                # string doubles as its y-axis category label. The reason
                # is attached as a real jamovi footnote (see step 6 below)
                # instead of being folded into the label itself.
                # ES: Mantener la etiqueta de tabla/gráfico corta -- una
                # oración explicativa completa aquí hacía que la tabla se
                # viera muy ancha y recortaba el título/eje del forest plot,
                # ya que este mismo texto también es su etiqueta de
                # categoría en el eje Y. El motivo se adjunta como una nota
                # al pie real de jamovi (ver paso 6 abajo) en vez de
                # incluirse en la etiqueta misma.
                add_row(tr("Krippendorff's α", "α de Krippendorff"), ka_val, interp,
                        if (level == "continuous") c(NA_real_, NA_real_) else boot_ci(kfn, max_b = 200L), NA_real_)
                if (level == "continuous") krip_row_idx <- length(rows)
                krip_val <- ka_val
            }

            # ── 5d. ICC — consistency & absolute agreement (continuous) ──────
            if (opt$icc && level == "continuous" && k >= 2L) {
                ic <- tryCatch(irr::icc(df_num, model = "twoway", type = "consistency", unit = "single"), error = function(e) NULL)
                ia <- tryCatch(irr::icc(df_num, model = "twoway", type = "agreement",   unit = "single"), error = function(e) NULL)
                if (!is.null(ic)) {
                    icc_c_val <- ic$value
                    add_row(tr("ICC (consistency)", "ICC (consistencia)"), icc_c_val, private$.interp_rel(icc_c_val),
                            c(ic$lbound, ic$ubound), ic$p.value)
                }
                if (!is.null(ia)) {
                    icc_a_val <- ia$value
                    add_row(tr("ICC (absolute agreement)", "ICC (acuerdo absoluto)"), icc_a_val, private$.interp_rel(icc_a_val),
                            c(ia$lbound, ia$ubound), ia$p.value)
                }
            } else if (opt$icc && level != "continuous") {
                add_row("ICC", NA_real_, tr("Requires continuous ratings", "Requiere calificaciones continuas"))
            }

            # ── 5d-bis. ICC assumptions check (normality, homoscedasticity,
            # linearity/additivity) plus a sample-size adequacy note. Only
            # meaningful for the ANOVA-based ICC -- Kappa/Gwet/Krippendorff/
            # Kendall's W are rank- or category-based and distribution-free,
            # so this block (and its "which other coefficients are
            # affected" paragraph) only runs alongside ICC.
            # ES: Verificación de supuestos del ICC (normalidad,
            # homocedasticidad, linealidad/aditividad) más una nota de
            # adecuación del tamaño muestral. Solo tiene sentido para el
            # ICC basado en ANOVA -- Kappa/Gwet/Krippendorff/W de Kendall
            # son de rango o categoría y libres de distribución, por lo que
            # este bloque (y su párrafo "qué otros coeficientes se afectan")
            # solo corre junto al ICC.
            if (opt$icc && isTRUE(opt$checkIccAssumptions) && level == "continuous" && k >= 2L) {
                long <- data.frame(
                    value   = as.vector(as.matrix(df_num)),
                    subject = factor(rep(seq_len(n), times = k)),
                    rater   = factor(rep(names(df_num), each = n)))
                fit  <- tryCatch(stats::aov(value ~ subject + rater, data = long), error = function(e) NULL)
                resd <- if (!is.null(fit)) stats::residuals(fit) else NULL

                sw  <- if (!is.null(resd) && length(resd) >= 3L && length(resd) <= 5000L)
                    tryCatch(stats::shapiro.test(resd), error = function(e) NULL) else NULL
                lev <- private$.levene_manual(long$value, long$rater)
                tuk <- private$.tukey_nonadditivity(df_num)

                verdict <- function(p) {
                    if (is.null(p) || is.na(p)) tr("N/A", "N/D")
                    else if (p < .05) tr("Violated", "Violado")
                    else tr("Met", "Cumplido")
                }

                assum_rows <- list(
                    list(assumption = tr("Normality of residuals", "Normalidad de residuos"),
                         test = "Shapiro-Wilk",
                         statistic = .clean_na(if (!is.null(sw)) unname(sw$statistic) else NA_real_),
                         df = "",
                         p_value = .clean_na(if (!is.null(sw)) sw$p.value else NA_real_),
                         verdict = verdict(if (!is.null(sw)) sw$p.value else NA_real_)),
                    list(assumption = tr("Homoscedasticity across raters", "Homocedasticidad entre jueces"),
                         test = "Levene",
                         statistic = .clean_na(lev$stat),
                         df = if (!is.na(lev$df1)) paste0(lev$df1, ", ", lev$df2) else "",
                         p_value = .clean_na(lev$p),
                         verdict = verdict(lev$p)),
                    list(assumption = tr("Linearity (subject × rater additivity)", "Linealidad (aditividad sujeto × juez)"),
                         test = tr("Tukey's non-additivity", "No aditividad de Tukey"),
                         statistic = .clean_na(tuk$stat),
                         df = if (!is.na(tuk$df2)) paste0(tuk$df1, ", ", tuk$df2) else "",
                         p_value = .clean_na(tuk$p),
                         verdict = verdict(tuk$p)))

                iat <- self$results$iccAssumptionsTable
                private$.reset_table(iat, length(assum_rows))
                for (i in seq_along(assum_rows)) iat$setRow(rowNo = i, values = assum_rows[[i]])

                n_flag <- if (n < 30L)
                    tr("small (n &lt; 30) — expect wide, imprecise ICC confidence intervals",
                       "pequeña (n &lt; 30) — espere intervalos de confianza del ICC amplios e imprecisos")
                else if (n < 50L)
                    tr("borderline — adequate for a preliminary estimate, but more subjects would tighten the CI for publication",
                       "límite — adecuada para una estimación preliminar, pero más sujetos estrecharían el IC para publicación")
                else
                    tr("adequate for a reasonably precise ICC estimate under common guidelines",
                       "adecuada para una estimación de ICC razonablemente precisa según las guías comunes")

                sw_bad  <- !is.null(sw)  && !is.na(sw$p.value)  && sw$p.value  < .05
                lev_bad <- !is.na(lev$p) && lev$p < .05
                tuk_bad <- !is.na(tuk$p) && tuk$p < .05

                note_html <- paste0(.fl_prose_open(),
                    "<p>", tr(
                        "The ICC is a variance-partition of the same subject &times; rater ANOVA that its confidence interval is computed from (Shrout &amp; Fleiss, 1979), so it inherits that model's usual assumptions. Unlike Kappa, Gwet's AC1/AC2, Krippendorff's &alpha;, and Kendall's W above (which are rank- or category-based and distribution-free), the ICC's point estimate and especially its F-distribution-based CI are sensitive to non-normal residuals, unequal error variance across raters, and a non-additive (curvilinear) rater relationship.",
                        "El ICC es una partición de varianza del mismo ANOVA sujeto &times; juez del que se calcula su intervalo de confianza (Shrout &amp; Fleiss, 1979), por lo que hereda los supuestos usuales de ese modelo. A diferencia del Kappa, el AC1/AC2 de Gwet, el &alpha; de Krippendorff y la W de Kendall de arriba (de rango o categoría y libres de distribución), el valor puntual del ICC y sobre todo su IC basado en la distribución F son sensibles a residuos no normales, varianza de error desigual entre jueces, y una relación entre jueces no aditiva (curvilínea)."
                    ), "</p>",
                    "<p>", if (sw_bad) paste0("&#9888; <b>", tr("Normality violated", "Normalidad violada"), ":</b> ",
                        tr("residuals depart from normality (Shapiro-Wilk p &lt; .05). The ICC point estimate is fairly robust to mild non-normality, but its confidence interval is not — treat the reported CI with caution and prefer the bootstrap CIs already computed above for the other coefficients when reporting precision.",
                           "los residuos se desvían de la normalidad (Shapiro-Wilk p &lt; .05). El valor puntual del ICC es razonablemente robusto a desviaciones leves de la normalidad, pero su intervalo de confianza no — trate el IC reportado con cautela y prefiera los IC por bootstrap ya calculados arriba para los otros coeficientes al reportar precisión."))
                        else paste0("&#10003; ", tr("No evidence against normality of residuals (Shapiro-Wilk p &ge; .05).",
                                                     "No hay evidencia contra la normalidad de los residuos (Shapiro-Wilk p &ge; .05).")), "</p>",
                    "<p>", if (lev_bad) paste0("&#9888; <b>", tr("Homoscedasticity violated", "Homocedasticidad violada"), ":</b> ",
                        tr("error variance differs across raters (Levene p &lt; .05) — at least one rater is far more (or less) internally consistent than the others across the score range, which biases the ICC's standard error and can invalidate its CI. Check the Rater Mean Score diagnostic plot above for which rater stands out.",
                           "la varianza de error difiere entre jueces (Levene p &lt; .05) — al menos un juez es mucho más (o menos) consistente internamente que los demás en el rango de puntajes, lo que sesga el error estándar del ICC y puede invalidar su IC. Revise el gráfico de diagnóstico de Puntaje Medio por Juez arriba para ver qué juez se distingue."))
                        else paste0("&#10003; ", tr("No evidence against equal error variance across raters (Levene p &ge; .05).",
                                                     "No hay evidencia contra la igualdad de varianza de error entre jueces (Levene p &ge; .05).")), "</p>",
                    "<p>", if (tuk_bad) paste0("&#9888; <b>", tr("Linearity/additivity violated", "Linealidad/aditividad violada"), ":</b> ",
                        tr("Tukey's test detects a subject &times; rater interaction (p &lt; .05): at least one rater's scores curve relative to the others rather than following the same straight-line relationship the ICC model assumes. This is the most severe of the three violations — the ICC will systematically understate true agreement when it happens. Inspect a rater-by-rater scatterplot for a curved (not straight-line) pattern before trusting the ICC value above; Kendall's W, already available in this analysis, is a rank-based alternative that does not assume linearity.",
                           "la prueba de Tukey detecta una interacción sujeto &times; juez (p &lt; .05): las puntuaciones de al menos un juez se curvan respecto a las de los demás en vez de seguir la misma relación lineal que asume el modelo del ICC. Esta es la más grave de las tres violaciones — el ICC subestimará sistemáticamente el acuerdo real cuando ocurre. Inspeccione un diagrama de dispersión juez contra juez en busca de un patrón curvo (no lineal) antes de confiar en el valor de ICC de arriba; la W de Kendall, ya disponible en este análisis, es una alternativa basada en rangos que no asume linealidad."))
                        else paste0("&#10003; ", tr("No evidence of a subject &times; rater interaction — the additivity/linearity assumption holds (Tukey p &ge; .05).",
                                                     "No hay evidencia de interacción sujeto &times; juez — se cumple el supuesto de aditividad/linealidad (Tukey p &ge; .05).")), "</p>",
                    "<p><b>", tr("Sample size", "Tamaño de muestra"), ":</b> ",
                    tr(paste0("n = ", n, " subjects rated by k = ", k, " raters — this is "),
                       paste0("n = ", n, " sujetos calificados por k = ", k, " jueces — esto es ")),
                    n_flag, ". ",
                    tr("A larger n is also the best protection against non-normality: with more subjects, F-based inference in the underlying ANOVA becomes more robust to mild departures, though bootstrap CIs remain the safer choice whenever a violation above is flagged (Koo &amp; Li, 2016; Bujang &amp; Baharum, 2017).",
                       "Un n mayor es también la mejor protección contra la no normalidad: con más sujetos, la inferencia basada en F del ANOVA subyacente se vuelve más robusta a desviaciones leves, aunque los IC por bootstrap siguen siendo la opción más segura cuando se marca una violación arriba (Koo &amp; Li, 2016; Bujang &amp; Baharum, 2017)."),
                    "</p>",
                    "</div>")
                self$results$iccAssumptionsNote$setContent(note_html)
            } else {
                self$results$iccAssumptionsTable$setVisible(FALSE)
                self$results$iccAssumptionsNote$setVisible(FALSE)
            }

            # ── 5e. Kendall's W (ordinal rankings / continuous scores) ───────
            if (opt$kendallW && k >= 2L && level != "nominal") {
                kw <- tryCatch(irr::kendall(df_num), error = function(e) NULL)
                if (!is.null(kw)) {
                    note <- if (nzchar(kw$error %||% "")) paste0(" (", tr("ties detected", "empates detectados"), ")") else ""
                    # irr::kendall() is cheap per call (~0.03 sec/replicate
                    # measured), unlike Krippendorff's interval method --
                    # the full bootstrapSamples cap (200) is fine here.
                    kwfn <- function(d) tryCatch(irr::kendall(d)$value, error = function(e) NA_real_)
                    add_row(paste0("Kendall's W", note), kw$value, private$.interp_rel(kw$value), boot_ci(kwfn, max_b = 200L), kw$p.value)
                }
            } else if (opt$kendallW && level == "nominal") {
                add_row("Kendall's W", NA_real_, tr("Not applicable to nominal data", "No aplica a datos nominales"))
            }

            # ── 6. Write mainTable ────────────────────────────────────────────
            mt <- self$results$mainTable
            private$.reset_table(mt, length(rows))
            for (i in seq_along(rows)) mt$setRow(rowNo = i, values = rows[[i]])
            if (!is.null(krip_row_idx))
                mt$addFootnote(col = "value", rowNo = krip_row_idx, note = tr(
                    "CI omitted: bootstrapping Krippendorff's α on near-continuous data is too costly to run by default.",
                    "IC omitido: calcular el IC de α de Krippendorff por bootstrap en datos casi continuos es muy costoso para hacerlo por defecto."))

            # Data for .plotComparison: only rows with a real value (skip
            # "not applicable" placeholders like the ICC row on nominal data).
            plot_rows <- Filter(function(r) !is.na(r$value), rows)
            if (length(plot_rows) > 0L) {
                private$.plot_rows <- data.frame(
                    coefficient = vapply(plot_rows, function(r) r$coefficient, character(1)),
                    value       = vapply(plot_rows, function(r) r$value, numeric(1)),
                    ci_lower    = vapply(plot_rows, function(r) if (is.na(r$ci_lower)) r$value else r$ci_lower, numeric(1)),
                    ci_upper    = vapply(plot_rows, function(r) if (is.na(r$ci_upper)) r$value else r$ci_upper, numeric(1)),
                    stringsAsFactors = FALSE)
            } else {
                self$results$plotComparison$setVisible(FALSE)
            }

            # ── 7. Discordance panel ──────────────────────────────────────────
            discord_html <- ""
            if (level %in% c("nominal", "ordinal") && !is.na(kappa_val) && !is.na(gwet_val)) {
                gap <- abs(kappa_val - gwet_val)
                gname <- if (level == "ordinal") "AC2" else "AC1"
                if (gap > .05) {
                    discord_html <- paste0("<p>&#9888; <b>", tr("Coefficients disagree", "Los coeficientes no coinciden"), ":</b> ",
                        tr(paste0("Kappa (", round(kappa_val, 3), ") and Gwet's ", gname, " (", round(gwet_val, 3),
                                  ") differ by more than .05. This is the classic “Kappa paradox”: Kappa penalizes agreement heavily when one category is much more common than the others, even when raters are genuinely consistent (Gwet, 2014). Trust Gwet's ", gname, " over Kappa here."),
                           paste0("El Kappa (", round(kappa_val, 3), ") y el ", gname, " de Gwet (", round(gwet_val, 3),
                                  ") difieren en más de .05. Esta es la clásica “paradoja del Kappa”: el Kappa penaliza fuertemente el acuerdo cuando una categoría es mucho más común que las demás, aun cuando los jueces son genuinamente consistentes (Gwet, 2014). Confíe en el ", gname, " de Gwet sobre el Kappa aquí.")),
                        "</p>")
                } else {
                    discord_html <- paste0("<p>&#10003; ", tr("Kappa and Gwet's coefficient agree closely — no evidence of a prevalence-driven paradox here.",
                                                               "El Kappa y el coeficiente de Gwet coinciden de cerca — no hay evidencia de una paradoja por prevalencia aquí."), "</p>")
                }
            } else if (level == "continuous" && !is.na(icc_c_val) && !is.na(icc_a_val)) {
                gap <- icc_c_val - icc_a_val
                if (gap > .05) {
                    discord_html <- paste0("<p>&#9888; <b>", tr("ICC forms disagree", "Las formas de ICC no coinciden"), ":</b> ",
                        tr(paste0("Consistency-type ICC (", round(icc_c_val, 3), ") is notably higher than absolute-agreement-type ICC (",
                                  round(icc_a_val, 3), "). This pattern indicates at least one rater has a systematic mean bias (shifted scores) even though raters rank cases similarly (Shrout &amp; Fleiss, 1979). Report the absolute-agreement form if raters' raw scores are meant to be used interchangeably."),
                           paste0("El ICC tipo consistencia (", round(icc_c_val, 3), ") es notablemente mayor que el ICC tipo acuerdo absoluto (",
                                  round(icc_a_val, 3), "). Este patrón indica que al menos un juez tiene un sesgo sistemático de media (puntajes desplazados) aunque los jueces ordenan los casos de forma similar (Shrout &amp; Fleiss, 1979). Reporte la forma de acuerdo absoluto si los puntajes brutos de los jueces se van a usar de forma intercambiable.")),
                        "</p>")
                } else {
                    discord_html <- paste0("<p>&#10003; ", tr("Consistency and absolute-agreement ICC agree closely — no evidence of systematic rater bias.",
                                                               "El ICC de consistencia y el de acuerdo absoluto coinciden de cerca — no hay evidencia de sesgo sistemático de jueces."), "</p>")
                }
            }
            self$results$discordanceNote$setContent(if (nzchar(discord_html)) .fl_prose(discord_html) else discord_html)
            if (!nzchar(discord_html)) self$results$discordanceNote$setVisible(FALSE)

            # ── 8. Data-grounded diagnostics for "Why" ────────────────────────
            # EN: Computed once here so both "Why" and "What to do now" can
            # ground their text in the actual pattern found in THIS data
            # (category prevalence + which rater deviates most from the
            # rest, for nominal/ordinal; per-rater mean bias, for
            # continuous) instead of restating design metadata already
            # shown in the Data Summary panel above.
            # ES: Calculado una sola vez aquí para que "Por qué" y "Qué
            # hacer ahora" ancoren su texto en el patrón real de ESTOS
            # datos (prevalencia de categoría + qué juez se desvía más del
            # resto, para nominal/ordinal; sesgo de media por juez, para
            # continuo) en vez de repetir metadatos de diseño que ya se
            # muestran en el panel de Resumen de Datos de arriba.
            top_cat <- top_pct <- NULL
            worst_rater <- worst_pct <- others_pct <- rater_gap <- NULL
            has_outlier_rater <- FALSE
            biased_rater <- bias_gap <- NULL
            has_biased_rater <- FALSE

            if (level %in% c("nominal", "ordinal") && k >= 2L) {
                all_vals <- unlist(lapply(df, as.character), use.names = FALSE)
                prev_tab <- sort(prop.table(table(all_vals)), decreasing = TRUE)
                top_cat  <- names(prev_tab)[1]
                top_pct  <- round(unname(prev_tab[1]) * 100, 1)

                pair_idx  <- combn(seq_len(k), 2)
                pairwise  <- apply(pair_idx, 2, function(ij) mean(df[[ij[1]]] == df[[ij[2]]]))
                rater_avg <- vapply(seq_len(k), function(j) {
                    in_pair <- apply(pair_idx, 2, function(ij) j %in% ij)
                    mean(pairwise[in_pair])
                }, numeric(1))
                names(rater_avg) <- names(df)
                worst_i     <- which.min(rater_avg)
                worst_rater <- names(rater_avg)[worst_i]
                worst_pct   <- round(rater_avg[worst_i] * 100, 1)
                others_pct  <- round(mean(rater_avg[-worst_i]) * 100, 1)
                rater_gap   <- round(others_pct - worst_pct, 1)
                has_outlier_rater <- rater_gap > 8

                private$.diag_type <- "prevalence"
                private$.diag_data <- data.frame(
                    category = names(prev_tab), pct = as.numeric(unname(prev_tab)) * 100,
                    stringsAsFactors = FALSE)
            } else if (level == "continuous" && k >= 2L) {
                rater_means <- colMeans(df_num, na.rm = TRUE)
                grand_mean  <- mean(rater_means)
                worst_i     <- which.max(abs(rater_means - grand_mean))
                biased_rater<- names(rater_means)[worst_i]
                bias_gap    <- round(rater_means[worst_i] - grand_mean, 1)
                has_biased_rater <- abs(bias_gap) > 3

                private$.diag_type  <- "rater_mean"
                private$.diag_data  <- data.frame(
                    rater = names(rater_means), mean = unname(rater_means),
                    stringsAsFactors = FALSE)
                private$.diag_extra <- grand_mean
            } else {
                self$results$plotDiagnostic$setVisible(FALSE)
            }

            # ── 9. Interpretation & recommendations (four questions) ─────────
            primary_val <- if (!is.na(kappa_val)) kappa_val
                           else if (!is.na(gwet_val)) gwet_val
                           else if (!is.na(icc_a_val)) icc_a_val
                           else if (!is.na(icc_c_val)) icc_c_val
                           else krip_val
            primary_lbl <- if (level == "continuous") private$.interp_rel(primary_val) else private$.interp_kappa(primary_val)
            has_discordance <- (level %in% c("nominal","ordinal") && !is.na(kappa_val) && !is.na(gwet_val) && abs(kappa_val - gwet_val) > .05) ||
                                (level == "continuous" && !is.na(icc_c_val) && !is.na(icc_a_val) && (icc_c_val - icc_a_val) > .05)
            band_is_weak <- level != "continuous" && !is.na(primary_val) && primary_val <= .40

            # ---- What happened: the mechanics, not just the number ---------
            happened_html <- if (level %in% c("nominal", "ordinal")) paste0(
                "<p>", tr(
                    paste0("Three estimators of the same question — how much do raters agree beyond chance? — were computed on the same ", k, " raters and ", n, " cases: ",
                           if (!is.na(kappa_val)) paste0("Kappa (", round(kappa_val,3), "), ") else "",
                           if (!is.na(gwet_val))  paste0("Gwet's coefficient (", round(gwet_val,3), "), ") else "",
                           if (!is.na(krip_val))  paste0("and Krippendorff's α (", round(krip_val,3), ")") else "",
                           ". They can legitimately disagree because each one defines “agreement expected by chance” differently: Kappa (Cohen, 1960 for 2 raters; Fleiss, 1971 for more than 2) derives it from the observed marginal distribution of each category, which makes it highly sensitive to skewed prevalence; Gwet's AC1/AC2 (Gwet, 2014) was designed specifically to not degrade under that same skew; and Krippendorff's α (Krippendorff, 2018) generalizes the calculation across any measurement level and tolerates missing data."),
                    paste0("Se calcularon tres estimadores de la misma pregunta —¿qué tanto concuerdan los jueces más allá del azar?— sobre los mismos ", k, " jueces y ", n, " casos: ",
                           if (!is.na(kappa_val)) paste0("Kappa (", round(kappa_val,3), "), ") else "",
                           if (!is.na(gwet_val))  paste0("el coeficiente de Gwet (", round(gwet_val,3), "), ") else "",
                           if (!is.na(krip_val))  paste0("y el α de Krippendorff (", round(krip_val,3), ")") else "",
                           ". Pueden diferir legítimamente porque cada uno define de forma distinta el “acuerdo esperado por azar”: el Kappa (Cohen, 1960 para 2 jueces; Fleiss, 1971 para más de 2) lo deriva de la distribución marginal observada de cada categoría, lo cual lo hace muy sensible a la prevalencia sesgada; el AC1/AC2 de Gwet (Gwet, 2014) fue diseñado específicamente para no degradarse ante esa misma prevalencia sesgada; y el α de Krippendorff (Krippendorff, 2018) generaliza el cálculo a cualquier nivel de medida y tolera datos faltantes.")
                ), "</p>",
                if (has_discordance) paste0("<p>", tr(
                    paste0("In this case the gap between Kappa and Gwet's coefficient (", round(abs(kappa_val-gwet_val),3), ") is large enough that the choice of formula changes the headline conclusion — see the discordance diagnosis above the tables."),
                    paste0("En este caso la brecha entre el Kappa y el coeficiente de Gwet (", round(abs(kappa_val-gwet_val),3), ") es suficientemente grande como para que la elección de fórmula cambie la conclusión principal — ver el diagnóstico de discordancia sobre las tablas.")
                ), "</p>") else paste0("<p>", tr(
                    "The estimators agree closely here, so the choice of formula does not change the substantive conclusion.",
                    "Los estimadores coinciden de cerca aquí, así que la elección de fórmula no cambia la conclusión sustantiva."
                ), "</p>")
            ) else paste0(
                "<p>", tr(
                    paste0("Reliability was assessed via two ICC forms and Krippendorff's interval α on the same ", k, " raters and ", n, " cases. ICC (consistency) asks whether raters RANK cases similarly, tolerating a rater who is systematically higher or lower than the others; ICC (absolute agreement) additionally requires their raw VALUES to be interchangeable (Shrout &amp; Fleiss, 1979)."),
                    paste0("La confiabilidad se evaluó mediante dos formas de ICC y el α de intervalo de Krippendorff sobre los mismos ", k, " jueces y ", n, " casos. El ICC (consistencia) pregunta si los jueces ORDENAN los casos de forma similar, tolerando un juez sistemáticamente más alto o más bajo que los demás; el ICC (acuerdo absoluto) exige además que sus VALORES brutos sean intercambiables (Shrout &amp; Fleiss, 1979).")
                ), "</p>",
                if (has_discordance) paste0("<p>", tr(
                    paste0("Here the gap between the two ICC forms (", round(abs(icc_c_val-icc_a_val),3), ") is large enough to matter — see the discordance diagnosis above the tables."),
                    paste0("Aquí la brecha entre las dos formas de ICC (", round(abs(icc_c_val-icc_a_val),3), ") es suficientemente grande como para importar — ver el diagnóstico de discordancia sobre las tablas.")
                ), "</p>") else paste0("<p>", tr(
                    "Both ICC forms agree closely here, so there is no evidence that rater interchangeability is a concern.",
                    "Ambas formas de ICC coinciden de cerca aquí, así que no hay evidencia de que la intercambiabilidad de jueces sea un problema."
                ), "</p>")
            )

            # ---- Why: grounded in THIS data's pattern, not design metadata --
            why_html <- if (level %in% c("nominal", "ordinal")) paste0(
                "<p>", tr(
                    paste0("Category “", top_cat, "” accounts for ", top_pct, "% of every rating issued across all ", k, " raters — a markedly uneven distribution. This is exactly the condition under which Kappa becomes conservative: when nearly every case falls in one category, the “chance agreement” Kappa subtracts is already high, leaving little room for Kappa to register genuine agreement even when raters are truly consistent (Gwet, 2014). AC1/AC2 and Krippendorff's α are not penalized this way."),
                    paste0("La categoría “", top_cat, "” concentra el ", top_pct, "% de todas las calificaciones emitidas entre los ", k, " jueces — una distribución marcadamente desigual. Esta es exactamente la condición bajo la cual el Kappa se vuelve conservador: cuando casi todos los casos caen en una sola categoría, el “acuerdo por azar” que el Kappa resta ya es alto de por sí, dejando poco margen para que el Kappa registre acuerdo genuino aun cuando los jueces sean realmente consistentes (Gwet, 2014). El AC1/AC2 y el α de Krippendorff no se penalizan de esta forma.")
                ), "</p>",
                if (has_outlier_rater) paste0("<p>", tr(
                    paste0("Rater “", worst_rater, "” also stands out: their average pairwise agreement with the other raters is ", worst_pct, "%, versus ", others_pct, "% among the remaining rater pairs — a ", rater_gap, "-point gap. This points to that specific rater applying a different criterion, rather than a problem with the measurement design itself."),
                    paste0("El juez “", worst_rater, "” también se distingue: su acuerdo promedio por pares con los demás jueces es ", worst_pct, "%, frente a ", others_pct, "% entre el resto de pares de jueces — una brecha de ", rater_gap, " puntos porcentuales. Esto apunta a que ese juez en particular está aplicando un criterio distinto, más que a un problema del diseño de medición en sí.")
                ), "</p>") else paste0("<p>", tr(
                    "No single rater stands out from the rest — pairwise agreement is fairly even across all rater pairs, so the pattern found here reflects the measurement design (category prevalence) rather than one rater's individual behavior.",
                    "Ningún juez se distingue del resto — el acuerdo por pares es bastante parejo entre todos los pares de jueces, así que el patrón encontrado aquí refleja el diseño de medición (prevalencia de categoría) más que el comportamiento individual de un juez."
                ), "</p>")
            ) else paste0(
                "<p>", tr(
                    paste0("Rater “", biased_rater, "” averages ", round(rater_means[worst_i],1), " points against an overall mean of ", round(grand_mean,1), " — a ", if(bias_gap>0) "+" else "", bias_gap, "-point systematic shift. This is exactly what produces a gap between the two ICC forms: raters agree on WHO scores higher or lower (preserving rank, hence high consistency-type ICC), but not on the exact MAGNITUDE of the score (hurting absolute-agreement-type ICC) (Shrout &amp; Fleiss, 1979)."),
                    paste0("El juez “", biased_rater, "” promedia ", round(rater_means[worst_i],1), " puntos frente a una media general de ", round(grand_mean,1), " — un desplazamiento sistemático de ", if(bias_gap>0) "+" else "", bias_gap, " puntos. Esto es exactamente lo que produce la brecha entre las dos formas de ICC: los jueces concuerdan en QUIÉN puntúa más alto o más bajo (preservando el orden, de ahí el ICC tipo consistencia alto), pero no en la MAGNITUD exacta del puntaje (perjudicando el ICC tipo acuerdo absoluto) (Shrout &amp; Fleiss, 1979).")
                ), "</p>")

            # ---- What it means: decision-relevant, not a "see above" pointer -
            implies_html <- if (level %in% c("nominal", "ordinal")) paste0(
                "<p>", tr(
                    paste0("A value of ", round(primary_val,3), " (", primary_lbl, ") on the Landis &amp; Koch (1977) scale means that, corrected for chance, rater agreement is ",
                           if (band_is_weak) "not yet strong enough to treat a single rater's classification — or even the consensus across raters — as trustworthy for consequential decisions (e.g. diagnosis, selection); a non-trivial share of individual classifications likely do not reflect a shared criterion."
                           else "strong enough to support using the raters' consensus classification (e.g. the modal category across raters) as a dependent variable with reasonable confidence."),
                    paste0("Un valor de ", round(primary_val,3), " (", primary_lbl, ") en la escala de Landis &amp; Koch (1977) significa que, corregido por azar, el acuerdo entre jueces es ",
                           if (band_is_weak) "todavía insuficiente para tratar la clasificación de un solo juez —o incluso el consenso entre jueces— como confiable para decisiones consecuentes (p. ej. diagnóstico, selección); una proporción no trivial de las clasificaciones individuales probablemente no refleja un criterio compartido."
                           else "suficientemente fuerte como para respaldar el uso de la clasificación consensuada de los jueces (p. ej. la categoría modal entre jueces) como variable dependiente con confianza razonable.")
                ), "</p>",
                if (has_discordance) paste0("<p>", tr(
                    "The Kappa/Gwet discordance is not a mere technicality: reporting only Kappa here would lead a reader to conclude the agreement is weaker than the rating pattern actually supports, given the observed category skew.",
                    "La discordancia Kappa/Gwet no es un simple tecnicismo: reportar solo el Kappa aquí llevaría a un lector a concluir que el acuerdo es más débil de lo que el patrón de calificaciones realmente respalda, dado el sesgo de categoría observado."
                ), "</p>") else ""
            ) else paste0(
                "<p>", tr(
                    paste0("An absolute-agreement ICC of ", round(icc_a_val,3), " means raters' raw scores can", if (!is.na(icc_a_val) && icc_a_val >= .75) "" else "not yet", " be used interchangeably (e.g. averaged into a single composite score) with reasonable confidence; the higher consistency-type ICC (", round(icc_c_val,3), ") means their RANKING of cases is more trustworthy than their absolute scale."),
                    paste0("Un ICC de acuerdo absoluto de ", round(icc_a_val,3), " significa que los puntajes brutos de los jueces ", if (!is.na(icc_a_val) && icc_a_val >= .75) "sí pueden" else "todavía no pueden", " usarse de forma intercambiable (p. ej. promediados en un puntaje compuesto único) con confianza razonable; el ICC tipo consistencia más alto (", round(icc_c_val,3), ") indica que su ORDENAMIENTO de los casos es más confiable que su escala absoluta.")
                ), "</p>")

            # ---- What to do now: concrete, data-driven methodological steps -
            action_items <- character(0)
            if (has_discordance && level %in% c("nominal","ordinal"))
                action_items <- c(action_items, tr("Report Gwet's coefficient (or Krippendorff's α) as the primary estimate, not Kappa — Kappa's value here is an artifact of category prevalence, not weaker rater agreement.",
                                                    "Reporte el coeficiente de Gwet (o el α de Krippendorff) como estimación primaria, no el Kappa — el valor del Kappa aquí es un artefacto de la prevalencia de categoría, no un acuerdo entre jueces más débil."))
            if (has_discordance && level == "continuous")
                action_items <- c(action_items, tr("Report ICC (absolute agreement) rather than ICC (consistency) if raters' raw scores must be interchangeable; if only relative ranking matters, the consistency form is the right one to report instead.",
                                                    "Reporte ICC (acuerdo absoluto) en vez de ICC (consistencia) si los puntajes brutos de los jueces deben ser intercambiables; si solo importa el orden relativo, la forma de consistencia es la correcta para reportar."))
            if (level %in% c("nominal","ordinal") && !is.na(top_pct) && (top_pct > 60 || has_discordance))
                action_items <- c(action_items, tr(paste0("The category system is heavily imbalanced (", top_pct, "% in one category). If more data will be collected, deliberately oversample the minority categories; if the categories themselves are the issue, consider whether they need to be redefined or collapsed."),
                                                    paste0("El sistema de categorías está muy desbalanceado (", top_pct, "% en una sola categoría). Si se recolectarán más datos, sobremuestree deliberadamente las categorías minoritarias; si las categorías en sí son el problema, considere si necesitan redefinirse o colapsarse.")))
            if (has_outlier_rater)
                action_items <- c(action_items, tr(paste0("Rater “", worst_rater, "” disagrees with the group more than the others do. Review that rater's criteria against the category definitions, consider a calibration/training session with shared examples, or add an adjudication step for cases where that rater's classification is the deciding vote."),
                                                    paste0("El juez “", worst_rater, "” discrepa del grupo más que los demás. Revise el criterio de ese juez frente a las definiciones de categoría, considere una sesión de calibración/entrenamiento con ejemplos compartidos, o agregue un paso de arbitraje para los casos en que la clasificación de ese juez sea el voto decisivo.")))
            if (has_biased_rater)
                action_items <- c(action_items, tr(paste0("Rater “", biased_rater, "” has a systematic mean shift (", if(bias_gap>0) "+" else "", bias_gap, " points). Consider a calibration session with shared anchor cases, or mean-center that rater's scores before combining them with the others."),
                                                    paste0("El juez “", biased_rater, "” tiene un desplazamiento sistemático de media (", if(bias_gap>0) "+" else "", bias_gap, " puntos). Considere una sesión de calibración con casos ancla compartidos, o centre la media de ese juez antes de combinar sus puntajes con los demás.")))
            if (band_is_weak)
                action_items <- c(action_items, tr("Overall agreement is Fair or worse: before using these ratings for any consequential decision, run a rater-training session with shared examples and explicit category anchors, then re-rate a fresh sample to confirm agreement improves.",
                                                    "El acuerdo general es Aceptable o peor: antes de usar estas calificaciones para cualquier decisión consecuente, realice una sesión de entrenamiento de jueces con ejemplos compartidos y anclas de categoría explícitas, y vuelva a calificar una muestra nueva para confirmar que el acuerdo mejora."))
            if (level == "ordinal" && k > 2L && opt$kappa)
                action_items <- c(action_items, tr("Weighted multi-rater Kappa is not available here for >2 raters; Krippendorff's ordinal α already accounts for the ordinal scale with any number of raters.",
                                                    "El Kappa ponderado multi-juez no está disponible aquí para >2 jueces; el α ordinal de Krippendorff ya considera la escala ordinal con cualquier número de jueces."))
            if (n < 30L)
                action_items <- c(action_items, tr(paste0("n = ", n, " is a small sample for agreement statistics; treat the confidence intervals above as wide."),
                                                    paste0("n = ", n, " es una muestra pequeña para estadísticos de acuerdo; trate los intervalos de confianza de arriba como amplios.")))
            if (length(action_items) == 0L)
                action_items <- c(tr("No specific corrective action indicated — the primary estimate can be reported as-is.",
                                      "No se indica ninguna acción correctiva específica — la estimación primaria puede reportarse tal cual."))

            action_html <- paste0("<ul style='line-height:1;'>", paste0("<li>", action_items, "</li>", collapse = ""), "</ul>")

            bands_html <- if (level %in% c("nominal", "ordinal")) paste0(
                "<table style='border-collapse:collapse;'>",
                "<tr><th style='padding:3px 8px;border:1px solid #ccc;'>", tr("Value", "Valor"), "</th><th style='padding:3px 8px;border:1px solid #ccc;'>", tr("Interpretation", "Interpretación"), "</th></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>&lt; 0</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Poor", "Pobre"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>0.00 – 0.20</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Slight", "Leve"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>0.21 – 0.40</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Fair", "Aceptable"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>0.41 – 0.60</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Moderate", "Moderado"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>0.61 – 0.80</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Substantial", "Sustancial"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>0.81 – 1.00</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Almost perfect", "Casi perfecto"), "</td></tr>",
                "</table>",
                "<p style='font-size:0.85em;color:#666;'>", tr("Landis &amp; Koch (1977) bands, applied to chance-corrected categorical agreement (Kappa, Gwet, Krippendorff on nominal/ordinal data).",
                                                              "Bandas de Landis &amp; Koch (1977), aplicadas al acuerdo categórico corregido por azar (Kappa, Gwet, Krippendorff en datos nominales/ordinales)."), "</p>"
            ) else ""

            # .fl_prose_open()/.fl_prose_close() (shared-helpers.R) wrap this
            # whole panel in Bibliography's own typographic convention (no
            # font-size override -- inherits jamovi's default, same as
            # Bibliography and the Fiability Library -- plus line-height: 1
            # and text-align: justify) so all four Html-producing modules
            # render body text identically instead of each hardcoding its
            # own size/spacing.
            # ES: .fl_prose_open()/.fl_prose_close() (shared-helpers.R)
            # envuelven todo este panel en la misma convención tipográfica
            # de Bibliography (sin sobreescribir font-size -- hereda el
            # predeterminado de jamovi, igual que Bibliography y la
            # Fiability Library -- más line-height: 1 y text-align: justify)
            # para que los cuatro módulos que producen Html rendericen el
            # texto corrido de forma idéntica en vez de que cada uno fije
            # su propio tamaño/espaciado.
            rec_html <- paste0(
                .fl_prose_open(),
                "<h4>", tr("What happened", "Qué pasó"), "</h4>",
                happened_html,
                "<h4>", tr("Why", "Por qué"), "</h4>",
                why_html,
                "<h4>", tr("What it means", "Qué implica"), "</h4>",
                implies_html,
                if (nzchar(bands_html)) paste0(
                    "<p style='margin-top:0.8em;font-weight:700;'>", tr("Interpretation Benchmarks", "Criterios de interpretación"), "</p>",
                    bands_html
                ) else "",
                "<h4>", tr("What to do now", "Qué hacer ahora"), "</h4>",
                action_html,
                "<p style='font-size:0.85em;color:#666;'>", tr(
                    "See Fiability Library → Inter-Rater Agreement for full definitions and assumptions, and Bibliography → Inter-Rater Reliability for references.",
                    "Vea Biblioteca de Confiabilidad → Acuerdo entre Jueces para definiciones y supuestos completos, y Bibliografía → Confiabilidad entre Jueces para las referencias."),
                "</p>",
                "</div>"
            )
            self$results$interpretation$setContent(rec_html)
        },

        # ── Plot: coefficient comparison (forest-plot style) ────────────────
        # EN: One point + CI whisker per computed coefficient -- the visual
        # counterpart of the discordance panel: a Kappa/Gwet or ICC-forms
        # gap that reads as one sentence in text is immediately visible
        # here as two dots sitting apart on the same 0-1 scale.
        # ES: Un punto + barra de error por coeficiente calculado -- la
        # contraparte visual del panel de discordancia: una brecha Kappa/
        # Gwet o entre formas de ICC que se lee como una oración en texto
        # es inmediatamente visible aquí como dos puntos separados en la
        # misma escala 0-1.
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

        # ── Plot: category prevalence (nominal/ordinal) or per-rater mean
        # (continuous) -- the visual counterpart of the "Why" diagnosis.
        # ES: Prevalencia de categoría (nominal/ordinal) o media por juez
        # (continuo) -- la contraparte visual del diagnóstico de "Por qué".
        .plotDiagnostic = function(image, ggtheme, theme, ...) {
            d <- private$.diag_data
            if (is.null(d) || nrow(d) == 0L) return(FALSE)
            tr <- private$.tr
            cols <- private$.plot_colors()
            if (identical(private$.diag_type, "prevalence")) {
                d$category <- factor(d$category, levels = rev(d$category))
                p <- ggplot2::ggplot(d, ggplot2::aes(x = pct, y = category)) +
                    ggplot2::geom_bar(stat = "identity", fill = cols$primary, alpha = .85, width = .6) +
                    ggplot2::labs(x = tr("% of all ratings", "% de todas las calificaciones"), y = NULL,
                                  title = tr("Category Prevalence", "Prevalencia de Categoría")) +
                    private$.plot_theme()
            } else if (identical(private$.diag_type, "rater_mean")) {
                grand_mean <- private$.diag_extra
                p <- ggplot2::ggplot(d, ggplot2::aes(x = rater, y = mean)) +
                    ggplot2::geom_bar(stat = "identity", fill = cols$primary, alpha = .85, width = .6) +
                    ggplot2::geom_hline(yintercept = grand_mean, linetype = "dashed",
                                        colour = cols$secondary, linewidth = .6) +
                    ggplot2::labs(x = NULL, y = tr("Mean score", "Puntaje medio"),
                                  title = tr("Rater Mean Score (dashed = grand mean)",
                                             "Puntaje Medio por Juez (línea punteada = media general)")) +
                    private$.plot_theme()
            } else return(FALSE)
            print(p)
            TRUE
        }
    )
)

`%||%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a
