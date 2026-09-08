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

        .tr = function(en, es) if (identical(self$options$reportLang, "es")) es else en,

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

        .bootstrap = function(df, stat_fn, B = 1000L) {
            n <- nrow(df)
            vals <- numeric(B)
            for (b in seq_len(B)) {
                idx <- sample.int(n, n, replace = TRUE)
                vals[b] <- tryCatch(stat_fn(df[idx, , drop = FALSE]), error = function(e) NA_real_)
            }
            vals <- vals[is.finite(vals)]
            if (length(vals) < 10L) return(list(lo = NA_real_, hi = NA_real_))
            list(lo = unname(quantile(vals, .025)), hi = unname(quantile(vals, .975)))
        },

        .reset_table = function(table, n_rows) {
            table$deleteRows()
            if (n_rows > 0L)
                for (i in seq_len(n_rows)) table$addRow(rowKey = i)
            invisible(table)
        },

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
                self$results$discordanceNote$setVisible(FALSE)
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
                self$results$discordanceNote$setVisible(FALSE)
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

            auto_html <- paste0(
                "<table style='border-collapse:collapse;font-size:13px;'>",
                "<tr><td style='padding:4px 10px;'><b>", tr("Raters", "Jueces"), "</b></td><td>", k, "</td></tr>",
                "<tr><td style='padding:4px 10px;'><b>", tr("Complete cases", "Casos completos"), "</b></td><td>", n,
                if (n_miss > 0) paste0(" <span style='color:orange;'>(", n_miss, " ", tr("removed listwise", "eliminados listwise"), ")</span>") else "",
                "</td></tr>",
                "<tr><td style='padding:4px 10px;'><b>", tr("Detected level", "Nivel detectado"), "</b></td><td>", level_lbl, "</td></tr>",
                "</table>")
            self$results$autoDetectNote$setContent(auto_html)

            # ── 4. Accumulate rows (value, interpretation, ci, p) ─────────────
            rows <- list()
            add_row <- function(name, val, interp, ci = c(NA_real_, NA_real_), p = NA_real_) {
                rows[[length(rows) + 1L]] <<- list(
                    coefficient = name, value = val, interpretation = interp,
                    ci_lower = ci[1], ci_upper = ci[2], p_value = p)
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
                add_row(tr("Krippendorff's α", "α de Krippendorff"), ka_val, interp, boot_ci(kfn, max_b = 200L), NA_real_)
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

            # ── 5e. Kendall's W (ordinal rankings / continuous scores) ───────
            if (opt$kendallW && k >= 2L && level != "nominal") {
                kw <- tryCatch(irr::kendall(df_num), error = function(e) NULL)
                if (!is.null(kw)) {
                    note <- if (nzchar(kw$error %||% "")) paste0(" (", tr("ties detected", "empates detectados"), ")") else ""
                    add_row(paste0("Kendall's W", note), kw$value, private$.interp_rel(kw$value), c(NA_real_, NA_real_), kw$p.value)
                }
            } else if (opt$kendallW && level == "nominal") {
                add_row("Kendall's W", NA_real_, tr("Not applicable to nominal data", "No aplica a datos nominales"))
            }

            # ── 6. Write mainTable ────────────────────────────────────────────
            mt <- self$results$mainTable
            private$.reset_table(mt, length(rows))
            for (i in seq_along(rows)) mt$setRow(rowNo = i, values = rows[[i]])

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
            self$results$discordanceNote$setContent(discord_html)
            if (!nzchar(discord_html)) self$results$discordanceNote$setVisible(FALSE)

            # ── 8. Interpretation & recommendations (four questions) ─────────
            primary_val <- if (!is.na(kappa_val)) kappa_val
                           else if (!is.na(gwet_val)) gwet_val
                           else if (!is.na(icc_a_val)) icc_a_val
                           else if (!is.na(icc_c_val)) icc_c_val
                           else krip_val
            primary_lbl <- if (level == "continuous") private$.interp_rel(primary_val) else private$.interp_kappa(primary_val)

            action_items <- character(0)
            if (level %in% c("nominal", "ordinal") && !is.na(kappa_val) && !is.na(gwet_val) && abs(kappa_val - gwet_val) > .05)
                action_items <- c(action_items, tr("Report Gwet's coefficient as the primary estimate, not Kappa.",
                                                    "Reporte el coeficiente de Gwet como estimación primaria, no el Kappa."))
            if (level == "continuous" && !is.na(icc_c_val) && !is.na(icc_a_val) && (icc_c_val - icc_a_val) > .05)
                action_items <- c(action_items, tr("Report ICC (absolute agreement) rather than ICC (consistency) if raw scores must be interchangeable across raters.",
                                                    "Reporte ICC (acuerdo absoluto) en vez de ICC (consistencia) si los puntajes brutos deben ser intercambiables entre jueces."))
            if (level == "ordinal" && k > 2L && opt$kappa)
                action_items <- c(action_items, tr("Weighted multi-rater Kappa is not available here for >2 raters; Krippendorff's ordinal α already accounts for the ordinal scale with any number of raters.",
                                                    "El Kappa ponderado multi-juez no está disponible aquí para >2 jueces; el α ordinal de Krippendorff ya considera la escala ordinal con cualquier número de jueces."))
            if (n < 30L)
                action_items <- c(action_items, tr(paste0("n = ", n, " is a small sample for agreement statistics; treat the confidence intervals above as wide."),
                                                    paste0("n = ", n, " es una muestra pequeña para estadísticos de acuerdo; trate los intervalos de confianza de arriba como amplios.")))

            action_html <- if (length(action_items) > 0L)
                paste0("<ul style='font-size:13px;line-height:1.8;'>", paste0("<li>", action_items, "</li>", collapse = ""), "</ul>")
            else paste0("<p>", tr("No specific corrective action indicated — the primary estimate can be reported as-is.",
                                   "No se indica ninguna acción correctiva específica — la estimación primaria puede reportarse tal cual."), "</p>")

            bands_html <- if (level %in% c("nominal", "ordinal")) paste0(
                "<table style='border-collapse:collapse;font-size:13px;'>",
                "<tr><th style='padding:3px 8px;border:1px solid #ccc;'>", tr("Value", "Valor"), "</th><th style='padding:3px 8px;border:1px solid #ccc;'>", tr("Interpretation", "Interpretación"), "</th></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>&lt; 0</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Poor", "Pobre"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>0.00 – 0.20</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Slight", "Leve"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>0.21 – 0.40</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Fair", "Aceptable"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>0.41 – 0.60</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Moderate", "Moderado"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>0.61 – 0.80</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Substantial", "Sustancial"), "</td></tr>",
                "<tr><td style='padding:3px 8px;border:1px solid #ccc;'>0.81 – 1.00</td><td style='padding:3px 8px;border:1px solid #ccc;'>", tr("Almost perfect", "Casi perfecto"), "</td></tr>",
                "</table>",
                "<p style='font-size:11px;color:#666;'>", tr("Landis &amp; Koch (1977) bands, applied to chance-corrected categorical agreement (Kappa, Gwet, Krippendorff on nominal/ordinal data).",
                                                              "Bandas de Landis &amp; Koch (1977), aplicadas al acuerdo categórico corregido por azar (Kappa, Gwet, Krippendorff en datos nominales/ordinales)."), "</p>"
            ) else ""

            rec_html <- paste0(
                "<h4>", tr("What happened", "Qué pasó"), "</h4>",
                "<p>", tr(paste0("The primary agreement estimate for this data is <b>", round(primary_val, 3), "</b> (", primary_lbl, ")."),
                          paste0("La estimación primaria de acuerdo para estos datos es <b>", round(primary_val, 3), "</b> (", primary_lbl, ").")), "</p>",
                discord_html,
                "<h4>", tr("Why", "Por qué"), "</h4>",
                "<p>", tr(paste0("Detected design: ", k, " raters, ", n, " complete cases, ", level_lbl, " measurement level."),
                          paste0("Diseño detectado: ", k, " jueces, ", n, " casos completos, nivel de medida ", level_lbl, ".")), "</p>",
                "<h4>", tr("What it means", "Qué implica"), "</h4>",
                "<p>", tr("See the Agreement Coefficients table above for the exact numbers; the Fiability Library (Inter-Rater Agreement section) documents each coefficient's assumptions and formula in full.",
                          "Vea la tabla de Coeficientes de Acuerdo arriba para las cifras exactas; la Biblioteca de Confiabilidad (sección Acuerdo entre Jueces) documenta el supuesto y la fórmula de cada coeficiente en detalle."), "</p>",
                "<h4>", tr("What to do now", "Qué hacer ahora"), "</h4>",
                action_html,
                if (nzchar(bands_html)) paste0("<h4>", tr("Interpretation Benchmarks", "Criterios de interpretación"), "</h4>", bands_html) else "",
                "<p style='font-size:11px;color:#666;'>", tr(
                    "See Fiability Library → Inter-Rater Agreement for full definitions and assumptions, and Bibliography → Inter-Rater Reliability for references.",
                    "Vea Biblioteca de Confiabilidad → Acuerdo entre Jueces para definiciones y supuestos completos, y Bibliografía → Confiabilidad entre Jueces para las referencias."),
                "</p>"
            )
            self$results$interpretation$setContent(rec_html)
        }
    )
)

`%||%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a
