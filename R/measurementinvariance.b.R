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

            fm_names <- c("chisq", "df", "cfi", "rmsea")
            fm_list <- lapply(fits, function(f) if (is.null(f)) NULL else tryCatch(lavaan::fitMeasures(f, fm_names), error = function(e) NULL))

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
                    lrt_holds <- is.na(p_val) || p_val >= .05
                    cfi_holds <- is.na(dcfi) || dcfi >= -.01
                    verdict <- if (lrt_holds || cfi_holds) tr("Held", "Se sostiene") else tr("Not held", "No se sostiene")
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

            not_held <- Filter(function(r) identical(r$verdict, tr("Not held", "No se sostiene")), inv_rows)
            first_not_held <- if (length(not_held) > 0L) not_held[[1]]$model else NULL
            failed_to_converge <- Filter(function(r) identical(r$verdict, tr("Did not converge", "No convergió")), inv_rows)

            estim_desc <- if (item_is_ordinal)
                tr("Items were treated as ordinal (WLSMV estimator on polychoric/tetrachoric correlations).",
                   "Los ítems se trataron como ordinales (estimador WLSMV sobre correlaciones policóricas/tetracóricas).")
                else if (identical(estimator_eff, "MLR"))
                tr("Items were treated as continuous, fit with MLR (robust to non-normality).",
                   "Los ítems se trataron como continuos, ajustados con MLR (robusto a la no normalidad).")
                else tr("Items were treated as continuous, fit with ML.", "Los ítems se trataron como continuos, ajustados con ML.")

            res$invarianceNote$setContent(.fl_prose(
                "<p>", estim_desc, " ", tr(
                    paste0("Each row after Configural is tested against the row just before it (not against Configural directly): a nonsignificant likelihood-ratio test (p &ge; .05) or a small drop in CFI (&Delta;CFI &ge; -.01, the sample-size-robust criterion of Cheung &amp; Rensvold, 2002) both count as the added restriction \"holding\" -- the LRT alone gets hypersensitive to trivial misfit in large samples, which is why both criteria are shown."),
                    paste0("Cada fila después de Configural se prueba contra la fila justo anterior (no contra Configural directamente): una prueba de razón de verosimilitud no significativa (p &ge; .05) o una caída pequeña en el CFI (&Delta;CFI &ge; -.01, el criterio robusto al tamaño muestral de Cheung &amp; Rensvold, 2002) cuentan como que la restricción agregada \"se sostiene\" -- el LRT por sí solo se vuelve hipersensible a desajustes triviales en muestras grandes, por lo que se muestran ambos criterios.")),
                "</p>",
                "<p>", tr(
                    "Configural invariance means the same items load on the same factors in every group, with everything else free -- the minimum requirement for the construct to even be comparable across groups. Metric (weak) invariance -- equal loadings -- is required before comparing regression/correlation coefficients involving the factor across groups. Scalar (strong) invariance -- also equal intercepts/thresholds -- is required before comparing group means or observed scores; without it, an observed mean difference may reflect item functioning differences, not a real difference on the construct. Strict invariance -- also equal residual variances -- is a stronger, less commonly required condition.",
                    "La invariancia configural significa que los mismos ítems cargan sobre los mismos factores en cada grupo, con todo lo demás libre -- el requisito mínimo para que el constructo sea siquiera comparable entre grupos. La invariancia métrica (débil) -- cargas iguales -- se requiere antes de comparar coeficientes de regresión/correlación que involucren al factor entre grupos. La invariancia escalar (fuerte) -- también interceptos/umbrales iguales -- se requiere antes de comparar medias de grupo o puntajes observados; sin ella, una diferencia de medias observada puede reflejar diferencias en el funcionamiento de los ítems, no una diferencia real en el constructo. La invariancia estricta -- también varianzas residuales iguales -- es una condición más fuerte, requerida con menos frecuencia."),
                "</p>",
                if (length(failed_to_converge) > 0L) paste0("<p>&#9888; ", tr(
                        "One or more models in the sequence did not converge -- the invariance question cannot be answered past that point with this data/structure.",
                        "Uno o más modelos de la secuencia no convergieron -- la pregunta de invariancia no puede responderse más allá de ese punto con estos datos/estructura."), "</p>")
                    else if (!is.null(first_not_held)) paste0("<p>&#9888; ", tr(
                        paste0(first_not_held, " invariance does not hold -- do not compare whatever that level of invariance is required for (see above) across ", esc(group_name), " groups without first identifying which specific parameters differ (partial invariance) via semTools::partialInvariance()/partialInvarianceCat()."),
                        paste0("La invariancia ", first_not_held, " no se sostiene -- no compare aquello para lo que se requiere ese nivel de invariancia (vea arriba) entre grupos de ", esc(group_name), " sin antes identificar qué parámetros específicos difieren (invariancia parcial) vía semTools::partialInvariance()/partialInvarianceCat().")), "</p>")
                    else paste0("<p>&#10003; ", tr("Full invariance holds through every level tested -- comparing group means/scores on this construct across these groups is on solid footing.",
                                                     "La invariancia completa se sostiene en todos los niveles probados -- comparar medias/puntajes de grupo en este constructo entre estos grupos está en una base sólida."), "</p>")))

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
                else if (!is.null(first_not_held))
                    paste0("<li>", tr("Before comparing group means or scores, investigate partial invariance to find which specific items break the restriction, rather than abandoning the comparison or forcing full invariance.",
                                      "Antes de comparar medias o puntajes de grupo, investigue la invariancia parcial para encontrar qué ítems específicos rompen la restricción, en vez de abandonar la comparación o forzar la invariancia completa."), "</li>")
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
