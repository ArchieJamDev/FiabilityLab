# FiabilityLab - Shared helpers used by more than one analysis module.
# Inspired by AssumptionsLab's own shared-helpers.R pattern: pure functions
# (no `self`/`private`) that each R6 class's private methods call, so a
# plot-style palette or a typography convention is defined once instead of
# being copy-pasted per module and drifting out of sync.
#
# ES: Ayudantes compartidos usados por más de un módulo de análisis.
# Inspirado en el propio patrón shared-helpers.R de AssumptionsLab: funciones
# puras (sin `self`/`private`) que los métodos privados de cada clase R6
# invocan, para que una paleta de estilo de gráfico o una convención
# tipográfica se defina una sola vez en vez de copiarse por módulo y
# desincronizarse con el tiempo.

# ── Plot style: background theme + colour palette, one coherent pair per
# named style ─────────────────────────────────────────────────────────────
# EN: All four analyses with plots (interRater, internalConsistency,
# advancedReliability, measurementInvariance) expose the same 6-option
# plotStyle setting. Each of the 6 names sets BOTH a background theme and a
# colour pair together -- not two independent switches (one for
# light/gray/linedraw, one for greenred/purpleorange/bluegreen) that only
# ever changed the ONE aspect their own name suggested and silently left
# the other at its default. Under that split design, picking "Gray" (a
# theme name) never changed the plot's colours, and picking "Green-Red" (a
# palette name) never changed its background -- exactly the "the palette
# doesn't seem to do anything" symptom this fixes.
# ES: Los cuatro análisis con gráficos (interRater, internalConsistency,
# advancedReliability, measurementInvariance) exponen la misma opción
# plotStyle de 6 opciones. Cada uno de los 6 nombres fija JUNTOS un tema de
# fondo y un par de colores -- no dos interruptores independientes (uno
# para light/gray/linedraw, otro para greenred/purpleorange/bluegreen) que
# solo cambiaban el ÚNICO aspecto que su propio nombre sugería y dejaban el
# otro silenciosamente en su valor por defecto. Bajo ese diseño dividido,
# elegir "Gray" (nombre de tema) nunca cambiaba los colores del gráfico, y
# elegir "Green-Red" (nombre de paleta) nunca cambiaba su fondo -- exactamente
# el síntoma de "la paleta no parece hacer nada" que esto corrige.
.fl_plot_style <- function(style) {
    switch(style,
        light        = list(theme = ggplot2::theme_light(),    primary = "#4E79A7", secondary = "#E15759"),
        linedraw     = list(theme = ggplot2::theme_linedraw(), primary = "#4E79A7", secondary = "#E15759"),
        greenred     = list(theme = ggplot2::theme_minimal(),  primary = "#2E8B57", secondary = "#D6604D"),
        purpleorange = list(theme = ggplot2::theme_minimal(),  primary = "#8E5FA8", secondary = "#E08214"),
        bluegreen    = list(theme = ggplot2::theme_minimal(),  primary = "#5B9BD5", secondary = "#66C2A4"),
        list(theme = ggplot2::theme_gray(), primary = "#4E79A7", secondary = "#E15759"))  # gray (default)
}

.fl_plot_theme  <- function(style) .fl_plot_style(style)$theme
.fl_plot_colors <- function(style) { s <- .fl_plot_style(style); list(primary = s$primary, secondary = s$secondary) }

# ── Typography: one convention across Bibliography, Fiability Library,
# and every analysis module's Html results ──────────────────────────────────
# EN: Bibliography (the module with the most text-heavy content) never
# overrides font-size or font-family -- it only sets line-height: 1 and
# text-align: justify on its content blocks, plus relative (em-based, not
# px) sizing for secondary/footnote text. These two helpers apply that same
# convention anywhere else in the module a block of prose or a footnote is
# built, so the four Html-producing analyses (Bibliography, Fiability
# Library, Internal Consistency, Inter-Rater Agreement) render with
# identical body typeface, size, line spacing, and justification instead of
# each module hand-rolling its own.
# ES: Bibliography (el módulo con más contenido de texto) nunca sobreescribe
# font-size ni font-family -- solo fija line-height: 1 y text-align: justify
# en sus bloques de contenido, más un tamaño relativo (en em, no px) para
# texto secundario/notas al pie. Estos dos ayudantes aplican esa misma
# convención en cualquier otra parte del módulo donde se construya un bloque
# de texto corrido o una nota al pie, para que los cuatro análisis que
# producen Html (Bibliography, Fiability Library, Internal Consistency,
# Inter-Rater Agreement) se rendericen con el mismo tipo de letra, tamaño,
# interlineado y justificación en vez de que cada módulo defina el suyo.
.fl_prose_open <- function() "<div style='line-height:1;text-align:justify;'>"
.fl_prose_close <- function() "</div>"
.fl_prose <- function(...) paste0(.fl_prose_open(), paste0(...), .fl_prose_close())

.fl_footnote <- function(...) paste0("<p style='font-size:0.85em;color:#666;'>", paste0(...), "</p>")

# ── Bilingual translation ────────────────────────────────────────────────────
# EN: Every analysis class defines its own `.tr = function(en, es) ...`
# private method reading `self$options$reportLang`; the body was byte-
# identical in interRater and internalConsistency. This pure version takes
# the already-read option value so each class's `.tr` stays a one-line call.
# ES: Cada clase de análisis define su propio método privado
# `.tr = function(en, es) ...` que lee `self$options$reportLang`; el cuerpo
# era idéntico letra por letra en interRater e internalConsistency. Esta
# versión pura toma el valor de la opción ya leído para que el `.tr` de cada
# clase quede en una sola línea.
.fl_tr <- function(en, es, report_lang) if (identical(report_lang, "es")) es else en

# ── Reliability/agreement interpretation bands (Kline, 2000; George &
# Mallery, 2003) ─────────────────────────────────────────────────────────────
# EN: Used by internalConsistency for every CTT coefficient (α, ω, GLB,
# split-half, Guttman λ, KR-20/21) and by interRater for ICC and
# Krippendorff's α on continuous data -- the same "proportion of variance/
# agreement" question, not the chance-corrected categorical scale Landis &
# Koch (1977) covers. `tr` is the caller's own translation closure (its
# `private$.tr`), so this stays independent of any one class's option access.
# ES: Usado por internalConsistency para cada coeficiente TCT (α, ω, GLB,
# mitades partidas, λ de Guttman, KR-20/21) y por interRater para el ICC y el
# α de Krippendorff en datos continuos -- la misma pregunta de "proporción de
# varianza/acuerdo", no la escala categórica corregida por azar de Landis &
# Koch (1977). `tr` es la propia función de traducción del llamador (su
# `private$.tr`), así que esto no depende del acceso a opciones de una clase.
.fl_interp_rel <- function(val, tr) {
    if (is.na(val) || !is.finite(val)) return(tr("N/A", "N/D"))
    if (val >= .95) return(tr("Excellent",    "Excelente"))
    if (val >= .90) return(tr("Good",         "Bueno"))
    if (val >= .80) return(tr("Acceptable",   "Aceptable"))
    if (val >= .70) return(tr("Questionable", "Cuestionable"))
    if (val >= .60) return(tr("Poor",         "Pobre"))
    tr("Unacceptable", "Inaceptable")
}

# ── jamovi Table helpers ─────────────────────────────────────────────────────
.fl_reset_table <- function(table, n_rows) {
    table$deleteRows()
    if (n_rows > 0L)
        for (i in seq_len(n_rows)) table$addRow(rowKey = i)
    invisible(table)
}

# ── Nonparametric bootstrap for a scalar statistic ──────────────────────────
# EN: Row-resamples `df` with replacement B times, applying `stat_fn` to
# each resample; returns the percentile 95% CI (and its SE) from whichever
# resamples didn't error or come back non-finite. Both interRater and
# internalConsistency had their own copy of this (interRater's dropped the
# `se` field internalConsistency's bootstrapTable needs) -- this version is
# the superset, so both classes' `boot_ci()`/step-10 callers work unchanged.
# ES: Remuestrea `df` por filas con reemplazo B veces, aplicando `stat_fn` a
# cada remuestra; retorna el IC 95% percentil (y su SE) de las remuestras que
# no fallaron ni volvieron no finitas. Tanto interRater como
# internalConsistency tenían su propia copia (la de interRater omitía el
# campo `se` que la bootstrapTable de internalConsistency necesita) -- esta
# versión es el superconjunto, así que los llamadores boot_ci()/paso 10 de
# ambas clases funcionan sin cambios.
# ── NA/NaN sanitizing for jamovi Table cells ────────────────────────────────
# EN: A numeric NA_real_/NaN can render as the literal text "NaN" once it
# crosses the real jamovi engine's serialization boundary, even though a
# direct-R asDF() check always shows it correctly as blank -- a real bug
# first caught in interRater's mainTable. Wrap every numeric value handed to
# `Table$setRow()` in this before storing it.
# ES: Un NA_real_/NaN numérico puede renderizarse como el texto literal
# "NaN" al cruzar el límite de serialización del motor real de jamovi,
# aunque una verificación directa en R con asDF() siempre lo muestre
# correctamente en blanco -- un bug real detectado primero en la mainTable
# de interRater. Envuelva en esto cada valor numérico que se pase a
# `Table$setRow()` antes de guardarlo.
.fl_clean_na <- function(x) if (is.na(x)) NA else x

# ── Low-cardinality (ordinal-like) numeric detection ────────────────────────
# EN: Shapiro-Wilk (and any continuous-normality test) assumes a genuinely
# continuous variable; run on a discrete Likert item (4-7 whole-number
# categories) it mostly detects the item's own discreteness/ties rather
# than a real distributional problem, so a "Non-normal" verdict is close to
# guaranteed regardless of the item's actual shape. This mirrors the same
# whole-number + <=7-categories heuristic interRater already uses to
# separate ordinal from continuous ratings, generalized here as a shared
# helper so internalConsistency's own normality check can skip Shapiro-Wilk
# on the same basis instead of running it unconditionally on every item.
# ES: Shapiro-Wilk (o cualquier prueba de normalidad continua) asume una
# variable genuinamente continua; aplicada a un ítem Likert discreto (4-7
# categorías de números enteros) detecta mayormente la propia discreción/
# empates del ítem en vez de un problema distribucional real, así que un
# veredicto "No normal" está casi garantizado sin importar la forma real
# del ítem. Esto refleja la misma heurística de números enteros + <=7
# categorías que interRater ya usa para separar calificaciones ordinales de
# continuas, generalizada aquí como ayudante compartido para que la propia
# verificación de normalidad de internalConsistency pueda omitir Shapiro-
# Wilk con el mismo criterio en vez de aplicarlo sin condición a cada ítem.
.fl_is_low_cardinality <- function(df, max_categories = 7L) {
    is_whole <- vapply(df, function(x) {
        xn <- suppressWarnings(as.numeric(x))
        !anyNA(xn) && all(abs(xn - round(xn)) < 1e-8)
    }, logical(1))
    n_unique <- vapply(df, function(x) length(unique(x)), integer(1))
    all(is_whole) && max(n_unique) <= max_categories
}

.fl_bootstrap <- function(df, stat_fn, B = 1000L) {
    n <- nrow(df)
    vals <- numeric(B)
    for (b in seq_len(B)) {
        idx <- sample.int(n, n, replace = TRUE)
        vals[b] <- tryCatch(stat_fn(df[idx, , drop = FALSE]), error = function(e) NA_real_)
    }
    vals <- vals[is.finite(vals)]
    if (length(vals) < 10L) return(list(se = NA_real_, lo = NA_real_, hi = NA_real_))
    list(se = sd(vals),
         lo = unname(quantile(vals, .025)),
         hi = unname(quantile(vals, .975)))
}
