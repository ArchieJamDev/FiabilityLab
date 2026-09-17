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

# ── Plot colours: derived from jamovi's own theme, not a module-local
# style option ──────────────────────────────────────────────────────────
# EN: jamovi's official module review (2026-09-16) found that every plot
# in this module used its own plotStyle option (theme_light()/
# theme_linedraw()/theme_minimal()/theme_gray() + a hand-picked colour
# pair) instead of the ggtheme/theme jamovi already passes into every
# render function -- so these plots didn't follow the user's jamovi
# theme/palette choice, looked different from every other analysis's
# plots in the same output, and changing jamovi's Theme setting did
# nothing for them. plotStyle and .fl_plot_style()/.fl_plot_theme() are
# removed entirely; every render function now does `+ ggtheme` instead
# (see jamovi's plot-theme guide,
# https://dev.jamovi.org/tutorial/tuts0302-plot-themes), and this
# function replaces .fl_plot_colors(style) with a version that reads the
# same primary/secondary pair from theme$color/theme$fill -- the two-
# colour semantic distinctions every plot here actually needs (a data
# series vs. a reference/comparison one, or two named models/conditions),
# not an arbitrary N-category "colour by group" palette (which none of
# this module's plots have -- unlike, say, AssumptionsLab's multi-class
# ROC curves, which is why AssumptionsLab's own migration additionally
# kept a plotPalette option and this one does not).
# ES: La revisión oficial de módulos de jamovi (2026-09-16) encontró que
# todo gráfico de este módulo usaba su propia opción plotStyle
# (theme_light()/theme_linedraw()/theme_minimal()/theme_gray() + un par
# de colores elegido a mano) en vez del ggtheme/theme que jamovi ya pasa
# a cada función de render -- así que estos gráficos no seguían el
# tema/paleta elegido por el usuario en jamovi, se veían distintos a los
# de cualquier otro análisis en la misma salida, y cambiar el ajuste de
# Tema de jamovi no hacía nada por ellos. plotStyle y
# .fl_plot_style()/.fl_plot_theme() se eliminan por completo; cada
# función de render ahora hace `+ ggtheme` en su lugar (ver la guía de
# temas de gráficos de jamovi,
# https://dev.jamovi.org/tutorial/tuts0302-plot-themes), y esta función
# reemplaza a .fl_plot_colors(style) con una versión que lee el mismo par
# primario/secundario desde theme$color/theme$fill -- las distinciones
# semánticas de dos colores que todo gráfico de este módulo realmente
# necesita (una serie de datos vs. una de referencia/comparación, o dos
# modelos/condiciones nombrados), no una paleta arbitraria de N
# categorías "colorear por grupo" (que ningún gráfico de este módulo
# tiene -- a diferencia de, por ejemplo, las curvas ROC multiclase de
# AssumptionsLab, razón por la cual la propia migración de AssumptionsLab
# además conservó una opción plotPalette y esta no lo hace).
.fl_plot_colors <- function(theme) {
    primary   <- if (!is.null(theme$color) && length(theme$color) >= 1) theme$color[1] else "#4E79A7"
    secondary <- if (!is.null(theme$color) && length(theme$color) >= 2) theme$color[2] else "#E15759"
    fill      <- if (!is.null(theme$fill)  && length(theme$fill)  >= 2) theme$fill[2]  else secondary
    list(primary = primary, secondary = secondary, fill = fill)
}

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

# jamovi's official module review (2026-09-16) found that variable names
# with spaces, hyphens or accented characters (e.g. "Item 1", "Q3-a" --
# common from spreadsheet headers) break lavaan's model-syntax parser when
# pasted directly into a formula string. The parse error was swallowed by
# the surrounding tryCatch and reported as "did not converge", sending the
# user off rearranging factors to fix what was really just a naming issue.
# jmvcore::toB64()/fromB64() (already used internally by jmvcore for the
# same reason) give a reversible, always-syntax-safe encoding: rename a
# data frame's columns and any item-name vector to the safe form before
# building lavaan syntax or fitting, and translate back only at the
# specific points where an item name is shown to the user (a factor's own
# id, like "F1", is never a real column name and passes through the
# reverse map unchanged since it was never encoded into it).
# ES: La revisión oficial de módulos de jamovi (2026-09-16) encontró que
# nombres de variable con espacios, guiones o caracteres acentuados (p.
# ej. "Item 1", "Q3-a" -- comunes en encabezados de hoja de cálculo)
# rompen el analizador de sintaxis de modelos de lavaan al pegarse
# directamente en una cadena de fórmula. El error de análisis quedaba
# absorbido por el tryCatch circundante y se reportaba como "no
# convergió", enviando al usuario a reordenar factores para arreglar lo
# que en realidad era solo un problema de nombres. jmvcore::toB64()/
# fromB64() (ya usados internamente por jmvcore por la misma razón) dan
# una codificación reversible y siempre segura para sintaxis: renombra las
# columnas de un data frame y cualquier vector de nombres de ítem a la
# forma segura antes de construir sintaxis de lavaan o ajustar el modelo,
# y traduce de vuelta solo en los puntos específicos donde se muestra un
# nombre de ítem al usuario (el propio id de un factor, como "F1", nunca
# es un nombre de columna real y pasa sin cambios por el mapa inverso ya
# que nunca se codificó en él).
.fl_safe_names <- function(names) {
    safe <- jmvcore::toB64(names)
    list(to_safe = stats::setNames(safe, names), to_raw = stats::setNames(names, safe))
}

# Look up `name` in a .fl_safe_names() reverse map (to_raw), falling back
# to `name` itself unchanged when it isn't in the map -- the case for a
# factor id ("F1", "G") or any other non-item label that flows through the
# same display code as an item name.
# ES: Busca `name` en el mapa inverso (to_raw) de .fl_safe_names(),
# devolviendo `name` sin cambios cuando no está en el mapa -- el caso de
# un id de factor ("F1", "G") o cualquier otra etiqueta que no sea de
# ítem y que fluya por el mismo código de presentación que un nombre de
# ítem.
.fl_unsafe_name <- function(name, to_raw) if (name %in% names(to_raw)) unname(to_raw[name]) else name

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
