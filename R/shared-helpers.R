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

# ── Plot style: background theme + colour palette ───────────────────────────
# EN: interRater and internalConsistency both expose the same 6-option
# plotStyle setting (3 background themes, 3 colour palettes) and both need
# the exact same theme object / primary-secondary hex pair for it.
# ES: interRater e internalConsistency exponen la misma opción plotStyle de
# 6 opciones (3 temas de fondo, 3 paletas de color) y ambos necesitan el
# mismo objeto de tema / par de hex primario-secundario para ella.
.fl_plot_theme <- function(style) {
    switch(style,
        light    = ggplot2::theme_light(),
        gray     = ggplot2::theme_gray(),
        linedraw = ggplot2::theme_linedraw(),
        ggplot2::theme_minimal())
}

.fl_plot_colors <- function(style) {
    switch(style,
        greenred     = list(primary = "#2E8B57", secondary = "#D6604D"),
        purpleorange = list(primary = "#8E5FA8", secondary = "#E08214"),
        bluegreen    = list(primary = "#5B9BD5", secondary = "#66C2A4"),
        list(primary = "#4E79A7", secondary = "#E15759"))
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
