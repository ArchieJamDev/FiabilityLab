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
# measurementInvariance regression tests.
# ES: Pruebas de regresión de measurementInvariance.
#
# The invariance-sequence verdict used to be a plain OR ("either the LRT or
# ΔCFI passing counts as holding"), which collapsed two criteria with
# different sensitivities and always resolved disagreement in the
# restriction's favor. It was replaced with a five-way classification
# (Supported by both/LRT only/ΔCFI only/Not supported/Unable to determine).
# These tests guard that a design with NO true difference between groups is
# classified as fully supported at every level, and that a design with a
# KNOWN, deliberately introduced scalar-invariance violation (one item's
# intercept shifted in one group only, loadings untouched) is caught
# specifically at the Scalar level -- not at Metric (which does not depend
# on intercepts) and not silently passed through.
#
# ES: El veredicto de la secuencia de invariancia solía ser un simple OR
# ("basta con que el LRT o el ΔCFI se cumplan para considerarse sostenido"),
# que colapsaba dos criterios con sensibilidades distintas y siempre
# resolvía el desacuerdo a favor de la restricción. Se reemplazó por una
# clasificación de cinco categorías (Respaldado por ambos/solo LRT/solo
# ΔCFI/No respaldado/No se pudo determinar). Estas pruebas protegen que un
# diseño SIN diferencia real entre grupos se clasifique como plenamente
# respaldado en todos los niveles, y que un diseño con una violación de
# invariancia escalar CONOCIDA e introducida deliberadamente (el intercepto
# de un ítem desplazado solo en un grupo, las cargas sin tocar) se detecte
# específicamente en el nivel Escalar -- no en Métrica (que no depende de
# los interceptos) y no se deje pasar en silencio.
# -----------------------------------------------------------------------------

test_that("a design with no true group difference is fully supported at every level", {

    d <- fixtureInvariantTwoGroupData()
    factors <- list(list(label = "F1", vars = paste0("item", 1:4)))

    res <- measurementInvariance(
        data = d, group = "group", factors = factors,
        itemType = "continuous", estimator = "ml"
    )

    row <- res$invarianceTable$asDF
    non_baseline <- row[row$model != "Configural", ]
    expect_true(all(non_baseline$verdict == "Supported by both criteria"))
})

test_that("a known scalar-invariance violation is caught specifically at the Scalar level", {

    d <- fixtureScalarViolationTwoGroupData()
    factors <- list(list(label = "F1", vars = paste0("item", 1:4)))

    res <- measurementInvariance(
        data = d, group = "group", factors = factors,
        itemType = "continuous", estimator = "ml"
    )

    row <- res$invarianceTable$asDF
    metric <- row[row$model == "Metric (weak)", ]
    scalar <- row[row$model == "Scalar (strong)", ]

    expect_equal(metric$verdict, "Supported by both criteria")
    expect_equal(scalar$verdict, "Not supported")
})

# -----------------------------------------------------------------------------
# measurementInvariance edge-case tests.
# ES: Pruebas de casos límite de measurementInvariance.
#
# jamovi's own submission guidance calls for testing pathological input:
# special characters in variable names, missing data, and near-empty data
# sets. Measurement Invariance additionally guards on having at least 2
# group levels before it attempts to fit the configural/metric/scalar/
# strict sequence (length(group_levels) < 2L in
# measurementinvariance.b.R). These tests do not assert any particular
# numeric result -- they assert that pathological input is met with the
# module's own explanatory note, never with an uncaught R or lavaan
# error, mirroring the edge-case suite already established for
# AssumptionsLab.
#
# ES: La propia guía de envío de jamovi pide probar entrada patológica:
# caracteres especiales en nombres de variable, datos faltantes y
# conjuntos de datos casi vacíos. Measurement Invariance además exige al
# menos 2 niveles de grupo antes de intentar ajustar la secuencia
# configural/métrica/escalar/estricta (length(group_levels) < 2L en
# measurementinvariance.b.R). Estas pruebas no verifican ningún resultado
# numérico particular -- verifican que la entrada patológica se resuelva
# con la propia nota explicativa del módulo, nunca con un error no
# controlado de R o de lavaan, siguiendo la misma suite de casos límite ya
# establecida para AssumptionsLab.
# -----------------------------------------------------------------------------

test_that("measurementInvariance rejects cleanly (not a crash) with accented and symbol item names on an underpowered design", {

    # edgeGroupItemsSpecialNameData()'s default n=30 (15/group) is below
    # the 20-per-group minimum regardless of naming, so this is expected
    # to reject on sample size, not to fit -- the point of this test is
    # that the rejection surfaces as jmvcore::reject()'s clean, informative
    # message (jamovi's own greyed error pane), not an uncaught R/lavaan
    # crash from broken variable-name syntax. See the dedicated fit-success
    # test below (a properly-sized, properly-structured fixture) for
    # confirmation that the safe-names fix itself works when the design can
    # actually support a real fit.
    d <- edgeGroupItemsSpecialNameData()
    items <- setdiff(names(d), "group")
    factors <- list(list(label = "F1", vars = items))

    expect_error(
        measurementInvariance(
            data = d, group = "group", factors = factors,
            itemType = "continuous", estimator = "ml"
        ),
        "per group"
    )
})

# -----------------------------------------------------------------------------
# measurementInvariance lavaan-safe-names regression test.
# ES: Prueba de regresión de nombres seguros para lavaan en
# measurementInvariance.
#
# jamovi's official module review (2026-09-16) found that item names with
# spaces, hyphens or accented characters break lavaan's model-syntax parser
# when pasted directly into the configural-model formula. The parse error
# was swallowed by a tryCatch and reported as "did not converge" (via
# bail(), itself since fixed to jmvcore::reject()) -- a graceful-looking
# failure the earlier accented-name edge-case test above could not
# distinguish from a genuine convergence failure, since it only asserted
# expect_no_error(). This test uses a properly-sized fixture (60/group,
# comfortably above the 20-per-group minimum) to confirm the configural
# model actually fits with these names, not just that nothing crashed.
#
# ES: La revisión oficial de módulos de jamovi (2026-09-16) encontró que
# nombres de ítem con espacios, guiones o caracteres acentuados rompen el
# analizador de sintaxis de modelos de lavaan al pegarse directamente en la
# fórmula del modelo configural. El error de análisis quedaba absorbido por
# un tryCatch y se reportaba como "no convergió" (vía bail(), ya arreglado
# a jmvcore::reject()) -- una falla de apariencia correcta que la prueba de
# caso límite de nombres acentuados de arriba no podía distinguir de una
# falla de convergencia genuina, ya que solo verificaba expect_no_error().
# Esta prueba usa un fixture de tamaño adecuado (60/grupo, cómodamente por
# encima del mínimo de 20 por grupo) para confirmar que el modelo
# configural realmente ajusta con estos nombres, no solo que nada falló.
# -----------------------------------------------------------------------------

test_that("measurementInvariance's configural model actually fits with accented/symbol item names", {

    d <- edgeGroupItemsSpecialNameFactorData()
    items <- setdiff(names(d), "group")
    factors <- list(list(label = "F1", vars = items))

    res <- measurementInvariance(
        data = d, group = "group", factors = factors,
        itemType = "continuous", estimator = "ml"
    )

    row <- res$invarianceTable$asDF
    configural <- row[row$model == "Configural", ]
    expect_false(is.na(configural$chisq))
    expect_false(is.na(configural$cfi))
})

# -----------------------------------------------------------------------------
# jamovi's official module review (2026-09-16) found this module hiding
# every result and leaving one Html message (via bail()) on conditions
# that genuinely block the whole analysis -- since fixed to call
# jmvcore::reject(), which throws so jamovi shows its own standard
# greyed-error presentation. Calling the exported wrapper function
# directly (as these tests do, outside jamovi Desktop) means that throw
# surfaces as a real R error -- the four tests below were written before
# that fix and asserted expect_no_error() for exactly these conditions;
# they now assert expect_error() with the expected message instead,
# confirming the module rejects cleanly with an informative reason rather
# than either silently doing nothing or crashing with a cryptic message.
# ES: La revisión oficial de módulos de jamovi (2026-09-16) encontró que
# este módulo ocultaba todo resultado y dejaba un solo mensaje Html (vía
# bail()) en condiciones que bloquean genuinamente todo el análisis -- ya
# arreglado para llamar a jmvcore::reject(), que lanza una excepción para
# que jamovi muestre su propia presentación estándar de error en gris.
# Llamar directamente a la función envoltorio exportada (como hacen estas
# pruebas, fuera de jamovi Desktop) significa que ese lanzamiento se
# manifiesta como un error real de R -- las cuatro pruebas de abajo se
# escribieron antes de ese arreglo y afirmaban expect_no_error() para
# exactamente estas condiciones; ahora afirman expect_error() con el
# mensaje esperado en su lugar, confirmando que el módulo rechaza
# limpiamente con una razón informativa en vez de no hacer nada en
# silencio o fallar con un mensaje críptico.
# -----------------------------------------------------------------------------

test_that("measurementInvariance rejects a single-row data set with an informative message", {

    d <- edgeGroupItemsSingleRowData()
    items <- setdiff(names(d), "group")
    factors <- list(list(label = "F1", vars = items))

    expect_error(
        measurementInvariance(
            data = d, group = "group", factors = factors,
            itemType = "continuous", estimator = "ml"
        ),
        "at least 2 levels"
    )
})

test_that("measurementInvariance rejects an item column that is entirely NA (leaves too few complete cases)", {

    d <- edgeAllNaData(fixtureInvariantTwoGroupData(n_per_group = 30), "item1")

    expect_error(
        measurementInvariance(
            data = d, group = "group",
            factors = list(list(label = "F1", vars = paste0("item", 1:4))),
            itemType = "continuous", estimator = "ml"
        ),
        "at least 2 levels"
    )
})

test_that("measurementInvariance rejects a zero-variance item column (configural model cannot converge)", {

    d <- edgeConstantData(fixtureInvariantTwoGroupData(n_per_group = 30), "item1")

    expect_error(
        measurementInvariance(
            data = d, group = "group",
            factors = list(list(label = "F1", vars = paste0("item", 1:4))),
            itemType = "continuous", estimator = "ml"
        ),
        "did not converge"
    )
})

test_that("measurementInvariance rejects a grouping variable with a single level, with an informative message", {

    d <- edgeGroupItemsSingleLevelData()
    items <- setdiff(names(d), "group")
    factors <- list(list(label = "F1", vars = items))

    expect_error(
        measurementInvariance(
            data = d, group = "group", factors = factors,
            itemType = "continuous", estimator = "ml"
        ),
        "at least 2 levels"
    )
})

# -----------------------------------------------------------------------------
# measurementInvariance plot-export regression test.
# ES: Prueba de regresión de exportación de gráficos de measurementInvariance.
#
# Same jamovi official module review finding (2026-09-16) as
# test-internalconsistency.R's own plot-export section -- see the comment
# there for the full explanation.
#
# ES: Mismo hallazgo de la revisión oficial de módulos de jamovi
# (2026-09-16) que la propia sección de exportación de gráficos de
# test-internalconsistency.R -- ver el comentario ahí para la explicación
# completa.
# -----------------------------------------------------------------------------

test_that("measurementInvariance's plot has usable state and exports without error", {

    d <- fixtureInvariantTwoGroupData(n_per_group = 100)

    res <- measurementInvariance(
        data = d, group = "group",
        factors = list(list(label = "F1", vars = paste0("item", 1:4))),
        itemType = "continuous", estimator = "ml"
    )

    expect_false(is.null(res$plotInvariance$state))

    f <- tempfile(fileext = ".png")
    on.exit(unlink(f), add = TRUE)
    expect_no_error(res$plotInvariance$saveAs(f))
    expect_true(file.exists(f) && file.size(f) > 0)
})
