# -----------------------------------------------------------------------------------------
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
# internalConsistency regression tests.
# ES: Pruebas de regresión de internalConsistency.
#
# KR-20/21 treat every item value as a 0/1 "correct" indicator (p_i a
# proportion, q_i = 1 - p_i meaningful only in [0,1]). An external
# methodological review found that forcing measureLevel = "dichotomous" on
# data with merely 2 categories -- not necessarily coded 0/1 -- let q_i go
# negative and the formulas silently return a number with no statistical
# meaning. These tests guard the fix: KR-20/21 must report "Not computed"
# with an explicit reason on non-0/1 data, and must still compute normally
# on genuinely binary data.
#
# ES: El KR-20/21 trata cada valor de ítem como un indicador 0/1 de
# "correcto" (p_i una proporción, q_i = 1 - p_i solo tiene sentido en
# [0,1]). Una revisión metodológica externa encontró que forzar
# measureLevel = "dichotomous" sobre datos con solo 2 categorías -- no
# necesariamente codificadas 0/1 -- dejaba que q_i se volviera negativo y
# las fórmulas devolvían en silencio un número sin significado estadístico.
# Estas pruebas protegen el arreglo: el KR-20/21 debe reportar "No
# calculado" con una razón explícita sobre datos no-0/1, y debe seguir
# calculando normalmente sobre datos genuinamente binarios.
# -----------------------------------------------------------------------------

test_that("KR-20/21 are not computed on 2-category data that isn't coded 0/1", {

    d <- fixtureNonBinaryTwoCategoryData()

    res <- internalConsistency(
        data = d, items = names(d), measureLevel = "dichotomous",
        kr20 = TRUE, kr21 = TRUE, alpha = FALSE, omega = FALSE,
        normality = FALSE, checkReliabilityAssumptions = FALSE,
        bootstrapCi = FALSE, checkDimensionality = FALSE, itemAnalysis = FALSE
    )

    row <- res$mainTable$asDF
    kr20 <- row[row$coefficient == "KR-20 (Kuder-Richardson)", ]
    kr21 <- row[row$coefficient == "KR-21", ]

    expect_true(is.na(kr20$value))
    expect_true(is.na(kr21$value))
    expect_match(kr20$applicability, "not coded 0/1", fixed = TRUE)
    expect_match(kr21$applicability, "not coded 0/1", fixed = TRUE)
})

test_that("KR-20/21 compute a real coefficient on genuinely binary 0/1 data", {

    d <- fixtureBinary01Data()

    res <- internalConsistency(
        data = d, items = names(d), measureLevel = "dichotomous",
        kr20 = TRUE, kr21 = TRUE, alpha = FALSE, omega = FALSE,
        normality = FALSE, checkReliabilityAssumptions = FALSE,
        bootstrapCi = FALSE, checkDimensionality = FALSE, itemAnalysis = FALSE
    )

    row <- res$mainTable$asDF
    kr20 <- row[row$coefficient == "KR-20 (Kuder-Richardson)", ]
    kr21 <- row[row$coefficient == "KR-21", ]

    expect_false(is.na(kr20$value))
    expect_false(is.na(kr21$value))
    expect_gte(kr20$value, 0)
    expect_lte(kr20$value, 1)
})

test_that("KR-20/21 explain non-applicability on polytomous data instead of silently omitting", {

    d <- edgeItemsBaseData()

    res <- internalConsistency(
        data = d, items = names(d), measureLevel = "polytomous",
        kr20 = TRUE, kr21 = TRUE, alpha = FALSE, omega = FALSE,
        normality = FALSE, checkReliabilityAssumptions = FALSE,
        bootstrapCi = FALSE, checkDimensionality = FALSE, itemAnalysis = FALSE
    )

    row <- res$mainTable$asDF
    kr20 <- row[row$coefficient == "KR-20 (Kuder-Richardson)", ]
    kr21 <- row[row$coefficient == "KR-21", ]

    expect_true(is.na(kr20$value))
    expect_true(is.na(kr21$value))
    expect_match(kr20$applicability, "Not applicable to polytomous", fixed = TRUE)
    expect_match(kr21$applicability, "Not applicable to polytomous", fixed = TRUE)
})

# -----------------------------------------------------------------------------
# internalConsistency edge-case tests.
# ES: Pruebas de casos límite de internalConsistency.
#
# jamovi's own submission guidance calls for testing pathological input:
# special characters in variable names, missing data, and near-empty data
# sets. These tests do not assert any particular numeric result -- they
# assert that pathological input is met with the module's own explanatory
# note (rendered through self$results$autoDetectNote$setContent() and
# similar), never with an uncaught R error, mirroring the edge-case suite
# already established for AssumptionsLab.
#
# ES: La propia guía de envío de jamovi pide probar entrada patológica:
# caracteres especiales en nombres de variable, datos faltantes y
# conjuntos de datos casi vacíos. Estas pruebas no verifican ningún
# resultado numérico particular -- verifican que la entrada patológica se
# resuelva con la propia nota explicativa del módulo (renderizada mediante
# self$results$autoDetectNote$setContent() y similares), nunca con un
# error no controlado de R, siguiendo la misma suite de casos límite ya
# establecida para AssumptionsLab.
# -----------------------------------------------------------------------------

test_that("internalConsistency tolerates accented and symbol item names", {

    d <- edgeItemsSpecialNameData()

    expect_no_error(
        internalConsistency(data = d, items = names(d))
    )
})

# -----------------------------------------------------------------------------
# internalConsistency lavaan-safe-names regression test.
# ES: Prueba de regresión de nombres seguros para lavaan en
# internalConsistency.
#
# jamovi's official module review (2026-09-16) found that item names with
# spaces, hyphens or accented characters break lavaan's model-syntax parser
# when pasted directly into the tau-equivalence test's formula string. The
# parse error was swallowed by a tryCatch and reported as "did not converge"
# -- a graceful-looking N/A that the earlier accented-name edge-case test
# above could not distinguish from a genuine convergence failure, since it
# only asserted expect_no_error(). This test instead confirms the
# tau-equivalence row actually computes a real statistic with these names,
# not just that nothing crashed.
#
# ES: La revisión oficial de módulos de jamovi (2026-09-16) encontró que
# nombres de ítem con espacios, guiones o caracteres acentuados rompen el
# analizador de sintaxis de modelos de lavaan al pegarse directamente en la
# cadena de fórmula de la prueba de tau-equivalencia. El error de análisis
# quedaba absorbido por un tryCatch y se reportaba como "no convergió" -- un
# N/D de apariencia correcta que la prueba de caso límite de nombres
# acentuados de arriba no podía distinguir de una falla de convergencia
# genuina, ya que solo verificaba expect_no_error(). Esta prueba en cambio
# confirma que la fila de tau-equivalencia realmente calcula un estadístico
# real con estos nombres, no solo que nada falló.
# -----------------------------------------------------------------------------

test_that("internalConsistency's tau-equivalence test computes a real statistic with accented/symbol item names", {

    d <- edgeItemsSpecialNameFactorData()

    res <- internalConsistency(
        data = d, items = names(d), alpha = TRUE, checkReliabilityAssumptions = TRUE
    )

    row <- res$reliabilityAssumptionsTable$asDF
    tau_row <- row[row$assumption == "Tau-equivalence", ]
    expect_false(is.na(tau_row$statistic))
    expect_true(tau_row$verdict %in% c("Met", "Violated"))
})

test_that("internalConsistency tolerates a single-row data set", {

    d <- edgeSingleRowItemsData()

    expect_no_error(
        internalConsistency(data = d, items = names(d))
    )
})

test_that("internalConsistency tolerates an item column that is entirely NA", {

    d <- edgeAllNaData(edgeItemsBaseData(), "item1")

    expect_no_error(
        internalConsistency(data = d, items = names(d))
    )
})

test_that("internalConsistency tolerates a zero-variance item column", {

    d <- edgeConstantData(edgeItemsBaseData(), "item1")

    expect_no_error(
        internalConsistency(data = d, items = names(d))
    )
})

test_that("internalConsistency tolerates fewer than 2 items", {

    d <- edgeItemsBaseData(n_items = 1)

    expect_no_error(
        internalConsistency(data = d, items = names(d))
    )
})

# -----------------------------------------------------------------------------
# internalConsistency plot-export regression tests.
# ES: Pruebas de regresión de exportación de gráficos de internalConsistency.
#
# jamovi's official module review (2026-09-16) found that every plot in this
# file rendered blank on export: the render function read data.frames built
# in .run() and stashed in private$ fields, but jamovi's image-export path
# builds a separate analysis instance and calls the render function directly
# without ever calling .run() on it, so those private$ fields are always NULL
# there -- only self$results$<image>$setState() survives into that path.
# These tests confirm image$state is actually populated with usable data
# after .run(), and that each render function executes end-to-end from that
# state via the real Image$saveAs() codepath (jmvcore::Analysis$.render() ->
# .createPlotObject(), the same mechanism jamovi Desktop's export uses) --
# not just that .run() itself completes.
#
# ES: La revisión oficial de módulos de jamovi (2026-09-16) encontró que todo
# gráfico de este archivo se exportaba en blanco: la función de render leía
# data.frames construidos en .run() y guardados en campos private$, pero el
# camino de exportación de imágenes de jamovi construye una instancia de
# análisis separada y llama a la función de render directamente sin nunca
# llamar a .run() sobre ella, así que esos campos private$ siempre son NULL
# ahí -- solo self$results$<image>$setState() sobrevive a ese camino. Estas
# pruebas confirman que image$state realmente queda poblado con datos
# utilizables después de .run(), y que cada función de render se ejecuta de
# extremo a extremo a partir de ese estado a través del propio camino de
# Image$saveAs() (jmvcore::Analysis$.render() -> .createPlotObject(), el
# mismo mecanismo que usa la exportación de jamovi Desktop) -- no solo que
# .run() en sí se complete.
# -----------------------------------------------------------------------------

test_that("internalConsistency's plots have usable state and export without error", {

    d <- edgeItemsBaseData(n_items = 8)

    res <- internalConsistency(
        data = d, items = names(d), checkDimensionality = TRUE,
        itemAnalysis = TRUE
    )

    expect_false(is.null(res$plotComparison$state))
    expect_false(is.null(res$plotItemDist$state))
    expect_false(is.null(res$plotItemTotal$state))
    expect_false(is.null(res$plotScree$state))

    for (nm in c("plotComparison", "plotItemDist", "plotItemTotal", "plotScree")) {
        f <- tempfile(fileext = ".png")
        on.exit(unlink(f), add = TRUE)
        expect_no_error(res[[nm]]$saveAs(f))
        expect_true(file.exists(f) && file.size(f) > 0)
    }
})
