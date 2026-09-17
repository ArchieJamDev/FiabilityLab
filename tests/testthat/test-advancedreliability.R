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
# advancedReliability edge-case tests.
# ES: Pruebas de casos límite de advancedReliability.
#
# jamovi's own submission guidance calls for testing pathological input:
# special characters in variable names, missing data, and near-empty data
# sets. Advanced Reliability additionally guards on having at least 3
# items across all declared factors before it attempts to fit a
# confirmatory measurement model (n_adv < 20L || length(all_items) < 3L in
# advancedreliability.b.R). These tests do not assert any particular
# numeric result -- they assert that pathological input is met with the
# module's own explanatory note, never with an uncaught R or lavaan error,
# mirroring the edge-case suite already established for AssumptionsLab.
#
# ES: La propia guía de envío de jamovi pide probar entrada patológica:
# caracteres especiales en nombres de variable, datos faltantes y
# conjuntos de datos casi vacíos. Advanced Reliability además exige al
# menos 3 ítems en total entre todos los factores declarados antes de
# intentar ajustar un modelo de medida confirmatorio (n_adv < 20L ||
# length(all_items) < 3L en advancedreliability.b.R). Estas pruebas no
# verifican ningún resultado numérico particular -- verifican que la
# entrada patológica se resuelva con la propia nota explicativa del
# módulo, nunca con un error no controlado de R o de lavaan, siguiendo la
# misma suite de casos límite ya establecida para AssumptionsLab.
# -----------------------------------------------------------------------------

test_that("advancedReliability tolerates accented and symbol item names", {

    d <- edgeItemsSpecialNameData()
    factors <- list(list(label = "F1", vars = names(d)))

    expect_no_error(
        advancedReliability(data = d, factors = factors, reportLang = "en")
    )
})

test_that("advancedReliability tolerates a single-row data set", {

    d <- edgeSingleRowItemsData()
    factors <- list(list(label = "F1", vars = names(d)))

    expect_no_error(
        advancedReliability(data = d, factors = factors, reportLang = "en")
    )
})

test_that("advancedReliability tolerates an item column that is entirely NA", {

    d <- edgeAllNaData(edgeItemsBaseData(), "item1")
    factors <- list(list(label = "F1", vars = names(d)))

    expect_no_error(
        advancedReliability(data = d, factors = factors, reportLang = "en")
    )
})

test_that("advancedReliability tolerates a zero-variance item column", {

    d <- edgeConstantData(edgeItemsBaseData(), "item1")
    factors <- list(list(label = "F1", vars = names(d)))

    expect_no_error(
        advancedReliability(data = d, factors = factors, reportLang = "en")
    )
})

test_that("advancedReliability tolerates fewer than 3 items across all factors", {

    d <- edgeItemsBaseData(n_items = 2)
    factors <- list(list(label = "F1", vars = names(d)))

    expect_no_error(
        advancedReliability(data = d, factors = factors, reportLang = "en")
    )
})

# -----------------------------------------------------------------------------
# advancedReliability regression test: second-order "reliability due to G".
# ES: Prueba de regresión de advancedReliability: "confiabilidad debida a G"
# de segundo orden.
#
# jamovi's official module review (Claudia, 2026-09-16) found that
# semTools::compRelSEM() returns a list, not a plain numeric vector, for a
# multi-factor model -- the second-order branch always has >=3 factors, so
# it always hits this. The single-bracket indexing previously used to read
# rel_g left it as a length-1 list; .fl_clean_na()'s is.na() check does not
# unwrap a list (returns FALSE), so the list passed through unchanged, and
# the later `rel_g / cr` division failed with "non-numeric argument to
# binary operator" for every successful second-order fit. This mirrors the
# cr_val/ave_val unwrap already done a few lines above in the same file
# (see the comment there) -- the same fix needed to be applied to rel_g too.
# This test exercises the actual code path with a genuine 3-factor
# hierarchical/bifactor fixture, not just static analysis.
#
# ES: La revisión oficial de módulos de jamovi (Claudia, 2026-09-16)
# encontró que semTools::compRelSEM() devuelve una lista, no un vector
# numérico plano, para un modelo multifactorial -- la rama de segundo orden
# siempre tiene 3 o más factores, así que siempre lo dispara. La indexación
# con corchete simple usada antes para leer rel_g lo dejaba como una lista
# de longitud 1; la verificación is.na() de .fl_clean_na() no desempaqueta
# una lista (devuelve FALSE), así que la lista pasaba sin cambios, y la
# división `rel_g / cr` posterior fallaba con "non-numeric argument to
# binary operator" en cada ajuste exitoso de segundo orden. Esto refleja el
# desempaquetado de cr_val/ave_val ya hecho unas líneas arriba en el mismo
# archivo (ver el comentario ahí) -- había que aplicar el mismo arreglo a
# rel_g también. Esta prueba ejercita el camino de código real con un
# fixture jerárquico/bifactor genuino de 3 factores, no solo análisis
# estático.
# -----------------------------------------------------------------------------

test_that("advancedReliability computes reliability due to G for a second-order model", {

    fx <- fixtureSecondOrderData()

    res <- advancedReliability(
        data = fx$data, factors = fx$factors, secondOrder = TRUE,
        itemType = "continuous", estimator = "ml", reportLang = "en"
    )

    row <- res$reliabilityTable$asDF
    expect_true(any(!is.na(row$rel_g)))
    expect_true(all(is.na(row$rel_g) | (row$rel_g >= 0 & row$rel_g <= 1)))
})

# -----------------------------------------------------------------------------
# advancedReliability plot-export regression tests.
# ES: Pruebas de regresión de exportación de gráficos de advancedReliability.
#
# Same jamovi official module review finding (2026-09-16) as
# test-internalconsistency.R's own plot-export section -- see the comment
# there for the full explanation. Reuses fixtureSecondOrderData() with
# secondOrder = TRUE so all four plots (parallel analysis, reliability
# comparison, loadings, and the correlated-factors-vs-second-order fit
# comparison, which only exists when secondOrder is on) get exercised in one
# fit, not just the three that a first-order-only model would produce.
#
# ES: Mismo hallazgo de la revisión oficial de módulos de jamovi
# (2026-09-16) que la propia sección de exportación de gráficos de
# test-internalconsistency.R -- ver el comentario ahí para la explicación
# completa. Reutiliza fixtureSecondOrderData() con secondOrder = TRUE para
# que los cuatro gráficos (análisis paralelo, comparación de confiabilidad,
# cargas, y la comparación de ajuste factores-correlacionados-vs-segundo-
# orden, que solo existe cuando secondOrder está activo) se ejerciten en un
# solo ajuste, no solo los tres que produciría un modelo de solo primer
# orden.
# -----------------------------------------------------------------------------

test_that("advancedReliability's plots have usable state and export without error", {

    fx <- fixtureSecondOrderData()

    res <- advancedReliability(
        data = fx$data, factors = fx$factors, secondOrder = TRUE,
        showParallelAnalysis = TRUE, itemType = "continuous",
        estimator = "ml", reportLang = "en"
    )

    expect_false(is.null(res$plotParallelAnalysis$state))
    expect_false(is.null(res$plotComparison$state))
    expect_false(is.null(res$plotLoadings$state))
    expect_false(is.null(res$plotFitComparison$state))

    for (nm in c("plotParallelAnalysis", "plotComparison", "plotLoadings", "plotFitComparison")) {
        f <- tempfile(fileext = ".png")
        on.exit(unlink(f), add = TRUE)
        expect_no_error(res[[nm]]$saveAs(f))
        expect_true(file.exists(f) && file.size(f) > 0)
    }
})
