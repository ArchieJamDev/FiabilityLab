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
        itemType = "continuous", estimator = "ml", reportLang = "en"
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
        itemType = "continuous", estimator = "ml", reportLang = "en"
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

test_that("measurementInvariance tolerates accented and symbol item names", {

    d <- edgeGroupItemsSpecialNameData()
    items <- setdiff(names(d), "group")
    factors <- list(list(label = "F1", vars = items))

    expect_no_error(
        measurementInvariance(
            data = d, group = "group", factors = factors,
            itemType = "continuous", estimator = "ml", reportLang = "en"
        )
    )
})

test_that("measurementInvariance tolerates a single-row data set", {

    d <- edgeGroupItemsSingleRowData()
    items <- setdiff(names(d), "group")
    factors <- list(list(label = "F1", vars = items))

    expect_no_error(
        measurementInvariance(
            data = d, group = "group", factors = factors,
            itemType = "continuous", estimator = "ml", reportLang = "en"
        )
    )
})

test_that("measurementInvariance tolerates an item column that is entirely NA", {

    d <- edgeAllNaData(fixtureInvariantTwoGroupData(n_per_group = 30), "item1")

    expect_no_error(
        measurementInvariance(
            data = d, group = "group",
            factors = list(list(label = "F1", vars = paste0("item", 1:4))),
            itemType = "continuous", estimator = "ml", reportLang = "en"
        )
    )
})

test_that("measurementInvariance tolerates a zero-variance item column", {

    d <- edgeConstantData(fixtureInvariantTwoGroupData(n_per_group = 30), "item1")

    expect_no_error(
        measurementInvariance(
            data = d, group = "group",
            factors = list(list(label = "F1", vars = paste0("item", 1:4))),
            itemType = "continuous", estimator = "ml", reportLang = "en"
        )
    )
})

test_that("measurementInvariance tolerates a grouping variable with a single level", {

    d <- edgeGroupItemsSingleLevelData()
    items <- setdiff(names(d), "group")
    factors <- list(list(label = "F1", vars = items))

    expect_no_error(
        measurementInvariance(
            data = d, group = "group", factors = factors,
            itemType = "continuous", estimator = "ml", reportLang = "en"
        )
    )
})
