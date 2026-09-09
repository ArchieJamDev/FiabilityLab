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
        bootstrapCi = FALSE, checkDimensionality = FALSE, itemAnalysis = FALSE,
        reportLang = "en"
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
        bootstrapCi = FALSE, checkDimensionality = FALSE, itemAnalysis = FALSE,
        reportLang = "en"
    )

    row <- res$mainTable$asDF
    kr20 <- row[row$coefficient == "KR-20 (Kuder-Richardson)", ]
    kr21 <- row[row$coefficient == "KR-21", ]

    expect_false(is.na(kr20$value))
    expect_false(is.na(kr21$value))
    expect_gte(kr20$value, 0)
    expect_lte(kr20$value, 1)
})
