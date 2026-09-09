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
