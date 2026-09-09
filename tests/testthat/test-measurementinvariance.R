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
