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
# interRater regression tests.
# ES: Pruebas de regresión de interRater.
#
# Both tests here guard fixes made after an external methodological review:
# Krippendorff's alpha is computed on t(as.matrix(items)) (raters in rows,
# subjects in columns, per irr::kripp.alpha()'s own coincidence.matrix()
# source, which reports subjects = dimx[2] and raters = dimx[1]) -- the
# review suspected this transposition was backwards. It was verified correct
# empirically (a perfect-agreement fixture gives alpha = 1 with the
# transposition, alpha = -0.23 without it, which is impossible for perfect
# agreement) before concluding the review's suspicion was a false positive;
# this test keeps that verification permanent instead of ad hoc. The ICC
# assumptions check's Levene test was found to run on raw scores instead of
# the subject+rater two-way ANOVA's own residuals -- a real bug, since ICC's
# homoscedasticity assumption is about that model's error variance, not
# about whether raters' raw score distributions have equal marginal
# variance (dominated by between-subject variance). These tests confirm the
# residual-based version correctly distinguishes homoscedastic from
# heteroscedastic rater error.
#
# ES: Ambas pruebas aquí protegen arreglos hechos tras una revisión
# metodológica externa: el alfa de Krippendorff se calcula sobre
# t(as.matrix(items)) (jueces en filas, sujetos en columnas, según el propio
# código fuente de coincidence.matrix() de irr::kripp.alpha(), que reporta
# subjects = dimx[2] y raters = dimx[1]) -- la revisión sospechó que esta
# transposición estaba invertida. Se verificó correcta empíricamente (un
# fixture de acuerdo perfecto da alpha = 1 con la transposición, alpha =
# -0.23 sin ella, lo cual es imposible para acuerdo perfecto) antes de
# concluir que la sospecha de la revisión era un falso positivo; esta prueba
# mantiene esa verificación de forma permanente en vez de ad hoc. Se
# encontró que la prueba de Levene de la verificación de supuestos del ICC
# corría sobre puntajes brutos en vez de sobre los propios residuos del
# ANOVA de dos vías sujeto+juez -- un bug real, ya que el supuesto de
# homocedasticidad del ICC es sobre la varianza de error de ese modelo, no
# sobre si las distribuciones de puntajes brutos de los jueces tienen
# varianza marginal igual (dominada por la varianza entre sujetos). Estas
# pruebas confirman que la versión basada en residuos distingue
# correctamente el error homocedástico del heterocedástico entre jueces.
# -----------------------------------------------------------------------------

test_that("Krippendorff's alpha is 1 for perfect rater agreement", {

    d <- fixturePerfectAgreementData()

    res <- interRater(
        data = d, ratings = names(d), dataType = "nominal",
        kappa = FALSE, gwet = FALSE, icc = FALSE, bootstrapCi = FALSE,
        reportLang = "en"
    )

    row <- res$mainTable$asDF
    expect_equal(row$value[row$coefficient == "Krippendorff's α"], 1, tolerance = 1e-8)
})

test_that("ICC's Levene check does not flag homoscedastic rater error", {

    d <- fixtureHomoscedasticRaterData()

    res <- interRater(
        data = d, ratings = names(d), dataType = "continuous",
        kappa = FALSE, gwet = FALSE, krippendorff = FALSE, icc = TRUE,
        checkIccAssumptions = TRUE, bootstrapCi = FALSE, reportLang = "en"
    )

    row <- res$iccAssumptionsTable$asDF
    lev <- row[row$assumption == "Homoscedasticity across raters", ]
    expect_gte(lev$p_value, .05)
    expect_equal(lev$verdict, "Met")
})

test_that("ICC's Levene check flags a rater with genuinely larger residual error variance", {

    d <- fixtureHeteroscedasticRaterData()

    res <- interRater(
        data = d, ratings = names(d), dataType = "continuous",
        kappa = FALSE, gwet = FALSE, krippendorff = FALSE, icc = TRUE,
        checkIccAssumptions = TRUE, bootstrapCi = FALSE, reportLang = "en"
    )

    row <- res$iccAssumptionsTable$asDF
    lev <- row[row$assumption == "Homoscedasticity across raters", ]
    expect_lt(lev$p_value, .05)
    expect_equal(lev$verdict, "Violated")
})

# -----------------------------------------------------------------------------
# interRater edge-case tests.
# ES: Pruebas de casos límite de interRater.
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

test_that("interRater tolerates accented and symbol rater names", {

    d <- edgeRatersSpecialNameData()

    expect_no_error(
        interRater(data = d, ratings = names(d), dataType = "continuous", reportLang = "en")
    )
})

test_that("interRater tolerates a single-row data set", {

    d <- edgeSingleRowRatersData()

    expect_no_error(
        interRater(data = d, ratings = names(d), dataType = "continuous", reportLang = "en")
    )
})

test_that("interRater tolerates a rater column that is entirely NA", {

    d <- edgeAllNaData(edgeRatersBaseData(), "rater1")

    expect_no_error(
        interRater(data = d, ratings = names(d), dataType = "continuous", reportLang = "en")
    )
})

test_that("interRater tolerates a zero-variance rater column", {

    d <- edgeConstantData(edgeRatersBaseData(), "rater1", constantValue = 50)

    expect_no_error(
        interRater(data = d, ratings = names(d), dataType = "continuous", reportLang = "en")
    )
})

test_that("interRater tolerates fewer than 2 raters", {

    d <- edgeRatersBaseData(k = 1)

    expect_no_error(
        interRater(data = d, ratings = names(d), dataType = "continuous", reportLang = "en")
    )
})
