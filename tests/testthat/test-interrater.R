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

# -----------------------------------------------------------------------------
# jamovi's official module review (2026-09-16) found this module hiding
# every result and leaving one Html message on conditions that genuinely
# block the whole analysis (fewer than 2 raters, not enough complete
# cases) -- since fixed to call jmvcore::reject(), which throws so jamovi
# shows its own standard greyed-error presentation. Calling the exported
# wrapper function directly (as these tests do, outside jamovi Desktop)
# means that throw surfaces as a real R error -- the three tests below
# were written before that fix and asserted expect_no_error() for exactly
# these three conditions; they now assert expect_error() with the expected
# message instead, confirming the module rejects cleanly with an
# informative reason rather than either silently doing nothing or crashing
# with a cryptic message.
# ES: La revisión oficial de módulos de jamovi (2026-09-16) encontró que
# este módulo ocultaba todo resultado y dejaba un solo mensaje Html en
# condiciones que bloquean genuinamente todo el análisis (menos de 2
# jueces, no hay suficientes casos completos) -- ya arreglado para llamar
# a jmvcore::reject(), que lanza una excepción para que jamovi muestre su
# propia presentación estándar de error en gris. Llamar directamente a la
# función envoltorio exportada (como hacen estas pruebas, fuera de jamovi
# Desktop) significa que ese lanzamiento se manifiesta como un error real
# de R -- las tres pruebas de abajo se escribieron antes de ese arreglo y
# afirmaban expect_no_error() para exactamente estas tres condiciones;
# ahora afirman expect_error() con el mensaje esperado en su lugar,
# confirmando que el módulo rechaza limpiamente con una razón informativa
# en vez de no hacer nada en silencio o fallar con un mensaje críptico.
# -----------------------------------------------------------------------------

test_that("interRater rejects a single-row data set with an informative message", {

    d <- edgeSingleRowRatersData()

    expect_error(
        interRater(data = d, ratings = names(d), dataType = "continuous", reportLang = "en"),
        "minimum 5"
    )
})

test_that("interRater rejects a rater column that is entirely NA (leaves too few complete cases)", {

    d <- edgeAllNaData(edgeRatersBaseData(), "rater1")

    expect_error(
        interRater(data = d, ratings = names(d), dataType = "continuous", reportLang = "en"),
        "minimum 5"
    )
})

test_that("interRater tolerates a zero-variance rater column", {

    d <- edgeConstantData(edgeRatersBaseData(), "rater1", constantValue = 50)

    expect_no_error(
        interRater(data = d, ratings = names(d), dataType = "continuous", reportLang = "en")
    )
})

test_that("interRater rejects fewer than 2 raters with an informative message", {

    d <- edgeRatersBaseData(k = 1)

    expect_error(
        interRater(data = d, ratings = names(d), dataType = "continuous", reportLang = "en"),
        "at least 2"
    )
})

# -----------------------------------------------------------------------------
# interRater plot-export regression tests.
# ES: Pruebas de regresión de exportación de gráficos de interRater.
#
# Same jamovi official module review finding (2026-09-16) as
# test-internalconsistency.R's own plot-export section -- see the comment
# there for the full explanation. .plotDiagnostic has two independent
# branches (category prevalence for nominal/ordinal data, per-rater mean for
# continuous data), each with its own setState() call in .run(), so both are
# exercised here rather than just one.
#
# ES: Mismo hallazgo de la revisión oficial de módulos de jamovi
# (2026-09-16) que la propia sección de exportación de gráficos de
# test-internalconsistency.R -- ver el comentario ahí para la explicación
# completa. .plotDiagnostic tiene dos ramas independientes (prevalencia de
# categoría para datos nominales/ordinales, media por juez para datos
# continuos), cada una con su propia llamada a setState() en .run(), así que
# ambas se ejercitan aquí en vez de solo una.
# -----------------------------------------------------------------------------

test_that("interRater's plots have usable state and export without error (nominal)", {

    d <- fixturePerfectAgreementData()

    res <- interRater(data = d, ratings = names(d), dataType = "nominal", reportLang = "en")

    expect_false(is.null(res$plotComparison$state))
    expect_false(is.null(res$plotDiagnostic$state))

    for (nm in c("plotComparison", "plotDiagnostic")) {
        f <- tempfile(fileext = ".png")
        on.exit(unlink(f), add = TRUE)
        expect_no_error(res[[nm]]$saveAs(f))
        expect_true(file.exists(f) && file.size(f) > 0)
    }
})

test_that("interRater's plots have usable state and export without error (continuous)", {

    d <- fixtureHomoscedasticRaterData()

    res <- interRater(data = d, ratings = names(d), dataType = "continuous", reportLang = "en")

    expect_false(is.null(res$plotComparison$state))
    expect_false(is.null(res$plotDiagnostic$state))

    for (nm in c("plotComparison", "plotDiagnostic")) {
        f <- tempfile(fileext = ".png")
        on.exit(unlink(f), add = TRUE)
        expect_no_error(res[[nm]]$saveAs(f))
        expect_true(file.exists(f) && file.size(f) > 0)
    }
})
