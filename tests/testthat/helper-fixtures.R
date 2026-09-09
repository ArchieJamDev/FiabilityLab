# -----------------------------------------------------------------------------------------------
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
# Fixtures. ES: Datos de prueba (fixtures).
#
# Small, deterministic synthetic datasets built to have a KNOWN correct
# answer, so a test failure means the code changed, not that a fixture's
# expected value needs updating. Used across more than one test-*.R file.
# Real project data (data/fiabilitylab_test_data.csv) is deliberately not
# used here: its "correct" coefficient values are not independently known,
# so it can confirm a calculation didn't silently error, but it cannot
# confirm the calculation is right.
#
# ES: Conjuntos de datos sintéticos pequeños y deterministas construidos
# para tener una respuesta correcta CONOCIDA, de modo que una prueba
# fallida signifique que el código cambió, no que haya que actualizar el
# valor esperado de un fixture. Usados en más de un archivo test-*.R. Los
# datos reales del proyecto (data/fiabilitylab_test_data.csv) deliberadamente
# no se usan aquí: sus valores "correctos" de coeficiente no se conocen de
# forma independiente, así que pueden confirmar que un cálculo no falló en
# silencio, pero no pueden confirmar que el cálculo sea correcto.
# -----------------------------------------------------------------------------

# EN: n cases, k raters, every rater gives the identical rating to every
# case -- the only design for which every chance-corrected agreement
# coefficient (Kappa, Gwet's AC1/AC2, Krippendorff's alpha) has a known
# value of exactly 1, regardless of the coefficient's own formula or the
# orientation its underlying function expects its input matrix in.
# ES: n casos, k jueces, cada juez da la misma calificación a cada caso
# -- el único diseño para el cual todo coeficiente de acuerdo corregido
# por azar (Kappa, AC1/AC2 de Gwet, alfa de Krippendorff) tiene un valor
# conocido de exactamente 1, sin importar la fórmula propia del
# coeficiente ni la orientación en que su función subyacente espera su
# matriz de entrada.
fixturePerfectAgreementData <- function(n = 20, k = 3, categories = 1:4) {
    set.seed(1)
    ratings <- sample(categories, n, replace = TRUE)
    d <- as.data.frame(matrix(rep(ratings, k), nrow = n, ncol = k))
    names(d) <- paste0("rater", seq_len(k))
    d
}

# EN: n subjects, k raters, every rater sharing the same error variance --
# a genuinely homoscedastic design, for the residual-based Levene test to
# correctly NOT flag.
# ES: n sujetos, k jueces, todos los jueces comparten la misma varianza de
# error -- un diseño genuinamente homocedástico, para que la prueba de
# Levene basada en residuos correctamente NO lo marque.
fixtureHomoscedasticRaterData <- function(n = 60, k = 4, seed = 2) {
    set.seed(seed)
    true_score <- stats::rnorm(n, mean = 50, sd = 10)
    d <- as.data.frame(matrix(NA_real_, nrow = n, ncol = k))
    names(d) <- paste0("rater", seq_len(k))
    for (j in seq_len(k)) d[[j]] <- true_score + stats::rnorm(n, mean = 0, sd = 1)
    d
}

# EN: Same subject effect as fixtureHomoscedasticRaterData(), but one
# rater's (`bad_rater`) error is perturbed with `bad_sd` times the noise
# of the others -- a KNOWN heteroscedastic error structure. Built with a
# genuine between-subject effect so a Levene test on raw scores (dominated
# by that between-subject variance) and a Levene test on the subject+rater
# model's own residuals can disagree, the way the real project data did.
# ES: El mismo efecto de sujeto que fixtureHomoscedasticRaterData(), pero
# el error de un juez (`bad_rater`) se perturba con `bad_sd` veces el
# ruido de los demás -- una estructura de error heterocedástica CONOCIDA.
# Construida con un efecto entre sujetos genuino para que una prueba de
# Levene sobre puntajes brutos (dominada por esa varianza entre sujetos) y
# una prueba de Levene sobre los propios residuos del modelo sujeto+juez
# puedan discrepar, tal como ocurrió con los datos reales del proyecto.
fixtureHeteroscedasticRaterData <- function(n = 60, k = 4, bad_rater = k, bad_sd = 6, seed = 2) {
    set.seed(seed)
    true_score <- stats::rnorm(n, mean = 50, sd = 10)
    d <- as.data.frame(matrix(NA_real_, nrow = n, ncol = k))
    names(d) <- paste0("rater", seq_len(k))
    for (j in seq_len(k)) {
        sdj <- if (j == bad_rater) bad_sd else 1
        d[[j]] <- true_score + stats::rnorm(n, mean = 0, sd = sdj)
    }
    d
}

# EN: m cases, n_items genuinely binary (0/1) items -- KR-20/21 should
# compute a real coefficient on this.
# ES: m casos, n_items ítems genuinamente binarios (0/1) -- el KR-20/21
# debería calcular un coeficiente real sobre esto.
fixtureBinary01Data <- function(m = 100, n_items = 6, seed = 3) {
    set.seed(seed)
    ability <- stats::rnorm(m)
    d <- as.data.frame(matrix(NA_integer_, nrow = m, ncol = n_items))
    names(d) <- paste0("item", seq_len(n_items))
    for (j in seq_len(n_items)) d[[j]] <- as.integer(stats::plogis(ability) > stats::runif(m))
    d
}

# EN: Same shape as fixtureBinary01Data() but coded 1/2 instead of 0/1 --
# a plausible "2 categories, not necessarily binary" case a user could
# force measureLevel = "dichotomous" onto.
# ES: Misma forma que fixtureBinary01Data() pero codificado 1/2 en vez de
# 0/1 -- un caso plausible de "2 categorías, no necesariamente binario"
# sobre el cual un usuario podría forzar measureLevel = "dichotomous".
fixtureNonBinaryTwoCategoryData <- function(m = 100, n_items = 6, seed = 3) {
    d <- fixtureBinary01Data(m, n_items, seed)
    as.data.frame(lapply(d, function(x) x + 1L))
}

# EN: Two groups, IDENTICAL item-generating loadings and intercepts -- a
# design for which every level of measurement invariance (configural
# through strict) should genuinely hold, since there is no true difference
# between groups to detect.
# ES: Dos grupos, cargas e interceptos de generación de ítems IDÉNTICOS --
# un diseño para el cual todo nivel de invariancia de medida (configural
# hasta estricta) debería sostenerse genuinamente, ya que no hay una
# diferencia real entre grupos que detectar.
fixtureInvariantTwoGroupData <- function(n_per_group = 250, loadings = c(.7, .7, .7, .7), seed = 4) {
    gen_factor <- function(n, ldg) {
        f <- stats::rnorm(n)
        items <- sapply(ldg, function(l) l * f + stats::rnorm(n, sd = sqrt(1 - l^2)))
        colnames(items) <- paste0("item", seq_along(ldg))
        as.data.frame(items)
    }
    set.seed(seed)
    d1 <- gen_factor(n_per_group, loadings); d1$group <- "A"
    d2 <- gen_factor(n_per_group, loadings); d2$group <- "B"
    rbind(d1, d2)
}

# EN: Same as fixtureInvariantTwoGroupData() but with item1 shifted by
# `shift` points for group B only -- a KNOWN scalar-invariance violation
# (loadings/metric structure untouched; only one item's mean level
# differs by group, which is exactly what scalar invariance checks for).
# ES: Igual que fixtureInvariantTwoGroupData() pero con item1 desplazado
# `shift` puntos solo para el grupo B -- una violación CONOCIDA de
# invariancia escalar (las cargas/estructura métrica no se tocan; solo el
# nivel medio de un ítem difiere por grupo, que es exactamente lo que la
# invariancia escalar verifica).
fixtureScalarViolationTwoGroupData <- function(n_per_group = 250, shift = 1.2, seed = 4) {
    d <- fixtureInvariantTwoGroupData(n_per_group, seed = seed)
    d$item1[d$group == "B"] <- d$item1[d$group == "B"] + shift
    d
}
