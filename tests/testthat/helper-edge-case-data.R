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
# Shared edge-case data fixtures.
# ES: Datos compartidos para casos límite.
#
# Every FiabilityLab analysis that actually computes something (Internal
# Consistency, Inter-Rater Agreement, Advanced Reliability, Measurement
# Invariance) is exercised, in tests/testthat/test-*.R, on pathological
# input drawn from these builders: non-ASCII/symbol column names, a
# single-row data set, an entirely missing column, a zero-variance column,
# and the minimum item/rater/group count each module's own design guard
# checks for. Fiability Library and Bibliography take no `data` option and
# are deliberately excluded, mirroring AssumptionsLab's own
# helper-edge-case-data.R, which excludes assumptionLibrary/bibliography
# for the same reason. Centralizing these here keeps each module's test
# file focused on which edge case is meaningful for that module, rather
# than on re-deriving the fixture itself.
#
# ES: Cada análisis de FiabilityLab que realmente calcula algo (Internal
# Consistency, Inter-Rater Agreement, Advanced Reliability, Measurement
# Invariance) se pone a prueba, en tests/testthat/test-*.R, con entradas
# patológicas construidas a partir de estos generadores: nombres de columna
# no ASCII o con símbolos, un conjunto de datos de una sola fila, una
# columna completamente faltante, una columna de varianza cero, y el
# número mínimo de ítems/jueces/grupos que la propia validación de diseño
# de cada módulo exige. Fiability Library y Bibliography no reciben la
# opción `data` y se excluyen a propósito, siguiendo el propio
# helper-edge-case-data.R de AssumptionsLab, que excluye a
# assumptionLibrary/bibliography por la misma razón. Centralizarlos aquí
# permite que el archivo de pruebas de cada módulo se concentre en qué
# caso límite es relevante para ese módulo, en lugar de reconstruir la
# misma base de datos.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Build a factor with an exact number of levels.
# ES: Construir un factor con un número exacto de niveles.
#
# Used to probe Measurement Invariance's documented "at least 2 groups"
# guard by generating a grouping factor with exactly one or two levels on
# demand.
#
# ES: Se usa para poner a prueba la validación de "al menos 2 grupos"
# documentada en Measurement Invariance, generando un factor de
# agrupación con exactamente uno o dos niveles bajo demanda.
# -----------------------------------------------------------------------------
edgeLevelFactor <- function(n, levelCount, ordered = FALSE) {

    labels <- LETTERS[seq_len(max(levelCount, 1))]
    values <- rep(labels, length.out = n)

    factor(values, levels = labels, ordered = ordered)
}

# -----------------------------------------------------------------------------
# Baseline valid item data set (Internal Consistency / Advanced Reliability).
# ES: Conjunto de datos base válido de ítems (Internal Consistency /
# Advanced Reliability).
#
# A small but well-formed set of continuous items, used as the starting
# point that individual tests then corrupt with one pathological column at
# a time, so a failure can be attributed to the specific corruption rather
# than to an unrelated data quirk.
#
# ES: Un conjunto pequeño pero bien formado de ítems continuos, usado como
# punto de partida que cada prueba corrompe luego con una sola columna
# patológica a la vez, de modo que una falla pueda atribuirse a la
# corrupción específica y no a un defecto de datos ajeno.
# -----------------------------------------------------------------------------
edgeItemsBaseData <- function(n = 30, n_items = 6, seed = 20260909) {

    set.seed(seed)

    items <- as.data.frame(replicate(
        n_items,
        stats::rnorm(n, mean = 3, sd = 1),
        simplify = FALSE
    ))
    names(items) <- paste0("item", seq_len(n_items))

    items
}

# -----------------------------------------------------------------------------
# Item data set with accented and symbol column names.
# ES: Conjunto de datos de ítems con nombres de columna acentuados y con
# símbolos.
#
# jamovi's own submission guidance calls for testing "special characters"
# in the data; the most consequential place they can appear is in the
# variable names themselves, since those names flow through non-standard
# evaluation (jmvcore::resolveQuo / marshalData) before reaching each
# module's .b.R file. Column names here mix non-ASCII letters, accents and
# a mathematical symbol, all of which are syntactically valid R names when
# backtick-quoted.
#
# ES: La propia guía de envío de jamovi pide probar "caracteres
# especiales" en los datos; el lugar donde más impacto tienen es en los
# propios nombres de variable, ya que esos nombres atraviesan evaluación
# no estándar (jmvcore::resolveQuo / marshalData) antes de llegar al
# archivo .b.R de cada módulo. Los nombres de columna aquí combinan letras
# no ASCII, acentos y un símbolo matemático, todos válidos sintácticamente
# en R si se citan con comillas invertidas.
# -----------------------------------------------------------------------------
edgeItemsSpecialNameData <- function(n = 30, seed = 20260909) {

    set.seed(seed)

    data.frame(
        `ítem_uno`     = stats::rnorm(n, mean = 3, sd = 1),
        `ítem_dós`     = stats::rnorm(n, mean = 3, sd = 1),
        `pregunta ± 3` = stats::rnorm(n, mean = 3, sd = 1),
        `reactivo_4`   = stats::rnorm(n, mean = 3, sd = 1),
        check.names = FALSE
    )
}

# -----------------------------------------------------------------------------
# Single-observation item data set.
# ES: Conjunto de datos de ítems con una sola observación.
#
# n = 1 is the smallest data set a jamovi user can still submit to an
# analysis (an empty data set is refused by jamovi itself before reaching
# R). Every downstream statistic that requires variance or more than one
# case is expected to degrade to a documented message rather than error
# out on subscript/length mismatches.
#
# ES: n = 1 es el conjunto de datos más pequeño que un usuario de jamovi
# aún puede enviar a un análisis (jamovi rechaza un conjunto de datos
# vacío antes de que llegue a R). Se espera que todo estadístico posterior
# que requiera varianza o más de un caso degrade a un mensaje documentado
# en lugar de fallar por desajustes de subíndices o longitudes.
# -----------------------------------------------------------------------------
edgeSingleRowItemsData <- function(n_items = 6) {

    row <- as.data.frame(as.list(rep(3, n_items)))
    names(row) <- paste0("item", seq_len(n_items))

    row
}

# -----------------------------------------------------------------------------
# Baseline valid rater data set (Inter-Rater Agreement).
# ES: Conjunto de datos base válido de jueces (Inter-Rater Agreement).
#
# Analogous to edgeItemsBaseData(), but shaped as k raters scoring n cases
# on a continuous scale.
#
# ES: Análogo a edgeItemsBaseData(), pero con la forma de k jueces
# calificando n casos en una escala continua.
# -----------------------------------------------------------------------------
edgeRatersBaseData <- function(n = 30, k = 4, seed = 20260909) {

    set.seed(seed)

    raters <- as.data.frame(replicate(
        k,
        stats::rnorm(n, mean = 50, sd = 10),
        simplify = FALSE
    ))
    names(raters) <- paste0("rater", seq_len(k))

    raters
}

# -----------------------------------------------------------------------------
# Rater data set with accented and symbol column names.
# ES: Conjunto de datos de jueces con nombres de columna acentuados y con
# símbolos.
#
# Same rationale as edgeItemsSpecialNameData(), for the rater-column shape
# Inter-Rater Agreement expects.
#
# ES: Misma justificación que edgeItemsSpecialNameData(), para la forma de
# columnas de jueces que espera Inter-Rater Agreement.
# -----------------------------------------------------------------------------
edgeRatersSpecialNameData <- function(n = 30, seed = 20260909) {

    set.seed(seed)

    data.frame(
        `juez_uno`      = stats::rnorm(n, mean = 50, sd = 10),
        `juez_dós`      = stats::rnorm(n, mean = 50, sd = 10),
        `evaluador ± 3` = stats::rnorm(n, mean = 50, sd = 10),
        `calificador_4` = stats::rnorm(n, mean = 50, sd = 10),
        check.names = FALSE
    )
}

# -----------------------------------------------------------------------------
# Single-observation rater data set.
# ES: Conjunto de datos de jueces con una sola observación.
#
# Same rationale as edgeSingleRowItemsData(), for the rater-column shape.
#
# ES: Misma justificación que edgeSingleRowItemsData(), para la forma de
# columnas de jueces.
# -----------------------------------------------------------------------------
edgeSingleRowRatersData <- function(k = 4) {

    row <- as.data.frame(as.list(rep(50, k)))
    names(row) <- paste0("rater", seq_len(k))

    row
}

# -----------------------------------------------------------------------------
# Force one column of an existing data set entirely to NA.
# ES: Forzar por completo a NA una columna de un conjunto de datos
# existente.
#
# Simulates a variable that was selected in the interface but never
# actually recorded (e.g. an entire item or rater skipped). Works on any
# of the base data sets above, or on fixtureInvariantTwoGroupData() from
# helper-fixtures.R, since it only needs a data frame and a column name.
#
# ES: Simula una variable que fue seleccionada en la interfaz pero nunca
# se registró (p. ej. un ítem o juez completo omitido). Funciona sobre
# cualquiera de los conjuntos base anteriores, o sobre
# fixtureInvariantTwoGroupData() de helper-fixtures.R, ya que solo
# necesita un data frame y un nombre de columna.
# -----------------------------------------------------------------------------
edgeAllNaData <- function(data, columnName) {

    # Assigning bare NA (logical) would silently coerce a numeric column to
    # logical, which jmvcore's own option-type validation then rejects
    # before the analysis ever runs. In-place assignment to every element
    # preserves the column's original class while still emptying it out.
    # ES: Asignar NA a secas (lógico) convertiría en silencio una columna
    # numérica a lógica, lo que la propia validación de tipos de opciones
    # de jmvcore rechazaría antes de que el análisis llegue a ejecutarse.
    # La asignación en el lugar a cada elemento conserva la clase original
    # de la columna y aun así la vacía por completo.
    data[[columnName]][] <- NA

    data
}

# -----------------------------------------------------------------------------
# Force one numeric column of an existing data set to zero variance.
# ES: Forzar a varianza cero una columna numérica de un conjunto de datos
# existente.
#
# A constant item or rater is a classic trigger for division-by-zero in
# correlation/covariance-based coefficients (Alpha, Omega, ICC) and for
# singular matrices in the SEM-based analyses.
#
# ES: Un ítem o juez constante es un disparador clásico de división por
# cero en coeficientes basados en correlación/covarianza (Alfa, Omega,
# ICC) y de matrices singulares en los análisis basados en SEM.
# -----------------------------------------------------------------------------
edgeConstantData <- function(data, columnName, constantValue = 3) {

    data[[columnName]] <- rep(constantValue, nrow(data))

    data
}

# -----------------------------------------------------------------------------
# Grouped-item data sets (Measurement Invariance).
# ES: Conjuntos de datos de ítems agrupados (Measurement Invariance).
#
# Measurement Invariance needs a grouping variable on top of the item
# columns, so its edge cases get their own small builders rather than
# reusing the ungrouped ones above: special/accented names (with a group
# column added), a single row (only one group level actually observed),
# and a grouping variable with only one level (its own documented "at
# least 2 groups" guard). The all-NA and zero-variance item-column cases
# reuse fixtureInvariantTwoGroupData() from helper-fixtures.R directly
# with edgeAllNaData()/edgeConstantData() above, since that fixture is
# already the right shape (group + item1..item4).
#
# ES: Measurement Invariance necesita una variable de agrupación además de
# las columnas de ítems, así que sus casos límite tienen sus propios
# generadores pequeños en vez de reutilizar los anteriores sin grupo:
# nombres especiales/acentuados (con una columna de grupo agregada), una
# sola fila (con un solo nivel de grupo realmente observado), y una
# variable de agrupación con un solo nivel (su propia validación
# documentada de "al menos 2 grupos"). Los casos de columna de ítem
# enteramente NA y de varianza cero reutilizan fixtureInvariantTwoGroupData()
# de helper-fixtures.R directamente con edgeAllNaData()/edgeConstantData()
# de arriba, ya que ese fixture ya tiene la forma correcta (group +
# item1..item4).
# -----------------------------------------------------------------------------
edgeGroupItemsSpecialNameData <- function(n = 30, seed = 20260909) {

    d <- edgeItemsSpecialNameData(n = n, seed = seed)
    d$group <- rep(c("A", "B"), length.out = n)

    d
}

edgeGroupItemsSingleRowData <- function(n_items = 4) {

    d <- edgeSingleRowItemsData(n_items = n_items)
    d$group <- factor("A", levels = c("A", "B"))

    d
}

edgeGroupItemsSingleLevelData <- function(n = 30, n_items = 4, seed = 20260909) {

    d <- edgeItemsBaseData(n = n, n_items = n_items, seed = seed)
    d$group <- edgeLevelFactor(n, 1)

    d
}
