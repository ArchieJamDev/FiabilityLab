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
# testthat entry point.
# ES: Punto de entrada de testthat.
#
# Discovers and runs every test-*.R file under tests/testthat/ against the
# installed FiabilityLab package.
#
# Deliberately uses test_dir() against this file's own source directory
# rather than the standard R CMD check / devtools::test() entry point
# (testthat::test_check(), which looks for tests/testthat/ INSIDE the
# installed package). jmvtools::install() -- the way this module is actually
# installed, into jamovi's own module library, not a standard R library via
# R CMD INSTALL -- does not bundle the tests/ directory into the installed
# copy, so test_check() fails with "No test files found" even though the
# package itself installed correctly. Running against the source directory
# instead is what actually works for a jamovi module's own test/install
# cycle; see DEVELOPER_GUIDE.md. Run this file with:
#   Rscript tests/testthat.R
# from the project root (not via R CMD check, for the same reason).
#
# lavaan/semTools (Suggests, used by Advanced Reliability and Measurement
# Invariance) are only reliably available inside jamovi's own module library
# -- installed there via jmvtools::install(), not necessarily present in the
# default R library. Prepending that path here, when it exists, lets these
# tests exercise the module the same way it actually runs inside jamovi,
# rather than silently skipping every SEM-dependent test because lavaan/
# semTools were not found in the default library.
#
# ES: Descubre y ejecuta cada archivo test-*.R bajo tests/testthat/ contra el
# paquete FiabilityLab instalado.
#
# Usa deliberadamente test_dir() contra el propio directorio fuente de este
# archivo en vez del punto de entrada estándar de R CMD check /
# devtools::test() (testthat::test_check(), que busca tests/testthat/ DENTRO
# del paquete instalado). jmvtools::install() -- la forma en que este módulo
# realmente se instala, en la propia biblioteca de módulos de jamovi, no una
# biblioteca de R estándar vía R CMD INSTALL -- no empaqueta el directorio
# tests/ en la copia instalada, así que test_check() falla con "No test
# files found" aunque el paquete en sí se haya instalado correctamente.
# Correr contra el directorio fuente en su lugar es lo que realmente
# funciona para el propio ciclo de prueba/instalación de un módulo de
# jamovi; ver DEVELOPER_GUIDE.md. Corra este archivo con:
#   Rscript tests/testthat.R
# desde la raíz del proyecto (no vía R CMD check, por la misma razón).
#
# lavaan/semTools (Suggests, usados por Advanced Reliability y Measurement
# Invariance) solo están disponibles de forma confiable dentro de la propia
# biblioteca de módulos de jamovi -- instalados ahí vía jmvtools::install(),
# no necesariamente presentes en la biblioteca de R por defecto. Anteponer
# esa ruta aquí, cuando existe, permite que estas pruebas ejerciten el
# módulo de la misma forma en que realmente corre dentro de jamovi, en vez
# de omitir en silencio cada prueba dependiente de SEM porque lavaan/
# semTools no se encontraron en la biblioteca por defecto.
# -----------------------------------------------------------------------------

jamovi_module_lib <- Sys.getenv(
    "FIABILITYLAB_JAMOVI_LIB",
    file.path(path.expand("~"), ".jamovi", "modules", "fiabilitylab", "R")
)
if (dir.exists(jamovi_module_lib)) .libPaths(c(jamovi_module_lib, .libPaths()))

library(testthat)
library(fiabilitylab)

test_dir("tests/testthat", package = "fiabilitylab")
