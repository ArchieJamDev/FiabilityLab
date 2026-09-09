# FiabilityLab Architecture

**Version:** 1.0
**Project:** FiabilityLab
**Suite:** Lab
**License:** GNU General Public License v3.0
**Author:** Arquímedes De León Chacón Chacón

---

# Table of Contents

1. Introduction
2. Architectural Philosophy
3. High-Level Architecture
4. Directory Structure
5. Module Architecture
6. Data Flow
7. Analysis Lifecycle
8. Report Generation Architecture
9. Interpretation Engine
10. Graphical Architecture
11. Methodological Library
12. Internationalization
13. Documentation Architecture
14. Future Expansion
15. Design Principles

---

# 1. Introduction

FiabilityLab has been designed as a modular scientific software platform for
evaluating measurement reliability — internal consistency, temporal
stability, inter-rater agreement, and construct-level composite reliability
— and for supporting the methodological decisions that follow from those
estimates.

The architecture prioritizes

• scientific rigor;

• modularity;

• maintainability;

• educational value;

• reproducibility.

Every component has a clearly defined responsibility. FiabilityLab shares
its architectural lineage with AssumptionsLab (Lab suite): the same
four-file module contract, the same bilingual reporting discipline, and the
same principle that the software must teach, not just compute.

-------------------------------------------------------------------------------

# Introducción

FiabilityLab ha sido diseñado como una plataforma modular para la evaluación
de la confiabilidad de la medición — consistencia interna, estabilidad
temporal, acuerdo entre jueces y confiabilidad compuesta a nivel de
constructo — y para apoyar las decisiones metodológicas que se derivan de
esas estimaciones.

La arquitectura prioriza

• rigor científico;

• modularidad;

• mantenibilidad;

• valor educativo;

• reproducibilidad.

Cada componente posee una responsabilidad claramente definida. FiabilityLab
comparte linaje arquitectónico con AssumptionsLab (suite Lab): el mismo
contrato de módulo de cuatro archivos, la misma disciplina de reporte
bilingüe, y el mismo principio de que el software debe enseñar, no solo
calcular.

-------------------------------------------------------------------------------

# 2. Architectural Philosophy

The architecture follows five fundamental principles.

### Separation of responsibilities

Each file performs one well-defined task.

### Progressive workflow

Every analysis follows the same logical sequence: describe the design,
compute the coefficient(s), compare them, interpret, recommend.

### Educational software

The software explains why a reliability coefficient is trustworthy — or
is not — rather than merely printing a number between 0 and 1.

### Scientific transparency

Every methodological decision (which coefficient applies to which design,
measurement level, and assumption set) can be identified inside the source
code and inside the Library.

### Scalability

New reliability designs (temporal stability, classification consistency,
generalizability theory) must integrate without modifying the existing
architecture.

-------------------------------------------------------------------------------

# Filosofía arquitectónica

Cada nuevo módulo debe adaptarse a la arquitectura existente.

La arquitectura nunca debe adaptarse a un módulo específico.

-------------------------------------------------------------------------------

# 3. High-Level Architecture

```
                USER

                  │

                  ▼

        User Interface (.yaml)

                  │

                  ▼

        Analysis Engine (.R)

                  │

                  ▼

     Reliability Estimation

                  │

                  ▼

   Coefficient Discordance Panel

                  │

                  ▼

 Methodological Interpretation

                  │

                  ▼

        Report Generation

                  │

                  ▼

            Final Output
```

The **Coefficient Discordance Panel** is FiabilityLab's distinguishing
stage: whenever more than one estimator is available for the same design
(alpha vs. omega vs. GLB; Cohen's Kappa vs. Gwet's AC1), the module reports
not only the values but whether they disagree — and disagreement is itself
diagnostic (e.g., alpha ≫ omega signals a broken tau-equivalence
assumption; Kappa ≪ AC1 signals the prevalence/bias paradox).

-------------------------------------------------------------------------------

# Arquitectura general

Toda información sigue un flujo unidireccional.

El panel de discordancia entre coeficientes no es un extra decorativo: es
el mecanismo diagnóstico central de FiabilityLab.

-------------------------------------------------------------------------------

# 4. Directory Structure

Current, as of this writing:

```
FiabilityLab/

│

├── R/

├── jamovi/

│   └── assets/        # module icon (fiabilitylab-icon.png/.svg)

├── inst/

│   └── i18n/

├── data/

├── data-raw/

├── docs/

├── tests/

├── build/

├── .github/           # CI workflow (jamovi-check.yml)

├── LICENSE

├── CODE_STYLE.md

├── DEVELOPER_GUIDE.md

├── CONTRIBUTING.md

├── ARCHITECTURE.md

├── README.md

├── NEWS.md

├── CITATION.cff

├── .zenodo.json

├── DESCRIPTION

└── NAMESPACE
```

Planned, not yet created (tracked here so the gap is explicit rather than
silently assumed done):

```
└── validation/      # Monte Carlo validation scripts (bias, coverage,
                     # robustness under realistic, not only asymptotic,
                     # conditions) -- deliberately deferred: no such study
                     # has been run for FiabilityLab's own estimators yet.
                     # Will be populated once the planned validation
                     # article's simulations exist; creating this folder
                     # with placeholder or fabricated results before then
                     # would violate section 13's scientific-integrity rule.
```

-------------------------------------------------------------------------------

## Responsibilities

### R/

Implements every statistical algorithm.

### jamovi/

Defines analysis options and user interfaces. `jamovi/assets/` holds the
module icon (`fiabilitylab-icon.png`, generated from the committed
`fiabilitylab-icon.svg` source), referenced from `jamovi/0000.yaml`'s
top-level `icon:` field.

### inst/

Package-installed resources: icons/logos, i18n resources, and the
plain-text `CITATION` file.

### data/ and data-raw/

`data/` ships the bundled example dataset(s) shown inside jamovi
(registered under `datasets:` in `jamovi/0000.yaml`) — at minimum one
multi-item scale dataset and one multi-rater agreement dataset;
`data-raw/` holds the scripts that produce them reproducibly.

### docs/

Contains project documentation, including the Master Document.

### tests/

Stores unit tests (`testthat`) for every estimator.

### validation/ (planned)

Will store Monte Carlo validation scripts that check bias, coverage, and
robustness of every reliability estimator under realistic — not only
asymptotic — conditions (non-normal item distributions, missingness,
small samples, unbalanced rater designs). Not yet created.

-------------------------------------------------------------------------------

# 5. Module Architecture

Every analysis consists of four primary components.

```
analysis.a.yaml

↓

analysis.u.yaml

↓

analysis.r.yaml

↓

analysis.b.R
```

-------------------------------------------------------------------------------

## analysis.a.yaml

Defines

• options

• variables

• controls

• defaults

-------------------------------------------------------------------------------

## analysis.u.yaml

Defines

• layout

• groups

• visibility

• interface organization, including collapsible option sections within a
  single analysis (e.g., Internal Consistency's Plot Style box) and, when
  a feature's own options grow too large for that (e.g., defining a
  dynamic, unlimited number of named factors for confirmatory analysis), a
  separate menu analysis instead (Advanced Reliability (SEM), split out
  from an originally-planned embedded tab)

-------------------------------------------------------------------------------

## analysis.r.yaml

Defines

• result tables

• images

• HTML outputs

• textual reports

-------------------------------------------------------------------------------

## analysis.b.R

Implements

• validation

• computations

• coefficient discordance diagnostics

• graphics

• interpretations

• reporting

-------------------------------------------------------------------------------

# 6. Data Flow

```
User Selection

↓

Design Detection (items / raters / occasions / factor structure)

↓

Input Validation

↓

Dataset Preparation

↓

Missing Data Processing

↓

Descriptive Statistics

↓

Measurement-Level Detection

↓

Reliability Estimation

↓

Coefficient Discordance Check

↓

Diagnostic Graphics

↓

Interpretation Engine

↓

Report Assembly

↓

Output
```

-------------------------------------------------------------------------------

# Flujo de datos

Cada etapa depende únicamente de la anterior.

La detección del diseño (ítems / jueces / ocasiones / estructura factorial)
ocurre antes de cualquier cálculo, para que el módulo ofrezca únicamente
los coeficientes aplicables a los datos presentes.

-------------------------------------------------------------------------------

# 7. Analysis Lifecycle

Every module should follow exactly the same lifecycle.

```
Initialization

↓

Validation

↓

Preparation

↓

Estimation

↓

Discordance Diagnostics

↓

Interpretation

↓

Recommendations

↓

Report

↓

Finish
```

This workflow must remain consistent throughout the project.

-------------------------------------------------------------------------------

# Ciclo de vida del análisis

La experiencia del usuario debe ser idéntica en todos los módulos.

-------------------------------------------------------------------------------

# 8. Report Generation Architecture

Reports are generated after every statistical computation has been
completed. The report is composed of

Introduction

↓

Design / Data Summary

↓

Reliability Coefficients

↓

Coefficient Discordance Diagnostics

↓

Interpretation

↓

Recommendations

↓

References

Each section should be generated independently.

-------------------------------------------------------------------------------

# Arquitectura del informe

La generación del informe nunca debe mezclarse con los cálculos
estadísticos.

-------------------------------------------------------------------------------

# 9. Interpretation Engine

The interpretation engine represents one of the most important components
of FiabilityLab. Its objective is to transform reliability estimates into
methodological recommendations.

```
Reliability Estimate(s)

↓

Assumption Rules (tau-equivalence, unidimensionality, prevalence, rater
independence, factorial invariance)

↓

Interpretation Templates

↓

Educational Explanation

↓

Recommendations
```

The engine should never merely reproduce numerical values. It must always
answer four questions.

-------------------------------------------------------------------------------

# Motor de interpretación

Toda interpretación debe responder

¿Qué tan confiable es la medida?

¿Por qué (qué supuesto sostiene o rompe ese valor)?

¿Qué implica para la interpretación de los puntajes?

¿Qué debe hacer ahora el investigador (aceptar, revisar ítems, cambiar de
coeficiente, recolectar más datos)?

-------------------------------------------------------------------------------

# 10. Graphical Architecture

Graphs are organized according to methodological purpose.

```
Item Response Distributions

↓

Item-Total Correlations

↓

Alpha-if-Dropped Sensitivity

↓

Dimensionality (Scree / Parallel Analysis)

↓

Agreement / Discordance Plots

↓

Reliable-Change / SEM Bands

↓

Final Diagnostics
```

Graphs should reinforce interpretation rather than duplicate numerical
results.

-------------------------------------------------------------------------------

# Arquitectura gráfica

Los gráficos constituyen herramientas metodológicas.

No elementos decorativos.

-------------------------------------------------------------------------------

# 11. Methodological Library

The Library is an independent educational subsystem — the same role it
plays in AssumptionsLab. It is not a secondary feature; it is the content
system every analysis module surfaces.

Its objectives are

• explain concepts;

• define reliability coefficients and their assumptions;

• describe when a coefficient applies and when it does not;

• support learning;

• complement reports.

**Admission contract:** no coefficient is implemented in an analysis module
until its Library entry (concept, assumptions, failure modes, guidance) and
its Bibliography citations already exist. Library and Bibliography grow
first; the `.b.R` computation follows.

The Library should remain independent from statistical computations.

-------------------------------------------------------------------------------

# Biblioteca metodológica

La Library constituye un diccionario metodológico integrado.

No realiza cálculos.

Explica resultados.

Ningún coeficiente entra al roadmap de código sin su ficha en Library y su
cita en Bibliography.

-------------------------------------------------------------------------------

# 12. Internationalization

FiabilityLab follows a bilingual philosophy.

Source code

English

↓

Spanish comments

User interface

Language files

↓

Translations

Reports

Localized text

↓

User language

Future translations should not require architectural modifications.

-------------------------------------------------------------------------------

# Internacionalización

La arquitectura está preparada para incorporar nuevos idiomas.

-------------------------------------------------------------------------------

# 13. Documentation Architecture

Documentation exists at four levels.

```
Repository

↓

Module

↓

Source File

↓

Function
```

Each level should answer progressively more detailed questions.

Repository

What is FiabilityLab?

Module

What reliability design is implemented?

Source File

How is the analysis organized?

Function

How is each task performed?

-------------------------------------------------------------------------------

# Arquitectura documental

La documentación constituye un componente de la arquitectura.

No un elemento accesorio.

-------------------------------------------------------------------------------

# 14. Future Expansion

The architecture has been designed to accommodate future developments
without major structural modifications. The confirmed roadmap is:

**Phase 1 — Inter-Rater Agreement (current priority)**
Cohen's/Fleiss' Kappa, Gwet's AC1/AC2, Intraclass Correlation (ICC forms
1,1 / 2,1 / 3,1), Krippendorff's Alpha, Kendall's W.

**Phase 2 — Temporal Stability**
Test-retest correlation, longitudinal ICC, Bland-Altman-style limits of
agreement, Standard Error of Measurement (SEM), Reliable Change Index
(RCI, Jacobson-Truax) — plus coefficient discordance panels extended to
Internal Consistency and Inter-Rater.

**Phase 3 — Advanced Reliability (SEM) + Measurement Invariance**
Implemented as `advancedReliability`, its own FiabilityLab menu analysis
(revised from the original plan of an embedded tab inside Internal
Consistency, once a point-and-click factor-definition UI with a dynamic,
unlimited number of named factors made a nested collapse box too
cluttered): AVE, Composite Reliability (CR), Hancock & Mueller's H
coefficient, omega generalized to second-order/hierarchical factor
structures, HTMT, and modification-index diagnostics (interpreted through
theory, never applied automatically, per MacCallum, Roznowski & Necowitz,
1992) — fit via `lavaan`/`semTools` once the user specifies a factor
structure (items→subscales, subscales→second-order factor). Alongside it,
`measurementInvariance` extends the same confirmatory measurement model to
multi-group configural/metric/scalar/strict invariance testing (LRT and
ΔCFI criteria).

**Phase 4 — Generalizability Theory (exploratory, no committed date)**
G-study/D-study variance-components decomposition. Classification/Decision
Consistency (Livingston-Lewis, Subkoviak) for cut-score-based instruments
remains a candidate for a future phase; it has not been implemented.

Every future module or tab should reuse the same architecture and honor
the Library/Bibliography admission contract.

-------------------------------------------------------------------------------

# Expansión futura

La escalabilidad constituye un principio fundamental del proyecto.

Advanced Reliability (SEM) es la misma pregunta de investigación que
Internal Consistency (¿qué tan confiable es este instrumento?) resuelta con
un motor distinto (modelo de medida confirmatorio en vez de estadística
directa sobre datos crudos), pero vive como su propio análisis de menú:
definir un número dinámico e ilimitado de factores nombrados, cada uno con
su propia lista de ítems, no cabía bien como una sección más dentro de las
opciones de Internal Consistency.

-------------------------------------------------------------------------------

# 15. Design Principles

Every architectural decision should satisfy the following principles.

### Scientific

Algorithms should faithfully implement accepted psychometric procedures.

### Educational

Users should understand every methodological decision.

### Modular

Components should remain independent whenever possible.

### Transparent

Every important decision should be documented.

### Maintainable

Future developers should understand the architecture without external
guidance.

### Consistent

Every module should behave as part of the same software ecosystem —
including consistency with its sibling project, AssumptionsLab.

### Reproducible

Analyses should produce reproducible results from identical data, and
every estimator should carry a Monte Carlo validation record before it is
considered stable.

-------------------------------------------------------------------------------

# Principios de diseño

La arquitectura de FiabilityLab pretende equilibrar

ingeniería del software,

psicometría,

experiencia del usuario

y

valor educativo.

Estos principios deberán preservarse durante toda la evolución del
proyecto.

-------------------------------------------------------------------------------

# Final Statement

FiabilityLab has been designed as an extensible scientific platform rather
than a collection of independent reliability calculators. Its architecture
seeks to guarantee scientific quality, educational excellence, software
sustainability and methodological transparency for researchers, students
and developers worldwide.

-------------------------------------------------------------------------------

# Declaración final

FiabilityLab ha sido concebido como una plataforma científica extensible y
no como una colección de calculadoras de confiabilidad independientes. Su
arquitectura busca garantizar calidad científica, excelencia educativa,
sostenibilidad del software y transparencia metodológica para
investigadores, estudiantes y desarrolladores de todo el mundo.

-------------------------------------------------------------------------------

**End of document**
