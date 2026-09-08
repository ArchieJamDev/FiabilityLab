# FiabilityLab Code Style Guide

**Version:** 1.0  
**Project:** FiabilityLab  
**Suite:** Lab  
**License:** GNU GPL v3.0

FiabilityLab source code must be readable as technical and methodological
documentation. Clarity is part of scientific reproducibility: a future reviewer
must be able to identify the data transformation, estimand, assumptions, warning
and output represented by each section.

## 1. General principles / Principios generales

- Readability over cleverness.
- Explicit methodology over implicit assumptions.
- Consistency over personal preference.
- Small functions over opaque pipelines.
- Evidence and validation over shortcuts.
- Bilingual meaning without duplicated contradictions.

El código no debe limitarse a ejecutarse: debe permitir revisar cómo y por qué se
obtiene cada resultado.

## 2. GPL header and bilingual file description

New source files must begin with a GPL notice appropriate to the repository’s
licensing policy, followed by an English and Spanish description:

```r
# -----------------------------------------------------------------------------
# FiabilityLab — Internal Consistency analysis engine
# ES: Motor del análisis de Consistencia Interna de FiabilityLab.
#
# Implements input validation, data-quality diagnostics and report assembly.
# ES: Implementa validación, diagnósticos de calidad de datos y generación
# ES: del informe.
# -----------------------------------------------------------------------------
```

The description must identify the file’s responsibility and must not claim that a
method is validated if it is still provisional.

## 3. Responsibilities and workflow

Every non-trivial `.b.R` file should state its workflow near the top:

```text
Validate -> prepare -> describe missingness -> diagnose design
-> compute supported estimands -> quantify uncertainty
-> interpret -> assemble report
```

Keep validation, computation, diagnostics, interpretation and rendering in
recognisable sections. Do not hide methodological decisions inside a long
expression or a plotting callback.

## 4. Naming

Use descriptive names that identify the unit and purpose:

```r
complete_case_data
number_of_raters
ordinal_correlation_matrix
bootstrap_confidence_interval
build_agreement_warning
```

Avoid `x`, `tmp`, `aux`, `foo`, unexplained abbreviations and single-letter names
outside short mathematical loops. Names for coefficients must match the reviewed
definition and should not reuse a familiar label for a different calculation.

Functions should describe actions or transformations:

```r
validate_rating_design()
compute_item_diagnostics()
summarise_missing_ratings()
build_reliability_interpretation()
assemble_results_report()
```

## 5. Function documentation

Document every non-trivial function with:

- purpose;
- inputs and expected types;
- output and missing-value behaviour;
- relevant options;
- methodological rationale and references;
- error conditions and boundary cases.

Example:

```r
# -----------------------------------------------------------------------------
# Summarise missing ratings.
# ES: Resumir las calificaciones ausentes.
#
# Returns counts by observational unit and rater so that exclusions are visible
# before any agreement coefficient is computed.
# ES: Devuelve recuentos por unidad y evaluador para hacer visibles las
# exclusiones antes de calcular cualquier coeficiente de acuerdo.
# -----------------------------------------------------------------------------
summarise_missing_ratings <- function(rating_data) {
    ...
}
```

## 6. Methodological comments

Comments must explain why a decision exists, not repeat obvious syntax.

Bad:

```r
mean_score <- mean(scores)
# Compute the mean.
```

Good:

```r
# The descriptive mean is reported to show the response scale and to support
# interpretation of item variability; it is not itself a reliability estimate.
# ES: La media descriptiva muestra la escala de respuesta y apoya la
# ES: interpretación de la variabilidad; no es una estimación de confiabilidad.
```

Document decisions about ordinal treatment, dichotomous coding, missing data,
dimensionality, weighting, rater design, uncertainty and interpretation. Include
a citation or issue reference when the decision follows a published definition.

## 7. Bilingual documentation

English is written first and Spanish immediately below. Do not mix languages on a
single line. Both versions must contain the same conditions, warnings and degree
of uncertainty. Translation helpers should not silently change statistical meaning.

User-facing labels, error messages, report sections and methodological warnings
must be reviewed in both languages.

## 8. R structure and defensive errors

Use four spaces, no tabs, one blank line between logical blocks and a recommended
maximum line length of 100 characters. Prefer explicit `if` branches and named
intermediate values when they make the estimand or data transformation visible.

Validate before calculating. Errors and warnings must explain:

1. what is invalid;
2. why the requested analysis cannot proceed or is conditional;
3. how the user can correct the data or options.

Do not catch errors merely to return a plausible-looking number. If an estimator
fails, report the failure and its scope. Do not suppress package warnings without
documenting the reason and testing the consequence.

## 9. Reliability-specific conventions

Internal consistency code must distinguish item-level and scale-level quantities,
record the number of complete observations, preserve response coding, and expose
the missing-data rule. Ordinal/Likert and dichotomous paths must not be selected
solely because a variable happens to have few unique values. Dimensionality checks
inform interpretation; they must not silently alter the estimand.

Inter-rater code must make the observational unit, number of raters, rating level,
missing ratings, weighting and agreement/consistency target explicit. Do not use a
coefficient label as shorthand for a design that has not been validated.

No definitive list of supported coefficients is established by this style guide.
New estimators require methodological approval, independent numerical references
and a documented limitation statement.

## 10. YAML style

Keep `.a.yaml`, `.u.yaml` and `.r.yaml` names synchronized. Use explicit defaults,
types and permitted values. YAML comments should explain why an option exists and
what methodological decision it controls; do not merely repeat the field name.

UI labels must be bilingual or connected to the project’s translation mechanism.
Do not expose disabled, placeholder or unimplemented estimators. Result tables
must declare stable column names, formats and visibility expressions.

After changing YAML, validate parsing and compare option/result names with the R
header and engine. Generated files must be regenerated through the documented
Jamovi workflow rather than edited inconsistently by hand.

## 11. Reports and graphics

Report construction follows the analytical workflow. A result must not appear
before the data-quality and design context needed to interpret it. Tables should
include estimates, uncertainty and conditions where supported. HTML should escape
user-provided names and preserve bilingual warnings.

Graphics are diagnostic tools. Titles, axes and legends must identify the data
scale and methodological purpose. Do not use colour alone to communicate a warning
or threshold. Plot functions should return a clear failure state when required
data are unavailable.

## 12. Testing

Every new calculation needs tests for:

- known numerical examples from independent references;
- minimum and maximum valid dimensions;
- constant items or raters;
- sparse and unbalanced categories;
- missing and invalid values;
- dichotomous and ordinal coding;
- unsupported designs and informative errors;
- deterministic behaviour where a seed is specified;
- result-table and Jamovi API integration;
- English/Spanish equivalence.

Parsing and compilation are necessary but insufficient. A test that only loads a
package does not prove that a Jamovi analysis populates its result objects.

## 13. Scientific integrity and compatibility

Never alter an algorithm to match an expected coefficient. Never remove cases,
warnings or failed estimates silently. Preserve the distinction between a
calculation, a diagnostic and a recommendation. Document any change that affects
backward compatibility, defaults, output columns, interpretation or references.

Documentation-only changes must not modify algorithms or numerical results.

## 14. Review checklist

- [ ] GPL header and bilingual description are present.
- [ ] Responsibilities and workflow are documented.
- [ ] Names identify data units and statistical roles.
- [ ] Non-trivial functions document inputs, outputs and assumptions.
- [ ] Methodological decisions have evidence or an explicit provisional status.
- [ ] Errors explain correction steps.
- [ ] YAML options and result objects match R code.
- [ ] Reports and translations have equivalent meaning.
- [ ] Numerical, boundary and integration tests are included.
- [ ] No unsupported estimator is exposed as production functionality.

## 15. Golden rule

Every comment should answer a question the code alone cannot answer: why this
transformation, why this estimator, why this warning, why this exclusion, or what
limitation follows. If it only repeats the syntax, remove it. If it preserves the
scientific reasoning, keep it.
