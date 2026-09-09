# Contributing to FiabilityLab

Thank you for contributing to FiabilityLab, a Lab-suite Jamovi module for
measurement reliability, inter-rater agreement, SEM-based reliability and
measurement invariance. Contributions are welcome when
they improve scientific accuracy, reproducibility, education, accessibility,
maintainability or Jamovi compatibility.

FiabilityLab treats statistical software as a scientific publication: a change to
an estimator, warning, default or report can change the conclusions users draw.

## Guiding principles / Principios

- Scientific evidence before implementation speed.
- Transparent assumptions and limitations.
- Reproducible calculations and documented dependencies.
- Educational explanations, not opaque numerical output.
- Consistent bilingual interface and terminology.
- Small, reviewable, maintainable changes.
- Integrity before attractive or expected results.

Las mismas reglas se aplican a código, interfaz, documentación, referencias,
pruebas y decisiones metodológicas.

## Ways to contribute

Useful contributions include:

- reproducible bug reports;
- statistical or numerical validation;
- review of reliability or agreement definitions;
- missing-data and boundary-case tests;
- improvements to item-level diagnostics or dimensionality explanations;
- Jamovi UI and accessibility improvements;
- English/Spanish documentation and translation review;
- dependency, build and packaging improvements;
- new estimators proposed with evidence (not assumed to be accepted).

Do not open a pull request that exposes a new coefficient before its methodological
proposal has been reviewed.

## Before starting

1. Read `DEVELOPER_GUIDE.md` and `CODE_STYLE.md`.
2. Search existing issues and pending work.
3. Define whether the change is software, interface, documentation or methodology.
4. For statistical work, write the estimand, data design, assumptions and evidence
   before writing implementation code.
5. Check whether the change affects existing outputs, defaults or translations.
6. Open a discussion for substantial changes or new estimators.

## Statistical and psychometric contributions

A proposal for a reliability or agreement estimator must include:

- the scientific question and intended interpretation;
- the observational unit and measurement design;
- admissible data types and coding;
- treatment of missing and invalid values;
- the exact mathematical definition and notation;
- uncertainty and inferential procedures, if any;
- known biases, boundary behaviour and limitations;
- primary literature and software references;
- independent reference calculations or published examples;
- a plan for user-facing warnings and bilingual explanations.

The proposal must distinguish agreement from consistency, reliability from
validity, ordinal from continuous assumptions, and statistical significance from
practical importance. The existence of an implementation in `psych`, `irr`,
`irrCAC`, `lavaan`, `semTools` or another package is not sufficient evidence of
appropriateness for FiabilityLab.

The project deliberately does not define a final list of supported coefficients in
this document. Each estimator remains provisional until the methodological review
is complete.

## Code and interface changes

Follow `CODE_STYLE.md` and preserve the coordinated Jamovi contract:

- `.a.yaml` option names, types and defaults;
- `.u.yaml` controls, labels and enablement;
- `.r.yaml` result objects and formats;
- generated `.h.R` files;
- `.b.R` validation, computation and report logic;
- module manifest and generated artifacts where applicable.

Never expose a control for functionality that is not implemented. Do not change a
default, warning or interpretation without explaining its scientific impact.

## Documentation contributions

Documentation is a deliverable, not an afterthought. Update the relevant user,
developer and methodological text when a change affects behaviour. English and
Spanish versions must be semantically equivalent. References must identify the
definition or recommendation they support and should include stable bibliographic
metadata.

## Testing requirements

Before submitting a change, run the checks relevant to its scope:

- R parsing and package load;
- YAML parsing and `.u/.a/.r` name consistency;
- JavaScript syntax when generated UI changes;
- unit tests for numerical functions;
- independent reference comparisons;
- missingness, sparse categories, constant variables and small samples;
- unsupported design and informative-error tests;
- Jamovi result-object integration;
- English and Spanish report snapshots or equivalent review;
- build and `.jmo` content inspection.

Report the commands run, environment versions and any tests that could not be
executed. A successful compilation alone is not evidence that an analysis runs.

## Pull requests

A pull request should be focused and include:

- a concise problem statement;
- the proposed change and affected files;
- methodological rationale and references;
- compatibility and migration impact;
- tests and numerical validation;
- documentation and translation impact;
- known limitations and follow-up work.

Keep source and generated changes distinguishable. Do not include unrelated
formatting or refactoring. Never modify R or YAML files as part of a documentation-
only change unless explicitly requested and reviewed as a separate change.

## Bug reports

Include:

- FiabilityLab and Jamovi versions;
- R and `jmvcore` versions when relevant;
- operating system and installation source;
- a minimal dataset or reproducible data description;
- selected options and measurement design;
- exact steps to reproduce;
- expected and observed results;
- warnings, error messages and logs;
- whether the issue concerns computation, interpretation, UI or packaging.

Do not upload confidential measurement data. An anonymised or simulated fixture is
preferred.

## Feature requests

Explain the users and decision the feature serves, the methodological gap, data
conditions, expected report, evidence base, testing strategy and likely risks.
Requests for “more coefficients” should explain why the added estimator changes a
decision and how it will avoid confusing users.

## Scientific integrity and conduct

Contributors must not suppress warnings, alter calculations to match an expected
answer, remove observations without disclosure, or present unsupported certainty.
Disagreements about methods should be resolved through definitions, evidence,
reproducible examples and respectful discussion. Harassment, discrimination and
personal attacks are not acceptable.

## Licensing

By contributing, you agree that your contribution may be distributed under the
GNU General Public License v3.0, consistent with FiabilityLab and the Lab suite.
Third-party code and data must retain their original license and attribution.

## Final review questions

- Does the change improve a real scientific or educational use case?
- Can another contributor reproduce the result from the documentation?
- Are the assumptions and limitations visible to users?
- Are numerical results independently validated?
- Is the Jamovi interface consistent and bilingual?
- Does the change preserve existing behaviour unless a deliberate migration is
  documented?

Thank you for helping make reliability and agreement analysis more transparent,
evidence-based and teachable.
