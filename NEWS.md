# FiabilityLab 1.5.0 (2026-09-17)

Response to jamovi's official module review (2026-09-16). All 6 analyses
(Internal Consistency, Inter-Rater Agreement, Advanced Reliability (SEM),
Measurement Invariance, Fiability Library, Bibliography) affected.

* Fixed 4 plots (`plotItemTotal`, `plotComparison`, `plotDiagnostic`,
  `plotInvariance`, and others) rendering blank on export by moving their
  data into `setState()`/`image$state`, instead of reading `private$`
  fields that are `NULL` on jamovi's export-only analysis instance.
- Fixed Advanced Reliability's `rel_g` (reliability-due-to-G) column
  showing `NA` for every multi-factor model due to an unwrapped
  `semTools::compRelSEM()` list value.
- Replaced ad hoc `setVisible(FALSE)`-everything error handling with
  `jmvcore::reject()` across all 6 analyses, so failed/blocked analyses
  show jamovi's own stable, greyed error pane instead of a blank one.
- Made variable names with spaces, accents, or symbols safe for lavaan
  model-syntax string-building (Advanced Reliability, Measurement
  Invariance), instead of producing malformed models or silent
  mis-parses.
- Replaced literal HTML entities in report text with their Unicode
  characters.
- Migrated every YAML-derived UI string (option titles, checkbox labels,
  table column headers) to jamovi's native `.()` translation catalog
  (`jamovi/i18n/catalog.pot`, `jamovi/i18n/es.po`), which switches
  language instantly with jamovi's own global UI language setting. Report
  *text* (the dynamically-generated prose, table interpretation labels,
  and error messages) keeps its own per-analysis `reportLang` option and
  `tr(en, es)` dispatch -- an initial attempt to migrate that too was
  reverted after confirming jamovi's native catalog freezes its
  translator once per analysis instance and does not pick up a language
  change without a full jamovi restart, unlike the instant, per-analysis
  `reportLang` dropdown. Fiability Library and Bibliography remain
  separate analyses.
- Migrated all plots from a custom `plotStyle` option to jamovi's native
  theme/palette system (`ggtheme`/`theme`, from `self$options$theme` and
  `self$options$palette`).
- Reorganized Internal Consistency's and Inter-Rater Agreement's option
  panels: reliability statistics/coefficients now sit directly under the
  variable box; secondary sections (Measurement Level, Item Analysis,
  Dimensionality, Reliability Assumptions, Bootstrap, Diagnostic Plots,
  Data Type, ICC Assumptions) start collapsed; long paragraph-length
  option descriptions moved into hover tooltips; only core coefficients
  are on by default, with bootstrap CIs, parallel analysis, and
  assumption checks now opt-in. KR-20/KR-21 default to off and report an
  explicit "Not applicable" row on non-dichotomous data instead of
  silently producing nothing.
- Fixed Fiability Library's category filter missing the "Measurement
  Invariance" option that its own results already supported.

# FiabilityLab 1.0.0 (2026-09-09)

Initial release.
