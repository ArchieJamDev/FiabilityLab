<p align="center">
  <img src="jamovi/assets/fiabilitylab-icon.png" width="120" alt="FiabilityLab logo">
</p>

# FiabilityLab

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](LICENSE)
[![GitHub release](https://img.shields.io/github/v/release/ArchieJamDev/FiabilityLab)](https://github.com/ArchieJamDev/FiabilityLab/releases)
[![CI](https://github.com/ArchieJamDev/FiabilityLab/actions/workflows/jamovi-check.yml/badge.svg)](https://github.com/ArchieJamDev/FiabilityLab/actions/workflows/jamovi-check.yml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22682200.svg)](https://doi.org/10.5281/zenodo.22682200)
![FiabilityLab views](https://komarev.com/ghpvc/?username=ArchieJamDev-FiabilityLab&style=flat-square&color=181717&label=FiabilityLab+Views)

> **A jamovi module for measurement reliability, inter-rater agreement, and measurement invariance, paired with methodological guidance.**

FiabilityLab is an open-source [jamovi](https://www.jamovi.org/) module built to help researchers, students, and practitioners evaluate the reliability of their measurement instruments and the agreement between raters. Unlike conventional statistical software that simply reports a coefficient, FiabilityLab pairs every estimator with its assumptions, its limitations, and an explanation of what it does and does not support — including a discordance panel that flags, and explains, when two coefficients for the same design disagree.

## Contents

- [Why FiabilityLab?](#why-fiabilitylab)
- [Modules](#modules)
- [Methodological Philosophy](#methodological-philosophy)
- [Installation](#installation)
- [For Developers](#for-developers)
- [Roadmap](#roadmap)
- [Contributing](#contributing)
- [Citation](#citation)
- [License & Author](#license--author)

---

## Why FiabilityLab?

Reliability coefficients are frequently reported as a single number checked once against a rule-of-thumb cutoff. But which coefficient is even appropriate depends on the measurement design (items vs. raters vs. occasions), the data's measurement level, and assumptions that are rarely checked explicitly — and different coefficients for the same design can disagree in ways that are themselves diagnostic.

FiabilityLab bridges the gap between statistical computation and methodological reasoning by providing:

- Evidence-based methodological guidance alongside every coefficient, not just the number.
- Explicit assumption checks (tau-equivalence, homoscedasticity of the ANOVA residuals, dimensionality, item coding) instead of silently assuming they hold.
- A discordance panel that surfaces and explains disagreement between coefficients (e.g., Alpha ≫ Omega signals a broken tau-equivalence assumption; Kappa ≪ Gwet's AC1 signals the prevalence/bias paradox) rather than only reporting whichever the user selected.
- A built-in methodological Library and a unified bibliography, so a coefficient can be traced back to its source literature without leaving jamovi.
- Full transparency about silent option overrides (e.g., when ordinal items force WLSMV estimation and listwise deletion regardless of what was selected) and about bootstrap replicate counts actually used, not just requested.

Rather than asking users to memorize reliability rules, FiabilityLab helps them understand **what kind of consistency a coefficient measures, under what conditions it is trustworthy, and what it does not tell them**.

---

## Modules

| Module | What it computes |
|---|---|
| **Internal Consistency** | Cronbach's Alpha (raw and ordinal/polychoric), McDonald's Omega (with hierarchical Omega when ≥2 factors are detected), Greatest Lower Bound (GLB), split-half reliability, Guttman's Lambda, KR-20/21 (only for genuinely 0/1-coded items), item-total correlations and item-level diagnostics, bootstrap confidence intervals. |
| **Inter-Rater Agreement** | Cohen's/Fleiss' Kappa, Gwet's AC1/AC2, Intraclass Correlation (ICC forms 1,1 / 2,1 / 3,1), Krippendorff's Alpha, Kendall's W, a coefficient-discordance panel, bootstrap confidence intervals. |
| **Advanced Reliability (SEM)** | Confirmatory-factor-based reliability: Average Variance Extracted (AVE), Composite Reliability (CR), Hancock & Mueller's H coefficient, hierarchical Omega for second-order factor structures, HTMT, and theory-guided modification-index diagnostics — fit via `lavaan`/`semTools` once the user specifies a factor structure. |
| **Measurement Invariance** | Multi-group configural/metric/scalar/strict invariance testing on the same confirmatory measurement model, using both the Likelihood Ratio Test and ΔCFI criteria, with a five-way verdict classification per level (supported by both criteria / by one only / not supported / undetermined) and an explicit statement of which comparisons the highest fully-supported level actually licenses. |
| **Fiability Library** | A bilingual (EN/ES) glossary explaining reliability concepts, designs, and assumptions — a reference, not a computation. |
| **Bibliography** | A unified, topic-filtered bibliography of the verified methodological sources cited throughout the module. |

Every computing module reports both English and Spanish interpretations and shares the same validate → compute → quantify uncertainty → interpret workflow.

---

## Methodological Philosophy

FiabilityLab is educational scientific software, not a coefficient calculator, built around the Lab-suite principles it shares with its sibling module, [AssumptionsLab](https://github.com/ArchieJamDev/AssumptionsLab):

1. Scientific rigor and methodological evidence come before implementation speed.
2. Every default, warning, and interpretation is transparent about the assumptions and limitations behind it.
3. Calculations, versions, and reports are reproducible.
4. Explanations are written for students and researchers to learn from, not just to read a number.
5. No estimator is presented as universally "good" or "bad" — a coefficient is evidence about a defined source of measurement error, not a substitute for validity evidence or substantive judgment.

---

## Installation

FiabilityLab has not yet been released (no GitHub Release or jamovi Library submission yet). For now, build it from source:

```bash
git clone https://github.com/ArchieJamDev/FiabilityLab.git
cd FiabilityLab
./Compilar_FiabilityLab.sh
```

A bundled example dataset (documented column by column in [`data/README.md`](data/README.md)) ships with the module and shows up under **File → Open → Data Library → FiabilityLab** in jamovi Desktop once installed.

---

## For Developers

Architecture, code conventions, and the full contributor workflow are documented separately so this README stays a project overview, not a manual:

- [`ARCHITECTURE.md`](ARCHITECTURE.md) — module structure, data flow, and roadmap phases.
- [`DEVELOPER_GUIDE.md`](DEVELOPER_GUIDE.md) — the statistical and interface development contract.
- [`CODE_STYLE.md`](CODE_STYLE.md) — the bilingual documentation and coding conventions every file follows.
- [`NEWS.md`](NEWS.md) — release history.

```bash
Rscript -e "jmvtools::prepare(); jmvtools::install()"   # regenerate .h.R from the yaml files and install into jamovi
Rscript tests/testthat.R                                 # run the test suite (see DEVELOPER_GUIDE.md section 12 for why)
```

---

## Roadmap

Planned future developments (see [`ARCHITECTURE.md`](ARCHITECTURE.md#14-future-development) for the full phase-by-phase rationale):

- Temporal Stability (test-retest correlation, longitudinal ICC, Bland-Altman limits of agreement, SEM, Reliable Change Index)
- Classification/Decision Consistency for cut-score-based instruments (Livingston-Lewis, Subkoviak)
- Generalizability Theory (G-study/D-study variance-components decomposition)

---

## Contributing

Contributions are welcome. Please read [`CONTRIBUTING.md`](CONTRIBUTING.md) before submitting issues, feature requests, or pull requests.

---

## Citation

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22682200.svg)](https://doi.org/10.5281/zenodo.22682200)

If you use FiabilityLab in research, please cite the software using the information in [`CITATION.cff`](CITATION.cff), or via GitHub's "Cite this repository" button. The DOI above is the concept DOI — it always resolves to the latest release; cite a version-specific DOI instead only if you need to pin the exact version used in a specific analysis.

---

## License & Author

FiabilityLab is licensed under the **GNU General Public License v3.0 (GPL-3.0)** — see [`LICENSE`](LICENSE).

**Arquímedes De León Chacón Chacón** — Psychologist · Data Scientist · Research Methodologist. Project Founder and Lead Developer. Universidad Católica Andrés Bello (UCAB), Caracas, Venezuela. [ORCID](https://orcid.org/0000-0002-7014-7513)

**Arquímedes De León Chacón Chacón** — Psicólogo · Científico de Datos · Metodólogo de Investigación. Fundador y Desarrollador Principal del proyecto. Universidad Católica Andrés Bello (UCAB), Caracas, Venezuela. [ORCID](https://orcid.org/0000-0002-7014-7513)

Copyright © 2026 Arquímedes De León Chacón Chacón
