# Discordance panel validation

Monte Carlo validation of FiabilityLab's **discordance panel** — the
feature that flags disagreement between two coefficients computed for the
same reliability/agreement design (e.g. Alpha vs. Omega, Kappa vs. Gwet's
AC1/AC2, ICC consistency vs. ICC absolute-agreement) and explains why.

## Scope and rationale

This does **not** validate the underlying coefficients themselves (Alpha,
Omega, Kappa, Gwet's AC1/AC2, ICC) — those already have decades of
published statistical literature behind them. It validates FiabilityLab's
own contribution on top of them: the three fixed-threshold rules that
decide when to flag a disagreement as diagnostically meaningful. This
mirrors how AssumptionsLab's own AJS Monte Carlo work validates that
module's internal decision rules (e.g. `groupCheck`'s classical-vs-Welch
recommendation), not the underlying statistical tests those rules sit on
top of.

Each script calls FiabilityLab's own exported analysis function
(`internalConsistency()` or `interRater()`, loaded via
`pkgload::load_all(".")`, no installation needed) on simulated data with
fully known ground truth, and reads whether the shipped discordance panel
actually fires (`results$discordanceNote$visible` and its content text).
**None of the three coefficients are reimplemented** — the validation
exercises the real, shipped code path, the same way a user's own analysis
would, rather than a separate idealized reimplementation of the formulas.

## The three rules under test

| Script | Rule | Source |
|---|---|---|
| `alpha_omega_mc.R` | flag iff `\|α − ω\| > .04` | `R/internalconsistency.b.R:898` |
| `kappa_gwet_mc.R` | flag iff `\|Kappa − Gwet's AC1/AC2\| > .05` (nominal/ordinal only) | `R/interrater.b.R:603-616` |
| `icc_consistency_agreement_mc.R` | flag iff `(ICC_consistency − ICC_absolute) > .05` — **signed, not `abs()`** (continuous only) | `R/interrater.b.R:640-651` |

All three are fixed absolute-difference thresholds, not statistical tests
— none of them scale with sample size or account for sampling
uncertainty in the coefficients being compared. Each script measures, as a
function of a manipulated ground-truth violation magnitude and sample
size `n`:

- **Specificity**: does the panel stay silent when the assumption behind
  the "better" coefficient actually holds (violation = 0, the null case)?
- **Sensitivity**: at what violation magnitude and `n` does the panel
  start reliably firing?

## Reproducing

Run each script from the package root (each takes 15 min to ~2 hours
depending on `n_reps` — see the runtime note at the top of each file; the
per-call cost is dominated by FiabilityLab's own computation, e.g.
`psych::omega()`'s internal parallel-analysis step, not by anything this
script does):

```bash
Rscript validation/alpha_omega_mc.R [n_reps]                    # default 60/cell
Rscript validation/kappa_gwet_mc.R [n_reps]                     # default 150/cell
Rscript validation/icc_consistency_agreement_mc.R [n_reps]      # default 150/cell
```

Each script fixes `set.seed(20260909)` before its simulation loop, so a
re-run with the same `n_reps` reproduces the same per-replicate numbers.
Every script writes its full per-replicate results to a CSV alongside it
(`results_*.csv`) as well as printing a firing-rate summary table to
stdout — the CSVs are committed so the numbers behind any figure/claim in
the article can be re-derived without re-running the simulation.

### `alpha_omega_mc.R`

Simulates a single-factor scale (`k = 6` items, mean loading `.65`) and
manipulates loading heterogeneity (`loading_sd`, 0 = tau-equivalent null
case up to .35 = strongly unequal loadings) across `n ∈ {50, 150, 400}`.

### `kappa_gwet_mc.R`

Simulates 2 raters independently misclassifying a true binary state at a
**fixed** per-rater error rate (.08, held constant across conditions so
raw observed agreement doesn't confound the manipulation) and manipulates
category prevalence (`.50` = balanced null case up to `.95` = extreme
skew) across `n ∈ {50, 150, 400}` — the classic "Kappa paradox" setup
(Gwet, 2014).

### `icc_consistency_agreement_mc.R`

Simulates `k = 3` raters scoring a common true score, with a systematic
additive shift applied to one rater (`shift`, 0 = null case up to 2.0)
across `n ∈ {20, 50, 150}`. Also runs a **directionality check**
(`results_icc_directionality_mc.csv`): since the rule is signed, not
`abs()`, pure-noise conditions with no systematic shift should never fire
the panel even as both ICC forms degrade together — a false-positive rate
above ~0 there would mean the rule reacts to general unreliability rather
than specifically to rater bias.

## Prior art checked

No existing Monte Carlo work validates FiabilityLab's specific
fixed-threshold flagging rules (a genuine gap this fills), but each
underlying phenomenon is independently well-established in the
literature, which grounds the simulation design:

- Alpha underestimates true reliability by 0.6%-11.1% under non-tau-
  equivalence, depending on violation severity (Trizano-Hermosilla &
  Alvarado, 2016, *Frontiers in Psychology*,
  [10.3389/fpsyg.2016.00769](https://doi.org/10.3389/fpsyg.2016.00769)).
- The Kappa paradox under skewed prevalence is well documented (Gwet,
  2014; see also Wongpakaran et al., 2013, on Kappa vs. Gwet's AC1 in
  practice).
- A consistency ICC exceeding absolute-agreement ICC is a validated
  simulation-literature signature of systematic rater bias (Shrout &
  Fleiss, 1979); near-equality of ICC forms indicates its absence.
