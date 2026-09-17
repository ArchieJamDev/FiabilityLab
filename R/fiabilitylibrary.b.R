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
# FiabilityLab - Fiability Library.
#
# A reference glossary explaining what each reliability coefficient and
# theoretical framework means and when it applies, across FiabilityLab. It
# does not analyze the user's data; each analysis module interprets its own
# results separately. Every claim that names a specific method is anchored to
# its source citation (Author, Year), matching every entry already curated in
# the Bibliography module -- no coefficient appears here without a citation,
# and no citation appears here without a matching Bibliography entry.
#
# ES: Un glosario de referencia que explica qué significa cada coeficiente de
# confiabilidad y marco teórico, y cuándo aplica, en todo FiabilityLab. No
# analiza los datos del usuario; cada módulo de análisis interpreta sus
# propios resultados por separado. Toda afirmación que nombra un método
# específico está anclada a su cita de origen (Autor, Año), coincidiendo con
# cada entrada ya curada en el módulo Bibliography -- ningún coeficiente
# aparece aquí sin una cita, y ninguna cita aparece aquí sin una entrada
# correspondiente en Bibliography.
# -----------------------------------------------------------------------------

fiabilityLibraryClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
  "fiabilityLibraryClass",
  inherit = fiabilityLibraryBase,
  private = list(

    .esc = function(x) {
      x <- gsub("&", "&amp;", x, fixed = TRUE)
      x <- gsub("<", "&lt;", x, fixed = TRUE)
      x <- gsub(">", "&gt;", x, fixed = TRUE)
      x
    },

    .title_html = function(title) {
      paste0('<p style="font-weight:700;font-size:1.1em;line-height:1;margin:0 0 0.5em 0;">', title, '</p>')
    },

    .table_html = function(headers, rows) {
      th <- paste0('<th style="text-align:left;padding:4px 8px;border-bottom:2px solid #999;font-weight:700;">',
                   headers, '</th>', collapse = "")
      trs <- vapply(rows, function(r) {
        tds <- paste0('<td style="text-align:left;padding:4px 8px;border-bottom:1px solid #ddd;vertical-align:top;">',
                      r, '</td>', collapse = "")
        paste0("<tr>", tds, "</tr>")
      }, character(1))
      paste0('<table style="border-collapse:collapse;width:100%;margin:0.5em 0 0.8em 0;font-size:0.92em;line-height:1;">',
             "<thead><tr>", th, "</tr></thead><tbody>", paste(trs, collapse = ""), "</tbody></table>")
    },

    # EN: text-align: justify here (and line-height: 1 on every element
    # below) is the same convention Bibliography uses for its own body
    # text -- see .fl_prose_open() in shared-helpers.R, used by
    # internalConsistency and interRater's Html panels for the same reason.
    # ES: text-align: justify aquí (y line-height: 1 en cada elemento de
    # abajo) es la misma convención que usa Bibliography para su propio
    # texto corrido -- ver .fl_prose_open() en shared-helpers.R, usado por
    # los paneles Html de internalConsistency e interRater por el mismo motivo.
    .section = function(title, body_html) {
      paste0('<div style="max-width:7.25in;width:100%;box-sizing:border-box;line-height:1;text-align:justify;">',
             private$.title_html(title), body_html, '</div>')
    },

    .p = function(...) paste0('<p style="margin:0 0 0.7em 0;line-height:1;">', paste0(...), '</p>'),

    .h4 = function(x) paste0('<p style="margin:0.9em 0 0.2em 0;font-weight:700;line-height:1;">', x, '</p>'),

    .run = function() {
      category <- self$options$category

      esc   <- private$.esc
      tbl   <- private$.table_html
      sect  <- private$.section
      p     <- private$.p
      h4    <- private$.h4

      # ── Intro (always shown) ──────────────────────────────────────────────
      intro_html <- sect(
        .("Fiability Library"),
        paste0(
          p(.("Welcome to FiabilityLab. <b>Reliability</b> is the consistency of a measure: an instrument is reliable if it produces similar results under consistent conditions. FiabilityLab covers three main designs — <b>internal consistency</b> (one instrument, one administration), <b>inter-rater agreement</b> (multiple raters, one occasion), and <b>temporal stability</b> (one instrument, repeated occasions; planned).")),
          p(.("Every coefficient below is anchored to the source that introduced or formalized it. Full bibliographic details for every citation live in the Bibliography module."))
        )
      )

      # ── Internal Consistency ───────────────────────────────────────────────
      ic_table <- tbl(
        c(.("Coefficient"), .("What it estimates"),
          .("Typical use"), .("Main limitation")),
        list(
          c("Cronbach's α",
            .("Reliability assuming &tau;-equivalence (equal item loadings)"),
            .("Likert/continuous items; quick default estimate"),
            .("Biased (usually deflated) when loadings are unequal or the scale is multidimensional (Cronbach, 1951)")),
          c(.("Ordinal α"),
            .("Same standardized-α formula, computed on polychoric correlations"),
            .("Ordinal/Likert items with few categories or skewed distributions"),
            .("Not interchangeable with raw α; can differ by .05–.10+ on skewed data (Zumbo et al., 2007)")),
          c(.("McDonald's ω (total)"),
            .("Reliability from actual factor loadings; no equal-loading assumption"),
            .("General-purpose default; robust to unequal loadings"),
            .("Requires a fitted factor model (McDonald, 1999)")),
          c(.("McDonald's ω hierarchical (ωₕ)"),
            .("Variance due to one general factor, net of group-factor variance"),
            .("Multidimensional/bifactor scales"),
            .("Meaningless with only 1 factor fit — collapses onto ω total (McDonald, 1999)")),
          c("GLB",
            .("Tightest model-free lower bound on reliability"),
            .("Sensitivity check against α/ω, no distributional assumptions"),
            .("Upward-biased in small samples relative to item count")),
          c(.("Split-half (Spearman-Brown)"),
            .("Full-test reliability estimated from two-half correlation"),
            .("Quick cross-check for long instruments"),
            .("Highly sensitive to which items land in which half")),
          c("Guttman λ2 / λ6",
            .("Model-free lower bounds (alternative to GLB)"),
            .("Tighter lower bound without fitting a factor model"),
            .("λ6 unstable with near-duplicate items")),
          c("KR-20 / KR-21",
            .("α specialized to dichotomous (0/1) items"),
            .("Knowledge/aptitude tests scored right/wrong"),
            .("KR-21 assumes equal item difficulty — understates reliability when difficulties vary (Kuder &amp; Richardson, 1937)"))
        )
      )

      ic_body <- paste0(
        p(.("<b>What it assesses:</b> Internal consistency evaluates whether the items of a single instrument, administered once, measure the same underlying construct. Under Classical Test Theory, an observed score X = T + E (true score plus error), and reliability is the proportion of observed variance attributable to T (Cronbach, 1951; McDonald, 1999; Nunnally &amp; Bernstein, 1994).")),
        p(.("<b>Where it's used in FiabilityLab:</b> the Internal Consistency module computes every coefficient in the table below.")),
        p(.("<b>How the module's discordance panel is interpreted:</b> McDonald's ω is treated as the general-purpose reference coefficient, since it uses the model's actual factor loadings instead of assuming they are equal; Cronbach's α is reported alongside it, and when the two diverge by more than a small margin, ω is trusted over α — a large gap signals broken &tau;-equivalence. Ordinal α is preferred over raw α when items are ordinal or markedly non-normal (Zumbo et al., 2007). GLB and the Guttman λs serve as model-free sensitivity checks. KR-20/KR-21 apply only to strictly dichotomous items (Kuder &amp; Richardson, 1937).")),
        ic_table,
        h4(.("Cronbach's α")),
        p(.("The average of all possible split-half reliabilities; equivalently, the proportion of total-score variance attributable to a common source among items, under the assumption that every item contributes equally to the true score (Cronbach, 1951).")),
        h4(.("α's Assumptions Check")),
        p(.("α equals the true reliability only when items are tau-equivalent — equal true-score loadings on a single common factor (Cronbach, 1951; Zumbo, Gadermann &amp; Zeisser, 2007). Unlike McDonald's ω, GLB, or the Guttman λ family (none of which require this), α is biased — usually downward — whenever tau-equivalence fails. FiabilityLab's Internal Consistency module tests this formally as a likelihood-ratio comparison between a congeneric single-factor CFA (item loadings free) and a tau-equivalent one (loadings constrained equal): a significant difference rejects tau-equivalence, in which case ω should be reported instead of α. This requires the lavaan package; when it is unavailable, the discordance panel's α-vs-ω gap remains as an indirect signal of the same thing.")),
        p(.("α's standard-error formula also assumes approximate multivariate normality and unidimensionality (a single common factor). FiabilityLab's assumptions check reuses the parallel-analysis factor count for the latter — so the same three checks (tau-equivalence, normality, unidimensionality) that determine whether α is trustworthy are reported together in one panel, the way interRater reports ICC's normality/homoscedasticity/linearity checks together.")),
        p(.("The normality check itself is measurement-level-aware rather than applying Shapiro-Wilk unconditionally: Shapiro-Wilk assumes a genuinely continuous variable, so running it on a discrete Likert item (a handful of whole-number categories) mostly detects the item's own discreteness/ties rather than a real distributional problem — a module whose purpose is checking assumptions should not itself apply a continuous-data test to ordinal data. FiabilityLab therefore runs Shapiro-Wilk only when items are genuinely continuous; for ordinal (Likert-type, ≤7 whole-number categories) items it instead flags items exceeding |skew| &gt; 2 or |kurtosis| &gt; 7, the same floor/ceiling-effect threshold already used elsewhere in this module; and it runs no normality check at all on dichotomous items, since the concept does not apply to a two-point (Bernoulli) variable.")),
        h4(.("Ordinal α (polychoric)")),
        p(.("The same standardized-α formula, applied to the polychoric correlation matrix — which models each ordinal item as a coarsely categorized continuous variable — instead of Pearson correlations (Zumbo, Gadermann &amp; Zeisser, 2007).")),
        h4(.("McDonald's ω (total and hierarchical)")),
        p(.("ω total is the proportion of total-score variance explained by a fitted factor model, computed from the model's actual loadings rather than assuming they are equal (McDonald, 1999). ω hierarchical (ωₕ) is the narrower quantity: the variance attributable specifically to ONE general factor running through all items, net of whatever group-factor (subscale) variance also exists — it requires a genuine bifactor/multi-factor model, and is not a distinct number from ω total when only one factor is fit.")),
        h4("GLB (Greatest Lower Bound)"),
        p(.("The mathematically tightest lower-bound estimate of reliability achievable without any distributional or factor-structure assumption, obtained by constrained optimization over the item covariance matrix. Known to be upward-biased in small samples relative to the number of items.")),
        h4(.("Split-half (Spearman-Brown)")),
        p(.("Correlates two halves of the test, then corrects that correlation upward with the Spearman-Brown prophecy formula to estimate the reliability of the full-length test. Highly sensitive to which items land in which half, which is why FiabilityLab reports the mean across many random splits rather than a single one.")),
        h4(.("Guttman λ2 / λ6")),
        p(.("Model-free lower-bound estimates of reliability, like GLB but computed differently: λ2 uses the covariance structure directly; λ6 uses each item's squared multiple correlation with the rest of the scale. λ6 becomes unstable when an item correlates almost perfectly with the rest of the scale.")),
        h4("KR-20 / KR-21 (Kuder-Richardson)"),
        p(.("KR-20 is Cronbach's α specialized to strictly dichotomous (0/1) items, computed from each item's p&middot;q variance; KR-21 further simplifies KR-20 by assuming all items share the same difficulty (proportion correct) (Kuder &amp; Richardson, 1937). KR-21 understates reliability when item difficulties actually vary."))
      )
      internalConsistency_html <- sect(.("Internal Consistency"), ic_body)

      # ── Inter-Rater Agreement (ahead of interRater's code, per the ─────────
      # Library/Bibliography admission contract) ─────────────────────────────
      irr_table <- tbl(
        c(.("Coefficient"), .("Measurement level"),
          .("Typical use"), .("Main limitation")),
        list(
          c(.("Cohen's Kappa"), .("Nominal, 2 raters"),
            .("Chance-corrected agreement, 2 raters, categorical"),
            .("Paradoxically low with skewed category prevalence, even when raw agreement is high (Cohen, 1960; Gwet, 2014)")),
          c(.("Fleiss' Kappa"), .("Nominal, &gt;2 raters"),
            .("Chance-corrected agreement, more than 2 raters"),
            .("Same prevalence paradox as Cohen's Kappa")),
          c("Gwet's AC1 / AC2", .("Nominal/ordinal, any number of raters"),
            .("Robust alternative to Kappa under high/low prevalence"),
            .("Less familiar/standard in some fields (Gwet, 2014)")),
          c("ICC", .("Continuous/ordinal"),
            .("Rater reliability for numeric scores; separates consistency from absolute agreement"),
            .("Choosing the wrong of the 6 ICC forms changes the conclusion (Shrout &amp; Fleiss, 1979)")),
          c(.("Krippendorff's α"),
            .("Nominal/ordinal/interval/ratio, any raters, handles missing data"),
            .("Universal coefficient across measurement levels"),
            .("Less intuitive interpretation scale than Kappa (Krippendorff, 2018)")),
          c(.("Kendall's W"), .("Ordinal rankings"),
            .("Agreement among rankers on the ordering of objects"),
            .("Only applies to ranking data, not raw ratings"))
        )
      )

      irr_body <- paste0(
        p(.("<b>What it assesses:</b> Inter-rater agreement evaluates whether two or more independent raters/judges, scoring the same cases, reach consistent conclusions — at nominal, ordinal, or continuous measurement levels.")),
        p(.("<b>Where it's used in FiabilityLab:</b> the Inter-Rater Agreement module (Kappa, Gwet's AC1/AC2, ICC, Krippendorff's α, Kendall's W).")),
        p(.("<b>How the module's discordance panel will be interpreted:</b> Cohen's/Fleiss' Kappa is compared against Gwet's AC1 — a large gap between them flags the “kappa paradox” produced by skewed category prevalence, not genuinely poor agreement (Gwet, 2014).")),
        irr_table,
        h4(.("Cohen's / Fleiss' Kappa")),
        p(.("Chance-corrected agreement for categorical (nominal) ratings: Cohen's Kappa for exactly 2 raters (Cohen, 1960), Fleiss' Kappa for more than 2 (Fleiss, 1971). Both can be paradoxically low even when raw agreement is high, whenever one category is much more common than the others — the “kappa paradox” (Gwet, 2014). FiabilityLab reports Kappa values against the Landis &amp; Koch (1977) interpretation bands (Poor/Slight/Fair/Moderate/Substantial/Almost perfect), the field-standard scale for this coefficient.")),
        h4("Gwet's AC1 / AC2"),
        p(.("Proposed as a more stable alternative to Kappa specifically because it does not degrade under extreme category prevalence or rater bias (Gwet, 2014). AC1 is for nominal data, AC2 extends the same logic to ordinal/interval scales.")),
        h4("ICC (Intraclass Correlation)"),
        p(.("For continuous or ordinal ratings. There are six standard forms, differing in whether raters are treated as a fixed or random sample, whether reliability is for a single rating or an average of several, and — the choice with the biggest practical consequence — whether it measures pure consistency (rank agreement) or absolute agreement (identical values) (Shrout &amp; Fleiss, 1979). A rater with a systematic mean bias can score high on consistency-type ICC and low on absolute-agreement-type ICC for the exact same data.")),
        h4(.("ICC's Assumptions")),
        p(.("Because the ICC is a variance-partition of a subject × rater ANOVA (Shrout &amp; Fleiss, 1979), it — unlike Kappa, Gwet's AC1/AC2, Krippendorff's α, or Kendall's W, all of which are rank- or category-based and distribution-free — inherits that model's usual assumptions: normally-distributed residuals, homogeneous error variance across raters (homoscedasticity), and an additive/linear subject × rater structure. FiabilityLab's Inter-Rater Agreement module tests all three automatically whenever ICC is computed: Shapiro-Wilk for normality, Levene's test (Levene, 1960) for homoscedasticity, and Tukey's one-degree-of-freedom test for non-additivity (Tukey, 1949) for linearity — the last of these is the most consequential violation, since a curvilinear rater relationship makes the ICC systematically understate true agreement.")),
        p(.("Sample size compounds all three: small samples (roughly n &lt; 30) both widen the ICC's confidence interval and make its F-based inference more sensitive to non-normality, while the coefficient's other assumptions (homoscedasticity, linearity) are unaffected by n but become harder to detect statistically with few subjects. FiabilityLab reports minimum-sample guidance from Koo &amp; Li (2016) and Bujang &amp; Baharum (2017) alongside the assumption tests. The bootstrap confidence intervals available for Kappa, Gwet, Krippendorff, and Kendall's W above are themselves the practical antidote to small-n imprecision for those coefficients, since none of them relies on ICC's F-distribution assumption in the first place.")),
        h4(.("Krippendorff's α")),
        p(.("A single, unified agreement coefficient that works across nominal, ordinal, interval, and ratio data, with any number of raters, and handles missing data natively (Krippendorff, 2018) — useful when a design mixes measurement levels or has incomplete rater coverage.")),
        h4(.("Kendall's W")),
        p(.("Measures agreement among multiple rankers on the relative ORDER of a set of objects, not on their raw ratings — applicable only when the data are genuinely rankings."))
      )
      interRater_html <- sect(.("Inter-Rater Agreement"), irr_body)

      # ── Advanced Reliability (SEM) ────────────────────────────────────────
      adv_body <- paste0(
        p(.("The classical coefficients above (α, ω, GLB, ...) treat the scale's factor structure as either unknown (a single common source) or only exploratorily detected (parallel analysis). When the structure is already known — from theory or a prior validation — the separate Advanced Reliability (SEM) analysis lets the user assign items to named factors and fits a confirmatory factor model (via lavaan/semTools) instead, giving structure-specific reliability and validity evidence.")),
        h4(.("Exploratory Dimensionality (Parallel Analysis)")),
        p(.("Before trusting a fixed confirmatory structure, parallel analysis (Horn, 1965) asks how many factors the items suggest on their own: it compares the items' actual eigenvalues against the eigenvalues expected from random data of the same size, and suggests a factor count wherever the real eigenvalue still exceeds its random counterpart. This is exploratory and does not know about the structure assigned in the analysis — a mismatch with the specified number of factors is worth investigating, not automatic proof the specified structure is wrong: theory-driven confirmatory structures legitimately group items in ways a purely data-driven exploratory method may not recover.")),
        h4(.("Estimator and Missing Data")),
        p(.("The confirmatory model's estimator depends on item type: continuous items fit via ML by default, or MLR for robustness to non-normality (adjusted standard errors and a scaled χ²); ordinal items (auto-detected as whole-number values with ≤7 categories, or set explicitly) always fit via WLSMV on their polychoric/tetrachoric correlations, which requires complete cases. Under a robust estimator (MLR or WLSMV), the plain χ²/CFI/TLI/RMSEA are not the recommended numbers to interpret — WLSMV's plain versions can even fall outside the [0,1] range — the scaled/robust versions are. For continuous items with missing values plausibly missing-at-random, Full Information Maximum Likelihood (FIML) uses all available information per case instead of discarding any case outright; it has no equivalent for ordinal/WLSMV items.")),
        h4(.("Solution Admissibility")),
        p(.("Convergence only means the optimizer stopped at some solution; it does not mean that solution is usable. FiabilityLab additionally checks lavaan's own post-estimation check for Heywood cases and non-positive-definite matrices, counts negative residual variances and standardized loadings at or beyond 1, reports the largest latent factor correlation (near/at 1 signals two factors may not be empirically distinct), and — for ordinal items — flags item pairs with an empty cell in their contingency table, which makes that pair's polychoric/tetrachoric correlation unstable or inestimable. None of CR, AVE, H, HTMT, or omega hierarchical should be interpreted from a model that fails this check.")),
        h4(.("Model Fit")),
        p(.("Composite Reliability, AVE, and H are only as trustworthy as the confirmatory model they come from. FiabilityLab reports χ², CFI, TLI, RMSEA (with 90% CI), and SRMR for the fitted model, following Hu &amp; Bentler's (1999) cutoffs (CFI/TLI ≥ .95, RMSEA ≤ .06, SRMR ≤ .08 for good fit) — a model with poor fit should be revised (or its structure reconsidered) before its reliability numbers are reported.")),
        h4(.("Composite Reliability (CR) / ω")),
        p(.("The proportion of a factor's composite-score variance attributable to its common factor, computed from the confirmatory model's own standardized loadings — CR (Fornell &amp; Larcker, 1981) and ω (McDonald, 1999) are the same underlying quantity from two different literatures (marketing/PLS vs. psychometrics), so FiabilityLab reports one number under both names. Unlike Cronbach's α, it does not assume equal (tau-equivalent) loadings.")),
        h4("AVE (Average Variance Extracted)"),
        p(.("The average of the squared standardized loadings of a factor's own items — the proportion of variance its indicators share with the factor itself, as opposed to error. AVE ≥ .50 is the field-standard threshold for adequate convergent validity (Fornell &amp; Larcker, 1981); it is a validity index, not a reliability coefficient, and is reported alongside CR/ω by convention.")),
        h4(.("Hancock &amp; Mueller's H")),
        p(.("An alternative construct-reliability coefficient, H = &Sigma;(λ²/(1&minus;λ²)) / [1 + &Sigma;(λ²/(1&minus;λ²))], computed from the same standardized loadings as CR/ω (Hancock &amp; Mueller, 2001). H is monotonically related to each item's own loading in a way CR/ω is not, making it somewhat less sensitive to adding or removing a single weak indicator — reported alongside CR/ω, not as a replacement for it.")),
        h4(.("Reading Standardized Loadings")),
        p(.("FiabilityLab's Standardized Loadings plot flags two different problems, not one: a low-positive loading (below .50, a common though not universal minimum) means the item is a weak indicator of its factor and is a candidate for revision or removal. A <b>negative</b> loading is a different problem entirely — it almost always means the item needs reverse-scoring (its raw values run opposite the rest of the factor, typically because it is worded in the opposite direction from the other items) rather than that it is a bad item; recode it and re-run the analysis before drawing any conclusion about its quality.")),
        h4("HTMT (Heterotrait-Monotrait Ratio)"),
        p(.("A discriminant-validity check between pairs of factors: whether two subscales are empirically distinct rather than measuring the same thing twice. HTMT &gt; .85 signals a discriminant-validity concern — the classical Fornell-Larcker criterion and cross-loading inspection were shown to miss this in common research situations that HTMT reliably detects (Henseler, Ringle &amp; Sarstedt, 2015). Only computed when 2 or more factors are defined.")),
        h4(.("Second-Order Omega Hierarchical")),
        p(.("When a second-order general factor (G) is specified over 3 or more first-order factors (fewer leaves the higher-order layer statistically unidentified), omega hierarchical is generalized to this confirmatory structure: the proportion of the TOTAL scale's variance attributable specifically to G, net of each subscale's own group-factor variance (McDonald, 1999) — the same concept the exploratory Omega Hierarchical checkbox in the classical tab estimates via Schmid-Leiman rotation, computed here instead from a user-specified, confirmatory structure.")),
        p(.("With exactly 3 first-order factors, the second-order model is mathematically equivalent to (has identical fit as) the correlated-factors model above it — a second-order factor over exactly 3 lower factors perfectly reproduces their 3 pairwise correlations without adding any constraint. The two fit rows will show identical χ²/CFI/RMSEA/SRMR in that case; a genuinely testable comparison of whether the general-factor structure fits worse than free correlations among subscales requires 4 or more first-order factors.")),
        h4(.("Model Comparison (Likelihood-Ratio Test)")),
        p(.("With 4 or more first-order factors, the second-order model is a constrained (nested) version of the freely-correlated one, so their χ²/df difference is itself χ²-distributed — a formal test of whether imposing one general factor costs a significant amount of fit. A significant difference means the general-factor structure fits worse than letting subscales correlate freely, and any general-factor score (or the omega hierarchical value above) should be trusted accordingly less; a non-significant difference supports the second-order structure statistically, though not on its own as evidence that it is the theoretically correct one.")),
        h4(.("Reliability Due to G")),
        p(.("When a second-order structure is fit, each factor's Reliability Table row also reports its reliability specifically attributable to G — a different (and typically smaller) quantity than that factor's own CR/ω, since it asks what share of the factor's reliable variance reflects the general trait rather than something specific to the subscale. A factor whose reliability-due-to-G is a small fraction of its own CR/ω is measuring its specific construct well but would lose most of that reliable variance if folded into a single general-factor or total score.")),
        h4(.("Modification Indices — Interpret Through Theory, Not Automatically")),
        p(.("When model fit is not Good, FiabilityLab reports the largest modification indices (MI) — the expected χ² improvement from freeing one additional cross-loading or correlated residual — as candidates, never as automatic edits. Purely data-driven model modification capitalizes on chance features of the specific sample and may not replicate in a new one (MacCallum, Roznowski &amp; Necowitz, 1992): a modification index only becomes a defensible change once it is justified by the instrument's theoretical structure (does an item plausibly reflect two constructs at once? does a correlated residual reflect known shared method variance, such as two reverse-worded items on the same scale?). Removing an item is only one of several possible responses to poor fit, not the default one — respecifying the model, or reconsidering whether the theoretical factor structure itself is correct, are equally valid next steps."))
      )
      advanced_html <- sect(.("Advanced Reliability (SEM)"), adv_body)

      # ── Measurement Invariance ───────────────────────────────────────────────
      inv_body <- paste0(
        p(.("Advanced Reliability (SEM) assumes a single population: its confirmatory model, and every coefficient derived from it, describes how the items function on average across everyone in the data. Before comparing group means, subscale scores, or regression coefficients involving a factor across levels of a grouping variable (sex, country, time point, condition, ...), the separate Measurement Invariance analysis checks whether that same measurement model actually holds across those groups — an observed group difference is only interpretable as a real difference on the construct if the items measure it the same way in each group. This is not just a software design choice: Raykov (2004) develops reliability and invariance evaluation jointly within the same latent-variable framework, and Jak &amp; Jorgensen (2017) formally show that under strong invariance the between-group residual variance vanishes and composite reliability reaches its ceiling (ω = 1) — the flip side being that, when invariance fails, a genuine between-group difference the model cannot attribute to the common factor is forced into that residual/error term instead, so a single pooled Omega/CR computed across non-invariant groups is not interpretable as either group's true reliability.")),
        h4(.("The Invariance Sequence")),
        p(.("Four increasingly restrictive multi-group confirmatory models are fit and compared in sequence, each against the model just before it: <b>Configural</b> — the same items load on the same factors in every group, with everything else free; the minimum requirement for the construct to even be comparable. <b>Metric (weak)</b> — factor loadings are additionally constrained equal across groups; required before comparing correlations or regression coefficients involving the factor across groups. <b>Scalar (strong)</b> — item intercepts (or thresholds, for ordinal items) are additionally constrained equal; required before comparing group means or observed scores — without it, an observed mean difference may reflect item functioning differences rather than a real difference on the construct. <b>Strict</b> — item residual variances are additionally constrained equal; a stronger, less commonly required condition.")),
        h4(.("Testing Each Level (LRT and ΔCFI)")),
        p(.("Since each level is nested within the one before it, their χ²/df difference is itself χ²-distributed — a formal likelihood-ratio test (via lavaan::lavTestLRT()) of whether the added restriction costs a significant amount of fit. But the LRT alone becomes hypersensitive to trivial misfit in large samples, so FiabilityLab also reports the change in CFI (ΔCFI) between consecutive levels and applies the sample-size-robust criterion of Cheung &amp; Rensvold (2002): a restriction is treated as holding if the LRT is non-significant (p ≥ .05) <i>or</i> ΔCFI ≥ -.01 — either criterion passing is enough, since they are two different ways of detecting the same problem and can disagree in either direction depending on sample size. Schmitt &amp; Kuljanin (2008) review how this sequence is actually applied in practice, including how often each level is tested and reported.")),
        h4(.("When a Level Does Not Hold: Partial Invariance")),
        p(.("A failed level does not mean the comparison must be abandoned, nor that full invariance should be forced regardless. The standard next step is to test partial invariance — freeing the constraint for specific items one at a time (via semTools::partialInvariance()/partialInvarianceCat()) to find which items break it, while keeping it for the rest — following a stepwise procedure that avoids testing stronger restrictions once a weaker one has already failed (Vandenberg &amp; Lance, 2000). A construct can still be meaningfully compared across groups under partial invariance, provided enough items remain invariant and the non-invariant ones are excluded from or flagged in that comparison."))
      )
      invariance_html <- sect(.("Measurement Invariance"), inv_body)

      # ── Theoretical Foundations ─────────────────────────────────────────────
      found_table <- tbl(
        c(.("Framework"), .("Core idea"), .("Key sources")),
        list(
          c(.("Classical Test Theory (CTT)"),
            "X = T + E", "Cronbach (1951); McDonald (1999); Nunnally &amp; Bernstein (1994)"),
          c(.("Generalizability Theory"),
            .("Extends CTT to decompose error into multiple simultaneous facets (raters, occasions, items)"),
            "Brennan (2001); Shavelson &amp; Webb (1991)"),
          c(.("Item Response Theory (IRT)"),
            .("Models the probability of a response as a function of person ability and item parameters"),
            "Embretson &amp; Reise (2000); Baker &amp; Kim (2017)")
        )
      )
      found_body <- paste0(
        found_table,
        h4(.("Classical Test Theory")),
        p(.("Models every observed score X as the sum of a true score T and random error E. Reliability, in this framework, is defined as the proportion of observed-score variance attributable to T — every coefficient in the Internal Consistency category above is an estimator of this same quantity, under different assumptions (Cronbach, 1951; McDonald, 1999; Nunnally &amp; Bernstein, 1994).")),
        h4(.("Generalizability Theory")),
        p(.("Extends CTT by decomposing measurement error into several simultaneous sources (facets) — raters, occasions, items, forms — instead of treating all error as one undifferentiated term. A G-study estimates the variance from each facet; a D-study then projects reliability for a specific planned measurement design (Brennan, 2001; Shavelson &amp; Webb, 1991). Not yet implemented as a FiabilityLab module (see the roadmap in ARCHITECTURE.md).")),
        h4(.("Item Response Theory")),
        p(.("Models the probability of a specific item response as a function of the respondent's ability (or trait level) and the item's own parameters (difficulty, discrimination, and, in some models, guessing) — a fundamentally different approach from CTT's total-score-variance decomposition (Embretson &amp; Reise, 2000; Baker &amp; Kim, 2017). Referenced here as a theoretical foundation; not yet implemented as a FiabilityLab module."))
      )
      foundations_html <- sect(.("Theoretical Foundations"), found_body)

      # ── Common Errors ───────────────────────────────────────────────────────
      err_table <- tbl(
        c(.("Error"), .("Why it's wrong"), .("What to do instead")),
        list(
          c(.("Using Cronbach's α on a multidimensional scale"),
            .("α assumes &tau;-equivalence (unidimensionality); with multiple dimensions it can be a misleading estimate"),
            .("Compute α/ω per subscale, or report ω/ωₕ from a fitted multi-factor model (Cronbach, 1951; McDonald, 1999)")),
          c(.("Interpreting Kappa without checking prevalence"),
            .("High raw agreement with low Kappa signals the prevalence paradox, not poor agreement"),
            .("Report Gwet's AC1/AC2 alongside Kappa (Gwet, 2014)")),
          c(.("Confusing reliability with validity"),
            .("A measure can be highly reliable (consistent) yet not valid (not measuring the intended construct)"),
            .("Reliability is necessary but not sufficient for validity (Nunnally &amp; Bernstein, 1994)")),
          c(.("Treating Ordinal α as interchangeable with raw α"),
            .("They are different estimators computed on different correlation matrices"),
            .("State which one was used, and prefer Ordinal α for skewed/ordinal items (Zumbo et al., 2007)"))
        )
      )
      errors_html <- sect(.("Common Errors"), err_table)

      # ── Set content, then hide sections outside the selected category ──────
      self$results$intro$setContent(intro_html)
      self$results$internalConsistency$setContent(internalConsistency_html)
      self$results$interRater$setContent(interRater_html)
      self$results$advanced$setContent(advanced_html)
      self$results$invariance$setContent(invariance_html)
      self$results$foundations$setContent(foundations_html)
      self$results$errors$setContent(errors_html)

      show <- function(name) category == "all" || category == name
      if (!show("internalConsistency")) self$results$internalConsistency$setVisible(FALSE)
      if (!show("interRater"))          self$results$interRater$setVisible(FALSE)
      if (!show("advanced"))            self$results$advanced$setVisible(FALSE)
      if (!show("invariance"))          self$results$invariance$setVisible(FALSE)
      if (!show("foundations"))         self$results$foundations$setVisible(FALSE)
      if (!show("errors"))              self$results$errors$setVisible(FALSE)
    }
  )
)
