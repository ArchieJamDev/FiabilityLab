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
      lang <- .fl_normalize_lang(self$options$reportLang)
      category <- self$options$category
      tr <- function(en, es) if (identical(lang, "es")) es else en

      esc   <- private$.esc
      tbl   <- private$.table_html
      sect  <- private$.section
      p     <- private$.p
      h4    <- private$.h4

      # ── Intro (always shown) ──────────────────────────────────────────────
      intro_html <- sect(
        tr("Fiability Library", "Biblioteca de Confiabilidad"),
        paste0(
          p(tr(
            "Welcome to FiabilityLab. <b>Reliability</b> is the consistency of a measure: an instrument is reliable if it produces similar results under consistent conditions. FiabilityLab covers three main designs — <b>internal consistency</b> (one instrument, one administration), <b>inter-rater agreement</b> (multiple raters, one occasion), and <b>temporal stability</b> (one instrument, repeated occasions; planned).",
            "Bienvenido a FiabilityLab. La <b>confiabilidad</b> es la consistencia de una medida: un instrumento es confiable si produce resultados similares bajo condiciones consistentes. FiabilityLab cubre tres diseños principales — <b>consistencia interna</b> (un instrumento, una administración), <b>acuerdo entre jueces</b> (varios jueces, una ocasión), y <b>estabilidad temporal</b> (un instrumento, ocasiones repetidas; planificado)."
          )),
          p(tr(
            "Every coefficient below is anchored to the source that introduced or formalized it. Full bibliographic details for every citation live in the Bibliography module.",
            "Cada coeficiente de abajo está anclado a la fuente que lo introdujo o formalizó. Los detalles bibliográficos completos de cada cita están en el módulo Bibliography."
          ))
        )
      )

      # ── Internal Consistency ───────────────────────────────────────────────
      ic_table <- tbl(
        c(tr("Coefficient", "Coeficiente"), tr("What it estimates", "Qué estima"),
          tr("Typical use", "Uso típico"), tr("Main limitation", "Limitación principal")),
        list(
          c("Cronbach's &alpha;",
            tr("Reliability assuming &tau;-equivalence (equal item loadings)", "Confiabilidad asumiendo &tau;-equivalencia (cargas de ítem iguales)"),
            tr("Likert/continuous items; quick default estimate", "Ítems Likert/continuos; estimación por defecto rápida"),
            tr("Biased (usually deflated) when loadings are unequal or the scale is multidimensional (Cronbach, 1951)",
               "Sesgada (usualmente subestimada) cuando las cargas son desiguales o la escala es multidimensional (Cronbach, 1951)")),
          c(tr("Ordinal &alpha;", "&alpha; Ordinal"),
            tr("Same standardized-&alpha; formula, computed on polychoric correlations", "Misma fórmula de &alpha; estandarizado, calculada sobre correlaciones policóricas"),
            tr("Ordinal/Likert items with few categories or skewed distributions", "Ítems ordinales/Likert con pocas categorías o distribuciones sesgadas"),
            tr("Not interchangeable with raw &alpha;; can differ by .05–.10+ on skewed data (Zumbo et al., 2007)",
               "No es intercambiable con el &alpha; bruto; puede diferir en .05–.10+ en datos sesgados (Zumbo et al., 2007)")),
          c(tr("McDonald's &omega; (total)", "&omega; de McDonald (total)"),
            tr("Reliability from actual factor loadings; no equal-loading assumption", "Confiabilidad a partir de cargas factoriales reales; sin supuesto de cargas iguales"),
            tr("General-purpose default; robust to unequal loadings", "Estimación por defecto de uso general; robusta a cargas desiguales"),
            tr("Requires a fitted factor model (McDonald, 1999)", "Requiere un modelo factorial ajustado (McDonald, 1999)")),
          c(tr("McDonald's &omega; hierarchical (&omega;ₕ)", "&omega; Jerárquico de McDonald (&omega;ₕ)"),
            tr("Variance due to one general factor, net of group-factor variance", "Varianza debida a un factor general, descontando la varianza de factores de grupo"),
            tr("Multidimensional/bifactor scales", "Escalas multidimensionales/bifactor"),
            tr("Meaningless with only 1 factor fit — collapses onto &omega; total (McDonald, 1999)",
               "Sin sentido con un ajuste de un solo factor — colapsa sobre el &omega; total (McDonald, 1999)")),
          c("GLB",
            tr("Tightest model-free lower bound on reliability", "Cota inferior libre de modelo más ajustada de confiabilidad"),
            tr("Sensitivity check against &alpha;/&omega;, no distributional assumptions", "Verificación de sensibilidad frente a &alpha;/&omega;, sin supuestos distribucionales"),
            tr("Upward-biased in small samples relative to item count", "Sesgado al alza en muestras pequeñas en relación con el número de ítems")),
          c(tr("Split-half (Spearman-Brown)", "Mitades partidas (Spearman-Brown)"),
            tr("Full-test reliability estimated from two-half correlation", "Confiabilidad de la prueba completa estimada de la correlación entre dos mitades"),
            tr("Quick cross-check for long instruments", "Verificación rápida para instrumentos largos"),
            tr("Highly sensitive to which items land in which half", "Muy sensible a qué ítems caen en cada mitad")),
          c("Guttman &lambda;2 / &lambda;6",
            tr("Model-free lower bounds (alternative to GLB)", "Cotas inferiores libres de modelo (alternativa al GLB)"),
            tr("Tighter lower bound without fitting a factor model", "Cota inferior más ajustada sin ajustar un modelo factorial"),
            tr("&lambda;6 unstable with near-duplicate items", "&lambda;6 inestable con ítems casi duplicados")),
          c("KR-20 / KR-21",
            tr("&alpha; specialized to dichotomous (0/1) items", "&alpha; especializado a ítems dicotómicos (0/1)"),
            tr("Knowledge/aptitude tests scored right/wrong", "Pruebas de conocimiento/aptitud calificadas correcto/incorrecto"),
            tr("KR-21 assumes equal item difficulty — understates reliability when difficulties vary (Kuder &amp; Richardson, 1937)",
               "El KR-21 asume igual dificultad de ítem — subestima la confiabilidad cuando las dificultades varían (Kuder &amp; Richardson, 1937)"))
        )
      )

      ic_body <- paste0(
        p(tr(
          "<b>What it assesses:</b> Internal consistency evaluates whether the items of a single instrument, administered once, measure the same underlying construct. Under Classical Test Theory, an observed score X = T + E (true score plus error), and reliability is the proportion of observed variance attributable to T (Cronbach, 1951; McDonald, 1999; Nunnally &amp; Bernstein, 1994).",
          "<b>Qué evalúa:</b> La consistencia interna evalúa si los ítems de un instrumento, administrado una vez, miden el mismo constructo subyacente. Bajo la Teoría Clásica de los Tests, una puntuación observada X = T + E (puntuación verdadera más error), y la confiabilidad es la proporción de varianza observada atribuible a T (Cronbach, 1951; McDonald, 1999; Nunnally &amp; Bernstein, 1994)."
        )),
        p(tr(
          "<b>Where it's used in FiabilityLab:</b> the Internal Consistency module computes every coefficient in the table below.",
          "<b>Dónde se usa en FiabilityLab:</b> el módulo Internal Consistency calcula cada coeficiente de la tabla de abajo."
        )),
        p(tr(
          "<b>How the module's discordance panel is interpreted:</b> McDonald's &omega; is treated as the general-purpose reference coefficient, since it uses the model's actual factor loadings instead of assuming they are equal; Cronbach's &alpha; is reported alongside it, and when the two diverge by more than a small margin, &omega; is trusted over &alpha; — a large gap signals broken &tau;-equivalence. Ordinal &alpha; is preferred over raw &alpha; when items are ordinal or markedly non-normal (Zumbo et al., 2007). GLB and the Guttman &lambda;s serve as model-free sensitivity checks. KR-20/KR-21 apply only to strictly dichotomous items (Kuder &amp; Richardson, 1937).",
          "<b>Cómo se interpreta el panel de discordancia del módulo:</b> el &omega; de McDonald se trata como el coeficiente de referencia de uso general, ya que usa las cargas factoriales reales del modelo en vez de asumir que son iguales; el &alpha; de Cronbach se reporta junto a él, y cuando ambos divergen más de un margen pequeño, se confía en el &omega; sobre el &alpha; — una brecha grande señala una ruptura de la &tau;-equivalencia. El &alpha; Ordinal se prefiere sobre el &alpha; bruto cuando los ítems son ordinales o marcadamente no normales (Zumbo et al., 2007). El GLB y los &lambda; de Guttman sirven como verificaciones de sensibilidad libres de modelo. El KR-20/KR-21 aplican solo a ítems estrictamente dicotómicos (Kuder &amp; Richardson, 1937)."
        )),
        ic_table,
        h4(tr("Cronbach's &alpha;", "&alpha; de Cronbach")),
        p(tr(
          "The average of all possible split-half reliabilities; equivalently, the proportion of total-score variance attributable to a common source among items, under the assumption that every item contributes equally to the true score (Cronbach, 1951).",
          "El promedio de todas las confiabilidades posibles de mitades partidas; equivalentemente, la proporción de la varianza del puntaje total atribuible a una fuente común entre ítems, bajo el supuesto de que cada ítem contribuye igualmente a la puntuación verdadera (Cronbach, 1951)."
        )),
        h4(tr("&alpha;'s Assumptions Check", "Verificación de Supuestos del &alpha;")),
        p(tr(
          "&alpha; equals the true reliability only when items are tau-equivalent — equal true-score loadings on a single common factor (Cronbach, 1951; Zumbo, Gadermann &amp; Zeisser, 2007). Unlike McDonald's &omega;, GLB, or the Guttman &lambda; family (none of which require this), &alpha; is biased — usually downward — whenever tau-equivalence fails. FiabilityLab's Internal Consistency module tests this formally as a likelihood-ratio comparison between a congeneric single-factor CFA (item loadings free) and a tau-equivalent one (loadings constrained equal): a significant difference rejects tau-equivalence, in which case &omega; should be reported instead of &alpha;. This requires the lavaan package; when it is unavailable, the discordance panel's &alpha;-vs-&omega; gap remains as an indirect signal of the same thing.",
          "El &alpha; equivale a la confiabilidad verdadera solo cuando los ítems son tau-equivalentes — iguales cargas de puntaje verdadero sobre un único factor común (Cronbach, 1951; Zumbo, Gadermann &amp; Zeisser, 2007). A diferencia del &omega; de McDonald, el GLB o la familia &lambda; de Guttman (ninguno de los cuales lo requiere), el &alpha; está sesgado — usualmente a la baja — cuando la tau-equivalencia falla. El módulo Internal Consistency de FiabilityLab prueba esto formalmente como una comparación de razón de verosimilitud entre un AFC congenérico de un factor (cargas de ítem libres) y uno tau-equivalente (cargas restringidas a ser iguales): una diferencia significativa rechaza la tau-equivalencia, en cuyo caso debe reportarse el &omega; en vez del &alpha;. Esto requiere el paquete lavaan; cuando no está disponible, la brecha &alpha;-vs-&omega; del panel de discordancia sigue siendo una señal indirecta de lo mismo."
        )),
        p(tr(
          "&alpha;'s standard-error formula also assumes approximate multivariate normality and unidimensionality (a single common factor). FiabilityLab's assumptions check reuses the parallel-analysis factor count for the latter — so the same three checks (tau-equivalence, normality, unidimensionality) that determine whether &alpha; is trustworthy are reported together in one panel, the way interRater reports ICC's normality/homoscedasticity/linearity checks together.",
          "La fórmula del error estándar del &alpha; también asume normalidad multivariada aproximada y unidimensionalidad (un solo factor común). La verificación de supuestos de FiabilityLab reutiliza el conteo de factores del análisis paralelo para esto último — así los mismos tres supuestos (tau-equivalencia, normalidad, unidimensionalidad) que determinan si el &alpha; es confiable se reportan juntos en un panel, de la misma forma en que interRater reporta juntos los supuestos de normalidad/homocedasticidad/linealidad del ICC."
        )),
        p(tr(
          "The normality check itself is measurement-level-aware rather than applying Shapiro-Wilk unconditionally: Shapiro-Wilk assumes a genuinely continuous variable, so running it on a discrete Likert item (a handful of whole-number categories) mostly detects the item's own discreteness/ties rather than a real distributional problem — a module whose purpose is checking assumptions should not itself apply a continuous-data test to ordinal data. FiabilityLab therefore runs Shapiro-Wilk only when items are genuinely continuous; for ordinal (Likert-type, &le;7 whole-number categories) items it instead flags items exceeding |skew| &gt; 2 or |kurtosis| &gt; 7, the same floor/ceiling-effect threshold already used elsewhere in this module; and it runs no normality check at all on dichotomous items, since the concept does not apply to a two-point (Bernoulli) variable.",
          "La propia verificación de normalidad es consciente del nivel de medida en vez de aplicar Shapiro-Wilk sin condición: Shapiro-Wilk asume una variable genuinamente continua, así que aplicarla a un ítem Likert discreto (un puñado de categorías de números enteros) detecta mayormente la propia discreción/empates del ítem en vez de un problema distribucional real — un módulo cuyo propósito es verificar supuestos no debería él mismo aplicar una prueba para datos continuos a datos ordinales. Por ello, FiabilityLab corre Shapiro-Wilk solo cuando los ítems son genuinamente continuos; para ítems ordinales (tipo Likert, &le;7 categorías de números enteros) marca en cambio los ítems que exceden |asimetría| &gt; 2 o |curtosis| &gt; 7, el mismo umbral de efecto techo/piso ya usado en otras partes de este módulo; y no aplica ninguna verificación de normalidad a ítems dicotómicos, ya que el concepto no aplica a una variable de dos puntos (Bernoulli)."
        )),
        h4(tr("Ordinal &alpha; (polychoric)", "&alpha; Ordinal (policórica)")),
        p(tr(
          "The same standardized-&alpha; formula, applied to the polychoric correlation matrix — which models each ordinal item as a coarsely categorized continuous variable — instead of Pearson correlations (Zumbo, Gadermann &amp; Zeisser, 2007).",
          "La misma fórmula del &alpha; estandarizado, aplicada a la matriz de correlación policórica — que modela cada ítem ordinal como una variable continua categorizada groseramente — en vez de correlaciones de Pearson (Zumbo, Gadermann &amp; Zeisser, 2007)."
        )),
        h4(tr("McDonald's &omega; (total and hierarchical)", "&omega; de McDonald (total y jerárquico)")),
        p(tr(
          "&omega; total is the proportion of total-score variance explained by a fitted factor model, computed from the model's actual loadings rather than assuming they are equal (McDonald, 1999). &omega; hierarchical (&omega;ₕ) is the narrower quantity: the variance attributable specifically to ONE general factor running through all items, net of whatever group-factor (subscale) variance also exists — it requires a genuine bifactor/multi-factor model, and is not a distinct number from &omega; total when only one factor is fit.",
          "El &omega; total es la proporción de varianza del puntaje total explicada por un modelo factorial ajustado, calculada a partir de las cargas reales del modelo en vez de asumir que son iguales (McDonald, 1999). El &omega; jerárquico (&omega;ₕ) es la cantidad más estrecha: la varianza atribuible específicamente a UN factor general que atraviesa todos los ítems, descontando la varianza de factores de grupo (subescalas) que también exista — requiere un modelo bifactor/multi-factor genuino, y no es un número distinto del &omega; total cuando solo se ajusta un factor."
        )),
        h4("GLB (Greatest Lower Bound)"),
        p(tr(
          "The mathematically tightest lower-bound estimate of reliability achievable without any distributional or factor-structure assumption, obtained by constrained optimization over the item covariance matrix. Known to be upward-biased in small samples relative to the number of items.",
          "La estimación de límite inferior matemáticamente más ajustada de confiabilidad alcanzable sin ningún supuesto distribucional o de estructura factorial, obtenida mediante optimización restringida sobre la matriz de covarianza de ítems. Se sabe que está sesgada al alza en muestras pequeñas en relación con el número de ítems."
        )),
        h4(tr("Split-half (Spearman-Brown)", "Mitades partidas (Spearman-Brown)")),
        p(tr(
          "Correlates two halves of the test, then corrects that correlation upward with the Spearman-Brown prophecy formula to estimate the reliability of the full-length test. Highly sensitive to which items land in which half, which is why FiabilityLab reports the mean across many random splits rather than a single one.",
          "Correlaciona dos mitades de la prueba, y luego corrige esa correlación al alza con la fórmula de profecía de Spearman-Brown para estimar la confiabilidad de la prueba completa. Muy sensible a qué ítems caen en cada mitad, por lo que FiabilityLab reporta el promedio entre muchas particiones aleatorias en vez de una sola."
        )),
        h4(tr("Guttman &lambda;2 / &lambda;6", "Guttman &lambda;2 / &lambda;6")),
        p(tr(
          "Model-free lower-bound estimates of reliability, like GLB but computed differently: &lambda;2 uses the covariance structure directly; &lambda;6 uses each item's squared multiple correlation with the rest of the scale. &lambda;6 becomes unstable when an item correlates almost perfectly with the rest of the scale.",
          "Estimaciones de límite inferior de confiabilidad libres de modelo, como el GLB pero calculadas de forma distinta: &lambda;2 usa directamente la estructura de covarianza; &lambda;6 usa la correlación múltiple al cuadrado de cada ítem con el resto de la escala. El &lambda;6 se vuelve inestable cuando un ítem correlaciona casi perfectamente con el resto de la escala."
        )),
        h4("KR-20 / KR-21 (Kuder-Richardson)"),
        p(tr(
          "KR-20 is Cronbach's &alpha; specialized to strictly dichotomous (0/1) items, computed from each item's p&middot;q variance; KR-21 further simplifies KR-20 by assuming all items share the same difficulty (proportion correct) (Kuder &amp; Richardson, 1937). KR-21 understates reliability when item difficulties actually vary.",
          "El KR-20 es el &alpha; de Cronbach especializado a ítems estrictamente dicotómicos (0/1), calculado a partir de la varianza p&middot;q de cada ítem; el KR-21 simplifica aún más al KR-20 asumiendo que todos los ítems comparten la misma dificultad (proporción de aciertos) (Kuder &amp; Richardson, 1937). El KR-21 subestima la confiabilidad cuando las dificultades de los ítems en realidad varían."
        ))
      )
      internalConsistency_html <- sect(tr("Internal Consistency", "Consistencia Interna"), ic_body)

      # ── Inter-Rater Agreement (ahead of interRater's code, per the ─────────
      # Library/Bibliography admission contract) ─────────────────────────────
      irr_table <- tbl(
        c(tr("Coefficient", "Coeficiente"), tr("Measurement level", "Nivel de medida"),
          tr("Typical use", "Uso típico"), tr("Main limitation", "Limitación principal")),
        list(
          c(tr("Cohen's Kappa", "Kappa de Cohen"), tr("Nominal, 2 raters", "Nominal, 2 jueces"),
            tr("Chance-corrected agreement, 2 raters, categorical", "Acuerdo corregido por azar, 2 jueces, categórico"),
            tr("Paradoxically low with skewed category prevalence, even when raw agreement is high (Cohen, 1960; Gwet, 2014)",
               "Paradójicamente bajo con prevalencia de categoría sesgada, aun cuando el acuerdo bruto es alto (Cohen, 1960; Gwet, 2014)")),
          c(tr("Fleiss' Kappa", "Kappa de Fleiss"), tr("Nominal, &gt;2 raters", "Nominal, &gt;2 jueces"),
            tr("Chance-corrected agreement, more than 2 raters", "Acuerdo corregido por azar, más de 2 jueces"),
            tr("Same prevalence paradox as Cohen's Kappa", "La misma paradoja de prevalencia que el Kappa de Cohen")),
          c("Gwet's AC1 / AC2", tr("Nominal/ordinal, any number of raters", "Nominal/ordinal, cualquier número de jueces"),
            tr("Robust alternative to Kappa under high/low prevalence", "Alternativa robusta al Kappa bajo prevalencia alta/baja"),
            tr("Less familiar/standard in some fields (Gwet, 2014)", "Menos conocido/estándar en algunos campos (Gwet, 2014)")),
          c("ICC", tr("Continuous/ordinal", "Continuo/ordinal"),
            tr("Rater reliability for numeric scores; separates consistency from absolute agreement", "Confiabilidad entre jueces para puntajes numéricos; separa consistencia de acuerdo absoluto"),
            tr("Choosing the wrong of the 6 ICC forms changes the conclusion (Shrout &amp; Fleiss, 1979)",
               "Elegir la forma incorrecta entre las 6 de ICC cambia la conclusión (Shrout &amp; Fleiss, 1979)")),
          c(tr("Krippendorff's &alpha;", "&alpha; de Krippendorff"),
            tr("Nominal/ordinal/interval/ratio, any raters, handles missing data", "Nominal/ordinal/intervalo/razón, cualquier número de jueces, maneja datos faltantes"),
            tr("Universal coefficient across measurement levels", "Coeficiente universal a través de niveles de medida"),
            tr("Less intuitive interpretation scale than Kappa (Krippendorff, 2018)", "Escala de interpretación menos intuitiva que el Kappa (Krippendorff, 2018)")),
          c(tr("Kendall's W", "W de Kendall"), tr("Ordinal rankings", "Rankings ordinales"),
            tr("Agreement among rankers on the ordering of objects", "Acuerdo entre evaluadores sobre el orden de los objetos"),
            tr("Only applies to ranking data, not raw ratings", "Solo aplica a datos de ranking, no a calificaciones brutas"))
        )
      )

      irr_body <- paste0(
        p(tr(
          "<b>What it assesses:</b> Inter-rater agreement evaluates whether two or more independent raters/judges, scoring the same cases, reach consistent conclusions — at nominal, ordinal, or continuous measurement levels.",
          "<b>Qué evalúa:</b> El acuerdo entre jueces evalúa si dos o más jueces/evaluadores independientes, calificando los mismos casos, llegan a conclusiones consistentes — a nivel de medida nominal, ordinal o continuo."
        )),
        p(tr(
          "<b>Where it's used in FiabilityLab:</b> the Inter-Rater Agreement module (Kappa, Gwet's AC1/AC2, ICC, Krippendorff's &alpha;, Kendall's W).",
          "<b>Dónde se usa en FiabilityLab:</b> el módulo Inter-Rater Agreement (Kappa, AC1/AC2 de Gwet, ICC, &alpha; de Krippendorff, W de Kendall)."
        )),
        p(tr(
          "<b>How the module's discordance panel will be interpreted:</b> Cohen's/Fleiss' Kappa is compared against Gwet's AC1 — a large gap between them flags the “kappa paradox” produced by skewed category prevalence, not genuinely poor agreement (Gwet, 2014).",
          "<b>Cómo se interpretará el panel de discordancia del módulo:</b> el Kappa de Cohen/Fleiss se compara frente al AC1 de Gwet — una brecha grande entre ambos señala la “paradoja del kappa” producida por prevalencia de categoría sesgada, no un acuerdo genuinamente pobre (Gwet, 2014)."
        )),
        irr_table,
        h4(tr("Cohen's / Fleiss' Kappa", "Kappa de Cohen / Fleiss")),
        p(tr(
          "Chance-corrected agreement for categorical (nominal) ratings: Cohen's Kappa for exactly 2 raters (Cohen, 1960), Fleiss' Kappa for more than 2 (Fleiss, 1971). Both can be paradoxically low even when raw agreement is high, whenever one category is much more common than the others — the “kappa paradox” (Gwet, 2014). FiabilityLab reports Kappa values against the Landis &amp; Koch (1977) interpretation bands (Poor/Slight/Fair/Moderate/Substantial/Almost perfect), the field-standard scale for this coefficient.",
          "Acuerdo corregido por azar para calificaciones categóricas (nominales): el Kappa de Cohen para exactamente 2 jueces (Cohen, 1960), el Kappa de Fleiss para más de 2 (Fleiss, 1971). Ambos pueden ser paradójicamente bajos aun con acuerdo bruto alto, cuando una categoría es mucho más común que las demás — la “paradoja del kappa” (Gwet, 2014). FiabilityLab reporta los valores de Kappa frente a las bandas de interpretación de Landis &amp; Koch (1977) (Pobre/Leve/Aceptable/Moderado/Sustancial/Casi perfecto), la escala estándar del campo para este coeficiente."
        )),
        h4("Gwet's AC1 / AC2"),
        p(tr(
          "Proposed as a more stable alternative to Kappa specifically because it does not degrade under extreme category prevalence or rater bias (Gwet, 2014). AC1 is for nominal data, AC2 extends the same logic to ordinal/interval scales.",
          "Propuesto como una alternativa más estable al Kappa específicamente porque no se degrada ante prevalencia de categoría extrema o sesgo del juez (Gwet, 2014). El AC1 es para datos nominales, el AC2 extiende la misma lógica a escalas ordinales/de intervalo."
        )),
        h4("ICC (Intraclass Correlation)"),
        p(tr(
          "For continuous or ordinal ratings. There are six standard forms, differing in whether raters are treated as a fixed or random sample, whether reliability is for a single rating or an average of several, and — the choice with the biggest practical consequence — whether it measures pure consistency (rank agreement) or absolute agreement (identical values) (Shrout &amp; Fleiss, 1979). A rater with a systematic mean bias can score high on consistency-type ICC and low on absolute-agreement-type ICC for the exact same data.",
          "Para calificaciones continuas u ordinales. Existen seis formas estándar, que difieren en si los jueces se tratan como una muestra fija o aleatoria, si la confiabilidad es para una calificación única o un promedio de varias, y — la elección con mayor consecuencia práctica — si mide consistencia pura (acuerdo de orden) o acuerdo absoluto (valores idénticos) (Shrout &amp; Fleiss, 1979). Un juez con un sesgo sistemático de media puede puntuar alto en el ICC tipo consistencia y bajo en el tipo acuerdo absoluto para los mismos datos exactos."
        )),
        h4(tr("ICC's Assumptions", "Supuestos del ICC")),
        p(tr(
          "Because the ICC is a variance-partition of a subject &times; rater ANOVA (Shrout &amp; Fleiss, 1979), it — unlike Kappa, Gwet's AC1/AC2, Krippendorff's &alpha;, or Kendall's W, all of which are rank- or category-based and distribution-free — inherits that model's usual assumptions: normally-distributed residuals, homogeneous error variance across raters (homoscedasticity), and an additive/linear subject &times; rater structure. FiabilityLab's Inter-Rater Agreement module tests all three automatically whenever ICC is computed: Shapiro-Wilk for normality, Levene's test (Levene, 1960) for homoscedasticity, and Tukey's one-degree-of-freedom test for non-additivity (Tukey, 1949) for linearity — the last of these is the most consequential violation, since a curvilinear rater relationship makes the ICC systematically understate true agreement.",
          "Como el ICC es una partición de varianza de un ANOVA sujeto &times; juez (Shrout &amp; Fleiss, 1979), a diferencia del Kappa, el AC1/AC2 de Gwet, el &alpha; de Krippendorff o la W de Kendall — todos de rango o categoría y libres de distribución — hereda los supuestos usuales de ese modelo: residuos normalmente distribuidos, varianza de error homogénea entre jueces (homocedasticidad), y una estructura sujeto &times; juez aditiva/lineal. El módulo Inter-Rater Agreement de FiabilityLab prueba los tres automáticamente siempre que se calcula el ICC: Shapiro-Wilk para normalidad, la prueba de Levene (Levene, 1960) para homocedasticidad, y la prueba de un grado de libertad de Tukey para no aditividad (Tukey, 1949) para linealidad — esta última es la violación más consecuente, ya que una relación curvilínea entre jueces hace que el ICC subestime sistemáticamente el acuerdo real."
        )),
        p(tr(
          "Sample size compounds all three: small samples (roughly n &lt; 30) both widen the ICC's confidence interval and make its F-based inference more sensitive to non-normality, while the coefficient's other assumptions (homoscedasticity, linearity) are unaffected by n but become harder to detect statistically with few subjects. FiabilityLab reports minimum-sample guidance from Koo &amp; Li (2016) and Bujang &amp; Baharum (2017) alongside the assumption tests. The bootstrap confidence intervals available for Kappa, Gwet, Krippendorff, and Kendall's W above are themselves the practical antidote to small-n imprecision for those coefficients, since none of them relies on ICC's F-distribution assumption in the first place.",
          "El tamaño de muestra agrava los tres supuestos: muestras pequeñas (aproximadamente n &lt; 30) tanto amplían el intervalo de confianza del ICC como hacen su inferencia basada en F más sensible a la no normalidad, mientras que los otros supuestos del coeficiente (homocedasticidad, linealidad) no dependen de n pero son más difíciles de detectar estadísticamente con pocos sujetos. FiabilityLab reporta orientación de tamaño mínimo de muestra de Koo &amp; Li (2016) y Bujang &amp; Baharum (2017) junto a las pruebas de supuestos. Los intervalos de confianza por bootstrap disponibles para Kappa, Gwet, Krippendorff y la W de Kendall arriba son en sí mismos el antídoto práctico ante la imprecisión por n pequeño para esos coeficientes, ya que ninguno depende del supuesto de distribución F del ICC."
        )),
        h4(tr("Krippendorff's &alpha;", "&alpha; de Krippendorff")),
        p(tr(
          "A single, unified agreement coefficient that works across nominal, ordinal, interval, and ratio data, with any number of raters, and handles missing data natively (Krippendorff, 2018) — useful when a design mixes measurement levels or has incomplete rater coverage.",
          "Un coeficiente de acuerdo único y unificado que funciona con datos nominales, ordinales, de intervalo y de razón, con cualquier número de jueces, y maneja datos faltantes de forma nativa (Krippendorff, 2018) — útil cuando un diseño mezcla niveles de medida o tiene cobertura incompleta de jueces."
        )),
        h4(tr("Kendall's W", "W de Kendall")),
        p(tr(
          "Measures agreement among multiple rankers on the relative ORDER of a set of objects, not on their raw ratings — applicable only when the data are genuinely rankings.",
          "Mide el acuerdo entre varios evaluadores sobre el ORDEN relativo de un conjunto de objetos, no sobre sus calificaciones brutas — aplicable solo cuando los datos son genuinamente rankings."
        ))
      )
      interRater_html <- sect(tr("Inter-Rater Agreement", "Acuerdo entre Jueces"), irr_body)

      # ── Advanced Reliability (SEM) ────────────────────────────────────────
      adv_body <- paste0(
        p(tr(
          "The classical coefficients above (&alpha;, &omega;, GLB, ...) treat the scale's factor structure as either unknown (a single common source) or only exploratorily detected (parallel analysis). When the structure is already known — from theory or a prior validation — the separate Advanced Reliability (SEM) analysis lets the user assign items to named factors and fits a confirmatory factor model (via lavaan/semTools) instead, giving structure-specific reliability and validity evidence.",
          "Los coeficientes clásicos de arriba (&alpha;, &omega;, GLB, ...) tratan la estructura factorial de la escala como desconocida (una sola fuente común) o solo detectada exploratoriamente (análisis paralelo). Cuando la estructura ya se conoce — por teoría o una validación previa — el análisis separado Confiabilidad Avanzada (SEM) permite asignar ítems a factores nombrados y ajusta en su lugar un modelo factorial confirmatorio (vía lavaan/semTools), dando evidencia de confiabilidad y validez específica a esa estructura."
        )),
        h4(tr("Exploratory Dimensionality (Parallel Analysis)", "Dimensionalidad Exploratoria (Análisis Paralelo)")),
        p(tr(
          "Before trusting a fixed confirmatory structure, parallel analysis (Horn, 1965) asks how many factors the items suggest on their own: it compares the items' actual eigenvalues against the eigenvalues expected from random data of the same size, and suggests a factor count wherever the real eigenvalue still exceeds its random counterpart. This is exploratory and does not know about the structure assigned in the analysis — a mismatch with the specified number of factors is worth investigating, not automatic proof the specified structure is wrong: theory-driven confirmatory structures legitimately group items in ways a purely data-driven exploratory method may not recover.",
          "Antes de confiar en una estructura confirmatoria fija, el análisis paralelo (Horn, 1965) pregunta cuántos factores sugieren los ítems por sí solos: compara los eigenvalores reales de los ítems contra los eigenvalores esperados de datos aleatorios del mismo tamaño, y sugiere un número de factores donde el eigenvalor real todavía supera a su contraparte aleatoria. Esto es exploratorio y no conoce la estructura asignada en el análisis — una discrepancia con el número especificado de factores vale la pena investigarla, no es prueba automática de que la estructura especificada esté mal: las estructuras confirmatorias guiadas por teoría legítimamente agrupan ítems de formas que un método exploratorio puramente guiado por datos puede no recuperar."
        )),
        h4(tr("Estimator and Missing Data", "Estimador y Datos Faltantes")),
        p(tr(
          "The confirmatory model's estimator depends on item type: continuous items fit via ML by default, or MLR for robustness to non-normality (adjusted standard errors and a scaled &chi;&sup2;); ordinal items (auto-detected as whole-number values with &le;7 categories, or set explicitly) always fit via WLSMV on their polychoric/tetrachoric correlations, which requires complete cases. Under a robust estimator (MLR or WLSMV), the plain &chi;&sup2;/CFI/TLI/RMSEA are not the recommended numbers to interpret — WLSMV's plain versions can even fall outside the [0,1] range — the scaled/robust versions are. For continuous items with missing values plausibly missing-at-random, Full Information Maximum Likelihood (FIML) uses all available information per case instead of discarding any case outright; it has no equivalent for ordinal/WLSMV items.",
          "El estimador del modelo confirmatorio depende del tipo de ítem: los ítems continuos se ajustan por defecto con ML, o con MLR para robustez a la no normalidad (errores estándar ajustados y un &chi;&sup2; escalado); los ítems ordinales (auto-detectados como valores enteros con &le;7 categorías, o fijados explícitamente) siempre se ajustan con WLSMV sobre sus correlaciones policóricas/tetracóricas, lo cual requiere casos completos. Bajo un estimador robusto (MLR o WLSMV), el &chi;&sup2;/CFI/TLI/RMSEA simples no son los números recomendados para interpretar — las versiones simples de WLSMV pueden incluso caer fuera del rango [0,1] — las versiones escaladas/robustas sí lo son. Para ítems continuos con valores faltantes plausiblemente al azar, la Máxima Verosimilitud de Información Completa (FIML) usa toda la información disponible por caso en vez de descartar cualquier caso por completo; no tiene equivalente para ítems ordinales/WLSMV."
        )),
        h4(tr("Solution Admissibility", "Admisibilidad de la Solución")),
        p(tr(
          "Convergence only means the optimizer stopped at some solution; it does not mean that solution is usable. FiabilityLab additionally checks lavaan's own post-estimation check for Heywood cases and non-positive-definite matrices, counts negative residual variances and standardized loadings at or beyond 1, reports the largest latent factor correlation (near/at 1 signals two factors may not be empirically distinct), and — for ordinal items — flags item pairs with an empty cell in their contingency table, which makes that pair's polychoric/tetrachoric correlation unstable or inestimable. None of CR, AVE, H, HTMT, or omega hierarchical should be interpreted from a model that fails this check.",
          "La convergencia solo significa que el optimizador se detuvo en alguna solución; no significa que esa solución sea utilizable. FiabilityLab adicionalmente verifica la propia comprobación posterior a la estimación de lavaan en busca de casos Heywood y matrices no definidas positivas, cuenta varianzas residuales negativas y cargas estandarizadas iguales o mayores a 1, reporta la mayor correlación de factor latente (cercana o igual a 1 señala que dos factores podrían no ser empíricamente distintos), y — para ítems ordinales — marca pares de ítems con una celda vacía en su tabla de contingencia, lo cual hace que la correlación policórica/tetracórica de ese par sea inestable o inestimable. Ninguno del CR, AVE, H, HTMT, u omega jerárquico debería interpretarse a partir de un modelo que falla esta verificación."
        )),
        h4(tr("Model Fit", "Ajuste del Modelo")),
        p(tr(
          "Composite Reliability, AVE, and H are only as trustworthy as the confirmatory model they come from. FiabilityLab reports &chi;&sup2;, CFI, TLI, RMSEA (with 90% CI), and SRMR for the fitted model, following Hu &amp; Bentler's (1999) cutoffs (CFI/TLI &ge; .95, RMSEA &le; .06, SRMR &le; .08 for good fit) — a model with poor fit should be revised (or its structure reconsidered) before its reliability numbers are reported.",
          "La Confiabilidad Compuesta, el AVE y el H son tan confiables como el modelo confirmatorio del que provienen. FiabilityLab reporta &chi;&sup2;, CFI, TLI, RMSEA (con IC 95%) y SRMR del modelo ajustado, siguiendo los criterios de Hu &amp; Bentler (1999) (CFI/TLI &ge; .95, RMSEA &le; .06, SRMR &le; .08 para buen ajuste) — un modelo con ajuste pobre debería revisarse (o reconsiderar su estructura) antes de reportar sus cifras de confiabilidad."
        )),
        h4(tr("Composite Reliability (CR) / &omega;", "Confiabilidad Compuesta (CR) / &omega;")),
        p(tr(
          "The proportion of a factor's composite-score variance attributable to its common factor, computed from the confirmatory model's own standardized loadings — CR (Fornell &amp; Larcker, 1981) and &omega; (McDonald, 1999) are the same underlying quantity from two different literatures (marketing/PLS vs. psychometrics), so FiabilityLab reports one number under both names. Unlike Cronbach's &alpha;, it does not assume equal (tau-equivalent) loadings.",
          "La proporción de la varianza del puntaje compuesto de un factor atribuible a su factor común, calculada a partir de las cargas estandarizadas propias del modelo confirmatorio — el CR (Fornell &amp; Larcker, 1981) y el &omega; (McDonald, 1999) son la misma cantidad subyacente proveniente de dos literaturas distintas (marketing/PLS vs. psicometría), así que FiabilityLab reporta un solo número bajo ambos nombres. A diferencia del Alfa de Cronbach, no asume cargas iguales (tau-equivalencia)."
        )),
        h4("AVE (Average Variance Extracted)"),
        p(tr(
          "The average of the squared standardized loadings of a factor's own items — the proportion of variance its indicators share with the factor itself, as opposed to error. AVE &ge; .50 is the field-standard threshold for adequate convergent validity (Fornell &amp; Larcker, 1981); it is a validity index, not a reliability coefficient, and is reported alongside CR/&omega; by convention.",
          "El promedio de las cargas estandarizadas al cuadrado de los propios ítems de un factor — la proporción de varianza que sus indicadores comparten con el factor mismo, en contraste con el error. AVE &ge; .50 es el umbral estándar del campo para validez convergente adecuada (Fornell &amp; Larcker, 1981); es un índice de validez, no un coeficiente de confiabilidad, y se reporta junto al CR/&omega; por convención."
        )),
        h4(tr("Hancock &amp; Mueller's H", "H de Hancock &amp; Mueller")),
        p(tr(
          "An alternative construct-reliability coefficient, H = &Sigma;(&lambda;&sup2;/(1&minus;&lambda;&sup2;)) / [1 + &Sigma;(&lambda;&sup2;/(1&minus;&lambda;&sup2;))], computed from the same standardized loadings as CR/&omega; (Hancock &amp; Mueller, 2001). H is monotonically related to each item's own loading in a way CR/&omega; is not, making it somewhat less sensitive to adding or removing a single weak indicator — reported alongside CR/&omega;, not as a replacement for it.",
          "Un coeficiente de confiabilidad de constructo alternativo, H = &Sigma;(&lambda;&sup2;/(1&minus;&lambda;&sup2;)) / [1 + &Sigma;(&lambda;&sup2;/(1&minus;&lambda;&sup2;))], calculado a partir de las mismas cargas estandarizadas que el CR/&omega; (Hancock &amp; Mueller, 2001). El H se relaciona monótonamente con la carga de cada ítem de una forma en que el CR/&omega; no lo hace, haciéndolo algo menos sensible a agregar o quitar un único indicador débil — se reporta junto al CR/&omega;, no como su reemplazo."
        )),
        h4(tr("Reading Standardized Loadings", "Interpretando las Cargas Estandarizadas")),
        p(tr(
          "FiabilityLab's Standardized Loadings plot flags two different problems, not one: a low-positive loading (below .50, a common though not universal minimum) means the item is a weak indicator of its factor and is a candidate for revision or removal. A <b>negative</b> loading is a different problem entirely — it almost always means the item needs reverse-scoring (its raw values run opposite the rest of the factor, typically because it is worded in the opposite direction from the other items) rather than that it is a bad item; recode it and re-run the analysis before drawing any conclusion about its quality.",
          "El gráfico de Cargas Estandarizadas de FiabilityLab marca dos problemas distintos, no uno: una carga positiva baja (bajo .50, un mínimo común aunque no universal) significa que el ítem es un indicador débil de su factor y es candidato a revisión o eliminación. Una carga <b>negativa</b> es un problema completamente distinto — casi siempre significa que el ítem necesita recodificación inversa (sus valores brutos corren en sentido opuesto al resto del factor, típicamente porque está redactado en la dirección opuesta a los demás ítems) y no que sea un mal ítem; recodifíquelo y vuelva a correr el análisis antes de sacar cualquier conclusión sobre su calidad."
        )),
        h4("HTMT (Heterotrait-Monotrait Ratio)"),
        p(tr(
          "A discriminant-validity check between pairs of factors: whether two subscales are empirically distinct rather than measuring the same thing twice. HTMT &gt; .85 signals a discriminant-validity concern — the classical Fornell-Larcker criterion and cross-loading inspection were shown to miss this in common research situations that HTMT reliably detects (Henseler, Ringle &amp; Sarstedt, 2015). Only computed when 2 or more factors are defined.",
          "Una verificación de validez discriminante entre pares de factores: si dos subescalas son empíricamente distintas en vez de medir dos veces lo mismo. HTMT &gt; .85 señala una preocupación de validez discriminante — se demostró que el criterio clásico de Fornell-Larcker y la inspección de cargas cruzadas no detectan esto en situaciones de investigación comunes que el HTMT sí detecta de forma confiable (Henseler, Ringle &amp; Sarstedt, 2015). Solo se calcula cuando se definen 2 o más factores."
        )),
        h4(tr("Second-Order Omega Hierarchical", "Omega Jerárquico de Segundo Orden")),
        p(tr(
          "When a second-order general factor (G) is specified over 3 or more first-order factors (fewer leaves the higher-order layer statistically unidentified), omega hierarchical is generalized to this confirmatory structure: the proportion of the TOTAL scale's variance attributable specifically to G, net of each subscale's own group-factor variance (McDonald, 1999) — the same concept the exploratory Omega Hierarchical checkbox in the classical tab estimates via Schmid-Leiman rotation, computed here instead from a user-specified, confirmatory structure.",
          "Cuando se especifica un factor general de segundo orden (G) sobre 3 o más factores de primer orden (menos deja la capa de orden superior estadísticamente no identificada), el omega jerárquico se generaliza a esta estructura confirmatoria: la proporción de la varianza de la escala TOTAL atribuible específicamente a G, descontando la varianza de factor de grupo propia de cada subescala (McDonald, 1999) — el mismo concepto que el checkbox exploratorio Omega Jerárquico de la pestaña clásica estima vía rotación Schmid-Leiman, calculado aquí en cambio a partir de una estructura confirmatoria especificada por el usuario."
        )),
        p(tr(
          "With exactly 3 first-order factors, the second-order model is mathematically equivalent to (has identical fit as) the correlated-factors model above it — a second-order factor over exactly 3 lower factors perfectly reproduces their 3 pairwise correlations without adding any constraint. The two fit rows will show identical &chi;&sup2;/CFI/RMSEA/SRMR in that case; a genuinely testable comparison of whether the general-factor structure fits worse than free correlations among subscales requires 4 or more first-order factors.",
          "Con exactamente 3 factores de primer orden, el modelo de segundo orden es matemáticamente equivalente (tiene el mismo ajuste) al modelo de factores correlacionados de arriba — un factor de segundo orden sobre exactamente 3 factores inferiores reproduce perfectamente sus 3 correlaciones por pares sin agregar ninguna restricción. Las dos filas de ajuste mostrarán &chi;&sup2;/CFI/RMSEA/SRMR idénticos en ese caso; una comparación genuinamente comprobable de si la estructura de factor general ajusta peor que correlaciones libres entre subescalas requiere 4 o más factores de primer orden."
        )),
        h4(tr("Model Comparison (Likelihood-Ratio Test)", "Comparación de Modelos (Prueba de Razón de Verosimilitud)")),
        p(tr(
          "With 4 or more first-order factors, the second-order model is a constrained (nested) version of the freely-correlated one, so their &chi;&sup2;/df difference is itself &chi;&sup2;-distributed — a formal test of whether imposing one general factor costs a significant amount of fit. A significant difference means the general-factor structure fits worse than letting subscales correlate freely, and any general-factor score (or the omega hierarchical value above) should be trusted accordingly less; a non-significant difference supports the second-order structure statistically, though not on its own as evidence that it is the theoretically correct one.",
          "Con 4 o más factores de primer orden, el modelo de segundo orden es una versión restringida (anidada) del libremente correlacionado, así que su diferencia de &chi;&sup2;/gl es en sí misma distribuida como &chi;&sup2; — una prueba formal de si imponer un factor general cuesta una cantidad significativa de ajuste. Una diferencia significativa significa que la estructura de factor general ajusta peor que dejar correlacionar libremente a las subescalas, y cualquier puntaje de factor general (o el valor de omega jerárquico de arriba) debería confiarse en consecuencia menos; una diferencia no significativa respalda estadísticamente la estructura de segundo orden, aunque no por sí sola como evidencia de que sea la teóricamente correcta."
        )),
        h4(tr("Reliability Due to G", "Confiabilidad Debida a G")),
        p(tr(
          "When a second-order structure is fit, each factor's Reliability Table row also reports its reliability specifically attributable to G — a different (and typically smaller) quantity than that factor's own CR/&omega;, since it asks what share of the factor's reliable variance reflects the general trait rather than something specific to the subscale. A factor whose reliability-due-to-G is a small fraction of its own CR/&omega; is measuring its specific construct well but would lose most of that reliable variance if folded into a single general-factor or total score.",
          "Cuando se ajusta una estructura de segundo orden, la fila de cada factor en la Tabla de Confiabilidad también reporta su confiabilidad atribuible específicamente a G — una cantidad distinta (y típicamente menor) que el propio CR/&omega; de ese factor, ya que pregunta qué parte de la varianza confiable del factor refleja el rasgo general en vez de algo específico de la subescala. Un factor cuya confiabilidad-debida-a-G es una fracción pequeña de su propio CR/&omega; está midiendo bien su constructo específico pero perdería la mayor parte de esa varianza confiable si se incorpora a un único puntaje de factor general o total."
        )),
        h4(tr("Modification Indices — Interpret Through Theory, Not Automatically", "Índices de Modificación — Interpretar Mediante Teoría, No Automáticamente")),
        p(tr(
          "When model fit is not Good, FiabilityLab reports the largest modification indices (MI) — the expected &chi;&sup2; improvement from freeing one additional cross-loading or correlated residual — as candidates, never as automatic edits. Purely data-driven model modification capitalizes on chance features of the specific sample and may not replicate in a new one (MacCallum, Roznowski &amp; Necowitz, 1992): a modification index only becomes a defensible change once it is justified by the instrument's theoretical structure (does an item plausibly reflect two constructs at once? does a correlated residual reflect known shared method variance, such as two reverse-worded items on the same scale?). Removing an item is only one of several possible responses to poor fit, not the default one — respecifying the model, or reconsidering whether the theoretical factor structure itself is correct, are equally valid next steps.",
          "Cuando el ajuste del modelo no es Bueno, FiabilityLab reporta los mayores índices de modificación (MI) — la mejora esperada en &chi;&sup2; al liberar una carga cruzada o un residuo correlacionado adicional — como candidatos, nunca como ediciones automáticas. La modificación del modelo guiada puramente por los datos capitaliza sobre características azarosas de la muestra específica y puede no replicarse en una nueva (MacCallum, Roznowski &amp; Necowitz, 1992): un índice de modificación solo se convierte en un cambio defendible una vez que se justifica por la estructura teórica del instrumento (¿un ítem refleja de forma plausible dos constructos a la vez? ¿un residuo correlacionado refleja varianza de método compartida conocida, como dos ítems redactados en reversa en la misma escala?). Eliminar un ítem es solo una de varias respuestas posibles a un ajuste pobre, no la predeterminada — reespecificar el modelo, o reconsiderar si la propia estructura factorial teórica es correcta, son pasos siguientes igualmente válidos."
        ))
      )
      advanced_html <- sect(tr("Advanced Reliability (SEM)", "Confiabilidad Avanzada (SEM)"), adv_body)

      # ── Measurement Invariance ───────────────────────────────────────────────
      inv_body <- paste0(
        p(tr(
          "Advanced Reliability (SEM) assumes a single population: its confirmatory model, and every coefficient derived from it, describes how the items function on average across everyone in the data. Before comparing group means, subscale scores, or regression coefficients involving a factor across levels of a grouping variable (sex, country, time point, condition, ...), the separate Measurement Invariance analysis checks whether that same measurement model actually holds across those groups — an observed group difference is only interpretable as a real difference on the construct if the items measure it the same way in each group. This is not just a software design choice: Raykov (2004) develops reliability and invariance evaluation jointly within the same latent-variable framework, and Jak &amp; Jorgensen (2017) formally show that under strong invariance the between-group residual variance vanishes and composite reliability reaches its ceiling (&omega; = 1) — the flip side being that, when invariance fails, a genuine between-group difference the model cannot attribute to the common factor is forced into that residual/error term instead, so a single pooled Omega/CR computed across non-invariant groups is not interpretable as either group's true reliability.",
          "Confiabilidad Avanzada (SEM) asume una sola población: su modelo confirmatorio, y cada coeficiente derivado de él, describe cómo funcionan los ítems en promedio a través de todos en los datos. Antes de comparar medias de grupo, puntajes de subescala, o coeficientes de regresión que involucren a un factor entre niveles de una variable de agrupación (sexo, país, momento temporal, condición, ...), el análisis separado Invariancia de Medida verifica si ese mismo modelo de medición realmente se sostiene entre esos grupos — una diferencia de grupo observada solo es interpretable como una diferencia real en el constructo si los ítems lo miden de la misma forma en cada grupo. Esto no es solo una elección de diseño de software: Raykov (2004) desarrolla la evaluación de confiabilidad e invariancia conjuntamente dentro del mismo marco de variables latentes, y Jak y Jorgensen (2017) muestran formalmente que bajo invariancia fuerte la varianza residual entre grupos se anula y la confiabilidad compuesta alcanza su tope (&omega; = 1) — la otra cara siendo que, cuando la invariancia falla, una diferencia real entre grupos que el modelo no puede atribuir al factor común queda forzada hacia ese término residual/de error en su lugar, así que un único Omega/CR agrupado calculado entre grupos no invariantes no es interpretable como la confiabilidad verdadera de ninguno de los dos grupos."
        )),
        h4(tr("The Invariance Sequence", "La Secuencia de Invariancia")),
        p(tr(
          "Four increasingly restrictive multi-group confirmatory models are fit and compared in sequence, each against the model just before it: <b>Configural</b> — the same items load on the same factors in every group, with everything else free; the minimum requirement for the construct to even be comparable. <b>Metric (weak)</b> — factor loadings are additionally constrained equal across groups; required before comparing correlations or regression coefficients involving the factor across groups. <b>Scalar (strong)</b> — item intercepts (or thresholds, for ordinal items) are additionally constrained equal; required before comparing group means or observed scores — without it, an observed mean difference may reflect item functioning differences rather than a real difference on the construct. <b>Strict</b> — item residual variances are additionally constrained equal; a stronger, less commonly required condition.",
          "Se ajustan y comparan en secuencia cuatro modelos confirmatorios multigrupo cada vez más restrictivos, cada uno contra el modelo justo anterior: <b>Configural</b> — los mismos ítems cargan sobre los mismos factores en cada grupo, con todo lo demás libre; el requisito mínimo para que el constructo sea siquiera comparable. <b>Métrica (débil)</b> — las cargas factoriales se restringen adicionalmente a ser iguales entre grupos; requerida antes de comparar correlaciones o coeficientes de regresión que involucren al factor entre grupos. <b>Escalar (fuerte)</b> — los interceptos de los ítems (o umbrales, para ítems ordinales) se restringen adicionalmente a ser iguales; requerida antes de comparar medias de grupo o puntajes observados — sin ella, una diferencia de medias observada puede reflejar diferencias en el funcionamiento de los ítems en vez de una diferencia real en el constructo. <b>Estricta</b> — las varianzas residuales de los ítems se restringen adicionalmente a ser iguales; una condición más fuerte, requerida con menos frecuencia."
        )),
        h4(tr("Testing Each Level (LRT and ΔCFI)", "Probando Cada Nivel (LRT y ΔCFI)")),
        p(tr(
          "Since each level is nested within the one before it, their &chi;&sup2;/df difference is itself &chi;&sup2;-distributed — a formal likelihood-ratio test (via lavaan::lavTestLRT()) of whether the added restriction costs a significant amount of fit. But the LRT alone becomes hypersensitive to trivial misfit in large samples, so FiabilityLab also reports the change in CFI (&Delta;CFI) between consecutive levels and applies the sample-size-robust criterion of Cheung &amp; Rensvold (2002): a restriction is treated as holding if the LRT is non-significant (p &ge; .05) <i>or</i> &Delta;CFI &ge; -.01 — either criterion passing is enough, since they are two different ways of detecting the same problem and can disagree in either direction depending on sample size. Schmitt &amp; Kuljanin (2008) review how this sequence is actually applied in practice, including how often each level is tested and reported.",
          "Como cada nivel está anidado dentro del anterior, su diferencia de &chi;&sup2;/gl es en sí misma distribuida como &chi;&sup2; — una prueba formal de razón de verosimilitud (vía lavaan::lavTestLRT()) de si la restricción agregada cuesta una cantidad significativa de ajuste. Pero el LRT por sí solo se vuelve hipersensible a desajustes triviales en muestras grandes, así que FiabilityLab también reporta el cambio en CFI (&Delta;CFI) entre niveles consecutivos y aplica el criterio robusto al tamaño muestral de Cheung &amp; Rensvold (2002): una restricción se trata como sostenida si el LRT no es significativo (p &ge; .05) <i>o</i> &Delta;CFI &ge; -.01 — basta con que uno de los dos criterios se cumpla, ya que son dos formas distintas de detectar el mismo problema y pueden discrepar en cualquier dirección según el tamaño muestral. Schmitt y Kuljanin (2008) revisan cómo se aplica esta secuencia en la práctica, incluyendo con qué frecuencia se prueba y reporta cada nivel."
        )),
        h4(tr("When a Level Does Not Hold: Partial Invariance", "Cuando un Nivel No Se Sostiene: Invariancia Parcial")),
        p(tr(
          "A failed level does not mean the comparison must be abandoned, nor that full invariance should be forced regardless. The standard next step is to test partial invariance — freeing the constraint for specific items one at a time (via semTools::partialInvariance()/partialInvarianceCat()) to find which items break it, while keeping it for the rest — following a stepwise procedure that avoids testing stronger restrictions once a weaker one has already failed (Vandenberg &amp; Lance, 2000). A construct can still be meaningfully compared across groups under partial invariance, provided enough items remain invariant and the non-invariant ones are excluded from or flagged in that comparison.",
          "Un nivel fallido no significa que la comparación deba abandonarse, ni que la invariancia completa deba forzarse de todas formas. El siguiente paso estándar es probar invariancia parcial — liberar la restricción para ítems específicos uno a la vez (vía semTools::partialInvariance()/partialInvarianceCat()) para encontrar qué ítems la rompen, mientras se mantiene para el resto — siguiendo un procedimiento escalonado que evita probar restricciones más fuertes una vez que una más débil ya falló (Vandenberg &amp; Lance, 2000). Un constructo aún puede compararse significativamente entre grupos bajo invariancia parcial, siempre que suficientes ítems permanezcan invariantes y los no invariantes se excluyan de esa comparación o se señalen en ella."
        ))
      )
      invariance_html <- sect(tr("Measurement Invariance", "Invariancia de Medida"), inv_body)

      # ── Theoretical Foundations ─────────────────────────────────────────────
      found_table <- tbl(
        c(tr("Framework", "Marco teórico"), tr("Core idea", "Idea central"), tr("Key sources", "Fuentes clave")),
        list(
          c(tr("Classical Test Theory (CTT)", "Teoría Clásica de los Tests (TCT)"),
            "X = T + E", "Cronbach (1951); McDonald (1999); Nunnally &amp; Bernstein (1994)"),
          c(tr("Generalizability Theory", "Teoría de la Generalizabilidad"),
            tr("Extends CTT to decompose error into multiple simultaneous facets (raters, occasions, items)",
               "Extiende la TCT para descomponer el error en múltiples facetas simultáneas (jueces, ocasiones, ítems)"),
            "Brennan (2001); Shavelson &amp; Webb (1991)"),
          c(tr("Item Response Theory (IRT)", "Teoría de Respuesta al Ítem (TRI)"),
            tr("Models the probability of a response as a function of person ability and item parameters",
               "Modela la probabilidad de una respuesta en función de la habilidad de la persona y los parámetros del ítem"),
            "Embretson &amp; Reise (2000); Baker &amp; Kim (2017)")
        )
      )
      found_body <- paste0(
        found_table,
        h4(tr("Classical Test Theory", "Teoría Clásica de los Tests")),
        p(tr(
          "Models every observed score X as the sum of a true score T and random error E. Reliability, in this framework, is defined as the proportion of observed-score variance attributable to T — every coefficient in the Internal Consistency category above is an estimator of this same quantity, under different assumptions (Cronbach, 1951; McDonald, 1999; Nunnally &amp; Bernstein, 1994).",
          "Modela toda puntuación observada X como la suma de una puntuación verdadera T y un error aleatorio E. La confiabilidad, en este marco, se define como la proporción de la varianza de la puntuación observada atribuible a T — cada coeficiente de la categoría Internal Consistency de arriba es un estimador de esta misma cantidad, bajo distintos supuestos (Cronbach, 1951; McDonald, 1999; Nunnally &amp; Bernstein, 1994)."
        )),
        h4(tr("Generalizability Theory", "Teoría de la Generalizabilidad")),
        p(tr(
          "Extends CTT by decomposing measurement error into several simultaneous sources (facets) — raters, occasions, items, forms — instead of treating all error as one undifferentiated term. A G-study estimates the variance from each facet; a D-study then projects reliability for a specific planned measurement design (Brennan, 2001; Shavelson &amp; Webb, 1991). Not yet implemented as a FiabilityLab module (see the roadmap in ARCHITECTURE.md).",
          "Extiende la TCT descomponiendo el error de medición en varias fuentes simultáneas (facetas) — jueces, ocasiones, ítems, formas — en vez de tratar todo el error como un único término indiferenciado. Un G-study estima la varianza de cada faceta; un D-study luego proyecta la confiabilidad para un diseño de medición planificado específico (Brennan, 2001; Shavelson &amp; Webb, 1991). Aún no implementado como módulo de FiabilityLab (ver la hoja de ruta en ARCHITECTURE.md)."
        )),
        h4(tr("Item Response Theory", "Teoría de Respuesta al Ítem")),
        p(tr(
          "Models the probability of a specific item response as a function of the respondent's ability (or trait level) and the item's own parameters (difficulty, discrimination, and, in some models, guessing) — a fundamentally different approach from CTT's total-score-variance decomposition (Embretson &amp; Reise, 2000; Baker &amp; Kim, 2017). Referenced here as a theoretical foundation; not yet implemented as a FiabilityLab module.",
          "Modela la probabilidad de una respuesta específica a un ítem en función de la habilidad (o nivel del rasgo) del respondiente y de los parámetros propios del ítem (dificultad, discriminación y, en algunos modelos, adivinación) — un enfoque fundamentalmente distinto de la descomposición de varianza del puntaje total de la TCT (Embretson &amp; Reise, 2000; Baker &amp; Kim, 2017). Referenciada aquí como fundamento teórico; aún no implementada como módulo de FiabilityLab."
        ))
      )
      foundations_html <- sect(tr("Theoretical Foundations", "Fundamentos Teóricos"), found_body)

      # ── Common Errors ───────────────────────────────────────────────────────
      err_table <- tbl(
        c(tr("Error", "Error"), tr("Why it's wrong", "Por qué está mal"), tr("What to do instead", "Qué hacer en su lugar")),
        list(
          c(tr("Using Cronbach's &alpha; on a multidimensional scale", "Usar el &alpha; de Cronbach en una escala multidimensional"),
            tr("&alpha; assumes &tau;-equivalence (unidimensionality); with multiple dimensions it can be a misleading estimate",
               "El &alpha; asume &tau;-equivalencia (unidimensionalidad); con múltiples dimensiones puede ser una estimación engañosa"),
            tr("Compute &alpha;/&omega; per subscale, or report &omega;/&omega;ₕ from a fitted multi-factor model (Cronbach, 1951; McDonald, 1999)",
               "Calcule &alpha;/&omega; por subescala, o reporte &omega;/&omega;ₕ de un modelo multi-factor ajustado (Cronbach, 1951; McDonald, 1999)")),
          c(tr("Interpreting Kappa without checking prevalence", "Interpretar el Kappa sin revisar la prevalencia"),
            tr("High raw agreement with low Kappa signals the prevalence paradox, not poor agreement",
               "Alto acuerdo bruto con Kappa bajo señala la paradoja de prevalencia, no un acuerdo pobre"),
            tr("Report Gwet's AC1/AC2 alongside Kappa (Gwet, 2014)", "Reporte el AC1/AC2 de Gwet junto al Kappa (Gwet, 2014)")),
          c(tr("Confusing reliability with validity", "Confundir confiabilidad con validez"),
            tr("A measure can be highly reliable (consistent) yet not valid (not measuring the intended construct)",
               "Una medida puede ser muy confiable (consistente) pero no válida (no medir el constructo pretendido)"),
            tr("Reliability is necessary but not sufficient for validity (Nunnally &amp; Bernstein, 1994)",
               "La confiabilidad es necesaria pero no suficiente para la validez (Nunnally &amp; Bernstein, 1994)")),
          c(tr("Treating Ordinal &alpha; as interchangeable with raw &alpha;", "Tratar el &alpha; Ordinal como intercambiable con el &alpha; bruto"),
            tr("They are different estimators computed on different correlation matrices",
               "Son estimadores distintos calculados sobre matrices de correlación distintas"),
            tr("State which one was used, and prefer Ordinal &alpha; for skewed/ordinal items (Zumbo et al., 2007)",
               "Indique cuál se usó, y prefiera el &alpha; Ordinal para ítems sesgados/ordinales (Zumbo et al., 2007)"))
        )
      )
      errors_html <- sect(tr("Common Errors", "Errores Comunes"), err_table)

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
