fiabilityLibraryClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
  "fiabilityLibraryClass",
  inherit = fiabilityLibraryBase,
  private = list(
    # ── Admission-contract entries: one card per coefficient implemented in
    # internalConsistency. No coefficient ships in an analysis module without
    # its card here (see ARCHITECTURE.md / Documento Maestro, section 3.1).
    .coefficients_html = function(tr, detail) {
      card <- function(name, def, assume, when, pitfall) {
        if (identical(detail, "summary")) {
          paste0("<p><b>", name, ":</b> ", def, "</p>")
        } else {
          paste0(
            "<div style='margin-bottom:14px;padding:8px 12px;border-left:3px solid #4E79A7;'>",
            "<p><b>", name, "</b></p>",
            "<p>", def, "</p>",
            "<p><i>", tr("Assumptions:", "Supuestos:"), "</i> ", assume, "</p>",
            "<p><i>", tr("Prefer it when:", "Prefiéralo cuando:"), "</i> ", when, "</p>",
            "<p><i>", tr("Common pitfall:", "Error común:"), "</i> ", pitfall, "</p>",
            "</div>")
        }
      }

      cards <- paste0(
        card(
          tr("Cronbach's α (Alpha)", "Alfa de Cronbach (α)"),
          tr("The average of all possible split-half reliabilities; equivalently, the proportion of total score variance attributable to a common source among items.",
             "El promedio de todas las confiabilidades posibles de mitades partidas; equivalentemente, la proporción de la varianza del puntaje total atribuible a una fuente común entre ítems."),
          tr("τ-equivalence (all items measure the same construct with equal true-score weight) and, for the raw-score formula, roughly continuous/normal item distributions.",
             "τ-equivalencia (todos los ítems miden el mismo constructo con igual peso de puntuación verdadera) y, para la fórmula de puntaje bruto, distribuciones de ítem razonablemente continuas/normales."),
          tr("Items are Likert/continuous, approximately normal, and you have reason to believe loadings are roughly equal.",
             "Los ítems son Likert/continuos, aproximadamente normales, y hay razones para creer que las cargas son aproximadamente iguales."),
          tr("Reporting α for a multidimensional scale, or when items visibly differ in discrimination — α is then a biased (usually deflated) estimate; McDonald's ω is the correct alternative.",
             "Reportar α para una escala multidimensional, o cuando los ítems difieren visiblemente en discriminación — el α es entonces una estimación sesgada (usualmente subestimada); el Omega de McDonald es la alternativa correcta.")
        ),
        card(
          tr("Ordinal α (polychoric)", "Alfa Ordinal (policórica)"),
          tr("The same standardized-alpha formula as above, but applied to the polychoric correlation matrix (which models items as coarsely-categorized continuous variables) instead of Pearson correlations.",
             "La misma fórmula del alfa estandarizado de arriba, pero aplicada a la matriz de correlación policórica (que modela los ítems como variables continuas categorizadas groseramente) en vez de correlaciones de Pearson."),
          tr("τ-equivalence at the level of the underlying continuous latent response, not the observed ordinal categories.",
             "τ-equivalencia a nivel de la respuesta latente continua subyacente, no de las categorías ordinales observadas."),
          tr("Items are ordinal/Likert with few response categories (≤ 5–7) or visibly skewed/non-normal — raw α underestimates reliability in exactly this case.",
             "Los ítems son ordinales/Likert con pocas categorías de respuesta (≤ 5–7) o visiblemente sesgados/no normales — el α bruto subestima la confiabilidad exactamente en este caso."),
          tr("Treating it as interchangeable with raw α in a manuscript without saying which one was used — the two are not the same estimator and can differ by .05–.10 or more on skewed Likert data.",
             "Tratarlo como intercambiable con el α bruto en un manuscrito sin indicar cuál se usó — no son el mismo estimador y pueden diferir en .05–.10 o más en datos Likert sesgados.")
        ),
        card(
          tr("McDonald's ω (total)", "Omega de McDonald (ω total)"),
          tr("The proportion of total score variance explained by a fitted factor model (one or more factors), computed from the model's factor loadings rather than assuming they are all equal.",
             "La proporción de la varianza del puntaje total explicada por un modelo factorial ajustado (uno o más factores), calculada a partir de las cargas factoriales del modelo en vez de asumir que son todas iguales."),
          tr("A factor model with at least 1 common factor can be fit to the items; does not require τ-equivalence.",
             "Que se pueda ajustar un modelo factorial con al menos 1 factor común a los ítems; no requiere τ-equivalencia."),
          tr("As the general-purpose default reliability estimate — it degrades gracefully to α's value when items ARE τ-equivalent, and stays correct when they are not.",
             "Como la estimación de confiabilidad por defecto de uso general — se degrada de forma correcta al valor del α cuando los ítems SÍ son τ-equivalentes, y se mantiene correcta cuando no lo son."),
          tr("Assuming any reported “ω” is automatically the hierarchical one — always check whether it is ω total (single/multi-factor total variance) or ωₕ (general-factor-only variance; see below), they answer different questions.",
             "Asumir que cualquier “ω” reportado es automáticamente el jerárquico — siempre verifique si es ω total (varianza total de uno o varios factores) o ωₕ (varianza solo del factor general; ver abajo), responden preguntas distintas.")
        ),
        card(
          tr("McDonald's ω hierarchical (ωₕ)", "Omega Jerárquico de McDonald (ωₕ)"),
          tr("The proportion of total score variance attributable specifically to ONE general factor running through all items, net of whatever separate group-factor (subscale) variance also exists — requires a genuine bifactor/multi-group-factor model, not a single-factor fit.",
             "La proporción de la varianza del puntaje total atribuible específicamente a UN factor general que atraviesa todos los ítems, descontando la varianza propia de los factores de grupo (subescalas) que también existan — requiere un modelo bifactor/multi-factor de grupo genuino, no un ajuste de un solo factor."),
          tr("The scale is multidimensional (≥ 2 group factors below a general factor) — with only 1 factor fit, ωₕ is not a distinct quantity from ω total and carries no extra information.",
             "La escala es multidimensional (≥ 2 factores de grupo bajo un factor general) — con un ajuste de un solo factor, ωₕ no es una cantidad distinta del ω total y no aporta información adicional."),
          tr("Reporting a scale is “suitable for a single total score” because ωₕ is high, without checking that a real multi-factor model was fit — a degenerate 1-factor ωₕ will look high for the wrong reason (it never separated group-factor variance to begin with).",
             "Reportar que una escala es “adecuada para un puntaje total único” porque ωₕ es alto, sin verificar que se ajustó un modelo multi-factor real — un ωₕ degenerado de 1 factor se verá alto por la razón equivocada (nunca separó la varianza de factores de grupo)."),
          tr("Its own single-factor case is the pitfall: FiabilityLab reports which factor count parallel analysis suggested alongside it for exactly this reason.",
             "Su propio caso de un solo factor es el error a evitar: por esto FiabilityLab reporta junto a él cuántos factores sugirió el análisis paralelo.")
        ),
        card(
          "GLB (Greatest Lower Bound)",
          tr("The mathematically tightest (largest, and therefore best) lower-bound estimate of reliability achievable without any distributional or factor-structure assumption, obtained by a constrained optimization over the item covariance matrix.",
             "La estimación de límite inferior matemáticamente más ajustada (mayor y, por tanto, mejor) de confiabilidad alcanzable sin ningún supuesto distribucional o de estructura factorial, obtenida mediante una optimización restringida sobre la matriz de covarianza de ítems."),
          tr("None beyond a usable (non-singular) item covariance matrix — this is its main appeal.",
             "Ninguno más allá de una matriz de covarianza de ítems utilizable (no singular) — este es su atractivo principal."),
          tr("You want the least assumption-dependent possible reliability estimate, e.g. as a sensitivity check against α/ω.",
             "Quiere la estimación de confiabilidad menos dependiente de supuestos posible, p. ej. como verificación de sensibilidad frente a α/ω."),
          tr("GLB is known to be upward-biased (overestimates true reliability) in small samples relative to the number of items — do not treat it as a strictly “safer” number than α/ω when n is small.",
             "Se sabe que el GLB está sesgado al alza (sobreestima la confiabilidad real) en muestras pequeñas en relación con el número de ítems — no lo trate como un número estrictamente “más seguro” que α/ω cuando n es pequeño.")
        ),
        card(
          tr("Split-half (Spearman-Brown)", "Mitades Partidas (Spearman-Brown)"),
          tr("Correlate two halves of the test, then correct that correlation upward with the Spearman-Brown prophecy formula to estimate the reliability of the FULL-length test (reliability rises with test length).",
             "Correlacione dos mitades de la prueba, y luego corrija esa correlación al alza con la fórmula de profecía de Spearman-Brown para estimar la confiabilidad de la prueba de LONGITUD COMPLETA (la confiabilidad aumenta con la longitud de la prueba)."),
          tr("The two halves are themselves parallel (equal true-score variance and equal error variance).",
             "Que las dos mitades sean en sí mismas paralelas (igual varianza de puntaje verdadero e igual varianza de error)."),
          tr("As a quick, model-free cross-check against α/ω, especially for a very long instrument.",
             "Como una verificación rápida y libre de modelo frente a α/ω, especialmente para un instrumento muy largo."),
          tr("Treating one particular split as definitive — the estimate is highly sensitive to WHICH items land in which half; FiabilityLab reports the mean across splits precisely because a single split is not a stable estimate on its own.",
             "Tratar una partición particular como definitiva — la estimación es muy sensible a QUÉ ítems caen en cada mitad; FiabilityLab reporta el promedio entre particiones precisamente porque una sola partición no es una estimación estable por sí sola.")
        ),
        card(
          tr("Guttman λ2 / λ6", "Guttman λ2 / λ6"),
          tr("Model-free lower-bound estimates of reliability (like GLB, but computed differently): λ2 uses the covariance structure directly; λ6 uses each item's squared multiple correlation with the rest of the scale.",
             "Estimaciones de límite inferior de confiabilidad libres de modelo (como el GLB, pero calculadas de forma distinta): λ2 usa directamente la estructura de covarianza; λ6 usa la correlación múltiple al cuadrado de cada ítem con el resto de la escala."),
          tr("None beyond a usable item covariance matrix, same as GLB.",
             "Ninguno más allá de una matriz de covarianza de ítems utilizable, igual que el GLB."),
          tr("As an alternative, usually tighter, lower bound than α when you want a model-free estimate without fitting a factor model.",
             "Como una alternativa, usualmente más ajustada, al α cuando se desea una estimación libre de modelo sin ajustar un modelo factorial."),
          tr("λ6 is undefined/unstable when an item correlates perfectly with the rest of the scale (a near-duplicate item) — check the item-total correlation table first if λ6 looks implausible.",
             "λ6 queda indefinido/inestable cuando un ítem correlaciona perfectamente con el resto de la escala (un ítem casi duplicado) — revise primero la tabla de correlación ítem-total si λ6 se ve implausible.")
        ),
        card(
          "KR-20 / KR-21 (Kuder-Richardson)",
          tr("KR-20 is Cronbach's α specialized to strictly dichotomous (0/1) items, computed from each item's p·q variance; KR-21 further simplifies KR-20 by assuming all items share the same difficulty (proportion correct).",
             "KR-20 es el α de Cronbach especializado a ítems estrictamente dicotómicos (0/1), calculado a partir de la varianza p·q de cada ítem; KR-21 simplifica aún más al KR-20 asumiendo que todos los ítems comparten la misma dificultad (proporción de aciertos)."),
          tr("Same as α (τ-equivalence); KR-21 additionally assumes equal item difficulty, which is rarely exactly true.",
             "Igual que el α (τ-equivalencia); el KR-21 asume adicionalmente igual dificultad de ítem, lo cual rara vez es exactamente cierto."),
          tr("Items are strictly binary (correct/incorrect, yes/no) — e.g. a knowledge or aptitude test scored right/wrong.",
             "Los ítems son estrictamente binarios (correcto/incorrecto, sí/no) — p. ej. una prueba de conocimiento o aptitud calificada como correcta/incorrecta."),
          tr("Using KR-21 as if it were interchangeable with KR-20 when item difficulties actually vary a lot — KR-21 will then understate reliability; report KR-20 (or α) instead.",
             "Usar el KR-21 como si fuera intercambiable con el KR-20 cuando las dificultades de los ítems en realidad varían mucho — el KR-21 subestimará entonces la confiabilidad; reporte el KR-20 (o el α) en su lugar.")
        )
      )

      paste0("<h3>", tr("Reliability Coefficients", "Coeficientes de Confiabilidad"), "</h3>",
             "<p style='font-size:12px;color:#666;'>", tr(
               "Every coefficient internalConsistency can compute, with its assumptions and common pitfalls. Full references: Bibliography → Classical Test Theory.",
               "Todo coeficiente que internalConsistency puede calcular, con sus supuestos y errores comunes. Referencias completas: Bibliografía → Teoría Clásica de los Tests."),
             "</p>", cards)
    },

    .run = function() {
      lang <- .fl_normalize_lang(self$options$reportLang)
      category <- self$options$category
      detail <- self$options$detail
      
      tr <- function(en, es) {
        if (identical(lang, "es")) es else en
      }
      
      title <- tr("Fiability Library", "Biblioteca de Confiabilidad")
      
      welcome <- tr(
        "Welcome to FiabilityLab. This library contains the theoretical foundations of reliability analysis.",
        "Bienvenido a FiabilityLab. Esta biblioteca contiene los fundamentos teóricos del análisis de confiabilidad."
      )
      
      content <- switch(
        category,
        concepts = tr(
          "<h3>Core Concepts</h3><p><b>Reliability</b> refers to the consistency of a measure. A measure is reliable if it produces similar results under consistent conditions.</p><p>Three main types: <i>internal consistency</i>, <i>stability (test-retest)</i>, and <i>inter-rater agreement</i>.</p>",
          "<h3>Conceptos Fundamentales</h3><p><b>Confiabilidad</b> se refiere a la consistencia de una medida. Una medida es confiable si produce resultados similares bajo condiciones consistentes.</p><p>Tres tipos principales: <i>consistencia interna</i>, <i>estabilidad (test-retest)</i>, y <i>acuerdo entre jueces</i>.</p>"
        ),
        theories = tr(
          "<h3>Theoretical Frameworks</h3><p><b>Classical Test Theory (CTT):</b> X = T + E. Observed score equals true score plus error.</p><p><b>Generalizability Theory:</b> Extends CTT to multiple facets of error simultaneously.</p><p><b>Item Response Theory (IRT):</b> Models the probability of a specific response as a function of person ability and item characteristics.</p>",
          "<h3>Marcos Teóricos</h3><p><b>Teoría Clásica de los Tests (TCT):</b> X = T + E. La puntuación observada es igual a la puntuación verdadera más el error.</p><p><b>Teoría de la Generalizabilidad:</b> Extiende la TCT a múltiples facetas de error simultáneamente.</p><p><b>Teoría de Respuesta al Ítem (TRI):</b> Modela la probabilidad de una respuesta específica en función de la habilidad de la persona y las características del ítem.</p>"
        ),
        coefficients = private$.coefficients_html(tr, detail),
        errors = tr(
          "<h3>Common Errors</h3><p>⚠️ <b>Using Cronbach's Alpha on multidimensional scales:</b> Alpha assumes unidimensionality. If your scale has multiple dimensions, compute Alpha per dimension.</p><p>⚠️ <b>Interpreting Kappa without checking prevalence:</b> High agreement with low Kappa may indicate the Kappa paradox (prevalence effect).</p><p>⚠️ <b>Confusing reliability with validity:</b> A measure can be reliable but not valid.</p>",
          "<h3>Errores Comunes</h3><p>⚠️ <b>Usar el Alfa de Cronbach en escalas multidimensionales:</b> El Alfa asume unidimensionalidad. Si su escala tiene múltiples dimensiones, calcule el Alfa por dimensión.</p><p>⚠️ <b>Interpretar el Kappa sin revisar la prevalencia:</b> Alto acuerdo con Kappa bajo puede indicar la paradoja del Kappa (efecto de prevalencia).</p><p>⚠️ <b>Confundir confiabilidad con validez:</b> Una medida puede ser confiable pero no válida.</p>"
        ),
        tr("<h3>Select a category</h3><p>Choose a category from the menu to explore content.</p>",
           "<h3>Seleccione una categoría</h3><p>Elija una categoría del menú para explorar el contenido.</p>")
      )
      
      html_content <- paste0("<h2>", title, "</h2><p>", welcome, "</p><hr/>", content)
      self$results$intro$setContent(html_content)
    }
  )
)
