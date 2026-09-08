fiabilityLibraryClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
  "fiabilityLibraryClass",
  inherit = fiabilityLibraryBase,
  private = list(
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
