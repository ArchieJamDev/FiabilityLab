interRaterClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
  "interRaterClass",
  inherit = interRaterBase,
  private = list(
    .run = function() {
      lang <- .fl_normalize_lang(self$options$reportLang)
      
      tr <- function(en, es) {
        if (identical(lang, "es")) es else en
      }
      
      ratings <- self$options$ratings
      if (length(ratings) < 2) {
        msg <- tr(
          "<p><i>Please select at least 2 rating variables to compute agreement coefficients.</i></p>",
          "<p><i>Por favor seleccione al menos 2 variables de calificación para calcular los coeficientes de acuerdo.</i></p>"
        )
        self$results$recommendations$setContent(msg)
        self$results$mainTable$setVisible(FALSE)
        return()
      }
      
      msg <- tr(
        "<h3>Inter-Rater Agreement Analysis</h3>",
        "<h3>Análisis de Acuerdo entre Jueces</h3>"
      )
      msg <- paste0(msg, "<p>", tr(
        "This module will compute Kappa, Gwet's AC1, Krippendorff's Alpha, ICC, and Kendall's W. <b>Coming in Phase 2.</b>",
        "Este módulo calculará Kappa, AC1 de Gwet, Alfa de Krippendorff, ICC y W de Kendall. <b>Disponible en Fase 2.</b>"
      ), "</p>")
      
      self$results$recommendations$setContent(msg)
      self$results$mainTable$setVisible(FALSE)
    }
  )
)
