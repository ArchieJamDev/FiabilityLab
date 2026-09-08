bibliographyClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
  "bibliographyClass",
  inherit = bibliographyBase,
  private = list(
    .run = function() {
      lang <- .fl_normalize_lang(self$options$reportLang)
      topic <- self$options$topic
      style <- self$options$citationStyle
      
      tr <- function(en, es) {
        if (identical(lang, "es")) es else en
      }
      
      style_label <- switch(style,
        apa = "APA 7th",
        vancouver = "Vancouver",
        ieee = "IEEE",
        "APA 7th"
      )
      
      title <- tr("Methodological Bibliography", "Bibliografía Metodológica")
      
      refs <- list(
        ctt = c(
          "Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests. <i>Psychometrika, 16</i>(3), 297-334. https://doi.org/10.1007/BF02310555",
          "George, D., & Mallery, P. (2003). <i>SPSS for Windows step by step</i> (4th ed.). Allyn & Bacon.",
          "Kline, P. (2000). <i>The handbook of psychological testing</i> (2nd ed.). Routledge.",
          "Kuder, G. F., & Richardson, M. W. (1937). The theory of the estimation of test reliability. <i>Psychometrika, 2</i>(3), 151-160. https://doi.org/10.1007/BF02288391",
          "McDonald, R. P. (1999). <i>Test theory: A unified treatment</i>. Lawrence Erlbaum Associates.",
          "Nunnally, J. C., & Bernstein, I. H. (1994). <i>Psychometric theory</i> (3rd ed.). McGraw-Hill.",
          "Revelle, W. (2024). <i>psych: Procedures for psychological, psychometric, and personality research</i> (R package). https://CRAN.R-project.org/package=psych",
          "Zumbo, B. D., Gadermann, A. M., & Zeisser, C. (2007). Ordinal versions of coefficients alpha and theta for Likert rating scales. <i>Journal of Modern Applied Statistical Methods, 6</i>(1), 21-29. https://doi.org/10.22237/jmasm/1177992180"
        ),
        irr = c(
          "Cohen, J. (1960). A coefficient of agreement for nominal scales. <i>Educational and Psychological Measurement, 20</i>(1), 37-46.",
          "Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in assessing rater reliability. <i>Psychological Bulletin, 86</i>(2), 420-428.",
          "Krippendorff, K. (2018). <i>Content analysis: An introduction to its methodology</i> (4th ed.). Sage.",
          "Gwet, K. L. (2014). <i>Handbook of inter-rater reliability</i> (4th ed.). Advanced Analytics, LLC."
        ),
        gtheory = c(
          "Brennan, R. L. (2001). <i>Generalizability theory</i>. Springer.",
          "Shavelson, R. J., & Webb, N. M. (1991). <i>Generalizability theory: A primer</i>. Sage."
        ),
        irt = c(
          "Embretson, S. E., & Reise, S. P. (2000). <i>Item response theory for psychologists</i>. Lawrence Erlbaum Associates.",
          "Baker, F. B., & Kim, S. H. (2017). <i>The basics of item response theory using R</i>. Springer."
        )
      )
      
      refs_list <- refs[[topic]]
      if (is.null(refs_list)) {
        refs_list <- tr(
          "<p>Select a topic to view references.</p>",
          "<p>Seleccione un tema para ver las referencias.</p>"
        )
      } else {
        refs_html <- paste0("<li>", refs_list, "</li>", collapse = "\n")
        refs_list <- paste0("<ul>", refs_html, "</ul>")
      }
      
      html_content <- paste0(
        "<h2>", title, "</h2>",
        "<p><i>", tr("Citation style:", "Estilo de citación:"), " </i><b>", style_label, "</b></p>",
        "<hr/>",
        refs_list
      )
      
      self$results$intro$setContent(html_content)
    }
  )
)
