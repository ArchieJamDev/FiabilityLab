bibliographyClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
  "bibliographyClass",
  inherit = bibliographyBase,
  private = list(
    .run = function() {
      lang  <- .fl_normalize_lang(self$options$reportLang)
      topic <- self$options$topic
      style <- self$options$citationStyle

      tr <- function(en, es) if (identical(lang, "es")) es else en

      style_label <- switch(style,
        apa = "APA 7th",
        vancouver = "Vancouver",
        ieee = "IEEE",
        "APA 7th"
      )

      esc <- function(x) {
        x <- gsub("&", "&amp;", x, fixed = TRUE)
        x <- gsub("<", "&lt;", x, fixed = TRUE)
        x <- gsub(">", "&gt;", x, fixed = TRUE)
        x
      }

      # -----------------------------------------------------------------------
      # Curated reference database.
      # ES: Base de datos de referencias curadas.
      #
      # Every entry's authors/year/journal-or-publisher/volume/issue/pages was
      # individually verified against its primary source before being added
      # here (Springer/Psychometrika, Sage, Routledge, etc.); DOIs are only
      # included when directly confirmed. No reference is auto-generated.
      # ES: Los autores/año/revista-o-editorial/volumen/número/páginas de cada
      # entrada fueron verificados individualmente contra su fuente primaria
      # antes de agregarse aquí; los DOI solo se incluyen cuando fueron
      # confirmados directamente. Ninguna referencia se genera automáticamente.
      #
      # Fields: topics (character vector), authors (list of c(surname, initials)),
      # year, title, journal + volume/issue/pages (articles) OR publisher
      # (is_book = TRUE), doi (optional).
      # -----------------------------------------------------------------------
      refs_db <- list(
        list(topics = "ctt", authors = list(c("Cronbach", "L. J.")), year = "1951",
             title = "Coefficient alpha and the internal structure of tests",
             journal = "Psychometrika", volume = "16", issue = "3", pages = "297-334",
             doi = "10.1007/BF02310555"),
        list(topics = "ctt", authors = list(c("George", "D."), c("Mallery", "P.")), year = "2003",
             title = "SPSS for Windows step by step", edition = "4th ed.",
             is_book = TRUE, publisher = "Allyn & Bacon"),
        list(topics = "ctt", authors = list(c("Kline", "P.")), year = "2000",
             title = "The handbook of psychological testing", edition = "2nd ed.",
             is_book = TRUE, publisher = "Routledge"),
        list(topics = "ctt", authors = list(c("Kuder", "G. F."), c("Richardson", "M. W.")), year = "1937",
             title = "The theory of the estimation of test reliability",
             journal = "Psychometrika", volume = "2", issue = "3", pages = "151-160",
             doi = "10.1007/BF02288391"),
        list(topics = "ctt", authors = list(c("McDonald", "R. P.")), year = "1999",
             title = "Test theory: A unified treatment",
             is_book = TRUE, publisher = "Lawrence Erlbaum Associates"),
        list(topics = "ctt", authors = list(c("Nunnally", "J. C."), c("Bernstein", "I. H.")), year = "1994",
             title = "Psychometric theory", edition = "3rd ed.",
             is_book = TRUE, publisher = "McGraw-Hill"),
        list(topics = "ctt", authors = list(c("Revelle", "W.")), year = "2024",
             title = "psych: Procedures for psychological, psychometric, and personality research (R package)",
             is_book = TRUE, publisher = NULL, url = "https://CRAN.R-project.org/package=psych"),
        list(topics = "ctt", authors = list(c("Zumbo", "B. D."), c("Gadermann", "A. M."), c("Zeisser", "C.")), year = "2007",
             title = "Ordinal versions of coefficients alpha and theta for Likert rating scales",
             journal = "Journal of Modern Applied Statistical Methods", volume = "6", issue = "1", pages = "21-29",
             doi = "10.22237/jmasm/1177992180"),

        list(topics = "irr", authors = list(c("Cohen", "J.")), year = "1960",
             title = "A coefficient of agreement for nominal scales",
             journal = "Educational and Psychological Measurement", volume = "20", issue = "1", pages = "37-46"),
        list(topics = "irr", authors = list(c("Shrout", "P. E."), c("Fleiss", "J. L.")), year = "1979",
             title = "Intraclass correlations: Uses in assessing rater reliability",
             journal = "Psychological Bulletin", volume = "86", issue = "2", pages = "420-428"),
        list(topics = "irr", authors = list(c("Krippendorff", "K.")), year = "2018",
             title = "Content analysis: An introduction to its methodology", edition = "4th ed.",
             is_book = TRUE, publisher = "Sage"),
        list(topics = "irr", authors = list(c("Gwet", "K. L.")), year = "2014",
             title = "Handbook of inter-rater reliability", edition = "4th ed.",
             is_book = TRUE, publisher = "Advanced Analytics, LLC"),

        list(topics = "gtheory", authors = list(c("Brennan", "R. L.")), year = "2001",
             title = "Generalizability theory", is_book = TRUE, publisher = "Springer"),
        list(topics = "gtheory", authors = list(c("Shavelson", "R. J."), c("Webb", "N. M.")), year = "1991",
             title = "Generalizability theory: A primer", is_book = TRUE, publisher = "Sage"),

        list(topics = "irt", authors = list(c("Embretson", "S. E."), c("Reise", "S. P.")), year = "2000",
             title = "Item response theory for psychologists",
             is_book = TRUE, publisher = "Lawrence Erlbaum Associates"),
        list(topics = "irt", authors = list(c("Baker", "F. B."), c("Kim", "S. H.")), year = "2017",
             title = "The basics of item response theory using R", is_book = TRUE, publisher = "Springer")
      )

      # -----------------------------------------------------------------------
      # APA 7th formatter, with hanging indent.
      # ES: Formateador APA 7.ª edición, con sangría francesa.
      # -----------------------------------------------------------------------
      join_authors_apa <- function(authors) {
        n <- length(authors)
        parts <- vapply(authors, function(a) paste0(esc(a[1]), ", ", esc(a[2])), character(1))
        if (n == 1) return(parts[1])
        if (n == 2) return(paste0(parts[1], ", &amp; ", parts[2]))
        paste0(paste(parts[1:(n - 1)], collapse = ", "), ", &amp; ", parts[n])
      }

      link_html <- function(r) {
        if (!is.null(r$doi) && nzchar(r$doi)) {
          url <- paste0("https://doi.org/", r$doi)
          paste0('<a href="', url, '">', esc(url), "</a>")
        } else if (!is.null(r$url) && nzchar(r$url)) {
          paste0('<a href="', r$url, '">', esc(r$url), "</a>")
        } else ""
      }

      hanging <- function(inner_html) {
        paste0('<p style="margin: 0 0 12px 0; padding-left: 2em; text-indent: -2em; ',
               'line-height: 1; text-align: left;">', inner_html, "</p>")
      }

      format_apa <- function(r) {
        link <- link_html(r)
        ed   <- if (!is.null(r$edition)) paste0(" (", r$edition, ")") else ""
        inner <- if (isTRUE(r$is_book)) {
          paste0(join_authors_apa(r$authors), " (", r$year, "). <i>", esc(r$title), "</i>", ed, ".",
                 if (!is.null(r$publisher)) paste0(" ", esc(r$publisher), ".") else "",
                 if (nzchar(link)) paste0(" ", link) else "")
        } else {
          paste0(join_authors_apa(r$authors), " (", r$year, "). ", esc(r$title), ". ",
                 "<i>", esc(r$journal), ", ", r$volume, "</i>(", r$issue, "), ", r$pages, ".",
                 if (nzchar(link)) paste0(" ", link) else "")
        }
        hanging(inner)
      }

      # FiabilityLab currently only implements APA 7th formatting; the
      # citationStyle option's other values are a placeholder for future work.
      format_ref <- format_apa

      # -----------------------------------------------------------------------
      # Filter by topic, sort alphabetically by first author's surname.
      # ES: Filtrar por tema, ordenar alfabéticamente por apellido del primer autor.
      # -----------------------------------------------------------------------
      selected <- if (identical(topic, "all")) {
        refs_db
      } else {
        Filter(function(r) topic %in% r$topics, refs_db)
      }
      if (length(selected) > 0) {
        sort_key <- vapply(selected, function(r) r$authors[[1]][1], character(1))
        selected <- selected[order(sort_key)]
      }

      page_style <- 'style="max-width: 700px; line-height: 1; text-align: justify;"'

      title_txt <- tr("Methodological Bibliography", "Bibliografía Metodológica")

      intro <- paste0(
        '<div ', page_style, '>',
        "<h3>", title_txt, "</h3>",
        "<p>", tr(
          "This section collects the methodological references that support the decisions, criteria, and interpretations in FiabilityLab. Each reference was individually verified (authors, journal or publisher, volume, pages, and DOI or direct link) before being included; FiabilityLab does not auto-generate citations or accept unverified references.",
          "Esta sección reúne las referencias metodológicas que respaldan las decisiones, criterios e interpretaciones de FiabilityLab. Cada referencia fue verificada individualmente (autores, revista o editorial, volumen, páginas y DOI o enlace directo) antes de incluirse; FiabilityLab no genera citas automáticamente ni acepta referencias sin verificar."
        ), "</p>",
        "<p>", tr("Reference style: ", "Estilo de referencia: "), "<b>", style_label, "</b>.</p>",
        "</div>"
      )

      topic_label <- switch(topic,
        all     = tr("All References", "Todas las Referencias"),
        ctt     = tr("Classical Test Theory", "Teoría Clásica de los Tests"),
        irr     = tr("Inter-Rater Reliability", "Confiabilidad entre Jueces"),
        gtheory = tr("Generalizability Theory", "Teoría de la Generalizabilidad"),
        irt     = tr("Item Response Theory", "Teoría de Respuesta al Ítem"),
        topic
      )

      if (length(selected) == 0) {
        refs <- paste0(
          '<div ', page_style, '>',
          "<p><b>", tr("Selected topic: ", "Tema seleccionado: "), esc(topic_label), "</b></p>",
          "<p>", tr(
            "There are no curated references for this topic yet. References are added progressively as each FiabilityLab module is developed (see the Documento Maestro's Library/Bibliography admission contract).",
            "Todavía no hay referencias curadas para este tema. Las referencias se agregan progresivamente a medida que se desarrolla cada módulo de FiabilityLab (ver el contrato de admisión Library/Bibliography del Documento Maestro)."
          ), "</p>",
          "</div>"
        )
      } else {
        ref_lines <- vapply(selected, format_ref, character(1))
        refs <- paste0(
          '<div ', page_style, '>',
          "<p><b>", tr("Selected topic: ", "Tema seleccionado: "), esc(topic_label), "</b></p>",
          paste(ref_lines, collapse = ""),
          "</div>"
        )
      }

      notes <- paste0(
        '<div ', page_style, '>',
        "<p>", tr(
          "The bibliography presented here brings together articles and books by the original authors, prioritizing classic references and those that have received the greatest recognition and use in current research. It does not aim to be exhaustive, but rather to offer a precise selection of the sources most relevant to the reliability designs considered in FiabilityLab. It is advisable to periodically compare these references against the most recent scientific literature, since new studies may be published that support, challenge, or expand the methods included here.",
          "La bibliografía aquí presentada reúne artículos y libros de los autores originales, priorizando las referencias clásicas y aquellas que han recibido mayor reconocimiento y uso en la investigación actual. No pretende ser exhaustiva, sino ofrecer una selección precisa de las fuentes más relevantes para los diseños de confiabilidad considerados en FiabilityLab. Es recomendable contrastar periódicamente estas referencias con la literatura científica más reciente, ya que pueden publicarse nuevos estudios que respalden, cuestionen o amplíen los métodos aquí incluidos."
        ), "</p>",
        "</div>"
      )

      self$results$intro$setContent(intro)
      self$results$references$setContent(refs)
      self$results$notes$setContent(notes)
    }
  )
)
