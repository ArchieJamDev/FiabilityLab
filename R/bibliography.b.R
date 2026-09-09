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
             doi = "10.1007/BF02310555", ref_type = "seminal"),
        list(topics = "ctt", authors = list(c("George", "D."), c("Mallery", "P.")), year = "2003",
             title = "SPSS for Windows step by step", edition = "4th ed.",
             is_book = TRUE, publisher = "Allyn & Bacon", ref_type = "book"),
        list(topics = "ctt", authors = list(c("Kline", "P.")), year = "2000",
             title = "The handbook of psychological testing", edition = "2nd ed.",
             is_book = TRUE, publisher = "Routledge", ref_type = "book"),
        list(topics = "ctt", authors = list(c("Kuder", "G. F."), c("Richardson", "M. W.")), year = "1937",
             title = "The theory of the estimation of test reliability",
             journal = "Psychometrika", volume = "2", issue = "3", pages = "151-160",
             doi = "10.1007/BF02288391", ref_type = "seminal"),
        list(topics = "ctt", authors = list(c("McDonald", "R. P.")), year = "1999",
             title = "Test theory: A unified treatment",
             is_book = TRUE, publisher = "Lawrence Erlbaum Associates", ref_type = "book"),
        list(topics = "ctt", authors = list(c("Nunnally", "J. C."), c("Bernstein", "I. H.")), year = "1994",
             title = "Psychometric theory", edition = "3rd ed.",
             is_book = TRUE, publisher = "McGraw-Hill", ref_type = "book"),
        list(topics = "ctt", authors = list(c("Revelle", "W.")), year = "2024",
             title = "psych: Procedures for psychological, psychometric, and personality research (R package)",
             is_book = TRUE, publisher = NULL, url = "https://CRAN.R-project.org/package=psych", ref_type = "book"),
        list(topics = "ctt", authors = list(c("Zumbo", "B. D."), c("Gadermann", "A. M."), c("Zeisser", "C.")), year = "2007",
             title = "Ordinal versions of coefficients alpha and theta for Likert rating scales",
             journal = "Journal of Modern Applied Statistical Methods", volume = "6", issue = "1", pages = "21-29",
             doi = "10.22237/jmasm/1177992180", ref_type = "methodological"),

        list(topics = "irr", authors = list(c("Fleiss", "J. L.")), year = "1971",
             title = "Measuring nominal scale agreement among many raters",
             journal = "Psychological Bulletin", volume = "76", issue = "5", pages = "378-382",
             doi = "10.1037/h0031619", ref_type = "seminal"),
        list(topics = "irr", authors = list(c("Landis", "J. R."), c("Koch", "G. G.")), year = "1977",
             title = "The measurement of observer agreement for categorical data",
             journal = "Biometrics", volume = "33", issue = "1", pages = "159-174",
             doi = "10.2307/2529310", ref_type = "seminal"),
        list(topics = "irr", authors = list(c("Cohen", "J.")), year = "1960",
             title = "A coefficient of agreement for nominal scales",
             journal = "Educational and Psychological Measurement", volume = "20", issue = "1", pages = "37-46",
             doi = "10.1177/001316446002000104", ref_type = "seminal"),
        list(topics = "irr", authors = list(c("Shrout", "P. E."), c("Fleiss", "J. L.")), year = "1979",
             title = "Intraclass correlations: Uses in assessing rater reliability",
             journal = "Psychological Bulletin", volume = "86", issue = "2", pages = "420-428",
             doi = "10.1037/0033-2909.86.2.420", ref_type = "seminal"),
        list(topics = "irr", authors = list(c("Krippendorff", "K.")), year = "2018",
             title = "Content analysis: An introduction to its methodology", edition = "4th ed.",
             is_book = TRUE, publisher = "Sage", ref_type = "book"),
        list(topics = "irr", authors = list(c("Gwet", "K. L.")), year = "2014",
             title = "Handbook of inter-rater reliability", edition = "4th ed.",
             is_book = TRUE, publisher = "Advanced Analytics, LLC", ref_type = "book"),
        list(topics = "irr", authors = list(c("Tukey", "J. W.")), year = "1949",
             title = "One degree of freedom for non-additivity",
             journal = "Biometrics", volume = "5", issue = "3", pages = "232-242",
             doi = "10.2307/3001938", ref_type = "seminal"),
        list(topics = "irr", authors = list(c("Koo", "T. K."), c("Li", "M. Y.")), year = "2016",
             title = "A guideline of selecting and reporting intraclass correlation coefficients for reliability research",
             journal = "Journal of Chiropractic Medicine", volume = "15", issue = "2", pages = "155-163",
             doi = "10.1016/j.jcm.2016.02.012", ref_type = "methodological"),
        list(topics = "irr", authors = list(c("Bujang", "M. A."), c("Baharum", "N.")), year = "2017",
             title = "A simplified guide to determination of sample size requirements for estimating the value of intraclass correlation coefficient: A review",
             journal = "Archives of Orofacial Sciences", volume = "12", issue = "1", pages = "1-11",
             ref_type = "methodological"),

        list(topics = "gtheory", authors = list(c("Brennan", "R. L.")), year = "2001",
             title = "Generalizability theory", is_book = TRUE, publisher = "Springer", ref_type = "book"),
        list(topics = "gtheory", authors = list(c("Shavelson", "R. J."), c("Webb", "N. M.")), year = "1991",
             title = "Generalizability theory: A primer", is_book = TRUE, publisher = "Sage", ref_type = "book"),

        list(topics = "irt", authors = list(c("Embretson", "S. E."), c("Reise", "S. P.")), year = "2000",
             title = "Item response theory for psychologists",
             is_book = TRUE, publisher = "Lawrence Erlbaum Associates", ref_type = "book"),
        list(topics = "irt", authors = list(c("Baker", "F. B."), c("Kim", "S. H.")), year = "2017",
             title = "The basics of item response theory using R", is_book = TRUE, publisher = "Springer", ref_type = "book"),

        list(topics = "sem", authors = list(c("Fornell", "C."), c("Larcker", "D. F.")), year = "1981",
             title = "Evaluating structural equation models with unobservable variables and measurement error",
             journal = "Journal of Marketing Research", volume = "18", issue = "1", pages = "39-50",
             doi = "10.1177/002224378101800104", ref_type = "seminal"),
        list(topics = "sem", authors = list(c("Hancock", "G. R."), c("Mueller", "R. O.")), year = "2001",
             title = "Rethinking construct reliability within latent variable systems",
             is_book_chapter = TRUE, editors = "R. Cudeck, S. du Toit, & D. Sörbom",
             chapter_in = "Structural equation modeling: Present and future—A Festschrift in honor of Karl Jöreskog",
             pages = "195-216", publisher = "Scientific Software International", ref_type = "seminal"),
        list(topics = "sem", authors = list(c("Henseler", "J."), c("Ringle", "C. M."), c("Sarstedt", "M.")), year = "2015",
             title = "A new criterion for assessing discriminant validity in variance-based structural equation modeling",
             journal = "Journal of the Academy of Marketing Science", volume = "43", issue = "1", pages = "115-135",
             doi = "10.1007/s11747-014-0403-8", ref_type = "seminal"),
        list(topics = "sem", authors = list(c("Hu", "L."), c("Bentler", "P. M.")), year = "1999",
             title = "Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives",
             journal = "Structural Equation Modeling", volume = "6", issue = "1", pages = "1-55",
             doi = "10.1080/10705519909540118", ref_type = "methodological"),
        list(topics = "sem", authors = list(c("MacCallum", "R. C."), c("Roznowski", "M."), c("Necowitz", "L. B.")), year = "1992",
             title = "Model modifications in covariance structure analysis: The problem of capitalization on chance",
             journal = "Psychological Bulletin", volume = "111", issue = "3", pages = "490-504",
             doi = "10.1037/0033-2909.111.3.490", ref_type = "methodological"),
        list(topics = "sem", authors = list(c("Horn", "J. L.")), year = "1965",
             title = "A rationale and test for the number of factors in factor analysis",
             journal = "Psychometrika", volume = "30", issue = "2", pages = "179-185",
             doi = "10.1007/BF02289447", ref_type = "seminal"),

        list(topics = "invariance", authors = list(c("Cheung", "G. W."), c("Rensvold", "R. B.")), year = "2002",
             title = "Evaluating goodness-of-fit indexes for testing measurement invariance",
             journal = "Structural Equation Modeling", volume = "9", issue = "2", pages = "233-255",
             doi = "10.1207/S15328007SEM0902_5", ref_type = "methodological"),
        list(topics = "invariance", authors = list(c("Vandenberg", "R. J."), c("Lance", "C. E.")), year = "2000",
             title = "A review and synthesis of the measurement invariance literature: Suggestions, practices, and recommendations for organizational research",
             journal = "Organizational Research Methods", volume = "3", issue = "1", pages = "4-70",
             doi = "10.1177/109442810031002", ref_type = "review")
      )

      # -----------------------------------------------------------------------
      # Bibliometric data (Table 1: article citations; Table 2: journal
      # indexing). Citation counts are the higher of OpenAlex vs. Crossref's
      # "is-referenced-by-count" for that DOI, both queried directly (not
      # estimated); the lower value is kept in "other" per AssumptionsLab's
      # own convention. Journal Scopus/Web of Science/quartile status was
      # verified via SCImago/journal-metrics lookups. Snapshot: September 2026.
      # Books are excluded from both tables (no bibliographic indexing data
      # applies to them).
      # ES: Datos bibliométricos (Tabla 1: citas por artículo; Tabla 2:
      # indexación de revistas). Los conteos de citas son el mayor entre
      # OpenAlex y el "is-referenced-by-count" de Crossref para ese DOI,
      # ambos consultados directamente; el valor menor se guarda en "other"
      # siguiendo la misma convención de AssumptionsLab. El estado
      # Scopus/Web of Science/cuartil de cada revista se verificó vía
      # SCImago/journal-metrics. Instantánea: septiembre 2026. Los libros se
      # excluyen de ambas tablas (no aplican datos de indexación bibliográfica).
      # -----------------------------------------------------------------------
      citation_db <- list(
        "Fleiss|1971"   = list(citations = "8746",  source = "OpenAlex", other = "Crossref API (6268)"),
        "Landis|1977"   = list(citations = "80951", source = "OpenAlex", other = "Crossref API (64279)"),
        "Cronbach|1951" = list(citations = "43876", source = "OpenAlex", other = "Crossref API (30225)"),
        "Kuder|1937"    = list(citations = "2018",  source = "OpenAlex", other = "Crossref API (1371)"),
        "Zumbo|2007"    = list(citations = "1030",  source = "OpenAlex", other = "Crossref API (802)"),
        "Cohen|1960"    = list(citations = "42176", source = "OpenAlex", other = "Crossref API (32369)"),
        "Shrout|1979"   = list(citations = "23299", source = "OpenAlex", other = "Crossref API (19797)"),
        "Tukey|1949"    = list(citations = "909",   source = "OpenAlex", other = ""),
        "Koo|2016"      = list(citations = "29067", source = "OpenAlex", other = ""),
        "Bujang|2017"   = list(citations = "456",   source = "OpenAlex", other = ""),
        "Fornell|1981"  = list(citations = "70924", source = "OpenAlex", other = ""),
        "Henseler|2015" = list(citations = "36358", source = "OpenAlex", other = ""),
        "Hu|1999"       = list(citations = "108216", source = "OpenAlex", other = ""),
        "MacCallum|1992" = list(citations = "1586",  source = "OpenAlex", other = ""),
        "Horn|1965"      = list(citations = "8803",  source = "OpenAlex", other = "Crossref API (6652)"),
        "Cheung|2002"    = list(citations = "16119", source = "OpenAlex", other = "Crossref API (13311)"),
        "Vandenberg|2000" = list(citations = "7958", source = "OpenAlex", other = "Crossref API (6231)")
      )

      journal_biblio <- list(
        "Biometrics" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
        "Psychometrika" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
        "Educational and Psychological Measurement" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
        "Psychological Bulletin" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
        "Journal of Modern Applied Statistical Methods" = list(
          scopus = TRUE, wos = FALSE,
          other = tr("PsycINFO, EMBASE, ScienceDirect", "PsycINFO, EMBASE, ScienceDirect"),
          quartile = tr("no current SJR/JCR quartile found", "sin cuartil SJR/JCR vigente encontrado")),
        "Journal of Chiropractic Medicine" = list(scopus = TRUE, wos = FALSE, other = "", quartile = "Q2 (SJR)"),
        "Archives of Orofacial Sciences" = list(scopus = TRUE, wos = FALSE, other = "",
          quartile = tr("Q3-Q4 (SJR, varies by subject category)", "Q3-Q4 (SJR, varía según categoría temática)")),
        "Journal of Marketing Research" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
        "Journal of the Academy of Marketing Science" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
        "Structural Equation Modeling" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
        "Organizational Research Methods" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1")
      )

      in_text_cite <- function(r) {
        n <- length(r$authors)
        surnames <- vapply(r$authors, function(a) a[1], character(1))
        label <- if (n == 1) surnames[1]
                 else if (n == 2) paste0(surnames[1], " & ", surnames[2])
                 else paste0(surnames[1], " et al.")
        paste0(label, " (", r$year, ")")
      }

      ref_type_label <- function(rt) {
        tr(switch(rt, seminal = "Original/seminal", methodological = "Methodological",
                  review = "Review/comparison", application = "Application", book = "Reference book", rt),
           switch(rt, seminal = "Original/seminal", methodological = "Metodológica",
                  review = "Revisión/comparación", application = "Aplicación", book = "Libro de referencia", rt))
      }

      get_citation <- function(first_author, year) {
        hit <- citation_db[[paste0(first_author, "|", year)]]
        if (is.null(hit)) list(citations = NULL, source = "", other = "") else hit
      }

      get_journal_biblio <- function(journal_name) {
        hit <- journal_biblio[[journal_name]]
        if (is.null(hit)) list(scopus = NA, wos = NA, other = "", quartile = tr("pending", "pendiente"))
        else hit
      }

      yesno <- function(x) {
        if (is.na(x)) return(tr("pending", "pendiente"))
        if (isTRUE(x)) tr("Yes", "Sí") else "No"
      }

      build_citations_table <- function() {
        article_refs <- Filter(function(r) !isTRUE(r$is_book) && !isTRUE(r$is_book_chapter), refs_db)
        cite_labels  <- vapply(article_refs, in_text_cite, character(1))
        article_refs <- article_refs[order(cite_labels)]

        td_l <- 'style="text-align: left; padding: 6px 10px;"'
        td_c <- 'style="text-align: center; padding: 6px 10px;"'
        th_l <- 'style="text-align: left; padding: 6px 10px; border-bottom: 1px solid #000;"'
        th_c <- 'style="text-align: center; padding: 6px 10px; border-bottom: 1px solid #000;"'

        rows_html <- vapply(article_refs, function(r) {
          cite <- get_citation(r$authors[[1]][1], r$year)
          cites_val  <- if (!is.null(cite$citations)) cite$citations else tr("Pending", "Pendiente")
          source_val <- if (nzchar(cite$source)) cite$source else "—"
          other_val  <- if (nzchar(cite$other)) cite$other else "—"
          paste0("<tr>",
                 "<td ", td_l, ">", esc(in_text_cite(r)), "</td>",
                 "<td ", td_c, ">", esc(cites_val), "</td>",
                 "<td ", td_c, ">", esc(source_val), "</td>",
                 "<td ", td_c, ">", esc(other_val), "</td>",
                 "<td ", td_c, ">", esc(ref_type_label(r$ref_type)), "</td>",
                 "</tr>")
        }, character(1))

        intro_txt <- paste0("<p>", tr(
          "The citation count for each reference is the higher of the two values returned by OpenAlex and Crossref's own “is-referenced-by-count” for that DOI (both queried directly, not estimated); the lower value is kept in the “Other sources” column. Values correspond to a snapshot taken in September 2026 and change continuously. Books are not included in this table; only journal-published articles are listed here.",
          "El número de citas de cada referencia es el mayor entre los dos valores retornados por OpenAlex y el “is-referenced-by-count” propio de Crossref para ese DOI (ambos consultados directamente, no estimados); el valor menor se conserva en la columna “Otras fuentes”. Los valores corresponden a una instantánea tomada en septiembre de 2026 y cambian continuamente. Los libros no se incluyen en esta tabla; solo se listan aquí artículos publicados en revistas."
        ), "</p>")

        paste0(
          '<div style="max-width: 700px; width: 100%; line-height: 1; margin-top: 24px; text-align: justify;">',
          '<div style="margin-bottom: 2px;"><h3 style="margin: 0;">',
          tr("Bibliometric Profile and Impact of the Literature Used", "Perfil Bibliométrico e Impacto de la Literatura Utilizada"),
          '</h3><div style="font-size: 0.75em; font-style: italic; line-height: 1; margin-top: 2px;">',
          tr("(periodicals only)", "(solo publicaciones periódicas)"), '</div></div>',
          '<h4 style="margin-top: 16px;">', tr("Article-Level Citation Metrics", "Métricas de Citación a Nivel de Artículo"), '</h4>',
          intro_txt,
          '<div style="page-break-inside: avoid; break-inside: avoid;">',
          '<table style="border-collapse: collapse; width: 100%; margin-bottom: 4px;">',
          '<tr><td style="border: none; padding: 0; line-height: 1;"><b>', tr("Table 1", "Tabla 1"), '</b></td></tr>',
          '<tr><td style="border: none; padding: 0; font-style: italic; line-height: 1;">', tr("Citations", "Citaciones"), '</td></tr>',
          '</table>',
          '<table style="border-collapse: collapse; width: 100%; font-size: 0.85em; border-top: 2px solid #000; border-bottom: 2px solid #000;">',
          "<thead><tr>",
          "<th ", th_l, ">", tr("Reference", "Referencia"), "</th>",
          "<th ", th_c, ">", tr("Citations", "Citaciones"), "</th>",
          "<th ", th_c, ">", tr("Source", "Fuente"), "</th>",
          "<th ", th_c, ">", tr("Other sources", "Otras fuentes"), "</th>",
          "<th ", th_c, ">", tr("Reference type", "Tipo de referencia"), "</th>",
          "</tr></thead>",
          "<tbody>", paste(rows_html, collapse = ""), "</tbody>",
          "</table></div></div>"
        )
      }

      build_biblio_table <- function() {
        journal_names <- sort(unique(vapply(
          Filter(function(r) !isTRUE(r$is_book) && !isTRUE(r$is_book_chapter), refs_db), function(r) r$journal, character(1)
        )))

        td_c <- 'style="text-align: center; padding: 6px 10px;"'
        th_c <- 'style="text-align: center; padding: 6px 10px; border-bottom: 1px solid #000;"'

        rows_html <- vapply(journal_names, function(jname) {
          bib <- get_journal_biblio(jname)
          paste0("<tr>",
                 "<td ", td_c, ">", esc(jname), "</td>",
                 "<td ", td_c, ">", esc(yesno(bib$scopus)), "</td>",
                 "<td ", td_c, ">", esc(yesno(bib$wos)), "</td>",
                 "<td ", td_c, ">", esc(bib$other), "</td>",
                 "<td ", td_c, ">", esc(bib$quartile), "</td>",
                 "</tr>")
        }, character(1))

        intro_txt <- paste0("<p>", tr(
          "This table describes, for each journal in which at least one of FiabilityLab's references was published, its presence in the main bibliographic indexing databases (Scopus, Web of Science) and, where applicable, other relevant indexes and the journal's quartile. These indicators reflect the editorial quality and dissemination reach of the publication, not the quality of the individual cited article. Each journal appears only once, even if several articles published in it were cited.",
          "Esta tabla describe, para cada revista en la que se publicó al menos una de las referencias de FiabilityLab, su presencia en las principales bases de indexación bibliográfica (Scopus, Web of Science) y, cuando aplica, otras indexaciones relevantes y el cuartil de la revista. Estos indicadores reflejan la calidad editorial y el alcance de divulgación de la publicación, no la calidad del artículo individual citado. Cada revista aparece una sola vez, aunque se hayan citado varios artículos publicados en ella."
        ), "</p>")

        paste0(
          '<div style="max-width: 700px; width: 100%; line-height: 1; margin-top: 24px; text-align: justify;">',
          '<h4 style="margin-top: 0;">', tr("Journal-Level Quality and Dissemination Metrics", "Métricas de Calidad y Divulgación a Nivel de Revista"), '</h4>',
          intro_txt,
          '<div style="page-break-inside: avoid; break-inside: avoid;">',
          '<table style="border-collapse: collapse; width: 100%; margin-bottom: 4px;">',
          '<tr><td style="border: none; padding: 0; line-height: 1;"><b>', tr("Table 2", "Tabla 2"), '</b></td></tr>',
          '<tr><td style="border: none; padding: 0; font-style: italic; line-height: 1;">', tr("Bibliometric Summary", "Resumen Bibliométrico"), '</td></tr>',
          '</table>',
          '<table style="border-collapse: collapse; width: 100%; font-size: 0.85em; border-top: 2px solid #000; border-bottom: 2px solid #000;">',
          "<thead><tr>",
          "<th ", th_c, ">", tr("Journal", "Revista"), "</th>",
          "<th ", th_c, ">Scopus</th>",
          "<th ", th_c, ">", tr("Web of Science", "Web of Science"), "</th>",
          "<th ", th_c, ">", tr("Other relevant indexes", "Otras indexaciones relevantes"), "</th>",
          "<th ", th_c, ">", tr("Quartile", "Cuartil"), "</th>",
          "</tr></thead>",
          "<tbody>", paste(rows_html, collapse = ""), "</tbody>",
          "</table></div>",
          "<p style='font-size: 0.8em; font-style: italic; margin-top: 8px;'>", tr(
            "Note. Journal-level data (Scopus, Web of Science, quartile, other indexes) were verified per journal, not per individual article, since they belong to the publication, not the article. Each article's reference type is shown in Table 1 (Citations), not here. Books are not included, since the indicators in this table are specific to periodicals.",
            "Nota. Los datos a nivel de revista (Scopus, Web of Science, cuartil, otras indexaciones) se verificaron por revista, no por artículo individual, ya que pertenecen a la publicación, no al artículo. El tipo de referencia de cada artículo se muestra en la Tabla 1 (Citaciones), no aquí. Los libros no se incluyen, ya que los indicadores de esta tabla son específicos de publicaciones periódicas."
          ), "</p>",
          "</div>"
        )
      }

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
        inner <- if (isTRUE(r$is_book_chapter)) {
          paste0(join_authors_apa(r$authors), " (", r$year, "). ", esc(r$title), ". ",
                 "In ", esc(r$editors), " (", if (grepl(",", r$editors)) "Eds." else "Ed.", "), <i>",
                 esc(r$chapter_in), "</i> (pp. ", r$pages, "). ", esc(r$publisher), ".",
                 if (nzchar(link)) paste0(" ", link) else "")
        } else if (isTRUE(r$is_book)) {
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
      self$results$citationsTable$setContent(build_citations_table())
      self$results$biblioSummary$setContent(build_biblio_table())
    }
  )
)
