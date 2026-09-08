# FiabilityLab - Sistema de traducciones bilingüe
# Inspirado en el sistema .al_* de AssumptionsLab

.fl_normalize_lang <- function(lang) {
  if (is.null(lang)) return("en")
  lang <- tolower(as.character(lang))
  if (lang %in% c("es", "esp", "spanish", "español")) return("es")
  "en"
}

.fl_text <- function(lang, section, key) {
  lang <- .fl_normalize_lang(lang)
  
  texts <- list(
    intro = list(
      title_en = "FiabilityLab",
      title_es = "FiabilityLab",
      welcome_en = "Welcome to FiabilityLab, a pedagogical module for reliability analysis.",
      welcome_es = "Bienvenido a FiabilityLab, un módulo pedagógico para el análisis de confiabilidad."
    ),
    consistency = list(
      title_en = "Internal Consistency",
      title_es = "Consistencia Interna",
      alpha_desc_en = "Cronbach's Alpha assumes tau-equivalence (equal factor loadings).",
      alpha_desc_es = "El Alfa de Cronbach asume tau-equivalencia (cargas factoriales iguales)."
    )
  )
  
  full_key <- paste0(key, "_", lang)
  val <- texts[[section]][[full_key]]
  if (is.null(val)) {
    fallback <- texts[[section]][[paste0(key, "_en")]]
    if (is.null(fallback)) return(key)
    return(fallback)
  }
  val
}

.fl_text_block <- function(lang, section, key) {
  .fl_text(lang, section, key)
}

.fl_clean_text <- function(text) {
  if (is.null(text) || length(text) == 0) return(character(0))
  trimws(unlist(strsplit(as.character(text), "\n")))
}
