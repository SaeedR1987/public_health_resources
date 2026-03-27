# ────────────────────────────────────────────────
# IPHRA Text Translation / Localization Utility
# ────────────────────────────────────────────────

# Translation dictionary (extend later)
iphra_translations <- list(
  en = list(
    export_tor = "Export ToR",
    validation_passed = "Validation checks passed (dummy mode).",
    tor_export_success = "ToR export simulated successfully ✅",
    # ---- auto-title static parts ----
    "Distribution of"            = "Distribution of",
    "Cumulative Distribution of" = "Cumulative Distribution of",
    "Z-Score Distribution of"    = "Z-Score Distribution of",
    "Age Distribution of"        = "Age Distribution of",
    "Prevalence of"              = "Prevalence of",
    "Mean of"                    = "Mean of",
    "Treemap of"                 = "Treemap of",
    "Response Distribution"      = "Response Distribution",
    "Domain Radar"               = "Domain Radar",
    "Correlation Matrix"         = "Correlation Matrix",
    "Age-Sex Pyramid"            = "Age-Sex Pyramid",
    "IYCF Area Graph"            = "IYCF Area Graph",
    "Flow Diagram"               = "Flow Diagram",
    "Frequency Table of"         = "Frequency Table of",
    "Cumulative Mean of"         = "Cumulative Mean of",
    "Cumulative SD of"           = "Cumulative SD of",
    "Digit Preference Score of"  = "Digit Preference Score of",
    "Cumulative Count of"        = "Cumulative Count of",
    "Cumulative Ratio of"        = "Cumulative Ratio of",
    ", by"                       = ", by"
  ),
  fr = list(
    export_tor = "Exporter les TdR",
    validation_passed = "Vérifications terminées avec succès (mode fictif).",
    tor_export_success = "Exportation simulée des TdR réussie ✅",
    # ---- auto-title static parts ----
    "Distribution of"            = "Distribution de",
    "Cumulative Distribution of" = "Distribution cumulative de",
    "Z-Score Distribution of"    = "Distribution des z-scores de",
    "Age Distribution of"        = "Distribution par âge de",
    "Prevalence of"              = "Prévalence de",
    "Mean of"                    = "Moyenne de",
    "Treemap of"                 = "Treemap de",
    "Response Distribution"      = "Distribution des réponses",
    "Domain Radar"               = "Radar des domaines",
    "Correlation Matrix"         = "Matrice de corrélation",
    "Age-Sex Pyramid"            = "Pyramide âge-sexe",
    "IYCF Area Graph"            = "Graphique ANJE",
    "Flow Diagram"               = "Diagramme de flux",
    "Frequency Table of"         = "Tableau de fréquence de",
    "Cumulative Mean of"         = "Moyenne cumulative de",
    "Cumulative SD of"           = "Écart-type cumulatif de",
    "Digit Preference Score of"  = "Score de préférence numérique de",
    "Cumulative Count of"        = "Nombre cumulatif de",
    "Cumulative Ratio of"        = "Ratio cumulatif de",
    ", by"                       = ", par"
  )
)

# Placeholder for current language (can later live in session$userData)
iphra_current_lang <- "en"

# ---- Safe Translation Lookup ----
iphra_txt <- function(key, lang = NULL, default = NULL, session = shiny::getDefaultReactiveDomain()) {
  # [🔗 FUTURE] When language reactivity is connected, use session$userData$lang()
  # If no reactive session available, fallback to iphra_current_lang or "en"

  # Use glue::glue to evaluate expressions in {} within the key string
  # This allows dynamic content like iphra_txt("Processing {n} items")
  key <- glue::glue(key, .envir = parent.frame())

  if (is.null(lang)) {
    if (!is.null(session) && !is.null(session$userData$lang)) {
      # Safe reactive access — only works once session$userData$lang is defined
      lang <- tryCatch(session$userData$lang(), error = function(e) NULL)
    }
  }

  # Fallback chain
  if (is.null(lang) || !lang %in% names(iphra_translations)) {
    if (exists("iphra_current_lang", envir = .GlobalEnv)) {
      lang <- get("iphra_current_lang", envir = .GlobalEnv)
    } else {
      lang <- "en"
    }
  }

  # ---- Lookup ----
  value <- iphra_translations[[lang]][[key]]

  # ---- Fallback logic ----
  if (is.null(value)) {
    if (!is.null(default)) return(default)
    return(paste0("⧫", key, "⧫"))  # visually marks missing keys
  }

  return(value)
}
