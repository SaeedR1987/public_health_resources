#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Create the Protocol Report Word Template
#
# Run this script once (from the package root directory) to generate the
# reference Word document used by Protocol$generate_report() and
# SurveyProtocol$generate_report():
#
#   source("dev/create_report_template.R")
#
# The resulting file is saved to inst/resources/protocol_report_template.docx
# and committed to the repository so that generate_report() can pick it up
# automatically via system.file().
# -----------------------------------------------------------------------------

library(officer)
library(flextable)

out_path <- file.path("inst", "resources", "protocol_report_template.docx")

# ---------------------------------------------------------------------------
# Build a minimal but well-structured Word document to serve as the template.
# All standard paragraph styles (heading 1 / 2, Normal, Table) are retained
# from Word's default theme so that generate_report() can apply them without
# needing to redefine each style.
# ---------------------------------------------------------------------------

doc <- read_docx()

# ── Cover / title page ──────────────────────────────────────────────────────
doc <- body_add_par(doc, "[Assessment Title]",        style = "Title")
doc <- body_add_par(doc, "[Country] | [Month Year]",  style = "Normal")
doc <- body_add_par(doc, paste0("Template generated: ", format(Sys.Date(), "%d %B %Y")),
                    style = "Normal")
doc <- body_add_break(doc)

# ── Section 1: Protocol Overview ────────────────────────────────────────────
doc <- body_add_par(doc, "1. Protocol Overview",  style = "heading 1")
doc <- body_add_par(doc, "Replace with metadata.", style = "Normal")

# ── Section 2: Research Objectives ──────────────────────────────────────────
doc <- body_add_par(doc, "2. Research Objectives", style = "heading 1")
doc <- body_add_par(doc, "Replace with objectives table.", style = "Normal")

# ── Section 3: Data Collection Tools ────────────────────────────────────────
doc <- body_add_par(doc, "3. Data Collection Tools", style = "heading 1")
doc <- body_add_par(doc, "Replace with tools and indicators.", style = "Normal")

# ── Section 4: Sampling Design (SurveyProtocol only) ────────────────────────
doc <- body_add_par(doc, "4. Sampling Design", style = "heading 1")
doc <- body_add_par(doc, "Replace with strata and selected PSUs.", style = "Normal")

# ── Save ─────────────────────────────────────────────────────────────────────
print(doc, target = out_path)
message("Template saved to: ", normalizePath(out_path))
