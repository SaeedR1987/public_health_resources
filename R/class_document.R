#' Document R6 Class
#'
#' @description
#' Subclass of \code{\link{Orchestrator}} that provides shared DOCX handling
#' helpers for protocol/report-like classes.
#'
#' @importFrom R6 R6Class
Document <- R6::R6Class(
  "Document",
  inherit = Orchestrator,
  public = list(
    #' @description
    #' Creates a new Document object.
    #' @return A new Document object.
    initialize = function() {
      super$initialize()
      invisible(self)
    }
  ),

  private = list(
    # Create an officer doc using the REACH TOR template when available, or blank.
    create_base_doc = function(reference_docx = NULL) {
      if (!is.null(reference_docx) && file.exists(reference_docx)) {
        return(officer::read_docx(reference_docx))
      }
      reach_path <- system.file("resources", "reach_tor_template.docx", package = "phr")
      if (nzchar(reach_path)) {
        return(officer::read_docx(reach_path))
      }
      sys_path <- system.file("resources", "protocol_report_template.docx", package = "phr")
      if (nzchar(sys_path)) {
        return(officer::read_docx(sys_path))
      }
      officer::read_docx()
    },

    # Apply protocol schema handling in a predictable order.
    apply_protocol_schema_sections = function(doc) {
      schema <- self$protocol_schema
      if (is.null(schema) || !is.data.frame(schema) || nrow(schema) == 0) {
        return(doc)
      }
      required_cols <- c("tag_name", "handling", "condition", "default_value")
      if (!all(required_cols %in% names(schema))) {
        return(doc)
      }

      handling_order <- c(
        "row_delete", "replace", "input", "checkbox_replace",
        "conditional_replace", "calculate", "table", "image"
      )

      for (handling in handling_order) {
        schema_handling <- if ("handling" %in% names(schema) && !is.null(schema$handling)) {
          as.character(schema$handling)
        } else {
          rep("", nrow(schema))
        }
        idx <- which(schema_handling == handling)
        if (length(idx) == 0L) next
        for (i in idx) {
          row <- schema[i, required_cols, drop = FALSE]
          doc <- switch(
            handling,
            replace             = private$handle_replace(doc, row),
            input               = private$handle_input(doc, row),
            calculate           = private$handle_calculate(doc, row),
            checkbox_replace    = private$handle_checkbox_replace(doc, row),
            row_delete          = private$handle_row_delete(doc, row),
            table               = private$handle_table(doc, row),
            image               = private$handle_image(doc, row),
            conditional_replace = private$handle_conditional_replace(doc, row),
            doc
          )
        }
      }

      doc
    },

    handle_replace = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      default_value <- as.character(row$default_value[[1L]] %||% "")
      private$.replace(doc, tag, default_value)
    },

    handle_input = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      key <- sub("^@", "", tag)
      value <- if (nzchar(key) && key %in% names(self$metadata)) self$metadata[[key]] else ""
      private$.replace(doc, tag, as.character(value %||% ""))
    },

    handle_calculate = function(doc, row) {
      doc
    },

    handle_checkbox_replace = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      key <- sub("^@", "", tag)
      value <- if (nzchar(key) && key %in% names(self$metadata)) self$metadata[[key]] else FALSE
      private$.replace(doc, tag, if (isTRUE(value)) "X" else "\u25a1")
    },

    handle_row_delete = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      private$.replace(doc, tag, "")
    },

    handle_table = function(doc, row) {
      doc
    },

    handle_image = function(doc, row) {
      doc
    },

    handle_conditional_replace = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      default_value <- as.character(row$default_value[[1L]] %||% "")
      key <- sub("^@", "", tag)
      value <- if (nzchar(key) && key %in% names(self$metadata)) self$metadata[[key]] else FALSE
      private$.replace(doc, tag, if (isTRUE(value)) default_value else "")
    },

    # Load protocol schema metadata with a blank fallback.
    .load_protocol_schema = function() {
      required_cols <- c("tag_name", "handling", "condition", "default_value")
      empty_schema <- as.data.frame(
        setNames(replicate(length(required_cols), character(0), simplify = FALSE),
                 required_cols),
        stringsAsFactors = FALSE
      )

      schema_path <- tryCatch(
        system.file("resources", "protocol_schema_blank.csv", package = "phr"),
        error = function(e) ""
      )
      if (!nzchar(schema_path) || !file.exists(schema_path)) {
        schema_path <- file.path("inst", "resources", "protocol_schema_blank.csv")
      }
      if (!file.exists(schema_path)) {
        return(empty_schema)
      }

      schema <- tryCatch(
        utils::read.csv(schema_path, stringsAsFactors = FALSE, na.strings = character(0)),
        error = function(e) NULL
      )
      if (!is.data.frame(schema)) return(empty_schema)
      for (nm in required_cols) {
        if (!nm %in% names(schema)) schema[[nm]] <- character(nrow(schema))
      }
      schema[required_cols]
    },

    .replace = function(doc, old, new_val) {
      if (!is.character(old) || length(old) != 1L || !nzchar(old)) {
        return(doc)
      }
      private$._replace_across_runs(doc, old, as.character(new_val %||% ""))
    },

    ._replace_across_runs = function(doc, tag, new_val) {
      if (!is.character(tag) || length(tag) != 1L || !nzchar(tag)) return(doc)
      new_val <- as.character(new_val %||% "")
      replacement_is_identity <- identical(tag, new_val)

      body_xml <- officer::docx_body_xml(doc)
      ns       <- xml2::xml_ns(body_xml)

      paras <- xml2::xml_find_all(body_xml, ".//w:p", ns = ns)
      for (para in paras) {
        repeat {
          text_nodes <- xml2::xml_find_all(para, ".//w:t", ns = ns)
          if (length(text_nodes) == 0L) break

          texts    <- vapply(text_nodes, xml2::xml_text, character(1L))
          combined <- paste(texts, collapse = "")
          nc       <- nchar(combined)
          if (nc == 0L || !grepl(tag, combined, fixed = TRUE)) break

          node_idx <- rep(seq_along(texts), times = nchar(texts))

          matches <- gregexpr(tag, combined, fixed = TRUE)[[1L]]
          if (length(matches) == 1L && matches[1L] < 0L) break
          tag_len <- nchar(tag)

          match_start <- NA_integer_
          for (m in as.integer(matches)) {
            if (is.na(m) || m < 1L) next
            end <- m + tag_len - 1L
            left_char <- if (m > 1L) substr(combined, m - 1L, m - 1L) else ""
            right_char <- if (end < nc) substr(combined, end + 1L, end + 1L) else ""
            left_ok  <- !nzchar(left_char)  || !grepl("[A-Za-z0-9_.\\-]", left_char, perl = TRUE)
            right_ok <- !nzchar(right_char) || !grepl("[A-Za-z0-9_.\\-]", right_char, perl = TRUE)
            if (left_ok && right_ok) {
              match_start <- m
              break
            }
          }
          if (is.na(match_start)) break

          tag_start <- as.integer(match_start)
          tag_end   <- tag_start + tag_len - 1L

          new_texts <- character(length(text_nodes))

          if (tag_start > 1L) {
            pre_chars <- strsplit(substr(combined, 1L, tag_start - 1L), "", fixed = TRUE)[[1L]]
            pre_runs  <- node_idx[seq_len(tag_start - 1L)]
            for (j in seq_along(pre_chars)) {
              new_texts[pre_runs[j]] <- paste0(new_texts[pre_runs[j]], pre_chars[j])
            }
          }

          if (tag_start > length(node_idx)) {
            phr_warning(
              phr_txt("._replace_across_runs: tag_start ({tag_start}) exceeds node_idx length ({length(node_idx)}) for tag '{tag}'; skipping paragraph."),
              origin = "Document$._replace_across_runs"
            )
            break
          }
          rep_run <- node_idx[[tag_start]]
          new_texts[[rep_run]] <- paste0(new_texts[[rep_run]], new_val)

          if (tag_end < nc) {
            suf_chars <- strsplit(substr(combined, tag_end + 1L, nc), "", fixed = TRUE)[[1L]]
            suf_runs  <- node_idx[seq(tag_end + 1L, nc)]
            for (j in seq_along(suf_chars)) {
              new_texts[[suf_runs[j]]] <- paste0(new_texts[[suf_runs[j]]], suf_chars[j])
            }
          }

          for (i in seq_along(text_nodes)) {
            xml2::xml_text(text_nodes[[i]]) <- new_texts[[i]]
          }

          if (replacement_is_identity) break
        }
      }
      doc
    }
  )
)
