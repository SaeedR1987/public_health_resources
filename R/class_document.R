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
    #' @field document Cached \code{officer::rdocx} object used for document
    #'   generation workflows.
    document = NULL,

    #' @field reference_doc_filename Optional template filename/path used to
    #'   initialize \code{document}.
    reference_doc_filename = NULL,

    #' @description
    #' Creates a new Document object.
    #' @param reference_doc_filename Optional template filename/path. If this is
    #'   a file path that exists, it is used directly; otherwise it is treated as
    #'   a candidate resource filename.
    #' @return A new Document object.
    initialize = function(reference_doc_filename = NULL) {
      super$initialize()
      self$reference_doc_filename <- reference_doc_filename
      self$document <- private$.create_base_doc(self$reference_doc_filename)
      invisible(self)
    },

    #' @description
    #' Generate a document from \code{protocol_schema} and write it to disk.
    #'
    #' This method uses the cached \code{self$document} created at initialization,
    #' applies schema sections, removes unreplaced tags, writes the output, and
    #' updates \code{self$document} with the generated document object.
    #'
    #' @param output_file Character output path.
    #' @param open Logical indicating whether to open the output in a browser.
    #' @return Invisibly returns \code{self}.
    generate_doc = function(output_file = "protocol_report.docx", open = FALSE) {
      phr_try({
        doc <- self$document
        if (is.null(doc)) {
          doc <- private$.create_base_doc(self$reference_doc_filename)
        }
        doc <- private$apply_protocol_schema_sections(doc)
        doc <- private$.remove_remaining_tags(doc)
        self$document <- doc
        print(doc, target = output_file)
        phr_message(
          phr_txt("Document saved to: {output_file}"),
          origin = "Document$generate_doc"
        )
        if (isTRUE(open)) utils::browseURL(output_file)
      }, on_error = "abort", origin = "Document$generate_doc")
      invisible(self)
    }
  ),

  private = list(
    #' @description Return default template filename candidates.
    #' @return Character vector of template filenames.
    .default_template_filenames = function() {
      c("reach_tor_template.docx", "protocol_report_template.docx")
    },

    #' @description Resolve and load the base reference document.
    #' @param reference_doc_filename Optional template filename/path.
    #' @return An \code{officer::rdocx} object.
    .create_base_doc = function(reference_doc_filename = NULL) {
      if (!is.null(reference_doc_filename) && file.exists(reference_doc_filename)) {
        self$document <- officer::read_docx(reference_doc_filename)
        return(self$document)
      }

      template_names <- unique(c(
        as.character(reference_doc_filename %||% character(0)),
        private$.default_template_filenames()
      ))
      template_names <- template_names[nzchar(template_names)]

      for (template_name in template_names) {
        packaged <- system.file("resources", template_name, package = "phr")
        if (nzchar(packaged) && file.exists(packaged)) {
          self$document <- officer::read_docx(packaged)
          return(self$document)
        }
        local_path <- file.path("inst", "resources", template_name)
        if (file.exists(local_path)) {
          self$document <- officer::read_docx(local_path)
          return(self$document)
        }
      }

      self$document <- officer::read_docx()
      self$document
    },

    # Apply protocol schema handling in a predictable order.
    apply_protocol_schema_sections = function(doc) {
      schema <- self$protocol_schema
      if (is.null(schema) || !is.data.frame(schema) || nrow(schema) == 0) {
        return(doc)
      }
      required_cols <- private$.schema_required_cols()
      if (!all(required_cols %in% names(schema))) {
        phr_warning(
          phr_txt("protocol_schema missing required columns: {paste(setdiff(required_cols, names(schema)), collapse=', ')}"),
          origin = "Document$apply_protocol_schema_sections"
        )
        return(doc)
      }

      handling_order <- c("row_delete", "replace", "input", "checkbox_replace", "calculate", "table", "image")

      for (handling in handling_order) {
        schema_handling <- if ("handling" %in% names(schema) && !is.null(schema$handling)) {
          as.character(schema$handling)
        } else {
          rep("", nrow(schema))
        }
        normalized_handling <- ifelse(schema_handling == "conditional_replace", "replace", schema_handling)
        idx <- which(normalized_handling == handling)
        if (length(idx) == 0L) next
        for (i in idx) {
          row <- schema[i, required_cols, drop = FALSE]
          if (!isTRUE(private$.should_apply_schema_row(row))) next
          doc <- switch(
            handling,
            replace             = private$.handle_replace(doc, row),
            input               = private$.handle_input(doc, row),
            calculate           = private$.handle_calculate(doc, row),
            checkbox_replace    = private$.handle_checkbox_replace(doc, row),
            row_delete          = private$.handle_row_delete(doc, row),
            table               = private$.handle_table(doc, row),
            image               = private$.handle_image(doc, row),
            doc
          )
        }
      }

      doc
    },

    .handle_replace = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      default_value <- as.character(row$default_value[[1L]] %||% "")
      private$.replace(doc, tag, default_value)
    },

    .handle_input = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      key <- sub("^@", "", tag)
      value <- if (nzchar(key) && key %in% names(self$metadata)) self$metadata[[key]] else ""
      private$.replace(doc, tag, as.character(value %||% ""))
    },

    .handle_calculate = function(doc, row) {
      doc
    },

    .handle_checkbox_replace = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      key <- sub("^@", "", tag)
      value <- if (nzchar(key) && key %in% names(self$metadata)) self$metadata[[key]] else FALSE
      private$.replace(doc, tag, if (isTRUE(value)) "X" else "\u25a1")
    },

    .handle_row_delete = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      private$.replace(doc, tag, "")
    },

    #' @description Handle a schema row with \code{handling == "table"}.
    #' @param doc \code{officer::rdocx} document object.
    #' @param row Single-row schema data frame.
    #' @return Updated document object.
    .handle_table = function(doc, row) {
      tag <- as.character(row[["tag_name"]][[1L]] %||% "")
      if (nzchar(tag)) {
        doc <- private$._replace_across_runs(doc, tag, tag)
      }
      private$.dispatch_schema_function(doc = doc, row = row)
    },

    #' @description Handle a schema row with \code{handling == "image"}.
    #' @param doc \code{officer::rdocx} document object.
    #' @param row Single-row schema data frame.
    #' @return Updated document object.
    .handle_image = function(doc, row) {
      tag <- as.character(row[["tag_name"]][[1L]] %||% "")
      if (nzchar(tag)) {
        doc <- private$._replace_across_runs(doc, tag, tag)
      }
      private$.dispatch_schema_function(doc = doc, row = row)
    },

    # Load protocol schema metadata with a blank fallback.
    .load_protocol_schema = function() {
      required_cols <- private$.schema_required_cols()
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

    .schema_required_cols = function() {
      c("tag_name", "handling", "condition", "default_value", "function_name")
    },

    .dispatch_schema_function = function(doc, row) {
      fn_name <- trimws(as.character(row[["function_name"]][[1L]] %||% ""))
      if (!nzchar(fn_name)) return(doc)
      fn <- tryCatch(get(fn_name, mode = "function"), error = function(e) NULL)
      if (!is.function(fn)) return(doc)
      out <- tryCatch(fn(self = self, doc = doc, row = row), error = function(e) NULL)
      if (is.null(out)) doc else out
    },

    .schema_row = function(tag) {
      schema <- self$protocol_schema
      if (!is.character(tag) || length(tag) != 1 || !nzchar(tag) ||
          is.null(schema) || !is.data.frame(schema) ||
          !"tag_name" %in% names(schema)) {
        return(NULL)
      }
      idx <- which(as.character(schema$tag_name) == tag)
      if (length(idx) == 0L) return(NULL)
      schema[idx[1L], , drop = FALSE]
    },

    .tag_is_missing_from_schema = function(tag) {
      startsWith(as.character(tag %||% ""), "@") && is.null(private$.schema_row(tag))
    },

    .checkbox = function(doc, tag, condition) {
      if (private$.tag_is_missing_from_schema(tag)) {
        return(private$.replace(doc, tag, ""))
      }
      private$.replace(doc, tag, if (isTRUE(condition)) "X" else "\u25a1")
    },

    .make_w_para = function(text, bold = FALSE, space_before_pt = 0L,
                            space_after_pt = 0L, font_size_pt = NULL) {
      W <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
      esc <- function(s) {
        s <- gsub("&", "&amp;", s, fixed = TRUE)
        s <- gsub("<", "&lt;",  s, fixed = TRUE)
        s <- gsub(">", "&gt;",  s, fixed = TRUE)
        s
      }
      sp_before <- as.integer(space_before_pt) * 20L
      sp_after  <- as.integer(space_after_pt)  * 20L
      ppr_xml <- sprintf('<w:pPr><w:spacing w:before="%d" w:after="%d"/></w:pPr>',
                         sp_before, sp_after)
      rpr_parts <- character(0)
      if (bold) rpr_parts <- c(rpr_parts, "<w:b/>")
      if (!is.null(font_size_pt) && !is.na(font_size_pt)) {
        sz <- as.integer(font_size_pt) * 2L
        rpr_parts <- c(rpr_parts,
                       sprintf('<w:sz w:val="%d"/>', sz),
                       sprintf('<w:szCs w:val="%d"/>', sz))
      }
      rpr_xml <- if (length(rpr_parts) > 0L) {
        paste0("<w:rPr>", paste(rpr_parts, collapse = ""), "</w:rPr>")
      } else ""
      xml2::read_xml(sprintf(
        '<w:p xmlns:w="%s">%s<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r></w:p>',
        W, ppr_xml, rpr_xml, esc(text)
      ))
    },

    .replace_tag_in_cell = function(doc, tag, items) {
      body_xml <- officer::docx_body_xml(doc)
      ns       <- xml2::xml_ns(body_xml)

      tc_paras <- xml2::xml_find_all(body_xml, ".//w:tc/w:p", ns = ns)
      target_para <- NULL
      for (p in tc_paras) {
        if (grepl(tag, xml2::xml_text(p), fixed = TRUE)) {
          target_para <- p
          break
        }
      }
      if (is.null(target_para)) return(FALSE)

      for (item in items) {
        node <- private$.make_w_para(
          text            = item$text,
          bold            = isTRUE(item$bold),
          space_before_pt = if (is.null(item$space_before_pt)) 0L else item$space_before_pt,
          space_after_pt  = if (is.null(item$space_after_pt))  0L else item$space_after_pt,
          font_size_pt    = item$font_size_pt
        )
        xml2::xml_add_sibling(target_para, node, .where = "before")
      }
      xml2::xml_remove(target_para)
      TRUE
    },

    .remove_remaining_tags = function(doc) {
      body_xml <- officer::docx_body_xml(doc)
      ns       <- xml2::xml_ns(body_xml)
      tag_pattern <- "@[A-Za-z0-9_.\\-]+"

      paras <- xml2::xml_find_all(body_xml, ".//w:p", ns = ns)
      for (para in paras) {
        text_nodes <- xml2::xml_find_all(para, ".//w:t", ns = ns)
        if (length(text_nodes) == 0L) next

        texts    <- vapply(text_nodes, xml2::xml_text, character(1L))
        combined <- paste(texts, collapse = "")
        if (!grepl(tag_pattern, combined, perl = TRUE)) next

        nc <- nchar(combined)
        if (nc == 0L) next

        node_idx <- rep(seq_along(texts), times = nchar(texts))
        matches    <- gregexpr(tag_pattern, combined, perl = TRUE)[[1L]]
        match_lens <- attr(matches, "match.length")
        remove_pos <- logical(nc)
        for (j in seq_along(matches)) {
          if (matches[j] > 0L) {
            start <- matches[j]
            end   <- min(matches[j] + match_lens[j] - 1L, nc)
            remove_pos[start:end] <- TRUE
          }
        }

        chars <- strsplit(combined, "", fixed = TRUE)[[1L]]
        for (i in seq_along(text_nodes)) {
          node_char_idx <- which(node_idx == i)
          if (length(node_char_idx) == 0L) next
          keep     <- !remove_pos[node_char_idx]
          new_text <- paste(chars[node_char_idx[keep]], collapse = "")
          xml2::xml_text(text_nodes[[i]]) <- new_text
        }
      }
      doc
    },

    .resolve_condition_flag = function(condition) {
      # Condition semantics:
      # - empty string: apply row
      # - literal true/false: apply according to literal
      # - otherwise: treat as key in conditional_metadata, then metadata
      condition <- trimws(as.character(condition %||% ""))
      if (!nzchar(condition)) return(TRUE)

      norm <- tolower(condition)
      if (norm %in% c("true", "false")) return(identical(norm, "true"))

      key <- sub("^@", "", condition)
      if (is.list(self$conditional_metadata) && key %in% names(self$conditional_metadata)) {
        return(isTRUE(self$conditional_metadata[[key]]))
      }
      if (is.list(self$metadata) && key %in% names(self$metadata)) {
        return(isTRUE(self$metadata[[key]]))
      }
      FALSE
    },

    .should_apply_schema_row = function(row) {
      if (!is.data.frame(row) || nrow(row) == 0L || !"condition" %in% names(row)) {
        return(TRUE)
      }
      private$.resolve_condition_flag(row$condition[[1L]] %||% "")
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
