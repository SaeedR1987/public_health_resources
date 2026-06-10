      # ...existing code...
    }ame", "handling", "condition", "default_value", "function_name")
    },

    ..resolve_active_binding_name = function(name) {
      key <- trimws(as.character(name %||% ""))
      if (!nzchar(key)) return(NULL)
      bare <- sub("^\\.", "", key)
      candidates <- unique(c(key, bare, paste0(".", bare)))
      candidates <- candidates[candidates %in% names(self)]
      if (length(candidates) == 0L) return(NULL)
      candidates[[1L]]
    },

    ..read_active_binding = function(binding_name) {
      if (!is.character(binding_name) || length(binding_name) != 1L || !nzchar(binding_name)) {
        return(NULL)
      }
      value <- tryCatch(self[[binding_name]], error = function(e) NULL)
      if (is.function(value)) {
        return(tryCatch(value(), error = function(e) NULL))
      }
      value
    },

    ..insert_image_at_tag = function(doc, tag, image_value, schema_kind = c("docx", "pptx")) {
      schema_kind <- match.arg(schema_kind)
      if (!is.character(tag) || length(tag) != 1L || !nzchar(tag) || is.null(image_value)) {
        return(doc)
      }

      if (identical(schema_kind, "pptx")) {
        if (is.character(image_value) && length(image_value) == 1L && file.exists(image_value)) {
          return(tryCatch({
            officer::ph_with(
              x = doc,
              value = officer::external_img(src = image_value),
              location = officer::ph_location_label(ph_label = tag)
            )
          }, error = function(e) doc))
        }
        return(doc)
      }

      if (!(is.character(image_value) && length(image_value) == 1L && file.exists(image_value))) {
        return(private$..replace(doc, tag, "", schema_kind = schema_kind))
      }

      doc <- private$.._replace_across_runs(doc, tag, tag)
      tryCatch({
        doc <- officer::cursor_reach(doc, keyword = tag)
        doc <- officer::body_add_img(doc, src = image_value, pos = "before")
        doc <- officer::cursor_forward(doc)
        officer::body_remove(doc)
      }, error = function(e) private$..replace(doc, tag, "", schema_kind = schema_kind))
    },

    ..dispatch_schema_function = function(row, schema_kind = c("docx", "pptx")) {
      schema_kind <- match.arg(schema_kind)
      fn_name <- trimws(as.character(row[["function_name"]][[1L]] %||% ""))
      if (!nzchar(fn_name)) return(NULL)
      fn <- tryCatch(get(fn_name, mode = "function"), error = function(e) NULL)
      if (!is.function(fn)) return(NULL)
      args <- private$..schema_dispatch_args(fn_name = fn_name, row = row, schema_kind = schema_kind)
      tryCatch(do.call(fn, args), error = function(e) NULL)
    },

    #' @description Build argument lists for schema-dispatched helper functions.
    #' @param fn_name Function name from the schema \code{function_name} column.
    #' @param row Single-row schema data frame.
    #' @param schema_kind Document kind (\code{"docx"} or \code{"pptx"}).
    #' @return Named list of arguments for \code{do.call()}.
    ..schema_dispatch_args = function(fn_name, row, schema_kind = c("docx", "pptx")) {
      schema_kind <- match.arg(schema_kind)
      sample_table <- self$sample_table
      if (is.null(sample_table) && !is.null(self$sample_object) && inherits(self$sample_object, "Sample")) {
        sample_table <- tryCatch(self$access_nested(field = "sample_object", member = "get_sample_table"), error = function(e) NULL)
      }
      master_schema <- tryCatch(self$access_nested(field = "framework", member = "master_objectives_schema"), error = function(e) NULL)
      tool_names <- names(self$tools %||% list())
      tool_codes <- lapply(tool_names, function(tn) {
        tool <- self$tools[[tn]]
        if (!is.null(tool) && inherits(tool, "Tool")) {
          return(as.character(tool$get_indicator_codes(prefer_revised = TRUE)))
        }
        character(0)
      })
      names(tool_codes) <- tool_names
      all_tool_codes <- unique(unlist(tool_codes, use.names = FALSE))

      switch(
        fn_name,
        table_primary_data_sources = list(
          master_schema = master_schema,
          tool_indicator_codes = tool_codes
        ),
        table_secondary_data_sources = list(
          master_schema = master_schema,
          secondary_data = self$secondary_data %||% list()
        ),
        table_sample_size_general = list(
          sample_table = sample_table
        ),
        table_sample_size_individual = list(
          sample_table = sample_table,
          indicator_codes = all_tool_codes
        ),
        table_sample_size_mortality = list(
          sample_table = sample_table,
          indicator_codes = all_tool_codes
        ),
        list()
      )
    },

    ..schema_row = function(tag) {
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

    ..tag_is_missing_from_schema = function(tag) {
      startsWith(as.character(tag %||% ""), "@") && is.null(private$..schema_row(tag))
    },

    ..checkbox = function(doc, tag, condition) {
      if (private$..tag_is_missing_from_schema(tag)) {
        return(private$..replace(doc, tag, ""))
      }
      private$..replace(doc, tag, if (isTRUE(condition)) "X" else "\u25a1")
    },

    ..make_w_para = function(text, bold = FALSE, space_before_pt = 0L,
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

    ..replace_tag_in_cell = function(doc, tag, items) {
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
        node <- private$..make_w_para(
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

    ..remove_remaining_tags = function(doc) {
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

    ..resolve_condition_flag = function(condition) {
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

    ..should_apply_schema_row = function(row) {
      if (!is.data.frame(row) || nrow(row) == 0L) {
        return(TRUE)
      }
      if (!"condition" %in% names(row)) return(TRUE)
      condition <- trimws(as.character(row$condition[[1L]] %||% ""))
      if (!nzchar(condition)) return(TRUE)
      value <- private$..evaluate_condition_row(row)
      isTRUE(value)
    },

    .._replace_across_runs = function(doc, tag, new_val) {
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
