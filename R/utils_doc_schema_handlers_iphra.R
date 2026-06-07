#' IPHRA document schema handlers
#'
#' @description
#' Standalone helpers used by \code{Document$handle_table()} and
#' \code{Document$handle_image()} through the \code{function_name} column in
#' protocol schema files.

.phr_doc_replace_tag <- function(doc, tag, value = "") {
  tryCatch(
    officer::body_replace_all_text(doc, old_value = tag, new_value = as.character(value), only_at_cursor = FALSE),
    error = function(e) doc
  )
}

.phr_doc_get_tool_indicator_codes <- function(self, tool_names = NULL, prefer_revised = TRUE) {
  if (is.null(self$tools) || length(self$tools) == 0L) return(character(0))
  names_to_use <- names(self$tools)
  if (!is.null(tool_names)) names_to_use <- intersect(names_to_use, as.character(tool_names))
  out <- character(0)
  for (tn in names_to_use) {
    tool <- self$tools[[tn]]
    if (!is.null(tool) && inherits(tool, "Tool")) {
      out <- c(out, as.character(tool$get_indicator_codes(prefer_revised = prefer_revised)))
    }
  }
  unique(out[!is.na(out) & nzchar(out)])
}

phr_doc_add_primary_data_sources_table <- function(self, doc, row = NULL) {
  tool_names <- names(self$tools %||% list())
  tag <- "@primary_data_sources_table"
  if (length(tool_names) == 0) return(.phr_doc_replace_tag(doc, tag, ""))

  master <- self$access_nested("framework", member = "master_objectives_schema")
  if (!is.data.frame(master) || nrow(master) == 0 ||
      !all(c("objective_code", "indicator_code", "text_objective") %in% names(master))) {
    return(.phr_doc_replace_tag(doc, tag, ""))
  }

  tool_codes <- lapply(tool_names, function(tn) .phr_doc_get_tool_indicator_codes(self, tool_names = tn))
  names(tool_codes) <- tool_names
  all_codes <- unique(unlist(tool_codes))
  if (length(all_codes) == 0) return(.phr_doc_replace_tag(doc, tag, ""))

  obj_sub <- unique(master[as.character(master$indicator_code) %in% all_codes,
                           c("objective_code", "text_objective"), drop = FALSE])
  obj_sub <- obj_sub[!is.na(obj_sub$objective_code), , drop = FALSE]
  if (nrow(obj_sub) == 0) return(.phr_doc_replace_tag(doc, tag, ""))
  obj_sub <- obj_sub[order(obj_sub$objective_code), , drop = FALSE]

  tool_labels <- gsub("^tool_", "", tool_names)
  tool_labels <- gsub("_iphra_v[0-9]+$", "", tool_labels)
  mat <- as.data.frame(matrix("", nrow = nrow(obj_sub), ncol = length(tool_names)), stringsAsFactors = FALSE)
  names(mat) <- tool_labels

  for (j in seq_along(tool_names)) {
    tc <- tool_codes[[tool_names[j]]]
    obj_covered <- unique(as.character(master$objective_code[as.character(master$indicator_code) %in% tc]))
    mat[[j]] <- ifelse(obj_sub$objective_code %in% obj_covered, "X", "")
  }

  result_df <- cbind(data.frame(Objective = obj_sub$text_objective, stringsAsFactors = FALSE), mat)
  ft <- flextable::theme_zebra(flextable::flextable(result_df))
  n_tool_cols <- length(tool_names)
  ft <- flextable::width(ft, j = 1, width = 3.25)
  if (n_tool_cols > 0) ft <- flextable::width(ft, j = seq(2, n_tool_cols + 1), width = 3.25 / n_tool_cols)
  ft <- flextable::fontsize(ft, size = 7, part = "all")
  ft <- flextable::set_table_properties(ft, layout = "fixed")

  tryCatch({
    doc <- officer::cursor_reach(doc, keyword = tag)
    doc <- flextable::body_add_flextable(doc, ft, pos = "before")
    doc <- officer::cursor_forward(doc)
    officer::body_remove(doc)
  }, error = function(e) .phr_doc_replace_tag(doc, tag, ""))
}

phr_doc_add_framework_image <- function(self, doc, row = NULL) {
  tag <- "@modified_framework_svg"
  svg_content <- tryCatch(self$access_nested("framework", member = "adjusted_svg"), error = function(e) NULL)
  if (is.null(svg_content) || !nzchar(svg_content)) {
    svg_content <- tryCatch(self$access_nested("framework", member = "master_svg"), error = function(e) NULL)
  }
  if (is.null(svg_content) || !nzchar(svg_content)) return(.phr_doc_replace_tag(doc, tag, ""))

  tmp_svg <- tempfile(fileext = ".svg")
  tmp_png <- tempfile(fileext = ".png")
  writeLines(svg_content, tmp_svg)
  png_inserted <- FALSE
  if (requireNamespace("magick", quietly = TRUE)) {
    try({
      magick::image_write(magick::image_read_svg(tmp_svg, density = 300), tmp_png, format = "png")
      doc <- officer::cursor_reach(doc, keyword = tag)
      doc <- officer::body_add_img(doc, src = tmp_png, width = 6.5, height = 4.5, pos = "before")
      doc <- officer::cursor_forward(doc)
      doc <- officer::body_remove(doc)
      png_inserted <- TRUE
    }, silent = TRUE)
  }
  if (!png_inserted && requireNamespace("rsvg", quietly = TRUE)) {
    try({
      rsvg::rsvg_png(tmp_svg, tmp_png, width = 3000)
      doc <- officer::cursor_reach(doc, keyword = tag)
      doc <- officer::body_add_img(doc, src = tmp_png, width = 6.5, height = 4.5, pos = "before")
      doc <- officer::cursor_forward(doc)
      doc <- officer::body_remove(doc)
      png_inserted <- TRUE
    }, silent = TRUE)
  }
  if (!png_inserted) doc <- .phr_doc_replace_tag(doc, tag, "[Framework diagram — attach SVG manually]")
  doc
}

phr_doc_add_secondary_data_sources_table <- function(self, doc, row = NULL) {
  tag <- "@secondary_data_sources_table"
  sdr <- self$secondary_data
  master <- self$access_nested("framework", member = "master_objectives_schema")
  if (!is.null(sdr) && length(sdr) > 0) {
    obj_labels <- setNames(vapply(names(sdr), function(code) {
      if (is.data.frame(master) && all(c("objective_code", "text_objective") %in% names(master))) {
        idx <- which(as.character(master$objective_code) == code)
        if (length(idx) > 0) return(as.character(master$text_objective[idx[1]]))
      }
      code
    }, character(1)), names(sdr))
    sdr_df <- data.frame(Objective = unname(obj_labels[names(sdr)]), Source = as.character(unname(sdr)),
                         Purpose = "", stringsAsFactors = FALSE)
  } else {
    sdr_df <- data.frame(Objective = "", Source = "", Purpose = "", stringsAsFactors = FALSE)
  }
  ft <- flextable::theme_zebra(flextable::flextable(sdr_df))
  if (!is.null(sdr) && length(sdr) > 0) ft <- flextable::merge_v(ft, j = "Objective")
  ft <- flextable::width(ft, j = 1, width = 2.6)
  ft <- flextable::width(ft, j = 2, width = 1.95)
  ft <- flextable::width(ft, j = 3, width = 1.95)
  ft <- flextable::fontsize(ft, size = 7, part = "all")
  ft <- flextable::set_table_properties(ft, layout = "fixed")
  tryCatch({
    doc <- officer::cursor_reach(doc, keyword = tag)
    doc <- flextable::body_add_flextable(doc, ft, pos = "before")
    doc <- officer::cursor_forward(doc)
    officer::body_remove(doc)
  }, error = function(e) .phr_doc_replace_tag(doc, tag, ""))
}

phr_doc_build_sample_size_table <- function(self, doc, tag, param_rows) {
  st <- self$access_nested(field = "sample_table", member = "get_sample_table")
  if (is.null(st) || nrow(st) == 0L) return(.phr_doc_replace_tag(doc, tag, ""))
  strata_names <- if ("stratum_name" %in% names(st)) as.character(st$stratum_name) else as.character(st$stratum_id)
  n_strata <- length(strata_names)
  col_names <- c("Parameter", strata_names, "Justification")
  rows_list <- lapply(param_rows, function(pr) {
    vals <- vapply(seq_len(n_strata), function(j) tryCatch(pr$col_fn(st[j, , drop = FALSE]), error = function(e) ""), character(1L))
    as.list(c(pr$label, vals, ""))
  })
  mat <- do.call(rbind, lapply(rows_list, function(r) as.data.frame(r, stringsAsFactors = FALSE, col.names = col_names)))
  names(mat) <- col_names
  total_labels <- c("Households to be Included", "Individuals to be Included", "Population to be Included", "Person-Time to be Included")
  total_row_idx <- which(mat[["Parameter"]] %in% total_labels)
  reach1_bg <- tryCatch(get_color_palette("reach1")[[1L]], error = function(e) "#EE5859")
  fp_col <- officer::fp_border(color = "black", width = 1)
  fp_outer <- officer::fp_border(color = "black", width = 1.5)
  strata_col_w <- 6.5 / (6 + n_strata); param_col_w <- strata_col_w * 3; just_col_w <- strata_col_w * 3
  ft <- flextable::flextable(mat)
  ft <- flextable::fontsize(ft, size = 8, part = "all")
  ft <- flextable::width(ft, j = 1L, width = param_col_w)
  ft <- flextable::width(ft, j = seq(2L, n_strata + 1L), width = strata_col_w)
  ft <- flextable::width(ft, j = n_strata + 2L, width = just_col_w)
  ft <- flextable::set_table_properties(ft, layout = "fixed")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bg(ft, bg = reach1_bg, part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bg(ft, bg = "white", part = "body")
  if (length(total_row_idx) > 0L) ft <- flextable::bg(ft, i = total_row_idx, bg = "#D3D3D3", part = "body")
  ft <- flextable::border_remove(ft)
  ft <- flextable::vline(ft, border = fp_col, part = "body")
  ft <- flextable::vline(ft, border = fp_col, part = "header")
  ft <- flextable::border_outer(ft, border = fp_outer, part = "all")
  ft <- flextable::hline_bottom(ft, border = fp_outer, part = "header")
  tryCatch({
    doc <- officer::cursor_reach(doc, keyword = tag)
    doc <- flextable::body_add_flextable(doc, ft, pos = "before")
    doc <- officer::cursor_forward(doc)
    officer::body_remove(doc)
  }, error = function(e) .phr_doc_replace_tag(doc, tag, ""))
}

phr_doc_add_sample_size_gen_table <- function(self, doc, row = NULL) {
  params <- list(
    list(label = "Indicator Name", col_fn = function(r) as.character(r$pop_indicator %||% "")),
    list(label = "Sampling Design", col_fn = function(r) phr_fmt_sampling_method(r$sampling_method %||% "")),
    list(label = "Estimated Prevalence (%)", col_fn = function(r) phr_fmt_pct(r$pop_expected_prevalence)),
    list(label = "Desired Precision", col_fn = function(r) phr_fmt_pct(r$pop_precision)),
    list(label = "Estimated population size", col_fn = function(r) phr_fmt_n(r$total_population)),
    list(label = "Design Effect", col_fn = function(r) if (is.null(r$pop_design_effect) || is.na(r$pop_design_effect)) "" else as.character(r$pop_design_effect)),
    list(label = "Finite Population Correction (FPC) used?", col_fn = function(r) phr_fmt_fpc(r$pop_fpc)),
    list(label = "Non-Response Rate", col_fn = function(r) phr_fmt_pct(r$pop_nonresponse)),
    list(label = "Households to be Included", col_fn = function(r) phr_fmt_n(r$General_HH_Sample_Size))
  )
  phr_doc_build_sample_size_table(self, doc, "@sample_size_hh_gen_table", params)
}

phr_doc_add_sample_size_ind_table <- function(self, doc, row = NULL) {
  inc_codes <- as.character(.phr_doc_get_tool_indicator_codes(self))
  if (!any(c("10701", "10702") %in% inc_codes)) return(.phr_doc_replace_tag(doc, "@sample_size_hh_ind_table", ""))
  params <- list(
    list(label = "Indicator Name", col_fn = function(r) as.character(r$ind_indicator %||% "")),
    list(label = "Sampling Design", col_fn = function(r) phr_fmt_sampling_method(r$sampling_method %||% "")),
    list(label = "Estimated Prevalence (%)", col_fn = function(r) phr_fmt_pct(r$ind_expected_prevalence)),
    list(label = "Desired Precision", col_fn = function(r) phr_fmt_pct(r$ind_precision)),
    list(label = "Estimated population size", col_fn = function(r) phr_fmt_n(r$total_population)),
    list(label = "Design Effect", col_fn = function(r) if (is.null(r$ind_design_effect) || is.na(r$ind_design_effect)) "" else as.character(r$ind_design_effect)),
    list(label = "Finite Population Correction (FPC) used?", col_fn = function(r) phr_fmt_fpc(r$ind_fpc)),
    list(label = "Individuals to be Included", col_fn = function(r) phr_fmt_n(r$Ind_Sample_Size)),
    list(label = "Average Household Size", col_fn = function(r) if (is.null(r$ind_avg_hh_size) || is.na(r$ind_avg_hh_size)) "" else as.character(r$ind_avg_hh_size)),
    list(label = "% sub-population", col_fn = function(r) phr_fmt_pct(r$ind_subpop_prop)),
    list(label = "Non-Response Rate", col_fn = function(r) phr_fmt_pct(r$ind_nonresponse)),
    list(label = "Households to be Included", col_fn = function(r) phr_fmt_n(r$Ind_HH_Sample_Size))
  )
  phr_doc_build_sample_size_table(self, doc, "@sample_size_hh_ind_table", params)
}

phr_doc_add_sample_size_mort_table <- function(self, doc, row = NULL) {
  inc_codes <- as.character(.phr_doc_get_tool_indicator_codes(self))
  if (!any(c("10501", "10502") %in% inc_codes)) return(.phr_doc_replace_tag(doc, "@sample_size_hh_mort_table", ""))
  params <- list(
    list(label = "Indicator Name", col_fn = function(r) as.character(r$mort_indicator %||% "")),
    list(label = "Sampling Design", col_fn = function(r) phr_fmt_sampling_method(r$sampling_method %||% "")),
    list(label = "Estimated death rate per 10,000/day", col_fn = function(r) if (is.null(r$mort_expected_death_rate) || is.na(r$mort_expected_death_rate)) "" else as.character(r$mort_expected_death_rate)),
    list(label = "Desired Precision", col_fn = function(r) if (is.null(r$mort_precision) || is.na(r$mort_precision)) "" else as.character(r$mort_precision)),
    list(label = "Recall Period", col_fn = function(r) if (is.null(r$mort_recall_days) || is.na(r$mort_recall_days)) "" else as.character(r$mort_recall_days)),
    list(label = "Population size (overall)", col_fn = function(r) phr_fmt_n(r$total_population)),
    list(label = "Design Effect", col_fn = function(r) if (is.null(r$mort_design_effect) || is.na(r$mort_design_effect)) "" else as.character(r$mort_design_effect)),
    list(label = "Finite Population Correction (FPC) used?", col_fn = function(r) phr_fmt_fpc(r$mort_fpc)),
    list(label = "Population to be Included", col_fn = function(r) phr_fmt_n(r$Mort_Ind_Sample_Size, "people")),
    list(label = "Person-Time to be Included", col_fn = function(r) phr_fmt_n(r$Mort_PT_Sample_Size, "person days")),
    list(label = "Average Household Size", col_fn = function(r) if (is.null(r$mort_avg_hh_size) || is.na(r$mort_avg_hh_size)) "" else as.character(r$mort_avg_hh_size)),
    list(label = "% Non-Respondents", col_fn = function(r) phr_fmt_pct(r$mort_nonresponse)),
    list(label = "Households to be Included", col_fn = function(r) phr_fmt_n(r$Mort_HH_Sample_Size, "households"))
  )
  phr_doc_build_sample_size_table(self, doc, "@sample_size_hh_mort_table", params)
}
