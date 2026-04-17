#' Objectives Management Functions
#'
#' @description
#' Functions for creating and managing research objectives in the protocol pipeline.

#' Create an objective
#'
#' Creates an objective using the standard fields from the reference schema:
#' sector, pillar, sub_pillar, short_objective, text_objective, and data_source.
#'
#' @param sector Character. Sector (e.g. "General", "FSL", "Health", "Nutrition", "WASH").
#' @param pillar Character. Pillar within the sector (e.g. "Demographics").
#' @param sub_pillar Character. Sub-pillar within the pillar.
#' @param short_objective Character. Short label for the objective (should be unique).
#' @param text_objective Character. Full descriptive text of the objective.
#' @param data_source Character. Data source for the objective (optional, default NA).
#' @return Named list representing an objective.
#' @export
create_objective <- function(sector,
                             pillar,
                             sub_pillar,
                             short_objective,
                             text_objective,
                             data_source = NA_character_) {

  origin <- "create_objective"

  phr_try({

    required <- list(
      sector          = sector,
      pillar          = pillar,
      sub_pillar      = sub_pillar,
      short_objective = short_objective,
      text_objective  = text_objective
    )

    for (nm in names(required)) {
      val <- required[[nm]]
      phr_assert(
        !is.null(val) && !(length(val) == 1L && is.na(val)) && nzchar(as.character(val)),
        message = phr_txt("'{nm}' is a required field and must be a non-empty string."),
        origin  = origin
      )
    }

    list(
      sector          = as.character(sector),
      pillar          = as.character(pillar),
      sub_pillar      = as.character(sub_pillar),
      short_objective = as.character(short_objective),
      text_objective  = as.character(text_objective),
      data_source     = if (is.null(data_source)) NA_character_ else as.character(data_source),
      created_date    = Sys.time()
    )

  }, on_error = "abort", origin = origin)
}

#' Create multiple objectives from a data frame
#'
#' @param objectives_df Data frame with columns: sector, pillar, sub_pillar,
#'   short_objective, text_objective, and optionally data_source.
#' @return List of objective objects.
#' @export
create_objectives_from_df <- function(objectives_df) {

  origin <- "create_objectives_from_df"

  phr_try({

    phr_validate_dataframe(objectives_df, origin = origin, soft = FALSE)

    required_cols <- c("sector", "pillar", "sub_pillar", "short_objective", "text_objective")
    phr_validate_columns(objectives_df, required_cols, origin = origin, soft = FALSE)

    objectives_list <- list()

    for (i in seq_len(nrow(objectives_df))) {
      row <- objectives_df[i, ]

      data_source <- if ("data_source" %in% names(row) && !is.na(row$data_source)) {
        as.character(row$data_source)
      } else {
        NA_character_
      }

      obj <- create_objective(
        sector          = as.character(row$sector),
        pillar          = as.character(row$pillar),
        sub_pillar      = as.character(row$sub_pillar),
        short_objective = as.character(row$short_objective),
        text_objective  = as.character(row$text_objective),
        data_source     = data_source
      )

      objectives_list[[length(objectives_list) + 1]] <- obj
    }

    objectives_list

  }, on_error = "abort", origin = origin)
}

#' Validate objectives
#'
#' @param objectives List of objective objects
#' @return List with validation results
#' @export
validate_objectives <- function(objectives) {

  origin <- "validate_objectives"

  phr_try({

    phr_assert(
      is.list(objectives) && length(objectives) > 0,
      message = phr_txt("Objectives must be a non-empty list."),
      origin  = origin
    )

    issues <- list()

    required_fields <- c("sector", "pillar", "sub_pillar", "short_objective", "text_objective")

    # Check for duplicate short_objective labels
    short_objs <- sapply(objectives, function(x) x$short_objective)
    if (any(duplicated(short_objs))) {
      issues$duplicate_short_objectives <- short_objs[duplicated(short_objs)]
      phr_warning(
        message = phr_txt("Duplicate short_objective labels found: {paste(issues$duplicate_short_objectives, collapse=', ')}"),
        origin  = origin,
        hint    = phr_txt("Each objective should have a unique short_objective label.")
      )
    }

    # Check for missing required fields
    for (i in seq_along(objectives)) {
      obj <- objectives[[i]]
      missing <- setdiff(required_fields, names(obj))
      if (length(missing) > 0) {
        issues$missing_fields <- c(issues$missing_fields,
                                    paste0("objective[", i, "]: ", paste(missing, collapse = ", ")))
      }
    }

    # Check for vague objectives (very short text_objective)
    for (i in seq_along(objectives)) {
      obj <- objectives[[i]]
      if (!is.null(obj$text_objective) && nchar(obj$text_objective) < 20) {
        issues$vague_objectives <- c(issues$vague_objectives, obj$short_objective)
        phr_warning(
          message = phr_txt("Objective '{obj$short_objective}' has a very short text_objective (< 20 characters)."),
          origin  = origin,
          hint    = phr_txt("Consider expanding the text_objective for clarity.")
        )
      }
    }

    if (length(issues) == 0) {
      list(valid = TRUE, message = phr_txt("All objectives are valid."))
    } else {
      list(valid = FALSE, issues = issues)
    }

  }, on_error = "abort", origin = origin)
}

#' Get objectives by sector
#'
#' @param objectives List of objectives
#' @param sector Character. Sector to filter by
#' @return List of objectives for the specified sector
#' @export
get_objectives_by_sector <- function(objectives, sector) {
  filtered <- Filter(function(x) x$sector == sector, objectives)
  return(filtered)
}

#' Convert objectives to data frame
#'
#' @param objectives List of objectives
#' @return Data frame with objective information
#' @export
objectives_to_df <- function(objectives) {

  origin <- "objectives_to_df"

  phr_try({

    phr_assert(
      is.list(objectives),
      message = phr_txt("objectives must be a list."),
      origin  = origin
    )

    if (length(objectives) == 0) {
      return(data.frame())
    }

    data.frame(
      sector          = sapply(objectives, function(x) x$sector),
      pillar          = sapply(objectives, function(x) x$pillar),
      sub_pillar      = sapply(objectives, function(x) x$sub_pillar),
      short_objective = sapply(objectives, function(x) x$short_objective),
      text_objective  = sapply(objectives, function(x) x$text_objective),
      data_source     = sapply(objectives, function(x) if (!is.null(x$data_source)) x$data_source else NA_character_),
      stringsAsFactors = FALSE
    )

  }, on_error = "abort", origin = origin)
}

#' Print objectives summary
#'
#' @param objectives List of objectives
#' @export
print_objectives_summary <- function(objectives) {

  if (length(objectives) == 0) {
    phr_message(phr_txt("No objectives defined."), origin = "print_objectives_summary")
    return(invisible(NULL))
  }

  sectors <- unique(sapply(objectives, function(x) x$sector))

  phr_message(phr_txt("Objectives Summary — {length(objectives)} total objective(s). Sectors: {paste(sectors, collapse=', ')}"),
              origin = "print_objectives_summary")

  for (sector in sectors) {
    sector_objs <- get_objectives_by_sector(objectives, sector)
    phr_message(phr_txt("{sector}: {length(sector_objs)} objective(s)"),
                origin = "print_objectives_summary")
  }
}
