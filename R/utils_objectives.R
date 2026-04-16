#' Objectives Management Functions
#'
#' @description
#' Functions for creating and managing research objectives in the protocol pipeline.

#' Create an objective
#'
#' @param objective_id Character. Unique identifier for the objective
#' @param objective_text Character. Description of the objective
#' @param data_type Character. Either "primary" or "secondary"
#' @param sector Character. Sector (FSL, Health, Nutrition, WASH, Protection, Education, Shelter)
#' @param indicators Character vector. List of indicator IDs related to this objective
#' @param rationale Character. Rationale for the objective (optional)
#' @return List representing an objective
#' @export
create_objective <- function(objective_id,
                             objective_text,
                             data_type,
                             sector,
                             indicators = NULL,
                             rationale = NULL) {
  
  # Validate data_type
  if (!data_type %in% c("primary", "secondary")) {
    stop("data_type must be 'primary' or 'secondary'")
  }
  
  # Validate sector
  valid_sectors <- c("FSL", "Health", "Nutrition", "WASH", "Protection", 
                    "Education", "Shelter", "Multi-sector", "Other")
  if (!sector %in% valid_sectors) {
    warning(paste("Sector", sector, "is not in standard list:", 
                 paste(valid_sectors, collapse = ", ")))
  }
  
  objective <- list(
    objective_id = objective_id,
    objective_text = objective_text,
    data_type = data_type,
    sector = sector,
    indicators = indicators,
    rationale = rationale,
    created_date = Sys.time()
  )
  
  return(objective)
}

#' Create multiple objectives
#'
#' @param objectives_df Data frame with columns: objective_id, objective_text, 
#'                      data_type, sector, indicators (optional), rationale (optional)
#' @return List of objectives
#' @export
create_objectives_from_df <- function(objectives_df) {
  if (!is.data.frame(objectives_df)) {
    stop("objectives_df must be a data frame")
  }
  
  required_cols <- c("objective_id", "objective_text", "data_type", "sector")
  missing_cols <- setdiff(required_cols, names(objectives_df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  objectives_list <- list()
  
  for (i in seq_len(nrow(objectives_df))) {
    row <- objectives_df[i, ]
    
    indicators <- if ("indicators" %in% names(row) && !is.na(row$indicators)) {
      strsplit(as.character(row$indicators), ",")[[1]]
    } else {
      NULL
    }
    
    rationale <- if ("rationale" %in% names(row) && !is.na(row$rationale)) {
      as.character(row$rationale)
    } else {
      NULL
    }
    
    obj <- create_objective(
      objective_id = as.character(row$objective_id),
      objective_text = as.character(row$objective_text),
      data_type = as.character(row$data_type),
      sector = as.character(row$sector),
      indicators = indicators,
      rationale = rationale
    )
    
    objectives_list[[length(objectives_list) + 1]] <- obj
  }
  
  return(objectives_list)
}

#' Validate objectives
#'
#' @param objectives List of objective objects
#' @return List with validation results
#' @export
validate_objectives <- function(objectives) {
  if (!is.list(objectives) || length(objectives) == 0) {
    return(list(valid = FALSE, message = "Objectives must be a non-empty list"))
  }
  
  issues <- list()
  
  # Check for duplicate IDs
  ids <- sapply(objectives, function(x) x$objective_id)
  if (any(duplicated(ids))) {
    issues$duplicate_ids <- ids[duplicated(ids)]
  }
  
  # Check for vague objectives (very short text)
  for (i in seq_along(objectives)) {
    obj <- objectives[[i]]
    if (nchar(obj$objective_text) < 20) {
      issues$vague_objectives <- c(issues$vague_objectives, obj$objective_id)
    }
  }
  
  # Check for objectives without indicators
  for (i in seq_along(objectives)) {
    obj <- objectives[[i]]
    if (is.null(obj$indicators) || length(obj$indicators) == 0) {
      issues$missing_indicators <- c(issues$missing_indicators, obj$objective_id)
    }
  }
  
  # Check data type
  for (i in seq_along(objectives)) {
    obj <- objectives[[i]]
    if (!obj$data_type %in% c("primary", "secondary")) {
      issues$invalid_data_type <- c(issues$invalid_data_type, obj$objective_id)
    }
  }
  
  if (length(issues) == 0) {
    return(list(valid = TRUE, message = "All objectives are valid"))
  } else {
    return(list(valid = FALSE, issues = issues))
  }
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

#' Get objectives by data type
#'
#' @param objectives List of objectives
#' @param data_type Character. Either "primary" or "secondary"
#' @return List of objectives for the specified data type
#' @export
get_objectives_by_data_type <- function(objectives, data_type) {
  if (!data_type %in% c("primary", "secondary")) {
    stop("data_type must be 'primary' or 'secondary'")
  }
  
  filtered <- Filter(function(x) x$data_type == data_type, objectives)
  return(filtered)
}

#' Convert objectives to data frame
#'
#' @param objectives List of objectives
#' @return Data frame with objective information
#' @export
objectives_to_df <- function(objectives) {
  if (length(objectives) == 0) {
    return(data.frame())
  }
  
  df <- data.frame(
    objective_id = sapply(objectives, function(x) x$objective_id),
    objective_text = sapply(objectives, function(x) x$objective_text),
    data_type = sapply(objectives, function(x) x$data_type),
    sector = sapply(objectives, function(x) x$sector),
    num_indicators = sapply(objectives, function(x) length(x$indicators)),
    stringsAsFactors = FALSE
  )
  
  return(df)
}

#' Print objectives summary
#'
#' @param objectives List of objectives
#' @export
print_objectives_summary <- function(objectives) {
  if (length(objectives) == 0) {
    cat("No objectives defined\n")
    return(invisible(NULL))
  }
  
  cat("Objectives Summary\n")
  cat("==================\n\n")
  cat("Total objectives:", length(objectives), "\n\n")
  
  # By data type
  primary <- get_objectives_by_data_type(objectives, "primary")
  secondary <- get_objectives_by_data_type(objectives, "secondary")
  cat("Primary data objectives:", length(primary), "\n")
  cat("Secondary data objectives:", length(secondary), "\n\n")
  
  # By sector
  sectors <- unique(sapply(objectives, function(x) x$sector))
  cat("Sectors covered:", paste(sectors, collapse = ", "), "\n\n")
  
  for (sector in sectors) {
    sector_objs <- get_objectives_by_sector(objectives, sector)
    cat("", sector, ":", length(sector_objs), "objective(s)\n")
  }
}
