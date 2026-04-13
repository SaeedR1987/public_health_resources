#' Tool Selection Functions
#'
#' @description
#' Functions for selecting and managing data collection tools.

#' Create a tool definition
#'
#' @param tool_id Character. Unique identifier for the tool
#' @param tool_name Character. Human-readable name
#' @param sector Character. Sector (FSL, Health, Nutrition, WASH, etc.)
#' @param tool_type Character. Type: "household", "key_informant", "observation", "facility"
#' @param indicators Character vector. List of indicators collected by this tool
#' @param estimated_duration Numeric. Estimated duration in minutes (optional)
#' @param description Character. Tool description (optional)
#' @return List representing a tool
#' @export
create_tool <- function(tool_id,
                       tool_name,
                       sector,
                       tool_type,
                       indicators = NULL,
                       estimated_duration = NULL,
                       description = NULL) {
  
  # Validate tool_type
  valid_types <- c("household", "key_informant", "observation", "facility", "other")
  if (!tool_type %in% valid_types) {
    warning(paste("tool_type", tool_type, "not in standard list:", 
                 paste(valid_types, collapse = ", ")))
  }
  
  # Validate sector
  valid_sectors <- c("FSL", "Health", "Nutrition", "WASH", "Protection", 
                    "Education", "Shelter", "Multi-sector", "Other")
  if (!sector %in% valid_sectors) {
    warning(paste("Sector", sector, "not in standard list:", 
                 paste(valid_sectors, collapse = ", ")))
  }
  
  tool <- list(
    tool_id = tool_id,
    tool_name = tool_name,
    sector = sector,
    tool_type = tool_type,
    indicators = indicators,
    estimated_duration = estimated_duration,
    description = description,
    created_date = Sys.time()
  )
  
  class(tool) <- c("iphra_tool", "list")
  return(tool)
}

#' Create household survey tool
#'
#' @param tool_id Character. Unique identifier
#' @param tool_name Character. Tool name
#' @param sector Character. Primary sector
#' @param modules Character vector. Module names included
#' @param estimated_duration Numeric. Estimated duration in minutes
#' @return List representing household tool
#' @export
create_household_tool <- function(tool_id,
                                  tool_name,
                                  sector = "Multi-sector",
                                  modules = NULL,
                                  estimated_duration = 60) {
  
  tool <- create_tool(
    tool_id = tool_id,
    tool_name = tool_name,
    sector = sector,
    tool_type = "household",
    estimated_duration = estimated_duration,
    description = paste("Household survey with modules:", 
                       paste(modules, collapse = ", "))
  )
  
  tool$modules <- modules
  return(tool)
}

#' Create key informant interview tool
#'
#' @param tool_id Character. Unique identifier
#' @param tool_name Character. Tool name
#' @param sector Character. Primary sector
#' @param informant_type Character. Type of informant (community_leader, health_worker, etc.)
#' @param estimated_duration Numeric. Estimated duration in minutes
#' @return List representing KII tool
#' @export
create_kii_tool <- function(tool_id,
                            tool_name,
                            sector,
                            informant_type = NULL,
                            estimated_duration = 45) {
  
  tool <- create_tool(
    tool_id = tool_id,
    tool_name = tool_name,
    sector = sector,
    tool_type = "key_informant",
    estimated_duration = estimated_duration,
    description = paste("Key informant interview with", informant_type)
  )
  
  tool$informant_type <- informant_type
  return(tool)
}

#' Create observation tool
#'
#' @param tool_id Character. Unique identifier
#' @param tool_name Character. Tool name
#' @param sector Character. Primary sector
#' @param observation_target Character. What is being observed
#' @param estimated_duration Numeric. Estimated duration in minutes
#' @return List representing observation tool
#' @export
create_observation_tool <- function(tool_id,
                                   tool_name,
                                   sector,
                                   observation_target,
                                   estimated_duration = 30) {
  
  tool <- create_tool(
    tool_id = tool_id,
    tool_name = tool_name,
    sector = sector,
    tool_type = "observation",
    estimated_duration = estimated_duration,
    description = paste("Observation of", observation_target)
  )
  
  tool$observation_target <- observation_target
  return(tool)
}

#' Validate tools
#'
#' @param tools List of tool objects
#' @return List with validation results
#' @export
validate_tools <- function(tools) {
  
  if (!is.list(tools) || length(tools) == 0) {
    return(list(valid = FALSE, message = "Tools must be a non-empty list"))
  }
  
  issues <- list()
  
  # Check for duplicate IDs
  ids <- sapply(tools, function(x) x$tool_id)
  if (any(duplicated(ids))) {
    issues$duplicate_ids <- ids[duplicated(ids)]
  }
  
  # Check required fields
  required_fields <- c("tool_id", "tool_name", "sector", "tool_type")
  for (i in seq_along(tools)) {
    tool <- tools[[i]]
    missing <- setdiff(required_fields, names(tool))
    if (length(missing) > 0) {
      issues$missing_fields[[tool$tool_id]] <- missing
    }
  }
  
  # Check for tools without indicators
  for (i in seq_along(tools)) {
    tool <- tools[[i]]
    if (is.null(tool$indicators) || length(tool$indicators) == 0) {
      issues$missing_indicators <- c(issues$missing_indicators, tool$tool_id)
    }
  }
  
  if (length(issues) == 0) {
    return(list(valid = TRUE, message = "All tools are valid"))
  } else {
    return(list(valid = FALSE, issues = issues))
  }
}

#' Check tool-objective alignment
#'
#' @param tools List of tool objects
#' @param objectives List of objective objects
#' @return List with alignment check results
#' @export
check_tool_objective_alignment <- function(tools, objectives) {
  
  if (length(tools) == 0 || length(objectives) == 0) {
    return(list(
      valid = TRUE, 
      message = "Cannot check alignment with empty tools or objectives"
    ))
  }
  
  # Extract sectors from objectives
  obj_sectors <- unique(sapply(objectives, function(x) x$sector))
  
  # Extract sectors from tools
  tool_sectors <- unique(sapply(tools, function(x) x$sector))
  
  # Check coverage
  missing_sectors <- setdiff(obj_sectors, tool_sectors)
  extra_sectors <- setdiff(tool_sectors, obj_sectors)
  
  # Check indicator coverage if available
  obj_indicators <- unique(unlist(sapply(objectives, function(x) x$indicators)))
  tool_indicators <- unique(unlist(sapply(tools, function(x) x$indicators)))
  
  missing_indicators <- setdiff(obj_indicators[!is.na(obj_indicators)], 
                               tool_indicators[!is.na(tool_indicators)])
  
  issues <- list()
  warnings <- list()
  
  if (length(missing_sectors) > 0) {
    issues$missing_sectors <- missing_sectors
  }
  
  if (length(extra_sectors) > 0) {
    warnings$extra_sectors <- extra_sectors
  }
  
  if (length(missing_indicators) > 0 && length(obj_indicators) > 0) {
    warnings$missing_indicators <- missing_indicators
  }
  
  result <- list()
  result$valid <- length(issues) == 0
  
  if (length(issues) > 0) {
    result$issues <- issues
  }
  
  if (length(warnings) > 0) {
    result$warnings <- warnings
  }
  
  if (result$valid) {
    result$message <- "Tools align with objectives"
    if (length(warnings) > 0) {
      result$message <- paste(result$message, "(with warnings)")
    }
  } else {
    result$message <- "Tools do not fully cover objectives"
  }
  
  return(result)
}

#' Get tools by sector
#'
#' @param tools List of tools
#' @param sector Character. Sector to filter by
#' @return List of tools for the specified sector
#' @export
get_tools_by_sector <- function(tools, sector) {
  filtered <- Filter(function(x) x$sector == sector, tools)
  return(filtered)
}

#' Get tools by type
#'
#' @param tools List of tools
#' @param tool_type Character. Tool type to filter by
#' @return List of tools of the specified type
#' @export
get_tools_by_type <- function(tools, tool_type) {
  filtered <- Filter(function(x) x$tool_type == tool_type, tools)
  return(filtered)
}

#' Calculate total data collection time
#'
#' @param tools List of tools with estimated_duration
#' @param sample_size Integer. Total sample size
#' @param tools_per_sample Numeric vector. Number of times each tool is applied per sample unit
#' @return List with time estimates
#' @export
calculate_data_collection_time <- function(tools, sample_size, tools_per_sample = NULL) {
  
  if (is.null(tools_per_sample)) {
    tools_per_sample <- rep(1, length(tools))
  }
  
  if (length(tools_per_sample) != length(tools)) {
    stop("tools_per_sample must have same length as tools")
  }
  
  # Calculate total time for each tool
  tool_times <- sapply(seq_along(tools), function(i) {
    tool <- tools[[i]]
    duration <- if (is.null(tool$estimated_duration)) 60 else tool$estimated_duration
    duration * sample_size * tools_per_sample[i]
  })
  
  total_minutes <- sum(tool_times)
  
  return(list(
    total_minutes = total_minutes,
    total_hours = total_minutes / 60,
    total_days = total_minutes / (60 * 8),  # Assuming 8-hour work day
    by_tool = data.frame(
      tool_id = sapply(tools, function(x) x$tool_id),
      tool_name = sapply(tools, function(x) x$tool_name),
      duration_minutes = sapply(tools, function(x) 
        if (is.null(x$estimated_duration)) 60 else x$estimated_duration),
      applications = tools_per_sample,
      total_minutes = tool_times,
      stringsAsFactors = FALSE
    )
  ))
}

#' Convert tools to data frame
#'
#' @param tools List of tools
#' @return Data frame with tool information
#' @export
tools_to_df <- function(tools) {
  if (length(tools) == 0) {
    return(data.frame())
  }
  
  df <- data.frame(
    tool_id = sapply(tools, function(x) x$tool_id),
    tool_name = sapply(tools, function(x) x$tool_name),
    sector = sapply(tools, function(x) x$sector),
    tool_type = sapply(tools, function(x) x$tool_type),
    estimated_duration = sapply(tools, function(x) 
      if (is.null(x$estimated_duration)) NA else x$estimated_duration),
    num_indicators = sapply(tools, function(x) length(x$indicators)),
    stringsAsFactors = FALSE
  )
  
  return(df)
}

#' Print tools summary
#'
#' @param tools List of tools
#' @export
print_tools_summary <- function(tools) {
  if (length(tools) == 0) {
    cat("No tools defined\n")
    return(invisible(NULL))
  }
  
  cat("Tools Summary\n")
  cat("=============\n\n")
  cat("Total tools:", length(tools), "\n\n")
  
  # By type
  types <- unique(sapply(tools, function(x) x$tool_type))
  cat("By Type:\n")
  for (type in types) {
    type_tools <- get_tools_by_type(tools, type)
    cat("", type, ":", length(type_tools), "\n")
  }
  
  cat("\nBy Sector:\n")
  sectors <- unique(sapply(tools, function(x) x$sector))
  for (sector in sectors) {
    sector_tools <- get_tools_by_sector(tools, sector)
    cat("", sector, ":", length(sector_tools), "\n")
  }
  
  # Total estimated duration
  total_duration <- sum(sapply(tools, function(x) 
    if (is.null(x$estimated_duration)) 0 else x$estimated_duration))
  
  if (total_duration > 0) {
    cat("\nTotal estimated duration:", round(total_duration, 1), "minutes\n")
    cat("Average per tool:", round(total_duration / length(tools), 1), "minutes\n")
  }
}
