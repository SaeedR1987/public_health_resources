#' Protocol R6 Class
#'
#' @description
#' Main class for managing the complete protocols pipeline workflow.
#' Allows flexible interaction with different workflow components:
#' 1. Objective Selection (Primary and Secondary)
#' 2. Strata Definition and Sample Size Calculations
#' 3. Sampling Frame Validation and Sample Drawing
#' 4. Tool and Indicator Selection
#'
#' @importFrom R6 R6Class
#' @export
Protocol <- R6::R6Class(
  "Protocol",
  public = list(
    #' @field primary_objectives List of primary research objectives
    primary_objectives = NULL,
    
    #' @field secondary_objectives List of secondary research objectives
    secondary_objectives = NULL,
    
    #' @field sample_table Data frame with rows=strata, columns=parameters
    sample_table = NULL,
    
    #' @field sampling_frame Data frame with sampling units and strata
    sampling_frame = NULL,
    
    #' @field drawn_sample List containing drawn sample and metadata
    drawn_sample = NULL,
    
    #' @field tools List of Tool objects (placeholder for Tool class instances)
    tools = NULL,
    
    #' @field selected_indicators List of selected indicators
    selected_indicators = NULL,
    
    #' @field issues List of validation issues and discrepancies
    issues = list(),
    
    #' @field metadata List containing protocol metadata
    metadata = list(
      created_date = NULL,
      modified_date = NULL,
      month_year = NULL,
      country_name = NULL,
      assessment_title = NULL,
      target_strata = list(),
      protocol_version = "1.0"
    ),
    
    #' @description Initialize a new Protocol
    #' @param assessment_title Character. Title of the assessment
    #' @param country_name Character. Country where assessment takes place
    #' @param month_year Character. Month and year of data collection (e.g., "January 2024")
    #' @return A new Protocol object
    initialize = function(assessment_title = NULL, country_name = NULL, month_year = NULL) {
      self$metadata$created_date <- Sys.time()
      self$metadata$modified_date <- Sys.time()
      self$metadata$assessment_title <- assessment_title
      self$metadata$country_name <- country_name
      self$metadata$month_year <- month_year
      self$primary_objectives <- list()
      self$secondary_objectives <- list()
      self$tools <- list()
      self$issues <- list()
      invisible(self)
    },
    
    #' @description Set primary research objectives
    #' @param objectives List of primary objectives
    set_primary_objectives = function(objectives) {
      if (!is.list(objectives)) {
        stop("Objectives must be a list")
      }
      
      # Validate objectives structure
      required_fields <- c("objective_id", "objective_text", "sector")
      for (obj in objectives) {
        missing <- setdiff(required_fields, names(obj))
        if (length(missing) > 0) {
          stop(paste("Objective missing required fields:", paste(missing, collapse = ", ")))
        }
      }
      
      self$primary_objectives <- objectives
      self$metadata$modified_date <- Sys.time()
      private$check_issues()
      invisible(self)
    },
    
    #' @description Set secondary research objectives
    #' @param objectives List of secondary objectives
    set_secondary_objectives = function(objectives) {
      if (!is.list(objectives)) {
        stop("Objectives must be a list")
      }
      
      # Validate objectives structure
      required_fields <- c("objective_id", "objective_text", "sector")
      for (obj in objectives) {
        missing <- setdiff(required_fields, names(obj))
        if (length(missing) > 0) {
          stop(paste("Objective missing required fields:", paste(missing, collapse = ", ")))
        }
      }
      
      self$secondary_objectives <- objectives
      self$metadata$modified_date <- Sys.time()
      private$check_issues()
      invisible(self)
    },
    
    #' @description Add a target stratum to metadata
    #' @param stratum_id Character. Unique identifier
    #' @param stratum_name Character. Human-readable name
    add_target_stratum = function(stratum_id, stratum_name) {
      self$metadata$target_strata[[stratum_id]] <- stratum_name
      self$metadata$modified_date <- Sys.time()
      invisible(self)
    },
    
    #' @description Add a stratum to the sample table
    #' @param stratum_id Character. Unique identifier for the stratum
    #' @param stratum_name Character. Human-readable name
    #' @param population_size Numeric. Population size for this stratum
    #' @param design_effect Numeric. Design effect (default = 1)
    #' @param precision Numeric. Desired precision (default = 0.05)
    #' @param confidence_level Numeric. Confidence level (default = 0.95)
    #' @param allocation_method Character. Method for allocation (default = "proportional")
    add_stratum = function(stratum_id, 
                          stratum_name,
                          population_size,
                          design_effect = 1,
                          precision = 0.05,
                          confidence_level = 0.95,
                          allocation_method = "proportional") {
      
      # Validate inputs
      if (design_effect < 1) {
        stop("Design effect must be >= 1")
      }
      if (precision <= 0 || precision >= 1) {
        stop("Precision must be between 0 and 1")
      }
      if (confidence_level <= 0 || confidence_level >= 1) {
        stop("Confidence level must be between 0 and 1")
      }
      if (population_size <= 0) {
        stop("Population size must be positive")
      }
      
      new_row <- data.frame(
        stratum_id = stratum_id,
        stratum_name = stratum_name,
        population_size = population_size,
        sample_size = NA_real_,
        design_effect = design_effect,
        precision = precision,
        confidence_level = confidence_level,
        allocation_method = allocation_method,
        number_of_days = NA_real_,
        number_of_teams = NA_real_,
        stringsAsFactors = FALSE
      )
      
      if (is.null(self$sample_table)) {
        self$sample_table <- new_row
      } else {
        if (stratum_id %in% self$sample_table$stratum_id) {
          stop(paste("Stratum ID", stratum_id, "already exists"))
        }
        self$sample_table <- rbind(self$sample_table, new_row)
      }
      
      self$metadata$modified_date <- Sys.time()
      private$check_issues()
      invisible(self)
    },
    
    #' @description Set the sampling frame
    #' @param frame Data frame with required columns: id, stratum, population_size
    set_sampling_frame = function(frame) {
      # Validate frame structure
      required_cols <- c("id", "stratum", "population_size")
      missing_cols <- setdiff(required_cols, names(frame))
      if (length(missing_cols) > 0) {
        stop(paste("Sampling frame missing required columns:", 
                   paste(missing_cols, collapse = ", ")))
      }
      
      # Check for duplicates
      if (any(duplicated(frame$id))) {
        stop("Duplicate IDs found in sampling frame")
      }
      
      # Check for missing values
      for (col in required_cols) {
        if (any(is.na(frame[[col]]))) {
          stop(paste("Missing values found in column:", col))
        }
      }
      
      self$sampling_frame <- frame
      self$metadata$modified_date <- Sys.time()
      private$check_issues()
      invisible(self)
    },
    
    #' @description Draw sample from sampling frame
    #' @param method Character. Sampling method: "srs", "proportional", "pps_cluster", "rlc", "systematic"
    #' @param seed Integer. Random seed for reproducibility
    #' @param cluster_size Numeric. Cluster size for PPS or RLC methods
    draw_sample = function(method = "srs", seed = NULL, cluster_size = NULL) {
      if (is.null(self$sampling_frame)) {
        stop("Must set sampling frame before drawing sample")
      }
      
      if (is.null(seed)) {
        seed <- as.integer(Sys.time())
      }
      set.seed(seed)
      
      # Draw sample by stratum
      drawn_samples <- list()
      
      for (i in seq_len(nrow(self$sample_table))) {
        stratum_row <- self$sample_table[i, ]
        stratum_id <- stratum_row$stratum_id
        n_needed <- stratum_row$sample_size
        
        # Get frame units for this stratum
        stratum_frame <- self$sampling_frame[self$sampling_frame$stratum == stratum_id, ]
        
        if (nrow(stratum_frame) < n_needed) {
          warning(paste("Not enough units in frame for stratum", stratum_id,
                       ". Needed:", n_needed, "Available:", nrow(stratum_frame)))
          n_needed <- nrow(stratum_frame)
        }
        
        # Draw sample based on method
        if (method == "srs") {
          # Simple random sampling
          sample_idx <- sample(seq_len(nrow(stratum_frame)), n_needed, replace = FALSE)
          stratum_sample <- stratum_frame[sample_idx, ]
          
        } else if (method == "proportional") {
          # Proportional allocation
          sample_idx <- sample(seq_len(nrow(stratum_frame)), n_needed, replace = FALSE)
          stratum_sample <- stratum_frame[sample_idx, ]
          
        } else if (method == "pps_cluster") {
          # PPS with provided cluster size
          if (is.null(cluster_size)) {
            stop("cluster_size must be provided for pps_cluster method")
          }
          n_clusters <- ceiling(n_needed / cluster_size)
          probs <- stratum_frame$population_size / sum(stratum_frame$population_size)
          sample_idx <- sample(seq_len(nrow(stratum_frame)), n_clusters, 
                              replace = FALSE, prob = probs)
          stratum_sample <- stratum_frame[sample_idx, ]
          stratum_sample$cluster_size <- cluster_size
          
        } else if (method == "rlc") {
          # Random Location Cluster with size 3 using PPS
          n_clusters <- ceiling(n_needed / 3)
          probs <- stratum_frame$population_size / sum(stratum_frame$population_size)
          sample_idx <- sample(seq_len(nrow(stratum_frame)), n_clusters, 
                              replace = FALSE, prob = probs)
          stratum_sample <- stratum_frame[sample_idx, ]
          stratum_sample$cluster_size <- 3
          
        } else if (method == "systematic") {
          # Systematic sampling
          interval <- floor(nrow(stratum_frame) / n_needed)
          start <- sample(seq_len(interval), 1)
          sample_idx <- seq(start, nrow(stratum_frame), by = interval)[1:n_needed]
          stratum_sample <- stratum_frame[sample_idx, ]
          
        } else {
          stop(paste("Unknown sampling method:", method))
        }
        
        drawn_samples[[stratum_id]] <- stratum_sample
      }
      
      # Store drawn sample with metadata
      self$drawn_sample <- list(
        samples = drawn_samples,
        method = method,
        seed = seed,
        date_drawn = Sys.time(),
        cluster_size = cluster_size
      )
      
      self$metadata$modified_date <- Sys.time()
      private$check_issues()
      invisible(self)
    },
    
    #' @description Add a single Tool object to the protocol by specifying its type.
    #' A new tool of the requested type is instantiated (loading its bundled
    #' default XLSForm template) and appended to the \code{tools} list.
    #' Call this method once per tool you wish to add.
    #' @param tool_type Character. Type of tool to create.  One of
    #'   \code{"household"}, \code{"key_informant"}, \code{"observation"}, or
    #'   \code{"generic"}.  Defaults to \code{"household"}.
    #' @param tool_name Optional character. Name for the new tool.
    #' @return Invisibly returns self for method chaining.
    add_tools = function(tool_type = "household", tool_name = NULL) {
      valid_types <- c("household", "key_informant", "observation", "generic")
      if (!tool_type %in% valid_types) {
        stop(paste(
          "tool_type must be one of:",
          paste(valid_types, collapse = ", ")
        ))
      }

      tool <- switch(
        tool_type,
        "household"     = HouseholdTool$new(name = tool_name),
        "key_informant" = KeyInformantTool$new(name = tool_name),
        "observation"   = ObservationTool$new(name = tool_name),
        Tool$new(name = tool_name)
      )

      if (is.null(self$tools)) {
        self$tools <- list()
      }

      self$tools <- c(self$tools, list(tool))
      self$metadata$modified_date <- Sys.time()
      private$check_issues()
      invisible(self)
    },
    
    #' @description Select indicators for data collection
    #' @param indicator_list List of indicators
    select_indicators = function(indicator_list) {
      self$selected_indicators <- indicator_list
      self$metadata$modified_date <- Sys.time()
      private$check_issues()
      invisible(self)
    },
    
    #' @description Get the sample table
    #' @return Data frame containing the sample table
    get_sample_table = function() {
      return(self$sample_table)
    },
    
    #' @description Get all issues
    #' @return List of validation issues
    get_issues = function() {
      return(self$issues)
    },
    
    #' @description Get protocol summary
    #' @return List with protocol summary information
    get_protocol_summary = function() {
      summary <- list(
        assessment_title = self$metadata$assessment_title,
        country_name = self$metadata$country_name,
        month_year = self$metadata$month_year,
        created = self$metadata$created_date,
        modified = self$metadata$modified_date,
        num_primary_objectives = length(self$primary_objectives),
        num_secondary_objectives = length(self$secondary_objectives),
        num_strata = if (is.null(self$sample_table)) 0 else nrow(self$sample_table),
        total_sample_size = if (is.null(self$sample_table)) 0 else sum(self$sample_table$sample_size, na.rm = TRUE),
        num_tools = length(self$tools),
        num_issues = length(self$issues)
      )
      return(summary)
    },
    
    #' @description Export protocol to a list
    #' @return List containing all protocol data
    export_protocol = function() {
      list(
        metadata = self$metadata,
        primary_objectives = self$primary_objectives,
        secondary_objectives = self$secondary_objectives,
        sample_table = self$sample_table,
        sampling_frame = self$sampling_frame,
        drawn_sample = self$drawn_sample,
        tools = self$tools,
        selected_indicators = self$selected_indicators,
        issues = self$issues,
        summary = self$get_protocol_summary()
      )
    }
  ),
  
  private = list(
    #' @description Check for issues and discrepancies in the protocol
    check_issues = function() {
      self$issues <- list()
      
      # Check if objectives have matching indicators in tools
      all_objectives <- c(self$primary_objectives, self$secondary_objectives)
      if (length(all_objectives) > 0 && length(self$tools) > 0) {
        obj_sectors <- unique(sapply(all_objectives, function(x) x$sector))
        
        # Placeholder: Check tool coverage (actual Tool class will define how to extract sectors)
        # For now, assume tools have a $sector field
        tool_sectors <- character(0)
        tryCatch({
          tool_sectors <- unique(sapply(self$tools, function(x) {
            if (is.list(x) && "sector" %in% names(x)) {
              return(x$sector)
            } else if (methods::is(x, "R6") && "sector" %in% names(x)) {
              return(x$sector)
            }
            return(NA_character_)
          }))
          tool_sectors <- tool_sectors[!is.na(tool_sectors)]
        }, error = function(e) {
          # Ignore extraction errors
        })
        
        missing_sectors <- setdiff(obj_sectors, tool_sectors)
        if (length(missing_sectors) > 0) {
          self$issues$tool_coverage <- paste(
            "Objectives require sectors not covered by tools:",
            paste(missing_sectors, collapse = ", ")
          )
        }
      }
      
      # Check strata consistency between frame and sample table
      if (!is.null(self$sample_table) && !is.null(self$sampling_frame)) {
        table_strata <- self$sample_table$stratum_id
        frame_strata <- unique(self$sampling_frame$stratum)
        
        if (!setequal(table_strata, frame_strata)) {
          missing_in_frame <- setdiff(table_strata, frame_strata)
          missing_in_table <- setdiff(frame_strata, table_strata)
          
          if (length(missing_in_frame) > 0) {
            self$issues$strata_missing_in_frame <- paste(
              "Strata in sample table but not in frame:",
              paste(missing_in_frame, collapse = ", ")
            )
          }
          if (length(missing_in_table) > 0) {
            self$issues$strata_missing_in_table <- paste(
              "Strata in frame but not in sample table:",
              paste(missing_in_table, collapse = ", ")
            )
          }
        }
      }
      
      invisible(self)
    }
  )
)
