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
    #' @field primary_objectives List of primary research objectives (plain lists)
    primary_objectives = NULL,

    #' @field secondary_objectives List of secondary research objectives (plain lists)
    secondary_objectives = NULL,

    #' @field objective_schema Data frame containing the loaded objective schema
    objective_schema = NULL,

    #' @field sample_table Master data frame with one row per stratum and all
    #'   relevant population, sample-size, and logistics parameters
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

    #' @description
    #' Creates a new Protocol object
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
      self$objective_schema <- private$default_objective_schema()
      invisible(self)
    },
    
    #' @description Set primary research objectives
    #' @param objectives List of primary objectives (plain lists with fields:
    #'   objective_id, objective_text, sector)
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
    #' @param objectives List of secondary objectives (plain lists with fields:
    #'   objective_id, objective_text, sector)
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
    
    #' @description Add a stratum row to the master sample table
    #'
    #' Each call appends one row to \code{sample_table}.  Every column in the
    #' master table is included; columns that are not supplied default to
    #' \code{NA}.  Parameters that have clear equivalents in the master table
    #' schema are mapped automatically (e.g. \code{population_size} maps to
    #' \code{Total_Population}).
    #'
    #' @param stratum_id Character. Unique identifier for the stratum (used as
    #'   the row key and for cross-referencing the sampling frame).
    #' @param stratum_name Character. Human-readable name (stored as
    #'   \code{Population_Name}).
    #' @param population_size Numeric. Total population for this stratum
    #'   (stored as \code{Total_Population}).
    #' @param total_households Numeric. Total number of households (stored as
    #'   \code{Total_Households}).  Defaults to \code{NA}.
    #' @param sampling_method Character. Primary sampling method for the
    #'   stratum (stored as \code{Sampling_Method}).  Also accepted via the
    #'   legacy alias \code{allocation_method}.  Defaults to \code{"srs"}.
    #' @param allocation_method Deprecated alias for \code{sampling_method}.
    #' @param pop_indicator Character. Indicator label for population-level
    #'   sample size calculation (default \code{"General"}).
    #' @param pop_expected_prevalence Numeric. Expected prevalence used for
    #'   population-level sample size (\%).
    #' @param pop_precision Numeric. Desired precision for population-level
    #'   estimate (\%).
    #' @param pop_nonresponse Numeric. Expected non-response rate for
    #'   population-level calculation (\%).
    #' @param pop_design_effect Numeric. Design effect for population-level
    #'   calculation.  Also accepted via the legacy alias \code{design_effect}.
    #' @param pop_fpc Logical. Apply finite population correction at population
    #'   level?  Defaults to \code{FALSE}.
    #' @param pop_result_dummy Numeric. Calculated (or placeholder) sample size
    #'   at population level.
    #' @param ind_indicator Character. Indicator label for individual-level
    #'   sample size calculation.
    #' @param ind_expected_prevalence Numeric. Expected prevalence for
    #'   individual-level calculation (\%).
    #' @param ind_precision Numeric. Desired precision for individual-level
    #'   estimate (\%).
    #' @param ind_nonresponse Numeric. Expected non-response rate for
    #'   individual-level calculation (\%).
    #' @param ind_design_effect Numeric. Design effect for individual-level
    #'   calculation.
    #' @param ind_avg_hh_size Numeric. Average household size (individual
    #'   level).
    #' @param ind_subpop_prop Numeric. Sub-population proportion (\%) for
    #'   individual-level calculation.
    #' @param ind_fpc Logical. Apply finite population correction at individual
    #'   level?  Defaults to \code{FALSE}.
    #' @param ind_result_dummy Numeric. Calculated (or placeholder) sample size
    #'   at individual level.
    #' @param mort_indicator Character. Indicator label for mortality-level
    #'   sample size calculation.
    #' @param mort_expected_death_rate Numeric. Expected crude death rate used
    #'   for mortality sample size calculation.
    #' @param mort_precision Numeric. Desired precision for mortality estimate.
    #' @param mort_nonresponse Numeric. Expected non-response rate for
    #'   mortality calculation (\%).
    #' @param mort_design_effect Numeric. Design effect for mortality
    #'   calculation.
    #' @param mort_recall_days Integer. Recall period in days for mortality
    #'   estimate.
    #' @param mort_avg_hh_size Numeric. Average household size (mortality
    #'   level).
    #' @param mort_fpc Logical. Apply finite population correction at mortality
    #'   level?  Defaults to \code{FALSE}.
    #' @param mort_result_dummy Numeric. Calculated (or placeholder) sample
    #'   size at mortality level.
    #' @param teams Numeric. Number of field teams.
    #' @param avg_interview_time Numeric. Average interview time in minutes.
    #' @param clusters_per_day Numeric. Number of clusters visited per day per
    #'   team.
    #' @param enumerators_per_team Numeric. Number of enumerators per team.
    #' @param avg_rest_time Numeric. Average rest/break time in minutes per
    #'   day.
    #' @param avg_travel_time Numeric. Average travel time to cluster in
    #'   minutes.
    #' @param start_time Character. Planned work start time (e.g.
    #'   \code{"08:00"}).
    #' @param end_time Character. Planned work end time (e.g.
    #'   \code{"17:00"}).
    #' @param design_effect Deprecated alias for \code{pop_design_effect}.
    #' @param precision Deprecated alias for \code{pop_precision}.
    #' @param confidence_level Deprecated / ignored in the new schema.
    #' @return Invisibly returns \code{self} for method chaining.
    add_stratum = function(
      stratum_id,
      stratum_name,
      population_size        = NA_real_,
      total_households       = NA_real_,
      sampling_method        = "srs",
      allocation_method      = NULL,   # legacy alias for sampling_method
      pop_indicator          = "General",
      pop_expected_prevalence = NA_real_,
      pop_precision          = NA_real_,
      pop_nonresponse        = NA_real_,
      pop_design_effect      = NA_real_,
      pop_fpc                = FALSE,
      pop_result_dummy       = NA_real_,
      ind_indicator          = NA_character_,
      ind_expected_prevalence = NA_real_,
      ind_precision          = NA_real_,
      ind_nonresponse        = NA_real_,
      ind_design_effect      = NA_real_,
      ind_avg_hh_size        = NA_real_,
      ind_subpop_prop        = NA_real_,
      ind_fpc                = FALSE,
      ind_result_dummy       = NA_real_,
      mort_indicator         = NA_character_,
      mort_expected_death_rate = NA_real_,
      mort_precision         = NA_real_,
      mort_nonresponse       = NA_real_,
      mort_design_effect     = NA_real_,
      mort_recall_days       = NA_real_,
      mort_avg_hh_size       = NA_real_,
      mort_fpc               = FALSE,
      mort_result_dummy      = NA_real_,
      teams                  = NA_real_,
      avg_interview_time     = NA_real_,
      clusters_per_day       = NA_real_,
      enumerators_per_team   = NA_real_,
      avg_rest_time          = NA_real_,
      avg_travel_time        = NA_real_,
      start_time             = NA_character_,
      end_time               = NA_character_,
      # Legacy / deprecated params kept for backward compatibility
      design_effect          = NULL,
      precision              = NULL,
      confidence_level       = NULL
    ) {

      # Resolve legacy aliases
      if (!is.null(allocation_method) && sampling_method == "srs") {
        sampling_method <- allocation_method
      }
      if (!is.null(design_effect) && is.na(pop_design_effect)) {
        pop_design_effect <- design_effect
      }
      if (!is.null(precision) && is.na(pop_precision)) {
        pop_precision <- precision
      }

      new_row <- data.frame(
        stratum_id               = stratum_id,
        Population_Name          = stratum_name,
        Total_Households         = as.numeric(total_households),
        Total_Population         = as.numeric(population_size),
        Sampling_Method          = sampling_method,
        pop_indicator            = pop_indicator,
        pop_expected_prevalence  = as.numeric(pop_expected_prevalence),
        pop_precision            = as.numeric(pop_precision),
        pop_nonresponse          = as.numeric(pop_nonresponse),
        pop_design_effect        = as.numeric(pop_design_effect),
        pop_fpc                  = as.logical(pop_fpc),
        pop_result_dummy         = as.numeric(pop_result_dummy),
        ind_indicator            = as.character(ind_indicator),
        ind_expected_prevalence  = as.numeric(ind_expected_prevalence),
        ind_precision            = as.numeric(ind_precision),
        ind_nonresponse          = as.numeric(ind_nonresponse),
        ind_design_effect        = as.numeric(ind_design_effect),
        ind_avg_hh_size          = as.numeric(ind_avg_hh_size),
        ind_subpop_prop          = as.numeric(ind_subpop_prop),
        ind_fpc                  = as.logical(ind_fpc),
        ind_result_dummy         = as.numeric(ind_result_dummy),
        mort_indicator           = as.character(mort_indicator),
        mort_expected_death_rate = as.numeric(mort_expected_death_rate),
        mort_precision           = as.numeric(mort_precision),
        mort_nonresponse         = as.numeric(mort_nonresponse),
        mort_design_effect       = as.numeric(mort_design_effect),
        mort_recall_days         = as.numeric(mort_recall_days),
        mort_avg_hh_size         = as.numeric(mort_avg_hh_size),
        mort_fpc                 = as.logical(mort_fpc),
        mort_result_dummy        = as.numeric(mort_result_dummy),
        teams                    = as.numeric(teams),
        avg_interview_time       = as.numeric(avg_interview_time),
        clusters_per_day         = as.numeric(clusters_per_day),
        enumerators_per_team     = as.numeric(enumerators_per_team),
        avg_rest_time            = as.numeric(avg_rest_time),
        avg_travel_time          = as.numeric(avg_travel_time),
        start_time               = as.character(start_time),
        end_time                 = as.character(end_time),
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
        n_needed <- stratum_row$pop_result_dummy
        
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
    
    #' @description Validate the structure of the master sample table
    #'
    #' Checks that \code{sample_table} exists and contains all required master
    #' table columns.  Returns a list with a \code{valid} flag and a
    #' \code{message}.
    #'
    #' @return Named list with elements \code{valid} (logical) and
    #'   \code{message} (character).
    validate_strata_table = function() {
      if (is.null(self$sample_table)) {
        return(list(valid = FALSE, message = "sample_table is NULL — no strata have been added yet."))
      }

      required_cols <- .strata_table_required_cols

      missing_cols <- setdiff(required_cols, names(self$sample_table))
      if (length(missing_cols) > 0) {
        return(list(
          valid   = FALSE,
          message = paste("sample_table is missing required columns:",
                          paste(missing_cols, collapse = ", "))
        ))
      }

      dupes <- self$sample_table$stratum_id[duplicated(self$sample_table$stratum_id)]
      if (length(dupes) > 0) {
        return(list(
          valid   = FALSE,
          message = paste("Duplicate stratum_id values:", paste(dupes, collapse = ", "))
        ))
      }

      list(valid = TRUE, message = "sample_table structure is valid.")
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
        total_sample_size = if (is.null(self$sample_table)) 0 else sum(self$sample_table$pop_result_dummy, na.rm = TRUE),
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
        objective_schema = self$objective_schema,
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
    # Load the default objective schema from the bundled reference.xlsx file
    default_objective_schema = function() {
      load_objective_schema()
    },
    # Check for issues and discrepancies in the protocol
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
