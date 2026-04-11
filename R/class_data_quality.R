#' IPHRA Base Data Quality Class
#'
#' The `DataQuality` R6 class is a standalone class for analyzing and
#' reporting on data quality metrics. It is generated from a Data object
#' and holds a passed dataset (standardized or clean, but not raw).
#'
#' @description
#' This class provides:
#' * Summary statistics and calculations on the data
#' * Quality check schema with tests, thresholds, and penalty scores
#' * Nested list structure for schema definition
#' * Utility methods for tabular format conversion
#' * Custom visualization methods (subclass-specific)
#'
#' @details
#' Key functionalities include:
#' * Quality test definitions with configurable thresholds
#' * Penalty scoring system for test failures
#' * Link back to the parent Data object
#' * Summary report generation
#' * Schema-to-table and table-to-schema conversions
#'
#' @field data Data frame containing the dataset (standardized or clean data, not raw)
#' @field parent_data_object Reference to the parent Data object that generated this quality object
#' @field dataset_name Character name for identifying this quality assessment
#' @field quality_schema Quality check schema with test definitions, thresholds, and penalty scores
#' @field outputs_schema Outputs schema defining visualizations and tables to create
#' @field visualizations List of ggplot2 graphics/visualizations
#' @field tables List of dataframes or formatted table objects
#' @field results Results of quality checks execution
#' @field summary_stats Computed summary statistics from the data
#' @field overall_score Overall data quality score
#' @field metadata Free-form metadata list
#' @field variable_label Named list of variable labels from parent Data object
#' @field value_label Named list of value labels from parent Data object
#'
#' @seealso [Data], [FSLDataQuality], [HealthDataQuality], [WASHDataQuality]
#' @export
DataQuality <- R6::R6Class(
  classname = "DataQuality",

  public = list(

    # Core fields
    data = NULL,                  # The dataset (standardized or clean)
    parent_data_object = NULL,    # Reference to the parent Data object
    dataset_name = NULL,          # Name for identification
    data_stage_name = NULL,       # Name of the data stage (e.g., "standardized", "clean")
    data_hash = NULL,             # Hash of the data from parent Data object
    variable_map = NULL,          # Variable mappings from Data object
    value_map = NULL,             # Value mappings from Data object
    variable_label = NULL,        # Variable labels from Data object
    value_label = NULL,           # Value labels from Data object
    quality_schema = NULL,        # Quality check schema with tests/thresholds/penalties
    outputs_schema = NULL,        # Outputs schema for visualizations and tables
    visualizations = NULL,        # List of ggplot2 graphics
    tables = NULL,                # List of dataframes/formatted tables
    results = NULL,               # Results of quality checks
    summary_stats = NULL,         # Computed summary statistics
    overall_score = NULL,         # Overall data quality score
    metadata = NULL,              # Free-form metadata

    #' @description
    #' Initialize a new DataQuality object
    #'
    #' @param data A data frame (standardized or clean data, not raw)
    #' @param parent_data_object The Data object that generated this quality object
    #' @param dataset_name A name for this quality assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param variable_label Variable labels from Data object
    #' @param value_label Value labels from Data object
    #' @param quality_schema Optional quality check schema
    #' @return A new DataQuality object
    initialize = function(data = NULL,
                          parent_data_object = NULL,
                          dataset_name = "DataQuality",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          quality_schema = NULL) {

      phr_try({

        # Validate data is provided
        if (is.null(data)) {
          phr_error(
            message = "No data provided for DataQuality initialization.",
            origin = dataset_name
          )
        }

        # Validate data is a data frame
        phr_validate_dataframe(data, origin = dataset_name, soft = FALSE)

        # Store the data
        self$data <- data
        self$parent_data_object <- parent_data_object
        self$dataset_name <- dataset_name
        self$data_stage_name <- data_stage_name
        self$data_hash <- data_hash
        self$variable_map <- variable_map
        self$value_map <- value_map
        self$variable_label <- variable_label %||% list()
        self$value_label <- value_label %||% list()
        self$metadata <- list(
          created_at = Sys.time(),
          n_records = nrow(data),
          n_columns = ncol(data),
          parent_name = if (!is.null(parent_data_object)) parent_data_object$dataset_name else NULL,
          data_stage_name = data_stage_name,
          data_hash = data_hash
        )

        # Initialize or set quality schema
        if (!is.null(quality_schema)) {
          self$set_quality_schema(quality_schema)
        } else {
          self$quality_schema <- self$default_quality_schema()
        }

        # Initialize outputs schema
        self$outputs_schema <- self$default_outputs_schema()

        # Initialize results, summary, and output containers
        self$results <- list()
        self$summary_stats <- list()
        self$overall_score <- NA_real_
        self$visualizations <- list()  # List of ggplot2 graphics
        self$tables <- list()          # List of dataframes/formatted tables

        phr_message(phr_txt(glue::glue("{dataset_name} initialized with {nrow(data)} records.")))

      }, on_error = "abort", origin = paste0(dataset_name, "$initialize"))
    },

    #' @description
    #' Get the default quality schema from template file
    #'
    #' Reads the quality_schema_template.xlsx file from package resources
    #' and converts it to a nested list of quality checks (schema only, no metadata).
    #'
    #' @return A list of quality checks (the schema itself)
    default_quality_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_template.xlsx",
        package = "iphRa"
      )

      # If template file doesn't exist, return empty schema
      if (!file.exists(file) || file == "") {
        # During development, try relative path
        file <- file.path("resources", "quality_schema_data_quality_template.xlsx")
        if (!file.exists(file)) {
          return(list())  # Return empty list (no checks)
        }
      }

      # Read the Excel template
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_warning(
            origin  = "DataQuality$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_template.xlsx: {e$message}"))
          )
          return(NULL)
        }
      )

      # If reading failed or file is empty, return empty schema
      if (is.null(df) || nrow(df) == 0) {
        return(list())  # Return empty list (no checks)
      }

      # Convert table to schema using quality_table_to_schema
      schema_with_metadata <- quality_table_to_schema(df)

      # Return only the checks (the schema itself), not the metadata wrapper
      if (!is.null(schema_with_metadata) && !is.null(schema_with_metadata$checks)) {
        return(schema_with_metadata$checks)
      }

      return(list())  # Return empty list if conversion failed
    },

    #' @description
    #' Get the default outputs schema from template file
    #'
    #' Reads the outputs_schema_data_analytics_template.xlsx file from package resources
    #' and converts it to a nested list of outputs definitions.
    #'
    #' @return A list of outputs definitions (nested list format)
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_template.xlsx",
        package = "iphRa"
      )

      # If template file doesn't exist, return empty schema
      if (!file.exists(file) || file == "") {
        # During development, try relative path
        file <- file.path("resources", "outputs_schema_data_analytics_template.xlsx")
        if (!file.exists(file)) {
          return(list())  # Return empty list (no outputs)
        }
      }

      # Read the Excel template
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_warning(
            origin  = "DataQuality$default_outputs_schema",
            message = phr_txt(glue::glue("Failed to read outputs_schema_data_analytics_template.xlsx: {e$message}"))
          )
          return(NULL)
        }
      )

      # If reading failed or file is empty, return empty schema
      if (is.null(df) || nrow(df) == 0) {
        return(list())  # Return empty list (no outputs)
      }

      # Convert table to schema using outputs_table_to_schema
      outputs_schema <- outputs_table_to_schema(df)

      return(outputs_schema)
    },

    #' @description
    #' Set the quality check schema
    #'
    #' @param schema A list defining quality checks, thresholds, and penalties
    set_quality_schema = function(schema) {

      phr_try({

        # Validate schema structure
        self$validate_quality_schema(schema)

        self$quality_schema <- schema

        phr_message(
          phr_txt(glue::glue("Quality schema set for {self$dataset_name}."))
        )

      }, on_error = "abort", origin = paste0(self$dataset_name, "$set_quality_schema"))
    },

    #' @description
    #' Get the quality check schema
    #'
    #' @return The current quality schema
    get_quality_schema = function() {
      self$quality_schema
    },

    #' @description
    #' Set the outputs schema
    #'
    #' Sets a complete outputs schema from a nested list structure.
    #' This follows the same pattern as set_variable_schema() in Data class.
    #'
    #' @param schema A list defining outputs (visualizations and tables)
    set_outputs_schema = function(schema) {

      phr_try({

        # 1. Validate nested schema structure
        outputs_validate_schema_to_table(
          outputs_schema = schema,
          origin = paste0(self$dataset_name, "$set_outputs_schema")
        )

        # 2. Convert to table
        tbl <- outputs_schema_to_table(schema)

        # 3. Validate the table
        outputs_validate_table_to_schema(df = tbl)

        # 4. Store
        self$outputs_schema <- schema

        phr_message(
          phr_txt(glue::glue("Outputs schema set for {self$dataset_name}."))
        )

      }, on_error = "abort", origin = paste0(self$dataset_name, "$set_outputs_schema"))
    },

    #' @description
    #' Get the outputs schema
    #'
    #' @return The current outputs schema
    get_outputs_schema = function() {
      self$outputs_schema
    },

    #' @description
    #' Validate the quality schema structure
    #'
    #' @param schema The schema to validate (a list of quality checks)
    #' @return TRUE if valid, throws error otherwise
    validate_quality_schema = function(schema) {

      phr_try({

        if (is.null(schema) || !is.list(schema)) {
          phr_error(
            message = "Quality schema must be a list.",
            origin = self$dataset_name
          )
        }

        # Schema is now the checks list itself (no wrapper with $checks)
        # Validate each check entry
        for (check_name in names(schema)) {
          check <- schema[[check_name]]

          if (!is.list(check)) {
            phr_error(
              message = phr_txt(glue::glue("Check '{check_name}' must be a list.")),
              origin = self$dataset_name
            )
          }

          required_fields <- c("check_name", "check_label", "statistical_test")
          missing <- setdiff(required_fields, names(check))

          if (length(missing) > 0) {
            phr_warning(
              message = phr_txt(glue::glue("Check '{check_name}' is missing recommended fields: {paste(missing, collapse=', ')}.")),
              origin = self$dataset_name
            )
          }
        }

        invisible(TRUE)

      }, on_error = "abort", origin = paste0(self$dataset_name, "$validate_quality_schema"))
    },

    #' @description
    #' Run all quality checks defined in the schema
    #'
    #' @return A list of check results
    run_quality_checks = function() {


      phr_try({

        # Schema is now the checks list itself (no wrapper)
        if (is.null(self$quality_schema) || length(self$quality_schema) == 0) {
          phr_warning(
            message = "No quality checks defined in schema.",
            origin = self$dataset_name
          )
          return(invisible(list()))
        }

        results <- list()

        # Iterate over checks directly (schema IS the checks list)
        for (check_name in names(self$quality_schema)) {


          check <- self$quality_schema[[check_name]]
          result <- self$execute_check(check)
          results[[check_name]] <- result
        }


        self$results <- results
        self$calculate_overall_score()

        # --- Plausibility tables -------------------------------------------
        # Initialise the plausibility sub-list inside self$tables
        if (is.null(self$tables[["plausibility"]])) {
          self$tables[["plausibility"]] <- list()
        }

        # Overall penalty summary
        results_df <- self$results_to_table()
        penalty_tbl <- table_quality_penalty_summary(
          results_df,
          title_name = "Data Quality Penalty Summary"
        )
        if (!is.null(penalty_tbl)) {
          self$tables[["plausibility"]][["penalty_summary"]] <- penalty_tbl
        }

        # Per-group penalty summaries for enum_id and stratum (if mapped)
        for (role in c("enum_id", "stratum")) {
          col_name <- self$get_variable(role)
          if (!is.null(col_name) && nzchar(col_name) &&
              col_name %in% names(self$data)) {
            per_group_df <- self$.compute_results_by_group(col_name)
            if (!is.null(per_group_df) && nrow(per_group_df) > 0) {
              tbl_key     <- paste0("penalty_summary_by_", role)
              group_label <- if (role == "enum_id") "Enumerator ID" else "Stratum"
              tbl_title   <- if (role == "enum_id") {
                "Data Quality Penalty Summary by Enumerator"
              } else {
                "Data Quality Penalty Summary by Stratum"
              }
              per_group_tbl <- table_quality_penalty_summary_by_group(
                per_group_df,
                group_col   = "group_value",
                group_label = group_label,
                title_name  = tbl_title
              )
              if (!is.null(per_group_tbl)) {
                self$tables[["plausibility"]][[tbl_key]] <- per_group_tbl
              }

              # Individual penalty summary tables — one per group value
              group_values <- sort(unique(per_group_df$group_value))
              for (gv in group_values) {
                gv_label   <- as.character(gv)
                gv_safe    <- gsub("[^A-Za-z0-9]", "_", gv_label)
                gv_results <- per_group_df %>%
                  dplyr::filter(group_value == gv) %>%
                  dplyr::select(-group_value)
                gv_title <- if (role == "enum_id") {
                  paste0("Data Quality Penalty Summary - Enumerator: ", gv_label)
                } else {
                  paste0("Data Quality Penalty Summary - Stratum: ", gv_label)
                }
                gv_tbl_key <- paste0("penalty_summary_", role, "_", gv_safe)
                gv_tbl <- table_quality_penalty_summary(
                  gv_results,
                  title_name = gv_title
                )
                if (!is.null(gv_tbl)) {
                  self$tables[["plausibility"]][[gv_tbl_key]] <- gv_tbl
                }
              }
            }
          }
        }
        # -------------------------------------------------------------------

        phr_message(
          phr_txt(glue::glue("Ran {length(results)} quality checks for {self$dataset_name}."))
        )

        invisible(results)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$run_quality_checks"))
    },

    #' @description
    #' Execute a single quality check using statistical tests
    #'
    #' @param check A single check definition from the schema
    #' @return A list containing the check result
    execute_check = function(check) {

      phr_try({


        result <- list(
          check_name = check$check_name %||% "unknown",
          check_label = check$check_label %||% "",
          check_group = check$check_group %||% NA_character_,
          test_statistic = NA,
          p_value = NA,
          penalty = 0,
          max_penalty = 0,
          threshold_expression = NA_character_,
          message = ""
        )

        test_name <- check$statistical_test
        variables <- check$variables
        thresholds <- check$thresholds
        test_params <- check$test_params

        # Check if variables exist in data
        if (is.null(variables) || length(variables) == 0) {
          result$message <- "No variables specified"
          return(result)
        }

        # Translate canonical variable names to actual column names using variable_map
        mapped_vars <- self$.translate_canonical_to_actual_vars(variables)

        # Check that ALL mapped variables exist in data
        available_vars <- intersect(mapped_vars, names(self$data))
        if (length(available_vars) != length(mapped_vars)) {
          missing_vars <- setdiff(mapped_vars, available_vars)
          result$message <- paste0("Required variables not found in data: ", paste(missing_vars, collapse = ", "))
          return(result)
        }

        # Execute the statistical test
        # First check if the function exists
        test_func_name <- paste0("quality_test_", test_name)

        # Try to find function in multiple environments:
        # 1. Package namespace (works when package is installed/loaded)
        # 2. Parent environment (works during development)
        # 3. Global environment (fallback)
        test_function <- NULL


        # Try package namespace first
        if (requireNamespace("iphRa", quietly = TRUE)) {
          # Use tryCatch to safely attempt to get the function from namespace
          tryCatch({
            ns <- asNamespace("iphRa")
            if (exists(test_func_name, envir = ns, mode = "function", inherits = FALSE)) {
              test_function <- get(test_func_name, envir = ns, mode = "function", inherits = FALSE)
            }
          }, error = function(e) {
            # Silently continue to next attempt if namespace access fails
          })
        }


        # If not found in package namespace, try parent environment
        if (is.null(test_function)) {
          tryCatch({
            if (exists(test_func_name, mode = "function", inherits = TRUE)) {
              test_function <- get(test_func_name, mode = "function", inherits = TRUE)
            }
          }, error = function(e) {
            # Silently continue if this also fails
          })
        }

        # If function still not found, return error
        if (is.null(test_function)) {
          print("I am here 5")
          result$message <- paste0("Unknown test: ", test_name, " (function ", test_func_name, " not found)")
          return(result)
        }

        # Call test function with appropriate parameters
        test_args <- list(data = self$data, variables = available_vars)
        if (!is.null(test_params)) {
          test_args <- c(test_args, test_params)
        }

        test_result <- do.call(test_function, test_args)

        # Handle case where test function returned NULL (error occurred)
        if (is.null(test_result)) {
          result$message <- paste0("Test function '", test_func_name, "' returned NULL (internal error occurred)")
          return(result)
        }

        # Extract test statistic and p_value
        if (is.list(test_result) && "statistic" %in% names(test_result)) {
          result$test_statistic <- test_result$statistic
          if ("p_value" %in% names(test_result)) {
            result$p_value <- test_result$p_value
          }
        } else {
          # Ensure we don't set test_statistic to NULL
          if (!is.null(test_result)) {
            result$test_statistic <- test_result
          }
        }

        # Evaluate thresholds using logical expressions
        if (!is.null(thresholds) && length(thresholds) > 0) {
          penalty_result <- self$evaluate_threshold_expressions(
            test_statistic = result$test_statistic,
            p_value = result$p_value,
            thresholds = thresholds
          )
          result$penalty <- penalty_result$penalty
          result$max_penalty <- penalty_result$max_penalty
          result$threshold_expression <- penalty_result$threshold_expression
          result$message <- penalty_result$message
        }

        return(result)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$execute_check"))
    },

    #' @description
    #' Evaluate threshold expressions to determine penalty
    #'
    #' Uses logical expressions to check which threshold is met and returns
    #' the associated penalty score.
    #'
    #' @param test_statistic The test statistic value
    #' @param p_value The p-value (if applicable)
    #' @param thresholds List of threshold definitions with expression and penalty
    #' @return List with penalty, max_penalty, threshold_expression, and message
    evaluate_threshold_expressions = function(test_statistic, p_value, thresholds) {

      # Default result
      result <- list(
        penalty = 0,
        max_penalty = 0,
        threshold_expression = "none",
        message = ""
      )

      if (is.null(thresholds) || length(thresholds) == 0) {
        return(result)
      }

      # Find max penalty for scoring
      # Support both 'penalty' and 'penalty_score' field names
      all_penalties <- sapply(thresholds, function(x) {
        x$penalty %||% x$penalty_score %||% 0
      })
      result$max_penalty <- max(all_penalties, na.rm = TRUE)

      # Evaluate each threshold expression
      for (i in seq_along(thresholds)) {
        threshold_def <- thresholds[[i]]

        # Support both 'expression' and 'threshold_expression' field names
        expression <- threshold_def$expression %||% threshold_def$threshold_expression

        # Support both 'penalty' and 'penalty_score' field names
        penalty <- threshold_def$penalty %||% threshold_def$penalty_score %||% 0

        if (is.null(expression) || is.na(expression) || !nzchar(expression)) {
          next
        }

        # Evaluate the logical expression
        # The expression can reference test_statistic and p_value
        phr_try({
          # Create evaluation environment with test_statistic and p_value
          eval_env <- new.env()
          eval_env$test_statistic <- test_statistic
          eval_env$p_value <- p_value

          # Evaluate the expression
          meets_threshold <- eval(parse(text = expression), envir = eval_env)

          # Check if result is TRUE
          if (isTRUE(meets_threshold)) {
            result$penalty <- penalty
            result$threshold_expression <- expression
            result$message <- paste0("Threshold met: ", expression,
                                    " (test_statistic=", round(test_statistic, 3),
                                    if (!is.na(p_value)) paste0(", p_value=", round(p_value, 4)) else "",
                                    "), penalty=", penalty)
            break  # Take first matching threshold
          }
        }, on_error = "warn", origin = "evaluate_threshold_expressions")
      }

      if (result$threshold_expression == "none") {
        result$message <- paste0("No threshold met (test_statistic=", round(test_statistic, 3),
                                if (!is.na(p_value)) paste0(", p_value=", round(p_value, 4)) else "",
                                ")")
      }

      return(result)
    },

    #' @description
    #' Calculate the overall quality score based on penalties
    #'
    #' @return The overall quality score (0-100)
    calculate_overall_score = function() {

      phr_try({

        if (length(self$results) == 0) {
          self$overall_score <- NA_real_
          return(NA_real_)
        }

        total_penalty <- 0
        max_penalty <- 0

        for (result in self$results) {
          if (!is.null(result$penalty)) {
            total_penalty <- total_penalty + result$penalty
          }
          if (!is.null(result$max_penalty)) {
            max_penalty <- max_penalty + result$max_penalty
          }
        }

        if (max_penalty > 0) {
          self$overall_score <- max(0, 100 - (total_penalty / max_penalty * 100))
        } else {
          self$overall_score <- 100
        }

        invisible(self$overall_score)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$calculate_overall_score"))
    },

    #' @description
    #' Compute summary statistics on the data
    #'
    #' @return A list of summary statistics
    compute_summary_stats = function() {

      phr_try({

        df <- self$data

        if (is.null(df) || nrow(df) == 0) {
          phr_warning(
            message = "No data available for summary statistics.",
            origin = self$dataset_name
          )
          return(list())
        }

        stats <- list(
          n_records = nrow(df),
          n_columns = ncol(df),
          columns = names(df),
          missing_by_column = sapply(df, function(x) sum(is.na(x))),
          missing_pct_by_column = sapply(df, function(x) round(sum(is.na(x)) / length(x) * 100, 2)),
          overall_missing_pct = round(sum(is.na(df)) / (nrow(df) * ncol(df)) * 100, 2)
        )

        self$summary_stats <- stats

        phr_message(
          phr_txt(glue::glue("Computed summary statistics for {self$dataset_name}."))
        )

        invisible(stats)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$compute_summary_stats"))
    },

    #' @description
    #' Get a summary of the data quality assessment
    #'
    #' @return A list with summary information
    summary = function() {

      list(
        dataset_name = self$dataset_name,
        n_records = nrow(self$data),
        n_columns = ncol(self$data),
        n_checks = length(self$quality_schema),  # Schema is now the checks list itself
        n_results = length(self$results),
        overall_score = self$overall_score,
        parent_object = if (!is.null(self$parent_data_object)) self$parent_data_object$dataset_name else NULL,
        created_at = self$metadata$created_at
      )
    },

    #' @description
    #' Convert quality results to a tabular format
    #'
    #' @details
    #' **Note:** This method now returns results in the new schema format (v3.0.0)
    #' with `test_statistic`, `p_value`, `threshold_expression`, and `max_penalty`
    #' columns. The previous `passed` and `value` columns have been replaced to
    #' align with the logical threshold expression approach.
    #'
    #' @return A tibble with quality check results including:
    #'   * `check_name` - Unique identifier for the check
    #'   * `check_label` - Human-readable description
    #'   * `test_statistic` - The statistical test result value
    #'   * `p_value` - P-value if applicable (NA otherwise)
    #'   * `penalty` - Penalty score assigned based on threshold expression
    #'   * `max_penalty` - Maximum possible penalty for this check
    #'   * `threshold_expression` - The logical expression that was matched
    #'   * `message` - Descriptive message about the result
    results_to_table = function() {

      phr_try({

        if (length(self$results) == 0) {
          return(tibble::tibble(
            check_name = character(),
            check_label = character(),
            check_group = character(),
            test_statistic = numeric(),
            p_value = numeric(),
            penalty = numeric(),
            max_penalty = numeric(),
            threshold_expression = character(),
            message = character()
          ))
        }

        rows <- lapply(names(self$results), function(nm) {
          r <- self$results[[nm]]
          tibble::tibble(
            check_name = r$check_name %||% nm,
            check_label = r$check_label %||% NA_character_,
            check_group = r$check_group %||% NA_character_,
            test_statistic = as.numeric(r$test_statistic %||% NA),
            p_value = as.numeric(r$p_value %||% NA),
            penalty = as.numeric(r$penalty %||% 0),
            max_penalty = as.numeric(r$max_penalty %||% 0),
            threshold_expression = r$threshold_expression %||% NA_character_,
            message = r$message %||% NA_character_
          )
        })

        dplyr::bind_rows(rows)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$results_to_table"))
    },

    #' @description
    #' Convert quality schema to a tabular format
    #'
    #' @return A tibble representing the quality schema
    schema_to_table = function() {
      quality_schema_to_table(self$quality_schema)
    },

    #' @description
    #' Import quality schema from a table
    #'
    #' @param df A data frame representing the quality schema
    import_schema_from_table = function(df) {

      phr_try({

        schema <- quality_table_to_schema(df)
        self$set_quality_schema(schema)

        phr_message(
          phr_txt(glue::glue("Imported quality schema from table for {self$dataset_name}."))
        )

        invisible(TRUE)

      }, on_error = "abort", origin = paste0(self$dataset_name, "$import_schema_from_table"))
    },

    #' @description
    #' Export Outputs Schema to Table
    #'
    #' Exports the current outputs schema as a data frame table.
    #' This follows the same pattern as export_variable_schema() in Data class.
    #'
    #' @return A tibble representing the outputs schema, or NULL if no schema is defined
    export_outputs_schema = function() {

      phr_try({

        if (is.null(self$outputs_schema) || length(self$outputs_schema) == 0) {
          phr_warning(
            phr_txt("No outputs schema defined for {self$dataset_name}."),
            origin = self$dataset_name
          )
          return(NULL)
        }

        outputs_schema_to_table(self$outputs_schema)

      }, on_error = "abort", origin = paste0(self$dataset_name, "$export_outputs_schema"))
    },

    #' @description
    #' Import Outputs Schema from Table
    #'
    #' Imports an outputs schema from a data frame and attaches it to the object.
    #' This follows the same pattern as import_variable_schema() in Data class.
    #'
    #' @param df A data frame representing the outputs schema
    #' @return The imported schema (invisibly)
    import_outputs_schema = function(df) {

      phr_try({

        phr_validate_dataframe(df, origin = "import_outputs_schema", soft = FALSE)

        # Convert table → structured schema list
        new_schema <- outputs_table_to_schema(df)

        # Assign schema
        self$outputs_schema <- new_schema

        phr_message(
          phr_txt(glue::glue("Outputs schema imported and attached to {self$dataset_name} ({length(new_schema)} output(s))."))
        )

        invisible(new_schema)

      }, on_error = "abort", origin = paste0(self$dataset_name, "$import_outputs_schema"))
    },

    #' @description
    #' Placeholder visualization method (to be overridden by subclasses)
    #'
    #' @param type The type of visualization
    #' @return A plot object or NULL
    visualize = function(type = "summary") {
      phr_message(
        phr_txt("Visualization not implemented in base DataQuality class.")
      )
      invisible(NULL)
    },

    #' @description
    #' Get the actual column name for a canonical variable role
    #'
    #' @param role Character string with canonical variable name (semantic role)
    #' @return Character string with actual column name, or NULL if role not mapped
    get_variable = function(role) {
      if (is.null(role) || length(role) == 0) {
        return(NULL)
      }

      # Return mapped column name, or NULL if not in variable_map
      if (!is.null(self$variable_map) && role %in% names(self$variable_map)) {
        return(self$variable_map[[role]])
      }

      return(NULL)
    },

    #' @description
    #' Translate canonical variable names to actual column names using variable_map
    #'
    #' @param canonical_vars Character vector of canonical variable names
    #' @return Character vector of actual column names (mapped or original if no mapping exists)
    .translate_canonical_to_actual_vars = function(canonical_vars) {

      if (is.null(canonical_vars) || length(canonical_vars) == 0) {
        return(character(0))
      }

      # If no variable_map exists, return canonical names as-is
      if (is.null(self$variable_map) || length(self$variable_map) == 0) {
        return(canonical_vars)
      }

      # Translate each canonical name to actual name using get_variable
      actual_vars <- sapply(canonical_vars, function(canonical_name) {
        # Use get_variable for consistency with Data class
        actual_name <- self$get_variable(canonical_name)

        # Return mapped name if it exists and is not empty, otherwise return canonical name
        if (!is.null(actual_name) && nzchar(actual_name)) {
          return(actual_name)
        } else {
          return(canonical_name)
        }
      }, USE.NAMES = FALSE)

      return(actual_vars)
    },

    #' @description
    #' Run quality checks on data subsets for each unique value of a grouping
    #' column and return a combined per-group results data frame.
    #'
    #' @param group_col Character string. The actual column name in
    #'   \code{self$data} to group by (e.g. the enumerator ID column or stratum
    #'   column resolved from \code{variable_map}).
    #' @return A tibble with columns \code{group_value}, \code{check_name},
    #'   \code{check_label}, \code{check_group}, \code{test_statistic},
    #'   \code{p_value}, \code{penalty}, \code{max_penalty}, or \code{NULL}
    #'   if the column is absent from the data or no non-NA group values exist.
    .compute_results_by_group = function(group_col) {

      phr_try({

        if (is.null(group_col) || !nzchar(group_col) ||
            !group_col %in% names(self$data)) {
          return(NULL)
        }

        group_values <- unique(self$data[[group_col]])
        group_values <- group_values[!is.na(group_values)]

        if (length(group_values) == 0) {
          return(NULL)
        }

        original_data <- self$data
        on.exit(self$data <- original_data)

        all_group_results <- list()

        for (gv in group_values) {
          self$data <- original_data[original_data[[group_col]] == gv, ]

          group_rows <- lapply(names(self$quality_schema), function(check_name) {
            check  <- self$quality_schema[[check_name]]
            result <- self$execute_check(check)
            tibble::tibble(
              group_value    = as.character(gv),
              check_name     = result$check_name     %||% check_name,
              check_label    = result$check_label    %||% NA_character_,
              check_group    = result$check_group    %||% NA_character_,
              test_statistic = as.numeric(result$test_statistic %||% NA),
              p_value        = as.numeric(result$p_value        %||% NA),
              penalty        = as.numeric(result$penalty        %||% 0),
              max_penalty    = as.numeric(result$max_penalty    %||% 0)
            )
          })

          all_group_results <- c(all_group_results, group_rows)
        }

        dplyr::bind_rows(all_group_results)

      }, on_error = "warn",
         origin = paste0(self$dataset_name, "$compute_results_by_group"))
    },

    #' @description
    #' Generate a quality report
    #'
    #' @return A list containing the full quality report
    generate_report = function() {

      phr_try({

        # Ensure checks have been run
        if (length(self$results) == 0) {
          self$run_quality_checks()
        }

        # Ensure summary stats are computed
        if (length(self$summary_stats) == 0) {
          self$compute_summary_stats()
        }

        report <- list(
          summary = self$summary(),
          summary_stats = self$summary_stats,
          results = self$results,
          results_table = self$results_to_table(),
          overall_score = self$overall_score,
          metadata = self$metadata,
          generated_at = Sys.time()
        )

        phr_message(
          phr_txt(glue::glue("Generated quality report for {self$dataset_name}."))
        )

        report

      }, on_error = "warn", origin = paste0(self$dataset_name, "$generate_report"))
    },

    #' @description
    #' Generate a plausibility report with statistical test results
    #'
    #' @return A list containing detailed plausibility assessment
    generate_plausibility_report = function() {

      phr_try({

        # Ensure checks have been run
        if (length(self$results) == 0) {
          self$run_quality_checks()
        }

        # Create summary table of test results
        test_summary <- data.frame(
          check_name = character(),
          check_label = character(),
          test_statistic = numeric(),
          p_value = numeric(),
          threshold_expression = character(),
          penalty = numeric(),
          max_penalty = numeric(),
          message = character(),
          stringsAsFactors = FALSE
        )

        for (check_name in names(self$results)) {
          result <- self$results[[check_name]]

          test_summary <- rbind(test_summary, data.frame(
            check_name = result$check_name %||% check_name,
            check_label = result$check_label %||% "",
            test_statistic = result$test_statistic %||% NA_real_,
            p_value = result$p_value %||% NA_real_,
            threshold_expression = result$threshold_expression %||% "none",
            penalty = result$penalty %||% 0,
            max_penalty = result$max_penalty %||% 0,
            message = result$message %||% "",
            stringsAsFactors = FALSE
          ))
        }

        # Calculate summary statistics
        total_checks <- nrow(test_summary)
        checks_passed <- sum(test_summary$penalty == 0, na.rm = TRUE)
        total_penalty <- sum(test_summary$penalty, na.rm = TRUE)
        total_max_penalty <- sum(test_summary$max_penalty, na.rm = TRUE)

        plausibility_score <- if (total_max_penalty > 0) {
          max(0, 100 - (total_penalty / total_max_penalty * 100))
        } else {
          100
        }

        # Group by threshold expressions
        threshold_summary <- table(test_summary$threshold_expression)

        report <- list(
          dataset_name = self$dataset_name,
          n_checks = total_checks,
          n_checks_passed = checks_passed,
          plausibility_score = round(plausibility_score, 2),
          total_penalty = total_penalty,
          max_possible_penalty = total_max_penalty,
          threshold_distribution = as.list(threshold_summary),
          test_results = test_summary,
          generated_at = Sys.time(),
          schema_version = self$quality_schema$metadata$version %||% "3.0.0"
        )

        phr_message(
          phr_txt(glue::glue("Generated plausibility report for {self$dataset_name}. Score: {round(plausibility_score, 2)}"))
        )

        return(report)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$generate_plausibility_report"))
    },

    #' @description
    #' Run all outputs defined in the outputs schema
    #'
    #' Iterates through the outputs schema, resolves \code{@variable_map},
    #' \code{@value_map}, \code{@variable_label}, \code{@value_label}, and
    #' \code{@results_table} references in \code{test_params}, calls the
    #' specified output function, and stores results in
    #' \code{self$visualizations} (for \code{output_type = "visualization"})
    #' or \code{self$tables} (for \code{output_type = "table"}) using
    #' \code{output_title} (or \code{output_name}) as the label/key.
    #'
    #' Special reference syntax in \code{test_params}:
    #' \describe{
    #'   \item{\code{@variable_map$role}}{Resolves to the mapped column name}
    #'   \item{\code{@value_map$role$value}}{Resolves to dataset values}
    #'   \item{\code{@variable_label$role}}{Resolves to the variable label string}
    #'   \item{\code{@value_label$role$value}}{Resolves to the value label string}
    #'   \item{\code{@results_table}}{Resolves to \code{self$results_to_table()} —
    #'     replaces the first positional argument, suitable for
    #'     \code{table_quality_penalty_summary()}}
    #' }
    #'
    #' @return Invisibly returns a list with \code{visualizations} and \code{tables}
    run_outputs = function() {

      phr_try({

        if (is.null(self$outputs_schema) || length(self$outputs_schema) == 0) {
          phr_warning(
            message = "No outputs defined in outputs schema.",
            origin = self$dataset_name
          )
          return(invisible(list(visualizations = self$visualizations, tables = self$tables)))
        }

        phr_message(phr_txt(glue::glue("Running {length(self$outputs_schema)} output(s) for {self$dataset_name}...")))

        for (out_name in names(self$outputs_schema)) {

          out <- self$outputs_schema[[out_name]]

          phr_try({

            # Get function name
            func_name <- out$output_func_name

            if (is.null(func_name) || is.na(func_name) || !nzchar(func_name)) {
              phr_warning(
                message = phr_txt(glue::glue("Output '{out_name}' has no output_func_name specified. Skipping.")),
                origin = self$dataset_name
              )
              next
            }

            # Resolve function from package namespace or calling environment
            output_function <- NULL

            if (requireNamespace("iphRa", quietly = TRUE)) {
              tryCatch({
                ns <- asNamespace("iphRa")
                if (exists(func_name, envir = ns, mode = "function", inherits = FALSE)) {
                  output_function <- get(func_name, envir = ns, mode = "function", inherits = FALSE)
                }
              }, error = function(e) NULL)
            }

            if (is.null(output_function)) {
              tryCatch({
                if (exists(func_name, mode = "function", inherits = TRUE)) {
                  output_function <- get(func_name, mode = "function", inherits = TRUE)
                }
              }, error = function(e) NULL)
            }

            if (is.null(output_function)) {
              phr_warning(
                message = phr_txt(glue::glue("Function '{func_name}' for output '{out_name}' not found. Skipping.")),
                origin = self$dataset_name
              )
              next
            }

            # Build function arguments starting with the dataset (passed positionally
            # so it matches the first parameter regardless of its name, e.g. df or .dataset).
            # For table outputs using @results_table, this will be overridden below.
            func_args <- list(self$data)

            # OUTPUT ARGUMENT RESOLUTION
            #
            # test_params values can reference variable/value maps using @ syntax:
            #
            # 1. @variable_map$role  → resolves to the mapped column name
            #    Example: "@variable_map$sex" → "sex_col"
            #
            # 2. @value_map$role$canonical_value → resolves to dataset values
            #    Example: "@value_map$sex$male" → c("1", "m", "male")
            #
            # 3. @value_map$role → resolves to the entire value mapping list
            #    Example: "@value_map$sex" → list(male = c(...), female = c(...))
            #
            # 4. c(@variable_map$role1, @value_map$role2$val) → resolves each element
            #    individually and returns a character vector
            #    Example: "c(@variable_map$sex, @variable_map$age)" → c("sex_col", "age_col")
            #
            # 5. @variable_label$role → resolves to the variable label string
            #    Example: "@variable_label$sex" → "Sex of Respondent"
            #
            # 6. @value_label$role$canonical_value → resolves to the value label string
            #    Example: "@value_label$sex$male" → "Male"
            #
            # 7. @value_label$role → resolves to the entire value label named vector
            #    Example: "@value_label$sex" → c(male = "Male", female = "Female")
            #
            # 8. @results_table → resolves to self$results_to_table() (quality check results)
            #    Use as the value of the 'results_df' parameter for table_quality_penalty_summary
            #    Example: test_params = "results_df=@results_table"
            #
            # This mirrors the indicator_schema argument resolution in the Data class,
            # including vector argument handling via .parse_indicator_arguments.

            # Check if any test_param uses @results_table to replace the first positional arg
            if (!is.null(out$test_params) && length(out$test_params) > 0) {
              for (arg_name_check in names(out$test_params)) {
                arg_val_check <- out$test_params[[arg_name_check]]
                if (is.character(arg_val_check) && arg_val_check == "@results_table") {
                  func_args[[1]] <- self$results_to_table()
                  break
                }
              }
            }

            if (!is.null(out$test_params) && length(out$test_params) > 0) {
              for (arg_name in names(out$test_params)) {
                arg_value <- out$test_params[[arg_name]]

                # Check if argument is a vector in c(...) format
                if (is.character(arg_value) && grepl("^c\\(", arg_value)) {
                  # Extract elements from c(...) format
                  vec_content <- sub("^c\\(", "", arg_value)
                  vec_content <- sub("\\)$", "", vec_content)

                  vec_elements <- .parse_indicator_arguments(vec_content)

                  resolved_elements <- character()
                  for (elem in vec_elements) {
                    elem <- trimws(elem)

                    # Resolve @variable_map references within vector
                    if (grepl("^@variable_map\\$", elem)) {
                      role <- sub("^@variable_map\\$", "", elem)
                      resolved <- self$variable_map[[role]]
                      if (!is.null(resolved)) {
                        resolved_elements <- c(resolved_elements, resolved)
                      } else {
                        phr_warning(
                          message = phr_txt(glue::glue("Variable map role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                          origin = self$dataset_name
                        )
                      }
                    }
                    # Resolve @value_map references within vector
                    else if (grepl("^@value_map\\$", elem)) {
                      parts <- strsplit(sub("^@value_map\\$", "", elem), "\\$")[[1]]
                      if (length(parts) >= 1) {
                        role <- parts[1]
                        if (!is.null(self$value_map[[role]])) {
                          if (length(parts) == 2) {
                            # Specific canonical value requested
                            canonical_val <- parts[2]
                            resolved <- self$value_map[[role]][[canonical_val]]
                            if (!is.null(resolved)) {
                              # value_map can itself be a vector, flatten it
                              resolved_elements <- c(resolved_elements, resolved)
                            } else {
                              phr_warning(
                                message = phr_txt(glue::glue("Value map '{elem}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                                origin = self$dataset_name
                              )
                            }
                          } else {
                            # Use entire value map for that role - unlist to flatten the nested list
                            # (e.g., list(yes = c("1","yes"), no = c("0","no")) → c("1","yes","0","no"))
                            resolved <- unlist(self$value_map[[role]], use.names = FALSE)
                            resolved_elements <- c(resolved_elements, resolved)
                          }
                        } else {
                          phr_warning(
                            message = phr_txt(glue::glue("Value map role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                            origin = self$dataset_name
                          )
                        }
                      }
                    }
                    # Resolve @variable_label references within vector
                    else if (grepl("^@variable_label\\$", elem)) {
                      role     <- sub("^@variable_label\\$", "", elem)
                      resolved <- self$variable_label[[role]]
                      if (!is.null(resolved)) {
                        resolved_elements <- c(resolved_elements, resolved)
                      } else {
                        phr_warning(
                          message = phr_txt(glue::glue("Variable label role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                          origin = self$dataset_name
                        )
                      }
                    }
                    # Resolve @value_label references within vector
                    else if (grepl("^@value_label\\$", elem)) {
                      parts <- strsplit(sub("^@value_label\\$", "", elem), "\\$")[[1]]
                      if (length(parts) >= 1) {
                        role <- parts[1]
                        if (!is.null(self$value_label[[role]])) {
                          if (length(parts) == 2) {
                            resolved <- self$value_label[[role]][[parts[2]]]
                            if (!is.null(resolved)) {
                              resolved_elements <- c(resolved_elements, resolved)
                            } else {
                              phr_warning(
                                message = phr_txt(glue::glue("Value label '{elem}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                                origin = self$dataset_name
                              )
                            }
                          } else {
                            resolved_elements <- c(resolved_elements, unlist(self$value_label[[role]], use.names = FALSE))
                          }
                        } else {
                          phr_warning(
                            message = phr_txt(glue::glue("Value label role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                            origin = self$dataset_name
                          )
                        }
                      }
                    } else {
                      # Literal value - remove quotes if present
                      elem <- gsub("^['\"]|['\"]$", "", elem)
                      resolved_elements <- c(resolved_elements, elem)
                    }
                  }

                  func_args[[arg_name]] <- resolved_elements
                }
                # Resolve @variable_label references
                else if (is.character(arg_value) && grepl("^@variable_label\\$", arg_value)) {
                  role <- sub("^@variable_label\\$", "", arg_value)
                  resolved <- self$variable_label[[role]]
                  if (!is.null(resolved)) {
                    func_args[[arg_name]] <- resolved
                  } else {
                    phr_warning(
                      message = phr_txt(glue::glue("Variable label role '{role}' not found for output '{out_name}'. Skipping argument '{arg_name}'.")),
                      origin = self$dataset_name
                    )
                  }
                }
                # Resolve @results_table reference (replaces first positional arg, skip named)
                else if (is.character(arg_value) && arg_value == "@results_table") {
                  # Already handled above (replaces func_args[[1]]); skip adding as named arg
                  next
                }
                # Resolve @value_label references
                else if (is.character(arg_value) && grepl("^@value_label\\$", arg_value)) {
                  parts <- strsplit(sub("^@value_label\\$", "", arg_value), "\\$")[[1]]
                  if (length(parts) >= 1) {
                    role <- parts[1]
                    if (!is.null(self$value_label[[role]])) {
                      if (length(parts) == 2) {
                        canonical_val <- parts[2]
                        resolved <- self$value_label[[role]][[canonical_val]]
                      } else {
                        resolved <- self$value_label[[role]]
                      }
                      if (!is.null(resolved)) {
                        func_args[[arg_name]] <- resolved
                      } else {
                        phr_warning(
                          message = phr_txt(glue::glue("Value label '{arg_value}' not found for output '{out_name}'. Using original value.")),
                          origin = self$dataset_name
                        )
                        func_args[[arg_name]] <- arg_value
                      }
                    } else {
                      phr_warning(
                        message = phr_txt(glue::glue("Value label role '{role}' not found for output '{out_name}'. Using original value.")),
                        origin = self$dataset_name
                      )
                      func_args[[arg_name]] <- arg_value
                    }
                  }
                }
                # Resolve @variable_map references
                else if (is.character(arg_value) && grepl("^@variable_map\\$", arg_value)) {
                  role <- sub("^@variable_map\\$", "", arg_value)
                  resolved <- self$variable_map[[role]]
                  if (!is.null(resolved)) {
                    func_args[[arg_name]] <- resolved
                  } else {
                    phr_warning(
                      message = phr_txt(glue::glue("Variable map role '{role}' not found for output '{out_name}'. Skipping argument '{arg_name}'.")),
                      origin = self$dataset_name
                    )
                  }
                }
                # Resolve @value_map references
                else if (is.character(arg_value) && grepl("^@value_map\\$", arg_value)) {
                  parts <- strsplit(sub("^@value_map\\$", "", arg_value), "\\$")[[1]]
                  if (length(parts) >= 1) {
                    role <- parts[1]
                    if (!is.null(self$value_map[[role]])) {
                      if (length(parts) == 2) {
                        canonical_val <- parts[2]
                        resolved <- self$value_map[[role]][[canonical_val]]
                      } else {
                        resolved <- self$value_map[[role]]
                      }
                      if (!is.null(resolved)) {
                        func_args[[arg_name]] <- resolved
                      } else {
                        phr_warning(
                          message = phr_txt(glue::glue("Value map '{arg_value}' not found for output '{out_name}'. Using original value.")),
                          origin = self$dataset_name
                        )
                        func_args[[arg_name]] <- arg_value
                      }
                    } else {
                      phr_warning(
                        message = phr_txt(glue::glue("Value map role '{role}' not found for output '{out_name}'. Using original value.")),
                        origin = self$dataset_name
                      )
                      func_args[[arg_name]] <- arg_value
                    }
                  }
                }
                else {
                  # Literal value - strip surrounding quotes if present
                  if (is.character(arg_value)) {
                    arg_value <- gsub("^['\"]|['\"]$", "", arg_value)
                  }
                  func_args[[arg_name]] <- arg_value
                }
              }
            }

            # Use output_title as the label, fall back to output_name
            label <- if (!is.null(out$output_title) && !is.na(out$output_title) && nzchar(out$output_title)) {
              out$output_title
            } else {
              out_name
            }

            # Determine outputs_group (optional nested storage level)
            group <- if (!is.null(out$outputs_group) && !is.na(out$outputs_group) && nzchar(out$outputs_group)) {
              out$outputs_group
            } else NULL

            # Helper to store a single result under the correct location
            store_result <- function(result, key) {
              if (!is.null(out$output_type) && out$output_type == "table") {
                if (!is.null(group)) {
                  if (is.null(self$tables[[group]])) self$tables[[group]] <- list()
                  self$tables[[group]][[key]] <- result
                } else {
                  self$tables[[key]] <- result
                }
                phr_message(phr_txt(glue::glue("Table '{key}' stored successfully.")))
              } else if (!is.null(out$output_type) && out$output_type == "visualization") {
                if (!is.null(group)) {
                  if (is.null(self$visualizations[[group]])) self$visualizations[[group]] <- list()
                  self$visualizations[[group]][[key]] <- result
                } else {
                  self$visualizations[[key]] <- result
                }
                phr_message(phr_txt(glue::glue("Visualization '{key}' stored successfully.")))
              } else {
                phr_warning(
                  message = phr_txt(glue::glue(
                    "Output '{out_name}' has unrecognized output_type '{out$output_type}'. ",
                    "Expected 'visualization' or 'table'. Result not stored."
                  )),
                  origin = self$dataset_name
                )
              }
            }

            # Check whether to iterate over per-group values
            per_group_ref <- if (!is.null(out$outputs_per_group) &&
                                 !is.na(out$outputs_per_group) &&
                                 nzchar(out$outputs_per_group)) out$outputs_per_group else NULL

            if (!is.null(per_group_ref)) {

              # Resolve the column name from variable_map if @-syntax is used
              if (grepl("^@variable_map\\$", per_group_ref)) {
                role <- sub("^@variable_map\\$", "", per_group_ref)
                col_name <- self$variable_map[[role]]
                var_label <- role
                if (is.null(col_name)) {
                  phr_warning(
                    message = phr_txt(glue::glue("outputs_per_group role '{role}' not found in variable_map for output '{out_name}'. Skipping per-group iteration.")),
                    origin = self$dataset_name
                  )
                  col_name <- NULL
                }
              } else {
                col_name <- per_group_ref
                var_label <- per_group_ref
              }

              if (!is.null(col_name) && col_name %in% names(self$data)) {
                unique_vals <- unique(self$data[[col_name]])
                unique_vals <- unique_vals[!is.na(unique_vals)]

                phr_message(phr_txt(glue::glue("Calling {func_name} for output '{out_name}' across {length(unique_vals)} group(s) of '{var_label}'...")))

                for (val in unique_vals) {
                  filtered_data <- self$data[self$data[[col_name]] == val, , drop = FALSE]
                  per_group_args <- func_args
                  per_group_args[[1]] <- filtered_data

                  amended_label <- paste0(label, "-", var_label, ".", val)

                  phr_try({
                    output_result <- do.call(output_function, per_group_args)
                    store_result(output_result, amended_label)
                  }, on_error = "warn", origin = paste0(self$dataset_name, "$run_outputs$", out_name, "$", val))
                }

              } else if (!is.null(col_name)) {
                phr_warning(
                  message = phr_txt(glue::glue("Column '{col_name}' for outputs_per_group not found in data for output '{out_name}'. Skipping per-group iteration.")),
                  origin = self$dataset_name
                )
              }

            } else {
              # Standard single output
              phr_message(phr_txt(glue::glue("Calling {func_name} for output '{out_name}'...")))
              output_result <- do.call(output_function, func_args)
              store_result(output_result, label)
            }

          }, on_error = "warn", origin = paste0(self$dataset_name, "$run_outputs$", out_name))
        }

        phr_message(
          phr_txt(glue::glue(
            "run_outputs complete: {length(self$visualizations)} visualization(s), {length(self$tables)} table(s)."
          ))
        )

        invisible(list(visualizations = self$visualizations, tables = self$tables))

      }, on_error = "warn", origin = paste0(self$dataset_name, "$run_outputs"))
    }
  )
)
