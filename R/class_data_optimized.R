#' IPHRA Base Data Class (Performance-Optimized Benchmark Copy)
#'
#' @description
#' `DataOptimized` is a benchmark copy of the [Data] class used to compare
#' performance before/after applying a set of targeted optimizations to the
#' `standardize()`/`clean()` workflow (see `R/class_data.R` for the
#' original). It is functionally identical to `Data`, except for the
#' following internal implementation changes:
#' 1. Uses `expand_select_multiple_optimized()` /
#'    `process_select_multiple_columns_optimized()` (vectorized dummy-column
#'    construction; dummy columns are `cbind()`-ed once instead of per column).
#' 2. `private$..apply_cleaning_changes()` uses a single precomputed
#'    `match()`-based uuid lookup instead of an O(n) `which()` scan per
#'    cleaning log row, and batches the final column writes.
#' 3. Loop-invariant constants (e.g. "other column" name patterns) are
#'    hoisted out of the per-column `standardize()` loop.
#' 4. `private$..map_schema_vars()` accumulates into local
#'    `variable_map`/`value_map` lists and writes back to the
#'    `.variable_map`/`.value_map` active bindings once, instead of once per
#'    matched variable/value; the `escape_regex()`/`extract_tokens()`
#'    helpers are hoisted out of the loop.
#' 5. `private$..map_schema_labels()` uses the same local-accumulation
#'    pattern for `variable_label`/`value_label`.
#'
#' See `tests/manual/benchmark_standardize_clean.R` for a script that runs
#' both `HouseholdData` (original) and `HouseholdDataOptimized` (this class,
#' via its subclass) on identical dummy data and compares timings.
#'
#' The original class-level documentation for `Data` follows, since the
#' public API and behavior are unchanged:
#'
#' The `Data` R6 class is the foundational structure for all IPHRA datasets.
#' It provides a standardized interface for loading, validating, standardizing,
#' cleaning, labeling, schema-checking, linking, and serializing survey data.
#'
#' This class defines the full data lifecycle within the IPHRA toolkit:
#' * `raw_data` -- immutable imported data
#' * `standardized_data` -- after standardization (names, types, values)
#' * `clean_data` -- after cleaning and validation
#' * `metadata` -- synchronized dataset attributes
#'
#' @details
#' Key functionalities include:
#' * Validation of data frame structure and required columns
#' * Type inference and safe coercion during standardization
#' * Schema definition and diagnostics
#' * Label management for variables and values
#' * Cleaning and deletion log scaffolding
#' * Autosave and export helpers
#' * Hash fingerprinting for data integrity
#' * Cross-object linking and validation
#'
#' @field raw_data Original imported data frame (immutable)
#' @field standardized_data Data frame after standardization of names, types, and values
#' @field clean_data Data frame after cleaning operations
#' @field dataset_name Character name of the dataset
#' @field metadata List of metadata attributes
#' @field uuid Character name of the UUID column
#' @field validated Logical indicating if data has been validated
#' @field standardized Logical indicating if data has been standardized
#' @field cleaned Logical indicating if data has been cleaned
#' @field required_columns Character vector of required column names
#' @field other_columns List of other (non-required) column names
#' @field variable_map List mapping variable roles to column names
#' @field value_map List mapping canonical values to dataset values
#' @field variable_label Named list of presentation-ready variable labels (populated by map_schema_labels)
#' @field value_label Named list of presentation-ready value labels for categorical variables (populated by map_schema_labels)
#' @field variable_schema Variable schema with types and allowed values
#' @field indicator_schema Schema for indicator calculations
#' @field dependency_schema Schema for variable dependencies
#' @field cleaning_log CleaningLog R6 object for tracking data corrections
#' @field deletion_log DeletionLog R6 object for tracking deletions
#' @field data_quality_flags Data frame storing quality flags per row
#' @field data_diagnostics List of diagnostic results
#' @field cleaning_log_issues Data frame of cleaning log rows that could not be applied (e.g. coercion failures)
#' @field autosave Logical for automatic saving (if TRUE, snapshots on key steps)
#' @field linked_objects List of linked Data objects with linkage specifications
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(id = 1:5, value = c(3,4,NA,5,6))
#'   d <- Data$new(data = df, dataset_name = "Example")
#'   d$validate()
#'   d$standardize()
#'   d$get_column_info()
#'   d$save_object("ExampleData.rds")
#' }
#'
#' @seealso [HouseholdData], [MortalityHouseholdData], [RosterData]
#' @export
DataOptimized <- R6::R6Class(
  classname = "DataOptimized",
  inherit = Asset,

  public = list(
    # Core data & state
    #
    # NOTE: The following fields are stored in private storage and
    # exposed exclusively through the inherited `Asset` API:
    #
    #   * `raw_data`, `standardized_data`, `clean_data` — data payloads
    #     (also exposed via read/write active bindings for legacy read
    #     access; prefer `get()`/`set()` in new code).
    #   * `variable_schema`, `indicator_schema`, `dependency_schema` —
    #     schemas.
    #   * `uuid`, `required_columns`, `other_columns` — structural
    #     metadata.
    #   * `cleaning_log`, `deletion_log` — nested R6 log objects (invoke
    #     their methods via `call(field = "cleaning_log", member = ...)`).
    #   * `data_quality_flags`, `data_diagnostics`, `cleaning_log_issues`
    #     — diagnostic data.
    #   * `variable_map`, `variable_label`, `value_map`, `value_label` —
    #     mappings and labels.
    #
    # Access these through:
    #   d$get(field = ".variable_map")
    #   d$set(field = ".variable_map", value = list(...))
    #   d$call(field = "cleaning_log", member = "add_row", ...)

    dataset_name = NULL,

    validated = FALSE,
    standardized = FALSE,
    cleaned = FALSE,

    # Persistence & linkage

    autosave = FALSE, # If TRUE, snapshot on key steps
    linked_objects = list(), # For dependency linking (name -> list(obj, by_self, by_other))

    #' @description
    #' Initialize a new Data object with survey data
    #'
    #' @param data Data frame containing the survey data
    #' @param metadata Optional list of metadata attributes
    #' @param dataset_name Character name for the dataset (default: "Data")
    #' @param uuid Character name of the UUID column (required)
    #' @param variable_map Optional named list mapping variable roles to column names
    #' @param value_map Optional named list mapping canonical values to dataset values
    #'
    #' @return A new Data R6 object
    initialize = function(
      data = NULL,
      metadata = NULL,
      dataset_name = "Data",
      uuid = NULL,
      variable_map = NULL,
      value_map = NULL
    ) {
      super$initialize()
      phrutils::phr_try(
        {
          if (is.null(data)) {
            phrutils::phr_error(
              phrutils::phr_txt("No data provided for initialization."),
              origin = dataset_name
            )
          }

          # --- NEW: enforce explicit uuid ---
          if (is.null(uuid) || !is.character(uuid) || length(uuid) != 1) {
            phrutils::phr_error(
              dataset_name,
              phrutils::phr_txt(
                "You must supply a valid uuid column name when creating a Data object."
              )
            )
          }

          phrutils::phr_validate_dataframe(
            data,
            origin = dataset_name,
            soft = FALSE
          )

          if (!uuid %in% names(data)) {
            phrutils::phr_error(
              dataset_name,
              phrutils::phr_txt("UUID column '{uuid}' not found in provided data.")
            )
          }

          private$..raw_data <- data
          private$..standardized_data <- NULL
          private$..clean_data <- NULL

          self$metadata <- metadata %||% list()
          self$dataset_name <- dataset_name
          private$..uuid <- uuid
          private$..required_columns <- uuid
          private$..other_columns <- list()
          self$.variable_map <- if (!is.null(variable_map)) {
            variable_map
          } else {
            list(uuid = uuid)
          }

          # NEW: initialize value_map
          self$.value_map <- if (!is.null(value_map)) value_map else list()

          private$..cleaning_log <- CleaningLog$new(
            log_name = paste0(dataset_name, "_CleaningLog")
          )

          private$..deletion_log <- DeletionLog$new(
            log_name = paste0(dataset_name, "_DeletionLog")
          )

          phrutils::phr_message(phrutils::phr_txt(
            "{dataset_name} initialized with {nrow(data)} records."
          ))

        },
        on_error = "abort",
        origin = paste0(dataset_name, "$initialize")
      )
    },

    # Validation pipeline

    #' Validate Data
    #'
    #' @description
    #' Validates the data structure, required columns, and data quality flags at a specified stage
    #'
    #' @param stage Character string specifying data stage: "raw", "standardized", or "clean".
    #'   If missing, automatically selects stage based on data state.
    #'
    #' @return Logical indicating validation success (invisibly)
    #'
    #' @details
    #' Validation process:
    #' 1. Calls pre_validate() hook for subclass-specific checks
    #' 2. Validates data frame structure and required columns
    #' 3. Checks quality flags if present
    #' 4. Calls post_validate() hook for additional validation
    #' 5. Updates metadata and sets validated flag
    validate = function(stage = c("raw", "standardized", "clean")) {
      validated <- TRUE
      had_warnings <- FALSE

      phrutils::phr_try(
        {
          phrutils::phr_message(phrutils::phr_txt(
            "Starting validation for {self$dataset_name}..."
          ))

          # Auto-select stage based on data state if default stage argument is used
          if (missing(stage)) {
            if (self$cleaned) {
              stage <- "clean"
            } else if (self$standardized) {
              stage <- "standardized"
            } else {
              stage <- "raw"
            }
          } else {
            stage <- match.arg(stage)
          }

          df <- self$get_data(stage)

          self$pre_validate()

          # Core checks
          phrutils::phr_validate_dataframe(
            df,
            origin = self$dataset_name,
            soft = FALSE
          )

          # Soft validations - warnings only, capture return status
          cols_valid <- phrutils::phr_validate_columns(
            df,
            required_cols = private$..required_columns,
            origin = self$dataset_name,
            soft = TRUE
          )
          if (!isTRUE(cols_valid)) {
            had_warnings <- TRUE
          }

          if (!private$..uuid %in% names(df)) {
            phrutils::phr_error(
              self$dataset_name,
              phrutils::phr_txt("UUID column '{private$..uuid}' not found in dataset.")
            )
          }

          no_missing_valid <- phrutils::phr_validate_no_missing(
            df,
            cols = private$..uuid,
            origin = self$dataset_name,
            soft = TRUE
          )
          if (!isTRUE(no_missing_valid)) {
            had_warnings <- TRUE
          }

          unique_valid <- phrutils::phr_validate_unique(
            df,
            cols = private$..uuid,
            origin = self$dataset_name,
            soft = TRUE
          )
          if (!isTRUE(unique_valid)) {
            had_warnings <- TRUE
          }

          # VARIABLE MAP CHECKS

          mapped <- unlist(self$.variable_map, use.names = FALSE)
          missing_mapped <- setdiff(mapped, names(df))

          if (length(missing_mapped) > 0) {
            phrutils::phr_warning(
              self$dataset_name,
              phrutils::phr_txt(
                "Mapped columns missing: {paste(missing_mapped, collapse=', ')}"
              )
            )
            had_warnings <- TRUE
          }

          # VALUE MAP CHECKS

          if (length(self$.value_map) > 0) {
            for (var in names(self$.value_map)) {
              mapped_values <- self$.value_map[[var]]

              # Missing role in variable_map
              if (!var %in% names(self$.variable_map)) {
                phrutils::phr_warning(
                  self$dataset_name,
                  phrutils::phr_txt(
                    "Value map role '{var}' is not linked to any variable_map entry."
                  )
                )
                had_warnings <- TRUE
                next
              }

              dataset_col <- self$.variable_map[[var]]

              if (!dataset_col %in% names(df)) {
                phrutils::phr_warning(
                  self$dataset_name,
                  phrutils::phr_txt(
                    "Value map references column '{dataset_col}' which is missing."
                  )
                )
                had_warnings <- TRUE
                next
              }

              dataset_vals <- unique(df[[dataset_col]])

              # Check if this is a select_multiple variable
              # For select_multiple, values are space-separated, so we need to extract tokens
              is_select_multiple <- private$..is_select_multiple(var)
              if (is_select_multiple) {
                # Extract individual tokens from space-separated values
                dataset_vals <- private$..extract_select_multiple_tokens(
                  dataset_vals
                )
              }

              # Detect format: nested (new) vs flat (old)
              # Nested format: list with named elements where names are canonical values
              # Flat format: atomic vector of dataset values
              is_nested_format <- is.list(mapped_values) &&
                !is.null(names(mapped_values)) &&
                length(names(mapped_values)) > 0 &&
                all(names(mapped_values) != "")

              if (is_nested_format) {
                # New nested format: canonical_value -> dataset_values
                # Flatten all dataset values for checking
                all_dataset_mapped_vals <- unlist(
                  mapped_values,
                  recursive = FALSE,
                  use.names = FALSE
                )
                missing_vals <- setdiff(all_dataset_mapped_vals, dataset_vals)

                if (length(missing_vals) > 0) {
                  phrutils::phr_warning(
                    self$dataset_name,
                    phrutils::phr_txt(
                      "Value map for role '{var}' has values not found in dataset: {paste(missing_vals, collapse=', ')}"
                    )
                  )
                  had_warnings <- TRUE
                }
              } else {
                # Old flat format: just a vector of values
                missing_vals <- setdiff(mapped_values, dataset_vals)

                if (length(missing_vals) > 0) {
                  phrutils::phr_warning(
                    self$dataset_name,
                    phrutils::phr_txt(
                      "Value map for role '{var}' has values not found in dataset: {paste(missing_vals, collapse=', ')}"
                    )
                  )
                  had_warnings <- TRUE
                }
              }
            }
          }

          # POST VALIDATION

          pv <- self$post_validate(df)

          # Combine everything
          validated <- (!had_warnings) && pv
          self$validated <- validated

          phrutils::phr_message(phrutils::phr_txt(
            "{self$dataset_name} validation complete."
          ))

        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$validate")
      )

      invisible(self$validated)
    },

    #' Pre-validation Hook
    #'
    #' @description
    #' Hook method called before validation begins. Override in subclasses for custom pre-validation logic.
    #'
    #' @return NULL (default implementation does nothing)
    pre_validate = function() {}, # subclass hook

    #' Post-validation Hook
    #'
    #' @description
    #' Hook method called after core validation completes. Override in subclasses for additional validation.
    #'
    #' @param df Data frame that was validated
    #'
    #' @return Logical indicating validation success (TRUE by default)
    post_validate = function(df) {
      # Default: no additional post-validation in base class
      return(TRUE)
    },

    # Standardize (adds type coercion helper)

    #' Standardize Data
    #'
    #' @description
    #' Standardizes column names, types, and values according to variable schema and mappings
    #'
    #' @param skip Character vector of standardization steps to skip (currently unused placeholder)
    #' @param stage Character string specifying source stage: "clean", "standardized", or "raw" (default: "clean")
    #'
    #' @return Invisible self for method chaining
    #'
    #' @details
    #' Standardization process:
    #' 1. Calls pre_standardize() hook
    #' 2. Copies source data to standardized_data
    #' 3. Standardizes column names (lowercase, snake_case)
    #' 4. Coerces types based on variable schema
    #' 5. Standardizes categorical values using value_map
    #' 6. Calls post_standardize() and post_standardize_domain() hooks
    #' 7. Updates metadata and sets standardized flag
    standardize = function(
      skip = c(
        # "start", "end"
      ),
      stage = c("clean", "standardized", "raw")
    ) {
      stage <- match.arg(stage)

      phrutils::phr_try(
        {
          # Ensure raw_data exists
          result <- phrutils::phr_try_step(
            {
              if (is.null(private$..raw_data)) {
                phrutils::phr_error(
                  "Raw data is NULL; cannot standardize.",
                  origin = paste0(self$dataset_name, "$standardize"),
                  hint = "Raw dataset has been removed or corrupted. Reinitialize the Data object."
                )
              }
            },
            step = "Check raw data",
            hint = phrutils::phr_txt("Ensure raw data exists before standardization")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          # Run validation to ensure data is validated before standardization
          result <- phrutils::phr_try_step(
            {
              self$validate()

              if (!self$validated) {
                phrutils::phr_warning(
                  self$dataset_name,
                  phrutils::phr_txt(
                    "Data validation failed. Proceed with standardization with caution."
                  )
                )
              }
            },
            step = "Validation",
            hint = phrutils::phr_txt("Check data structure and required columns")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          # Run pre-standardize hook (subclass extension point)
          result <- phrutils::phr_try_step(
            {
              self$pre_standardize(stage = stage)
            },
            step = "Pre-standardize hook",
            hint = phrutils::phr_txt("Subclass-specific setup before standardization")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          # Update variable and value maps after pre_standardize
          # (in case pre_standardize added new columns or values)
          result <- phrutils::phr_try_step(
            {
              private$..map_schema_vars(stage = "raw")
              private$..map_schema_labels()
            },
            step = "Map schema variables",
            hint = phrutils::phr_txt("Auto-map schema variables to dataset columns")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          phrutils::phr_message(phrutils::phr_txt("Standardizing {self$dataset_name}..."))

          data_copy <- private$..raw_data
          sch <- private$..variable_schema %||% list()

          # normalize skip vector
          skip <- intersect(skip, names(data_copy))

          # reset other-columns tracker
          private$..other_columns <- list()

          schema_cols <- names(sch$types %||% list())

          # OPTIMIZATION (#3): Hoist loop-invariant constants out of the
          # per-column loop below. `other_col_patterns` was previously
          # re-created as a new vector on every single loop iteration even
          # though its value never changes.
          other_col_patterns <- c(
            "_other_text$",
            "_other_specify$",
            "_other_value$",
            "_autre$",
            "_other$"
          )

          # ---- iterate over each column with type coercion
          result <- phrutils::phr_try_step(
            {
              for (nm in names(data_copy)) {
                # skip designated columns entirely
                if (nm %in% skip) {
                  next
                }

                col <- data_copy[[nm]]
                origin_tag <- paste0(self$dataset_name, "$standardize$", nm)

                # (a) SCHEMA TYPE CONVERSION

                if (nm %in% schema_cols) {
                  want <- sch$types[[nm]]

                  if (phrutils::is_safely_coercible(col, want)) {
                    # numeric
                    if (want == "numeric") {
                      new <- suppressWarnings(as.numeric(col))

                      # character
                    } else if (want == "character") {
                      new <- trimws(as.character(col))

                      # logical (explicit TRUE and FALSE mapping)
                    } else if (want == "logical") {
                      lc <- tolower(trimws(as.character(col)))
                      true_vals <- c("true", "t", "yes", "y", "1")
                      false_vals <- c("false", "f", "no", "n", "0")
                      new <- ifelse(
                        lc %in% true_vals,
                        TRUE,
                        ifelse(lc %in% false_vals, FALSE, NA)
                      )

                      # date
                    } else if (want == "date") {
                      new <- phrutils::phr_convert_date(col)

                      # datetime
                    } else if (want == "datetime") {
                      new <- phrutils::phr_convert_datetime(col)

                      # fallback
                    } else {
                      new <- trimws(as.character(col))
                    }

                    data_copy[[nm]] <- new
                    next # IMPORTANT: do not continue to inference
                  } else {
                    # NOT safely coercible
                    phrutils::phr_warning(
                      self$dataset_name,
                      phrutils::phr_txt(
                        "Column '{nm}' cannot be safely coerced to schema type '{want}'. Leaving as-is."
                      )
                    )
                    next # do NOT attempt further inference
                  }
                }

                # (b) INFERENCE-BASED COERCION (non-schema columns only)

                inferred <- tryCatch(
                  phrutils::phr_infer_column_type(col, name = nm),
                  error = function(e) "character"
                )

                # numeric
                if (
                  identical(inferred, "numeric") &&
                    phrutils::is_safely_coercible(col, "numeric")
                ) {
                  new <- suppressWarnings(as.numeric(col))

                  # logical
                } else if (
                  identical(inferred, "logical") &&
                    phrutils::is_safely_coercible(col, "logical")
                ) {
                  lc <- tolower(trimws(as.character(col)))
                  true_vals <- c("true", "t", "yes", "y", "1")
                  false_vals <- c("false", "f", "no", "n", "0")
                  new <- ifelse(
                    lc %in% true_vals,
                    TRUE,
                    ifelse(lc %in% false_vals, FALSE, NA)
                  )

                  # date
                } else if (
                  identical(inferred, "date") &&
                    phrutils::is_safely_coercible(col, "Date")
                ) {
                  new <- phrutils::phr_convert_date(col)

                  # fallback → clean character
                } else {
                  new <- trimws(as.character(col))
                }

                data_copy[[nm]] <- new

                # (d) DETECT LIKELY "OTHER" COLUMNS (non-schema columns only)

                if (!(nm %in% schema_cols)) {
                  # Check if column name matches "other" patterns
                  # Common patterns: var_other_text -> var, var_other -> var
                  matches_other_pattern <- FALSE
                  inferred_links <- character(0)

                  for (pattern in other_col_patterns) {
                    if (grepl(pattern, nm)) {
                      matches_other_pattern <- TRUE
                      base_name <- sub(pattern, "", nm)
                      if (base_name %in% names(data_copy)) {
                        inferred_links <- c(inferred_links, base_name)
                      }
                      break
                    }
                  }

                  # Only consider as "other" column if:
                  # 1. Column name matches an "other" pattern
                  # 2. Column is not numeric (other responses are typically text)
                  # 3. Has some meaningful data (at least one non-empty value)
                  if (matches_other_pattern) {
                    is_numeric_col <- is.numeric(new)
                    blank_pct <- mean(is.na(new) | new == "")
                    uniq_n <- length(unique(new[!is.na(new) & new != ""]))

                    # Add to other_columns if not numeric and has at least one unique value
                    if (!is_numeric_col && uniq_n > 0) {
                      private$..other_columns[[nm]] <- list(
                        other_column = nm,
                        other_linked_columns = inferred_links
                      )
                    }
                  }
                }
              }
            },
            step = "Type coercion and other column detection",
            hint = phrutils::phr_txt("Check schema types and column conversion logic")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          # (e) PROCESS SELECT_MULTIPLE COLUMNS
          # If schema has question_types with select_multiple, expand those columns

          result <- phrutils::phr_try_step(
            {
              if (
                !is.null(sch$question_types) && length(sch$question_types) > 0
              ) {
                sm_result <- process_select_multiple_columns_optimized(data_copy, sch)

                if (length(sm_result$expanded_columns) > 0) {
                  phrutils::phr_message(
                    phrutils::phr_txt(
                      "Expanded {length(sm_result$expanded_columns)} dummy columns from select_multiple questions."
                    )
                  )
                }

                # Track "other" related columns from select_multiple
                if (length(sm_result$other_related_columns) > 0) {
                  for (var_info in sm_result$other_related_columns) {
                    # For select_multiple with "other", create entry with text column as main
                    # and dummy + original select_multiple as linked columns

                    text_col <- var_info$text_other_column

                    # If there's a text column, use it as the main other_column
                    if (!is.null(text_col) && text_col != "") {
                      linked_cols <- c(var_info$original_column)
                      if (!is.null(var_info$dummy_other_column)) {
                        linked_cols <- c(
                          linked_cols,
                          var_info$dummy_other_column
                        )
                      }

                      private$..other_columns[[text_col]] <- list(
                        other_column = text_col,
                        other_linked_columns = linked_cols
                      )
                    } else {
                      # If no text column, use dummy column as main with original as linked
                      dummy_col <- var_info$dummy_other_column
                      if (!is.null(dummy_col)) {
                        private$..other_columns[[dummy_col]] <- list(
                          other_column = dummy_col,
                          other_linked_columns = c(var_info$original_column)
                        )
                      }
                    }
                  }

                  phrutils::phr_message(
                    phrutils::phr_txt(
                      "Tracked {length(sm_result$other_related_columns)} select_multiple column(s) with 'other' responses."
                    )
                  )
                }

                # Return the modified data
                sm_result$data
              } else {
                # No select_multiple columns to process
                data_copy
              }
            },
            step = "Process select_multiple columns",
            hint = phrutils::phr_txt("Check select_multiple schema and expansion logic")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }
          # Update data_copy with result from step
          data_copy <- result

          # (f) ADD SCHEMA-IDENTIFIED "OTHER" COLUMNS
          # If schema has is_other field, add those to private$..other_columns

          result <- phrutils::phr_try_step(
            {
              if (!is.null(sch$is_other) && length(sch$is_other) > 0) {
                # More efficient: directly filter TRUE values
                schema_other_cols <- names(sch$is_other)[unlist(
                  sch$is_other,
                  use.names = FALSE
                )]
                # Only include columns that exist in the dataset
                schema_other_cols <- schema_other_cols[
                  schema_other_cols %in% names(data_copy)
                ]

                if (length(schema_other_cols) > 0) {
                  for (col in schema_other_cols) {
                    # Skip if already added by inference or select_multiple processing
                    if (col %in% names(private$..other_columns)) {
                      next
                    }

                    # Get linked columns from schema if available
                    linked_cols <- character(0)
                    if (
                      !is.null(sch$other_column_link) &&
                        col %in% names(sch$other_column_link)
                    ) {
                      linked_cols <- sch$other_column_link[[col]]
                      # Ensure linked columns exist in dataset
                      linked_cols <- linked_cols[
                        linked_cols %in% names(data_copy)
                      ]
                    }

                    # Add as list entry
                    private$..other_columns[[col]] <- list(
                      other_column = col,
                      other_linked_columns = linked_cols
                    )
                  }
                  phrutils::phr_message(
                    phrutils::phr_txt(
                      "Added {length(schema_other_cols)} schema-identified 'other' columns."
                    )
                  )
                }
              }
            },
            step = "Add schema-identified other columns",
            hint = phrutils::phr_txt("Check schema is_other field")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          # (e.1) Process indicator schema (if present) - BEFORE assigning standardized data
          result <- phrutils::phr_try_step(
            {
              # Start with current data
              working_data <- data_copy

              if (
                !is.null(private$..indicator_schema) &&
                  length(private$..indicator_schema) > 0
              ) {
                phrutils::phr_message(phrutils::phr_txt(
                  "Processing {length(private$..indicator_schema)} indicator(s) from indicator schema..."
                ))

                for (ind_name in names(private$..indicator_schema)) {
                  ind <- private$..indicator_schema[[ind_name]]

                  phrutils::phr_try(
                    {
                      # Get function name (should start with add_)
                      func_name <- ind$function_name

                      if (
                        is.null(func_name) ||
                          func_name == "" ||
                          is.na(func_name)
                      ) {
                        phrutils::phr_warning(
                          self$dataset_name,
                          phrutils::phr_txt(
                            "Indicator '{ind_name}' has no function_name specified. Skipping."
                          )
                        )
                        next
                      }

                      # Check if function exists
                      if (!exists(func_name, mode = "function")) {
                        phrutils::phr_warning(
                          self$dataset_name,
                          phrutils::phr_txt(
                            "Function '{func_name}' for indicator '{ind_name}' not found. Skipping."
                          )
                        )
                        next
                      }

                      # Check if required variables exist in working_data
                      # Variables in indicator schema are canonical names, so resolve to mapped names
                      required_vars <- ind$variables
                      if (
                        !is.null(required_vars) && length(required_vars) > 0
                      ) {
                        # Resolve canonical variable names to mapped column names using vectorized operations

                        # Check which variables have valid mappings
                        has_mapping <- required_vars %in%
                          names(self$.variable_map)

                        # Get mapped values for variables that have mappings
                        mapped_values <- lapply(
                          required_vars[has_mapping],
                          function(var) {
                            val <- self$.variable_map[[var]]
                            if (!is.null(val) && val != "") val else NULL
                          }
                        )

                        # Filter out NULL/empty mappings
                        valid_mapped <- !sapply(mapped_values, is.null)
                        mapped_vars <- unlist(mapped_values[valid_mapped])

                        # Identify variables with missing or invalid mappings
                        invalid_mapped_vars <- required_vars[has_mapping][
                          !valid_mapped
                        ]
                        unmapped_vars <- required_vars[!has_mapping]
                        missing_canonical_vars <- c(
                          unmapped_vars,
                          invalid_mapped_vars
                        )

                        # Check if any canonical variables lack valid mapping
                        if (length(missing_canonical_vars) > 0) {
                          phrutils::phr_warning(
                            self$dataset_name,
                            phrutils::phr_txt(
                              "Indicator '{ind_name}' requires variables not mapped in variable_map: {paste(missing_canonical_vars, collapse=', ')}. Skipping."
                            )
                          )
                          next
                        }

                        # Check if mapped columns exist in the dataset
                        missing_cols <- setdiff(
                          mapped_vars,
                          names(working_data)
                        )
                        if (length(missing_cols) > 0) {
                          phrutils::phr_warning(
                            self$dataset_name,
                            phrutils::phr_txt(
                              "Indicator '{ind_name}' requires columns not present in dataset: {paste(missing_cols, collapse=', ')}. Skipping."
                            )
                          )
                          next
                        }
                      }

                      # Prepare arguments for the function call (using working_data, not private$..standardized_data)
                      func_args <- list(.dataset = working_data)

                      # Add user-specified arguments

                      # INDICATOR ARGUMENT RESOLUTION

                      # Indicator arguments can reference mapped variables and values using
                      # explicit @ syntax:
                      #
                      # 1. @variable_map$role → Resolves to dataset column name
                      #    Example: "@variable_map$fever" → "q7_fever_column"
                      #
                      # 2. @value_map$role$canonical_value → Resolves to dataset values
                      #    Example: "@value_map$fever$yes" → c("yes", "y", "oui")
                      #
                      # 3. @value_map$role → Resolves to entire value mapping
                      #    Example: "@value_map$fever" → list(yes = c(...), no = c(...))
                      #
                      # This explicit syntax ensures clarity about which arguments are mapped
                      # vs literal values, which is important for function parameters.
                      #
                      # For comprehensive documentation, see:
                      # docs/variable_value_mapping_guide.md

                      if (
                        !is.null(ind$arguments) && length(ind$arguments) > 0
                      ) {
                        for (arg_name in names(ind$arguments)) {
                          arg_value <- ind$arguments[[arg_name]]

                          # Check if argument is a vector in c(...) format
                          if (
                            is.character(arg_value) && grepl("^c\\(", arg_value)
                          ) {
                            # Extract elements from c(...) format
                            vec_content <- sub("^c\\(", "", arg_value)
                            vec_content <- sub("\\)$", "", vec_content)

                            # Split by comma while respecting nested parentheses (if any)
                            # Note: .parse_indicator_arguments is called here for the second time
                            # (first was to parse the full argument string into pairs).
                            # This is intentional: we need to parse vector elements separately
                            # because each element may be a @variable_map or @value_map reference
                            # that requires individual resolution.
                            vec_elements <- .parse_indicator_arguments(
                              vec_content
                            )

                            # Resolve each element using the shared
                            # private$..resolve_variable_map_reference() /
                            # private$..resolve_value_map_reference() helpers.
                            resolved_elements <- character()
                            for (elem in vec_elements) {
                              elem <- trimws(elem)

                              # Resolve @variable_map references
                              var_ref <- private$..resolve_variable_map_reference(
                                elem,
                                tags = TRUE
                              )
                              val_ref <- private$..resolve_value_map_reference(
                                elem,
                                tags = TRUE
                              )

                              if (var_ref$is_reference) {
                                if (!is.null(var_ref$value)) {
                                  resolved_elements <- c(
                                    resolved_elements,
                                    var_ref$value
                                  )
                                } else {
                                  phrutils::phr_warning(
                                    self$dataset_name,
                                    phrutils::phr_txt(
                                      "Variable map role '{var_ref$role}' not found in vector argument '{arg_name}' for indicator '{ind_name}'."
                                    )
                                  )
                                }
                              } else if (val_ref$is_reference) {
                                # Resolve @value_map references
                                if (val_ref$role_found) {
                                  if (!is.null(val_ref$canonical_value)) {
                                    # Specific canonical value requested
                                    if (!is.null(val_ref$value)) {
                                      # value_map can itself be a vector, flatten it
                                      resolved_elements <- c(
                                        resolved_elements,
                                        val_ref$value
                                      )
                                    } else {
                                      phrutils::phr_warning(
                                        self$dataset_name,
                                        phrutils::phr_txt(
                                          "Value map '{elem}' not found in vector argument '{arg_name}' for indicator '{ind_name}'."
                                        )
                                      )
                                    }
                                  } else {
                                    # Use entire value map for that role - this returns all values
                                    resolved <- unlist(
                                      val_ref$value,
                                      use.names = FALSE
                                    )
                                    resolved_elements <- c(
                                      resolved_elements,
                                      resolved
                                    )
                                  }
                                } else {
                                  phrutils::phr_warning(
                                    self$dataset_name,
                                    phrutils::phr_txt(
                                      "Value map role '{val_ref$role}' not found in vector argument '{arg_name}' for indicator '{ind_name}'."
                                    )
                                  )
                                }
                              } else {
                                # Literal value - remove quotes if present
                                elem <- gsub("^['\"]|['\"]$", "", elem)
                                resolved_elements <- c(resolved_elements, elem)
                              }
                            }

                            numeric_attempt <- suppressWarnings(as.numeric(
                              resolved_elements
                            ))
                            if (
                              length(resolved_elements) > 0 &&
                                !any(is.na(numeric_attempt))
                            ) {
                              func_args[[arg_name]] <- numeric_attempt
                            } else if (
                              length(resolved_elements) > 0 &&
                                all(
                                  toupper(resolved_elements) %in%
                                    c("TRUE", "FALSE")
                                )
                            ) {
                              func_args[[arg_name]] <- as.logical(
                                resolved_elements
                              )
                            } else {
                              func_args[[arg_name]] <- resolved_elements
                            }
                          } else if (
                            is.character(arg_value) &&
                              grepl("^@variable_map\\$", arg_value)
                          ) {
                            # Resolve variable_map references (e.g., "@variable_map$fsl_fcs_cereal")
                            var_ref <- private$..resolve_variable_map_reference(
                              arg_value,
                              tags = TRUE
                            )
                            if (!is.null(var_ref$value)) {
                              func_args[[arg_name]] <- var_ref$value
                            } else {
                              phrutils::phr_warning(
                                self$dataset_name,
                                phrutils::phr_txt(
                                  "Variable map role '{var_ref$role}' not found for indicator '{ind_name}'. Passing NULL for optional parameter '{arg_name}'."
                                )
                              )
                              func_args[[arg_name]] <- NULL
                            }
                          } else if (
                            is.character(arg_value) &&
                              grepl("^@value_map\\$", arg_value)
                          ) {
                            # Resolve value_map references (e.g., "@value_map$status$yes")
                            val_ref <- private$..resolve_value_map_reference(
                              arg_value,
                              tags = TRUE
                            )
                            if (val_ref$role_found) {
                              if (!is.null(val_ref$value)) {
                                func_args[[arg_name]] <- val_ref$value
                              } else {
                                phrutils::phr_warning(
                                  self$dataset_name,
                                  phrutils::phr_txt(
                                    "Value map '{arg_value}' not found for indicator '{ind_name}'. Using original value."
                                  )
                                )
                                func_args[[arg_name]] <- arg_value
                              }
                            } else {
                              phrutils::phr_warning(
                                self$dataset_name,
                                phrutils::phr_txt(
                                  "Value map role '{val_ref$role}' not found for indicator '{ind_name}'. Using original value."
                                )
                              )
                              func_args[[arg_name]] <- arg_value
                            }
                          } else {
                            # Use literal value
                            func_args[[arg_name]] <- arg_value
                          }
                        }
                      }

                      # Call the add_ function
                      phrutils::phr_message(phrutils::phr_txt(
                        "Calling {func_name} for indicator '{ind_name}'..."
                      ))
                      ind_result <- do.call(func_name, func_args)

                      # Update working_data with result (not private$..standardized_data)
                      if (is.data.frame(ind_result)) {
                        working_data <- ind_result
                        phrutils::phr_message(phrutils::phr_txt(
                          "Indicator '{ind_name}' computed successfully."
                        ))

                        # Update variable and value maps after each indicator
                        # Temporarily assign working_data to standardized_data so map_schema_vars can access it
                        temp_standardized <- private$..standardized_data
                        private$..standardized_data <- working_data

                        phrutils::phr_try(
                          {
                            private$..map_schema_vars(stage = "standardized")
                            private$..map_schema_labels()
                          },
                          on_error = "warn",
                          origin = paste0(
                            self$dataset_name,
                            "$standardize$map_vars_",
                            ind_name
                          )
                        )

                        # Restore original standardized_data (will be set properly at the end)
                        private$..standardized_data <- temp_standardized
                      } else {
                        phrutils::phr_warning(
                          self$dataset_name,
                          phrutils::phr_txt(
                            "Function '{func_name}' did not return a data frame. Result ignored."
                          )
                        )
                      }
                    },
                    on_error = "warn",
                    origin = paste0(
                      self$dataset_name,
                      "$standardize$indicator$",
                      ind_name
                    )
                  )
                }
              }

              # Return the final working data
              working_data
            },
            step = "Process indicator schema",
            hint = phrutils::phr_txt("Check indicator functions and required variables")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }
          # Update data_copy with result from step
          data_copy <- result

          # (e.2) GENERATE cluster_id_numeric FROM cluster_id
          # If cluster_id is mapped, create a safe numeric cluster identifier
          # that maps each unique cluster to an integer from 1 to n clusters.
          # This column is stored as "cluster_id_numeric" and used by
          # DataAnalytics$create_survey_design() for reliable survey design creation.
          result <- phrutils::phr_try_step(
            {
              cluster_col <- self$.variable_map[["cluster_id"]]
              if (!is.null(cluster_col) && cluster_col %in% names(data_copy)) {
                cluster_vals <- data_copy[[cluster_col]]
                # Build a lookup: each unique (non-NA) cluster gets a sequential integer
                unique_clusters <- sort(unique(cluster_vals[
                  !is.na(cluster_vals)
                ]))
                numeric_map <- stats::setNames(
                  seq_along(unique_clusters),
                  unique_clusters
                )
                data_copy[["cluster_id_numeric"]] <- ifelse(
                  is.na(cluster_vals),
                  NA_integer_,
                  as.integer(numeric_map[as.character(cluster_vals)])
                )
                self$.variable_map[[
                  "cluster_id_numeric"
                ]] <- "cluster_id_numeric"
                phrutils::phr_message(
                  phrutils::phr_txt(
                    "Created cluster_id_numeric with {length(unique_clusters)} unique cluster(s)."
                  )
                )
              }
              data_copy
            },
            step = "Generate cluster_id_numeric",
            hint = phrutils::phr_txt("Check variable_map for cluster_id role")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }
          data_copy <- result

          # assign standardized data
          private$..standardized_data <- data_copy
          self$standardized <- TRUE
          phrutils::phr_message(phrutils::phr_txt(
            "{self$dataset_name} standardization complete."
          ))

          # (f) Run quality checks automatically if schema exists
          result <- phrutils::phr_try_step(
            {
              if (
                !is.null(private$..standardized_data) &&
                  !is.null(private$..variable_schema)
              ) {
                phrutils::phr_message(phrutils::phr_txt(
                  "Running quality checks on {self$dataset_name}..."
                ))
                self$run_quality_checks(stage = "standardized")
              }
            },
            step = "Run quality checks",
            hint = phrutils::phr_txt("Check dependency schema and type validations")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          # (g) Subclass extension hook
          result <- phrutils::phr_try_step(
            {
              self$post_standardize()
            },
            step = "Post-standardize hook",
            hint = phrutils::phr_txt("Subclass-specific processing after standardization")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          # Update variable and value maps after post_standardize
          # (in case post_standardize added new columns or values)
          result <- phrutils::phr_try_step(
            {
              private$..map_schema_vars(stage = "standardized")
              private$..map_schema_labels()
            },
            step = "Update variable and value maps (final)",
            hint = phrutils::phr_txt("Final map update after post-standardize")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$standardize")
      )
    },

    #' Pre-standardization Hook
    #'
    #' @description
    #' Hook method called before standardization begins. Override in subclasses for custom pre-processing.
    #'
    #' @param stage Character string specifying source stage: "clean", "standardized", or "raw"
    #'
    #' @return NULL (default implementation does nothing)
    pre_standardize = function(stage = c("clean", "standardized", "raw")) {}, # subclass hook - called before standardization begins

    #' Post-standardization Hook
    #'
    #' @description
    #' Hook method called after standardization completes. Override in subclasses for custom post-processing.
    #'
    #' @return NULL (default implementation does nothing)
    post_standardize = function() {}, # subclass hook

    # Cleaning

    #' Clean Data
    #'
    #' @description
    #' Applies cleaning operations from cleaning_log and deletion_log to standardized data
    #'
    #' @return Invisible self for method chaining
    #'
    #' @details
    #' Cleaning process:
    #' 1. Validates data if not already validated
    #' 2. Ensures data is standardized
    #' 3. Copies standardized_data to clean_data
    #' 4. Applies cleaning log edits (if present)
    #' 5. Applies deletion log removals (if present)
    #' 6. Updates metadata and sets cleaned flag
    clean = function() {
      phrutils::phr_try(
        {
          # Run validation to ensure data is validated before cleaning
          self$validate()

          if (!self$validated) {
            phrutils::phr_warning(
              message = phrutils::phr_txt(
                "Data validation failed. Proceeding with cleaning with caution."
              ),
              origin = paste0(self$dataset_name, "$clean")
            )
          }

          if (!self$standardized) {
            phrutils::phr_warning(
              message = phrutils::phr_txt(
                "Data should be standardized before cleaning. Using fallback."
              ),
              origin = paste0(self$dataset_name, "$clean")
            )
          }

          phrutils::phr_message(phrutils::phr_txt(
            "Starting cleaning for {self$dataset_name}..."
          ))

          # BASELINE CLEAN DATA (robust fallback)

          if (
            !is.null(private$..standardized_data) &&
              is.data.frame(private$..standardized_data)
          ) {
            private$..clean_data <- private$..standardized_data
          } else {
            # Only warn if standardized_data should have existed
            if (!self$standardized) {
              phrutils::phr_warning(
                message = phrutils::phr_txt(
                  "Data should be standardized before cleaning. Using fallback."
                ),
                origin = paste0(self$dataset_name, "$clean")
              )
            }

            if (
              !is.null(private$..standardized_data) &&
                !is.data.frame(private$..standardized_data)
            ) {
              phrutils::phr_warning(
                message = phrutils::phr_txt(
                  "Standardized data is invalid or corrupted; falling back to raw data."
                ),
                origin = paste0(self$dataset_name, "$clean")
              )
            }

            private$..clean_data <- private$..raw_data
          }

          # VALIDATE & APPLY CLEANING LOG

          if (
            !is.null(private$..cleaning_log) &&
              inherits(private$..cleaning_log, "CleaningLog")
          ) {
            # validation (internal + schema)
            private$..cleaning_log$validate()

            # post-validate: check cleaning log against dataset
            private$..cleaning_log$post_validate(self, stage = "clean")

            # apply changes (authoritative mode option A)
            if (nrow(private$..cleaning_log$log_df) > 0) {
              private$..clean_data <- private$..apply_cleaning_changes(
                df = private$..clean_data,
                log_df = private$..cleaning_log$log_df,
                uuid_col = private$..uuid
              )
            }
          }

          # VALIDATE & APPLY DELETION LOG

          if (
            !is.null(private$..deletion_log) &&
              inherits(private$..deletion_log, "DeletionLog")
          ) {
            private$..deletion_log$validate()
            private$..deletion_log$post_validate(self, stage = "clean")

            # apply deletions
            if (nrow(private$..deletion_log$log_df) > 0) {
              delete_ids <- as.character(private$..deletion_log$log_df$uuid)
              private$..clean_data <- private$..clean_data[
                !as.character(private$..clean_data[[private$..uuid]]) %in% delete_ids,
              ]
            }
          }

          # FINALIZE

          self$cleaned <- TRUE
          phrutils::phr_message(phrutils::phr_txt(
            "{self$dataset_name} cleaning complete."
          ))

        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$clean")
      )
    },

    #' Set Variable Schema from List
    #'
    #' @description
    #' Sets the variable schema directly from a structured list
    #'
    #' @param schema_list List with named elements for types, allowed_values, etc.
    #'
    #' @return Invisible self for method chaining
    #'
    #' @details
    #' The schema list is validated for structure before assignment.
    #' Use this method when programmatically building schemas.
    set_variable_schema = function(schema_list) {
      phrutils::phr_try(
        {
          # 1. Validate nested schema structure
          data_validate_schema_to_table(
            schema_list = schema_list
          )

          # 2. Convert to table
          tbl <- private$..schema_to_table(
            schema_type = "variable",
            schema_list = schema_list
          )

          # 3. Validate the table
          data_validate_table_to_schema(
            df = tbl
          )

          # 4. Store
          private$..variable_schema <- schema_list

          phrutils::phr_message(
            phrutils::phr_txt("Variable schema attached to {self$dataset_name}.")
          )

          # 5. Auto-update variable and value maps now that schema is available.
          #    map_schema_vars handles NULL/empty raw_data gracefully (returns early).
          private$..map_schema_vars(stage = "raw")
          private$..map_schema_labels()
        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$set_variable_schema")
      )
    },

    #' Get Variable Schema
    #'
    #' @description
    #' Returns the current variable schema list
    #'
    #' @return List containing variable schema (types, allowed_values, etc.), or NULL if not set
    get_variable_schema = function() private$..variable_schema,

    #' @description
    #' Set indicator schema from list
    #'
    #' @param indicator_schema_list List containing indicator definitions
    set_indicator_schema = function(indicator_schema_list) {
      phrutils::phr_try(
        {
          if (!is.list(indicator_schema_list)) {
            phrutils::phr_error(
              self$dataset_name,
              phrutils::phr_txt("Indicator schema must be a list.")
            )
          }

          private$..indicator_schema <- indicator_schema_list

          phrutils::phr_message(
            phrutils::phr_txt(
              "Indicator schema set for {self$dataset_name} ({length(indicator_schema_list)} indicator(s))."
            )
          )
        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$set_indicator_schema")
      )
    },

    #' @description
    #' Get indicator schema
    #'
    #' @return Indicator schema list, or NULL if not set
    get_indicator_schema = function() private$..indicator_schema,

    #' @description
    #' Set dependency schema from list
    #'
    #' @param dependency_schema_list List containing dependency definitions
    set_dependency_schema = function(dependency_schema_list) {
      phrutils::phr_try(
        {
          if (!is.list(dependency_schema_list)) {
            phrutils::phr_error(
              self$dataset_name,
              phrutils::phr_txt("Dependency schema must be a list.")
            )
          }

          private$..dependency_schema <- dependency_schema_list

          phrutils::phr_message(
            phrutils::phr_txt(
              "Dependency schema set for {self$dataset_name} ({length(dependency_schema_list$dependencies %||% list())} dependency/ies, {length(dependency_schema_list$soft_dependencies %||% list())} soft dependency/ies)."
            )
          )
        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$set_dependency_schema")
      )
    },

    #' @description
    #' Get dependency schema
    #'
    #' @return Dependency schema list, or NULL if not set
    get_dependency_schema = function() private$..dependency_schema,

    # Data access

    #' Get Data at Specified Stage
    #'
    #' @description
    #' Retrieves the data frame at the specified processing stage
    #'
    #' @param stage Character string: "raw", "standardized", or "clean"
    #'
    #' @return Data frame at the specified stage, or NULL if stage not yet available
    get_data = function(stage = c("raw", "standardized", "clean")) {
      stage <- match.arg(stage)
      phrutils::phr_try(
        {
          if (stage == "raw") {
            return(private$..raw_data)
          }
          if (stage == "standardized") {
            return(private$..standardized_data)
          }
          if (stage == "clean") return(private$..clean_data)
        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$get_data")
      )
    },

    # Variable map helpers

    #' Set Variable Mapping
    #'
    #' @description
    #' Maps a semantic role to a column name in the dataset
    #'
    #' @param role Character string with semantic role (e.g., "uuid", "cluster_id")
    #' @param column_name Character string with actual column name in the data
    #' @param stage Character string specifying stage to validate against: "raw", "standardized", or "clean"
    #'
    #' @return Invisible NULL (updates variable_map internally)
    #'
    #' @details
    #' The column name is validated to ensure it exists in the specified data stage.
    set_variable = function(
      role,
      column_name,
      stage = c("raw", "standardized", "clean")
    ) {
      stage <- match.arg(stage)

      # --- Basic input validation ---
      if (!is.character(role) || length(role) != 1) {
        phrutils::phr_error(
          self$dataset_name,
          phrutils::phr_txt("Role must be a single character string.")
        )
      }

      if (!is.character(column_name) || length(column_name) != 1) {
        phrutils::phr_error(
          self$dataset_name,
          phrutils::phr_txt("Column name must be a single character string.")
        )
      }

      # --- Use unified accessor for correct stage ---
      df <- self$get_data(stage)

      if (is.null(df)) {
        phrutils::phr_warning(
          self$dataset_name,
          phrutils::phr_txt(
            "No {stage} dataset available when setting variable '{role}'."
          )
        )
      } else if (!column_name %in% names(df)) {
        phrutils::phr_warning(
          self$dataset_name,
          phrutils::phr_txt("Column '{column_name}' not found in {stage} dataset.")
        )
      }

      # --- Set the variable map ---
      self$.variable_map[[role]] <- column_name

      phrutils::phr_message(phrutils::phr_txt(
        "Mapped role '{role}' → '{column_name}' (checked on {stage} data)."
      ))
    },

    #' Get Variable Column Name by Role
    #'
    #' @description
    #' Returns the column name mapped to a semantic role
    #'
    #' @param role Character string with semantic role
    #'
    #' @return Character string with column name, or NULL if role not mapped
    get_variable = function(role) self$.variable_map[[role]],

    #' Resolve Column from Role
    #'
    #' @description
    #' Resolves a semantic role to its column name and optionally retrieves mapped values and data diagnostics
    #'
    #' @param role Character string with semantic role or direct column name
    #' @param stage Character string specifying data stage: "raw", "standardized", or "clean" (default: "raw")
    #' @param values Logical; if TRUE, returns list with column and mapped values
    #' @param full Logical; if TRUE, returns full diagnostic information including values in data
    #'
    #' @return Character string (column name), list (if values=TRUE or full=TRUE), or NULL if not found
    #'
    #' @details
    #' Resolution priority: variable_map role lookup, then direct column name fallback.
    #' With full=TRUE, returns: role, column, values, exists_in_data, values_in_data.
    resolve_column = function(
      role,
      stage = "raw",
      values = FALSE,
      full = FALSE
    ) {
      if (missing(role) || is.null(role) || role == "") {
        return(NULL)
      }

      # Determine column name from variable_map
      if (!is.null(self$.variable_map) && role %in% names(self$.variable_map)) {
        col <- self$.variable_map[[role]]
      } else {
        col <- role # fallback assume direct column name
      }

      df <- self$get_data(stage)

      # Warn if column missing
      exists_in_data <- !is.null(df) && col %in% names(df)
      if (!exists_in_data) {
        phrutils::phr_warning(
          self$dataset_name,
          phrutils::phr_txt("Column '{col}' not found in data (role='{role}').")
        )
      }

      # ---- VALUE MAP AWARENESS
      mapped_vals <- NULL
      if (!is.null(self$.value_map) && role %in% names(self$.value_map)) {
        mapped_vals <- self$.value_map[[role]]
      }

      # Return column name only (current behaviour)
      if (!values && !full) {
        return(if (exists_in_data) col else NULL)
      }

      # Return column + mapped values
      if (values && !full) {
        return(list(
          column = if (exists_in_data) col else NULL,
          values = mapped_vals
        ))
      }

      # Full diagnostic mode
      vals_in_data <- NULL
      if (exists_in_data) {
        vals_in_data <- unique(df[[col]])
      }

      return(list(
        role = role,
        column = if (exists_in_data) col else NULL,
        values = mapped_vals,
        exists_in_data = exists_in_data,
        values_in_data = vals_in_data
      ))
    },

    # Schema diagnostics

    #' Diagnose Data Against Schema
    #'
    #' @description
    #' Compares data columns against variable schema to identify type mismatches and invalid values
    #'
    #' @param stage Character string: "raw", "standardized", or "clean"
    #'
    #' @return Data frame with diagnostic results (variable, expected_type, actual_type, type_ok, values_ok, issues)
    #'
    #' @details
    #' For each variable in the schema:
    #' * Checks if type matches expected type
    #' * Validates values against allowed_values (if defined)
    #' * Reports all issues found
    #' Returns NULL if no schema is defined or no data available.
    data_diagnose = function(stage = c("raw", "standardized", "clean")) {
      stage <- match.arg(stage)

      phrutils::phr_try(
        {
          # 1. Get data for selected stage
          df <- self$get_data(stage)
          if (is.null(df)) {
            phrutils::phr_warning(
              self$dataset_name,
              phrutils::phr_txt("No data available at selected stage '{stage}'.")
            )
            return(NULL)
          }

          # 2. Check if schema exists
          sch <- private$..variable_schema
          if (is.null(sch) || (is.list(sch) && length(sch) == 0)) {
            phrutils::phr_warning(
              self$dataset_name,
              phrutils::phr_txt("No variable schema defined.")
            )
            return(NULL)
          }

          # 3. Build diagnostic table
          # Each row represents a canonical variable/value pair from the schema

          diagnostic_rows <- list()

          # Get schema components
          types <- sch$types %||% list()
          value_map_schema <- sch$value_map %||% list()
          col_names_schema <- sch$col_names %||% list()
          comments <- sch$comments %||% list()

          # Get current mappings
          vm <- self$.variable_map %||% list()
          vmap <- self$.value_map %||% list()

          # Iterate over all variables in the schema
          for (var_role in names(types)) {
            required_type <- types[[var_role]]
            comment <- comments[[var_role]] %||% NA_character_

            # Get mapped variable name from variable_map
            mapped_variable <- vm[[var_role]] %||% NA_character_

            # Check if variable is mapped and exists in data
            var_exists <- !is.na(mapped_variable) &&
              mapped_variable %in% names(df)

            # Determine safely_coercible
            safely_coercible <- NA
            if (var_exists) {
              safely_coercible <- phrutils::is_safely_coercible(
                df[[mapped_variable]],
                required_type
              )
            }

            # Check if this variable has value mappings in schema
            has_value_map <- var_role %in% names(value_map_schema)

            if (has_value_map) {
              # This variable has canonical values defined in schema
              canonical_values <- value_map_schema[[var_role]]

              # canonical_values is a list: canonical_value_name -> c(dataset_values)
              if (is.list(canonical_values) && length(canonical_values) > 0) {
                for (canonical_val in names(canonical_values)) {
                  # Get the mapped values from value_map for this canonical value
                  mapped_values <- NA_character_
                  if (var_role %in% names(vmap) && is.list(vmap[[var_role]])) {
                    if (canonical_val %in% names(vmap[[var_role]])) {
                      dataset_vals <- vmap[[var_role]][[canonical_val]]
                      if (length(dataset_vals) > 0) {
                        # Store as string, but we'll use the actual vector for validation
                        mapped_values <- paste(dataset_vals, collapse = ", ")
                        mapped_vals_vector <- dataset_vals # Keep the actual vector for validation
                      }
                    }
                  }

                  # Determine issues for this variable/value pair
                  issues <- character(0)

                  # Check if variable is not mapped
                  if (is.na(mapped_variable)) {
                    issues <- c(issues, "variable not mapped")
                  } else if (!var_exists) {
                    issues <- c(issues, "mapped variable not in dataset")
                  }

                  # Check if value is not mapped
                  if (is.na(mapped_values)) {
                    issues <- c(issues, "value not mapped")
                    mapped_vals_vector <- character(0) # Empty vector for unmapped
                  } else {
                    # Check if mapped values exist in data
                    if (var_exists && exists("mapped_vals_vector")) {
                      data_vals <- unique(df[[mapped_variable]])

                      # Check if this is a select_multiple variable
                      # For select_multiple, values are space-separated, so we need to extract tokens
                      is_select_multiple <- private$..is_select_multiple(var_role)
                      if (is_select_multiple) {
                        # Extract individual tokens from space-separated values
                        data_vals <- private$..extract_select_multiple_tokens(
                          data_vals
                        )
                      }

                      missing_vals <- setdiff(mapped_vals_vector, data_vals)
                      if (length(missing_vals) > 0) {
                        issues <- c(
                          issues,
                          paste0(
                            "mapped values not in dataset: ",
                            paste(missing_vals, collapse = ", ")
                          )
                        )
                      }
                    }
                  }

                  # Check type coercion issues
                  if (var_exists && isFALSE(safely_coercible)) {
                    issues <- c(issues, "not safely coercible to required type")
                  }

                  # Create issues string
                  issues_str <- if (length(issues) == 0) {
                    "ok"
                  } else {
                    paste(issues, collapse = "; ")
                  }

                  # Add row
                  diagnostic_rows[[length(diagnostic_rows) + 1]] <- list(
                    required_variable = var_role,
                    required_value = canonical_val,
                    required_type = required_type,
                    mapped_variable = mapped_variable,
                    mapped_value = mapped_values,
                    safely_coercible = safely_coercible,
                    comment = comment,
                    issues = issues_str
                  )
                }
              } else {
                # Value map exists but is empty or not in list format - add single row for variable
                diagnostic_rows[[length(diagnostic_rows) + 1]] <- list(
                  required_variable = var_role,
                  required_value = NA_character_,
                  required_type = required_type,
                  mapped_variable = mapped_variable,
                  mapped_value = NA_character_,
                  safely_coercible = safely_coercible,
                  comment = comment,
                  issues = if (is.na(mapped_variable)) {
                    "variable not mapped"
                  } else if (!var_exists) {
                    "mapped variable not in dataset"
                  } else if (isFALSE(safely_coercible)) {
                    "not safely coercible to required type"
                  } else {
                    "ok"
                  }
                )
              }
            } else {
              # No value map - add single row for just the variable
              issues <- character(0)
              if (is.na(mapped_variable)) {
                issues <- c(issues, "variable not mapped")
              } else if (!var_exists) {
                issues <- c(issues, "mapped variable not in dataset")
              }
              if (var_exists && isFALSE(safely_coercible)) {
                issues <- c(issues, "not safely coercible to required type")
              }

              issues_str <- if (length(issues) == 0) {
                "ok"
              } else {
                paste(issues, collapse = "; ")
              }

              diagnostic_rows[[length(diagnostic_rows) + 1]] <- list(
                required_variable = var_role,
                required_value = NA_character_,
                required_type = required_type,
                mapped_variable = mapped_variable,
                mapped_value = NA_character_,
                safely_coercible = safely_coercible,
                comment = comment,
                issues = issues_str
              )
            }
          }

          # Convert to data frame
          if (length(diagnostic_rows) == 0) {
            # Empty schema or no variables
            result <- data.frame(
              required_variable = character(0),
              required_value = character(0),
              required_type = character(0),
              mapped_variable = character(0),
              mapped_value = character(0),
              safely_coercible = logical(0),
              comment = character(0),
              issues = character(0),
              stringsAsFactors = FALSE
            )
          } else {
            result <- do.call(
              rbind,
              lapply(diagnostic_rows, function(row) {
                data.frame(
                  required_variable = row$required_variable,
                  required_value = row$required_value,
                  required_type = row$required_type,
                  mapped_variable = row$mapped_variable,
                  mapped_value = row$mapped_value,
                  safely_coercible = row$safely_coercible,
                  comment = row$comment,
                  issues = row$issues,
                  stringsAsFactors = FALSE
                )
              })
            )
          }

          # Store in data_diagnostics field
          private$..data_diagnostics <- result

          phrutils::phr_message(
            phrutils::phr_txt(
              "Generated diagnostic table with {nrow(result)} row(s) for {self$dataset_name}."
            )
          )

          # Return the table
          return(result)
        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$data_diagnose")
      )
    },

    #' Run Data Quality Checks
    #'
    #' @description
    #' Executes data quality checks defined in dependency_schema and generates quality flags
    #'
    #' @param stage Character string: "standardized" (default) or "clean"
    #'
    #' @return A list containing check results and flags, or NULL if no dependency schema
    #'
    #' @details
    #' Quality checks include:
    #' * Dependency validations (e.g., if A is answered, B must be answered)
    #' * Range checks and logical consistency
    #' * Custom validation rules from dependency_schema
    #' Results are stored in private$..data_quality_flags for later use.
    run_quality_checks = function(stage = "standardized") {
      phrutils::phr_try(
        {
          # SAFE LOAD OF DATA & SCHEMA

          df <- phrutils::phr_try(
            self$get_data(stage),
            on_error = "abort",
            origin = paste0(self$dataset_name, "_DQ_load")
          )

          if (is.null(df)) {
            phrutils::phr_error(
              message = "No dataset is available at the selected stage.",
              origin = paste0(self$dataset_name, "$run_quality_checks")
            )
          }

          # Load dependency_schema (primary) and variable_schema (for types only)
          dep_schema <- phrutils::phr_try(
            private$..dependency_schema,
            on_error = "abort",
            origin = paste0(self$dataset_name, "_DQ_dep_schema_load")
          )

          var_schema <- phrutils::phr_try(
            private$..variable_schema,
            on_error = "abort",
            origin = paste0(self$dataset_name, "_DQ_var_schema_load")
          )

          # Check if we have any schema to work with
          has_dep_schema <- !is.null(dep_schema) &&
            (length(dep_schema$dependencies) > 0 ||
              length(dep_schema$soft_dependencies) > 0)

          has_types <- !is.null(var_schema) &&
            !is.null(var_schema$types) &&
            length(var_schema$types) > 0

          if (!has_dep_schema && !has_types) {
            phrutils::phr_warning(
              self$dataset_name,
              "No dependency schema or type information available; skipping data quality checks."
            )
            private$..data_quality_flags <- NULL
            return(invisible(NULL))
          }

          # FLAG COLLECTION

          flags <- list()

          add_flag <- function(name, vec) {
            phrutils::phr_try(
              {
                if (!is.null(vec) && length(vec) == nrow(df)) {
                  flags[[name]] <<- vec
                }
              },
              on_error = "warn",
              origin = paste0(self$dataset_name, "_DQ_addflag_", name)
            )
          }

          # 1. Type coercion checks (row-level flags)
          # Use var_schema$types to determine intended types
          # This check identifies specific rows that cannot be coerced

          phrutils::phr_try(
            {
              if (has_types) {
                for (col in names(var_schema$types)) {
                  if (!col %in% names(df)) {
                    next
                  }
                  want <- var_schema$types[[col]]
                  x <- df[[col]]

                  # Column-level coercibility
                  is_ok <- phrutils::is_safely_coercible(x, want)

                  if (is_ok) {
                    add_flag(paste0("flag_", col, "_type"), rep(0, nrow(df)))
                  } else {
                    bad_rows <- phrutils::which_bad_coercible(x, want)
                    add_flag(
                      paste0("flag_", col, "_type"),
                      ifelse(bad_rows, 1, 0)
                    )
                  }
                }
              }
            },
            on_error = "warn",
            origin = paste0(self$dataset_name, "_DQ_typecheck")
          )

          # 2. Dependencies from dependency_schema
          # All other quality checks (allowed values, ranges, patterns, unique, mutex, etc.)
          # should now be defined as dependency rules in the dependency_schema

          phrutils::phr_try(
            {
              if (has_dep_schema) {
                # Process all dependencies (action field determines treatment)
                if (length(dep_schema$dependencies) > 0) {
                  for (flag_name in names(dep_schema$dependencies)) {
                    # Wrap each dependency processing in phr_try to continue on errors
                    phrutils::phr_try(
                      {
                        rule <- dep_schema$dependencies[[flag_name]]

                        # CHANGE 1: Check if required variables are present in dataset
                        # Variables in dependency schema use canonical names from variable_map
                        required_vars <- rule[["variables"]]

                        skip_dependency <- FALSE

                        if (
                          !is.null(required_vars) && length(required_vars) > 0
                        ) {
                          # Resolve canonical variable names to dataset column names
                          mapped_cols <- character(0)
                          missing_vars <- character(0)

                          for (var_canonical in required_vars) {
                            # Try to resolve using variable_map
                            # Suppress warnings from resolve_column since we handle missing variables gracefully
                            var_actual <- suppressWarnings(self$resolve_column(
                              var_canonical,
                              stage = stage
                            ))

                            if (is.null(var_actual)) {
                              # Variable not found in dataset
                              missing_vars <- c(missing_vars, var_canonical)
                            } else {
                              mapped_cols <- c(mapped_cols, var_actual)
                            }
                          }

                          # Skip this dependency if any required variables are missing
                          if (length(missing_vars) > 0) {
                            phrutils::phr_message(
                              phrutils::phr_txt(
                                "Skipping dependency '{flag_name}': required variable(s) not present in dataset: {paste(missing_vars, collapse=', ')}"
                              )
                            )
                            skip_dependency <- TRUE
                          }
                        }

                        if (!skip_dependency) {
                          # Read the action field before checking required fields, since
                          # flag_delete dependencies may omit the 'then' clause
                          rule_action <- rule[["action"]] %||% ""

                          rule_if <- rule[["if"]] %||%
                            rule[["condition_if"]] %||%
                            rule[["condition"]]

                          rule_then <- rule[["then"]] %||%
                            rule[["require"]]

                          if (is.null(rule_if)) {
                            phrutils::phr_warning(
                              self$dataset_name,
                              phrutils::phr_txt(
                                "Invalid dependency rule structure for '{flag_name}': missing 'if/condition_if'."
                              )
                            )
                            skip_dependency <- TRUE
                          } else if (
                            is.null(rule_then) && rule_action != "flag_delete"
                          ) {
                            # 'then' is required for all actions except flag_delete,
                            # which may use condition_if alone to identify rows for deletion
                            phrutils::phr_warning(
                              self$dataset_name,
                              phrutils::phr_txt(
                                "Invalid dependency rule structure for '{flag_name}': missing 'then'."
                              )
                            )
                            skip_dependency <- TRUE
                          }
                        }

                        # Only process dependency if it should not be skipped
                        if (!skip_dependency) {
                          # DEPENDENCY EXPRESSION TRANSLATION

                          # Translate canonical variable names and values in dependency expressions
                          # to dataset-specific column names and values.
                          #
                          # Example:
                          # Input:  "fever == 'yes' & !is.na(temperature)"
                          # Output: "fever_col %in% c('yes', 'y', 'oui') & !is.na(temp_col)"
                          #
                          # Translation uses:
                          # - variable_map: Maps canonical names (fever) to columns (fever_col)
                          # - value_map: Maps canonical values ('yes') to dataset values (c('yes', 'y'))
                          #
                          # This allows dependency rules to be written using portable canonical names
                          # rather than dataset-specific column names.
                          #
                          # For comprehensive documentation, see:
                          # docs/variable_value_mapping_guide.md

                          rule_if_translated <- private$..translate_expression(
                            rule_if,
                            stage = stage
                          )

                          cond_if <- phrutils::phr_try(
                            {
                              out <- with(
                                df,
                                eval(parse(text = rule_if_translated))
                              )
                              if (!is.logical(out)) {
                                rep(FALSE, nrow(df))
                              } else if (length(out) == 1) {
                                # Scalar logical value (e.g., TRUE or FALSE) - replicate to all rows
                                rep(out, nrow(df))
                              } else if (length(out) != nrow(df)) {
                                # Length mismatch - default to FALSE
                                rep(FALSE, nrow(df))
                              } else {
                                out
                              }
                            },
                            on_error = "warn",
                            origin = paste0(
                              self$dataset_name,
                              "_DQ_if_",
                              flag_name
                            ),
                            hint = tryCatch(
                              {
                                # Try to parse to get specific error
                                parse(text = rule_if_translated)
                                NULL # If parse succeeds, no special hint needed
                              },
                              error = function(e) {
                                private$..get_expression_parse_hint(
                                  rule_if_translated,
                                  conditionMessage(e)
                                )
                              }
                            )
                          )

                          # Evaluate 'then' only when present
                          cond_then <- NULL
                          if (!is.null(rule_then)) {
                            rule_then_translated <- private$..translate_expression(
                              rule_then,
                              stage = stage
                            )

                            cond_then <- phrutils::phr_try(
                              {
                                out <- with(
                                  df,
                                  eval(parse(text = rule_then_translated))
                                )
                                if (!is.logical(out)) {
                                  rep(FALSE, nrow(df))
                                } else if (length(out) == 1) {
                                  # Scalar logical value (e.g., TRUE or FALSE) - replicate to all rows
                                  rep(out, nrow(df))
                                } else if (length(out) != nrow(df)) {
                                  # Length mismatch - default to FALSE
                                  rep(FALSE, nrow(df))
                                } else {
                                  out
                                }
                              },
                              on_error = "warn",
                              origin = paste0(
                                self$dataset_name,
                                "_DQ_then_",
                                flag_name
                              ),
                              hint = tryCatch(
                                {
                                  # Try to parse to get specific error
                                  parse(text = rule_then_translated)
                                  NULL # If parse succeeds, no special hint needed
                                },
                                error = function(e) {
                                  private$..get_expression_parse_hint(
                                    rule_then_translated,
                                    conditionMessage(e)
                                  )
                                }
                              )
                            )
                          }

                          # Calculate flag for this rule.
                          # When 'then' is present: flag rows where cond_if is TRUE but cond_then is FALSE.
                          # When 'then' is absent (flag_delete with condition_if only): flag rows where
                          # cond_if is TRUE — those rows are the ones to be deleted.
                          # Note: NA positions in flag_vec are converted to 1 (flagged) since we cannot
                          # confirm the condition is met when the evaluation result is NA.
                          flag_vec <- rep(0, nrow(df))
                          if (!is.null(cond_then)) {
                            flag_vec[cond_if & !cond_then] <- 1
                          } else {
                            flag_vec[cond_if] <- 1
                          }
                          flag_vec[is.na(flag_vec)] <- 1

                          # Add flag with consistent "flag_" prefix
                          add_flag(flag_name, flag_vec)
                        }
                      },
                      on_error = "warn",
                      origin = paste0(self$dataset_name, "_DQ_dep_", flag_name)
                    )
                  }
                }

                # Note: soft_dependencies removed - use action field to determine treatment
              }
            },
            on_error = "warn",
            origin = paste0(self$dataset_name, "_DQ_dependencies")
          )

          # Final assembly of flags

          if (length(flags) == 0) {
            phrutils::phr_message(
              self$dataset_name,
              "No data quality issues detected."
            )
            private$..data_quality_flags <- NULL
            return(invisible(NULL))
          }

          flag_df <- as.data.frame(flags, stringsAsFactors = FALSE)

          if (private$..uuid %in% names(df)) {
            flag_df[[private$..uuid]] <- df[[private$..uuid]]
          }

          private$..data_quality_flags <- flag_df

          # Append to standardized data

          phrutils::phr_try(
            {
              std <- private$..standardized_data
              if (!is.null(std) && private$..uuid %in% names(std)) {
                join_col <- private$..uuid
                if (join_col %in% names(flag_df)) {
                  # Remove columns from std that already exist in flag_df (excluding
                  # the join key) to prevent dplyr from creating .x/.y duplicates.
                  overlapping_cols <- setdiff(
                    intersect(names(std), names(flag_df)),
                    join_col
                  )
                  if (length(overlapping_cols) > 0) {
                    std <- std[,
                      !names(std) %in% overlapping_cols,
                      drop = FALSE
                    ]
                  }

                  std <- dplyr::left_join(std, flag_df, by = join_col)
                  private$..standardized_data <- std

                  phrutils::phr_message(
                    phrutils::phr_txt(
                      "Appended {ncol(flag_df)-1} data quality flag columns onto standardized dataset."
                    )
                  )
                }
              }
            },
            on_error = "warn",
            origin = paste0(self$dataset_name, "_DQ_append")
          )

          phrutils::phr_message(
            phrutils::phr_txt(
              "Data quality check complete. {ncol(flag_df)-1} flag types generated."
            )
          )

          invisible(flag_df)
        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$run_quality_checks")
      )
    },

    # Cleaning & Deletion Logs



    #' @description
    #' Generate cleaning and deletion log entries from data quality flags.
    #'
    #' Populates the existing CleaningLog with entries for each quality flag violation.
    #' Populates the DeletionLog for flags with action "flag_delete" and for duplicate
    #' values in variables marked as unique in the variable schema.
    #' Must be called after run_quality_checks() has been executed.
    #'
    #' @param stage Data stage to use ("standardized" or "clean")
    #' @param overwrite If TRUE, clears existing log before adding new entries
    #' @return Invisible private$..cleaning_log
    generate_cleaning_log = function(
      stage = "standardized",
      overwrite = FALSE
    ) {
      phrutils::phr_try(
        {
          df <- self$get_data(stage)
          if (is.null(df)) {
            phrutils::phr_error(
              self$dataset_name,
              phrutils::phr_txt("No data available at stage '{stage}'.")
            )
          }

          # Warns on no quality flags
          if (is.null(private$..data_quality_flags)) {
            phrutils::phr_warning(
              self$dataset_name,
              "No data quality flags available. Run run_quality_checks() first."
            )
            # return(invisible(private$..cleaning_log))
          }

          # Clear log if requested
          if (overwrite) {
            private$..cleaning_log$clear()
          }

          # Extract flag_* columns (excluding UUID)
          # Note: All quality check flags now use "flag_" prefix
          flag_df <- private$..data_quality_flags
          flag_cols <- grep("^flag_", names(flag_df), value = TRUE)
          flag_cols <- setdiff(flag_cols, private$..uuid)

          # Get enum_id and device_id from variable_map if available
          enum_id_col <- self$.variable_map$enum_id %||% NA_character_
          device_id_col <- self$.variable_map$device_id %||% NA_character_

          entries_added <- 0
          deletions_added <- 0

          # For each flagged column, create cleaning log entries
          for (col in flag_cols) {
            # Find rows where flag = 1 (issue present)
            bad_rows <- which(flag_df[[col]] == 1)

            if (length(bad_rows) > 0) {
              # Check if this is a dependency check flag
              dep_variables <- private$..get_flag_variables_from_schema(col)

              # Determine changed value based on action from schema
              changed_val <- "no" # default
              action <- NULL

              # Type coercion checks are always treated as flag_autoclean
              # (they come from variable_schema, not dependency_schema)
              if (grepl("_type$", col)) {
                changed_val <- "yes"
              } else {
                # For other flags, check action from dependency_schema
                action <- private$..get_flag_action_from_schema(col)
                if (
                  !is.null(action) &&
                    !is.na(action) &&
                    action == "flag_autoclean"
                ) {
                  changed_val <- "yes"
                }
              }

              # Handle flag_delete: add records to deletion log instead of cleaning log
              if (
                !is.null(action) && !is.na(action) && action == "flag_delete"
              ) {
                for (idx in bad_rows) {
                  uuid_val <- df[[private$..uuid]][idx]

                  enum_id_val <- NA_character_
                  device_id_val <- NA_character_

                  if (!is.na(enum_id_col) && enum_id_col %in% names(df)) {
                    enum_id_val <- as.character(df[[enum_id_col]][idx])
                  }

                  if (!is.na(device_id_col) && device_id_col %in% names(df)) {
                    device_id_val <- as.character(df[[device_id_col]][idx])
                  }

                  private$..deletion_log$add_deletion(
                    uuid = uuid_val,
                    enum_id = enum_id_val,
                    device_id = device_id_val,
                    issue = col,
                    feedback = paste0(
                      "Auto-flagged for deletion by quality check: ",
                      col
                    )
                  )

                  deletions_added <- deletions_added + 1
                }
                next
              }

              # Determine which variables to create log entries for
              if (!is.null(dep_variables) && length(dep_variables) > 0) {
                # This is a dependency check - create entries for all variables mentioned
                variables_to_log <- dep_variables
              } else {
                # Not a dependency check - infer variable name from flag name
                # Remove "flag_" prefix and common suffixes
                var_name <- sub("^flag_", "", col)
                var_name <- sub("_(type|unique)$", "", var_name)
                variables_to_log <- var_name
              }

              # Pre-resolve canonical variable names to actual column names and filter
              # out variables that should not produce cleaning log entries:
              #   - Self-referential: variable is the quality flag column itself
              #     (derived indicator, not a raw survey field to be corrected).
              #   - Missing: variable column is absent from the dataset at this stage.
              # Both checks are done once here, before the per-row loop, to avoid
              # emitting repeated log messages for every flagged row.
              filtered_variables <- character(0) # actual column names to log
              skipped_self_ref <- character(0)
              skipped_missing_cols <- character(0)

              for (var_canonical in variables_to_log) {
                var_actual_col <- self$resolve_column(
                  var_canonical,
                  stage = stage
                )
                if (is.null(var_actual_col)) {
                  var_actual_col <- var_canonical
                }

                if (var_actual_col == col) {
                  skipped_self_ref <- c(skipped_self_ref, var_actual_col)
                  next
                }
                if (!var_actual_col %in% names(df)) {
                  skipped_missing_cols <- c(
                    skipped_missing_cols,
                    var_actual_col
                  )
                  next
                }
                filtered_variables <- c(filtered_variables, var_actual_col)
              }

              if (length(skipped_self_ref) > 0) {
                phrutils::phr_message(
                  phrutils::phr_txt(
                    "Skipping self-referential cleaning log variable(s) for flag '{col}': {paste(skipped_self_ref, collapse=', ')}."
                  )
                )
              }
              if (length(skipped_missing_cols) > 0) {
                phrutils::phr_message(
                  phrutils::phr_txt(
                    "Skipping cleaning log variable(s) for flag '{col}' not found in dataset at stage '{stage}': {paste(skipped_missing_cols, collapse=', ')}."
                  )
                )
              }

              # Add entries to cleaning log for each affected variable
              for (idx in bad_rows) {
                uuid_val <- df[[private$..uuid]][idx]

                # Get enum_id and device_id values for this row
                enum_id_val <- NA_character_
                device_id_val <- NA_character_

                if (!is.na(enum_id_col) && enum_id_col %in% names(df)) {
                  enum_id_val <- as.character(df[[enum_id_col]][idx])
                }

                if (!is.na(device_id_col) && device_id_col %in% names(df)) {
                  device_id_val <- as.character(df[[device_id_col]][idx])
                }

                # Create one log entry for each filtered variable
                for (var_actual_col in filtered_variables) {
                  # Get old value from dataset using the actual column name
                  old_val <- as.character(df[[var_actual_col]][idx])

                  private$..cleaning_log$add_change(
                    uuid = uuid_val,
                    enum_id = enum_id_val,
                    device_id = device_id_val,
                    question.name = var_actual_col, # Use actual column name from dataset
                    issue = col,
                    feedback = paste0("Auto-flagged by quality check: ", col),
                    changed = changed_val,
                    old.value = old_val,
                    new.value = NA_character_
                  )

                  entries_added <- entries_added + 1
                }
              }
            }
          }

          phrutils::phr_message(
            phrutils::phr_txt(
              "Generated {entries_added} cleaning log entries and {deletions_added} deletion log entries from quality flags."
            )
          )

          # PROCESS "OTHER" COLUMNS

          # Read from private$..other_columns which is now a list where each entry has:
          # - other_column: main open text response column name
          # - other_linked_columns: vector of related column names
          # Generate one row for each relevant column

          other_entries_added <- 0

          if (length(private$..other_columns) > 0) {
            for (entry_name in names(private$..other_columns)) {
              entry <- private$..other_columns[[entry_name]]

              # Extract other_column and linked columns
              other_col <- entry$other_column
              linked_cols <- entry$other_linked_columns

              # Skip if main other_column doesn't exist in data
              if (!other_col %in% names(df)) {
                next
              }

              # Find rows where the other_column has a value
              has_value_idx <- which(
                !is.na(df[[other_col]]) & df[[other_col]] != ""
              )

              if (length(has_value_idx) > 0) {
                for (idx in has_value_idx) {
                  uuid_val <- df[[private$..uuid]][idx]
                  other_val <- as.character(df[[other_col]][idx])

                  # Get enum_id and device_id values for this row
                  enum_id_val <- NA_character_
                  device_id_val <- NA_character_

                  if (!is.na(enum_id_col) && enum_id_col %in% names(df)) {
                    enum_id_val <- as.character(df[[enum_id_col]][idx])
                  }

                  if (!is.na(device_id_col) && device_id_col %in% names(df)) {
                    device_id_val <- as.character(df[[device_id_col]][idx])
                  }

                  # Entry 1: The main "other" column itself
                  private$..cleaning_log$add_change(
                    uuid = uuid_val,
                    enum_id = enum_id_val,
                    device_id = device_id_val,
                    question.name = other_col,
                    issue = "other_response",
                    feedback = paste0("'Other' response provided: ", other_val),
                    changed = "no",
                    old.value = other_val,
                    new.value = NA_character_
                  )

                  other_entries_added <- other_entries_added + 1

                  # Entry 2+: Each linked column
                  if (!is.null(linked_cols) && length(linked_cols) > 0) {
                    for (linked_col in linked_cols) {
                      if (linked_col %in% names(df)) {
                        linked_val <- as.character(df[[linked_col]][idx])

                        private$..cleaning_log$add_change(
                          uuid = uuid_val,
                          enum_id = enum_id_val,
                          device_id = device_id_val,
                          question.name = linked_col,
                          issue = "has_other_response",
                          feedback = paste0(
                            "Linked to 'other' response: ",
                            other_val
                          ),
                          changed = "no",
                          old.value = linked_val,
                          new.value = NA_character_
                        )

                        other_entries_added <- other_entries_added + 1
                      }
                    }
                  }
                }
              }
            }

            if (other_entries_added > 0) {
              phrutils::phr_message(
                phrutils::phr_txt(
                  "Generated {other_entries_added} cleaning log entries from 'other' columns."
                )
              )
            }
          }

          # PROCESS UNIQUE VARIABLE CONSTRAINTS
          #
          # For each variable in variable_schema$unique, check for duplicate values
          # in the dataset. Any row whose value for that variable is a duplicate of an
          # earlier row (past the 1st occurrence) is added to the deletion log.

          unique_deletions_added <- 0

          if (
            !is.null(private$..variable_schema) &&
              !is.null(private$..variable_schema$unique) &&
              length(private$..variable_schema$unique) > 0
          ) {
            for (var_canonical in private$..variable_schema$unique) {
              # Resolve canonical variable name to the actual dataset column name
              # using the variable_map (same approach as the rest of generate_cleaning_log)
              var_col <- suppressWarnings(self$resolve_column(
                var_canonical,
                stage = stage
              ))

              # Skip if column not found in dataset
              if (is.null(var_col) || !var_col %in% names(df)) {
                next
              }

              vals <- df[[var_col]]

              # Flag rows that are duplicates (not the first occurrence) and not NA
              dup_idx <- which(duplicated(vals) & !is.na(vals))

              if (length(dup_idx) > 0) {
                for (idx in dup_idx) {
                  uuid_val <- df[[private$..uuid]][idx]

                  dup_enum_id_val <- NA_character_
                  dup_device_id_val <- NA_character_

                  if (!is.na(enum_id_col) && enum_id_col %in% names(df)) {
                    dup_enum_id_val <- as.character(df[[enum_id_col]][idx])
                  }

                  if (!is.na(device_id_col) && device_id_col %in% names(df)) {
                    dup_device_id_val <- as.character(df[[device_id_col]][idx])
                  }

                  private$..deletion_log$add_deletion(
                    uuid = uuid_val,
                    enum_id = dup_enum_id_val,
                    device_id = dup_device_id_val,
                    issue = paste0("duplicate_", var_col),
                    feedback = paste0(
                      "Duplicate value in unique variable '",
                      var_col,
                      "': ",
                      as.character(vals[idx])
                    )
                  )

                  unique_deletions_added <- unique_deletions_added + 1
                }
              }
            }

            if (unique_deletions_added > 0) {
              phrutils::phr_message(
                phrutils::phr_txt(
                  "Generated {unique_deletions_added} deletion log entries from unique variable constraint checks."
                )
              )
            }
          }

          invisible(private$..cleaning_log)
        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$generate_cleaning_log")
      )
    },






    #' Export Data to File
    #'
    #' @description
    #' Exports data from a specified stage to CSV, RDS, or XLSX format
    #'
    #' @param stage Character string: "clean", "standardized", or "raw"
    #' @param format Character string: "csv", "rds", or "xlsx"
    #' @param file_path Character string with output path (auto-generated if NULL)
    #'
    #' @return File path (invisibly)
    #'
    #' @details
    #' If file_path is NULL, generates filename as: \{dataset_name\}_\{stage\}.\{format\}
    export_data = function(
      stage = c("clean", "standardized", "raw"),
      format = c("csv", "rds", "xlsx"),
      file_path = NULL
    ) {
      stage <- match.arg(stage)
      format <- match.arg(format)
      phrutils::phr_try(
        {
          df <- self$get_data(stage)
          if (is.null(df)) {
            phrutils::phr_error(
              self$dataset_name,
              phrutils::phr_txt("No data available at stage '{stage}' to export.")
            )
          }
          if (is.null(file_path)) {
            file_path <- paste0(self$dataset_name, "_", stage, ".", format)
          }
          if (format == "csv") {
            utils::write.csv(df, file_path, row.names = FALSE)
          } else if (format == "rds") {
            saveRDS(df, file_path)
          } else if (format == "xlsx") {
            if (!requireNamespace("openxlsx", quietly = TRUE)) {
              phrutils::phr_error(
                self$dataset_name,
                phrutils::phr_txt("Package 'openxlsx' is required for XLSX export.")
              )
            }
            openxlsx::write.xlsx(df, file = file_path)
          }

          # ---- DUMMY SESSION EXPORT HOOK
          # if (exists("session") && !is.null(session$userData)) {
          #   session$userData$last_export_path <- file_path
          # }

          phrutils::phr_message(phrutils::phr_txt(
            "Exported {stage} data to '{file_path}'."
          ))
          invisible(file_path)
        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$export_data")
      )
    },

    #' Summarize Data Object
    #'
    #' @description
    #' Generates a summary of the dataset and its current state
    #'
    #' @return List with summary information including dataset name, record/column counts,
    #'   validation status, schemas, and labels
    summary = function() {
      phrutils::phr_try(
        {
          if (is.null(private$..raw_data)) {
            phrutils::phr_warning(
              self$dataset_name,
              phrutils::phr_txt("No data loaded for summary.")
            )
            return(NULL)
          }
          list(
            dataset_name = self$dataset_name,
            n_records = nrow(private$..raw_data),
            n_columns = ncol(private$..raw_data),
            uuid = private$..uuid,
            validated = self$validated,
            standardized = self$standardized,
            cleaned = self$cleaned,
            required_columns = private$..required_columns,
            variable_map = self$.variable_map,
            labels_defined = list(
              vars = names(self$.variable_label),
              value_labelled_vars = names(self$.value_label)
            ),
            variable_schema_attached = !is.null(private$..variable_schema),
            indicator_schema_attached = !is.null(private$..indicator_schema),
            dependency_schema_attached = !is.null(private$..dependency_schema)
          )
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "$summary")
      )
    },

    # Linking & cross-object validation

    #' Add Linked Dataset
    #'
    #' @description
    #' Establishes a named link to another Data object for cross-validation
    #'
    #' @param name Character string with unique link name
    #' @param other_object Another Data R6 object to link to
    #' @param by_self_role Character string with role name in this object (e.g., "household_id")
    #' @param by_other_role Character string with role name in other object
    #'
    #' @return Logical TRUE (invisibly)
    #'
    #' @details
    #' Creates a foreign key relationship for use with validate_links().
    add_linked_dataset = function(
      name,
      other_object,
      by_self_role,
      by_other_role
    ) {
      if (missing(name) || !is.character(name) || length(name) != 1) {
        phrutils::phr_error(
          self$dataset_name,
          phrutils::phr_txt("Link 'name' must be a single character string.")
        )
      }
      self$linked_objects[[name]] <- list(
        object = other_object,
        by_self_role = by_self_role,
        by_other_role = by_other_role
      )
      phrutils::phr_message(phrutils::phr_txt(
        "Linked '{name}' to {self$dataset_name} (by {by_self_role} -> {by_other_role})."
      ))
      invisible(TRUE)
    },


    # Data Analytics Generation Hook

    #' @description
    #' Generate a DataAnalytics object for this dataset.
    #'
    #' Creates a unified `DataAnalytics` object that integrates both data quality
    #' checks and quantitative analysis. This is a generic hook that should be
    #' overridden by subclasses to return sector-specific DataAnalytics subclasses.
    #'
    #' @param stage The data stage to use ("standardized" or "clean")
    #' @param analysis_config Optional analysis configuration (data analysis plan tibble)
    #' @return A DataAnalytics object or NULL
    generate_data_analytics = function(
      stage = c("standardized", "clean"),
      analysis_config = NULL
    ) {
      stage <- match.arg(stage)

      phrutils::phr_try(
        {
          df <- self$get_data(stage)

          if (is.null(df)) {
            phrutils::phr_warning(
              self$dataset_name,
              phrutils::phr_txt("No {stage} data available for DataAnalytics generation.")
            )
            return(NULL)
          }

          data_hash <- self$get(field = "..metadata", role = "hash_id")
          variable_map <- self$.variable_map
          value_map <- self$.value_map

          analytics <- DataAnalytics$new(
            data = df,
            dap = analysis_config,
            parent_data_object = self,
            dataset_name = paste0(self$dataset_name, "_DataAnalytics"),
            data_stage_name = stage,
            data_hash = data_hash,
            variable_map = variable_map,
            value_map = value_map,
            variable_label = self$.variable_label,
            value_label = self$.value_label
          )

          phrutils::phr_message(
            phrutils::phr_txt(
              "Generated general DataAnalytics object for {self$dataset_name}."
            )
          )

          return(analytics)
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "$generate_data_analytics")
      )
    },

    # Map Schema Variables & Repair Maps


    #'
    #' This is a helper function to safely update the variable_map and value_map
    #' by accepting dataframes as input. This provides a programmatic way to
    #' update mappings that can be called from UI components.
    #'
    #' @param variable_map_df A dataframe with columns 'role' and 'column_name'
    #'        to update the variable_map. Set column_name to NA to remove a mapping.
    #' @param value_map_df A dataframe with columns 'role' and 'values'
    #'        to update the value_map. The 'values' column should contain
    #'        comma-separated strings of allowed values. Set to NA to remove.
    #' @param mode Either "merge" (default) to merge with existing maps,
    #'        or "replace" to completely replace existing maps.
    #' @param validate_columns Whether to validate that mapped columns exist
    #'        in the dataset (default TRUE).
    #' @param stage The data stage to validate against ("raw", "standardized", "clean")
    #' @return A list with 'success' (logical), 'variables_updated' (count),
    #'         'values_updated' (count), and 'warnings' (character vector)
    #'
    #' @details
    #' Future UI Integration: This method is designed to be called from a modal
    #' UI that allows users to manually configure variable and value mappings.
    #' The UI would present the user with:
    #' - A table showing current variable_map entries
    #' - A table showing current value_map entries
    #' - Options to add, edit, or remove mappings
    #' - Validation feedback on invalid mappings
    #'
    #' Placeholder: UI modal implementation will be developed in a separate branch.
    repair_maps = function(
      variable_map_df = NULL,
      value_map_df = NULL,
      mode = c("merge", "replace"),
      validate_columns = TRUE,
      stage = "raw"
    ) {
      mode <- match.arg(mode)

      result <- list(
        success = TRUE,
        variables_updated = 0,
        values_updated = 0,
        warnings = character(0)
      )

      phrutils::phr_try(
        {
          df <- if (validate_columns) self$get_data(stage) else NULL
          data_cols <- if (!is.null(df)) names(df) else character(0)

          # --- VARIABLE MAP UPDATES ---

          if (!is.null(variable_map_df)) {
            # Validate input structure
            if (!is.data.frame(variable_map_df)) {
              phrutils::phr_error(
                self$dataset_name,
                phrutils::phr_txt("variable_map_df must be a data frame.")
              )
            }

            required_cols <- c("role", "column_name")
            missing <- setdiff(required_cols, names(variable_map_df))
            if (length(missing) > 0) {
              phrutils::phr_error(
                self$dataset_name,
                phrutils::phr_txt(
                  "variable_map_df missing required columns: {paste(missing, collapse=', ')}"
                )
              )
            }

            # Start with empty map if replacing
            if (mode == "replace") {
              # Keep uuid mapping as it's critical
              uuid_mapping <- self$.variable_map$uuid
              self$.variable_map <- list(uuid = uuid_mapping)
            }

            # Process each row
            for (i in seq_len(nrow(variable_map_df))) {
              row <- variable_map_df[i, ]
              role <- as.character(row$role)
              col_name <- as.character(row$column_name)

              if (is.na(role) || role == "") {
                next
              }

              # Remove mapping if column_name is NA
              if (is.na(col_name) || col_name == "") {
                if (role != "uuid") {
                  # Never remove uuid mapping
                  self$.variable_map[[role]] <- NULL
                  result$variables_updated <- result$variables_updated + 1
                }
                next
              }

              # Validate column exists if requested
              if (
                validate_columns &&
                  length(data_cols) > 0 &&
                  !(col_name %in% data_cols)
              ) {
                warn_msg <- phrutils::phr_txt(
                  "Column '{col_name}' for role '{role}' not found in dataset."
                )
                result$warnings <- c(result$warnings, warn_msg)
                phrutils::phr_warning(self$dataset_name, warn_msg)
                next
              }

              # Set the mapping
              self$.variable_map[[role]] <- col_name
              result$variables_updated <- result$variables_updated + 1
            }
          }

          # --- VALUE MAP UPDATES ---

          if (!is.null(value_map_df)) {
            # Validate input structure
            if (!is.data.frame(value_map_df)) {
              phrutils::phr_error(
                self$dataset_name,
                phrutils::phr_txt("value_map_df must be a data frame.")
              )
            }

            required_cols <- c("role", "values")
            missing <- setdiff(required_cols, names(value_map_df))
            if (length(missing) > 0) {
              phrutils::phr_error(
                self$dataset_name,
                phrutils::phr_txt(
                  "value_map_df missing required columns: {paste(missing, collapse=', ')}"
                )
              )
            }

            # Start with empty map if replacing
            if (mode == "replace") {
              self$.value_map <- list()
            }

            # Process each row
            for (i in seq_len(nrow(value_map_df))) {
              row <- value_map_df[i, ]
              role <- as.character(row$role)
              values_str <- as.character(row$values)

              if (is.na(role) || role == "") {
                next
              }

              # Remove mapping if values is NA
              if (is.na(values_str) || values_str == "") {
                self$.value_map[[role]] <- NULL
                result$values_updated <- result$values_updated + 1
                next
              }

              # Parse comma-separated values
              parsed_values <- trimws(strsplit(values_str, ",")[[1]])
              parsed_values <- parsed_values[parsed_values != ""]

              if (length(parsed_values) == 0) {
                self$.value_map[[role]] <- NULL
              } else {
                self$.value_map[[role]] <- parsed_values
              }
              result$values_updated <- result$values_updated + 1
            }
          }

          if (result$variables_updated > 0 || result$values_updated > 0) {
            phrutils::phr_message(
              phrutils::phr_txt(
                "Updated {result$variables_updated} variable mapping(s) and {result$values_updated} value mapping(s) for {self$dataset_name}."
              )
            )
          }

          result$success <- length(result$warnings) == 0

          return(result)
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "$repair_maps")
      )
    },

    # Placeholder for future UI modal integration
    # TODO: Implement in separate branch
    #
    # show_repair_maps_modal = function(session = NULL) {
    #   # This will be implemented in a separate branch to provide
    #   # a Shiny modal UI for manually editing variable and value maps.
    #   #
    #   # The modal will include:
    #   # - A table editor for variable_map (role -> column_name)
    #   # - A table editor for value_map (role -> allowed values)
    #   # - Validation feedback
    #   # - Save/Cancel buttons
    #   #
    #   # When saved, it will call repair_maps() with the edited dataframes.
    #   phr_message("UI modal for repair_maps not yet implemented.")
    #   return(NULL)
    # }

    #' @description
    #' Get the current variable and value maps as dataframes.
    #'
    #' Helper function to export maps in a format suitable for
    #' repair_maps() or UI display.
    #'
    #' @return A list with 'variable_map_df' and 'value_map_df' dataframes
    get_maps_as_df = function() {
      # Variable map dataframe
      if (length(self$.variable_map) > 0) {
        variable_map_df <- data.frame(
          role = names(self$.variable_map),
          column_name = unlist(self$.variable_map, use.names = FALSE),
          stringsAsFactors = FALSE
        )
      } else {
        variable_map_df <- data.frame(
          role = character(0),
          column_name = character(0),
          stringsAsFactors = FALSE
        )
      }

      # Value map dataframe
      if (length(self$.value_map) > 0) {
        value_map_df <- data.frame(
          role = names(self$.value_map),
          values = vapply(
            self$.value_map,
            function(v) paste(v, collapse = ","),
            character(1)
          ),
          stringsAsFactors = FALSE
        )
      } else {
        value_map_df <- data.frame(
          role = character(0),
          values = character(0),
          stringsAsFactors = FALSE
        )
      }

      list(
        variable_map_df = variable_map_df,
        value_map_df = value_map_df
      )
    }
  ),

  private = list(
    # @field ..raw_data Data frame containing the raw imported data.
    #   Never mutated after initialization. Access publicly via the
    #   `raw_data` active binding, `get(field = "raw_data")`, or
    #   `get_data(stage = "raw")`.
    # @keywords internal
    ..raw_data = NULL,

    # @field ..standardized_data Data frame after standardization of
    #   names, types, and values. Access publicly via the
    #   `standardized_data` active binding, `get(field = "standardized_data")`,
    #   or `get_data(stage = "standardized")`.
    # @keywords internal
    ..standardized_data = NULL,

    # @field ..clean_data Data frame after cleaning operations. Access
    #   publicly via the `clean_data` active binding,
    #   `get(field = "clean_data")`, or `get_data(stage = "clean")`.
    # @keywords internal
    ..clean_data = NULL,

    # -- Structural metadata (moved from public in comment 5853431834) --

    # @field ..uuid Character name of the UUID column. Access via
    #   `get(field = "..uuid")` / `set(field = "..uuid", value = …)`.
    # @keywords internal
    ..uuid = NULL,

    # @field ..required_columns Character vector of required column names.
    # @keywords internal
    ..required_columns = NULL,

    # @field ..other_columns List of other (non-required) column names.
    # @keywords internal
    ..other_columns = list(),

    # -- Maps and labels (moved from public) --
    #
    # These four fields are exposed via the read/write active bindings
    # `.variable_map`, `.value_map`, `.variable_label`, and `.value_label`
    # (single-dot prefix) so both `d$.variable_map` and the inherited
    # `d$get(field = ".variable_map")` API resolve them.

    # @field ..variable_map List mapping variable roles to column names.
    # @keywords internal
    ..variable_map = list(),

    # @field ..value_map List mapping canonical values to dataset values.
    # @keywords internal
    ..value_map = list(),

    # @field ..variable_label Named list of presentation-ready variable
    #   labels (populated by ..map_schema_labels()).
    # @keywords internal
    ..variable_label = list(),

    # @field ..value_label Named list of presentation-ready value labels
    #   for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..value_label = list(),

    # -- Schemas (moved from public) --

    # @field ..variable_schema Variable schema with types and allowed values.
    # @keywords internal
    ..variable_schema = NULL,

    # @field ..indicator_schema Schema for indicator calculations.
    # @keywords internal
    ..indicator_schema = NULL,

    # @field ..dependency_schema Schema for variable dependencies.
    # @keywords internal
    ..dependency_schema = NULL,

    # -- Log objects and diagnostic data (moved from public) --

    # @field ..cleaning_log CleaningLog R6 object for tracking data
    #   corrections. Invoke its methods via
    #   `call(field = "..cleaning_log", member = "...")`.
    # @keywords internal
    ..cleaning_log = NULL,

    # @field ..deletion_log DeletionLog R6 object for tracking deletions.
    #   Invoke its methods via `call(field = "..deletion_log",
    #   member = "...")`.
    # @keywords internal
    ..deletion_log = NULL,

    # @field ..data_quality_flags Data frame storing quality flags per row.
    # @keywords internal
    ..data_quality_flags = NULL,

    # @field ..data_diagnostics List of diagnostic results.
    # @keywords internal
    ..data_diagnostics = NULL,

    # @field ..cleaning_log_issues Data frame of cleaning-log rows that
    #   could not be applied (e.g. coercion failures).
    # @keywords internal
    ..cleaning_log_issues = NULL,

    # @description Convert one of the class's nested-list schemas
    #   (variable, indicator, or dependency) to a flat data-frame form.
    #   Consolidates the previous private wrappers
    #   \code{..data_schema_to_table} / \code{..indicator_schema_to_table} /
    #   \code{..dependency_schema_to_table} into a single dispatcher and
    #   inlines the row-building logic that previously lived in the
    #   exported \code{data_schema_to_table()},
    #   \code{indicator_schema_to_table()} and
    #   \code{dependency_schema_to_table()} utilities.
    # @param schema_type Character scalar identifying which schema to
    #   convert: one of \code{"variable"}, \code{"indicator"} or
    #   \code{"dependency"}.
    # @param schema_list Optional nested-list schema to convert. When
    #   \code{NULL} (default), the object's own \code{variable_schema},
    #   \code{indicator_schema} or \code{dependency_schema} is used.
    # @return A tibble representation of the requested schema.
    # @keywords internal
    ..schema_to_table = function(
      schema_type = c("variable", "indicator", "dependency"),
      schema_list = NULL
    ) {
      schema_type <- match.arg(schema_type)

      if (is.null(schema_list)) {
        schema_list <- switch(
          schema_type,
          variable   = private$..variable_schema,
          indicator  = private$..indicator_schema,
          dependency = private$..dependency_schema
        )
      }

      switch(
        schema_type,
        variable = {
          # ---- Inlined body of data_schema_to_table() ----
          data_validate_schema_to_table(
            schema_list = schema_list,
            origin      = "..schema_to_table(variable)"
          )

          fixed_cols <- c(
            "rule_type", "variable", "value", "required", "type",
            "question_type", "is_other", "other_column_link",
            "allowed", "col_names",
            "unique",
            "variable_label_en", "variable_label_fr", "variable_label_ar",
            "value_label_en",    "value_label_fr",    "value_label_ar",
            "label", "comment"
          )

          empty_row <- function() {
            x <- setNames(as.list(rep(NA_character_, length(fixed_cols))), fixed_cols)
            x$required <- NA
            x
          }

          out <- list()

          vars <- unique(c(
            names(schema_list$types %||% list()),
            names(schema_list$allowed_values %||% list()),
            names(schema_list$value_map %||% list()),
            names(schema_list$question_types %||% list()),
            names(schema_list$is_other %||% list()),
            names(schema_list$other_column_link %||% list()),
            schema_list$required %||% character(0)
          ))

          for (v in vars) {
            has_value_map <- !is.null(schema_list$value_map[[v]])

            if (has_value_map) {
              value_mappings <- schema_list$value_map[[v]]

              for (canonical_value in names(value_mappings)) {
                row <- empty_row()
                row$rule_type <- "variable"
                row$variable  <- v
                row$value     <- canonical_value
                row$required  <- v %in% (schema_list$required %||% character(0))

                if (!is.null(schema_list$types[[v]])) {
                  row[["type"]] <- schema_list$types[[v]]
                } else {
                  row[["type"]] <- NA_character_
                }

                dataset_values <- value_mappings[[canonical_value]]
                if (!is.null(dataset_values) && length(dataset_values) > 0) {
                  row[["allowed"]] <- paste(dataset_values, collapse = ",")
                } else {
                  row[["allowed"]] <- NA_character_
                }

                if (!is.null(schema_list$col_names[[v]])) {
                  row[["col_names"]] <- paste(schema_list$col_names[[v]], collapse = ",")
                } else {
                  row[["col_names"]] <- NA_character_
                }

                if (!is.null(schema_list$unique) && v %in% schema_list$unique) {
                  row$unique <- "TRUE"
                } else {
                  row$unique <- NA_character_
                }

                if (!is.null(schema_list$question_types[[v]])) {
                  row$question_type <- schema_list$question_types[[v]]
                } else {
                  row$question_type <- NA_character_
                }

                if (!is.null(schema_list$is_other[[v]])) {
                  row$is_other <- ifelse(isTRUE(schema_list$is_other[[v]]), "TRUE", "FALSE")
                } else {
                  row$is_other <- NA_character_
                }

                if (!is.null(schema_list$other_column_link[[v]])) {
                  if (is.character(schema_list$other_column_link[[v]])) {
                    row$other_column_link <- paste(schema_list$other_column_link[[v]], collapse = ",")
                  } else {
                    row$other_column_link <- as.character(schema_list$other_column_link[[v]])
                  }
                } else {
                  row$other_column_link <- NA_character_
                }

                for (lang in c("en", "fr", "ar")) {
                  col <- paste0("variable_label_", lang)
                  lbl <- (schema_list$variable_labels %||% list())[[lang]][[v]]
                  row[[col]] <- if (!is.null(lbl)) as.character(lbl) else NA_character_
                }

                for (lang in c("en", "fr", "ar")) {
                  col <- paste0("value_label_", lang)
                  lbl <- (schema_list$value_labels %||% list())[[lang]][[v]][[canonical_value]]
                  row[[col]] <- if (!is.null(lbl)) as.character(lbl) else NA_character_
                }

                out <- append(out, list(row))
              }
            } else {
              row <- empty_row()
              row$rule_type <- "variable"
              row$variable  <- v
              row$value     <- NA_character_
              row$required  <- v %in% (schema_list$required %||% character(0))

              if (!is.null(schema_list$types[[v]])) {
                row[["type"]] <- schema_list$types[[v]]
              } else {
                row[["type"]] <- NA_character_
              }

              if (!is.null(schema_list$allowed_values[[v]])) {
                row[["allowed"]] <- paste(schema_list$allowed_values[[v]], collapse = ",")
              } else {
                row[["allowed"]] <- NA_character_
              }

              if (!is.null(schema_list$col_names[[v]])) {
                row[["col_names"]] <- paste(schema_list$col_names[[v]], collapse = ",")
              } else {
                row[["col_names"]] <- NA_character_
              }

              if (!is.null(schema_list$unique) && v %in% schema_list$unique) {
                row$unique <- "TRUE"
              } else {
                row$unique <- NA_character_
              }

              if (!is.null(schema_list$question_types[[v]])) {
                row$question_type <- schema_list$question_types[[v]]
              } else {
                row$question_type <- NA_character_
              }

              if (!is.null(schema_list$is_other[[v]])) {
                row$is_other <- ifelse(isTRUE(schema_list$is_other[[v]]), "TRUE", "FALSE")
              } else {
                row$is_other <- NA_character_
              }

              if (!is.null(schema_list$other_column_link[[v]])) {
                if (is.character(schema_list$other_column_link[[v]])) {
                  row$other_column_link <- paste(schema_list$other_column_link[[v]], collapse = ",")
                } else {
                  row$other_column_link <- as.character(schema_list$other_column_link[[v]])
                }
              } else {
                row$other_column_link <- NA_character_
              }

              for (lang in c("en", "fr", "ar")) {
                col <- paste0("variable_label_", lang)
                lbl <- (schema_list$variable_labels %||% list())[[lang]][[v]]
                row[[col]] <- if (!is.null(lbl)) as.character(lbl) else NA_character_
              }

              for (lang in c("en", "fr", "ar")) {
                row[[paste0("value_label_", lang)]] <- NA_character_
              }

              out <- append(out, list(row))
            }
          }

          deps <- schema_list$dependencies %||% list()
          if (length(deps) > 0) {
            for (dep_name in names(deps)) {
              d <- deps[[dep_name]]
              row <- empty_row()
              row[["rule_type"]] <- "dependency"
              row[["variable"]]  <- NA_character_
              row[["variables"]] <- paste(d[["variables"]], collapse = ",")
              row[["condition_if"]] <- d[["condition_if"]] %||% NA_character_
              row[["then"]]        <- d[["then"]]         %||% NA_character_

              if (grepl("^flag_", dep_name)) {
                row[["dep_group"]] <- sub("^flag_", "", dep_name)
              } else if (grepl("^dq_dep_", dep_name)) {
                row[["dep_group"]] <- NA_character_
              } else {
                row[["dep_group"]] <- dep_name
              }

              row[["action"]] <- d[["action"]] %||% NA_character_

              out <- append(out, list(row))
            }
          }

          soft <- schema_list$soft_dependencies %||% list()
          if (length(soft) > 0) {
            for (dep_name in names(soft)) {
              d <- soft[[dep_name]]
              row <- empty_row()
              row[["rule_type"]] <- "soft_dependency"
              row[["variable"]]  <- ""
              row[["variables"]] <- paste(d[["variables"]], collapse = ",")
              row[["condition_if"]] <- d[["condition_if"]] %||% NA_character_
              row[["then"]]        <- d[["then"]]         %||% NA_character_

              if (grepl("^flag_", dep_name)) {
                row[["dep_group"]] <- sub("^flag_", "", dep_name)
              } else if (grepl("^dq_soft_dep_", dep_name)) {
                row[["dep_group"]] <- NA_character_
              } else {
                row[["dep_group"]] <- dep_name
              }

              row[["action"]] <- d[["action"]] %||% NA_character_

              out <- append(out, list(row))
            }
          }

          tibble::as_tibble(do.call(
            rbind,
            lapply(out, function(x) as.data.frame(x, stringsAsFactors = FALSE))
          ))
        },
        indicator = {
          # ---- Inlined body of indicator_schema_to_table() ----
          if (is.null(schema_list) || !is.list(schema_list)) {
            phrutils::phr_error("..schema_to_table(indicator)", "Indicator schema must be a list object.")
          }

          fixed_cols <- c(
            "indicator_name",
            "function_name",
            "variables",
            "arguments",
            "label",
            "comment"
          )

          empty_row <- function() {
            setNames(as.list(rep(NA_character_, length(fixed_cols))), fixed_cols)
          }

          out <- list()

          if (length(schema_list) > 0) {
            for (ind_name in names(schema_list)) {
              ind <- schema_list[[ind_name]]
              row <- empty_row()

              row[["indicator_name"]] <- ind[["indicator_name"]] %||% ind_name
              row[["function_name"]]  <- ind[["function_name"]] %||% NA_character_
              row[["variables"]]      <- paste(ind[["variables"]] %||% character(0), collapse = ",")
              row[["label"]]          <- ind[["label"]] %||% NA_character_
              row[["comment"]]        <- ind[["comment"]] %||% NA_character_

              args <- ind[["arguments"]] %||% list()
              if (length(args) > 0) {
                arg_strings <- mapply(
                  function(name, value) paste0(name, "=", value),
                  names(args),
                  args,
                  SIMPLIFY = TRUE
                )
                row[["arguments"]] <- paste(arg_strings, collapse = ",")
              } else {
                row[["arguments"]] <- NA_character_
              }

              out <- append(out, list(row))
            }
          }

          if (length(out) == 0) {
            return(tibble::tibble(
              indicator_name = character(0),
              function_name = character(0),
              variables = character(0),
              arguments = character(0),
              label = character(0),
              comment = character(0)
            ))
          }

          tibble::as_tibble(do.call(
            rbind,
            lapply(out, function(x) as.data.frame(x, stringsAsFactors = FALSE))
          ))
        },
        dependency = {
          # ---- Inlined body of dependency_schema_to_table() ----
          dependency_validate_schema_to_table(
            dependency_schema_list = schema_list,
            origin = "..schema_to_table(dependency)"
          )

          fixed_cols <- c(
            "rule_type",
            "dep_name",
            "variables",
            "condition_if",
            "then",
            "action",
            "label",
            "comment"
          )

          empty_row <- function() {
            setNames(as.list(rep(NA_character_, length(fixed_cols))), fixed_cols)
          }

          out <- list()

          deps <- schema_list$dependencies %||% list()
          if (length(deps) > 0) {
            for (dep_name in names(deps)) {
              d <- deps[[dep_name]]
              row <- empty_row()

              row[["rule_type"]]    <- "dependency"
              row[["dep_name"]]     <- dep_name
              row[["variables"]]    <- paste(d[["variables"]] %||% character(0), collapse = ",")
              row[["condition_if"]] <- d[["condition_if"]] %||% NA_character_
              row[["then"]]         <- d[["then"]] %||% NA_character_
              row[["action"]]       <- d[["action"]] %||% NA_character_
              row[["label"]]        <- d[["label"]] %||% NA_character_
              row[["comment"]]      <- d[["comment"]] %||% NA_character_

              out <- append(out, list(row))
            }
          }

          soft <- schema_list$soft_dependencies %||% list()
          if (length(soft) > 0) {
            for (dep_name in names(soft)) {
              d <- soft[[dep_name]]
              row <- empty_row()

              row[["rule_type"]]    <- "soft_dependency"
              row[["dep_name"]]     <- dep_name
              row[["variables"]]    <- paste(d[["variables"]] %||% character(0), collapse = ",")
              row[["condition_if"]] <- d[["condition_if"]] %||% NA_character_
              row[["then"]]         <- d[["then"]] %||% NA_character_
              row[["action"]]       <- d[["action"]] %||% NA_character_
              row[["label"]]        <- d[["label"]] %||% NA_character_
              row[["comment"]]      <- d[["comment"]] %||% NA_character_

              out <- append(out, list(row))
            }
          }

          if (length(out) == 0) {
            return(tibble::tibble(
              rule_type = character(0),
              dep_name = character(0),
              variables = character(0),
              condition_if = character(0),
              then = character(0),
              action = character(0),
              label = character(0),
              comment = character(0)
            ))
          }

          tibble::as_tibble(do.call(
            rbind,
            lapply(out, function(x) as.data.frame(x, stringsAsFactors = FALSE))
          ))
        }
      )
    },

    # @description Private, unified export method that converts one of
    #   the object's nested-list schemas (variable, indicator, or
    #   dependency) into a data frame with logging and error handling.
    #   Intended to be invoked through the inherited \code{Asset$call()}
    #   API, e.g.
    #   \code{d$call(field = "..export_schema_to_table", schema_type = "variable")}.
    # @param schema_type Character scalar identifying the schema to
    #   export: one of \code{"variable"}, \code{"indicator"}, or
    #   \code{"dependency"}.
    # @return Data frame representation of the requested schema, or
    #   \code{NULL} when the schema is not set (a warning is emitted).
    # @keywords internal
    ..export_schema_to_table = function(
      schema_type = c("variable", "indicator", "dependency")
    ) {
      schema_type <- match.arg(schema_type)
      phrutils::phr_try(
        {
          schema <- switch(
            schema_type,
            variable   = private$..variable_schema,
            indicator  = private$..indicator_schema,
            dependency = private$..dependency_schema
          )
          if (is.null(schema)) {
            phrutils::phr_warning(
              self$dataset_name,
              phrutils::phr_txt(
                "No {schema_type} schema available to export."
              )
            )
            return(NULL)
          }
          tbl <- private$..schema_to_table(
            schema_type = schema_type,
            schema_list = schema
          )
          phrutils::phr_message(
            phrutils::phr_txt(
              "Exported {schema_type} schema from {self$dataset_name} ({nrow(tbl)} row(s))."
            )
          )
          tbl
        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$..export_schema_to_table")
      )
    },
    #' @description
    #' Resolve a "@variable_map$role" schema reference (or a bare role name)
    #' to its mapped dataset column name.
    #'
    #' Centralizes the parsing/lookup logic used when indicator schema
    #' arguments (single values or elements of a `c(...)` vector) reference
    #' `variable_map` using the `@variable_map$role` tag syntax.
    #'
    #' @param ref Character string to check/resolve. When `tags = TRUE`,
    #'   this is expected to be a `@variable_map$role` tag (e.g.
    #'   "@variable_map$fever"). When `tags = FALSE`, `ref` is treated
    #'   directly as the role name, with no `@` stripping.
    #' @param tags Logical. If `TRUE`, `ref` is parsed as a `@variable_map$role`
    #'   tag. If `FALSE` (default), `ref` is resolved directly as a role name.
    #'
    #' @return A list with:
    #'   * `is_reference` - TRUE if `ref` uses the `@variable_map$` tag syntax
    #'     (when `tags = TRUE`), or always TRUE (when `tags = FALSE`).
    #'   * `role` - the extracted role name (NULL if not a reference).
    #'   * `value` - the resolved column name, or NULL if the role is not
    #'     mapped (or not a valid/non-empty mapping).
    ..resolve_variable_map_reference = function(ref, tags = FALSE) {
      if (!is.character(ref) || length(ref) != 1) {
        return(list(is_reference = FALSE, role = NULL, value = NULL))
      }

      if (tags) {
        if (!grepl("^@variable_map\\$", ref)) {
          return(list(is_reference = FALSE, role = NULL, value = NULL))
        }
        role <- sub("^@variable_map\\$", "", ref)
      } else {
        role <- ref
      }

      value <- self$.variable_map[[role]]

      if (is.null(value) || identical(value, "")) {
        value <- NULL
      }

      list(is_reference = TRUE, role = role, value = value)
    },

    #' @description
    #' Resolve a "@value_map$role" or "@value_map$role$canonical_value"
    #' schema reference (or a bare "role"/"role$canonical_value" string) to
    #' its mapped dataset value(s).
    #'
    #' Centralizes the parsing/lookup logic used when indicator schema
    #' arguments (single values or elements of a `c(...)` vector) reference
    #' `value_map` using the `@value_map$role` or
    #' `@value_map$role$canonical_value` tag syntax.
    #'
    #' @param ref Character string to check/resolve. When `tags = TRUE`,
    #'   this is expected to be a `@value_map$role` or
    #'   `@value_map$role$canonical_value` tag (e.g.
    #'   "@value_map$status$yes"). When `tags = FALSE`, `ref` is treated
    #'   directly as "role" or "role$canonical_value", with no `@` stripping.
    #' @param tags Logical. If `TRUE`, `ref` is parsed as a `@value_map$...`
    #'   tag. If `FALSE` (default), `ref` is resolved directly.
    #'
    #' @return A list with:
    #'   * `is_reference` - TRUE if `ref` uses the `@value_map$` tag syntax
    #'     (when `tags = TRUE`), or always TRUE (when `tags = FALSE`).
    #'   * `role` - the extracted role name (NULL if not a reference).
    #'   * `canonical_value` - the extracted canonical value name, if provided
    #'     (NULL otherwise).
    #'   * `role_found` - TRUE if `role` exists in `value_map`.
    #'   * `value` - the resolved value(s): a single mapping (when
    #'     `canonical_value` is given), the entire role's value map (when it
    #'     is not), or NULL if the role/canonical value could not be resolved.
    ..resolve_value_map_reference = function(ref, tags = FALSE) {
      if (!is.character(ref) || length(ref) != 1) {
        return(list(
          is_reference = FALSE,
          role = NULL,
          canonical_value = NULL,
          role_found = FALSE,
          value = NULL
        ))
      }

      if (tags) {
        if (!grepl("^@value_map\\$", ref)) {
          return(list(
            is_reference = FALSE,
            role = NULL,
            canonical_value = NULL,
            role_found = FALSE,
            value = NULL
          ))
        }
        ref <- sub("^@value_map\\$", "", ref)
      }

      parts <- strsplit(ref, "\\$")[[1]]
      role <- parts[1]
      canonical_value <- if (length(parts) == 2) parts[2] else NULL
      role_map <- self$.value_map[[role]]

      if (is.null(role_map)) {
        return(list(
          is_reference = TRUE,
          role = role,
          canonical_value = canonical_value,
          role_found = FALSE,
          value = NULL
        ))
      }

      value <- if (!is.null(canonical_value)) {
        role_map[[canonical_value]]
      } else {
        role_map
      }

      list(
        is_reference = TRUE,
        role = role,
        canonical_value = canonical_value,
        role_found = TRUE,
        value = value
      )
    },

    #' @description
    #' Helper function to look up action for a flag from schema dependencies.
    #'
    #' @param flag_name Name of the flag column (e.g., "flag_other_check" or "dq_dep_1")
    #' @return Character string with action ("flag_autoclean", "flag_warning", etc.) or empty string
    ..get_flag_action_from_schema = function(flag_name) {
      phrutils::phr_try(
        {
          # Check separate dependency_schema first
          if (!is.null(private$..dependency_schema)) {
            # Check hard dependencies
            if (flag_name %in% names(private$..dependency_schema$dependencies)) {
              dep <- private$..dependency_schema$dependencies[[flag_name]]
              action <- dep[["action"]] %||% ""
              return(action)
            }
            # Check soft dependencies
            if (
              flag_name %in% names(private$..dependency_schema$soft_dependencies)
            ) {
              dep <- private$..dependency_schema$soft_dependencies[[flag_name]]
              action <- dep[["action"]] %||% ""
              return(action)
            }
          }

          # Fall back to variable_schema dependencies for backward compatibility
          if (
            !is.null(private$..variable_schema) &&
              !is.null(private$..variable_schema$dependencies)
          ) {
            if (flag_name %in% names(private$..variable_schema$dependencies)) {
              dep <- private$..variable_schema$dependencies[[flag_name]]
              action <- dep[["action"]] %||% ""
              return(action)
            }
          }

          # No match found
          return("")
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "$get_flag_action_from_schema")
      )
    },
    #' @description
    #' Helper function to look up variables for a flag from schema dependencies.
    #'
    #' @param flag_name Name of the flag column (e.g., "flag_other_check" or "dq_dep_1")
    #' @return Character vector of canonical variable names involved in the dependency, or NULL
    ..get_flag_variables_from_schema = function(flag_name) {
      phrutils::phr_try(
        {
          # Check separate dependency_schema first
          if (!is.null(private$..dependency_schema)) {
            # Check hard dependencies
            if (flag_name %in% names(private$..dependency_schema$dependencies)) {
              dep <- private$..dependency_schema$dependencies[[flag_name]]
              variables <- dep[["variables"]] %||% NULL
              return(variables)
            }
            # Check soft dependencies
            if (
              flag_name %in% names(private$..dependency_schema$soft_dependencies)
            ) {
              dep <- private$..dependency_schema$soft_dependencies[[flag_name]]
              variables <- dep[["variables"]] %||% NULL
              return(variables)
            }
          }

          # Fall back to variable_schema dependencies for backward compatibility
          if (
            !is.null(private$..variable_schema) &&
              !is.null(private$..variable_schema$dependencies)
          ) {
            if (flag_name %in% names(private$..variable_schema$dependencies)) {
              dep <- private$..variable_schema$dependencies[[flag_name]]
              variables <- dep[["variables"]] %||% NULL
              return(variables)
            }
          }

          # No match found: either not a dependency check, or dependency has no variables field
          return(NULL)
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "$get_flag_variables_from_schema")
      )
    },
    #' INTERNAL HELPER -- apply cleaning log changes (option A authoritative mode)
    #'
    #' @description
    #' Internal helper to apply cleaning changes from a log data frame to a dataset
    #'
    #' @param df Data frame to modify
    #' @param log_df Data frame containing cleaning log entries
    #' @param uuid_col Character name of UUID column in df
    #'
    #' @return Modified data frame with cleaning changes applied
    ..apply_cleaning_changes = function(df, log_df, uuid_col) {
      issues <- list()

      n_log <- nrow(log_df)
      if (n_log == 0) {
        return(df)
      }

      # OPTIMIZATION (#2): Precompute a single hash-based lookup from log
      # row uuid -> df row index (via match()) once, instead of doing an
      # O(nrow(df)) linear `which(... == ...)` scan for every cleaning log
      # row (which made this O(nrow(log_df) * nrow(df)) overall). Rows
      # whose uuid is duplicated in `df` are flagged so that behavior
      # matches the original `length(idx) == 1` guard exactly.
      df_uuid_chr <- as.character(df[[uuid_col]])
      dup_uuid <- duplicated(df_uuid_chr) | duplicated(df_uuid_chr, fromLast = TRUE)
      match_idx <- match(as.character(log_df$uuid), df_uuid_chr)

      # OPTIMIZATION (#2): Accumulate the (position, value) pairs to write
      # per target column, then apply one vectorized assignment per column
      # after the loop instead of writing `df[[col]][idx] <- typed_val`
      # (a full-column copy-on-write) on every single log row.
      pending_positions <- list()
      pending_values <- list()

      for (i in seq_len(n_log)) {
        row <- log_df[i, ]

        u <- row$uuid
        col <- row$question.name
        new_val <- row$new.value

        idx <- match_idx[i]
        idx_valid <- !is.na(idx) && !dup_uuid[idx]

        if (
          idx_valid && col %in% names(df) && isTRUE(row$changed == "yes")
        ) {
          # Coerce new_val to the target column type before assignment
          target_class <- class(df[[col]])[1]

          typed_val <- if (is.na(new_val) || identical(new_val, "NA")) {
            # NA is always safe for any column type
            switch(
              target_class,
              "numeric" = NA_real_,
              "integer" = NA_integer_,
              "logical" = NA,
              "character" = NA_character_,
              "Date" = as.Date(NA),
              NA
            )
          } else if (target_class == "character") {
            # Character columns accept anything
            as.character(new_val)
          } else {
            # Map integer to numeric for is_safely_coercible check
            check_type <- if (target_class == "integer") {
              "numeric"
            } else {
              target_class
            }

            if (!phrutils::is_safely_coercible(new_val, check_type)) {
              phrutils::phr_warning(
                message = sprintf(
                  "Cleaning log row %d skipped: new.value '%s' cannot be safely coerced to %s for column '%s' (uuid: %s).",
                  i,
                  new_val,
                  target_class,
                  col,
                  u
                ),
                origin = ""
              )
              issues[[length(issues) + 1L]] <- data.frame(
                row_index = i,
                uuid = as.character(u),
                question.name = col,
                new.value = as.character(new_val),
                target_type = target_class,
                reason = sprintf(
                  "Value '%s' is not coercible to %s",
                  new_val,
                  target_class
                ),
                stringsAsFactors = FALSE
              )
              next
            }

            switch(
              target_class,
              "numeric" = suppressWarnings(as.numeric(new_val)),
              "integer" = suppressWarnings(as.integer(new_val)),
              # mirrors the accepted values validated by is_safely_coercible("logical")
              "logical" = {
                lc <- tolower(trimws(as.character(new_val)))
                if (lc %in% c("true", "t", "1")) {
                  TRUE
                } else if (lc %in% c("false", "f", "0")) {
                  FALSE
                } else {
                  NA
                }
              },
              "Date" = tryCatch(phrutils::phr_convert_date(new_val), error = function(e) {
                as.Date(NA)
              }),
              "POSIXct" = tryCatch(
                phrutils::phr_convert_datetime(new_val),
                error = function(e) {
                  phrutils::phr_warning(
                    sprintf(
                      "Cleaning log row %d: phr_convert_datetime('%s') failed for column '%s' (uuid: %s): %s",
                      i,
                      new_val,
                      col,
                      u,
                      conditionMessage(e)
                    ),
                    call. = FALSE
                  )
                  as.POSIXct(NA_real_, origin = "1970-01-01")
                }
              ),
              "POSIXlt" = tryCatch(
                as.POSIXlt(phrutils::phr_convert_datetime(new_val)),
                error = function(e) {
                  phrutils::phr_warning(
                    sprintf(
                      "Cleaning log row %d: phr_convert_datetime('%s') failed for column '%s' (uuid: %s): %s",
                      i,
                      new_val,
                      col,
                      u,
                      conditionMessage(e)
                    ),
                    call. = FALSE
                  )
                  as.POSIXlt(NA_real_, origin = "1970-01-01")
                }
              ),
              new_val
            )
          }

          pending_positions[[col]] <- c(pending_positions[[col]], idx)
          pending_values[[col]] <- c(pending_values[[col]], list(typed_val))
        }
      }

      # OPTIMIZATION (#2): Apply all accumulated edits for each column in a
      # single vectorized assignment. `do.call(c, ...)` (rather than
      # `unlist()`) is used so that Date/POSIXct/POSIXlt classes are
      # preserved when combining the per-row typed values. Positions are
      # written in original log-row order, so if the same df row/column
      # pair was edited more than once, the last edit wins -- identical to
      # the original sequential row-by-row assignment.
      for (col in names(pending_positions)) {
        pos <- pending_positions[[col]]
        vals <- do.call(c, pending_values[[col]])
        df[[col]][pos] <- vals
      }

      # Store any issues for user follow-up
      if (length(issues) > 0) {
        private$..cleaning_log_issues <- do.call(rbind, issues)
      }

      df
    },
    #' INTERNAL HELPER -- extract tokens from select_multiple values
    #'
    #' @description
    #' Internal helper to extract individual tokens from space-separated values
    #' in select_multiple columns.
    #'
    #' @param values Character vector containing space-separated values
    #'
    #' @return Character vector of unique tokens
    ..extract_select_multiple_tokens = function(values) {
      # Use lapply for efficient token collection
      token_list <- lapply(values, function(val) {
        if (!is.na(val) && nchar(val) > 0) {
          tokens <- trimws(strsplit(as.character(val), " ", fixed = TRUE)[[1]])
          tokens[tokens != ""]
        } else {
          character(0)
        }
      })
      # Flatten and return unique tokens
      unique(unlist(token_list, use.names = FALSE))
    },
    #' INTERNAL HELPER -- check if variable is select_multiple type
    #'
    #' @description
    #' Internal helper to check if a variable role is marked as select_multiple
    #' in the schema.
    #'
    #' @param var_role Variable role name
    #'
    #' @return Logical indicating if variable is select_multiple
    ..is_select_multiple = function(var_role) {
      if (is.null(private$..variable_schema)) {
        return(FALSE)
      }
      question_types <- private$..variable_schema$question_types %||% list()
      return(
        !is.null(question_types[[var_role]]) &&
          question_types[[var_role]] == "select_multiple"
      )
    },
    #' INTERNAL HELPER -- provide hints for expression parse errors
    #'
    #' @description
    #' Internal helper to provide helpful hints for common syntax errors in dependency expressions.
    #'
    #' @param expr Character string containing the R expression that failed to parse
    #' @param error_msg Character string containing the error message from parse attempt
    #'
    #' @return Character string with a helpful error message including hints
    ..get_expression_parse_hint = function(expr, error_msg) {
      # Provide helpful hints for common syntax errors in dependency expressions

      # Check for "unexpected symbol" which often means missing quotes
      if (grepl("unexpected symbol", error_msg, ignore.case = TRUE)) {
        # Check if expression contains c(...) which is common source of errors
        if (grepl("\\bc\\s*\\(", expr)) {
          return(paste0(
            "Syntax error in expression '",
            substr(expr, 1, 100),
            ifelse(nchar(expr) > 100, "...'", "'"),
            ". Common cause: Missing quotes around values in c() vector. ",
            "Ensure all string values in c() are properly quoted, e.g., ",
            "c('value1','value2') not c('value1',value2)."
          ))
        }
      }

      # Check for unbalanced quotes
      # Note: This is a simple heuristic that doesn't handle escaped quotes (e.g., \").
      # For dependency expressions, this is acceptable as they typically use simple quoted strings.
      single_quotes <- lengths(regmatches(
        expr,
        gregexpr("'", expr, fixed = TRUE)
      ))
      double_quotes <- lengths(regmatches(
        expr,
        gregexpr('"', expr, fixed = TRUE)
      ))

      if (single_quotes %% 2 != 0 || double_quotes %% 2 != 0) {
        return(paste0(
          "Syntax error in expression '",
          substr(expr, 1, 100),
          ifelse(nchar(expr) > 100, "...'", "'"),
          ". Detected unbalanced quotes. Check that all string values are properly quoted."
        ))
      }

      # Generic hint
      return(paste0(
        "Syntax error in expression '",
        substr(expr, 1, 100),
        ifelse(nchar(expr) > 100, "...'", "'"),
        ". Check R syntax: ",
        error_msg
      ))
    },
    #' INTERNAL HELPER -- translate expression to use mapped column names and values
    #'
    #' @description
    #' Internal helper to translate quality check expressions from canonical names/values
    #' to dataset-specific names/values using variable_map and value_map.
    #'
    #' Translation occurs in two steps:
    #' 1. Replace canonical variable names (roles) with dataset column names from variable_map
    #' 2. Replace canonical values with dataset values from value_map
    #'
    #'
    #' @param expr Character string containing the R expression to translate
    #' @param stage Data stage to use for validation ("raw", "standardized", "clean")
    #'
    #' @return Translated expression as a character string
    #'
    #' @details
    #' Supported operators for value translation:
    #' - Equality: `var == 'value'` or `'value' == var`
    #' - Inequality: `var != 'value'` or `'value' != var`
    #' - Set membership: `var %in% c('value1', 'value2', ...)`
    #'
    #' For single dataset values: Uses simple equality (`var == 'value'`)
    #' For multiple dataset values: Uses membership (`var %in% c('val1', 'val2')`)
    #' For %in% expressions: Expands each canonical value to its dataset values
    #'
    #' @note
    #' - Other operators (<, >, <=, >=, etc.) are not currently translated for values
    #' - Variable names in functions (e.g., is.na(var)) are translated
    #' - Special regex characters in names/values are automatically escaped
    ..translate_expression = function(expr, stage = "standardized") {
      if (is.null(expr) || expr == "") {
        return(expr)
      }

      translated <- expr

      # Helper function to escape special regex characters
      escape_regex <- function(str) {
        gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", str)
      }

      # Step 1: Replace canonical variable names with mapped dataset column names
      if (!is.null(self$.variable_map) && length(self$.variable_map) > 0) {
        for (role in names(self$.variable_map)) {
          dataset_col <- self$.variable_map[[role]]
          if (!is.null(dataset_col) && dataset_col != "") {
            # Use word boundaries to avoid partial replacements
            # Pattern: match role as a whole word (not part of another identifier)
            pattern <- paste0("\\b", escape_regex(role), "\\b")
            translated <- gsub(pattern, dataset_col, translated)
          }
        }
      }

      # Step 2: Replace canonical values with dataset values using value_map
      # This is more complex because we need to handle nested value_map structure
      # Supports ==, !=, and %in% operators for value translation
      if (!is.null(self$.value_map) && length(self$.value_map) > 0) {
        for (role in names(self$.value_map)) {
          value_mapping <- self$.value_map[[role]]

          # Check if it's nested format (canonical -> dataset values)
          is_nested <- is.list(value_mapping) &&
            !is.null(names(value_mapping)) &&
            length(names(value_mapping)) > 0 &&
            all(names(value_mapping) != "")

          if (is_nested) {
            # Get the dataset column name for this role
            dataset_col <- self$.variable_map[[role]]

            if (!is.null(dataset_col)) {
              dataset_col_escaped <- escape_regex(dataset_col)

              # Handle %in% expressions: var %in% c('val1', 'val2', ...)
              # We need to expand each canonical value in the c() to its dataset values

              # Pattern: variable %in% c('value1', 'value2', ...)
              # Match: dataset_col %in% c(...)
              pattern_in <- paste0(
                "\\b",
                dataset_col_escaped,
                "\\s*%in%\\s*c\\s*\\(([^)]+)\\)"
              )

              # Find all %in% expressions for this variable
              matches <- gregexpr(pattern_in, translated, perl = TRUE)
              match_data <- regmatches(translated, matches)

              if (length(match_data) > 0 && length(match_data[[1]]) > 0) {
                for (match_text in match_data[[1]]) {
                  # Extract the values inside c(...)
                  values_part <- sub(
                    paste0(".*", dataset_col_escaped, "\\s*%in%\\s*c\\s*\\("),
                    "",
                    match_text
                  )
                  values_part <- sub("\\)$", "", values_part)

                  # Parse comma-separated values, respecting quoted strings
                  # Use a more robust approach that handles commas inside quotes
                  value_items <- character(0)
                  current_item <- ""
                  in_quotes <- FALSE
                  quote_char <- ""

                  for (i in seq_len(nchar(values_part))) {
                    char <- substr(values_part, i, i)

                    if (!in_quotes && (char == "'" || char == '"')) {
                      # Start of quoted string
                      in_quotes <- TRUE
                      quote_char <- char
                      current_item <- paste0(current_item, char)
                    } else if (in_quotes && char == quote_char) {
                      # End of quoted string
                      in_quotes <- FALSE
                      current_item <- paste0(current_item, char)
                    } else if (!in_quotes && char == ",") {
                      # Comma outside quotes - separator
                      if (nchar(trimws(current_item)) > 0) {
                        value_items <- c(value_items, trimws(current_item))
                      }
                      current_item <- ""
                    } else {
                      # Regular character
                      current_item <- paste0(current_item, char)
                    }
                  }

                  # Add the last item
                  if (nchar(trimws(current_item)) > 0) {
                    value_items <- c(value_items, trimws(current_item))
                  }

                  # Process each value item
                  expanded_vals <- character(0)
                  has_na <- FALSE # Track if NA is in the list

                  for (item in value_items) {
                    # Check if this is an unquoted NA constant
                    item_trimmed <- trimws(item)
                    # NA is unquoted if it equals "NA" and doesn't have surrounding quotes
                    is_unquoted_na <- (item_trimmed == "NA" &&
                      !grepl("^['\"].*['\"]$", item_trimmed))

                    if (is_unquoted_na) {
                      # This is the R constant NA, not the string "NA"
                      has_na <- TRUE
                      next # Don't add to expanded_vals, handle separately
                    }

                    # Remove quotes to get the canonical value
                    item_clean <- gsub("^['\"]|['\"]$", "", item_trimmed)

                    # Look up in value_map
                    if (item_clean %in% names(value_mapping)) {
                      # This is a canonical value - expand it
                      dataset_vals <- value_mapping[[item_clean]]
                      expanded_vals <- c(expanded_vals, dataset_vals)
                    } else {
                      # Not a canonical value - keep as-is (might be a literal value)
                      expanded_vals <- c(expanded_vals, item_clean)
                    }
                  }

                  # Build the replacement expression
                  expanded_vals <- unique(expanded_vals) # Remove duplicates

                  # Build value list, handling NA specially
                  if (length(expanded_vals) > 0) {
                    replacement_vals <- paste0(
                      "'",
                      expanded_vals,
                      "'",
                      collapse = ", "
                    )
                  } else {
                    replacement_vals <- ""
                  }

                  # Add NA_character_ if NA was present
                  if (has_na) {
                    if (replacement_vals != "") {
                      replacement_vals <- paste0(
                        replacement_vals,
                        ", NA_character_"
                      )
                    } else {
                      replacement_vals <- "NA_character_"
                    }
                  }

                  replacement_expr <- paste0(
                    dataset_col,
                    " %in% c(",
                    replacement_vals,
                    ")"
                  )

                  # Replace this specific match using fixed string replacement
                  translated <- sub(
                    match_text,
                    replacement_expr,
                    translated,
                    fixed = TRUE
                  )
                }
              }

              # Handle == and != operators (existing logic)

              # Nested format: canonical_value -> c(dataset_value1, dataset_value2, ...)
              # For each canonical value, replace it with an expression that checks
              # if the variable is in the set of dataset values
              for (canonical_val in names(value_mapping)) {
                dataset_vals <- value_mapping[[canonical_val]]

                if (length(dataset_vals) > 0) {
                  # Escape special characters in canonical value
                  canonical_val_escaped <- escape_regex(canonical_val)

                  # Build the replacement expression for the canonical value
                  # Handle both single and multiple dataset values
                  if (length(dataset_vals) == 1) {
                    # Single value: can use simple equality
                    val_expr <- paste0("'", dataset_vals[1], "'")
                  } else {
                    # Multiple values: use %in% operator
                    val_list <- paste0(
                      "c(",
                      paste0("'", dataset_vals, "'", collapse = ", "),
                      ")"
                    )
                    val_expr <- val_list
                  }

                  # Pattern 1: variable == 'canonical_value'
                  pattern_eq <- paste0(
                    "\\b",
                    dataset_col_escaped,
                    "\\s*==\\s*['\"]",
                    canonical_val_escaped,
                    "['\"]"
                  )
                  if (length(dataset_vals) == 1) {
                    replacement_eq <- paste0(dataset_col, " == ", val_expr)
                  } else {
                    replacement_eq <- paste0(dataset_col, " %in% ", val_expr)
                  }
                  translated <- gsub(pattern_eq, replacement_eq, translated)

                  # Pattern 2: variable != 'canonical_value'
                  pattern_ne <- paste0(
                    "\\b",
                    dataset_col_escaped,
                    "\\s*!=\\s*['\"]",
                    canonical_val_escaped,
                    "['\"]"
                  )
                  if (length(dataset_vals) == 1) {
                    replacement_ne <- paste0(dataset_col, " != ", val_expr)
                  } else {
                    replacement_ne <- paste0(
                      "!(",
                      dataset_col,
                      " %in% ",
                      val_expr,
                      ")"
                    )
                  }
                  translated <- gsub(pattern_ne, replacement_ne, translated)

                  # Pattern 3: 'canonical_value' == variable (reversed order)
                  pattern_eq_rev <- paste0(
                    "['\"]",
                    canonical_val_escaped,
                    "['\"]\\s*==\\s*\\b",
                    dataset_col_escaped,
                    "\\b"
                  )
                  if (length(dataset_vals) == 1) {
                    replacement_eq_rev <- paste0(val_expr, " == ", dataset_col)
                  } else {
                    replacement_eq_rev <- paste0(
                      dataset_col,
                      " %in% ",
                      val_expr
                    )
                  }
                  translated <- gsub(
                    pattern_eq_rev,
                    replacement_eq_rev,
                    translated
                  )

                  # Pattern 4: 'canonical_value' != variable (reversed order)
                  pattern_ne_rev <- paste0(
                    "['\"]",
                    canonical_val_escaped,
                    "['\"]\\s*!=\\s*\\b",
                    dataset_col_escaped,
                    "\\b"
                  )
                  if (length(dataset_vals) == 1) {
                    replacement_ne_rev <- paste0(val_expr, " != ", dataset_col)
                  } else {
                    replacement_ne_rev <- paste0(
                      "!(",
                      dataset_col,
                      " %in% ",
                      val_expr,
                      ")"
                    )
                  }
                  translated <- gsub(
                    pattern_ne_rev,
                    replacement_ne_rev,
                    translated
                  )
                }
              }
            }
          }
        }
      }

      return(translated)
    },
    #' Validate Linked Objects
    #'
    #' @description
    #' Validates foreign key relationships with all linked objects
    #'
    #' @param stage_self Character string specifying data stage for this object (default: "clean")
    #' @param stage_other Character string specifying data stage for linked objects (default: "clean")
    #'
    #' @return Logical TRUE (invisibly) if all links valid, or list of missing foreign keys if problems found
    #'
    #' @details
    #' For each linked object:
    #' * Resolves key columns from roles
    #' * Checks for missing foreign key values
    #' * Reports orphaned records
    ..validate_links = function(stage_self = "clean", stage_other = "clean") {
      phrutils::phr_try(
        {
          if (length(self$linked_objects) == 0) {
            phrutils::phr_message(phrutils::phr_txt(
              "No linked objects to validate for {self$dataset_name}."
            ))
            return(invisible(TRUE))
          }

          problems <- list()

          for (nm in names(self$linked_objects)) {
            link <- self$linked_objects[[nm]]
            other <- link$object

            # 1. Resolve COLUMN NAMES
            key_self_col <- self$resolve_column(
              link$by_self_role,
              stage = stage_self
            )
            key_other_col <- other$resolve_column(
              link$by_other_role,
              stage = stage_other
            )

            # If either column is missing, skip (warning already emitted)
            if (is.null(key_self_col) || is.null(key_other_col)) {
              next
            }

            # 2. Extract DATA
            df_self <- self$get_data(stage_self)
            df_other <- other$get_data(stage_other)

            # 3. Compute missing foreign key VALUES
            missing_fk <- setdiff(
              unique(df_other[[key_other_col]]),
              unique(df_self[[key_self_col]])
            )

            if (length(missing_fk) > 0) {
              problems[[nm]] <- missing_fk
            }
          }

          # If no problems → TRUE
          if (length(problems) == 0) {
            phrutils::phr_message(phrutils::phr_txt(
              "All links validated successfully for {self$dataset_name}."
            ))
            return(invisible(TRUE))
          }

          # Otherwise return list of missing FKs
          phrutils::phr_warning(
            self$dataset_name,
            phrutils::phr_txt(
              "Link validation found missing foreign keys in: {paste(names(problems), collapse=', ')}"
            )
          )

          return(problems)
        },
        on_error = "abort",
        origin = paste0(self$dataset_name, "$validate_links")
      )
    },
    #' @description
    #' Automatically map schema variables to dataset columns and values.
    #'
    #' This method searches for default column names in the dataset as referenced
    #' in the schema's `col_names` field. If a matching column is found, it is
    #' added to the variable_map. For variables with value_map (non-numeric),
    #' it checks which canonical values' allowed dataset values exist in the data
    #' and builds the corresponding nested value_map structure.
    #'
    #' This is a "soft" search - found matches are added to maps, but no error
    #' is thrown if a variable or value is not found.
    #'
    #' @param stage The data stage to search in ("raw", "standardized", or "clean")
    #' @return Invisible self for method chaining
    ..map_schema_vars = function(stage = "raw") {
      phrutils::phr_try(
        {
          sch <- private$..variable_schema

          # Early exit if no schema is defined
          if (is.null(sch) || length(sch) == 0) {
            phrutils::phr_message(
              phrutils::phr_txt(
                "No variable schema defined for {self$dataset_name}; skipping auto-mapping."
              )
            )
            return(invisible(self))
          }

          df <- self$get_data(stage)

          if (is.null(df)) {
            phrutils::phr_warning(
              self$dataset_name,
              phrutils::phr_txt("No {stage} data available for schema variable mapping.")
            )
            return(invisible(self))
          }

          data_cols <- names(df)
          col_names_map <- sch$col_names %||% list()
          schema_value_map <- sch$value_map %||% list()
          allowed_vals <- sch$allowed_values %||% list()
          types <- sch$types %||% list()

          vars_mapped <- 0
          vals_mapped <- 0

          # OPTIMIZATION (#4): `.variable_map`/`.value_map` are active
          # bindings that read back the *entire* stored list on access and
          # write back the *entire* list on assignment (see the active
          # bindings at the bottom of this class). Writing
          # `self$.variable_map[[var_role]] <- ...` once per matched
          # variable therefore copies the whole map on every iteration,
          # making the loop O(n_vars^2) for schemas with many variables.
          # Accumulate into plain local lists instead, and write back to
          # the active bindings exactly once, after the loop.
          local_variable_map <- self$.variable_map
          local_value_map <- self$.value_map

          # OPTIMIZATION (#4): Hoist the `escape_regex()`/`extract_tokens()`
          # closures out of the `for (var_role in ...)` loop -- they don't
          # depend on any loop-local state, so they were being needlessly
          # re-created on every iteration.

          # Helper function to escape regex special characters
          # Escapes: . | ( ) \ ^ { } + $ * ? [ ]
          escape_regex <- function(str) {
            gsub("([.|()\\\\^{}+$*?\\[\\]])", "\\\\\\1", str)
          }

          # Helper function to extract tokens from select_multiple values
          extract_tokens <- function(values) {
            # Use lapply for efficient token collection
            token_list <- lapply(values, function(val) {
              if (!is.na(val) && nchar(val) > 0) {
                tokens <- trimws(strsplit(
                  as.character(val),
                  " ",
                  fixed = TRUE
                )[[1]])
                tokens[tokens != ""]
              } else {
                character(0)
              }
            })
            # Flatten and return unique tokens
            unique(unlist(token_list, use.names = FALSE))
          }

          # Iterate over schema variables that have col_names defined
          for (var_role in names(col_names_map)) {
            possible_cols <- col_names_map[[var_role]]

            # Search for matching column name in the dataset (in order of preference)
            matched_col <- NULL
            for (col_candidate in possible_cols) {
              if (col_candidate %in% data_cols) {
                matched_col <- col_candidate
                break
              }
            }

            # Check if we should update the mapping
            should_update <- FALSE

            if (is.null(matched_col)) {
              # No match found, skip this role
              next
            } else if (var_role %in% names(local_variable_map)) {
              # Role is already mapped - check if new match is more preferred
              existing_col <- local_variable_map[[var_role]]

              if (is.null(existing_col) || !(existing_col %in% data_cols)) {
                # Existing mapping is invalid, update with new match
                should_update <- TRUE
              } else if (matched_col != existing_col) {
                # Check if matched_col is more preferred (appears earlier in possible_cols)
                existing_idx <- match(existing_col, possible_cols)
                matched_idx <- match(matched_col, possible_cols)

                if (
                  !is.na(matched_idx) &&
                    !is.na(existing_idx) &&
                    matched_idx < existing_idx
                ) {
                  # New match is more preferred
                  should_update <- TRUE
                }
              }
              # If matched_col == existing_col, no update needed (already correct)
            } else {
              # Role not yet mapped, add new mapping
              should_update <- TRUE
            }

            # Update variable_map if needed
            if (should_update) {
              local_variable_map[[var_role]] <- matched_col
              vars_mapped <- vars_mapped + 1

              # Now check for value mapping (only for non-numeric types)
              var_type <- types[[var_role]]
              is_numeric_type <- !is.null(var_type) &&
                var_type %in% c("numeric", "integer", "double")

              if (!is_numeric_type) {
                data_values <- unique(df[[matched_col]])
                data_values <- data_values[!is.na(data_values)]

                # Check if this is a select_multiple question type
                question_types <- sch$question_types %||% list()
                is_select_multiple <- !is.null(question_types[[var_role]]) &&
                  question_types[[var_role]] == "select_multiple"

                # Check if schema has value_map for this variable
                if (var_role %in% names(schema_value_map)) {
                  # New nested value_map structure
                  canonical_mappings <- schema_value_map[[var_role]]

                  if (is_select_multiple) {
                    # SPECIAL HANDLING FOR SELECT_MULTIPLE
                    # Values are space-separated and unordered in the same cell
                    # Extract all unique tokens from all data values
                    all_tokens <- extract_tokens(data_values)

                    # For each canonical value, use grepl to search for allowed values in tokens
                    matched_canonical <- list()
                    for (canonical_val in names(canonical_mappings)) {
                      allowed_for_canonical <- canonical_mappings[[
                        canonical_val
                      ]]

                      # Find which allowed values appear in the tokens
                      # Use vectorized approach for efficiency
                      matches <- sapply(
                        allowed_for_canonical,
                        function(allowed_val) {
                          escaped_val <- escape_regex(allowed_val)
                          any(grepl(
                            paste0("\\b", escaped_val, "\\b"),
                            all_tokens
                          ))
                        }
                      )
                      found_values <- allowed_for_canonical[matches]

                      if (length(found_values) > 0) {
                        matched_canonical[[canonical_val]] <- found_values
                      }
                    }

                    if (length(matched_canonical) > 0) {
                      local_value_map[[var_role]] <- matched_canonical
                      vals_mapped <- vals_mapped + 1
                    }
                  } else {
                    # STANDARD HANDLING FOR SELECT_ONE AND OTHER TYPES
                    # Pre-compute data values for efficient matching
                    data_values_set <- data_values

                    # For each canonical value, check which dataset values exist
                    matched_canonical <- list()
                    for (canonical_val in names(canonical_mappings)) {
                      allowed_for_canonical <- canonical_mappings[[
                        canonical_val
                      ]]

                      # Find which of these allowed values exist in the data
                      found_values <- allowed_for_canonical[
                        allowed_for_canonical %in% data_values_set
                      ]

                      if (length(found_values) > 0) {
                        matched_canonical[[canonical_val]] <- found_values
                      }
                    }

                    if (length(matched_canonical) > 0) {
                      local_value_map[[var_role]] <- matched_canonical
                      vals_mapped <- vals_mapped + 1
                    }
                  }
                } else if (var_role %in% names(allowed_vals)) {
                  # Fallback: old allowed_values structure (for backward compatibility)
                  schema_allowed <- allowed_vals[[var_role]]

                  if (is_select_multiple) {
                    # SPECIAL HANDLING FOR SELECT_MULTIPLE with allowed_values
                    # Extract all unique tokens using helper
                    all_tokens <- extract_tokens(data_values)

                    # Find which allowed values appear in the tokens
                    # Use vectorized approach for efficiency
                    matches <- sapply(schema_allowed, function(allowed_val) {
                      escaped_val <- escape_regex(allowed_val)
                      any(grepl(paste0("\\b", escaped_val, "\\b"), all_tokens))
                    })
                    found_values <- schema_allowed[matches]

                    if (length(found_values) > 0) {
                      # Store as flat list for backward compatibility
                      local_value_map[[var_role]] <- found_values
                      vals_mapped <- vals_mapped + 1
                    }
                  } else {
                    # STANDARD HANDLING FOR SELECT_ONE
                    # Find which allowed values exist in the data
                    found_values <- intersect(schema_allowed, data_values)

                    if (length(found_values) > 0) {
                      # Store as flat list for backward compatibility
                      local_value_map[[var_role]] <- found_values
                      vals_mapped <- vals_mapped + 1
                    }
                  }
                }
              }
            }
          }

          # OPTIMIZATION (#4): Write the accumulated maps back to the
          # active bindings exactly once, instead of once per mapped
          # variable/value inside the loop above.
          self$.variable_map <- local_variable_map
          self$.value_map <- local_value_map

          if (vars_mapped > 0 || vals_mapped > 0) {
            phrutils::phr_message(
              phrutils::phr_txt(
                "Auto-mapped {vars_mapped} variable(s) and {vals_mapped} value set(s) for {self$dataset_name}."
              )
            )
          }

          invisible(self)
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "$map_schema_vars")
      )
    },
    #' Map Schema Labels to variable_label and value_label
    #'
    #' @description
    #' Reads the current `variable_map` and `value_map`, then populates the
    #' `variable_label` and `value_label` fields from the labels stored in
    #' the variable schema for the requested language. Only variables and
    #' values that are currently in `variable_map` / `value_map` are labelled.
    #'
    #' This method is called automatically after every `map_schema_vars()` call.
    #' It can also be called manually to switch languages.
    #'
    #' @param language Character string: one of `"english"` (default), `"french"`, or `"arabic"`.
    #'
    #' @return Invisible self (for method chaining)
    #'
    #' @details
    #' Labels are sourced from the variable schema fields:
    #' * `variable_labels$en / $fr / $ar` -- one label per variable role
    #' * `value_labels$en / $fr / $ar`    -- one label per canonical value per role
    #'
    #' These fields are populated when the schema is loaded from an xlsx template
    #' that contains the `variable_label_en`, `variable_label_fr`,
    #' `variable_label_ar`, `value_label_en`, `value_label_fr`, and
    #' `value_label_ar` columns.
    ..map_schema_labels = function(language = "english") {
      phrutils::phr_try(
        {
          sch <- private$..variable_schema

          if (is.null(sch) || length(sch) == 0) {
            return(invisible(self))
          }

          # Resolve the two-letter language code
          lang <- switch(
            tolower(trimws(language)),
            "english" = "en",
            "french" = "fr",
            "arabic" = "ar",
            {
              phrutils::phr_warning(
                self$dataset_name,
                phrutils::phr_txt(
                  "Unknown language '{language}' in map_schema_labels(); defaulting to 'english'."
                )
              )
              "en"
            }
          )

          schema_var_labels <- (sch$variable_labels %||% list())[[lang]] %||%
            list()
          schema_val_labels <- (sch$value_labels %||% list())[[lang]] %||%
            list()

          vars_labelled <- 0
          vals_labelled <- 0

          # OPTIMIZATION (#5): As in `..map_schema_vars()`, avoid repeated
          # full-list read/write-back through the `.variable_label` /
          # `.value_label` active bindings inside the loops below by
          # accumulating into local lists and writing back once.
          local_variable_label <- self$.variable_label
          local_value_label <- self$.value_label

          # Populate variable_label for each role present in variable_map
          for (var_role in names(self$.variable_map)) {
            if (!is.null(schema_var_labels[[var_role]])) {
              local_variable_label[[var_role]] <- schema_var_labels[[var_role]]
              vars_labelled <- vars_labelled + 1
            }
          }

          # Populate value_label for each role present in value_map
          for (var_role in names(self$.value_map)) {
            val_label_entry <- schema_val_labels[[var_role]]
            if (!is.null(val_label_entry) && length(val_label_entry) > 0) {
              local_value_label[[var_role]] <- val_label_entry
              vals_labelled <- vals_labelled + 1
            }
          }

          self$.variable_label <- local_variable_label
          self$.value_label <- local_value_label

          if (vars_labelled > 0 || vals_labelled > 0) {
            phrutils::phr_message(
              phrutils::phr_txt(
                "Labelled {vars_labelled} variable(s) and {vals_labelled} value set(s) for {self$dataset_name} ({language})."
              )
            )
          }

          invisible(self)
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "$map_schema_labels")
      )
    }
  ),

  active = list(
    #' @field raw_data Active binding exposing the private `..raw_data`
    #'   field (original imported data frame). Reads return the stored
    #'   data; assignments write through to the private field. Prefer
    #'   the inherited `get()` / `set()` API or `get_data(stage = "raw")`
    #'   in new code.
    raw_data = function(value) {
      if (missing(value)) {
        private$..raw_data
      } else {
        private$..raw_data <- value
        invisible(value)
      }
    },

    #' @field standardized_data Active binding exposing the private
    #'   `..standardized_data` field (data frame after standardization).
    #'   Reads return the stored data; assignments write through to the
    #'   private field. Prefer the inherited `get()` / `set()` API or
    #'   `get_data(stage = "standardized")` in new code.
    standardized_data = function(value) {
      if (missing(value)) {
        private$..standardized_data
      } else {
        private$..standardized_data <- value
        invisible(value)
      }
    },

    #' @field clean_data Active binding exposing the private
    #'   `..clean_data` field (data frame after cleaning). Reads return
    #'   the stored data; assignments write through to the private
    #'   field. Prefer the inherited `get()` / `set()` API or
    #'   `get_data(stage = "clean")` in new code.
    clean_data = function(value) {
      if (missing(value)) {
        private$..clean_data
      } else {
        private$..clean_data <- value
        invisible(value)
      }
    },

    #' @field .variable_map Active binding exposing the private
    #'   `..variable_map` field. Reads return the stored list;
    #'   assignments write through to the private field. Also resolvable
    #'   via the inherited `get(field = ".variable_map")` /
    #'   `set(field = ".variable_map", value = …)` API.
    .variable_map = function(value) {
      if (missing(value)) {
        private$..variable_map
      } else {
        private$..variable_map <- value
        invisible(value)
      }
    },

    #' @field .value_map Active binding exposing the private
    #'   `..value_map` field. Reads return the stored list; assignments
    #'   write through to the private field. Also resolvable via the
    #'   inherited `get(field = ".value_map")` /
    #'   `set(field = ".value_map", value = …)` API.
    .value_map = function(value) {
      if (missing(value)) {
        private$..value_map
      } else {
        private$..value_map <- value
        invisible(value)
      }
    },

    #' @field .variable_label Active binding exposing the private
    #'   `..variable_label` field. Reads return the stored list;
    #'   assignments write through to the private field. Also resolvable
    #'   via the inherited `get(field = ".variable_label")` /
    #'   `set(field = ".variable_label", value = …)` API.
    .variable_label = function(value) {
      if (missing(value)) {
        private$..variable_label
      } else {
        private$..variable_label <- value
        invisible(value)
      }
    },

    #' @field .value_label Active binding exposing the private
    #'   `..value_label` field. Reads return the stored list;
    #'   assignments write through to the private field. Also resolvable
    #'   via the inherited `get(field = ".value_label")` /
    #'   `set(field = ".value_label", value = …)` API.
    .value_label = function(value) {
      if (missing(value)) {
        private$..value_label
      } else {
        private$..value_label <- value
        invisible(value)
      }
    }
  )
)
