#' IPHRA IYCF Data Analytics Class
#'
#' The `NutritionDataAnalytics` R6 class extends `DataAnalytics` to provide
#' Infant and Young Child Feeding specific quality checks in a unified
#' analytics object.
#'
#' @description
#' This class provides Nutrition-specific (including anthropometric) functionality:
#' * Quantitative analysis indicators via analysis_schema
#' * Quality checks via two separate schemas:
#'   - Anthropometric plausibility (`quality_schema_anthro`)
#'   - IYCF plausibility (`quality_schema_iycf`)
#' * All visualizations/tables via outputs_schema
#' * Optional MUAC age-adjusted analysis via \code{post_run_analysis(muac_age_weights = TRUE)}
#'
#' @seealso [DataAnalytics]
#' @export
NutritionDataAnalytics <- R6::R6Class(
  classname = "NutritionDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    #' @description
    #' Initialize a new IYCFDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean IYCF data)
    #' @param dap Optional data analysis plan (tibble)
    #' @param parent_data_object The Data object that generated this
    #' @param dataset_name A name for this analytics assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param variable_label Variable labels from Data object
    #' @param value_label Value labels from Data object
    #' @param quality_schema Optional quality check schema
    #' @return A new IYCFDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "NutritionDataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          quality_schema = NULL) {

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = dataset_name,
        message = phrutils::phr_txt("Started initialization for {dataset_name}.")
      )

      super$initialize(
        data = data,
        dap = dap,
        parent_data_object = parent_data_object,
        dataset_name = dataset_name,
        data_stage_name = data_stage_name,
        data_hash = data_hash,
        variable_map = variable_map,
        value_map = value_map,
        variable_label = variable_label,
        value_label = value_label,
        quality_schema = quality_schema
      )

      # Step 1: Loading IYCF quality schema
      result <- phrutils::phr_try_step(
        {
          private$..load_default_schema(
            filename = private$..default_quality_schema_iycf_filename,
            target_field = "..quality_schema_iycf",
            required_cols = c(
              "check_group",
              "check_name",
              "check_label",
              "variables",
              "statistical_test",
              "threshold_expression",
              "penalty_score",
              "test_params"
            ),
            schema_name = "quality",
            dataset_name = private$..dataset_name
          )
        },
      step = phrutils::phr_txt("Loading iycf quality schema"),
      hint = phrutils::phr_txt("Ensure raw data exists before standardization")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      # Step 2: Create MUAC age-weighted survey design object
      result <- phrutils::phr_try_step(
        {

          self$set(field = "..survey_design_muac",
                   value = private$..build_muac_survey_design(
                   data = private$..data
                  ))

        },
        step = phrutils::phr_txt("Creating Muac age weighted survey design object"),
        hint = phrutils::phr_txt("Ensure raw data exists before standardization")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      # Step 3: Create MUAC data analysis plan for age-weighted analysis
      result <- phrutils::phr_try_step(
        {

          self$set(field = "..survey_design_muac",
                   value = private$..build_muac_survey_design(
                     data = private$..data
                   ))

        },
        step = phrutils::phr_txt("Loading iycf quality schema"),
        hint = phrutils::phr_txt("Ensure raw data exists before standardization")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      # Step 4: Create MUAC age weighted data analysis plans
      result <- phrutils::phr_try_step(
        {
          # Generate per-dataset analysis plans for roster and deaths
          private$..data_analysis_plan_muac <- QuantDataAnalysisPlanLog$new(
            log_df   = NULL,
            log_name = phrutils::phr_txt("Quant Data Analysis Plan (Roster)")
          )

          # Build the DAP for roster dataset from available columns and analysis_schema_roster
          if (!is.null(private$..data)) {

            filtered_schema <- private$..analysis_schema |>
              dplyr::filter(
                !is.na(var_name) &
                  grepl("muac", var_name, ignore.case = TRUE)
              )

            result <- private$..build_dap_from_schema(
              analysis_schema = filtered_schema,
              variable_map    = private$..variable_map %||% list(),
              available_vars  = names(private$..data),
              dataset_label   = "muac"
            )

            private$..data_analysis_plan_muac$set(field = "log_df", value = result$dap_df)

          }
        },
        step = phrutils::phr_txt("creating data analysis plan for muac for alternative age weighting"),
        hint = phrutils::phr_txt("check if nutrition analysis schema contains muac indicators.")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      private$..touch(
        log = TRUE,
        touch = TRUE,
        name = private$..dataset_name,
        message = phrutils::phr_txt("{dataset_name} initialized successfully.")
      )
    },

    #' Export CDC MUAC assessment workbook
    #'
    #' Summarizes MUAC screening results by age group, sex, and nutritional
    #' status, populates the corresponding cells in the CDC MUAC assessment
    #' Excel template, and exports a completed workbook while preserving all
    #' existing worksheets, formulas, formatting, and calculated fields.
    #'
    #' Children are classified into the following categories:
    #' \itemize{
    #'   \item SAM: MUAC < 115 mm and/or oedema present.
    #'   \item MAM: MUAC >= 115 mm and < 125 mm.
    #'   \item Normal: MUAC >= 125 mm.
    #' }
    #'
    #' Results are summarized separately for males and females aged
    #' 6--23 months and 24--59 months and written to the
    #' `"MUAC Analysis"` worksheet of the CDC MUAC template.
    #'
    #' @param data A data frame containing individual-level MUAC screening data.
    #'   Variable names and coded values are resolved using the object's
    #'   variable and value maps.
    #' @param output_file Character. Path to the output Excel workbook to create.
    #'
    #' @return Invisibly returns a named list containing the counts written to the
    #'   CDC MUAC template cells.
    #'
    #' @details
    #' The method loads the CDC MUAC assessment template distributed with the
    #' package, writes summary counts into the designated cells of the
    #' `"MUAC Analysis"` worksheet, and saves the completed workbook to
    #' `output_file`. All other worksheets, formulas, cell formatting, and
    #' workbook structure are preserved.
    #'
    #' If an oedema variable is available, children with oedema are included in
    #' the SAM category regardless of MUAC. If no oedema variable is available,
    #' SAM classification is based solely on MUAC < 115 mm.
    #'
    #' @seealso \code{\link[openxlsx]{loadWorkbook}},
    #'   \code{\link[openxlsx]{saveWorkbook}}
    #'
    #' @export
    export_cdc_muac_file = function(data, output_file) {

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = private$..dataset_name,
        message = phrutils::phr_txt("Starting export for CDC MUAC excel file for {private$..dataset_name}.")
      )

      phrutils::phr_try({

      # Step 1: Load CDC MUAC template
      result <- phrutils::phr_try_step(
        {
          openxlsx::loadWorkbook(
            system.file(
              private$..default_cdc_muac_template_filename,
              package = "phr"
            )
          )
        },
        step = phrutils::phr_txt("Loading CDC MUAC excel template"),
        hint = phrutils::phr_txt("Check if CDC MUAC template file exists in resources.")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      wb <- result

      # Step 2: Get core variable names for age, sex, muac
      result <- phrutils::phr_try_step(
        {
          list(
            age_months_col = private$..variable_map[["age_months"]],
            sex_col = private$..variable_map[["sex"]],
            sex_male_val = private$..value_map$sex[["male"]],
            sex_female_val = private$..value_map$sex[["female"]],
            muac_col = private$..variable_map[["nut_muac_mm"]]
          )
        },
        step = phrutils::phr_txt("Retrieve column names and values from variable and value maps for sex, age_months, nut_muac_mm"),
        hint = phrutils::phr_txt("Check whether sex, age, and muac have been appropriately mapped in variable and value map.")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      } else {
        age_months_col <- result$age_months_col
        sex_col <- result$sex_col
        sex_male_val <- result$sex_male_val
        sex_female_val <- result$sex_female_val
        muac_col <- result$muac_col
      }

      oedema_col <- NULL
      oedema_yes_val <- NULL

      # Step 3: Get supplemental variable names for age, sex, muac
      result <- phrutils::phr_try(
        {
          list(
            oedema_col = private$..variable_map[["nut_edema_confirm"]] %||%
              private$..variable_map[["nut_edema"]],
            oedema_yes_val = private$..value_map$nut_edema_confirm[["yes"]] %||%
              private$..value_map$nut_edema[["yes"]]
          )
        },
        on_error = "warn",
        step = phrutils::phr_txt("Retrieve column names and values from variable and value maps for nut_edema_confirm or nut_edema"),
        hint = phrutils::phr_txt("Check whether nut_edema_confirm or nut_edema.")
      )
      if (!phrutils::phr_failed(result)) {
        oedema_col <- result$oedema_col
        oedema_yes_val <- result$oedema_yes_val
      }

      # Step 4: Sum cell values
      result <- phrutils::phr_try(
        {
          # Age groups
          age_6_23  <- data[[age_months_col]] >= 6  & data[[age_months_col]] <= 23
          age_24_59 <- data[[age_months_col]] >= 24 & data[[age_months_col]] <= 59

          # Sex
          male   <- data[[sex_col]] == sex_male_val
          female <- data[[sex_col]] == sex_female_val

          # Categories
          sam <- data[[muac_col]] < 115

          if (!is.null(oedema_col)) {
            oedema_yes <- !is.na(data[[oedema_col]]) &
              data[[oedema_col]] == oedema_yes_val

            sam <- sam | oedema_yes
          }
          mam <- data[[muac_col]] >= 115 & data[[muac_col]] < 125
          normal <- data[[muac_col]] >= 125

          values <- list(

            # Male 6-23 months
            C5 = sum(male & age_6_23 & sam, na.rm = TRUE),
            C6 = sum(male & age_6_23 & mam, na.rm = TRUE),
            C7 = sum(male & age_6_23 & normal, na.rm = TRUE),

            # Male 24-59 months
            C9  = sum(male & age_24_59 & sam, na.rm = TRUE),
            C10 = sum(male & age_24_59 & mam, na.rm = TRUE),
            C11 = sum(male & age_24_59 & normal, na.rm = TRUE),

            # Female 6-23 months
            E5 = sum(female & age_6_23 & sam, na.rm = TRUE),
            E6 = sum(female & age_6_23 & mam, na.rm = TRUE),
            E7 = sum(female & age_6_23 & normal, na.rm = TRUE),

            # Female 24-59 months
            E9  = sum(female & age_24_59 & sam, na.rm = TRUE),
            E10 = sum(female & age_24_59 & mam, na.rm = TRUE),
            E11 = sum(female & age_24_59 & normal, na.rm = TRUE)
          )

          values
        },
        on_error = "warn",
        step = phrutils::phr_txt("Summing cell values for CDC MUAC template"),
        hint = phrutils::phr_txt("Check for issues in sex, age_months, muac, or edema variables.")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      values <- result

      # Step 5: Set cell values and export excel sheet
      result <- phrutils::phr_try(
        {

          cell_map <- list(
            C5  = c(3, 5),
            C6  = c(3, 6),
            C7  = c(3, 7),
            C9  = c(3, 9),
            C10 = c(3, 10),
            C11 = c(3, 11),
            E5  = c(5, 5),
            E6  = c(5, 6),
            E7  = c(5, 7),
            E9  = c(5, 9),
            E10 = c(5, 10),
            E11 = c(5, 11)
          )

          for (cell in names(cell_map)) {
            openxlsx::writeData(
              wb,
              sheet = "MUAC Analysis",
              x = values[[cell]],
              startCol = cell_map[[cell]][1],
              startRow = cell_map[[cell]][2]
            )
          }

          openxlsx::saveWorkbook(
            wb,
            output_file,
            overwrite = TRUE
          )

        },
        on_error = "warn",
        step = phrutils::phr_txt("Exporting CDC MUAC workbook with new values"),
        hint = phrutils::phr_txt("Check if issues with CDC MUAC template")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      })

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = private$..dataset_name,
        message = phrutils::phr_txt("CDC MUAC excel file for {private$..dataset_name} exported successfully.")
      )

      invisible(values)
    },

    #' Export ENA anthropometry dataset
    #'
    #' Exports a standardized child anthropometry dataset in a structure
    #' compatible with ENA for SMART and related nutrition analysis workflows.
    #' Variables are identified using the object's variable map, reordered,
    #' renamed to the required ENA field names, and optionally exported as
    #' either a CSV or Excel file.
    #'
    #' The exported dataset contains the following variables:
    #' \code{SURVDATE}, \code{CLUSTER}, \code{TEAM}, \code{ID},
    #' \code{HH}, \code{SEX}, \code{BIRTHDAT}, \code{MONTHS},
    #' \code{WEIGHT}, \code{HEIGHT}, \code{EDEMA}, and \code{MUAC}.
    #'
    #' Missing source variables are retained in the output as empty columns.
    #'
    #' If \code{MONTHS} is unavailable but both survey date and date of birth
    #' are available, age in months is calculated automatically and rounded
    #' down to the nearest whole month.
    #'
    #' If MUAC is available only in centimetres
    #' (\code{nut_muac_cm}), values are converted to millimetres prior to
    #' export.
    #'
    #' Records with confirmed age greater than 59 months are excluded from the
    #' exported dataset.
    #'
    #' @param data A data frame containing child anthropometry data.
    #' @param output_file Character. Path to the file to be created.
    #' @param format Character. Export format. One of
    #'   \code{"csv"} or \code{"xlsx"}. Defaults to \code{"csv"}.
    #'
    #' @return Invisibly returns the exported data frame.
    #'
    #' @details
    #' Variable names are resolved from the object's variable map using the
    #' following canonical variables:
    #'
    #' \itemize{
    #'   \item \code{date_survey -> SURVDATE}
    #'   \item \code{cluster_id_numeric} or \code{cluster_id -> CLUSTER}
    #'   \item \code{enum_id -> TEAM}
    #'   \item \code{sex -> SEX}
    #'   \item \code{dob_final -> BIRTHDAT}
    #'   \item \code{age_months -> MONTHS}
    #'   \item \code{nut_weight -> WEIGHT}
    #'   \item \code{nut_height -> HEIGHT}
    #'   \item \code{nut_edema_confirm} or \code{nut_edema -> EDEMA}
    #'   \item \code{nut_muac_mm} or \code{nut_muac_cm -> MUAC}
    #' }
    #'
    #' @seealso \code{\link[utils]{write.csv}},
    #'   \code{\link[openxlsx]{write.xlsx}}
    #'
    #' @export
    export_ena_anthro_file = function(
    data,
    output_file,
    format = "csv"
    ) {

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = private$..dataset_name,
        message = phrutils::phr_txt("CDC MUAC excel file for {private$..dataset_name} exported successfully.")
      )

      phrutils::phr_try({

        private$..touch(
          log = TRUE,
          touch = FALSE,
          name = private$..dataset_name,
          message = phrutils::phr_txt("Starting ENA formatted anthropometry data export for {private$..dataset_name}.")
        )

      format <- match.arg(format)

      get_var <- function(...) {

        candidates <- c(...)

        for (candidate in candidates) {

          col <- private$..variable_map[[candidate]]

          if (!is.null(col) && col %in% names(data)) {
            return(col)
          }
        }

        NULL
      }

      # Step 1: Retrieve variable names from variable map
      result <- phrutils::phr_try_step(
        {
          survdate_col <- get_var("date_survey")
          cluster_col  <- get_var("cluster_id_numeric", "cluster_id")
          team_col     <- get_var("enum_id")
          sex_col      <- get_var("sex")
          birthdat_col <- get_var("dob_final")
          months_col   <- get_var("age_months")
          weight_col   <- get_var("nut_weight")
          height_col   <- get_var("nut_height")
          edema_col    <- get_var("nut_edema_confirm", "nut_edema")

          muac_mm_col <- get_var("nut_muac_mm")
          muac_cm_col <- get_var("nut_muac_cm")

          list(
            survdate_col = survdate_col,
            cluster_col = cluster_col,
            team_col = team_col,
            sex_col = sex_col,
            birthdat_col = birthdat_col,
            months_col = months_col,
            weight_col = weight_col,
            height_col = height_col,
            edema_col = edema_col,
            muac_mm_col = muac_mm_col,
            muac_cm_col = muac_cm_col
          )
        },
        on_error = "warn",
        step = phrutils::phr_txt("Retrieve variable names from variable map"),
        hint = phrutils::phr_txt("Check availability of variables needed for ENA anthro in the dataset.")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      survdate_col <- result$survdate_col
      cluster_col <- result$cluster_col
      team_col <- result$team_col
      sex_col <- result$sex_col
      birthdat_col <- result$birthdat_col
      months_col <- result$months_col
      weight_col <- result$weight_col
      height_col <- result$height_col
      edema_col <- result$edema_col
      muac_mm_col <- result$muac_mm_col
      muac_cm_col <- result$muac_cm_col

      # Step 2: Create outgoing dataframe
      result <- phrutils::phr_try_step(
        {
          n <- nrow(data)

          output <- data.frame(
            SURVDATE = NA,
            CLUSTER  = NA,
            TEAM     = NA,
            ID       = NA,
            HH       = NA,
            SEX      = NA,
            BIRTHDAT = NA,
            MONTHS   = NA,
            WEIGHT   = NA,
            HEIGHT   = NA,
            EDEMA    = NA,
            MUAC     = NA,
            stringsAsFactors = FALSE
          )

          output <- output[rep(1, n), , drop = FALSE]

          if (!is.null(survdate_col)) output$SURVDATE <- data[[survdate_col]]
          if (!is.null(cluster_col))  output$CLUSTER  <- data[[cluster_col]]
          if (!is.null(team_col))     output$TEAM     <- data[[team_col]]
          if (!is.null(sex_col))      output$SEX      <- data[[sex_col]]
          if (!is.null(birthdat_col)) output$BIRTHDAT <- data[[birthdat_col]]
          if (!is.null(months_col))   output$MONTHS   <- data[[months_col]]
          if (!is.null(weight_col))   output$WEIGHT   <- data[[weight_col]]
          if (!is.null(height_col))   output$HEIGHT   <- data[[height_col]]
          if (!is.null(edema_col))    output$EDEMA    <- data[[edema_col]]

          if (!is.null(muac_mm_col)) {

            output$MUAC <- data[[muac_mm_col]]

          } else if (!is.null(muac_cm_col)) {

            output$MUAC <- data[[muac_cm_col]] * 10
          }

          if (all(c("SURVDATE", "BIRTHDAT") %in% names(output))) {

            missing_months <- is.na(output$MONTHS)

            if (any(missing_months)) {

              surv_date <- as.Date(output$SURVDATE[missing_months])
              birth_date <- as.Date(output$BIRTHDAT[missing_months])

              output$MONTHS[missing_months] <- floor(
                as.numeric(surv_date - birth_date) / 30.4375
              )
            }
          }

          output$SURVDATE <- ifelse(
            is.na(output$SURVDATE),
            "",
            format(as.Date(output$SURVDATE), "%d.%m.%Y")
          )

          output$BIRTHDAT <- ifelse(
            is.na(output$BIRTHDAT),
            "",
            format(as.Date(output$BIRTHDAT), "%d.%m.%Y")
          )

          output <- output[
            is.na(output$MONTHS) | output$MONTHS <= 59,
            ,
            drop = FALSE
          ]

          output
        },
        on_error = "warn",
        step = phrutils::phr_txt("Retrieve variable names from variable map"),
        hint = phrutils::phr_txt("Check availability of variables needed for ENA anthro in the dataset.")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      output <- result

      # Step 3: Export the dataframe
      result <- phrutils::phr_try_step(
        {
          if (format == "csv") {

            utils::write.csv(
              output,
              file = output_file,
              row.names = FALSE,
              na = ""
            )

          } else {

            openxlsx::write.xlsx(
              output,
              file = output_file,
              overwrite = TRUE
            )
          }
        },
        on_error = "warn",
        step = phrutils::phr_txt("Retrieve variable names from variable map"),
        hint = phrutils::phr_txt("Check availability of variables needed for ENA anthro in the dataset.")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      })

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = private$..dataset_name,
        message = phrutils::phr_txt("ENA formatted anthropometry data for {private$..dataset_name} exported successfully.")
      )

      invisible(output)

    }

  ),

  private = list(

    # iycf-specific quality schema fields
    ..quality_schema_iycf      = NULL,
    ..data_analysis_plan_muac = NULL,

    # @field ..default_quality_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_quality_schema_filename = c(
      "resources",
      "quality_schema_data_quality_nutrition_template.xlsx"
    ),

    # @field ..default_quality_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_quality_schema_iycf_filename = c(
      "resources",
      "quality_schema_data_quality_iycf_template.xlsx"
    ),

    # @field ..default_outputs_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_outputs_schema_filename = c(
      "resources",
      "outputs_schema_data_analytics_nutrition_template.xlsx"
    ),

    # @field ..default_analysis_schema_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_analysis_schema_filename = c(
      "resources",
      "analysis_schema_quant_data_analysis_nutrition_template.xlsx"
    ),

    # @field ..default_cdc_muac_template_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_cdc_muac_template_filename = c(
      "resources",
      "CDC_MUACAssessment_Template.xlsx"
    ),

    #' @description
    #' Returns subclass-specific fields accessible via `get()`.
    #'
    #' @return Character vector of field names.
    ..additional_get_fields = function() {
      c(
        super$..additional_get_fields(),
        "..quality_schema_iycf",
        "..data_analysis_plan_muac",
      )
    },

    #' Additional Accessible Fields Hook
    #'
    #' @description
    #' Returns subclass-specific fields accessible via `get()`.
    #'
    #' @return Character vector of field names.
    ..additional_call_fields = function() {
      c(
        super$..additional_call_fields(),
        "export_cdc_muac_file",
        "export_ena_anthro_file"
      )
    },

    #' Additional Settable Fields Hook
    #'
    #' @description
    #' Returns subclass-specific fields that may be modified via `set()`.
    #'
    #' @return Character vector of field names.
    ..additional_set_fields = function() {
      c(
        super$..additional_set_fields(),
        "..quality_schema_iycf",
        "..data_analysis_plan_muac"
      )
    },

    #' @description Pre-hook providing the input sets used by \code{run_quality_checks()}.
    #'
    #' The default implementation returns a single set (named \code{main})
    #' pointing at the core \code{data}, \code{quality_schema},
    #' \code{base_survey_design}, \code{survey_design} and \code{variable_map}
    #' fields. Subclasses that hold multiple datasets or quality schemas can
    #' override this method to return one entry per dataset/schema. Each entry
    #' must be a named list whose elements give the names of the public fields
    #' holding the respective inputs.
    #'
    #' When more than one set is returned (or the single set is not named
    #' \code{main}), quality results are stored under the set's role name in
    #' \code{plausibility_results} and penalty tables under
    #' \code{tables[[role]][["plausibility"]]}.
    #'
    #' @return A named list of field sets.
    ..pre_run_quality_checks = function() {
      list(
        anthro = list(
          data = "..data",
          quality_schema = "..quality_schema",
          base_survey_design = "..base_survey_design",
          survey_design = "..survey_design",
          variable_map = "..variable_map"
        ),
        iycf = list(
          data = "data",
          quality_schema = "..quality_schema_iycf",
          base_survey_design = "..base_survey_design",
          survey_design = "..survey_design",
          variable_map = "..variable_map"
        )
      )
    },

    #' @description
    #' Pre-hook providing the input sets used by the inherited \code{run_analysis()}.
    #'
    #' Returns one input set per available mortality dataset:
    #' \describe{
    #'   \item{household}{Always included, using the core \code{data},
    #'     \code{survey_design} and \code{data_analysis_plan} fields. Results
    #'     stored in \code{analysis_results$household}.}
    #'   \item{roster}{Included when \code{linked_ind_roster_data} is available and
    #'     \code{data_analysis_plan_roster} contains at least one indicator. Results
    #'     stored in \code{analysis_results$roster}.}
    #'   \item{deaths}{Included when \code{linked_ind_deaths_data} is available and
    #'     \code{data_analysis_plan_deaths} contains at least one indicator. Results
    #'     stored in \code{analysis_results$deaths}.}
    #' }
    #'
    #' @return A named list of field sets.
    ..pre_run_analysis = function() {
      origin <- paste0(private$..dataset_name, "$pre_run_analysis")

      sets <- super$..pre_run_analysis()

      if (!is.null(private$..data)) {
        if (is.null(private$..data_analysis_plan_muac$get(field = "log_df"))) {
          sets$muac_alt <- list(
            data = "..data",
            survey_design = "..survey_design_muac",
            data_analysis_plan = "..data_analysis_plan_muac"
          )
        } else {
          phrutils::phr_message(origin, phrutils::phr_txt("Data present but data_analysis_plan_muac is empty. Skipping muac alt-weighting analysis."))
        }
      }

      sets
    },

    # Build a temporary survey design using `weight_col` as the weight column.
    # Cluster, strata, and FPC are resolved from self$variable_map as usual.
    # Any variable (including weight_col) that is absent from modified_data
    # is silently omitted so that a simple random-sample design is always
    # producible as a minimum.  Returns NULL on failure.
    ..build_muac_survey_design = function(data, weight_col) {

      origin <- paste0(private$..dataset_name, "$..post_run_analysis$..build_muac_survey_design")

      if (is.null(data)) return(NULL)

      data <- private$..compute_weights_muac_alt(data = data)

      cluster_col <- private$..variable_map[["cluster_id_numeric"]]
      if (is.null(cluster_col) || !cluster_col %in% names(data)) {
        cluster_col <- private$..variable_map[["cluster_id"]]
      }
      if (is.null(cluster_col) || !cluster_col %in% names(data)) {
        cluster_col <- NULL
      }

      strata_col <- private$..variable_map[["stratum"]]
      if (is.null(strata_col) || !strata_col %in% names(data)) strata_col <- NULL

      fpc_col <- private$..variable_map[["fpc"]]
      if (is.null(fpc_col) || !fpc_col %in% names(data)) fpc_col <- NULL

      # weight_col is the composite weight created by post_run_analysis; treat
      # it as optional so the design can always be built at minimum as an SRS.

      weight_col <- private$..variable_map[["weight"]]

      if (is.null(weight_col) || !weight_col %in% names(data)) {

        weight_col <- "weight"
        data[[weight_col]] <- data$alt_weights
        self$set(field = "..variable_map", role = "weight", value = weight_col)

      } else {

        data[[weight_col]] <- data[[weight_col]] * data$alt_weights

      }

      ids_sym    <- if (!is.null(cluster_col)) rlang::sym(cluster_col) else 1
      strata_sym <- if (!is.null(strata_col))  rlang::sym(strata_col)  else NULL
      weight_sym <- if (!is.null(weight_col))  rlang::sym(weight_col)  else NULL
      fpc_sym    <- if (!is.null(fpc_col))     rlang::sym(fpc_col)     else NULL

      design <- phrutils::phr_try(
        srvyr::as_survey_design(
          .data   = data,
          ids     = !!ids_sym,
          strata  = !!strata_sym,
          weights = !!weight_sym,
          fpc     = !!fpc_sym,
          nest    = TRUE
        ),
        on_error = "warn",
        origin   = origin,
        hint     = phrutils::phr_txt("Check that the composite weight column and cluster/strata columns contain valid data.")
      )

      return(design)
    },

    # Compute MUAC age-adjustment weights as a numeric vector (not stored).
    #
    # For children aged 0-23 months:
    #   weights_muac_alt = expected_prop_0_23 / sample_prop_0_23
    # For children aged 24-59 months:
    #   weights_muac_alt = (1 - expected_prop_0_23) / sample_prop_24_59
    # All other rows receive NA.
    #
    # Data are sourced from private$..survey_design$variables to ensure the same
    # data (and weights) used in the survey design are referenced.
    #
    # Returns the weight vector (same length as nrow(survey_design$variables))
    # or NULL if the survey design / age column is absent or there are no
    # eligible children.
    ..compute_weights_muac_alt = function(data, expected_prop_0_23 = 1 / 3) {

      origin <- paste0(private$..dataset_name, "$..compute_weights_muac_alt")

      if (is.null(private$..survey_design)) return(NULL)

      sd_vars <- data

      # Resolve the age-in-months column via variable_map
      age_col <- private$..variable_map[["age_months"]]
      if (is.null(age_col) || !age_col %in% names(sd_vars)) {
        phrutils::phr_message(
          origin,
          "No age_months column found in data; MUAC age-adjustment weights cannot be computed."
        )
        return(NULL)
      }

      age_vec  <- suppressWarnings(as.numeric(sd_vars[[age_col]]))

      in_0_23  <- !is.na(age_vec) & age_vec >= 0  & age_vec < 24
      in_24_59 <- !is.na(age_vec) & age_vec >= 24 & age_vec < 60

      n_0_23  <- sum(in_0_23)
      n_24_59 <- sum(in_24_59)
      n_total <- n_0_23 + n_24_59

      if (n_total == 0) {
        phrutils::phr_message(
          origin,
          "No children aged 0-59 months found; MUAC age-adjustment weights cannot be computed."
        )
        return(NULL)
      }

      sample_prop_0_23  <- n_0_23  / n_total
      sample_prop_24_59 <- n_24_59 / n_total

      expected_prop_24_59 <- 1 - expected_prop_0_23

      alt_weights <- rep(NA_real_, nrow(sd_vars))

      if (sample_prop_0_23 > 0) {
        alt_weights[in_0_23]  <- expected_prop_0_23  / sample_prop_0_23
      }
      if (sample_prop_24_59 > 0) {
        alt_weights[in_24_59] <- expected_prop_24_59 / sample_prop_24_59
      }

      phrutils::phr_message(
        origin,
        glue::glue(
          "MUAC age-adjustment weights computed: {n_0_23} children 0-23 months ",
          "(sample prop: {round(sample_prop_0_23, 3)}, expected: {round(expected_prop_0_23, 3)}), ",
          "{n_24_59} children 24-59 months ",
          "(sample prop: {round(sample_prop_24_59, 3)}, expected: {round(expected_prop_24_59, 3)})."
        )
      )

      data$alt_weights <- alt_weights
      return(data)

    }
  )
)
