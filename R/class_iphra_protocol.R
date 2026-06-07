#' IPHRAProtocol R6 Class
#'
#' @description
#' Subclass of \code{\link{SurveyProtocol}} for the Integrated Population
#' Health and Risk Assessment (IPHRA) methodology.  On initialisation the
#' class automatically creates an \code{\link{ANAFramework}} and exposes a
#' curated list of approved IPHRA data collection tools.
#'
#' Calling \code{add_tools(tool_name)} with one of the recognised tool names
#' listed below will instantiate the correct \code{\link{Tool}} subclass,
#' load its bundled XLSForm template from the package resources, and register
#' it in \code{self$tools} under the supplied name.
#'
#' Recognised tool names and their types:
#' \describe{
#'   \item{tool_household_iphra_v2}{\code{\link{HouseholdTool}}}
#'   \item{tool_kii_community_iphra_v2}{\code{\link{KeyInformantTool}}}
#'   \item{tool_kii_fsl_service_provider_iphra_v2}{\code{\link{KeyInformantTool}}}
#'   \item{tool_kii_wash_service_provider_iphra_v2}{\code{\link{KeyInformantTool}}}
#'   \item{tool_kii_markets_iphra_v2}{\code{\link{KeyInformantTool}}}
#'   \item{tool_kii_nutrition_service_provider_iphra_v2}{\code{\link{KeyInformantTool}}}
#'   \item{tool_kii_health_service_provider_iphra_v2}{\code{\link{KeyInformantTool}}}
#'   \item{tool_obs_community_iphra_v2}{\code{\link{ObservationTool}}}
#'   \item{tool_obs_crop_livestock_iphra_v1}{\code{\link{ObservationTool}}}
#'   \item{tool_obs_health_facility_iphra_v2}{\code{\link{ObservationTool}}}
#'   \item{tool_obs_latrine_iphra_v2}{\code{\link{ObservationTool}}}
#'   \item{tool_obs_water_point_iphra_v2}{\code{\link{ObservationTool}}}
#' }
#'
#' @importFrom R6 R6Class
#' @export
IPHRAProtocol <- R6::R6Class(
  "IPHRAProtocol",
  inherit = SurveyProtocol,

  public = list(

    #' @description
    #' Creates a new IPHRAProtocol object.
    #'
    #' An \code{\link{ANAFramework}} is automatically created and stored in
    #' \code{self$framework}.
    #'
    #' @param assessment_title Character. Title of the assessment.
    #' @param country_name Character. Country where the assessment takes place.
    #' @param month_year Character. Month and year of data collection
    #'   (e.g. \code{"January 2024"}).
    #' @param version Integer or character. Version number for the TOR document.
    #'   Defaults to \code{1}.
    #' @param type_of_emergency Character. One of \code{"natural_disaster"},
    #'   \code{"conflict"}, or \code{"other"}.
    #' @param type_of_crisis Character. One of \code{"sudden_onset"},
    #'   \code{"slow_onset"}, or \code{"protracted"}.
    #' @param mandating_body Character. Name(s) of the mandating body/agency.
    #' @param project_code Character. IMPACT project code.
    #' @param overall_timeframe Character. Overall research timeframe description.
    #' @param geographic_coverage Character. Description of the geographic area
    #'   covered.
    #' @param general_objective Character. The overall/general objective text.
    #'   Defaults to the standard IPHRA general objective.
    #' @param pilot_date Character or Date. Pilot/training date.
    #' @param data_start_date Character or Date. Data collection start date.
    #' @param data_end_date Character or Date. Data collection end date.
    #' @param analysis_date Character or Date. Data analysis date.
    #' @param data_validation_date Character or Date. Data validation (sent for
    #'   validation) date.
    #' @param prelim_presentation_date Character or Date. Preliminary
    #'   presentation date.
    #' @param output_validation_date Character or Date. Outputs sent for
    #'   validation date.
    #' @param output_published_date Character or Date. Outputs published date.
    #' @param final_presentation_date Character or Date. Final presentation date.
    #' @param humanitarian_milestones Character vector. Any subset of
    #'   \code{"donor"}, \code{"inter_cluster"}, \code{"cluster"},
    #'   \code{"ngo_platform"}, \code{"other"}.
    #' @param audience_type.strategic Logical. Whether the strategic audience
    #'   type applies.  Defaults to \code{FALSE}.
    #' @param audience_type.operational Logical. Whether the operational audience
    #'   type applies.  Defaults to \code{TRUE}.
    #' @param audience_type.programmatic Logical. Whether the programmatic
    #'   audience type applies.  Defaults to \code{TRUE}.
    #' @param audience_type.other Logical. Whether another audience type applies.
    #'   Defaults to \code{FALSE}.
    #' @param dissemination Character vector. Any subset of
    #'   \code{"general_mailing"}, \code{"cluster_mailing"},
    #'   \code{"presentation"}, \code{"website"}, \code{"other"}.
    #' @param recall_period Character. Recall period description (e.g.
    #'   \code{"Past 6 months"}).
    #' @param geographic_description Character. Deprecated alias for
    #'   \code{geographic_coverage}; retained for backwards compatibility.
    #' @param population Character vector. Population groups assessed (legacy).
    #'   Any subset of \code{"idp_camp"}, \code{"idp_informal"},
    #'   \code{"idp_host"}, \code{"idp_other"}, \code{"refugee_camp"},
    #'   \code{"refugee_informal"}, \code{"refugee_host"},
    #'   \code{"refugee_other"}, \code{"host_community"}, \code{"other"}.
    #' @param pop_idpcamp Logical. IDP camp population included.
    #' @param pop_idphost Logical. IDP host community population included.
    #' @param pop_idpinformal Logical. IDP informal settlement population
    #'   included.
    #' @param pop_idpother Logical. Other IDP population included.
    #' @param pop_refugee Logical. Refugee (camp) population included.
    #' @param pop_refugeeinformal Logical. Refugee informal settlement population
    #'   included.
    #' @param pop_refugeehost Logical. Refugee host community population
    #'   included.
    #' @param pop_refugeeother Logical. Other refugee population included.
    #' @param pop_host Logical. Host community population included.
    #' @param pop_other Logical. Other population group included.
    #' @param stakeholder_mapping Logical. Whether stakeholder mapping is
    #'   included.  Defaults to \code{FALSE}.
    #' @param num_geographic_units Numeric. Number of geographic units.
    #'   Defaults to \code{NA}.
    #' @param popsize_known_geographic_unit Logical. Whether population size
    #'   per geographic unit is known.  Defaults to \code{FALSE}.
    #' @param popsize_known_strata_unit Logical. Whether population size per
    #'   strata unit is known.  Defaults to \code{FALSE}.
    #' @param num_kii_health_target Numeric. Target number of health facility
    #'   KII interviews.  Defaults to \code{NA}.
    #' @param num_kii_market_target Numeric. Target number of market KII
    #'   interviews.  Defaults to \code{NA}.
    #' @param num_kii_fsl_target Numeric. Target number of FSL KII interviews.
    #'   Defaults to \code{NA}.
    #' @param num_kii_wash_target Numeric. Target number of WASH KII interviews.
    #'   Defaults to \code{NA}.
    #' @param num_kii_nutrition_target Numeric. Target number of nutrition KII
    #'   interviews.  Defaults to \code{NA}.
    #' @param num_obs_health_target Numeric. Target number of health facility
    #'   observations.  Defaults to \code{NA}.
    #' @param num_obs_latrine_target Numeric. Target number of latrine
    #'   observations.  Defaults to \code{NA}.
    #' @param num_obs_waterpoint_target Numeric. Target number of water-point
    #'   observations.  Defaults to \code{NA}.
    #' @param gender_disaggregation Logical. Whether gender disaggregation is
    #'   planned.  Defaults to \code{TRUE}.
    #' @param sex_disaggregation Logical. Whether sex/age disaggregation is
    #'   planned.  Defaults to \code{TRUE}.
    #' @param data_management_platform Character vector. Any subset of
    #'   \code{"IMPACT"}, \code{"UNHCR"}, \code{"other"}.
    #'   Defaults to \code{"IMPACT"}.
    #' @param expected_output_type Character vector. Any subset of
    #'   \code{"situation_overview"}, \code{"report"}, \code{"profile"},
    #'   \code{"prelim_presentation"}, \code{"final_presentation"},
    #'   \code{"factsheet"}, \code{"dashboard"}, \code{"webmap"},
    #'   \code{"map"}, \code{"other"}.
    #' @param access Character. One of \code{"public"} or
    #'   \code{"restricted"}.
    #' @return A new IPHRAProtocol object.
    initialize = function(assessment_title          = NULL,
                          country_name              = NULL,
                          month_year                = NULL,
                          version                   = 1L,
                          type_of_emergency         = NULL,
                          type_of_crisis            = NULL,
                          mandating_body            = NULL,
                          project_code              = NULL,
                          overall_timeframe         = NULL,
                          geographic_coverage       = NULL,
                          general_objective         = phr_txt("To assess the severity of the public health outcomes and identify initial public health priorities for response to mitigate excess morbidity, malnutrition, and mortality."),
                          pilot_date                = NULL,
                          data_start_date           = NULL,
                          data_end_date             = NULL,
                          analysis_date             = NULL,
                          data_validation_date      = NULL,
                          prelim_presentation_date  = NULL,
                          output_validation_date    = NULL,
                          output_published_date     = NULL,
                          final_presentation_date   = NULL,
                          humanitarian_milestones   = NULL,
                          `audience_type.strategic`    = FALSE,
                          `audience_type.operational`  = TRUE,
                          `audience_type.programmatic` = TRUE,
                          `audience_type.other`        = FALSE,
                          dissemination             = NULL,
                          recall_period             = NULL,
                          geographic_description    = NULL,
                          population                = NULL,
                          pop_idpcamp               = FALSE,
                          pop_idphost               = FALSE,
                          pop_idpinformal           = FALSE,
                          pop_idpother              = FALSE,
                          pop_refugee               = FALSE,
                          pop_refugeeinformal       = FALSE,
                          pop_refugeehost           = FALSE,
                          pop_refugeeother          = FALSE,
                          pop_host                  = FALSE,
                          pop_other                 = FALSE,
                          stakeholder_mapping       = FALSE,
                          num_geographic_units      = NA_real_,
                          popsize_known_geographic_unit = FALSE,
                          popsize_known_strata_unit = FALSE,
                          num_kii_health_target     = NA_real_,
                          num_kii_market_target     = NA_real_,
                          num_kii_fsl_target        = NA_real_,
                          num_kii_wash_target       = NA_real_,
                          num_kii_nutrition_target  = NA_real_,
                          num_obs_health_target     = NA_real_,
                          num_obs_latrine_target    = NA_real_,
                          num_obs_waterpoint_target = NA_real_,
                          gender_disaggregation     = TRUE,
                          sex_disaggregation        = TRUE,
                          data_management_platform  = "IMPACT",
                          expected_output_type      = NULL,
                          access                    = NULL) {
      super$initialize(
        assessment_title = assessment_title,
        country_name     = country_name,
        month_year       = month_year,
        framework_type   = "ana",
        reference_doc_filename = "reach_tor_iphra_template.docx"
      )
      # Store version
      self$metadata$version <- as.integer(version)
      self$valid_tool_types <- c("household", "key_informant", "observation", "generic")

      # Store all optional IPHRA-specific metadata.
      # String category fields default to NA_character_ (comparison with "==" safely
      # yields NA which isTRUE() treats as FALSE — correct "no box checked" default).
      # Free-text fields default to "" so that %||% "" in the rendering code works
      # without extra NA guards.  Character-vector fields default to character(0).
      self$metadata$type_of_emergency        <- type_of_emergency        %||% NA_character_
      self$metadata$type_of_crisis           <- type_of_crisis           %||% NA_character_
      self$metadata$mandating_body           <- mandating_body           %||% ""
      self$metadata$project_code             <- project_code             %||% ""
      self$metadata$overall_timeframe        <- overall_timeframe        %||% ""
      # geographic_coverage supersedes the deprecated geographic_description parameter.
      # Both are set to the same resolved value so that code that reads either field
      # continues to work during the transition period.
      self$metadata$geographic_coverage      <- (geographic_coverage %||% geographic_description) %||% ""
      self$metadata$geographic_description   <- (geographic_coverage %||% geographic_description) %||% ""
      self$metadata$general_objective        <- general_objective
      self$metadata$pilot_date               <- phr_fmt_date_tor(pilot_date)
      self$metadata$data_start_date          <- phr_fmt_date_tor(data_start_date)
      self$metadata$data_end_date            <- phr_fmt_date_tor(data_end_date)
      self$metadata$analysis_date            <- phr_fmt_date_tor(analysis_date)
      self$metadata$data_validation_date     <- phr_fmt_date_tor(data_validation_date)
      self$metadata$prelim_presentation_date <- phr_fmt_date_tor(prelim_presentation_date)
      self$metadata$output_validation_date   <- phr_fmt_date_tor(output_validation_date)
      self$metadata$output_published_date    <- phr_fmt_date_tor(output_published_date)
      self$metadata$final_presentation_date  <- phr_fmt_date_tor(final_presentation_date)
      self$metadata$humanitarian_milestones  <- humanitarian_milestones  %||% character(0)
      # Audience type as four distinct boolean fields
      self$metadata[["audience_type.strategic"]]    <- isTRUE(`audience_type.strategic`)
      self$metadata[["audience_type.operational"]]  <- isTRUE(`audience_type.operational`)
      self$metadata[["audience_type.programmatic"]] <- isTRUE(`audience_type.programmatic`)
      self$metadata[["audience_type.other"]]        <- isTRUE(`audience_type.other`)
      self$metadata$dissemination            <- dissemination            %||% character(0)
      self$metadata$recall_period            <- recall_period            %||% ""
      self$metadata$population               <- population               %||% character(0)
      # Population boolean fields (underscore naming)
      self$metadata[["pop_idpcamp"]]         <- isTRUE(pop_idpcamp)
      self$metadata[["pop_idphost"]]         <- isTRUE(pop_idphost)
      self$metadata[["pop_idpinformal"]]     <- isTRUE(pop_idpinformal)
      self$metadata[["pop_idpother"]]        <- isTRUE(pop_idpother)
      self$metadata[["pop_refugee"]]         <- isTRUE(pop_refugee)
      self$metadata[["pop_refugeeinformal"]] <- isTRUE(pop_refugeeinformal)
      self$metadata[["pop_refugeehost"]]     <- isTRUE(pop_refugeehost)
      self$metadata[["pop_refugeeother"]]    <- isTRUE(pop_refugeeother)
      self$metadata[["pop_host"]]            <- isTRUE(pop_host)
      self$metadata[["pop_other"]]           <- isTRUE(pop_other)
      # Stakeholder mapping
      self$metadata$stakeholder_mapping      <- isTRUE(stakeholder_mapping)
      # Geographic / strata numeric and boolean fields
      self$metadata$num_geographic_units     <- as.numeric(num_geographic_units)
      self$metadata$popsize_known_geographic_unit <- isTRUE(popsize_known_geographic_unit)
      self$metadata$num_strata_units         <- 0L
      self$metadata$popsize_known_strata_unit <- isTRUE(popsize_known_strata_unit)
      # User-defined numeric KII / observation targets
      self$metadata$num_kii_health_target     <- as.numeric(num_kii_health_target)
      self$metadata$num_kii_market_target     <- as.numeric(num_kii_market_target)
      self$metadata$num_kii_fsl_target        <- as.numeric(num_kii_fsl_target)
      self$metadata$num_kii_wash_target       <- as.numeric(num_kii_wash_target)
      self$metadata$num_kii_nutrition_target  <- as.numeric(num_kii_nutrition_target)
      self$metadata$num_obs_health_target     <- as.numeric(num_obs_health_target)
      self$metadata$num_obs_latrine_target    <- as.numeric(num_obs_latrine_target)
      self$metadata$num_obs_waterpoint_target <- as.numeric(num_obs_waterpoint_target)
      self$metadata$gender_disaggregation    <- isTRUE(gender_disaggregation)
      self$metadata$sex_disaggregation       <- isTRUE(sex_disaggregation)
      self$metadata$data_management_platform <- data_management_platform %||% character(0)
      self$metadata$expected_output_type     <- expected_output_type     %||% character(0)
      self$metadata$access                   <- access                   %||% NA_character_

      # Load protocol schema (tags + defaults) from the bundled resource
      private$.load_protocol_schema()
      private$initialize_conditional_metadata()
      private$sync_sampling_conditional_metadata()

      phr_message(phr_txt("IPHRAProtocol initialized."), origin = "IPHRAProtocol$initialize")
      invisible(self)
    },

    #' @description Add an IPHRA data collection tool by name.
    #'
    #' The \code{tool_name} must be one of the recognised IPHRA tool names
    #' (see class description).  The appropriate \code{\link{Tool}} subclass
    #' is instantiated, its bundled XLSForm template is loaded from the
    #' package resources folder (when available), and the object is stored
    #' in \code{self$tools[[tool_name]]}.
    #'
    #' @param tool_name Character. One of the recognised IPHRA tool names.
    #' @return Invisibly returns \code{self} for method chaining.
    add_tools = function(tool_name) {
      phr_try({
        allowable <- private$.iphra_tools

        phr_assert(
          is.character(tool_name) && length(tool_name) == 1 && nzchar(tool_name),
          message = phr_txt("tool_name must be a non-empty character string."),
          origin  = "IPHRAProtocol$add_tools"
        )
        phr_assert(
          tool_name %in% names(allowable),
          message = phr_txt(
            "'{tool_name}' is not a recognised IPHRA tool. Allowable tools: {paste(names(allowable), collapse=', ')}."
          ),
          origin = "IPHRAProtocol$add_tools"
        )

        tool_spec  <- allowable[[tool_name]]
        tool_class <- tool_spec$class
        xlsx_file  <- tool_spec$file

        # Locate the XLSForm resource file (gracefully handles missing files)
        tool_path <- system.file("resources", xlsx_file, package = "phr")
        if (!nzchar(tool_path) || !file.exists(tool_path)) {
          tool_path <- file.path("resources", xlsx_file)
        }

        tool <- if (identical(tool_class, "HouseholdTool")) {
          if (file.exists(tool_path)) {
            t <- HouseholdTool$new(name = tool_name, survey = NULL, choices = NULL)
            private$.load_tool_from_path(t, tool_path)
            t
          } else {
            phr_warning(
              message = phr_txt("XLSForm file not found for '{tool_name}': {xlsx_file}. Creating empty tool."),
              origin  = "IPHRAProtocol$add_tools"
            )
            HouseholdTool$new(name = tool_name)
          }
        } else if (identical(tool_class, "KeyInformantTool")) {
          if (file.exists(tool_path)) {
            t <- KeyInformantTool$new(name = tool_name, survey = NULL, choices = NULL)
            private$.load_tool_from_path(t, tool_path)
            t
          } else {
            phr_warning(
              message = phr_txt("XLSForm file not found for '{tool_name}': {xlsx_file}. Creating empty tool."),
              origin  = "IPHRAProtocol$add_tools"
            )
            KeyInformantTool$new(name = tool_name)
          }
        } else {
          # ObservationTool
          if (file.exists(tool_path)) {
            t <- ObservationTool$new(name = tool_name, survey = NULL, choices = NULL)
            private$.load_tool_from_path(t, tool_path)
            t
          } else {
            phr_warning(
              message = phr_txt("XLSForm file not found for '{tool_name}': {xlsx_file}. Creating empty tool."),
              origin  = "IPHRAProtocol$add_tools"
            )
            ObservationTool$new(name = tool_name)
          }
        }

        if (is.null(self$tools)) self$tools <- list()
        self$tools[[tool_name]] <- tool
        private$.touch()
        phr_message(
          phr_txt("IPHRA tool '{tool_name}' added."),
          origin = "IPHRAProtocol$add_tools"
        )
      }, on_error = "abort", origin = "IPHRAProtocol$add_tools")
      invisible(self)
    },

    #' @description Return the names of all allowable IPHRA tools.
    #' @return Character vector of tool names.
    get_allowable_tools = function() {
      names(private$.iphra_tools)
    },

    #' @description Override \code{add_stratum()} to keep \code{num_strata_units}
    #'   in sync with the sample table.
    #'
    #' Delegates all arguments to \code{SurveyProtocol$add_stratum()} via
    #' \code{super$add_stratum(...)}, then recounts the number of unique stratum
    #' values in the updated sample table and stores the result in
    #' \code{self$metadata$num_strata_units}.
    #'
    #' @param ... Arguments forwarded to \code{SurveyProtocol$add_stratum()}.
    #' @return Invisibly returns \code{self} for method chaining.
    add_stratum = function(...) {
      super$add_stratum(...)
      private$.sync_state()
      private$.touch()
      invisible(self)
    },

    #' @description Override inherited \code{SurveyProtocol$calculate_sample_sizes()}
    #'   to keep sampling conditional metadata synchronized after sample table updates.
    #' @return Invisibly returns \code{self} for method chaining.
    calculate_sample_sizes = function() {
      super$calculate_sample_sizes()
      private$.sync_state()
      private$.touch()
      invisible(self)
    },

    #' @description Update the recall date in the household tool's calculate rows.
    #'
    #' Updates the \code{calculation} column of the \code{recall_event},
    #' \code{recall_date}, and \code{recall_month} rows in the
    #' \code{tool_household_iphra_v2} tool (both \code{survey} and
    #' \code{revised_survey}).  The recall date controls the mortality recall
    #' period used in the form.
    #'
    #' @param recall_date Character or Date. The recall start date.  Accepts a
    #'   \code{Date} object or a character string in \code{"YYYY-MM-DD"} format.
    #' @param tool_name Character. Name of the household tool to update.
    #'   Defaults to \code{"tool_household_iphra_v2"}.
    #' @return Invisibly returns \code{self} for method chaining.
    update_recall_date = function(recall_date,
                                  tool_name = "tool_household_iphra_v2") {
      phr_try({
        phr_assert(
          !is.null(recall_date),
          message = phr_txt("recall_date must not be NULL."),
          origin  = "IPHRAProtocol$update_recall_date"
        )

        # Normalise to Date then format
        date_obj <- tryCatch(
          as.Date(recall_date),
          error = function(e) {
            phr_error(
              message = phr_txt("recall_date could not be coerced to a Date: {conditionMessage(e)}"),
              origin  = "IPHRAProtocol$update_recall_date"
            )
          }
        )

        date_str       <- format(date_obj, "%Y-%m-%d")
        month_first    <- format(date_obj, "%Y-%m-01")
        recall_event_str <- format(date_obj, "%d %B %Y")

        phr_assert(
          !is.null(self$tools) && tool_name %in% names(self$tools),
          message = phr_txt("Tool '{tool_name}' not found. Add it first with add_tools()."),
          origin  = "IPHRAProtocol$update_recall_date"
        )

        tool <- self$tools[[tool_name]]

        .update_recall_in_survey <- function(sv) {
          if (is.null(sv) || !"name" %in% names(sv) || !"calculation" %in% names(sv)) {
            return(sv)
          }
          idx_event <- which(sv$name == "recall_event")
          idx_date  <- which(sv$name == "recall_date")
          idx_month <- which(sv$name == "recall_month")
          if (length(idx_event) > 0) {
            sv$calculation[idx_event] <- paste0("if(1=1, '", recall_event_str, "','')")
          }
          if (length(idx_date) > 0) {
            sv$calculation[idx_date]  <- paste0("if(1=1, date('", date_str,     "'),'')")
          }
          if (length(idx_month) > 0) {
            sv$calculation[idx_month] <- paste0("if(1=1, date('", month_first,  "'),'')")
          }
          sv
        }

        tool$survey         <- .update_recall_in_survey(tool$survey)
        tool$revised_survey <- .update_recall_in_survey(tool$revised_survey)

        private$.touch()
        phr_message(
          phr_txt("Recall date updated to '{date_str}' in tool '{tool_name}'."),
          origin = "IPHRAProtocol$update_recall_date"
        )
      }, on_error = "abort", origin = "IPHRAProtocol$update_recall_date")
      invisible(self)
    },

    #' @description Generate an IPHRA document report.
    #' @param output_file Character output \code{.docx} path.
    #' @param open Logical indicating whether to open the output path.
    #' @return Invisibly returns \code{self}.
    generate_doc = function(output_file = "protocol_report.docx", open = FALSE) {
      super$generate_doc(output_file = output_file, open = open)
    }
  ),

  active = list(
    sync_iphra_state = function(value) {
      if (!missing(value)) {
        stop("sync_iphra_state is a read-only active binding.")
      }
      st <- private$.sync_state(field = "sample_table", member = "get_sample_table")
      self$metadata$num_strata_units <- if (!is.null(st) && is.data.frame(st) && "stratum_id" %in% names(st)) {
        length(unique(st$stratum_id))
      } else {
        0L
      }
      private$sync_sampling_conditional_metadata()
      invisible(NULL)
    }
  ),

  private = list(

    # Row labels in the sample-size tables that represent total/summary values.
    # These rows receive a light-grey background to visually distinguish them.
    .sample_size_total_labels = c(
      "Households to be Included",
      "Individuals to be Included",
      "Population to be Included",
      "Person-Time to be Included"
    ),

    # Named list: tool_name -> list(class = <class_name>, file = <xlsx_filename>)
    .iphra_tools = list(
      tool_household_iphra_v2 = list(
        class = "HouseholdTool",
        file  = "tool_household_iphra_v2.xlsx"
      ),
      tool_kii_community_iphra_v2 = list(
        class = "KeyInformantTool",
        file  = "tool_kii_community_iphra_v2.xlsx"
      ),
      tool_kii_fsl_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file  = "tool_kii_fsl_service_provider_iphra_v2.xlsx"
      ),
      tool_kii_wash_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file  = "tool_kii_wash_service_provider_iphra_v2.xlsx"
      ),
      tool_kii_markets_iphra_v2 = list(
        class = "KeyInformantTool",
        file  = "tool_kii_markets_iphra_v2.xlsx"
      ),
      tool_kii_nutrition_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file  = "tool_kii_nutrition_service_provider_iphra_v2.xlsx"
      ),
      tool_kii_health_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file  = "tool_kii_health_service_provider_iphra_v2.xlsx"
      ),
      tool_obs_community_iphra_v2 = list(
        class = "ObservationTool",
        file  = "tool_obs_community_iphra_v2.xlsx"
      ),
      tool_obs_crop_livestock_iphra_v1 = list(
        class = "ObservationTool",
        file  = "tool_obs_crop_livestock_iphra_v1.xlsx"
      ),
      tool_obs_health_facility_iphra_v2 = list(
        class = "ObservationTool",
        file  = "tool_obs_health_facility_iphra_v2.xlsx"
      ),
      tool_obs_latrine_iphra_v2 = list(
        class = "ObservationTool",
        file  = "tool_obs_latrine_iphra_v2.xlsx"
      ),
      tool_obs_water_point_iphra_v2 = list(
        class = "ObservationTool",
        file  = "tool_obs_water_point_iphra_v2.xlsx"
      )
    ),

    .sampling_conditional_keys = c(
      "srs_srs",
      "srs_systematic",
      "srs_rlc",
      "systematic",
      "systematic_rlc",
      "proportional_rlc",
      "cluster_rlc",
      "cluster",
      "proportional",
      "purposive"
    ),

    # ── TOR generation private methods ─────────────────────────────────────

    .default_template_filenames = function() {
      c("reach_tor_iphra_template.docx", "reach_tor_template.docx", "protocol_report_template.docx")
    },

    .schema_metadata_key = function(tag) {
      key <- sub("^@", "", as.character(tag))
      aliases <- list(
        country = "country_name",
        audience_strategic = "audience_type.strategic",
        audience_operational = "audience_type.operational",
        audience_programmatic = "audience_type.programmatic",
        audience_other = "audience_type.other"
      )
      aliases[[key]] %||% key
    },

    .schema_metadata_value = function(tag) {
      key <- private$.schema_metadata_key(tag)
      if (key %in% names(self$metadata)) self$metadata[[key]] else NULL
    },

    initialize_conditional_metadata = function() {
      schema_conditions <- character(0)
      if (!is.null(self$protocol_schema) &&
          is.data.frame(self$protocol_schema) &&
          "condition" %in% names(self$protocol_schema)) {
        schema_conditions <- trimws(as.character(self$protocol_schema$condition %||% ""))
        schema_conditions <- schema_conditions[nzchar(schema_conditions)]
      }
      all_keys <- unique(c(private$.sampling_conditional_keys, schema_conditions))
      self$conditional_metadata <- setNames(as.list(rep(FALSE, length(all_keys))), all_keys)
      invisible(NULL)
    },

    sync_sampling_conditional_metadata = function() {
      if (!is.list(self$conditional_metadata) || length(self$conditional_metadata) == 0L) {
        private$initialize_conditional_metadata()
      }

      methods_used <- character(0)
      st <- self$get_sample_table()
      if (!is.null(st) &&
          is.data.frame(st) &&
          "sampling_method" %in% names(st)) {
        methods_used <- trimws(tolower(as.character(st$sampling_method %||% character(0))))
        methods_used <- methods_used[!is.na(methods_used) & nzchar(methods_used)]
      }

      has_method <- function(x) any(methods_used %in% x)
      sampling_flags <- list(
        srs_srs          = has_method("simple_random"),
        srs_systematic   = has_method("systematic"),
        srs_rlc          = has_method("simple_random_rlc"),
        systematic       = has_method("systematic"),
        systematic_rlc   = has_method("systematic_rlc"),
        proportional_rlc = has_method("proportional_rlc"),
        cluster_rlc      = has_method("pps_rlc"),
        cluster          = has_method("pps_cluster"),
        proportional     = has_method("proportional"),
        purposive        = has_method("purposive")
      )

      for (nm in names(sampling_flags)) {
        self$conditional_metadata[[nm]] <- isTRUE(sampling_flags[[nm]])
      }
      invisible(NULL)
    },

    .schema_flag_from_tag = function(tag) {
      key <- sub("^@", "", as.character(tag))
      if (grepl("^crisis_", key)) {
        if (key %in% c("crisis_natural_disaster", "crisis_conflict", "crisis_other")) {
          return(isTRUE(self$metadata$type_of_emergency == sub("^crisis_", "", key)))
        }
        if (key %in% c("crisis_sudden_onset", "crisis_sudden")) {
          return(isTRUE(self$metadata$type_of_crisis == "sudden_onset"))
        }
        if (key %in% c("crisis_slow_onset", "crisis_slow")) {
          return(isTRUE(self$metadata$type_of_crisis == "slow_onset"))
        }
        if (key == "crisis_protracted") {
          return(isTRUE(self$metadata$type_of_crisis == "protracted"))
        }
      }
      if (grepl("^milestone_", key)) {
        ms <- self$metadata$humanitarian_milestones %||% character(0)
        needle <- if (key == "milestone_intercluster") "inter_cluster" else sub("^milestone_", "", key)
        return(needle %in% ms)
      }
      if (grepl("^platform_", key)) {
        plat <- self$metadata$data_management_platform %||% character(0)
        needle <- if (key == "platform_unhcr") "UNHCR" else if (key == "platform_impact") "IMPACT" else "other"
        return(needle %in% plat)
      }
      if (grepl("^output_", key)) {
        ot <- self$metadata$expected_output_type %||% character(0)
        return(sub("^output_", "", key) %in% ot)
      }
      if (key %in% c("gender_disagg_yes", "gender_disagg_no")) {
        return(if (key == "gender_disagg_yes") isTRUE(self$metadata$gender_disaggregation) else !isTRUE(self$metadata$gender_disaggregation))
      }
      if (key %in% c("sex_disagg_yes", "sex_disagg_no")) {
        return(if (key == "sex_disagg_yes") isTRUE(self$metadata$sex_disaggregation) else !isTRUE(self$metadata$sex_disaggregation))
      }
      if (key %in% c("access_public", "access_restricted")) {
        return(if (key == "access_public") isTRUE(self$metadata$access == "public") else isTRUE(self$metadata$access == "restricted"))
      }
      if (key %in% c("stakeholder_mapping_yes", "stakeholder_mapping_no")) {
        return(if (key == "stakeholder_mapping_yes") isTRUE(self$metadata$stakeholder_mapping) else !isTRUE(self$metadata$stakeholder_mapping))
      }
      if (grepl("^sampling_", key)) {
        methods_used <- self$get_sampling_methods()
        if (key == "sampling_srs") return(any(methods_used %in% c("simple_random", "simple_random_rlc")))
        if (key == "sampling_systematic") return(any(methods_used %in% c("systematic", "systematic_rlc")))
        if (key == "sampling_cluster") return(any(methods_used %in% c("pps_cluster", "pps_rlc")))
        if (key == "sampling_exhaustive") return(any(methods_used %in% c("proportional", "proportional_rlc")))
        if (key == "sampling_purposive") return(any(methods_used %in% c("purposive")))
        if (key == "sampling_hh_srs") return(any(methods_used %in% c("simple_random", "systematic")))
        if (key == "sampling_hh_rlc") return(any(methods_used %in% c("pps_rlc", "simple_random_rlc", "systematic_rlc", "proportional_rlc")))
        if (key == "sampling_stratified") return(length(self$get_strata_names()) > 1)
      }
      isTRUE(private$.schema_metadata_value(tag))
    },

    # ── Schema-type dispatch methods ───────────────────────────────────────

    # Handle all schema 'replace' type rows.
    # Rows are sorted by tag length (longest first) to prevent shorter tag names
    # from matching as prefixes inside longer ones.  Static replacement text is
    # taken from the 'condition' column when present, otherwise 'default_value'.
    # @version_number uses the metadata version field.
    # @definition_* tags are only replaced when the relevant indicator codes are
    # present in the included tools (preserving the conditional behaviour from
    # the previous add_definition_tags method).
    handle_replace = function(doc, rows) {
      # Definition tags require specific indicator codes to be present
      def_tag_codes <- list(
        "@definition_cdr"                    = c("10501", "10502"),
        "@definition_u5dr"                   = c("10501", "10502"),
        "@definition_gam"                    = c("10701"),
        "@definition_muac"                   = c("10701"),
        "@definition_gam_women"              = c("10702"),
        "@definition_complementary_feeding"  = c("10802")
      )
      inc_codes <- as.character(self$get_indicator_codes_from_tools())

      rows <- rows[order(-nchar(as.character(rows$tag_name %||% ""))), , drop = FALSE]
      for (i in seq_len(nrow(rows))) {
        if (!private$.should_apply_schema_row(rows[i, , drop = FALSE])) next
        tag <- as.character(rows$tag_name[i] %||% "")
        if (!nzchar(tag)) next

        # Definition tags: skip when required indicators are absent
        if (tag %in% names(def_tag_codes)) {
          if (!any(def_tag_codes[[tag]] %in% inc_codes)) next
        }

        # @version_number uses the metadata version field
        val <- if (tag == "@version_number") {
          paste0("v", self$metadata$version %||% 1L)
        } else {
          dv   <- as.character(rows$default_value[i] %||% "")
          dv
        }

        doc <- private$.replace(doc, tag, val)
      }
      doc
    },

    # Handle all schema 'checkbox_replace' type rows.
    # Rows are sorted by tag length (longest first) so that longer tag names
    # (e.g. @pop_refugeehost) are processed before shorter prefix matches
    # (e.g. @pop_refugee), preventing partial replacements.
    handle_checkbox_replace = function(doc, rows) {
      rows <- rows[order(-nchar(as.character(rows$tag_name %||% ""))), , drop = FALSE]
      for (i in seq_len(nrow(rows))) {
        if (!private$.should_apply_schema_row(rows[i, , drop = FALSE])) next
        tag <- as.character(rows$tag_name[i] %||% "")
        if (nzchar(tag))
          doc <- private$.checkbox(doc, tag, private$.schema_flag_from_tag(tag))
      }
      doc
    },

    # Handle all schema 'calculate' type rows plus derived sampling targets.
    # Schema rows are dispatched by tag name; derived targets (@sample_site_target,
    # @sample_hh_target, @num_kii_community_target, @num_obs_community_target)
    # are always computed from the sample table and tool inclusion flags.
    handle_calculate = function(doc, calculate_rows) {
      st <- self$get_sample_table()
      m  <- self$metadata

      # ── Schema-driven calculate tags ────────────────────────────────────
      for (i in seq_len(nrow(calculate_rows))) {
        if (!private$.should_apply_schema_row(calculate_rows[i, , drop = FALSE])) next
        tag <- as.character(calculate_rows$tag_name[i] %||% "")
        doc <- switch(
          tag,
          "@release_date" = private$.replace(doc, tag,
                                             format(Sys.Date(), "%d/%m/%Y")),

          "@num_geographic_units" = private$.replace(doc, tag, {
            v <- m$num_geographic_units
            if (!is.null(v) && !is.na(v)) as.character(v) else ""
          }),

          "@num_strata_units" = private$.replace(doc, tag,
                                                 as.character(m$num_strata_units %||% 0L)),

          "@num_other_units" = private$.replace(doc, tag, ""),

          "@specific_objectives" = {
            # Replace @specific_objectives with pillar-grouped text objectives
            # drawn from tool_objective_catalog_revised, which reflects the
            # actual indicators present in the revised tools.
            tool_cat_so <- self$tool_objective_catalog_revised
            if (length(tool_cat_so) == 0) {
              private$.replace(doc, "@specific_objectives", "")
            } else {
              # Aggregate unique objectives across all tools
              seen_codes_so <- character(0)
              obj_df_so     <- data.frame(
                pillar         = character(0),
                text_objective = character(0),
                stringsAsFactors = FALSE
              )
              for (tn_so in names(tool_cat_so)) {
                for (code_so in names(tool_cat_so[[tn_so]])) {
                  if (code_so %in% seen_codes_so) next
                  obj_so <- tool_cat_so[[tn_so]][[code_so]]
                  txt_so    <- obj_so$text_objective     %||% ""
                  pillar_so <- obj_so$pillar             %||% ""
                  if (nzchar(txt_so)) {
                    seen_codes_so <- c(seen_codes_so, code_so)
                    obj_df_so <- rbind(
                      obj_df_so,
                      data.frame(pillar = pillar_so, text_objective = txt_so,
                                 stringsAsFactors = FALSE)
                    )
                  }
                }
              }
              if (nrow(obj_df_so) == 0) {
                private$.replace(doc, "@specific_objectives", "")
              } else {
                pillars_so <- unique(obj_df_so$pillar[nzchar(obj_df_so$pillar)])
                items_so <- list()
                first_pillar_so <- TRUE
                for (p_so in pillars_so) {
                  sub_objs_so <- unique(obj_df_so$text_objective[obj_df_so$pillar == p_so])
                  sub_objs_so <- sub_objs_so[nzchar(sub_objs_so)]
                  items_so <- c(items_so, list(list(text = p_so, bold = TRUE,
                                                    space_before_pt = if (first_pillar_so) 0L else 6L,
                                                    space_after_pt = 0L, font_size_pt = 10L)))
                  for (obj_so in sub_objs_so) {
                    items_so <- c(items_so, list(list(text = paste0("\u2022 ", obj_so), bold = FALSE,
                                                      space_before_pt = 0L, space_after_pt = 0L, font_size_pt = 10L)))
                  }
                  first_pillar_so <- FALSE
                }
                # Also include objectives with no pillar grouping
                no_pillar_objs <- unique(obj_df_so$text_objective[!nzchar(obj_df_so$pillar)])
                for (obj_so in no_pillar_objs) {
                  items_so <- c(items_so, list(list(text = paste0("\u2022 ", obj_so), bold = FALSE,
                                                    space_before_pt = 0L, space_after_pt = 0L, font_size_pt = 10L)))
                }
                inserted_so <- tryCatch(
                  private$.replace_tag_in_cell(doc, "@specific_objectives", items_so),
                  error = function(e) {
                    phr_warning(phr_txt("Could not insert formatted specific objectives: {conditionMessage(e)}"),
                                origin = "IPHRAProtocol$handle_calculate")
                    FALSE
                  }
                )
                if (!inserted_so) {
                  lines_so <- vapply(items_so, `[[`, character(1L), "text")
                  doc <- private$.replace(doc, "@specific_objectives", paste(lines_so, collapse = "\n"))
                }
                doc
              }
            }
          },

          "@research_questions" = {
            # Replace @research_questions using objective_research_question
            # values from tool_objective_catalog_revised, grouped by pillar.
            tool_cat_rq <- self$tool_objective_catalog_revised
            if (length(tool_cat_rq) == 0) {
              private$.replace(doc, "@research_questions", "")
            } else {
              # Aggregate unique objective_research_questions across all tools
              seen_codes_rq <- character(0)
              orq_df_rq <- data.frame(
                pillar = character(0),
                orq    = character(0),
                stringsAsFactors = FALSE
              )
              for (tn_rq in names(tool_cat_rq)) {
                for (code_rq in names(tool_cat_rq[[tn_rq]])) {
                  if (code_rq %in% seen_codes_rq) next
                  obj_rq <- tool_cat_rq[[tn_rq]][[code_rq]]
                  orq_rq    <- obj_rq$objective_research_question %||% ""
                  pillar_rq <- obj_rq$pillar                      %||% ""
                  if (nzchar(orq_rq)) {
                    seen_codes_rq <- c(seen_codes_rq, code_rq)
                    orq_df_rq <- rbind(
                      orq_df_rq,
                      data.frame(pillar = pillar_rq, orq = orq_rq,
                                 stringsAsFactors = FALSE)
                    )
                  }
                }
              }
              if (nrow(orq_df_rq) == 0) {
                private$.replace(doc, "@research_questions", "")
              } else {
                pillars_rq <- unique(orq_df_rq$pillar[nzchar(orq_df_rq$pillar)])
                items_rq <- list()
                first_pillar_rq <- TRUE
                for (p_rq in pillars_rq) {
                  sub_rqs <- unique(orq_df_rq$orq[orq_df_rq$pillar == p_rq])
                  sub_rqs <- sub_rqs[nzchar(sub_rqs)]
                  items_rq <- c(items_rq, list(list(text = p_rq, bold = TRUE,
                                                    space_before_pt = if (first_pillar_rq) 0L else 6L,
                                                    space_after_pt = 0L, font_size_pt = 10L)))
                  for (rq_item in sub_rqs) {
                    items_rq <- c(items_rq, list(list(text = paste0("\u2022 ", rq_item), bold = FALSE,
                                                      space_before_pt = 0L, space_after_pt = 0L, font_size_pt = 10L)))
                  }
                  first_pillar_rq <- FALSE
                }
                # Also include research questions with no pillar grouping
                no_pillar_rqs <- unique(orq_df_rq$orq[!nzchar(orq_df_rq$pillar)])
                for (rq_item in no_pillar_rqs) {
                  items_rq <- c(items_rq, list(list(text = paste0("\u2022 ", rq_item), bold = FALSE,
                                                    space_before_pt = 0L, space_after_pt = 0L, font_size_pt = 10L)))
                }
                inserted_rq <- tryCatch(
                  private$.replace_tag_in_cell(doc, "@research_questions", items_rq),
                  error = function(e) {
                    phr_warning(phr_txt("Could not insert formatted research questions: {conditionMessage(e)}"),
                                origin = "IPHRAProtocol$handle_calculate")
                    FALSE
                  }
                )
                if (!inserted_rq) {
                  lines_rq <- vapply(items_rq, `[[`, character(1L), "text")
                  doc <- private$.replace(doc, "@research_questions", paste(lines_rq, collapse = "\n"))
                }
                doc
              }
            }
          },

          "@list_secondary_data" = {
            # Replace @list_secondary_data with formatted secondary data sources
            sdr_lsd <- self$secondary_data
            if (is.null(sdr_lsd) || length(sdr_lsd) == 0) {
              doc <- private$.replace(doc, "@list_secondary_data", "")
              doc
            } else {
              master_lsd <- self$get_schema(type = "master")
              get_obj_text_lsd <- function(code_lsd) {
                if (!is.null(master_lsd) && is.data.frame(master_lsd) &&
                    all(c("objective_code", "text_objective") %in% names(master_lsd))) {
                  idx_lsd <- which(as.character(master_lsd$objective_code) == code_lsd)
                  if (length(idx_lsd) > 0L) return(as.character(master_lsd$text_objective[idx_lsd[1L]]))
                }
                code_lsd
              }
              codes_lsd <- names(sdr_lsd)
              unique_codes_lsd <- unique(codes_lsd)
              items_lsd <- list()
              first_obj_lsd <- TRUE
              for (code_lsd in unique_codes_lsd) {
                items_lsd <- c(items_lsd, list(list(text = get_obj_text_lsd(code_lsd), bold = TRUE,
                                                    space_before_pt = if (first_obj_lsd) 0L else 6L,
                                                    space_after_pt = 0L, font_size_pt = NULL)))
                first_obj_lsd <- FALSE
                src_indices_lsd <- which(codes_lsd == code_lsd)
                for (i_lsd in src_indices_lsd) {
                  items_lsd <- c(items_lsd, list(list(text = as.character(sdr_lsd[[i_lsd]]), bold = FALSE,
                                                      space_before_pt = 0L, space_after_pt = 0L, font_size_pt = NULL)))
                }
              }
              inserted_lsd <- tryCatch(
                private$.replace_tag_in_cell(doc, "@list_secondary_data", items_lsd),
                error = function(e) FALSE
              )
              if (!inserted_lsd) {
                body_xml_lsd <- officer::docx_body_xml(doc)
                ns_lsd <- xml2::xml_ns(body_xml_lsd)
                all_paras_lsd <- xml2::xml_find_all(body_xml_lsd, ".//w:p", ns = ns_lsd)
                target_para_lsd <- NULL
                for (p_lsd in all_paras_lsd) {
                  if (grepl("@list_secondary_data", xml2::xml_text(p_lsd), fixed = TRUE)) {
                    target_para_lsd <- p_lsd
                    break
                  }
                }
                if (!is.null(target_para_lsd)) {
                  for (item_lsd in items_lsd) {
                    node_lsd <- private$.make_w_para(
                      text = item_lsd$text, bold = isTRUE(item_lsd$bold),
                      space_before_pt = if (is.null(item_lsd$space_before_pt)) 0L else item_lsd$space_before_pt,
                      space_after_pt  = if (is.null(item_lsd$space_after_pt))  0L else item_lsd$space_after_pt,
                      font_size_pt = item_lsd$font_size_pt
                    )
                    xml2::xml_add_sibling(target_para_lsd, node_lsd, .where = "before")
                  }
                  xml2::xml_remove(target_para_lsd)
                } else {
                  doc <- private$.replace(doc, "@list_secondary_data",
                                          paste(vapply(items_lsd, `[[`, character(1L), "text"), collapse = "\n"))
                }
              }
              doc
            }
          },

          "@precision_gen_indicator" = private$.replace(doc, tag, {
            if (!is.null(st) && all(c("pop_precision", "pop_indicator") %in% names(st))) {
              prec <- suppressWarnings(as.numeric(st$pop_precision))
              idx  <- which(!is.na(prec))
              if (length(idx) > 0L) {
                best   <- which.max(prec[idx]); row_j <- idx[best]
                gen_nm <- as.character(st$pop_indicator[row_j])
                if (!is.na(gen_nm) && nzchar(gen_nm))
                  sprintf("+/- %s%% margin of error for %s", prec[row_j], gen_nm)
                else ""
              } else ""
            } else ""
          }),

          "@precision_ind_indicator" = private$.replace(doc, tag, {
            if (!is.null(st) && all(c("ind_precision", "ind_indicator") %in% names(st))) {
              prec <- suppressWarnings(as.numeric(st$ind_precision))
              idx  <- which(!is.na(prec))
              if (length(idx) > 0L) {
                best   <- which.max(prec[idx]); row_j <- idx[best]
                ind_nm <- as.character(st$ind_indicator[row_j])
                if (!is.na(ind_nm) && nzchar(ind_nm))
                  sprintf("+/- %s%% margin of error for %s", prec[row_j], ind_nm)
                else ""
              } else ""
            } else ""
          }),

          "@precision_mort_indicator" = private$.replace(doc, tag, {
            if (!is.null(st) && all(c("mort_precision", "mort_indicator") %in% names(st))) {
              prec <- suppressWarnings(as.numeric(st$mort_precision))
              idx  <- which(!is.na(prec))
              if (length(idx) > 0L) {
                best    <- which.max(prec[idx]); row_j <- idx[best]
                mort_nm <- as.character(st$mort_indicator[row_j])
                if (!is.na(mort_nm) && nzchar(mort_nm))
                  sprintf("+/- %s%% margin of error for %s", prec[row_j], mort_nm)
                else ""
              } else ""
            } else ""
          }),

          doc  # default: leave any unrecognised calculate tag for cleanup
        )
      }

      # ── Derived sampling targets (always computed) ───────────────────────
      site_target <- if (!is.null(st) && "n_sites" %in% names(st)) {
        s <- sum(suppressWarnings(as.numeric(st$n_sites)), na.rm = TRUE)
        if (s > 0) as.character(round(s)) else "_"
      } else "_"
      hh_target <- if (!is.null(st) && "Final_HH_Sample_Size" %in% names(st)) {
        s <- sum(suppressWarnings(as.numeric(st$Final_HH_Sample_Size)), na.rm = TRUE)
        if (s > 0) as.character(round(s)) else "_"
      } else "_"
      doc <- private$.replace(doc, "@sample_site_target", site_target)
      doc <- private$.replace(doc, "@sample_hh_target",   hh_target)

      total_sites <- if (!is.null(st) && "n_sites" %in% names(st)) {
        s <- sum(suppressWarnings(as.numeric(st$n_sites)), na.rm = TRUE)
        if (s > 0) round(s) else 0L
      } else 0L

      kii_community_txt <- if (self$is_tool_included("tool_kii_community_iphra_v2") &&
                                total_sites > 0L) {
        as.character(total_sites * 3L)
      } else ""
      doc <- private$.replace(doc, "@num_kii_community_target", kii_community_txt)

      obs_community_txt <- if (self$is_tool_included("tool_obs_community_iphra_v2") &&
                                total_sites > 0L) {
        as.character(total_sites)
      } else ""
      doc <- private$.replace(doc, "@num_obs_community_target", obs_community_txt)

      doc
    },

    # Handle all schema 'row_delete' type rows.
    # For each tag, look up the corresponding tool name.  If the tool is
    # included, replace the tag with ""; if not, delete the entire table row
    # (w:tr) that contains the tag.
    handle_row_delete = function(doc, rows) {
      tag_tool_map <- c(
        "@household_tool_inc" = "tool_household_iphra_v2",
        "@kii_community_inc"  = "tool_kii_community_iphra_v2",
        "@kii_health_inc"     = "tool_kii_health_service_provider_iphra_v2",
        "@kii_market_inc"     = "tool_kii_markets_iphra_v2",
        "@kii_fsl_inc"        = "tool_kii_fsl_service_provider_iphra_v2",
        "@kii_wash_inc"       = "tool_kii_wash_service_provider_iphra_v2",
        "@kii_nut_inc"        = "tool_kii_nutrition_service_provider_iphra_v2",
        "@obs_community_inc"  = "tool_obs_community_iphra_v2",
        "@obs_health_inc"     = "tool_obs_health_facility_iphra_v2",
        "@obs_latrine_inc"    = "tool_obs_latrine_iphra_v2",
        "@obs_water_inc"      = "tool_obs_water_point_iphra_v2"
      )
      for (i in seq_len(nrow(rows))) {
        if (!private$.should_apply_schema_row(rows[i, , drop = FALSE])) next
        tag <- as.character(rows$tag_name[i] %||% "")
        if (!nzchar(tag)) next
        if (!tag %in% names(tag_tool_map)) {
          phr_warning(
            paste0("Unrecognized row_delete tag in protocol schema: '", tag, "'."),
            origin = "IPHRAProtocol$handle_row_delete"
          )
          doc <- private$.replace(doc, tag, "")
          next
        }
        included <- self$is_tool_included(tag_tool_map[[tag]])
        if (included) {
          doc <- private$.replace(doc, tag, "")
        } else {
          body_xml <- officer::docx_body_xml(doc)
          ns       <- xml2::xml_ns(body_xml)
          tr_nodes <- xml2::xml_find_all(body_xml, ".//w:tr", ns = ns)
          for (tr in tr_nodes) {
            if (grepl(tag, xml2::xml_text(tr), fixed = TRUE))
              xml2::xml_remove(tr)
          }
        }
      }
      doc
    },

    # Handle all schema 'input' type rows.
    # For each tag, look up the corresponding metadata value and replace.
    handle_input = function(doc, rows) {
      for (i in seq_len(nrow(rows))) {
        if (!private$.should_apply_schema_row(rows[i, , drop = FALSE])) next
        tag <- as.character(rows$tag_name[i] %||% "")
        if (!nzchar(tag)) next
        v   <- private$.schema_metadata_value(tag)
        doc <- private$.replace(doc, tag, as.character(v %||% ""))
      }
      doc
    },

    # Backward-compatible alias: conditional_replace rows are handled through
    # the same replace pathway, with row conditions enforced uniformly.
    handle_conditional_replace = function(doc, rows) {
      private$handle_replace(doc, rows)
    },

    # Replace @primary_data_sources_table with an objective × tool matrix.
    # Rows = distinct objectives included in at least one tool.
    # Columns = tool short labels.
    # Cell = "X" where a tool covers at least one indicator for that objective.
    add_primary_data_sources_table = function(doc) {
      tool_names <- self$get_tool_names()
      if (length(tool_names) == 0) {
        return(private$.replace(doc, "@primary_data_sources_table", ""))
      }

      master <- self$get_schema(type = "master")
      if (!is.data.frame(master) || nrow(master) == 0 ||
          !all(c("objective_code", "indicator_code", "text_objective") %in% names(master))) {
        return(private$.replace(doc, "@primary_data_sources_table", ""))
      }

      # Gather all included indicator codes per tool
      tool_codes <- lapply(tool_names, function(tn) {
        self$get_indicator_codes_from_tools(tool_names = tn)
      })
      names(tool_codes) <- tool_names

      all_codes <- unique(unlist(tool_codes))
      if (length(all_codes) == 0) {
        return(private$.replace(doc, "@primary_data_sources_table", ""))
      }

      # Unique objectives covered by included indicators
      obj_sub <- unique(master[as.character(master$indicator_code) %in% all_codes,
                                c("objective_code", "text_objective"), drop = FALSE])
      obj_sub <- obj_sub[!is.na(obj_sub$objective_code), , drop = FALSE]
      if (nrow(obj_sub) == 0) {
        return(private$.replace(doc, "@primary_data_sources_table", ""))
      }
      obj_sub <- obj_sub[order(obj_sub$objective_code), , drop = FALSE]

      # Build matrix data frame
      # Short tool labels (strip common prefix for readability)
      tool_labels <- gsub("^tool_", "", tool_names)
      tool_labels <- gsub("_iphra_v[0-9]+$", "", tool_labels)

      mat <- as.data.frame(
        matrix("", nrow = nrow(obj_sub), ncol = length(tool_names)),
        stringsAsFactors = FALSE
      )
      names(mat) <- tool_labels

      for (j in seq_along(tool_names)) {
        tc <- tool_codes[[tool_names[j]]]
        obj_covered <- unique(
          as.character(master$objective_code[as.character(master$indicator_code) %in% tc])
        )
        mat[[j]] <- ifelse(obj_sub$objective_code %in% obj_covered, "X", "")
      }

      result_df <- cbind(
        data.frame(Objective = obj_sub$text_objective, stringsAsFactors = FALSE),
        mat
      )

      ft <- flextable::flextable(result_df)
      ft <- flextable::theme_zebra(ft)

      # Page layout: standard portrait page with 1-inch margins ≈ 6.5 in usable
      page_width_in  <- 6.5
      n_tool_cols    <- length(tool_names)
      obj_col_width  <- page_width_in * 0.50
      tool_col_width <- if (n_tool_cols > 0) (page_width_in * 0.50) / n_tool_cols else 0

      ft <- flextable::width(ft, j = 1, width = obj_col_width)
      if (n_tool_cols > 0) {
        ft <- flextable::width(ft, j = seq(2, n_tool_cols + 1), width = tool_col_width)
      }
      ft <- flextable::fontsize(ft, size = 7, part = "all")
      ft <- flextable::set_table_properties(ft, layout = "fixed")

      # Navigate to the @primary_data_sources_table paragraph and replace it
      tryCatch({
        doc <- officer::cursor_reach(doc, keyword = "@primary_data_sources_table")
        doc <- flextable::body_add_flextable(doc, ft, pos = "before")
        doc <- officer::cursor_forward(doc)
        doc <- officer::body_remove(doc)
      }, error = function(e) {
        phr_warning(phr_txt("Could not insert primary data sources table: {conditionMessage(e)}"),
                    origin = "IPHRAProtocol$add_primary_data_sources_table")
        doc <<- private$.replace(doc, "@primary_data_sources_table", "")
      })
      doc
    },

    # Replace @modified_framework_svg with the adjusted SVG rendered as a
    # high-resolution image, and @secondary_data_sources_table with a flextable.
    #
    # Resolution strategy (tried in order, first success wins):
    #   1. magick  — rasterises at 300 DPI (print quality) if available.
    #   2. rsvg    — renders at 3 000 px wide (≈ 3× previous 900 px).
    #   3. Fallback placeholder text.
    #
    # Display dimensions are fixed to the portrait page content width (6.5 in)
    # so that Word never stretches a low-resolution image.
    add_sdr_table = function(doc) {
      # ── Framework SVG ──────────────────────────────────────────────────
      svg_content <- tryCatch(
        self$framework$adjusted_svg %||% self$framework$master_svg,
        error = function(e) NULL
      )

      if (!is.null(svg_content) && nzchar(svg_content)) {
        png_inserted <- FALSE

        tmp_svg <- tempfile(fileext = ".svg")
        tmp_png <- tempfile(fileext = ".png")
        writeLines(svg_content, tmp_svg)

        # Strategy 1: magick — 300 DPI rasterisation (best quality)
        if (!png_inserted && requireNamespace("magick", quietly = TRUE)) {
          tryCatch({
            img <- magick::image_read_svg(tmp_svg, density = 300)
            magick::image_write(img, tmp_png, format = "png")
            tryCatch({
              doc <- officer::cursor_reach(doc, keyword = "@modified_framework_svg")
              doc <- officer::body_add_img(doc, src = tmp_png,
                                           width = 6.5, height = 4.5,
                                           pos = "before")
              doc <- officer::cursor_forward(doc)
              doc <- officer::body_remove(doc)
              png_inserted <- TRUE
            }, error = function(e2) {
              phr_warning(phr_txt("Could not insert framework SVG image (magick): {conditionMessage(e2)}"),
                          origin = "IPHRAProtocol$add_sdr_table")
            })
          }, error = function(e) {
            phr_warning(phr_txt("magick SVG rasterisation failed: {conditionMessage(e)}"),
                        origin = "IPHRAProtocol$add_sdr_table")
          })
        }

        # Strategy 2: rsvg — render at 3 000 px wide (high resolution)
        if (!png_inserted && requireNamespace("rsvg", quietly = TRUE)) {
          tryCatch({
            rsvg::rsvg_png(tmp_svg, tmp_png, width = 3000)
            tryCatch({
              doc <- officer::cursor_reach(doc, keyword = "@modified_framework_svg")
              doc <- officer::body_add_img(doc, src = tmp_png,
                                           width = 6.5, height = 4.5,
                                           pos = "before")
              doc <- officer::cursor_forward(doc)
              doc <- officer::body_remove(doc)
              png_inserted <- TRUE
            }, error = function(e2) {
              phr_warning(phr_txt("Could not insert framework SVG image (rsvg): {conditionMessage(e2)}"),
                          origin = "IPHRAProtocol$add_sdr_table")
            })
          }, error = function(e) {
            phr_warning(phr_txt("rsvg SVG-to-PNG conversion failed: {conditionMessage(e)}"),
                        origin = "IPHRAProtocol$add_sdr_table")
          })
        }

        if (!png_inserted) {
          doc <- private$.replace(doc, "@modified_framework_svg",
                                  "[Framework diagram — attach SVG manually]")
        }
      } else {
        doc <- private$.replace(doc, "@modified_framework_svg", "")
      }

      # ── Secondary data sources table ───────────────────────────────────
      sdr <- self$secondary_data

      # Build the data frame (with data, or one blank row if no data provided)
      if (!is.null(sdr) && length(sdr) > 0) {
        master <- self$get_schema(type = "master")

        obj_labels <- setNames(
          if (!is.null(master) && is.data.frame(master) &&
              all(c("objective_code", "text_objective") %in% names(master))) {
            vapply(names(sdr), function(code) {
              match_idx <- which(as.character(master$objective_code) == code)
              if (length(match_idx) > 0) {
                as.character(master$text_objective[match_idx[1]])
              } else {
                code
              }
            }, character(1))
          } else {
            names(sdr)
          },
          names(sdr)
        )

        sdr_df <- data.frame(
          Objective = unname(obj_labels[names(sdr)]),
          Source    = as.character(unname(sdr)),
          Purpose   = "",
          stringsAsFactors = FALSE
        )
      } else {
        sdr_df <- data.frame(
          Objective = "",
          Source    = "",
          Purpose   = "",
          stringsAsFactors = FALSE
        )
      }

      # Page layout: standard portrait with 1-inch margins ≈ 6.5 in usable
      page_width_in <- 6.5
      col1_width    <- page_width_in * 0.40
      col2_width    <- page_width_in * 0.30
      col3_width    <- page_width_in * 0.30

      ft <- flextable::flextable(sdr_df)
      if (!is.null(sdr) && length(sdr) > 0) {
        ft <- flextable::merge_v(ft, j = "Objective")
      }
      ft <- flextable::theme_zebra(ft)
      ft <- flextable::width(ft, j = 1, width = col1_width)
      ft <- flextable::width(ft, j = 2, width = col2_width)
      ft <- flextable::width(ft, j = 3, width = col3_width)
      ft <- flextable::fontsize(ft, size = 7, part = "all")
      ft <- flextable::set_table_properties(ft, layout = "fixed")

      tryCatch({
        doc <- officer::cursor_reach(doc, keyword = "@secondary_data_sources_table")
        doc <- flextable::body_add_flextable(doc, ft, pos = "before")
        doc <- officer::cursor_forward(doc)
        doc <- officer::body_remove(doc)
      }, error = function(e) {
        phr_warning(phr_txt("Could not insert secondary data table: {conditionMessage(e)}"),
                    origin = "IPHRAProtocol$add_sdr_table")
        doc <<- private$.replace(doc, "@secondary_data_sources_table", "")
      })

      doc
    },

    # ── Sample-size table helpers ──────────────────────────────────────────

    # Build and insert a sample-size flextable for a given tag.
    # 'param_rows' is a list; each element has:
    #   $label     : row label (character)
    #   $col_fn    : function(st_row) → cell value string for one stratum
    # 'n_blank'    : number of blank rows appended after the data rows.
    .build_sample_size_table = function(doc, tag, param_rows) {
      st <- self$get_sample_table()
      if (is.null(st) || nrow(st) == 0L) {
        doc <- private$.replace(doc, tag, "")
        return(doc)
      }

      strata_names <- if ("stratum_name" %in% names(st)) as.character(st$stratum_name)
                      else as.character(st$stratum_id)

      n_strata <- length(strata_names)
      page_width_in  <- 6.5
      strata_col_w   <- page_width_in / (6L + n_strata)
      param_col_w    <- strata_col_w * 3L
      just_col_w     <- strata_col_w * 3L

      # Build data frame: rows = parameters, cols = Parameter + strata + Justification
      col_names <- c("Parameter", strata_names, "Justification")
      rows_list <- lapply(param_rows, function(pr) {
        vals <- vapply(seq_len(n_strata), function(j) {
          tryCatch(pr$col_fn(st[j, , drop = FALSE]), error = function(e) "")
        }, character(1L))
        as.list(c(pr$label, vals, ""))
      })

      mat <- do.call(rbind, lapply(rows_list, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE, col.names = col_names)
      }))
      names(mat) <- col_names

      # Rows whose Parameter label represents a summary/total value get a
      # light-grey background to visually distinguish them from other rows.
      total_row_idx <- which(mat[["Parameter"]] %in% private$.sample_size_total_labels)

      # REACH1 header colour (first colour in the reach1 palette = #EE5859)
      reach1_bg <- tryCatch(
        get_color_palette("reach1")[[1L]],
        error = function(e) {
          phr_warning(phr_txt("Could not load reach1 palette, using fallback colour: {conditionMessage(e)}"),
                      origin = "IPHRAProtocol$.build_sample_size_table")
          "#EE5859"
        }
      )

      fp_col   <- officer::fp_border(color = "black", width = 1)
      fp_outer <- officer::fp_border(color = "black", width = 1.5)

      ft <- flextable::flextable(mat)
      ft <- flextable::fontsize(ft, size = 8, part = "all")
      ft <- flextable::width(ft, j = 1L,                     width = param_col_w)
      ft <- flextable::width(ft, j = seq(2L, n_strata + 1L), width = strata_col_w)
      ft <- flextable::width(ft, j = n_strata + 2L,          width = just_col_w)
      ft <- flextable::set_table_properties(ft, layout = "fixed")

      # Header: bold, REACH1 background, white text
      ft <- flextable::bold(ft, part = "header")
      ft <- flextable::bg(ft, bg = reach1_bg, part = "header")
      ft <- flextable::color(ft, color = "white", part = "header")

      # Body: white background for all rows; light grey for total/summary rows
      ft <- flextable::bg(ft, bg = "white", part = "body")
      if (length(total_row_idx) > 0L) {
        ft <- flextable::bg(ft, i = total_row_idx, bg = "#D3D3D3", part = "body")
      }

      # Borders: start from a clean slate
      ft <- flextable::border_remove(ft)
      # Vertical column separators at 1 pt (body and header)
      ft <- flextable::vline(ft, border = fp_col, part = "body")
      ft <- flextable::vline(ft, border = fp_col, part = "header")
      # Outer border of the whole table at 1.5 pt
      ft <- flextable::border_outer(ft, border = fp_outer, part = "all")
      # Header row bottom border at 1.5 pt (visually separates header from body)
      ft <- flextable::hline_bottom(ft, border = fp_outer, part = "header")

      tryCatch({
        doc <- officer::cursor_reach(doc, keyword = tag)
        doc <- flextable::body_add_flextable(doc, ft, pos = "before")
        doc <- officer::cursor_forward(doc)
        doc <- officer::body_remove(doc)
      }, error = function(e) {
        phr_warning(phr_txt("Could not insert sample-size table for '{tag}': {conditionMessage(e)}"),
                    origin = "IPHRAProtocol$.build_sample_size_table")
        doc <<- private$.replace(doc, tag, "")
      })
      doc
    },

    # Replace @sample_size_hh_gen_table — general (household-level indicator) table.
    add_sample_size_gen_table = function(doc) {
      params <- list(
        list(label = "Indicator Name",
             col_fn = function(r) as.character(r$pop_indicator %||% "")),
        list(label = "Sampling Design",
             col_fn = function(r) phr_fmt_sampling_method(r$sampling_method %||% "")),
        list(label = "Estimated Prevalence (%)",
             col_fn = function(r) phr_fmt_pct(r$pop_expected_prevalence)),
        list(label = "Desired Precision",
             col_fn = function(r) phr_fmt_pct(r$pop_precision)),
        list(label = "Estimated population size",
             col_fn = function(r) phr_fmt_n(r$total_population)),
        list(label = "Design Effect",
             col_fn = function(r) {
               v <- r$pop_design_effect
               if (is.null(v) || is.na(v)) "" else as.character(v)
             }),
        list(label = "Finite Population Correction (FPC) used?",
             col_fn = function(r) phr_fmt_fpc(r$pop_fpc)),
        list(label = "Non-Response Rate",
             col_fn = function(r) phr_fmt_pct(r$pop_nonresponse)),
        list(label = "Households to be Included",
             col_fn = function(r) phr_fmt_n(r$General_HH_Sample_Size))
      )
      private$.build_sample_size_table(doc, "@sample_size_hh_gen_table", params)
    },

    # Replace @sample_size_hh_ind_table — individual-level indicator table.
    # Only inserted when indicator_code 10701 (GAM/MUAC) or 10702 (GAM women)
    # is present in the included tools.
    add_sample_size_ind_table = function(doc) {
      inc_codes <- as.character(self$get_indicator_codes_from_tools())
      if (!any(c("10701", "10702") %in% inc_codes)) {
        doc <- private$.replace(doc, "@sample_size_hh_ind_table", "")
        return(doc)
      }
      params <- list(
        list(label = "Indicator Name",
             col_fn = function(r) as.character(r$ind_indicator %||% "")),
        list(label = "Sampling Design",
             col_fn = function(r) phr_fmt_sampling_method(r$sampling_method %||% "")),
        list(label = "Estimated Prevalence (%)",
             col_fn = function(r) phr_fmt_pct(r$ind_expected_prevalence)),
        list(label = "Desired Precision",
             col_fn = function(r) phr_fmt_pct(r$ind_precision)),
        list(label = "Estimated population size",
             col_fn = function(r) phr_fmt_n(r$total_population)),
        list(label = "Design Effect",
             col_fn = function(r) {
               v <- r$ind_design_effect
               if (is.null(v) || is.na(v)) "" else as.character(v)
             }),
        list(label = "Finite Population Correction (FPC) used?",
             col_fn = function(r) phr_fmt_fpc(r$ind_fpc)),
        list(label = "Individuals to be Included",
             col_fn = function(r) phr_fmt_n(r$Ind_Sample_Size)),
        list(label = "Average Household Size",
             col_fn = function(r) {
               v <- r$ind_avg_hh_size
               if (is.null(v) || is.na(v)) "" else as.character(v)
             }),
        list(label = "% sub-population",
             col_fn = function(r) phr_fmt_pct(r$ind_subpop_prop)),
        list(label = "Non-Response Rate",
             col_fn = function(r) phr_fmt_pct(r$ind_nonresponse)),
        list(label = "Households to be Included",
             col_fn = function(r) phr_fmt_n(r$Ind_HH_Sample_Size))
      )
      private$.build_sample_size_table(doc, "@sample_size_hh_ind_table", params)
    },

    # Replace @sample_size_hh_mort_table — mortality indicator table.
    # Only inserted when indicator_code 10501 (CDR) or 10502 (U5DR)
    # is present in the included tools.
    add_sample_size_mort_table = function(doc) {
      inc_codes <- as.character(self$get_indicator_codes_from_tools())
      if (!any(c("10501", "10502") %in% inc_codes)) {
        doc <- private$.replace(doc, "@sample_size_hh_mort_table", "")
        return(doc)
      }
      params <- list(
        list(label = "Indicator Name",
             col_fn = function(r) as.character(r$mort_indicator %||% "")),
        list(label = "Sampling Design",
             col_fn = function(r) phr_fmt_sampling_method(r$sampling_method %||% "")),
        list(label = "Estimated death rate per 10,000/day",
             col_fn = function(r) {
               v <- r$mort_expected_death_rate
               if (is.null(v) || is.na(v)) "" else as.character(v)
             }),
        list(label = "Desired Precision",
             col_fn = function(r) {
               v <- r$mort_precision
               if (is.null(v) || is.na(v)) "" else as.character(v)
             }),
        list(label = "Recall Period",
             col_fn = function(r) {
               v <- r$mort_recall_days
               if (is.null(v) || is.na(v)) "" else as.character(v)
             }),
        list(label = "Population size (overall)",
             col_fn = function(r) phr_fmt_n(r$total_population)),
        list(label = "Design Effect",
             col_fn = function(r) {
               v <- r$mort_design_effect
               if (is.null(v) || is.na(v)) "" else as.character(v)
             }),
        list(label = "Finite Population Correction (FPC) used?",
             col_fn = function(r) phr_fmt_fpc(r$mort_fpc)),
        list(label = "Population to be Included",
             col_fn = function(r) phr_fmt_n(r$Mort_Ind_Sample_Size, "people")),
        list(label = "Person-Time to be Included",
             col_fn = function(r) phr_fmt_n(r$Mort_PT_Sample_Size, "person days")),
        list(label = "Average Household Size",
             col_fn = function(r) {
               v <- r$mort_avg_hh_size
               if (is.null(v) || is.na(v)) "" else as.character(v)
             }),
        list(label = "% Non-Respondents",
             col_fn = function(r) phr_fmt_pct(r$mort_nonresponse)),
        list(label = "Households to be Included",
             col_fn = function(r) phr_fmt_n(r$Mort_HH_Sample_Size, "households"))
      )
      private$.build_sample_size_table(doc, "@sample_size_hh_mort_table", params)
    },

    # Load survey/choices/settings from an xlsx path into an existing Tool object.
    .load_tool_from_path = function(tool, path) {
      if (!file.exists(path)) return(invisible(NULL))
      available_sheets <- tryCatch(
        readxl::excel_sheets(path),
        error = function(e) character(0)
      )
      if ("survey" %in% available_sheets) {
        sv <- tryCatch(
          as.data.frame(readxl::read_excel(path, sheet = "survey")),
          error = function(e) NULL
        )
        if (!is.null(sv)) {
          tool$survey <- sv
          tool$revised_survey <- sv
        }
      }
      if ("choices" %in% available_sheets) {
        ch <- tryCatch(
          as.data.frame(readxl::read_excel(path, sheet = "choices")),
          error = function(e) NULL
        )
        if (!is.null(ch)) {
          tool$choices <- ch
          tool$revised_choices <- ch
        }
      }
      if ("settings" %in% available_sheets) {
        st <- tryCatch(
          as.data.frame(readxl::read_excel(path, sheet = "settings")),
          error = function(e) NULL
        )
        if (!is.null(st)) {
          tool$settings <- st
          tool$revised_settings <- st
        }
      }
      invisible(NULL)
    },

    # Load protocol_schema_iphra.xlsx into self$protocol_schema.
    # Expected columns are tag_name, handling, condition, default_value, and function_name.
    .load_protocol_schema = function() {
      schema_path <- tryCatch(
        system.file("resources", "protocol_schema_iphra.xlsx", package = "phr"),
        error = function(e) ""
      )
      if (!nzchar(schema_path) || !file.exists(schema_path)) {
        # Try relative path (development / test context)
        schema_path <- file.path("inst", "resources", "protocol_schema_iphra.xlsx")
      }
      if (!file.exists(schema_path)) {
        phr_warning(
          phr_txt("protocol_schema_iphra.xlsx not found; skipping schema load."),
          origin = "IPHRAProtocol$.load_protocol_schema"
        )
        return(invisible(NULL))
      }
      schema <- tryCatch(
        as.data.frame(readxl::read_excel(schema_path, sheet = "schema",
                                         col_types = "text")),
        error = function(e) NULL
      )
      required_cols <- c("tag_name", "handling", "condition", "default_value", "function_name")
      if (is.null(schema) || !all(required_cols %in% names(schema))) {
        return(invisible(NULL))
      }
      schema <- private$.normalize_schema_tags(schema)
      # Store schema for reference
      self$protocol_schema <- schema[required_cols]
      invisible(NULL)
    },

    .normalize_schema_tags = function(schema) {
      if (is.null(schema) || !is.data.frame(schema) || !"tag_name" %in% names(schema)) {
        return(schema)
      }
      schema$tag_name[schema$tag_name == "@kii_nutrition_inc"] <- "@kii_nut_inc"
      schema
    }
  )
)
