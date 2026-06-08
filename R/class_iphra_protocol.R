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
      self$valid_tool_types <- c("household", "key_informant", "observation", "generic")
      self$metadata$assessment_title <- assessment_title
      self$metadata$country_name <- country_name
      self$metadata$month_year <- month_year
      self$metadata$framework_type <- "ana"

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
    },

    #' @description Post-sync hook for IPHRA-specific synchronization.
    #' @param field Optional top-level field name.
    #' @param member Optional nested member name.
    #' @param target_field Optional destination field path.
    #' @param name Optional named list entry inside \code{field}.
    #' @param role Optional role-based list resolution key.
    #' @return Invisibly returns \code{NULL}.
    post_sync_state = function(field = NULL, member = NULL, target_field = NULL,
                               name = NULL, role = NULL) {
      super$post_sync_state(
        field = field, member = member, target_field = target_field,
        name = name, role = role
      )
      st <- self$sample_table
      self$metadata$num_strata_units <- if (!is.null(st) && is.data.frame(st) && "stratum_id" %in% names(st)) {
        length(unique(st$stratum_id))
      } else {
        0L
      }
      private$sync_sampling_conditional_metadata()
      invisible(NULL)
    }
  ),

  active = list(
    `Special Situations` = function(value) FALSE,
    cluster_site_selection = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.sample_has_any_method(c("pps_cluster", "pps_rlc"))
    },
    exhaustive_site_selection = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.sample_has_any_method("proportional")
    },
    general_survey = function(value) FALSE,
    ind_ecfies = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      hh_revised_survey <- tryCatch(
        self$access_nested(field = "tools", role = "household", member = "revised_survey"),
        error = function(e) NULL
      )
      if (is.null(hh_revised_survey) || !is.data.frame(hh_revised_survey) ||
          !"indicator_code" %in% names(hh_revised_survey)) {
        return(FALSE)
      }
      indicator_codes <- trimws(as.character(hh_revised_survey$indicator_code))
      any(!is.na(indicator_codes) & indicator_codes == "10801")
    },
    ind_iycfe = function(value) FALSE,
    ind_measles_vaccination = function(value) FALSE,
    ind_muac_children = function(value) FALSE,
    ind_muac_plw = function(value) FALSE,
    ind_vitamin_a_coverage = function(value) FALSE,
    individual_survey = function(value) FALSE,
    kii_community = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.has_tool_role("kii_community")
    },
    kii_fsl_provider = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.has_tool_role("kii_fsl_service_provider")
    },
    kii_health_provider = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.has_tool_role("kii_health_service_provider")
    },
    kii_nutrition_provider = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.has_tool_role("kii_nutrition_service_provider")
    },
    kii_service_providers = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      isTRUE(self$kii_fsl_provider) ||
        isTRUE(self$kii_health_provider) ||
        isTRUE(self$kii_nutrition_provider) ||
        isTRUE(self$kii_wash_provider)
    },
    kii_wash_provider = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.has_tool_role("kii_wash_service_provider")
    },
    mortality_survey = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.household_has_any_indicator(c("10501", "10502"))
    },
    muac_survey = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.household_has_any_indicator(c("10701", "10702"))
    },
    multiple_methods_no = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      methods <- private$.sample_methods_used()
      length(methods) == 1L
    },
    multiple_methods_yes = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      methods <- private$.sample_methods_used()
      length(methods) > 1L
    },
    multiple_strata = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      st <- private$.sample_table_from_nested()
      is.data.frame(st) && nrow(st) > 1L
    },
    obs_community = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.has_tool_role("obs_community")
    },
    obs_crops_livestock = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.has_tool_role("obs_crop_livestock")
    },
    obs_health_facility = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.has_tool_role("obs_health_facility")
    },
    obs_latrines = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.has_tool_role("obs_latrine")
    },
    obs_service_providers = function(value) FALSE,
    obs_water_point = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.has_tool_role("obs_water_point")
    },
    purposive_site_selection = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      private$.sample_has_any_method("purposive")
    },
    rate_individual_survey = function(value) FALSE,
    rate_survey = function(value) FALSE,
    rlc_household_selection = function(value) {
      if (!missing(value)) return(invisible(FALSE))
      st <- tryCatch(
        self$access_nested(field = "sample_object", member = "get_sample_table"),
        error = function(e) NULL
      )
      if (is.null(st) || !is.data.frame(st) || !"sampling_method" %in% names(st)) {
        return(FALSE)
      }
      methods <- trimws(tolower(as.character(st$sampling_method)))
      any(!is.na(methods) & methods %in% c(
        "pps_rlc", "simple_random_rlc", "systematic_rlc", "proportional_rlc"
      ))
    },
    srs_household_selection = function(value) FALSE,
    srs_site_selection = function(value) FALSE,
    systematic_household_selection = function(value) FALSE,
    systematic_site_selection = function(value) FALSE
  ),

  private = list(

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

    .has_tool_role = function(role) {
      out <- tryCatch(
        self$access_nested(field = "tools", role = role, member = "get_name"),
        error = function(e) NULL
      )
      is.character(out) && length(out) == 1L && nzchar(out)
    },

    .sample_table_from_nested = function() {
      tryCatch(
        self$access_nested(field = "sample_object", member = "get_sample_table"),
        error = function(e) NULL
      )
    },

    .sample_methods_used = function() {
      st <- private$.sample_table_from_nested()
      if (!is.data.frame(st) || !"sampling_method" %in% names(st)) {
        return(character(0))
      }
      methods <- trimws(tolower(as.character(st$sampling_method)))
      methods <- methods[!is.na(methods) & nzchar(methods)]
      unique(methods)
    },

    .sample_has_any_method = function(methods) {
      methods_used <- private$.sample_methods_used()
      length(intersect(methods_used, tolower(as.character(methods)))) > 0L
    },

    .household_has_any_indicator = function(indicator_codes) {
      if (!private$.has_tool_role("household")) return(FALSE)
      hh_codes <- tryCatch(
        self$access_nested(field = "tools", role = "household", member = "get_indicator_codes"),
        error = function(e) character(0)
      )
      hh_codes <- trimws(as.character(hh_codes))
      hh_codes <- hh_codes[!is.na(hh_codes) & nzchar(hh_codes)]
      length(intersect(hh_codes, as.character(indicator_codes))) > 0L
    },

    initialize_conditional_metadata = function() {
      schema_conditions <- private$.condition_keys_from_schema()
      all_keys <- unique(c(private$.sampling_conditional_keys, schema_conditions))
      self$conditional_metadata <- setNames(as.list(rep(FALSE, length(all_keys))), all_keys)
      invisible(NULL)
    },

    .condition_keys_from_schema = function() {
      c(
        "Special Situations",
        "cluster_site_selection",
        "definition_complementary_feeding",
        "definition_gam",
        "definition_gam_women",
        "definition_household",
        "definition_muac",
        "exhaustive_site_selection",
        "general_survey",
        "ind_ecfies",
        "ind_iycfe",
        "ind_measles_vaccination",
        "ind_muac_children",
        "ind_muac_plw",
        "ind_vitamin_a_coverage",
        "individual_survey",
        "kii_community",
        "kii_fsl_provider",
        "kii_health_provider",
        "kii_nutrition_provider",
        "kii_service_providers",
        "kii_wash_provider",
        "mortality_survey",
        "muac_survey",
        "multiple_methods_no",
        "multiple_methods_yes",
        "multiple_strata",
        "obs_community",
        "obs_crops_livestock",
        "obs_health_facility",
        "obs_latrines",
        "obs_service_providers",
        "obs_water_point",
        "purposive_site_selection",
        "rate_individual_survey",
        "rate_survey",
        "rlc_household_selection",
        "srs_household_selection",
        "srs_site_selection",
        "systematic_household_selection",
        "systematic_site_selection"
      )
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

    #' @description Retrieve the framework master objectives schema.
    #' @return A data frame of master objectives; empty data frame when unavailable.
    .get_master_schema = function() {
      schema <- self$access_nested(field = "framework", member = "master_objectives_schema")
      if (is.null(schema) || !is.data.frame(schema)) return(data.frame())
      as.data.frame(schema, stringsAsFactors = FALSE)
    },

    #' @description Collect unique indicator codes from included tools.
    #' @param tool_names Optional character vector of tool names to query.
    #' @param prefer_revised Logical. When TRUE, prefer revised survey codes.
    #' @return Character vector of unique indicator codes.
    .get_tool_indicator_codes = function(tool_names = NULL, prefer_revised = TRUE) {
      selected <- self$get_tool_names()
      if (!is.null(tool_names)) {
        selected <- intersect(selected, as.character(tool_names))
      }
      if (length(selected) == 0L) return(character(0))

      out <- character(0)
      for (tn in selected) {
        tool_codes <- tryCatch(
          self$access_nested(
            field = "tools",
            name = tn,
            member = "get_indicator_codes",
            prefer_revised = prefer_revised
          ),
          error = function(e) character(0)
        )
        out <- c(out, as.character(tool_codes %||% character(0)))
      }
      unique(out[nzchar(out)])
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
