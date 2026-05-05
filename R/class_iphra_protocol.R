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
    #' @param pop.idpcamp Logical. IDP camp population included.
    #' @param pop.idphost Logical. IDP host community population included.
    #' @param pop.idpinformal Logical. IDP informal settlement population
    #'   included.
    #' @param pop.idpother Logical. Other IDP population included.
    #' @param pop.refugee Logical. Refugee (camp) population included.
    #' @param pop.refugeeinformal Logical. Refugee informal settlement population
    #'   included.
    #' @param pop.refugeehost Logical. Refugee host community population
    #'   included.
    #' @param pop.refugeeother Logical. Other refugee population included.
    #' @param pop.host Logical. Host community population included.
    #' @param pop.other Logical. Other population group included.
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
                          general_objective         = "To assess the severity of the public health outcomes and identify initial public health priorities for response to mitigate excess morbidity, malnutrition, and mortality.",
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
                          `pop.idpcamp`             = FALSE,
                          `pop.idphost`             = FALSE,
                          `pop.idpinformal`         = FALSE,
                          `pop.idpother`            = FALSE,
                          `pop.refugee`             = FALSE,
                          `pop.refugeeinformal`     = FALSE,
                          `pop.refugeehost`         = FALSE,
                          `pop.refugeeother`        = FALSE,
                          `pop.host`                = FALSE,
                          `pop.other`               = FALSE,
                          gender_disaggregation     = TRUE,
                          sex_disaggregation        = TRUE,
                          data_management_platform  = "IMPACT",
                          expected_output_type      = NULL,
                          access                    = NULL) {
      super$initialize(
        assessment_title = assessment_title,
        country_name     = country_name,
        month_year       = month_year,
        framework_type   = "ana"
      )
      # Store version
      self$metadata$version <- as.integer(version)

      # Store all optional IPHRA-specific metadata
      self$metadata$type_of_emergency        <- type_of_emergency
      self$metadata$type_of_crisis           <- type_of_crisis
      self$metadata$mandating_body           <- mandating_body
      self$metadata$project_code             <- project_code
      self$metadata$overall_timeframe        <- overall_timeframe
      # geographic_coverage supersedes the deprecated geographic_description parameter.
      # Both are set to the same resolved value so that code that reads either field
      # continues to work during the transition period.
      self$metadata$geographic_coverage      <- geographic_coverage %||% geographic_description
      self$metadata$geographic_description   <- geographic_coverage %||% geographic_description
      self$metadata$general_objective        <- general_objective
      self$metadata$pilot_date               <- private$.fmt_date(pilot_date)
      self$metadata$data_start_date          <- private$.fmt_date(data_start_date)
      self$metadata$data_end_date            <- private$.fmt_date(data_end_date)
      self$metadata$analysis_date            <- private$.fmt_date(analysis_date)
      self$metadata$data_validation_date     <- private$.fmt_date(data_validation_date)
      self$metadata$prelim_presentation_date <- private$.fmt_date(prelim_presentation_date)
      self$metadata$output_validation_date   <- private$.fmt_date(output_validation_date)
      self$metadata$output_published_date    <- private$.fmt_date(output_published_date)
      self$metadata$final_presentation_date  <- private$.fmt_date(final_presentation_date)
      self$metadata$humanitarian_milestones  <- humanitarian_milestones
      # Audience type as four distinct boolean fields
      self$metadata[["audience_type.strategic"]]    <- isTRUE(`audience_type.strategic`)
      self$metadata[["audience_type.operational"]]  <- isTRUE(`audience_type.operational`)
      self$metadata[["audience_type.programmatic"]] <- isTRUE(`audience_type.programmatic`)
      self$metadata[["audience_type.other"]]        <- isTRUE(`audience_type.other`)
      self$metadata$dissemination            <- dissemination
      self$metadata$recall_period            <- recall_period
      self$metadata$population               <- population
      # Population boolean fields
      self$metadata[["pop.idpcamp"]]         <- isTRUE(`pop.idpcamp`)
      self$metadata[["pop.idphost"]]         <- isTRUE(`pop.idphost`)
      self$metadata[["pop.idpinformal"]]     <- isTRUE(`pop.idpinformal`)
      self$metadata[["pop.idpother"]]        <- isTRUE(`pop.idpother`)
      self$metadata[["pop.refugee"]]         <- isTRUE(`pop.refugee`)
      self$metadata[["pop.refugeeinformal"]] <- isTRUE(`pop.refugeeinformal`)
      self$metadata[["pop.refugeehost"]]     <- isTRUE(`pop.refugeehost`)
      self$metadata[["pop.refugeeother"]]    <- isTRUE(`pop.refugeeother`)
      self$metadata[["pop.host"]]            <- isTRUE(`pop.host`)
      self$metadata[["pop.other"]]           <- isTRUE(`pop.other`)
      self$metadata$gender_disaggregation    <- isTRUE(gender_disaggregation)
      self$metadata$sex_disaggregation       <- isTRUE(sex_disaggregation)
      self$metadata$data_management_platform <- data_management_platform
      self$metadata$expected_output_type     <- expected_output_type
      self$metadata$access                   <- access

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
        self$touch()
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

        self$touch()
        phr_message(
          phr_txt("Recall date updated to '{date_str}' in tool '{tool_name}'."),
          origin = "IPHRAProtocol$update_recall_date"
        )
      }, on_error = "abort", origin = "IPHRAProtocol$update_recall_date")
      invisible(self)
    },

    #' @description Generate a Word document Terms of Reference based on the
    #'   bundled IPHRA template.
    #'
    #' Produces a \code{.docx} file from the REACH IPHRA TOR template, filling
    #' in all \code{@}-tagged placeholders with data drawn from this Protocol's
    #' metadata, sampling design, and tool/framework content.
    #'
    #' @param output_file Character. Output \code{.docx} file path.
    #'   Defaults to \code{"protocol_report.docx"}.
    #' @param reference_docx Character or \code{NULL}. Path to a custom
    #'   \code{.docx} template.  Uses the bundled REACH IPHRA TOR template by
    #'   default.
    #' @param open Logical. Open the file after writing.  Defaults to
    #'   \code{FALSE}.
    #' @return Invisibly returns \code{self} for method chaining.
    generate_reach_tor = function(output_file    = "protocol_report.docx",
                                  reference_docx = "reach_tor_iphra_template.docx",
                                  open           = FALSE) {
      phr_try({
        doc <- private$create_base_doc(reference_docx)
        doc <- private$process_tool_row_tags(doc)
        doc <- private$add_metadata_section(doc)
        doc <- private$process_sampling_tags(doc)
        doc <- private$add_specific_objectives_section(doc)
        doc <- private$add_research_questions_section(doc)
        doc <- private$add_primary_data_sources_table(doc)
        doc <- private$add_sdr_section(doc)
        doc <- private$remove_remaining_tags(doc)
        print(doc, target = output_file)
        phr_message(
          phr_txt("IPHRA TOR saved to: {output_file}"),
          origin = "IPHRAProtocol$generate_reach_tor"
        )
        if (isTRUE(open)) utils::browseURL(output_file)
      }, on_error = "abort", origin = "IPHRAProtocol$generate_reach_tor")
      invisible(self)
    }
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

    # ── Template helpers ────────────────────────────────────────────────────

    # Format a date value to "DD/MM/YYYY" for TOR display.
    # Returns an empty string for NULL/NA inputs.
    .fmt_date = function(d) {
      if (is.null(d) || (length(d) == 1 && is.na(d))) return("")
      tryCatch(format(as.Date(d), "%d/%m/%Y"), error = function(e) as.character(d))
    },

    # Replace tag with "X" if condition is TRUE, "□" if FALSE.
    .checkbox = function(doc, tag, condition) {
      officer::body_replace_all_text(doc, tag,
                                     if (isTRUE(condition)) "X" else "\u25a1",
                                     fixed = TRUE)
    },

    # Safe body_replace_all_text; warns on failure instead of aborting.
    .replace = function(doc, old, new_val) {
      tryCatch(
        officer::body_replace_all_text(doc, old, as.character(new_val %||% ""),
                                       fixed = TRUE),
        error = function(e) {
          phr_warning(phr_txt("Tag replacement failed for '{old}': {conditionMessage(e)}"),
                      origin = "IPHRAProtocol$generate_reach_tor")
          doc
        }
      )
    },

    # ── TOR generation private methods ─────────────────────────────────────

    # Use the IPHRA-specific template rather than the generic REACH TOR.
    create_base_doc = function(reference_docx = NULL) {
      if (!is.null(reference_docx) && file.exists(reference_docx)) {
        return(officer::read_docx(reference_docx))
      }
      iphra_path <- system.file("resources", "reach_tor_iphra_template.docx",
                                package = "phr")
      if (nzchar(iphra_path) && file.exists(iphra_path)) {
        return(officer::read_docx(iphra_path))
      }
      # Fallback to generic template
      reach_path <- system.file("resources", "reach_tor_template.docx", package = "phr")
      if (nzchar(reach_path) && file.exists(reach_path)) {
        return(officer::read_docx(reach_path))
      }
      officer::read_docx()
    },

    # Delete table rows whose text contains a tool @-tag when that tool is
    # NOT included in self$tools.  After deletion, remove any residual tag
    # text from rows that ARE included.
    process_tool_row_tags = function(doc) {
      # Mapping: @tag → tool_name in self$tools
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

      body_xml <- officer::docx_body_xml(doc)
      ns       <- xml2::xml_ns(body_xml)

      for (tag in names(tag_tool_map)) {
        tool_name <- tag_tool_map[[tag]]
        included  <- self$is_tool_included(tool_name)

        if (!included) {
          # Remove ALL table rows that mention this tag
          rows <- xml2::xml_find_all(body_xml, ".//w:tr", ns = ns)
          for (row_node in rows) {
            if (grepl(tag, xml2::xml_text(row_node), fixed = TRUE)) {
              xml2::xml_remove(row_node)
            }
          }
        } else {
          # Just strip the tag text from the document
          doc <- private$.replace(doc, tag, "")
        }
      }
      doc
    },

    # Replace all @sampling_* and @sample_*_target tags based on sample_table.
    process_sampling_tags = function(doc) {
      methods_used <- self$get_sampling_methods()

      # Site-level method groups
      is_srs        <- any(methods_used %in% c("simple_random", "simple_random_rlc"))
      is_systematic <- any(methods_used %in% c("systematic", "systematic_rlc"))
      is_cluster    <- any(methods_used %in% c("pps_cluster", "pps_rlc"))
      is_exhaustive <- any(methods_used %in% c("proportional", "proportional_rlc"))
      is_purposive  <- any(methods_used %in% c("purposive"))
      # Household-level method groups
      is_hh_srs  <- any(methods_used %in% c("simple_random", "systematic"))
      is_hh_rlc  <- any(methods_used %in%
                          c("pps_rlc", "simple_random_rlc", "systematic_rlc", "proportional_rlc"))

      doc <- private$.checkbox(doc, "@sampling_srs",        is_srs)
      doc <- private$.checkbox(doc, "@sampling_systematic",  is_systematic)
      doc <- private$.checkbox(doc, "@sampling_cluster",     is_cluster)
      doc <- private$.checkbox(doc, "@sampling_exhaustive",  is_exhaustive)
      doc <- private$.checkbox(doc, "@sampling_purposive",   is_purposive)
      doc <- private$.checkbox(doc, "@sampling_hh_srs",     is_hh_srs)
      doc <- private$.checkbox(doc, "@sampling_hh_rlc",     is_hh_rlc)
      # @sampling_stratified — TRUE if more than one stratum
      is_stratified <- length(self$get_strata_names()) > 1
      doc <- private$.checkbox(doc, "@sampling_stratified",  is_stratified)
      # @sampling_modified_epi — not directly derivable; leave as □
      doc <- private$.replace(doc, "@sampling_modified_epi", "\u25a1")

      # Sample size targets (sum across strata)
      st <- self$sample_table
      site_target <- if (!is.null(st) && "n_sites" %in% names(st)) {
        s <- sum(as.numeric(st$n_sites), na.rm = TRUE)
        if (s > 0) as.character(round(s)) else "_"
      } else "_"
      hh_target <- if (!is.null(st) && "Final_HH_Sample_Size" %in% names(st)) {
        s <- sum(as.numeric(st$Final_HH_Sample_Size), na.rm = TRUE)
        if (s > 0) as.character(round(s)) else "_"
      } else "_"

      doc <- private$.replace(doc, "@sample_site_target", site_target)
      doc <- private$.replace(doc, "@sample_hh_target",   hh_target)
      doc
    },

    # Fill in all @-tagged metadata placeholders in the IPHRA template.
    add_metadata_section = function(doc) {
      m <- self$metadata

      # ── Simple text fields ─────────────────────────────────────────────
      doc <- private$.replace(doc, "@country",       m$country_name      %||% "")
      doc <- private$.replace(doc, "@release_date",  format(Sys.Date(), "%d/%m/%Y"))
      doc <- private$.replace(doc, "@version_number",
                               paste0("v", m$version %||% 1L))
      doc <- private$.replace(doc, "@mandating_body",  m$mandating_body  %||% "")
      doc <- private$.replace(doc, "@project_code",    m$project_code    %||% "")
      doc <- private$.replace(doc, "@overall_timeframe",
                               m$overall_timeframe %||% "")
      doc <- private$.replace(doc, "@geographic_coverage",
                               m$geographic_coverage %||% m$geographic_description %||% "")
      doc <- private$.replace(doc, "@recall_period", m$recall_period %||% "")
      doc <- private$.replace(doc, "@general_objective",
                               m$general_objective %||% "")

      # ── Date fields ────────────────────────────────────────────────────
      doc <- private$.replace(doc, "@pilot_date",               m$pilot_date               %||% "")
      doc <- private$.replace(doc, "@data_start_date",          m$data_start_date          %||% "")
      doc <- private$.replace(doc, "@data_end_date",            m$data_end_date            %||% "")
      doc <- private$.replace(doc, "@analysis_date",            m$analysis_date            %||% "")
      doc <- private$.replace(doc, "@data_validation_date",     m$data_validation_date     %||% "")
      doc <- private$.replace(doc, "@prelim_presentation_date", m$prelim_presentation_date %||% "")
      doc <- private$.replace(doc, "@output_validation_date",   m$output_validation_date   %||% "")
      doc <- private$.replace(doc, "@output_published_date",    m$output_published_date    %||% "")
      doc <- private$.replace(doc, "@final_presentation_date",  m$final_presentation_date  %||% "")

      # ── Type of Emergency ──────────────────────────────────────────────
      em <- m$type_of_emergency %||% ""
      doc <- private$.checkbox(doc, "@em_natural_d",   em == "natural_disaster")
      doc <- private$.checkbox(doc, "@em_conflict",    em == "conflict")
      doc <- private$.checkbox(doc, "@em_other",       em == "other")

      # ── Type of Crisis ─────────────────────────────────────────────────
      cr <- m$type_of_crisis %||% ""
      doc <- private$.checkbox(doc, "@crisis_sudden",     cr == "sudden_onset")
      doc <- private$.checkbox(doc, "@crisis_slow",       cr == "slow_onset")
      doc <- private$.checkbox(doc, "@crisis_protracted", cr == "protracted")

      # ── Humanitarian milestones ────────────────────────────────────────
      ms <- m$humanitarian_milestones %||% character(0)
      doc <- private$.checkbox(doc, "@milestone_donor",         "donor"         %in% ms)
      doc <- private$.checkbox(doc, "@milestone_inter_cluster", "inter_cluster" %in% ms)
      doc <- private$.checkbox(doc, "@milestone_cluster",       "cluster"       %in% ms)
      doc <- private$.checkbox(doc, "@milestone_ngo_platform",  "ngo_platform"  %in% ms)
      doc <- private$.checkbox(doc, "@milestone_other",         "other"         %in% ms)

      # ── Audience type (four distinct boolean fields) ───────────────────
      doc <- private$.checkbox(doc, "@audience_type.strategic",
                               isTRUE(m[["audience_type.strategic"]]))
      doc <- private$.checkbox(doc, "@audience_type.programmatic",
                               isTRUE(m[["audience_type.programmatic"]]))
      doc <- private$.checkbox(doc, "@audience_type.operational",
                               isTRUE(m[["audience_type.operational"]]))
      doc <- private$.checkbox(doc, "@audience_type.other",
                               isTRUE(m[["audience_type.other"]]))

      # ── Dissemination ──────────────────────────────────────────────────
      diss <- m$dissemination %||% character(0)
      doc <- private$.checkbox(doc, "@dissem_general_mailing",  "general_mailing"  %in% diss)
      doc <- private$.checkbox(doc, "@dissem_cluster_mailing",  "cluster_mailing"  %in% diss)
      doc <- private$.checkbox(doc, "@dissem_presentation",     "presentation"     %in% diss)
      doc <- private$.checkbox(doc, "@dissem_website",          "website"          %in% diss)
      doc <- private$.checkbox(doc, "@dissem_other",            "other"            %in% diss)

      # ── Population boolean fields ─────────────────────────────────────
      doc <- private$.checkbox(doc, "@pop.idpcamp",         isTRUE(m[["pop.idpcamp"]]))
      doc <- private$.checkbox(doc, "@pop.idphost",         isTRUE(m[["pop.idphost"]]))
      doc <- private$.checkbox(doc, "@pop.idpinformal",     isTRUE(m[["pop.idpinformal"]]))
      doc <- private$.checkbox(doc, "@pop.idpother",        isTRUE(m[["pop.idpother"]]))
      doc <- private$.checkbox(doc, "@pop.refugee",         isTRUE(m[["pop.refugee"]]))
      doc <- private$.checkbox(doc, "@pop.refugeeinformal", isTRUE(m[["pop.refugeeinformal"]]))
      doc <- private$.checkbox(doc, "@pop.refugeehost",     isTRUE(m[["pop.refugeehost"]]))
      doc <- private$.checkbox(doc, "@pop.refugeeother",    isTRUE(m[["pop.refugeeother"]]))
      doc <- private$.checkbox(doc, "@pop.host",            isTRUE(m[["pop.host"]]))
      doc <- private$.checkbox(doc, "@pop.other",           isTRUE(m[["pop.other"]]))

      # ── Gender / Sex disaggregation ────────────────────────────────────
      doc <- private$.checkbox(doc, "@gender_disagg_yes", isTRUE(m$gender_disaggregation))
      doc <- private$.checkbox(doc, "@gender_disagg_no",  !isTRUE(m$gender_disaggregation))
      doc <- private$.checkbox(doc, "@sex_disagg_yes",    isTRUE(m$sex_disaggregation))
      doc <- private$.checkbox(doc, "@sex_disagg_no",     !isTRUE(m$sex_disaggregation))

      # ── Data management platform ───────────────────────────────────────
      plat <- m$data_management_platform %||% character(0)
      doc <- private$.checkbox(doc, "@platform_impact", "IMPACT" %in% plat)
      doc <- private$.checkbox(doc, "@platform_unhcr",  "UNHCR"  %in% plat)
      doc <- private$.checkbox(doc, "@platform_other",  "other"  %in% plat)

      # ── Expected output type ───────────────────────────────────────────
      ot <- m$expected_output_type %||% character(0)
      doc <- private$.checkbox(doc, "@output_situation_overview",   "situation_overview"   %in% ot)
      doc <- private$.checkbox(doc, "@output_report",               "report"               %in% ot)
      doc <- private$.checkbox(doc, "@output_profile",              "profile"              %in% ot)
      doc <- private$.checkbox(doc, "@output_prelim_presentation",  "prelim_presentation"  %in% ot)
      doc <- private$.checkbox(doc, "@output_final_presentation",   "final_presentation"   %in% ot)
      doc <- private$.checkbox(doc, "@output_factsheet",            "factsheet"            %in% ot)
      doc <- private$.checkbox(doc, "@output_dashboard",            "dashboard"            %in% ot)
      doc <- private$.checkbox(doc, "@output_webmap",               "webmap"               %in% ot)
      doc <- private$.checkbox(doc, "@output_map",                  "map"                  %in% ot)
      doc <- private$.checkbox(doc, "@output_other",                "other"                %in% ot)

      # ── Access ─────────────────────────────────────────────────────────
      ac <- m$access %||% ""
      doc <- private$.checkbox(doc, "@access_public",     ac == "public")
      doc <- private$.checkbox(doc, "@access_restricted", ac == "restricted")

      # ── Secondary data plain list (@list_secondary_data) ──────────────
      sdr <- self$secondary_data
      sdr_text <- if (!is.null(sdr) && length(sdr) > 0) {
        paste(names(sdr), sdr, sep = ": ", collapse = "\n")
      } else {
        ""
      }
      doc <- private$.replace(doc, "@list_secondary_data", sdr_text)

      doc
    },

    # Replace @specific_objectives in the metadata table cell with a
    # pillar-grouped list of text objectives for all indicators in tools.
    # Headers (pillars) are bolded, each objective is on its own line, and a
    # blank line separates pillar groups.
    add_specific_objectives_section = function(doc) {
      inc_codes <- self$get_indicator_codes_from_tools()
      if (length(inc_codes) == 0) {
        return(private$.replace(doc, "@specific_objectives", ""))
      }

      schema <- self$get_schema_for_indicators(inc_codes, type = "master")
      if (!is.data.frame(schema) || nrow(schema) == 0) {
        return(private$.replace(doc, "@specific_objectives", ""))
      }

      # Build: pillar → unique text_objectives
      cols_needed <- c("pillar", "objective_code", "text_objective")
      if (!all(cols_needed %in% names(schema))) {
        return(private$.replace(doc, "@specific_objectives", ""))
      }

      obj_df <- unique(schema[, cols_needed, drop = FALSE])
      obj_df <- obj_df[!is.na(obj_df$text_objective) & nzchar(obj_df$text_objective), ]

      if (nrow(obj_df) == 0) {
        return(private$.replace(doc, "@specific_objectives", ""))
      }

      pillars <- unique(obj_df$pillar[!is.na(obj_df$pillar) & nzchar(obj_df$pillar)])

      # Try to insert formatted paragraphs (bold headers) using officer cursor API
      inserted <- FALSE
      tryCatch({
        doc <- officer::cursor_reach(doc, keyword = "@specific_objectives")
        fp_bold <- officer::fp_text(bold = TRUE)
        first_pillar <- TRUE
        for (p in pillars) {
          sub_objs <- obj_df$text_objective[obj_df$pillar == p]
          sub_objs <- unique(sub_objs[!is.na(sub_objs) & nzchar(sub_objs)])
          if (!first_pillar) {
            # Blank line before each subsequent pillar group
            doc <- officer::body_add_par(doc, "", pos = "before")
            doc <- officer::cursor_forward(doc)
          }
          # Bold pillar header
          header_fpar <- officer::fpar(officer::ftext(p, prop = fp_bold))
          doc <- officer::body_add_fpar(doc, header_fpar, pos = "before")
          doc <- officer::cursor_forward(doc)
          # One bullet per objective
          for (obj in sub_objs) {
            doc <- officer::body_add_par(doc, paste0("\u2022 ", obj), pos = "before")
            doc <- officer::cursor_forward(doc)
          }
          first_pillar <- FALSE
        }
        # Remove the original tag paragraph
        doc <- officer::body_remove(doc)
        inserted <- TRUE
      }, error = function(e) {
        phr_warning(
          phr_txt("Could not insert formatted specific objectives: {conditionMessage(e)}"),
          origin = "IPHRAProtocol$add_specific_objectives_section"
        )
      })

      if (!inserted) {
        # Fallback: plain text replacement
        lines <- character(0)
        first_pillar <- TRUE
        for (p in pillars) {
          if (!first_pillar) lines <- c(lines, "")
          lines <- c(lines, p)
          sub_objs <- obj_df$text_objective[obj_df$pillar == p]
          sub_objs <- unique(sub_objs[!is.na(sub_objs) & nzchar(sub_objs)])
          for (obj in sub_objs) {
            lines <- c(lines, paste0("\u2022 ", obj))
          }
          first_pillar <- FALSE
        }
        doc <- private$.replace(doc, "@specific_objectives", paste(lines, collapse = "\n"))
      }
      doc
    },

    # Replace @research_questions in the metadata table cell.
    # Groups research_question values under their objective_research_question
    # (bolded header), each question on its own line, blank line between groups.
    add_research_questions_section = function(doc) {
      inc_codes <- self$get_indicator_codes_from_tools()
      if (length(inc_codes) == 0) {
        return(private$.replace(doc, "@research_questions", ""))
      }

      schema <- self$get_schema_for_indicators(inc_codes, type = "master")
      if (!is.data.frame(schema) || nrow(schema) == 0 ||
          !"research_question" %in% names(schema)) {
        return(private$.replace(doc, "@research_questions", ""))
      }

      has_orq <- "objective_research_question" %in% names(schema)
      # Build objective_research_question → research_questions map
      # Preserve ordering by first appearance
      seen_orq <- character(0)
      orq_to_rqs <- list()

      for (i in seq_len(nrow(schema))) {
        orq <- if (has_orq) as.character(schema$objective_research_question[i]) else NA_character_
        rq  <- as.character(schema$research_question[i])
        if (is.na(orq) || !nzchar(orq)) orq <- "(General)"
        if (!orq %in% seen_orq) {
          seen_orq <- c(seen_orq, orq)
          orq_to_rqs[[orq]] <- character(0)
        }
        if (!is.na(rq) && nzchar(rq) && !rq %in% orq_to_rqs[[orq]]) {
          orq_to_rqs[[orq]] <- c(orq_to_rqs[[orq]], rq)
        }
      }

      # Try to insert formatted paragraphs (bold headers) using officer cursor API
      inserted <- FALSE
      tryCatch({
        doc <- officer::cursor_reach(doc, keyword = "@research_questions")
        fp_bold <- officer::fp_text(bold = TRUE)
        first_orq <- TRUE
        for (orq in seen_orq) {
          if (!first_orq) {
            doc <- officer::body_add_par(doc, "", pos = "before")
            doc <- officer::cursor_forward(doc)
          }
          header_fpar <- officer::fpar(officer::ftext(orq, prop = fp_bold))
          doc <- officer::body_add_fpar(doc, header_fpar, pos = "before")
          doc <- officer::cursor_forward(doc)
          for (rq in orq_to_rqs[[orq]]) {
            doc <- officer::body_add_par(doc, paste0("\u2022 ", rq), pos = "before")
            doc <- officer::cursor_forward(doc)
          }
          first_orq <- FALSE
        }
        doc <- officer::body_remove(doc)
        inserted <- TRUE
      }, error = function(e) {
        phr_warning(
          phr_txt("Could not insert formatted research questions: {conditionMessage(e)}"),
          origin = "IPHRAProtocol$add_research_questions_section"
        )
      })

      if (!inserted) {
        lines <- character(0)
        first_orq <- TRUE
        for (orq in seen_orq) {
          if (!first_orq) lines <- c(lines, "")
          lines <- c(lines, orq)
          for (rq in orq_to_rqs[[orq]]) {
            lines <- c(lines, paste0("\u2022 ", rq))
          }
          first_orq <- FALSE
        }
        doc <- private$.replace(doc, "@research_questions", paste(lines, collapse = "\n"))
      }
      doc
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
    add_sdr_section = function(doc) {
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
                          origin = "IPHRAProtocol$add_sdr_section")
            })
          }, error = function(e) {
            phr_warning(phr_txt("magick SVG rasterisation failed: {conditionMessage(e)}"),
                        origin = "IPHRAProtocol$add_sdr_section")
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
                          origin = "IPHRAProtocol$add_sdr_section")
            })
          }, error = function(e) {
            phr_warning(phr_txt("rsvg SVG-to-PNG conversion failed: {conditionMessage(e)}"),
                        origin = "IPHRAProtocol$add_sdr_section")
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

        # Merge consecutive cells with the same Objective label
        ft <- flextable::flextable(sdr_df)
        ft <- flextable::merge_v(ft, j = "Objective")
        ft <- flextable::theme_zebra(ft)
        ft <- flextable::autofit(ft)

        tryCatch({
          doc <- officer::cursor_reach(doc, keyword = "@secondary_data_sources_table")
          doc <- flextable::body_add_flextable(doc, ft, pos = "before")
          doc <- officer::cursor_forward(doc)
          doc <- officer::body_remove(doc)
        }, error = function(e) {
          phr_warning(phr_txt("Could not insert secondary data table: {conditionMessage(e)}"),
                      origin = "IPHRAProtocol$add_sdr_section")
          doc <<- private$.replace(doc, "@secondary_data_sources_table", "")
        })
      } else {
        doc <- private$.replace(doc, "@secondary_data_sources_table", "")
      }

      doc
    },

    # After all known tag replacements, remove any remaining @-prefixed tags
    # so they do not appear in the exported document.
    remove_remaining_tags = function(doc) {
      body_xml <- officer::docx_body_xml(doc)
      ns       <- xml2::xml_ns(body_xml)

      # Collect all text nodes in the document body
      text_nodes <- xml2::xml_find_all(body_xml, ".//w:t", ns = ns)
      # Pattern matches all @-prefixed placeholder tags used in TOR templates.
      # Underscores and periods appear in tag names (e.g. @pop.idpcamp, @data_start_date).
      # Hyphens are included to handle any hyphenated tag variants in custom templates.
      tag_pattern <- "@[A-Za-z0-9_.\\-]+"

      for (node in text_nodes) {
        txt <- xml2::xml_text(node)
        if (grepl(tag_pattern, txt)) {
          cleaned <- gsub(tag_pattern, "", txt, perl = TRUE)
          xml2::xml_text(node) <- cleaned
        }
      }
      doc
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
    }
  )
)
