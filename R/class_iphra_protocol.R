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
    initialize = function(
      assessment_title = NULL,
      country_name = NULL,
      month_year = NULL,
      version = 1L,
      type_of_emergency = NULL,
      type_of_crisis = NULL,
      mandating_body = NULL,
      project_code = NULL,
      overall_timeframe = NULL,
      geographic_coverage = NULL,
      general_objective = phr_txt(
        "To assess the severity of the public health outcomes and identify initial public health priorities for response to mitigate excess morbidity, malnutrition, and mortality."
      ),
      pilot_date = NULL,
      data_start_date = NULL,
      data_end_date = NULL,
      analysis_date = NULL,
      data_validation_date = NULL,
      prelim_presentation_date = NULL,
      output_validation_date = NULL,
      output_published_date = NULL,
      final_presentation_date = NULL,
      humanitarian_milestones = NULL,
      `audience_type.strategic` = FALSE,
      `audience_type.operational` = TRUE,
      `audience_type.programmatic` = TRUE,
      `audience_type.other` = FALSE,
      dissemination = NULL,
      recall_period = NULL,
      geographic_description = NULL,
      population = NULL,
      pop_idpcamp = FALSE,
      pop_idphost = FALSE,
      pop_idpinformal = FALSE,
      pop_idpother = FALSE,
      pop_refugee = FALSE,
      pop_refugeeinformal = FALSE,
      pop_refugeehost = FALSE,
      pop_refugeeother = FALSE,
      pop_host = FALSE,
      pop_other = FALSE,
      stakeholder_mapping = FALSE,
      num_geographic_units = NA_real_,
      popsize_known_geographic_unit = FALSE,
      popsize_known_strata_unit = FALSE,
      num_kii_health_target = NA_real_,
      num_kii_market_target = NA_real_,
      num_kii_fsl_target = NA_real_,
      num_kii_wash_target = NA_real_,
      num_kii_nutrition_target = NA_real_,
      num_obs_health_target = NA_real_,
      num_obs_latrine_target = NA_real_,
      num_obs_waterpoint_target = NA_real_,
      gender_disaggregation = TRUE,
      sex_disaggregation = TRUE,
      data_management_platform = "IMPACT",
      expected_output_type = NULL,
      access = NULL
    ) {
      super$initialize(
        assessment_title = assessment_title,
        country_name = country_name,
        month_year = month_year,
        framework_type = "ana",
        reference_doc_filename = "reach_tor_iphra_template.docx"
      )
      self$valid_tool_types <- c(
        "household",
        "key_informant",
        "observation",
        "generic"
      )
      self$metadata$assessment_title <- assessment_title
      self$metadata$country_name <- country_name
      self$metadata$month_year <- month_year
      self$metadata$framework_type <- "ana"

      # Load protocol schema (tags + defaults) from the bundled resource
      private$..load_protocol_schema()
      private$..initialize_conditional_metadata()

      phr_message(
        phr_txt("IPHRAProtocol initialized."),
        origin = "IPHRAProtocol$initialize"
      )
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
      phr_try(
        {
          allowable <- private$..iphra_tools

          phr_assert(
            is.character(tool_name) &&
              length(tool_name) == 1 &&
              nzchar(tool_name),
            message = phr_txt(
              "tool_name must be a non-empty character string."
            ),
            origin = "IPHRAProtocol$add_tools"
          )
          phr_assert(
            tool_name %in% names(allowable),
            message = phr_txt(
              "'{tool_name}' is not a recognised IPHRA tool. Allowable tools: {paste(names(allowable), collapse=', ')}."
            ),
            origin = "IPHRAProtocol$add_tools"
          )

          tool_spec <- allowable[[tool_name]]
          tool_class <- tool_spec$class
          xlsx_file <- tool_spec$file

          # Locate the XLSForm resource file (gracefully handles missing files)
          tool_path <- system.file("resources", xlsx_file, package = "phr")
          if (!nzchar(tool_path) || !file.exists(tool_path)) {
            tool_path <- file.path("resources", xlsx_file)
          }

          tool <- if (identical(tool_class, "HouseholdTool")) {
            if (file.exists(tool_path)) {
              t <- HouseholdTool$new(
                name = tool_name,
                survey = NULL,
                choices = NULL
              )
              private$..load_tool_from_path(t, tool_path)
              t
            } else {
              phr_warning(
                message = phr_txt(
                  "XLSForm file not found for '{tool_name}': {xlsx_file}. Creating empty tool."
                ),
                origin = "IPHRAProtocol$add_tools"
              )
              HouseholdTool$new(name = tool_name)
            }
          } else if (identical(tool_class, "KeyInformantTool")) {
            if (file.exists(tool_path)) {
              t <- KeyInformantTool$new(
                name = tool_name,
                survey = NULL,
                choices = NULL
              )
              private$..load_tool_from_path(t, tool_path)
              t
            } else {
              phr_warning(
                message = phr_txt(
                  "XLSForm file not found for '{tool_name}': {xlsx_file}. Creating empty tool."
                ),
                origin = "IPHRAProtocol$add_tools"
              )
              KeyInformantTool$new(name = tool_name)
            }
          } else {
            # ObservationTool
            if (file.exists(tool_path)) {
              t <- ObservationTool$new(
                name = tool_name,
                survey = NULL,
                choices = NULL
              )
              private$..load_tool_from_path(t, tool_path)
              t
            } else {
              phr_warning(
                message = phr_txt(
                  "XLSForm file not found for '{tool_name}': {xlsx_file}. Creating empty tool."
                ),
                origin = "IPHRAProtocol$add_tools"
              )
              ObservationTool$new(name = tool_name)
            }
          }

          if (is.null(self$tools)) {
            self$tools <- list()
          }
          self$tools[[tool_name]] <- tool
          private$..touch()
          phr_message(
            phr_txt("IPHRA tool '{tool_name}' added."),
            origin = "IPHRAProtocol$add_tools"
          )
        },
        on_error = "abort",
        origin = "IPHRAProtocol$add_tools"
      )
      invisible(self)
    },

    #' @description Return the names of all allowable IPHRA tools.
    #' @return Character vector of tool names.
    get_allowable_tools = function() {
      names(private$..iphra_tools)
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
    update_recall_date = function(
      recall_date,
      tool_name = "tool_household_iphra_v2"
    ) {
      phr_try(
        {
          phr_assert(
            !is.null(recall_date),
            message = phr_txt("recall_date must not be NULL."),
            origin = "IPHRAProtocol$update_recall_date"
          )

          # Normalise to Date then format
          date_obj <- tryCatch(
            as.Date(recall_date),
            error = function(e) {
              phr_error(
                message = phr_txt(
                  "recall_date could not be coerced to a Date: {conditionMessage(e)}"
                ),
                origin = "IPHRAProtocol$update_recall_date"
              )
            }
          )

          date_str <- format(date_obj, "%Y-%m-%d")
          month_first <- format(date_obj, "%Y-%m-01")
          recall_event_str <- format(date_obj, "%d %B %Y")

          phr_assert(
            !is.null(self$tools) && tool_name %in% names(self$tools),
            message = phr_txt(
              "Tool '{tool_name}' not found. Add it first with add_tools()."
            ),
            origin = "IPHRAProtocol$update_recall_date"
          )

          tool <- self$tools[[tool_name]]

          .update_recall_in_survey <- function(sv) {
            if (
              is.null(sv) ||
                !"name" %in% names(sv) ||
                !"calculation" %in% names(sv)
            ) {
              return(sv)
            }
            idx_event <- which(sv$name == "recall_event")
            idx_date <- which(sv$name == "recall_date")
            idx_month <- which(sv$name == "recall_month")
            if (length(idx_event) > 0) {
              sv$calculation[idx_event] <- paste0(
                "if(1=1, '",
                recall_event_str,
                "','')"
              )
            }
            if (length(idx_date) > 0) {
              sv$calculation[idx_date] <- paste0(
                "if(1=1, date('",
                date_str,
                "'),'')"
              )
            }
            if (length(idx_month) > 0) {
              sv$calculation[idx_month] <- paste0(
                "if(1=1, date('",
                month_first,
                "'),'')"
              )
            }
            sv
          }

          tool$survey <- .update_recall_in_survey(tool$survey)
          tool$revised_survey <- .update_recall_in_survey(tool$revised_survey)

          private$..touch()
          phr_message(
            phr_txt(
              "Recall date updated to '{date_str}' in tool '{tool_name}'."
            ),
            origin = "IPHRAProtocol$update_recall_date"
          )
        },
        on_error = "abort",
        origin = "IPHRAProtocol$update_recall_date"
      )
      invisible(self)
    },

    #' @description Generate an IPHRA document report.
    #' @param output_file Character output \code{.docx} path.
    #' @param open Logical indicating whether to open the output path.
    #' @return Invisibly returns \code{self}.
    generate_doc = function(
      output_file = "protocol_report.docx",
      open = FALSE
    ) {
      super$generate_doc(output_file = output_file, open = open)
    },

    #' @description Post-sync hook for IPHRA-specific synchronization.
    #' @param field Optional top-level field name.
    #' @param member Optional nested member name.
    #' @param target_field Optional destination field path.
    #' @param name Optional named list entry inside \code{field}.
    #' @param role Optional role-based list resolution key.
    #' @return Invisibly returns \code{NULL}.
    post_sync_state = function(
      field = NULL,
      member = NULL,
      target_field = NULL,
      name = NULL,
      role = NULL
    ) {
      super$post_sync_state(
        field = field,
        member = member,
        target_field = target_field,
        name = name,
        role = role
      )
      st <- self$sample_table
      self$metadata$num_strata_units <- if (
        !is.null(st) && is.data.frame(st) && "stratum_id" %in% names(st)
      ) {
        length(unique(st$stratum_id))
      } else {
        0L
      }
      private$..sync_sampling_conditional_metadata()
      invisible(NULL)
    }
  ),

  active = list(
    .kii_community_yes = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("kii_community")
    },
    .kii_fsl_provider_yes = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("kii_fsl_service_provider")
    },
    .kii_health_provider_yes = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("kii_health_service_provider")
    },
    .kii_nutrition_provider_yes = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("kii_nutrition_service_provider")
    },
    .kii_service_providers_yes = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      isTRUE(self$.kii_fsl_provider) ||
        isTRUE(self$.kii_health_provider) ||
        isTRUE(self$.kii_nutrition_provider) ||
        isTRUE(self$.kii_wash_provider)
    },
    .kii_wash_provider_yes = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("kii_wash_service_provider")
    },
    .kii_community_no = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("kii_community")
    },
    .kii_fsl_provider_no = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      !private$..has_tool_role("kii_fsl_service_provider")
    },
    .kii_health_provider_no = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      !private$..has_tool_role("kii_health_service_provider")
    },
    .kii_nutrition_provider_no = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      !private$..has_tool_role("kii_nutrition_service_provider")
    },
    .kii_service_providers_no = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      !(isTRUE(self$.kii_fsl_provider) ||
        isTRUE(self$.kii_health_provider) ||
        isTRUE(self$.kii_nutrition_provider) ||
        isTRUE(self$.kii_wash_provider))
    },
    .kii_wash_provider_no = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      !private$..has_tool_role("kii_wash_service_provider")
    },
    .any_health_facility_data = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("obs_health_facility") ||
        private$..has_tool_role("kii_health_service_provider") ||
        private$..has_tool_role("kii_nutrition_service_provider")
    },

    # ── KII / Observation target active bindings ────────────────────────────
    .num_kii_community_target = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"n_sites" %in% names(st)) {
        return(NULL)
      }
      sum(as.numeric(st$n_sites), na.rm = TRUE) * 1L
    },
    .num_kii_health_target = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st)) {
        return(NULL)
      }
      nrow(st) * 1L
    },
    .num_kii_market_target = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st)) {
        return(NULL)
      }
      nrow(st) * 1L
    },
    .num_kii_fsl_target = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st)) {
        return(NULL)
      }
      nrow(st) * 1L
    },
    .num_kii_wash_target = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st)) {
        return(NULL)
      }
      nrow(st) * 1L
    },
    .num_kii_nutrition_target = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st)) {
        return(NULL)
      }
      nrow(st) * 1L
    },
    .num_obs_community_target = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"n_sites" %in% names(st)) {
        return(NULL)
      }
      sum(as.numeric(st$n_sites), na.rm = TRUE) * 1L
    },
    .num_obs_health_target = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st)) {
        return(NULL)
      }
      nrow(st) * 1L
    },
    .num_obs_latrine_target = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"n_sites" %in% names(st)) {
        return(NULL)
      }
      sum(as.numeric(st$n_sites), na.rm = TRUE) * 1L
    },
    .num_obs_water_target = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"n_sites" %in% names(st)) {
        return(NULL)
      }
      sum(as.numeric(st$n_sites), na.rm = TRUE) * 1L
    },
    .num_obs_crops_livestock_target = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"n_sites" %in% names(st)) {
        return(NULL)
      }
      sum(as.numeric(st$n_sites), na.rm = TRUE) * 1L
    },
    .ind_ecfies = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("10801"))
    },
    .ind_iycfe = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("10802"))
    },
    .ind_measles_vaccination = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("14304"))
    },
    .ind_muac_children = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("10701"))
    },
    .ind_muac_plw = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("10702"))
    },
    .ind_vitamin_a_coverage = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("14305"))
    },
    .mortality_survey = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("10501", "10502"))
    },
    .muac_survey = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("10701", "10702"))
    },

    .obs_community = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("obs_community")
    },
    .obs_crops_livestock = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("obs_crop_livestock")
    },
    .obs_health_facility = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("obs_health_facility")
    },
    .obs_latrines = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("obs_latrine")
    },
    .obs_service_providers = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      any(
        self$.obs_water_point,
        self$.obs_latrines,
        self$.obs_health_facility,
        self$.obs_crops_livestock
      )
    },
    .obs_water_point = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("obs_water_point")
    },
  ),

  private = list(
    # Named list: tool_name -> list(class = <class_name>, file = <xlsx_filename>)
    ..iphra_tools = list(
      tool_household_iphra_v2 = list(
        class = "HouseholdTool",
        file = "tool_household_iphra_v2.xlsx"
      ),
      tool_kii_community_iphra_v2 = list(
        class = "KeyInformantTool",
        file = "tool_kii_community_iphra_v2.xlsx"
      ),
      tool_kii_fsl_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file = "tool_kii_fsl_service_provider_iphra_v2.xlsx"
      ),
      tool_kii_wash_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file = "tool_kii_wash_service_provider_iphra_v2.xlsx"
      ),
      tool_kii_markets_iphra_v2 = list(
        class = "KeyInformantTool",
        file = "tool_kii_markets_iphra_v2.xlsx"
      ),
      tool_kii_nutrition_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file = "tool_kii_nutrition_service_provider_iphra_v2.xlsx"
      ),
      tool_kii_health_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file = "tool_kii_health_service_provider_iphra_v2.xlsx"
      ),
      tool_obs_community_iphra_v2 = list(
        class = "ObservationTool",
        file = "tool_obs_community_iphra_v2.xlsx"
      ),
      tool_obs_crop_livestock_iphra_v1 = list(
        class = "ObservationTool",
        file = "tool_obs_crop_livestock_iphra_v1.xlsx"
      ),
      tool_obs_health_facility_iphra_v2 = list(
        class = "ObservationTool",
        file = "tool_obs_health_facility_iphra_v2.xlsx"
      ),
      tool_obs_latrine_iphra_v2 = list(
        class = "ObservationTool",
        file = "tool_obs_latrine_iphra_v2.xlsx"
      ),
      tool_obs_water_point_iphra_v2 = list(
        class = "ObservationTool",
        file = "tool_obs_water_point_iphra_v2.xlsx"
      )
    ),

    # ── TOR generation private methods ─────────────────────────────────────

    ..default_template_filenames = function() {
      c(
        "reach_tor_iphra_template.docx",
        "reach_tor_template.docx",
        "protocol_report_template.docx"
      )
    },

    # Load protocol_schema_iphra.xlsx into self$protocol_schema.
    # Expected columns are tag_name, handling, condition, default_value, and function_name.
    ..load_protocol_schema = function() {
      schema_path <- tryCatch(
        system.file("resources", "protocol_schema_iphra.xlsx", package = "phr"),
        error = function(e) ""
      )
      if (!nzchar(schema_path) || !file.exists(schema_path)) {
        # Try relative path (development / test context)
        schema_path <- file.path(
          "inst",
          "resources",
          "protocol_schema_iphra.xlsx"
        )
      }
      if (!file.exists(schema_path)) {
        phr_warning(
          phr_txt(
            "protocol_schema_iphra.xlsx not found; skipping schema load."
          ),
          origin = "IPHRAProtocol$.load_protocol_schema"
        )
        return(invisible(NULL))
      }
      schema <- tryCatch(
        as.data.frame(readxl::read_excel(
          schema_path,
          sheet = "schema",
          col_types = "text"
        )),
        error = function(e) NULL
      )
      required_cols <- c(
        "tag_name",
        "handling",
        "condition",
        "default_value",
        "function_name"
      )
      if (is.null(schema) || !all(required_cols %in% names(schema))) {
        return(invisible(NULL))
      }
      schema <- private$..normalize_schema_tags(schema)
      # Store schema for reference
      self$protocol_schema <- schema[required_cols]
      invisible(NULL)
    },
  )
)
