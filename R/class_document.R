#' Document R6 Class
#'
#' @description
#' Subclass of \code{\link{Asset}} that provides shared DOCX handling
#' helpers for protocol/report-like classes.
#'
#' @importFrom R6 R6Class
#' @export
Document <- R6::R6Class(
  "Document",
  inherit = Asset,
  public = list(
    #' @field reference_doc_filename Optional template filename/path used to
    #'   initialize \code{document}.
    reference_doc_filename = NULL,

    #' @field reference_ppt_filename Optional template filename/path used to
    #'   initialize \code{powerpoint}.
    reference_ppt_filename = NULL,

    #' @description
    #' Create a new Document object with optional Word and PowerPoint template
    #' filenames.
    #'
    #' @param reference_doc_filename Optional character path or filename of a
    #'   Word template used to initialize \code{reference_doc_filename}.
    #' @param reference_ppt_filename Optional character path or filename of a
    #'   PowerPoint template used to initialize \code{reference_ppt_filename}.
    #' @return A new Document object.
    initialize = function(
      reference_doc_filename = NULL,
      reference_ppt_filename = NULL
    ) {
      super$initialize()

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = "Document$initialize",
        message = private$..phr_txt("Starting initialization of Document.")
      )

      self$reference_doc_filename <- private$..default_template_filenames()
      self$reference_ppt_filename <- private$..default_ppt_template_filenames()

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = "Document$initialize",
        message = private$..phr_txt("Document initialization completed.")
      )

      invisible(self)
    },
    #' @description
    #' Generate a Word document using Quarto with template substitution.
    #'
    #' This method renders a Quarto template with specified parameters to
    #' produce a Word document output.
    #'
    #' @param output_file Character output path for the rendered document.
    #' @param template_file Character path to the Quarto template file. If NULL,
    #'   uses "quarto_doc_template.qmd" from package resources.
    #' @param params Named list of parameters to substitute in the template.
    #' @param open Logical indicating whether to open the output in a browser.
    #' @return Invisibly returns \code{self}.
    generate_quarto_doc = function(
      output_file = "document_report.docx",
      template_file = NULL,
      params = list(),
      open = FALSE
    ) {
      origin <- "Document$generate_quarto_doc"

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = origin,
        message = private$..phr_txt("Starting Quarto Word document generation to {output_file}.")
      )

      # Step 1: Resolve template path
      result <- phrutils::phr_try_step(
        {
          if (is.null(template_file)) {
            template_file <- private$..default_word_template_path()
          }

          if (!file.exists(template_file)) {
            phrutils::phr_error(
              private$..phr_txt("Quarto template file not found: {template_file}"),
              origin = origin
            )
          }

          template_file
        },
        step = private$..phr_txt("Resolving Quarto Word template"),
        hint = private$..phr_txt("Check that the template file exists.")
      )
      if (phrutils::phr_failed(result)) {
        private$..touch(log = TRUE, touch = FALSE, name = paste0(origin, "[resolve_template]"), display = TRUE, message = result$error)
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }
      template_file <- result

      # Step 2: Build parameters
      result <- phrutils::phr_try_step(
        {
          default_params <- list(
            title = "Document Report",
            author = "Author",
            date = format(Sys.Date(), "%Y-%m-%d")
          )

          all_params <- modifyList(default_params, self$get_quarto_params())
          all_params <- modifyList(all_params, params)

          # Keep only params declared in YAML
          declared_params <- names(
            rmarkdown::yaml_front_matter(template_file)$params
          )
          all_params[names(all_params) %in% declared_params]
        },
        step = private$..phr_txt("Building Quarto parameters"),
        hint = private$..phr_txt("Check the params argument and the template YAML front matter.")
      )
      if (phrutils::phr_failed(result)) {
        private$..touch(log = TRUE, touch = FALSE, name = paste0(origin, "[build_params]"), display = TRUE, message = result$error)
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }
      all_params <- result

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = paste0(origin, "[build_params]"),
        message = private$..phr_txt("Prepared {length(all_params)} Quarto parameter(s).")
      )

      # Step 3: Render and move output
      result <- phrutils::phr_try_step(
        {
          quarto::quarto_render(
            input = template_file,
            output_file = basename(output_file),
            output_format = "docx",
            execute_params = all_params
          )

          rendered_file <- file.path(
            dirname(template_file),
            basename(output_file)
          )
          if (
            normalizePath(rendered_file, mustWork = FALSE) !=
              normalizePath(output_file, mustWork = FALSE)
          ) {
            file.copy(rendered_file, output_file, overwrite = TRUE)
          }
          TRUE
        },
        step = private$..phr_txt("Rendering Quarto Word document"),
        hint = private$..phr_txt("Check that Quarto is installed and the template renders.")
      )
      if (phrutils::phr_failed(result)) {
        private$..touch(log = TRUE, touch = FALSE, name = paste0(origin, "[render]"), display = TRUE, message = result$error)
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      phrutils::phr_message(
        private$..phr_txt("Quarto document saved to: {output_file}"),
        origin = origin
      )

      if (isTRUE(open)) utils::browseURL(output_file)

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = origin,
        message = private$..phr_txt("Quarto Word document saved to {output_file}.")
      )

      invisible(self)
    },

    #' @description
    #' Generate a PowerPoint presentation using Quarto with template substitution.
    #'
    #' This method renders a Quarto template with specified parameters to
    #' produce a PowerPoint presentation output.
    #'
    #' @param output_file Character output path for the rendered presentation.
    #' @param template_file Character path to the Quarto template file. If NULL,
    #'   uses "quarto_ppt_template.qmd" from package resources.
    #' @param params Named list of parameters to substitute in the template.
    #' @param content Character string containing the main presentation content
    #'   (Markdown formatted).
    #' @param open Logical indicating whether to open the output in a browser.
    #' @return Invisibly returns \code{self}.
    generate_quarto_ppt = function(
      output_file = "presentation_report.pptx",
      template_file = NULL,
      params = list(),
      content = "",
      open = FALSE
    ) {
      origin <- "Document$generate_quarto_ppt"

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = origin,
        message = private$..phr_txt("Starting Quarto PowerPoint generation to {output_file}.")
      )

      # Step 1: Resolve and read template
      result <- phrutils::phr_try_step(
        {
          if (is.null(template_file)) {
            template_file <- system.file(
              "resources",
              "quarto_ppt_template.qmd",
              package = "phr"
            )
          }
          if (!nzchar(template_file) || !file.exists(template_file)) {
            template_file <- file.path(
              "inst",
              "resources",
              "quarto_ppt_template.qmd"
            )
          }
          if (!file.exists(template_file)) {
            phrutils::phr_error(
              private$..phr_txt("Quarto template file not found: {template_file}"),
              origin = origin
            )
          }

          list(
            template_file = template_file,
            template_text = paste(readLines(template_file), collapse = "\n")
          )
        },
        step = private$..phr_txt("Resolving Quarto PowerPoint template"),
        hint = private$..phr_txt("Check that the template file exists.")
      )
      if (phrutils::phr_failed(result)) {
        private$..touch(log = TRUE, touch = FALSE, name = paste0(origin, "[resolve_template]"), display = TRUE, message = result$error)
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }
      template_text <- result$template_text

      # Step 2: Substitute parameters and render
      result <- phrutils::phr_try_step(
        {
          default_params <- list(
            title = "Presentation Report",
            author = "Author",
            date = format(Sys.Date(), "%Y-%m-%d"),
            reference_doc = "null",
            content = ""
          )

          all_params <- modifyList(default_params, params)
          all_params$content <- content

          for (param_name in names(all_params)) {
            pattern <- paste0("\\{\\{", param_name, "\\}\\}")
            replacement <- as.character(all_params[[param_name]])
            template_text <- gsub(
              pattern,
              replacement,
              template_text,
              fixed = FALSE
            )
          }

          temp_qmd <- tempfile(fileext = ".qmd")
          writeLines(template_text, temp_qmd)

          quarto::quarto_render(
            input = temp_qmd,
            output_file = basename(output_file),
            output_format = "pptx"
          )

          rendered_file <- file.path(
            dirname(temp_qmd),
            basename(output_file)
          )
          if (
            normalizePath(rendered_file, mustWork = FALSE) !=
              normalizePath(output_file, mustWork = FALSE)
          ) {
            file.copy(rendered_file, output_file, overwrite = TRUE)
          }
          length(all_params)
        },
        step = private$..phr_txt("Rendering Quarto PowerPoint presentation"),
        hint = private$..phr_txt("Check that Quarto is installed and the template renders.")
      )
      if (phrutils::phr_failed(result)) {
        private$..touch(log = TRUE, touch = FALSE, name = paste0(origin, "[render]"), display = TRUE, message = result$error)
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = paste0(origin, "[render]"),
        message = private$..phr_txt("Substituted {result} template parameter(s) and rendered presentation.")
      )

      phrutils::phr_message(
        private$..phr_txt("Quarto presentation saved to: {output_file}"),
        origin = origin
      )

      if (isTRUE(open)) utils::browseURL(output_file)

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = origin,
        message = private$..phr_txt("Quarto presentation saved to {output_file}.")
      )

      invisible(self)
    },
    #' @description Get Quarto parameters for rendering.
    #' @return A named list of parameters to pass to Quarto rendering.
    get_quarto_params = function() {
      list(
        r_version = self$.r_version
      )
    }
  ),
  active = list(
    #' @field .r_version Active binding returning the current R version string.
    .r_version = function(value) {
      version$version.string
    }
  ),

  private = list(
    # @description
    # Resolve the default Quarto Word template path.
    #
    # @return Character scalar giving the resolved path to the default Word
    #   Quarto template.
    #
    # @keywords internal
    ..default_word_template_path = function() {
      template_file <- "quarto_doc_revised_template.qmd"

      template_path <- system.file(
        "resources",
        template_file,
        package = "phr"
      )

      if (!nzchar(template_path) || !file.exists(template_path)) {
        template_path <- file.path(
          "inst",
          "resources",
          template_file
        )
      }

      template_path
    },
    # @description Return default template filename candidates.
    # @return Character vector of template filenames.
    # @keywords internal
    ..default_template_filenames = function() {
      c("reach_tor_template.docx", "protocol_report_template.docx")
    },

    # @description Return default PowerPoint template filename candidates.
    # @return Character vector of template filenames.
    # @keywords internal
    ..default_ppt_template_filenames = function() {
      c("protocol_report_template.pptx")
    },
    # @description Sanitize a data frame for Quarto rendering by replacing
    # missing values with type-appropriate defaults.
    # @param df A data frame to sanitize.
    # @return A sanitized data frame, or the input unchanged if it is not a data frame.
    # @keywords internal
    ..sanitize_quarto_df = function(df) {
      if (!is.data.frame(df)) {
        return(df)
      }

      df[] <- lapply(df, function(x) {
        if (is.character(x)) {
          x[is.na(x)] <- ""
        } else if (is.numeric(x) || is.integer(x)) {
          x[is.na(x)] <- 0
        } else if (is.logical(x)) {
          x[is.na(x)] <- FALSE
        }
        x
      })

      df
    }
  )
)
