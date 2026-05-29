#' Orchestrator R6 Class
#'
#' @description
#' Base orchestration class that provides generalized nested-object access and
#' synchronization hooks.
#'
#' @importFrom R6 R6Class
Orchestrator <- R6::R6Class(
  "Orchestrator",
  public = list(
    #' @field metadata List containing orchestration metadata.
    metadata = list(
      created_datetime = NULL,
      modified_datetime = NULL
    ),

    #' @description
    #' Creates a new Orchestrator object.
    #' @return A new Orchestrator object.
    initialize = function() {
      self$metadata$created_datetime <- Sys.time()
      self$metadata$modified_datetime <- Sys.time()
      invisible(self)
    },

    #' @description
    #' Generalized accessor for nested objects stored on the class.
    #'
    #' Resolves a top-level field (for example \code{tools} or \code{framework}),
    #' optionally resolves a named element if the field is a list, and then
    #' returns a nested field or invokes a nested method.
    #'
    #' @param field Character scalar naming the stored top-level field.
    #' @param name Optional character scalar naming a list element in
    #'   \code{field}.
    #' @param member Optional character scalar naming a field or method on the
    #'   resolved object.
    #' @param role Optional character scalar used to resolve a list element in
    #'   \code{field} by role-like name (for example \code{"household"} for
    #'   \code{"tool_household_iphra_v2"}).
    #' @param ... Arguments passed to the nested method when \code{member}
    #'   resolves to a function.
    #' @return The requested nested value, or the nested method result.
    access_nested = function(field, name = NULL, member = NULL, role = NULL, ...) {
      phr_try({
        target <- private$.resolve_nested_target(field = field, name = name, role = role)

        if (is.null(member)) {
          private$sync_state()
          private$touch()
          return(target)
        }

        phr_assert(
          is.character(member) && length(member) == 1L && nzchar(member),
          message = phr_txt("member must be a non-empty character string."),
          origin = "Orchestrator$access_nested"
        )
        phr_assert(
          !is.null(target[[member]]),
          message = phr_txt("Member '{member}' does not exist on the resolved target."),
          origin = "Orchestrator$access_nested"
        )

        value <- target[[member]]
        out <- if (is.function(value)) {
          do.call(value, list(...))
        } else {
          value
        }

        private$sync_state()
        private$touch()
        out
      }, on_error = "abort", origin = "Orchestrator$access_nested")
    },

    #' @description
    #' Generalized nested field setter.
    #'
    #' Resolves a top-level field (optionally a named list element), sets
    #' \code{member} to \code{value}, then calls synchronization hooks and
    #' updates the modified timestamp.
    #'
    #' @param field Character scalar naming the stored top-level field.
    #' @param member Character scalar naming a writable field in the resolved
    #'   nested object.
    #' @param value Value to assign.
    #' @param name Optional character scalar naming a list element in
    #'   \code{field}.
    #' @return Invisibly returns \code{self}.
    #' @param role Optional character scalar used to resolve a list element in
    #'   \code{field} by role-like name.
    set_nested = function(field, member, value, name = NULL, role = NULL) {
      phr_try({
        phr_assert(
          is.character(member) && length(member) == 1L && nzchar(member),
          message = phr_txt("member must be a non-empty character string."),
          origin = "Orchestrator$set_nested"
        )
        target <- private$.resolve_nested_target(field = field, name = name, role = role)
        target[[member]] <- value
        private$sync_state()
        private$touch()
      }, on_error = "abort", origin = "Orchestrator$set_nested")
      invisible(self)
    }
  ),

  private = list(
    touch = function() {
      if (is.null(self$metadata) || !is.list(self$metadata)) {
        self$metadata <- list()
      }
      self$metadata$modified_datetime <- Sys.time()
      invisible(NULL)
    },

    sync_state = function() {
      sync_names <- grep("^sync_", names(self), value = TRUE)
      if (length(sync_names) == 0L) return(invisible(NULL))
      for (nm in sync_names) {
        val <- tryCatch(self[[nm]], error = function(e) NULL)
        if (is.function(val)) {
          tryCatch(val(), error = function(e) NULL)
        }
      }
      invisible(NULL)
    },

    .resolve_nested_target = function(field, name = NULL, role = NULL) {
      phr_assert(
        is.character(field) && length(field) == 1L && nzchar(field),
        message = phr_txt("field must be a non-empty character string."),
        origin = "Orchestrator$.resolve_nested_target"
      )
      phr_assert(
        !is.null(self[[field]]),
        message = phr_txt("Field '{field}' is not available on this object."),
        origin = "Orchestrator$.resolve_nested_target"
      )
      container <- self[[field]]

      phr_assert(
        !( !is.null(name) && !is.null(role) ),
        message = phr_txt("Provide only one of name or role."),
        origin = "Orchestrator$.resolve_nested_target"
      )

      if (is.null(name) && is.null(role)) return(container)

      phr_assert(
        is.list(container),
        message = phr_txt("Field '{field}' must be a list when resolving name/role."),
        origin = "Orchestrator$.resolve_nested_target"
      )

      if (!is.null(name)) {
        phr_assert(
          is.character(name) && length(name) == 1L && nzchar(name),
          message = phr_txt("name must be a non-empty character string when provided."),
          origin = "Orchestrator$.resolve_nested_target"
        )
        phr_assert(
          !is.null(container[[name]]),
          message = phr_txt("Name '{name}' was not found in field '{field}'."),
          origin = "Orchestrator$.resolve_nested_target"
        )
        return(container[[name]])
      }

      phr_assert(
        is.character(role) && length(role) == 1L && nzchar(role),
        message = phr_txt("role must be a non-empty character string when provided."),
        origin = "Orchestrator$.resolve_nested_target"
      )

      nms <- names(container)
      phr_assert(
        !is.null(nms) && length(nms) > 0L,
        message = phr_txt("Field '{field}' has no named elements for role-based lookup."),
        origin = "Orchestrator$.resolve_nested_target"
      )

      role_key <- private$.normalize_role_name(role)
      normalized_names <- vapply(nms, private$.normalize_role_name, character(1L))
      idx <- which(normalized_names == role_key)

      if (length(idx) == 0L) {
        idx <- grep(paste0("^", role_key, "$|_", role_key, "_|_", role_key, "$"), normalized_names)
      }

      phr_assert(
        length(idx) == 1L,
        message = if (length(idx) == 0L) {
          phr_txt("Role '{role}' was not found in field '{field}'.")
        } else {
          phr_txt("Role '{role}' matched multiple elements in field '{field}'.")
        },
        origin = "Orchestrator$.resolve_nested_target"
      )
      container[[idx]]
    },

    .normalize_role_name = function(x) {
      x <- tolower(as.character(x %||% ""))
      x <- gsub("^tool_", "", x)
      x <- gsub("_iphra(_v[0-9]+)?$", "", x)
      x <- gsub("_v[0-9]+$", "", x)
      x
    }
  )
)
