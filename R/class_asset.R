#' Asset R6 Class
#'
#' @description
#' Base class providing core, reusable functionality for R6 classes in this
#' package: private metadata (including a content-based \code{hash_id}
#' fingerprint and a \code{version} change counter), generalized
#' nested-object access (\code{get()}/\code{call()}), a generalized field
#' setter (\code{set()}), and synchronization hooks. Metadata is retrieved
#' via the \code{metadata} active binding or \code{get(field = "metadata")}.
#'
#' \code{Tool}, \code{Document}, \code{Framework}, and \code{Sample} (and any
#' of their subclasses) inherit from \code{Asset}.
#'
#' @importFrom R6 R6Class
#' @export
Asset <- R6::R6Class(
  "Asset",
  public = list(
    #' @description
    #' Creates a new Asset object.
    #' @return A new Asset object.
    initialize = function() {
      timestamp <- Sys.time()
      private$..metadata <- list(
        created_datetime = timestamp,
        modified_datetime = timestamp,
        version = 0L,
        hash_id = private$..compute_hash_id()
      )
      private$..touch()
      invisible(self)
    },

    #' @description Hook executed before \code{sync_state()} logic.
    #' @param field Optional top-level field name.
    #' @param member Optional nested member name.
    #' @param target_field Optional destination field path.
    #' @param name Optional named list entry inside \code{field}.
    #' @param role Optional role-based list resolution key.
    #' @return Invisibly returns \code{NULL}.
    pre_sync_state = function(
      field = NULL,
      member = NULL,
      target_field = NULL,
      name = NULL,
      role = NULL
    ) {
      invisible(NULL)
    },

    #' @description Hook executed after \code{sync_state()} logic.
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
      invisible(NULL)
    },

    #' @description Synchronize asset state.
    #' @param field Optional top-level field name.
    #' @param member Optional nested member name.
    #' @param target_field Optional destination field path.
    #' @param name Optional named list entry inside \code{field}.
    #' @param role Optional role-based list resolution key.
    #' @return Invisibly returns \code{self}.
    sync_state = function(
      field = NULL,
      member = NULL,
      target_field = NULL,
      name = NULL,
      role = NULL
    ) {
      private$..sync_state(
        field = field,
        member = member,
        target_field = target_field,
        name = name,
        role = role
      )
      invisible(self)
    },

    #' @description
    #' Generalized accessor for field/member values stored on this object or
    #' on a nested object.
    #'
    #' Resolves a top-level field (public or private; for example
    #' \code{tools} or \code{framework}), optionally resolves a named or
    #' role-matched 1st level list element (and, with \code{role2}, a 2nd
    #' level role-matched element nested within it), and returns a field or
    #' list member value. Unlike \code{call()}, \code{get()} refuses to
    #' return a function; use \code{call()} to invoke nested methods.
    #'
    #' @param field Character scalar naming a public or private top-level
    #'   field on this object.
    #' @param name Optional character scalar naming a list element in
    #'   \code{field} by exact name. Cannot be combined with \code{role}.
    #' @param member Optional character scalar naming a public or private
    #'   field on the resolved target. When \code{NULL}, the resolved target
    #'   itself is returned.
    #' @param role Optional character scalar used to resolve a 1st level list
    #'   element in \code{field} by role-like name (for example
    #'   \code{"household"} for \code{"tool_household_iphra_v2"}). Cannot be
    #'   combined with \code{name}.
    #' @param role2 Optional character scalar used to resolve a 2nd level list
    #'   element nested within the \code{name}/\code{role}-resolved element.
    #' @param update_sync Logical indicating whether to synchronize state after access.
    #' @param update_modified Logical indicating whether to update the modified timestamp.
    #' @return The requested field or member value.
    get = function(
      field,
      name = NULL,
      member = NULL,
      role = NULL,
      role2 = NULL,
      update_sync = FALSE,
      update_modified = FALSE
    ) {
      phrutils::phr_try(
        {
          target <- private$..resolve_nested_target(
            field = field,
            name = name,
            role = role,
            role2 = role2
          )

          out <- if (is.null(member)) {
            target
          } else {
            phrutils::phr_assert(
              is.character(member) && length(member) == 1L && nzchar(member),
              message = phr_txt("member must be a non-empty character string."),
              origin = "Asset$get"
            )
            private$..resolve_member_value(target, member)
          }

          phrutils::phr_assert(
            !is.function(out),
            message = phr_txt(
              "Member '{member}' resolves to a function; use call() instead of get()."
            ),
            origin = "Asset$get"
          )

          if (update_sync) {
            private$..sync_state()
          }
          if (update_modified) {
            private$..touch()
          }
          out
        },
        on_error = "abort",
        origin = "Asset$get"
      )
    },

    #' @description
    #' Generalized invoker for methods stored on this object or on a nested
    #' object.
    #'
    #' Resolves a top-level field (public or private), optionally resolves a
    #' named or role-matched 1st level list element (and, with \code{role2},
    #' a 2nd level role-matched element nested within it), and invokes
    #' \code{member} as a function on the resolved target. Unlike
    #' \code{get()}, \code{call()} requires \code{member} to resolve to a
    #' function.
    #'
    #' @param field Character scalar naming a public or private top-level
    #'   field on this object.
    #' @param name Optional character scalar naming a list element in
    #'   \code{field} by exact name. Cannot be combined with \code{role}.
    #' @param member Optional character scalar naming a method on the
    #'   resolved target. When \code{NULL}, the resolved target itself is
    #'   invoked as a function.
    #' @param role Optional character scalar used to resolve a 1st level list
    #'   element in \code{field} by role-like name. Cannot be combined with
    #'   \code{name}.
    #' @param role2 Optional character scalar used to resolve a 2nd level list
    #'   element nested within the \code{name}/\code{role}-resolved element.
    #' @param update_sync Logical indicating whether to synchronize state after the call.
    #' @param update_modified Logical indicating whether to update the modified timestamp.
    #' @param ... Arguments passed to the resolved method.
    #' @return The result of invoking the resolved method.
    call = function(
      field,
      name = NULL,
      member = NULL,
      role = NULL,
      role2 = NULL,
      update_sync = FALSE,
      update_modified = TRUE,
      ...
    ) {
      phrutils::phr_try(
        {
          target <- private$..resolve_nested_target(
            field = field,
            name = name,
            role = role,
            role2 = role2
          )

          value <- if (is.null(member)) {
            target
          } else {
            phrutils::phr_assert(
              is.character(member) && length(member) == 1L && nzchar(member),
              message = phr_txt("member must be a non-empty character string."),
              origin = "Asset$call"
            )
            private$..resolve_member_value(target, member)
          }

          phrutils::phr_assert(
            is.function(value),
            message = phr_txt(
              "Member '{member}' does not resolve to a function; use get() instead of call()."
            ),
            origin = "Asset$call"
          )

          out <- do.call(value, list(...))

          if (update_sync) {
            private$..sync_state()
          }
          if (update_modified) {
            private$..touch()
          }
          out
        },
        on_error = "abort",
        origin = "Asset$call"
      )
    },

    #' @description
    #' Generalized, scope-safe field setter.
    #'
    #' Mirrors \code{get()}'s \code{field}/\code{role}/\code{role2}/
    #' \code{member} arguments, but safely writes \code{value} instead of
    #' reading. \code{field} may resolve to either a public or a private
    #' field, and \code{member} is optional: when omitted, \code{value}
    #' replaces the resolved top-level (or role/role2-resolved) target
    #' directly. If the \code{role} (and/or \code{role2}) list element does
    #' not yet exist, it is created (using \code{role}/\code{role2} as the
    #' literal new element name) rather than raising an error, so that
    #' \code{set()} can be used to populate a new list or nested list
    #' element. Writing to a resolved member or target that currently holds
    #' a function is rejected, to avoid accidentally clobbering methods. When
    #' the resolved target is itself an R6 object exposing its own
    #' \code{set()} method (for example a nested \code{Asset} or \code{Log}),
    #' \code{member}/\code{value} are delegated to that object's
    #' \code{set(field, value)} instead of being written directly into its
    #' environment, so private members and validation on the nested object
    #' are respected.
    #'
    #' @param field Character scalar naming a public or private top-level
    #'   field on this object.
    #' @param value Value to assign.
    #' @param member Optional character scalar naming a writable field on the
    #'   resolved target. When \code{NULL}, \code{value} is assigned directly
    #'   to the resolved target.
    #' @param role Optional character scalar used to resolve (or create) a
    #'   1st level list element in \code{field} by role-like name.
    #' @param role2 Optional character scalar used to resolve (or create) a
    #'   2nd level list element nested within the \code{role}-resolved
    #'   element. Cannot be supplied without \code{role}.
    #' @param update_sync Logical indicating whether to synchronize state
    #'   after the assignment.
    #' @param update_modified Logical indicating whether to update the
    #'   modified timestamp.
    #' @return Invisibly returns \code{self}.
    set = function(
      field,
      value,
      member = NULL,
      role = NULL,
      role2 = NULL,
      update_sync = FALSE,
      update_modified = TRUE
    ) {
      phrutils::phr_try(
        {
          phrutils::phr_assert(
            !(is.null(role) && !is.null(role2)),
            message = phrutils::phr_txt(
              "Cannot provide 2nd level list element (role2) without a 1st level list element (role)."
            ),
            origin = "Asset$set"
          )

          # Handles whether field is a public or private and returns the field.
          resolved <- private$..resolve_field_scope(
            field = field,
            origin = "Asset$set"
          )
          container <- resolved$value

          key <- NULL
          key2 <- NULL
          # `level1` holds the (possibly newly created) 1st level list
          # element resolved by `role`, before any `role2` resolution.
          level1 <- NULL

          if (!is.null(role)) {

            key <- private$..resolve_list_key(
              container = container,
              field = field,
              role = role,
              create_if_missing = TRUE,
              origin = "Asset$set"
            )

            level1 <- if (is.null(container)) NULL else container[[key]]

            if (!is.null(role2)) {
              phrutils::phr_assert(
                is.null(level1) || is.list(level1),
                message = phr_txt(
                  "role2 can only be resolved from a list returned by role."
                ),
                origin = "Asset$set"
              )

              key2 <- private$..resolve_list_key(
                container = level1,
                field = field,
                role = role2,
                create_if_missing = TRUE,
                origin = "Asset$set"
              )
            }
          }

          target <- if (!is.null(role) && !is.null(role2)) {
            if (is.null(level1)) NULL else level1[[key2]]
          } else if (!is.null(role)) {
            level1
          } else {
            container
          }

          if (is.null(member)) {
            phrutils::phr_assert(
              !is.function(target),
              message = phr_txt(
                "Refusing to overwrite function member '{field}'."
              ),
              origin = "Asset$set"
            )
            new_target <- value
          } else {
            phrutils::phr_assert(
              is.character(member) && length(member) == 1L && nzchar(member),
              message = phr_txt("member must be a non-empty character string."),
              origin = "Asset$set"
            )

            # Auto-vivify: a `role`/`role2`-resolved target that does not
            # yet exist is NULL; treat it as a new, empty list so that a
            # member can be set on it instead of erroring.
            if (is.null(target)) {
              target <- list()
            }

            phrutils::phr_assert(
              is.list(target) || is.environment(target),
              message = phr_txt(
                "Resolved target for field '{field}' must be a list or environment to set member '{member}'."
              ),
              origin = "Asset$set"
            )

            if (
              is.environment(target) &&
                !is.null(target$.__enclos_env__) &&
                is.function(target$set)
            ) {
              # `target` is itself an R6 object (e.g. a nested Asset/Log)
              # exposing its own `set()` API. Delegate to it instead of
              # writing directly into its environment: `member` may resolve
              # to a private field, and direct assignment would either
              # bypass the nested object's own validation or fail outright
              # by attempting to add a new binding to a locked environment.
              phrutils::phr_assert(
                !is.function(private$..resolve_member_value(target, member)),
                message = phr_txt(
                  "Refusing to overwrite function member '{member}'."
                ),
                origin = "Asset$set"
              )
              target$set(field = member, value = value)
            } else {
              phrutils::phr_assert(
                !is.function(target[[member]]),
                message = phr_txt(
                  "Refusing to overwrite function member '{member}'."
                ),
                origin = "Asset$set"
              )
              target[[member]] <- value
            }
            new_target <- target
          }

          if (is.null(role)) {

            container <- new_target

          } else if (is.null(role2)) {

            container[[key]] <- new_target

          } else {

            level1[[key2]] <- new_target
            container[[key]] <- level1
          }

          private$..assign_field_scope(
            scope = resolved$scope,
            field = field,
            value = container
          )

          if (update_sync) {
            private$..sync_state()
          }
          if (update_modified) {
            private$..touch()
          }
        },
        on_error = "abort",
        origin = "Asset$set"
      )
      invisible(self)
    }
  ),

  private = list(
    # @field ..metadata List containing private asset metadata, including
    #   `created_datetime`, `modified_datetime`, `version`, and `hash_id`.
    # @keywords internal
    ..metadata = list(
      created_datetime = NULL,
      modified_datetime = NULL,
      version = 0L,
      hash_id = NA_character_
    ),

    # @field ..change_log Data frame log of changes made to this object, with
    #   columns `id`, `version`, `name`, `datetime`, and `log_message`.
    # @keywords internal
    ..change_log = NULL,

    # @description Update modified timestamp, bump the version counter, and
    #   recompute the hash_id fingerprint metadata.
    # @param log Logical indicating whether to also append a row to the
    #   change log via \code{..track()}. Defaults to \code{FALSE}.
    # @param name Optional character scalar identifying the change, passed
    #   through to \code{..track()} when \code{log} is \code{TRUE}.
    # @param message Optional character scalar describing the change, passed
    #   through to \code{..track()} when \code{log} is \code{TRUE}.
    # @return Invisibly returns \code{NULL}.
    # @keywords internal
    ..touch = function(log = FALSE, name = NULL, message = NULL) {
      if (is.null(private$..metadata) || !is.list(private$..metadata)) {
        private$..metadata <- list()
      }
      private$..metadata$modified_datetime <- Sys.time()
      private$..metadata$version <- (private$..metadata$version %||% 0L) + 1L

      if (isTRUE(log)) {
        private$..track(name = name, message = message)
      }

      invisible(NULL)
    },

    # @description Append a row to the private change log, recording the
    #   current `hash_id`/`version` metadata alongside a name and message
    #   describing the change.
    # @param name Character scalar identifying the change (for example, the
    #   field or method that was updated).
    # @param message Character scalar describing the change in more detail.
    # @return Invisibly returns \code{NULL}.
    # @keywords internal
    ..track = function(name = NULL, message = NULL) {
      entry <- tibble::tibble(
        id = private$..metadata$hash_id %||% NA_character_,
        version = private$..metadata$version %||% NA_integer_,
        name = name %||% NA_character_,
        datetime = Sys.time(),
        log_message = message %||% NA_character_
      )

      private$..change_log <- if (is.null(private$..change_log)) {
        entry
      } else {
        dplyr::bind_rows(private$..change_log, entry)
      }

      invisible(NULL)
    },

    # @description Compute a content-based hash fingerprint of the object's
    #   current public state (excluding functions, environments, and
    #   metadata itself).
    # @return Character scalar hash, or NA if the digest package is
    #   unavailable.
    # @keywords internal
    ..compute_hash_id = function() {
      if (!requireNamespace("digest", quietly = TRUE)) {
        return(NA_character_)
      }
      nms <- setdiff(names(self), c("metadata", "clone"))
      nms <- Filter(function(nm) !isTRUE(bindingIsActive(nm, self)), nms)
      snapshot <- list()
      for (nm in nms) {
        val <- tryCatch(self[[nm]], error = function(e) NULL)
        if (is.function(val) || is.environment(val)) {
          next
        }
        snapshot[[nm]] <- val
      }
      digest::digest(snapshot, algo = "md5")
    },

    # @description Synchronize asset state.
    #
    # When \code{field/member} are provided, returns the resolved nested value
    # and optionally assigns it to \code{target_field}. Without arguments, this
    # runs inherited synchronization hooks (\code{sync_*} members).
    # @param field Optional top-level field name.
    # @param member Optional nested member name.
    # @param target_field Optional destination field path (supports \code{$}).
    # @param name Optional named list entry inside \code{field}.
    # @param role Optional role-based list resolution key.
    # @return Invisibly returns resolved value (targeted mode) or \code{NULL}.
    # @keywords internal
    ..sync_state = function(
      field = NULL,
      member = NULL,
      target_field = NULL,
      name = NULL,
      role = NULL
    ) {
      self$pre_sync_state(
        field = field,
        member = member,
        target_field = target_field,
        name = name,
        role = role
      )
      if (
        !is.null(field) ||
          !is.null(member) ||
          !is.null(target_field) ||
          !is.null(name) ||
          !is.null(role)
      ) {
        phrutils::phr_assert(
          is.character(field) && length(field) == 1L && nzchar(field),
          message = phr_txt("field must be a non-empty character string."),
          origin = "Asset$sync_state"
        )
        phrutils::phr_assert(
          is.character(member) && length(member) == 1L && nzchar(member),
          message = phr_txt("member must be a non-empty character string."),
          origin = "Asset$sync_state"
        )
        target <- private$..resolve_nested_target(
          field = field,
          name = name,
          role = role
        )
        phrutils::phr_assert(
          !is.null(target[[member]]),
          message = phr_txt(
            "Member '{member}' does not exist on the resolved target."
          ),
          origin = "Asset$sync_state"
        )
        value <- target[[member]]
        if (is.function(value)) {
          value <- value()
        }
        if (!is.null(target_field)) {
          private$..assign_sync_value(
            target_field = target_field,
            value = value
          )
        }
        self$post_sync_state(
          field = field,
          member = member,
          target_field = target_field,
          name = name,
          role = role
        )
        return(invisible(value))
      }

      # Backward compatibility: support older subclasses that still define
      # synchronize_state(), then fall back to sync_* members.
      sync_runner <- tryCatch(self$synchronize_state, error = function(e) NULL)
      if (is.function(sync_runner)) {
        sync_runner()
        self$post_sync_state(
          field = field,
          member = member,
          target_field = target_field,
          name = name,
          role = role
        )
        return(invisible(NULL))
      }

      sync_names <- setdiff(
        grep("^sync_", names(self), value = TRUE),
        "sync_state"
      )
      if (length(sync_names) == 0L) {
        self$post_sync_state(
          field = field,
          member = member,
          target_field = target_field,
          name = name,
          role = role
        )
        return(invisible(NULL))
      }
      for (nm in sync_names) {
        val <- tryCatch(self[[nm]], error = function(e) NULL)
        if (is.function(val)) {
          tryCatch(val(), error = function(e) NULL)
        }
      }
      self$post_sync_state(
        field = field,
        member = member,
        target_field = target_field,
        name = name,
        role = role
      )
      invisible(NULL)
    },

    # @description Resolve a top-level or nested target object.
    # @param field Top-level field name.
    # @param name Optional exact list element name. Cannot be combined with
    #   `role`.
    # @param role Optional role-style key for fuzzy list lookup. Cannot be
    #   combined with `name`.
    # @param role2 Optional role-style key for a 2nd level list lookup,
    #   nested within the `name`/`role`-resolved element.
    # @return Resolved object.
    # @keywords internal
    ..resolve_nested_target = function(
      field,
      name = NULL,
      role = NULL,
      role2 = NULL
    ) {
      resolved <- private$..resolve_field_scope(
        field = field,
        origin = "Asset$..resolve_nested_target"
      )
      target <- resolved$value

      phrutils::phr_assert(
        !(!is.null(name) && !is.null(role)),
        message = phrutils::phr_txt(
          "Cannot provide both name and role; they are alternative ways to resolve a 1st level list element."
        ),
        origin = "Asset$..resolve_nested_target"
      )

      phrutils::phr_assert(
        !(is.null(name) && is.null(role) && !is.null(role2)),
        message = phrutils::phr_txt("Cannot provide 2nd level list element (role2) without a 1st level list element (name or role)."),
        origin = "Asset$.resolve_nested_target"
      )

      if (is.null(name) && is.null(role) && is.null(role2)) {
        return(target)
      }

      if (!is.null(name) || !is.null(role)) {
        key <- private$..resolve_list_key(
          container = target,
          field = field,
          name = name,
          role = role,
          origin = "Asset$.resolve_nested_target"
        )
        target <- target[[key]]
      }

      if (!is.null(role2)) {
        phrutils::phr_assert(
          is.list(target),
          message = phrutils::phr_txt(
            "role2 can only be resolved from a list returned by name/role."
          ),
          origin = "Asset$..resolve_nested_target"
        )
        target <- target[[role2]]
      }

      target

    },

    # @description Resolve a public or private member value on a resolved
    #   target, supporting both plain lists and R6 objects (whose private
    #   fields are reached via `.__enclos_env__$private`).
    # @param target A list or R6 object to resolve `member` on.
    # @param member Character scalar naming the field/method to resolve.
    # @return The resolved value, or \code{NULL} if not found.
    # @keywords internal
    ..resolve_member_value = function(target, member) {
      if (is.environment(target) && !is.null(target$.__enclos_env__)) {
        value <- target[[member]]
        if (is.null(value)) {
          priv <- target$.__enclos_env__$private
          if (!is.null(priv)) {
            value <- priv[[member]]
          }
        }
        return(value)
      }
      target[[member]]
    },

    # @description Resolve the list index/name identifying an element within
    #   \code{container}, using either an exact \code{name} or a role-like
    #   \code{role} lookup. Shared by \code{..resolve_nested_target()} and
    #   \code{set()}. When \code{create_if_missing} is \code{TRUE},
    #   \code{container} may be \code{NULL} or an empty/unnamed list (e.g.
    #   when creating the very first element of a new list field), and a
    #   missing \code{name}/\code{role} returns the literal \code{name}/
    #   \code{role} string as the new element's key instead of raising an
    #   error.
    # @param container A list to search within (or \code{NULL} when the
    #   field has not yet been initialized as a list).
    # @param field Top-level field name (used only for error messages).
    # @param name Optional exact list element name. Cannot be combined with
    #   \code{role}.
    # @param role Optional role-style key for list lookup. Cannot be
    #   combined with \code{name}.
    # @param create_if_missing Logical; when \code{TRUE}, an unresolved
    #   \code{name}/\code{role} returns the literal key to create instead of
    #   raising an error.
    # @param origin Character scalar identifying the calling context for
    #   error messages.
    # @return The resolved list key (character name or integer index).
    # @keywords internal
    ..resolve_list_key = function(
      container,
      field,
      name = NULL,
      role = NULL,
      create_if_missing = FALSE,
      origin = "Asset$..resolve_list_key"
    ) {
      phrutils::phr_assert(
        !(is.null(name) && is.null(role)) && !(!is.null(name) && !is.null(role)),
        message = phrutils::phr_txt(
          "Exactly one of name or role must be provided."
        ),
        origin = origin
      )

      phrutils::phr_assert(
        is.null(container) || is.list(container),
        message = phrutils::phr_txt(
          "Field '{field}' must be a list when resolving name/role."
        ),
        origin = origin
      )

      nms <- if (is.null(container)) NULL else names(container)

      if (!is.null(name)) {
        phrutils::phr_assert(
          is.character(name) && length(name) == 1L && nzchar(name),
          message = phrutils::phr_txt(
            "name must be a non-empty character string when provided."
          ),
          origin = origin
        )

        if (!is.null(nms) && name %in% nms) {
          return(name)
        }

        if (isTRUE(create_if_missing)) {
          return(name)
        }

        phrutils::phr_assert(
          FALSE,
          message = phrutils::phr_txt(
            "Name '{name}' was not found in field '{field}'."
          ),
          origin = origin
        )
      }

      phrutils::phr_assert(
        is.character(role) && length(role) == 1L && nzchar(role),
        message = phrutils::phr_txt(
          "role must be a non-empty character string when provided."
        ),
        origin = origin
      )

      if (is.null(nms) || length(nms) == 0L) {
        if (isTRUE(create_if_missing)) {
          return(role)
        }
        phrutils::phr_assert(
          FALSE,
          message = phrutils::phr_txt(
            "Field '{field}' has no named elements for role-based lookup."
          ),
          origin = origin
        )
      }

      role_key <- private$..normalize_role_name(role)

      normalized_names <- vapply(
        nms,
        private$..normalize_role_name,
        character(1L)
      )
      idx <- which(normalized_names == role_key)

      if (length(idx) == 0L) {
        idx <- grep(
          paste0("^", role_key, "$|_", role_key, "_|_", role_key, "$"),
          normalized_names
        )
      }

      if (length(idx) == 1L) {
        return(idx)
      }

      phrutils::phr_assert(
        !(length(idx) > 1L),
        message = phr_txt(
          "Role '{role}' matched multiple elements in field '{field}'."
        ),
        origin = origin
      )

      if (isTRUE(create_if_missing)) {
        return(role)
      }

      phrutils::phr_assert(
        FALSE,
        message = phrutils::phr_txt(
          "Role '{role}' was not found in field '{field}'."
        ),
        origin = origin
      )

    },

    # @description Resolve which scope ("public" or "private") owns a
    #   top-level field, so that `get()`/`call()`/`set()` can safely
    #   read/write fields regardless of visibility.
    # @param field Character scalar naming the field to resolve.
    # @param origin Character scalar identifying the calling context for
    #   error messages.
    # @return A list with `scope` ("public" or "private") and `value` (the
    #   field's current value).
    # @keywords internal
    ..resolve_field_scope = function(field, origin = "Asset$set") {
      phrutils::phr_assert(
        is.character(field) && length(field) == 1L && nzchar(field),
        message = phrutils::phr_txt("field must be a non-empty character string."),
        origin = origin
      )

      if (field %in% names(self)) {
        return(list(scope = "public", value = self[[field]]))
      }
      if (field %in% names(private)) {
        return(list(scope = "private", value = private[[field]]))
      }

      phrutils::phr_assert(
        FALSE,
        message = phrutils::phr_txt(
          "Field '{field}' is not available on this object."
        ),
        origin = origin
      )
    },

    # @description Assign `value` back to a field in the scope identified by
    #   `..resolve_field_scope()`.
    # @param scope Either "public" or "private".
    # @param field Character scalar naming the field to assign.
    # @param value Value to assign.
    # @return Invisibly returns \code{NULL}.
    # @keywords internal
    ..assign_field_scope = function(scope, field, value) {
      if (identical(scope, "private")) {
        private[[field]] <- value
      } else {
        self[[field]] <- value
      }
      invisible(NULL)
    },

    # @description Normalize role names for fuzzy list matching.
    # @param x Character role/name input.
    # @return Normalized character key.
    # @keywords internal
    ..normalize_role_name = function(x) {
      x <- tolower(as.character(x %||% ""))
      x <- gsub("^tool_", "", x)
      x <- gsub("_iphra(_v[0-9]+)?$", "", x)
      x <- gsub("_v[0-9]+$", "", x)
      x
    },

    # @description Assign synchronized values to a target field path.
    # @param target_field Character path using \code{$} separators.
    # @param value Value to assign.
    # @return Invisibly returns \code{NULL}.
    # @keywords internal
    ..assign_sync_value = function(target_field, value) {
      phrutils::phr_assert(
        is.character(target_field) &&
          length(target_field) == 1L &&
          nzchar(target_field),
        message = phr_txt("target_field must be a non-empty character string."),
        origin = "Asset$.assign_sync_value"
      )

      path <- strsplit(target_field, "\\$", fixed = FALSE)[[1L]]
      path <- path[nzchar(path)]
      phrutils::phr_assert(
        length(path) >= 1L,
        message = phr_txt("target_field path is invalid."),
        origin = "Asset$.assign_sync_value"
      )

      if (length(path) == 1L) {
        self[[path[[1L]]]] <- value
        return(invisible(NULL))
      }

      set_path <- function(x, keys, val) {
        if (length(keys) == 1L) {
          x[[keys[[1L]]]] <- val
          return(x)
        }
        key <- keys[[1L]]
        next_val <- x[[key]]
        if (
          is.null(next_val) || (!is.list(next_val) && !is.environment(next_val))
        ) {
          next_val <- list()
        }
        x[[key]] <- set_path(next_val, keys[-1L], val)
        x
      }

      root_name <- path[[1L]]
      root <- self[[root_name]]
      if (is.null(root) || (!is.list(root) && !is.environment(root))) {
        root <- list()
      }
      self[[root_name]] <- set_path(root, path[-1L], value)
      invisible(NULL)
    }
  )
)
