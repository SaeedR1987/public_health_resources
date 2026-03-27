`%||%` <- function(x, y) if (is.null(x)) y else x

#' #' @title IPHRA Shiny-Aware Error and Warning Handling (with session logs)
#' #' @description
#' #' Provides standardized, Shiny-aware functions for emitting and logging
#' #' errors, warnings, and informational messages in a user-friendly way.
#' #' Logs are stored in a session-scoped reactive value (via `reactiveVal`)
#' #' and can be accessed reactively with `iphra_get_logs()`.
#' #' Logs vanish automatically when the Shiny session ends.
#' #'
#' #' @keywords internal
#'
#'
#' # ---- Reactive logging store ----------------------------------------
#'
#' #' @title Initialize an Empty IPHRA Log Store
#' #' @description
#' #' Creates a new Shiny `reactiveVal` object containing an empty
#' #' data frame with standard IPHRA log columns:
#' #' `time`, `level`, `origin`, and `message`.
#' #' Used internally to initialize session-scoped logging.
#' #'
#' #' @return A `reactiveVal` containing an empty log data frame.
#' #' @keywords internal
#' iphra_init_log <- function() {
#'   shiny::reactiveVal(
#'     data.frame(
#'       time = character(),
#'       level = character(),
#'       origin = character(),
#'       message = character(),
#'       stringsAsFactors = FALSE
#'     )
#'   )
#' }
#'
#' # global reactiveVal handle (created per session lazily)
#' iphra_log_store <- NULL
#'
#' #' @title Get or Create the Session Log Store
#' #' @description
#' #' Uses Shiny's `session$userData` to store a per-session reactive log.
#' #' Automatically initializes the log store if missing.
#' #'
#' #' @return A `reactiveVal` object containing the log data frame.
#' #' @keywords internal
#' iphra_get_log_store <- function(session = shiny::getDefaultReactiveDomain()) {
#'   if (is.null(session)) {
#'     stop("[IPHRA::Error] No active Shiny session found; cannot access log store.")
#'   }
#'   if (is.null(session$userData$iphra_log_store)) {
#'     session$userData$iphra_log_store <- iphra_init_log()
#'   }
#'   session$userData$iphra_log_store
#' }
#'
#' @title Append a Log Entry
#' @keywords internal
iphra_log <- function(level, message, origin = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE) || !shiny::isRunning()) return(invisible())
  session <- shiny::getDefaultReactiveDomain()
  log_fun <- iphra_get_log_store(session)
  current <- log_fun()
  entry <- data.frame(
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    level = level,
    origin = origin %||% "",
    message = message,
    stringsAsFactors = FALSE
  )
  log_fun(rbind(current, entry))
  invisible(NULL)
}
#'
#' #' @title Get Current IPHRA Session Logs
#' #' @return A reactive expression that returns the session log data frame.
#' #' @export
#' iphra_get_logs <- function(session = shiny::getDefaultReactiveDomain()) {
#'   store <- iphra_get_log_store(session)
#'   shiny::reactive(store())
#' }


# ---- Internal notifier ---------------------------------------------

#' @title Internal Notification Dispatcher
#' @description
#' Handles displaying messages, warnings, and errors either via Shiny notifications
#' (if the app is running) or via console output (when outside Shiny).
#'
#' @param message The text to display.
#' @param type One of `"message"`, `"warning"`, or `"error"`.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
.iphra_notify <- function(message, type = c("message", "warning", "error")) {
  type <- match.arg(type)
  if (requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()) {
    shiny::showNotification(message, type = type)
  } else {
    switch(
      type,
      message = base::message(message),
      warning = base::warning(message, call. = FALSE),
      error   = base::message(paste0("[IPHRA::Error] ", message))
    )
  }
}

# Internal notification wrapper for testing and Shiny-safe mocking
.iphra_showNotification <- function(message, type = "default") {
  if (requireNamespace("shiny", quietly = TRUE)) {
    fn <- get("showNotification", asNamespace("shiny"))
    fn(message, type = type)
  } else {
    message(sprintf("[IPHRA::Notify Fallback] %s (%s)", message, type))
  }
}


# ---- Core functions ------------------------------------------------

#' @title Throw a Shiny-Aware Standardized IPHRA Error
#' @description
#' Displays or raises a structured error safely.
#' In Shiny, it shows a non-crashing notification and stops only
#' the local operation using `shiny::req(FALSE)`.
#' Outside Shiny, it aborts execution with a standardized error message.
#'
#' @param message The error text.
#' @param type Character string specifying the type (default = `"Error"`).
#' @param origin Optional name of the originating function or process.
#' @param hint Optional string providing a corrective suggestion for the user.
#'
#' @return No return value; execution stops locally.
#' @export
iphra_error <- function(message, type = "Error", origin = NULL, hint = NULL) {
  full_msg <- paste0(
    "[IPHRA::", type, "] ",
    if (!is.null(origin)) paste0("In `", origin, "`: ") else "",
    message,
    if (!is.null(hint)) paste0("\n • Hint: ", hint) else ""
  )

  # iphra_log("error", full_msg, origin)

  if (requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()) {
    .iphra_showNotification(full_msg, type = "error")

    # ✅ Skip shiny::req(FALSE) only during testing
    if (!isTRUE(getOption("IPHRA_TEST_MODE", FALSE))) {
      shiny::req(FALSE)
    } else {
      message("[IPHRA_TEST_MODE active] — skipping shiny::req(FALSE)")
    }
  } else {
    rlang::abort(message = full_msg, class = paste0("iphra_", tolower(type)))
  }
}

#' @title Issue a Shiny-Aware IPHRA Warning
#' @description
#' Displays a structured warning in either the console or the Shiny app,
#' and logs the message for the current session.
#'
#' @param message The warning text.
#' @param type Character string specifying the type (default = `"Warning"`).
#' @param origin Optional name of the originating function.
#' @param hint Optional string suggesting corrective action.
#'
#' @return Invisibly returns `NULL`.
#' @export
iphra_warning <- function(message, type = "Warning", origin = NULL, hint = NULL) {
  full_msg <- paste0(
    "[IPHRA::", type, "] ",
    if (!is.null(origin)) paste0("In `", origin, "`: ") else "",
    message,
    if (!is.null(hint)) paste0("\n • Hint: ", hint) else ""
  )
  # iphra_log("warning", full_msg, origin)
  .iphra_notify(full_msg, type = "warning")
  invisible(NULL)
}

#' @title Emit a Shiny-Aware IPHRA Message
#' @description
#' Displays an informational message in the console or via a Shiny notification
#' and records it in the session log.
#'
#' @param message Informational message text.
#' @param origin Optional name of the originating function.
#'
#' @return Invisibly returns `NULL`.
#' @export
iphra_message <- function(message, origin = NULL) {
  full_msg <- paste0(
    "[IPHRA::Message] ",
    if (!is.null(origin)) paste0("In `", origin, "`: ") else "",
    message
  )
  # iphra_log("message", full_msg, origin)
  .iphra_notify(full_msg, type = "message")
  invisible(NULL)
}

#' @title IPHRA Shiny-Aware Assertion
#' @description
#' Tests a condition and displays a safe, Shiny-compatible error
#' if the assertion fails.
#' Works similarly to `stopifnot()` but with standardized IPHRA messaging and logging.
#'
#' @param condition Logical expression to evaluate.
#' @param message Error message to show if the condition is `FALSE`.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective suggestion for the user.
#'
#' @return No return value. Stops execution locally if the condition fails.
#' @export
iphra_assert <- function(condition, message, origin = NULL, hint = NULL) {
  if (!isTRUE(condition)) {
    iphra_error(message = message, type = "AssertionError", origin = origin, hint = hint)
  }
}

#' @title Safe IPHRA Try Wrapper (Enhanced with Nested Context Support)
#' @description
#' Evaluates an expression safely and reports structured feedback
#' to the console or Shiny UI without crashing the app.
#' Preserves and passes along origin and hint metadata for better context.
#'
#' Supports nested error handling via the `step` parameter, which allows
#' inner try blocks to add sub-context to error messages. When `step` is
#' provided alongside `origin`, messages are formatted as "origin → step".
#'
#' @param expr Expression to evaluate.
#' @param on_error One of "warn", "return", or "abort".
#' @param origin Optional string identifying where the try block is called from.
#' @param hint Optional corrective hint to show in case of error.
#' @param step Optional string identifying the step within a larger operation.
#'   When provided, adds sub-context to error messages for nested try blocks.
#'   If both `origin` and `step` are provided, the full origin becomes "origin → step".
#'
#' @return If `on_error = "return"`, returns list(success = FALSE, error = message, origin = ..., step = ..., hint = ...).
#'   On success, returns the result of `expr` (or NULL if no explicit return).
#' @export
#'
#' @examples
#' \dontrun{
#' # Outer catch-all with nested step tracking
#' observe({
#'   iphra_try({
#'     # Validation step
#'     result <- iphra_try({
#'       if (is.null(input$value)) stop("Value required")
#'     }, on_error = "return", step = "Validation")
#'     if (isFALSE(result$success)) return(result)
#'
#'     # Core logic step
#'     result <- iphra_try({
#'       process_data(input$value)
#'     }, on_error = "return", step = "Core Logic")
#'     if (isFALSE(result$success)) return(result)
#'
#'   },
#'   on_error = "warn",
#'   origin = "Module: Operation",
#'   hint = "Check input values"
#'   )
#' })
#' }
iphra_try <- function(expr,
                      on_error = c("warn", "return", "abort"),
                      origin = NULL,
                      hint = NULL,
                      step = NULL) {
  on_error <- match.arg(on_error)

  # Build full origin with step context if provided
  full_origin <- if (!is.null(step) && !is.null(origin)) {
    paste0(origin, " \u2192 ", step)  # Unicode arrow →
  } else if (!is.null(step)) {
    step
  } else {
    origin
  }

  tryCatch(
    expr,
    error = function(e) {
      base_msg <- conditionMessage(e)

      # Check if error came from a nested iphra_try with step info
      # If so, preserve the nested context chain
      if (grepl("\\[IPHRA::", base_msg)) {
        # Already has IPHRA structure - check if we need to add outer context
        if (!is.null(origin) && !grepl(origin, base_msg, fixed = TRUE)) {
          # Add outer origin context to nested error
          msg <- sub("\\[IPHRA::TryError\\] ",
                     paste0("[IPHRA::TryError] ", origin, " \u2192 "),
                     base_msg)
        } else {
          msg <- base_msg
        }
      } else {
        # New error - format with full origin (including step if present)
        if (!is.null(full_origin)) {
          msg <- paste0("[IPHRA::TryError] ", full_origin, ": ", base_msg)
        } else {
          msg <- paste0("[IPHRA::TryError] ", base_msg)
        }
      }

      # Log the failure
      # iphra_log("error", msg, full_origin)

      # Act according to user preference
      switch(
        on_error,
        warn = iphra_warning(msg, origin = full_origin %||% "iphra_try", hint = hint),
        abort = iphra_error(msg, origin = full_origin %||% "iphra_try", hint = hint),
        return = list(success = FALSE, error = msg, origin = origin, step = step, hint = hint)
      )
    }
  )
}


#' @title Lightweight Step-Level Try Wrapper for Nested Error Handling
#' @description
#' A convenience wrapper around `iphra_try` designed for inner try blocks
#' within a nested error handling pattern. Always uses `on_error = "return"`
#' so that errors bubble up to the outer handler.
#'
#' This function is intended for use inside an outer `iphra_try` block to
#' provide granular step-level error context (e.g., "Validation", "Core Logic",
#' "Result Handling").
#'
#' @param expr Expression to evaluate.
#' @param step String identifying the step within the larger operation.
#' @param hint Optional corrective hint to show in case of error.
#'
#' @return A list with `success = FALSE` and error details if an error occurs,
#'   otherwise returns the result of `expr`.
#' @export
#'
#' @examples
#' \dontrun{
#' observe({
#'   iphra_try({
#'     # Use iphra_try_step for each logical step
#'     result <- iphra_try_step({
#'       validate_input(input$data)
#'     }, step = "Validation")
#'     if (isFALSE(result$success)) return(result)
#'
#'     result <- iphra_try_step({
#'       process_data(input$data)
#'     }, step = "Core Logic")
#'     if (isFALSE(result$success)) return(result)
#'
#'   },
#'   on_error = "warn",
#'   origin = "Module: Operation"
#'   )
#' })
#' }
iphra_try_step <- function(expr, step, hint = NULL) {
  iphra_try(
    expr,
    on_error = "return",
    origin = NULL,  # Let outer handler provide origin
    step = step,
    hint = hint
  )
}


#' @title Check if an iphra_try Result Indicates Failure
#' @description
#' Utility function to check if the result from `iphra_try` or `iphra_try_step`
#' indicates a failure. Returns TRUE if the result is a list with `success = FALSE`.
#'
#' @param result The result from an `iphra_try` or `iphra_try_step` call.
#'
#' @return TRUE if the result indicates failure, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' result <- iphra_try_step({ stop("error") }, step = "Test")
#' if (iphra_failed(result)) {
#'   return(result)  # Bubble up to outer handler
#' }
#' }
iphra_failed <- function(result) {
  is.list(result) && !is.null(result$success) && isTRUE(result$success == FALSE)
}
