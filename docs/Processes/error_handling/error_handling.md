# Error Handling System

## Overview

The iphRa package provides a comprehensive error handling system designed for both interactive (Shiny) and non-interactive (console) use. The system ensures graceful error handling, informative messages, and proper error propagation throughout the data pipeline.

## Core Components

### 1. Error Emission Functions

#### `phr_error()`
Throws a standardized error that is Shiny-aware.

```r
phr_error(
  message,           # Error message text
  type = "Error",    # Error type (default: "Error")
  origin = NULL,     # Function/process where error occurred
  hint = NULL        # Corrective suggestion for user
)
```

**Behavior:**
- **In Shiny**: Displays notification and stops local operation using `shiny::req(FALSE)`
- **In Console**: Aborts execution with structured error message

**Example:**
```r
phr_error(
  "Raw data is NULL; cannot standardize.",
  origin = "MyData$standardize",
  hint = "Reinitialize the Data object."
)
```

#### `phr_warning()`
Issues a non-fatal warning message.

```r
phr_warning(
  message,           # Warning message text
  type = "Warning",  # Warning type (default: "Warning")
  origin = NULL,     # Function/process where warning occurred
  hint = NULL        # Corrective suggestion
)
```

**Example:**
```r
phr_warning(
  "Column 'age' cannot be safely coerced to numeric. Leaving as-is.",
  origin = "MyData$standardize"
)
```

#### `phr_message()`
Displays informational messages.

```r
phr_message(
  message,      # Message text
  origin = NULL # Function/process origin
)
```

**Example:**
```r
phr_message("Standardizing household data...")
```

### 2. Safe Execution Wrappers

#### `phr_try()`
Safely evaluates an expression with configurable error handling.

```r
phr_try(
  expr,                           # Expression to evaluate
  on_error = c("warn", "return", "abort"),
  origin = NULL,                  # Function/process identifier
  hint = NULL,                    # Corrective hint
  step = NULL                     # Step within larger operation
)
```

**Error Handling Modes:**

1. **`on_error = "warn"`**: Logs warning and continues execution
2. **`on_error = "return"`**: Returns error list, allows caller to handle
3. **`on_error = "abort"`**: Stops execution immediately

**Return Values:**
- **On success**: Returns the result of `expr`
- **On error (with `on_error = "return"`)**: Returns list with:
  - `success = FALSE`
  - `error = "error message"`
  - `origin = "origin string"`
  - `step = "step name"`
  - `hint = "hint text"`

**Example:**
```r
result <- phr_try({
  process_data(my_data)
}, on_error = "return", origin = "MyModule")

if (phr_failed(result)) {
  return(result)  # Bubble up error
}
```

#### `phr_try_step()`
Convenience wrapper for nested error handling within a larger operation.

```r
phr_try_step(
  expr,        # Expression to evaluate
  step,        # Step name for error context
  hint = NULL  # Optional corrective hint
)
```

This function always uses `on_error = "return"` to enable error bubbling in nested contexts.

**Example:**
```r
# Outer handler
phr_try({
  
  # Validation step
  result <- phr_try_step({
    validate_input(input$data)
  }, step = "Validation")
  if (phr_failed(result)) return(result)
  
  # Processing step
  result <- phr_try_step({
    process_data(input$data)
  }, step = "Processing")
  if (phr_failed(result)) return(result)
  
}, on_error = "warn", origin = "MyModule")
```

### 3. Error Checking Functions

#### `phr_failed()`
Checks if a result from `phr_try()` or `phr_try_step()` indicates failure.

```r
phr_failed(result)
```

**Returns:** `TRUE` if result is a failure, `FALSE` otherwise

**Implementation (as of January 2026):**
```r
phr_failed <- function(result) {
  is.list(result) && !is.null(result$success) && isTRUE(result$success == FALSE)
}
```

**Behavior:**
- Returns `TRUE` only if:
  1. `result` is a list
  2. `result$success` field exists (not NULL)
  3. `result$success` equals `FALSE`
- Returns `FALSE` for:
  - Non-list results (e.g., `NULL`, numbers, strings)
  - Lists without a `success` field
  - Successful results (`success = TRUE`)

**Important Update (January 2026):**

Prior to this revision, `phr_failed()` did not check for the existence of the `success` field before accessing it:

```r
# Old implementation (caused warnings)
phr_failed <- function(result) {
  is.list(result) && isTRUE(result$success == FALSE)
}
```

This caused R to emit warnings like:
```
Warning: Unknown or uninitialised column: `success`
```

The issue occurred because when `phr_try_step()` succeeds without errors, it returns the result of the expression (often `NULL` or data), not an error list. The old implementation would attempt to access `result$success` even when it didn't exist, causing `NULL` to be returned and triggering the warning when evaluated in the comparison.

**The Fix:**

The updated implementation adds an explicit check `!is.null(result$success)` before accessing the field:

```r
# New implementation (no warnings)
phr_failed <- function(result) {
  is.list(result) && !is.null(result$success) && isTRUE(result$success == FALSE)
}
```

This ensures short-circuit evaluation: if `result$success` is NULL, the function returns `FALSE` without triggering the warning.

**Why This Matters:**

In the `standardize()` method and other pipeline operations, `phr_try_step()` is called repeatedly:

```r
result <- phr_try_step({
  self$pre_standardize()  # Returns NULL on success
}, step = "Pre-standardize hook")
if (phr_failed(result)) return(result)
```

When `pre_standardize()` succeeds (common case), it returns `NULL`. The old `phr_failed()` would try to check `NULL$success`, triggering warnings throughout the standardization process. The fix eliminates these spurious warnings while maintaining correct behavior.

### 4. Assertion Function

#### `phr_assert()`
Tests a condition and emits an error if it fails.

```r
phr_assert(
  condition,    # Logical expression to test
  message,      # Error message if condition is FALSE
  origin = NULL,
  hint = NULL
)
```

**Example:**
```r
phr_assert(
  !is.null(data),
  "Data cannot be NULL",
  origin = "process_data"
)
```

## Error Handling Patterns

### Pattern 1: Catch-All Error Handler

Use for top-level operations where you want to log errors but continue:

```r
phr_try({
  perform_operation()
}, on_error = "warn", origin = "MyModule")
```

### Pattern 2: Nested Error Handling

Use for multi-step operations where you need granular error context:

```r
phr_try({
  # Step 1: Validation
  result <- phr_try_step({
    validate_input(data)
  }, step = "Validation", hint = "Check data structure")
  if (phr_failed(result)) return(result)
  
  # Step 2: Processing
  result <- phr_try_step({
    process_data(data)
  }, step = "Processing", hint = "Check column types")
  if (phr_failed(result)) return(result)
  
  # Step 3: Output
  result <- phr_try_step({
    save_results(data)
  }, step = "Save Results")
  if (phr_failed(result)) return(result)
  
}, on_error = "abort", origin = "DataPipeline")
```

### Pattern 3: Return-Based Error Propagation

Use when you want to return errors to the caller:

```r
my_function <- function(data) {
  result <- phr_try({
    process_data(data)
  }, on_error = "return", origin = "my_function")
  
  if (phr_failed(result)) {
    return(result)  # Return error to caller
  }
  
  return(result)  # Return successful result
}
```

## Usage in Data Pipeline

### Standardization Process

The `standardize()` method uses nested error handling extensively:

```r
standardize = function() {
  phr_try({
    
    # Check raw data exists
    result <- phr_try_step({
      if (is.null(self$raw_data)) {
        phr_error("Raw data is NULL", origin = "standardize")
      }
    }, step = "Check raw data")
    if (phr_failed(result)) return(result)
    
    # Run validation
    result <- phr_try_step({
      self$validate()
    }, step = "Validation")
    if (phr_failed(result)) return(result)
    
    # Pre-standardize hook
    result <- phr_try_step({
      self$pre_standardize()
    }, step = "Pre-standardize hook")
    if (phr_failed(result)) return(result)
    
    # ... more steps ...
    
  }, on_error = "abort", origin = "MyData$standardize")
}
```

### Validation Process

Similar pattern used in `validate()`:

```r
validate = function() {
  phr_try({
    
    result <- phr_try_step({
      check_data_structure()
    }, step = "Structure check")
    if (phr_failed(result)) return(result)
    
    result <- phr_try_step({
      check_required_columns()
    }, step = "Required columns")
    if (phr_failed(result)) return(result)
    
    # ... more validation steps ...
    
  }, on_error = "warn", origin = "MyData$validate")
}
```

## Best Practices

### 1. Always Provide Origin

```r
# Good
phr_error("Invalid input", origin = "process_data")

# Bad
phr_error("Invalid input")
```

### 2. Use Descriptive Step Names

```r
# Good
phr_try_step({ ... }, step = "Load configuration file")

# Bad
phr_try_step({ ... }, step = "Step 1")
```

### 3. Provide Actionable Hints

```r
# Good
phr_error(
  "Column 'age' not found",
  hint = "Check that the dataset contains required columns"
)

# Bad
phr_error("Column 'age' not found")
```

### 4. Check Failures After Each Step

```r
# Good
result <- phr_try_step({ ... }, step = "Process")
if (phr_failed(result)) return(result)

# Bad - continuing without checking
result <- phr_try_step({ ... }, step = "Process")
# ... continue with more operations ...
```

### 5. Use Appropriate Error Modes

- **`on_error = "abort"`**: For critical operations that must succeed
- **`on_error = "warn"`**: For non-critical operations or top-level handlers
- **`on_error = "return"`**: For operations that need to bubble up errors

## Testing Error Handling

### Test Success Cases

```r
test_that("phr_failed returns FALSE for successful operations", {
  result <- phr_try_step({
    42  # Returns a value
  }, step = "Test")
  
  expect_false(phr_failed(result))
  expect_equal(result, 42)
})
```

### Test Failure Cases

```r
test_that("phr_failed returns TRUE for errors", {
  result <- phr_try_step({
    stop("Test error")
  }, step = "Test")
  
  expect_true(phr_failed(result))
  expect_true(is.list(result))
  expect_false(result$success)
})
```

### Test Edge Cases

```r
test_that("phr_failed handles edge cases", {
  # Non-list results
  expect_false(phr_failed(NULL))
  expect_false(phr_failed(42))
  expect_false(phr_failed("text"))
  
  # Lists without success field
  result <- list(data = 42, other = "value")
  expect_false(phr_failed(result))
  
  # Successful results
  result <- list(success = TRUE)
  expect_false(phr_failed(result))
})
```

## Related Documentation

- **Standardization Process**: See `docs/standardization_process.md`
- **Validation Process**: See `docs/validation_process.md`
- **Schema Overview**: See `docs/schema_overview.md`
- **Indicator Schema**: See `docs/indicator_schema.md`

## Revision History

### January 2026
- **Fixed `phr_failed()` to check for `success` field existence**: Added `!is.null(result$success)` check to prevent warnings when checking results from successful operations that don't return error lists. This eliminated spurious "Unknown or uninitialised column: `success`" warnings in the standardization pipeline.
