# Error Handling System

## Overview

The iphRa package provides a comprehensive error handling system designed for both interactive (Shiny) and non-interactive (console) use. The system ensures graceful error handling, informative messages, and proper error propagation throughout the data pipeline.

## Core Components

### 1. Error Emission Functions

#### `iphra_error()`
Throws a standardized error that is Shiny-aware.

```r
iphra_error(
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
iphra_error(
  "Raw data is NULL; cannot standardize.",
  origin = "MyData$standardize",
  hint = "Reinitialize the Data object."
)
```

#### `iphra_warning()`
Issues a non-fatal warning message.

```r
iphra_warning(
  message,           # Warning message text
  type = "Warning",  # Warning type (default: "Warning")
  origin = NULL,     # Function/process where warning occurred
  hint = NULL        # Corrective suggestion
)
```

**Example:**
```r
iphra_warning(
  "Column 'age' cannot be safely coerced to numeric. Leaving as-is.",
  origin = "MyData$standardize"
)
```

#### `iphra_message()`
Displays informational messages.

```r
iphra_message(
  message,      # Message text
  origin = NULL # Function/process origin
)
```

**Example:**
```r
iphra_message("Standardizing household data...")
```

### 2. Safe Execution Wrappers

#### `iphra_try()`
Safely evaluates an expression with configurable error handling.

```r
iphra_try(
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
result <- iphra_try({
  process_data(my_data)
}, on_error = "return", origin = "MyModule")

if (iphra_failed(result)) {
  return(result)  # Bubble up error
}
```

#### `iphra_try_step()`
Convenience wrapper for nested error handling within a larger operation.

```r
iphra_try_step(
  expr,        # Expression to evaluate
  step,        # Step name for error context
  hint = NULL  # Optional corrective hint
)
```

This function always uses `on_error = "return"` to enable error bubbling in nested contexts.

**Example:**
```r
# Outer handler
iphra_try({
  
  # Validation step
  result <- iphra_try_step({
    validate_input(input$data)
  }, step = "Validation")
  if (iphra_failed(result)) return(result)
  
  # Processing step
  result <- iphra_try_step({
    process_data(input$data)
  }, step = "Processing")
  if (iphra_failed(result)) return(result)
  
}, on_error = "warn", origin = "MyModule")
```

### 3. Error Checking Functions

#### `iphra_failed()`
Checks if a result from `iphra_try()` or `iphra_try_step()` indicates failure.

```r
iphra_failed(result)
```

**Returns:** `TRUE` if result is a failure, `FALSE` otherwise

**Implementation (as of January 2026):**
```r
iphra_failed <- function(result) {
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

Prior to this revision, `iphra_failed()` did not check for the existence of the `success` field before accessing it:

```r
# Old implementation (caused warnings)
iphra_failed <- function(result) {
  is.list(result) && isTRUE(result$success == FALSE)
}
```

This caused R to emit warnings like:
```
Warning: Unknown or uninitialised column: `success`
```

The issue occurred because when `iphra_try_step()` succeeds without errors, it returns the result of the expression (often `NULL` or data), not an error list. The old implementation would attempt to access `result$success` even when it didn't exist, causing `NULL` to be returned and triggering the warning when evaluated in the comparison.

**The Fix:**

The updated implementation adds an explicit check `!is.null(result$success)` before accessing the field:

```r
# New implementation (no warnings)
iphra_failed <- function(result) {
  is.list(result) && !is.null(result$success) && isTRUE(result$success == FALSE)
}
```

This ensures short-circuit evaluation: if `result$success` is NULL, the function returns `FALSE` without triggering the warning.

**Why This Matters:**

In the `standardize()` method and other pipeline operations, `iphra_try_step()` is called repeatedly:

```r
result <- iphra_try_step({
  self$pre_standardize()  # Returns NULL on success
}, step = "Pre-standardize hook")
if (iphra_failed(result)) return(result)
```

When `pre_standardize()` succeeds (common case), it returns `NULL`. The old `iphra_failed()` would try to check `NULL$success`, triggering warnings throughout the standardization process. The fix eliminates these spurious warnings while maintaining correct behavior.

### 4. Assertion Function

#### `iphra_assert()`
Tests a condition and emits an error if it fails.

```r
iphra_assert(
  condition,    # Logical expression to test
  message,      # Error message if condition is FALSE
  origin = NULL,
  hint = NULL
)
```

**Example:**
```r
iphra_assert(
  !is.null(data),
  "Data cannot be NULL",
  origin = "process_data"
)
```

## Error Handling Patterns

### Pattern 1: Catch-All Error Handler

Use for top-level operations where you want to log errors but continue:

```r
iphra_try({
  perform_operation()
}, on_error = "warn", origin = "MyModule")
```

### Pattern 2: Nested Error Handling

Use for multi-step operations where you need granular error context:

```r
iphra_try({
  # Step 1: Validation
  result <- iphra_try_step({
    validate_input(data)
  }, step = "Validation", hint = "Check data structure")
  if (iphra_failed(result)) return(result)
  
  # Step 2: Processing
  result <- iphra_try_step({
    process_data(data)
  }, step = "Processing", hint = "Check column types")
  if (iphra_failed(result)) return(result)
  
  # Step 3: Output
  result <- iphra_try_step({
    save_results(data)
  }, step = "Save Results")
  if (iphra_failed(result)) return(result)
  
}, on_error = "abort", origin = "DataPipeline")
```

### Pattern 3: Return-Based Error Propagation

Use when you want to return errors to the caller:

```r
my_function <- function(data) {
  result <- iphra_try({
    process_data(data)
  }, on_error = "return", origin = "my_function")
  
  if (iphra_failed(result)) {
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
  iphra_try({
    
    # Check raw data exists
    result <- iphra_try_step({
      if (is.null(self$raw_data)) {
        iphra_error("Raw data is NULL", origin = "standardize")
      }
    }, step = "Check raw data")
    if (iphra_failed(result)) return(result)
    
    # Run validation
    result <- iphra_try_step({
      self$validate()
    }, step = "Validation")
    if (iphra_failed(result)) return(result)
    
    # Pre-standardize hook
    result <- iphra_try_step({
      self$pre_standardize()
    }, step = "Pre-standardize hook")
    if (iphra_failed(result)) return(result)
    
    # ... more steps ...
    
  }, on_error = "abort", origin = "MyData$standardize")
}
```

### Validation Process

Similar pattern used in `validate()`:

```r
validate = function() {
  iphra_try({
    
    result <- iphra_try_step({
      check_data_structure()
    }, step = "Structure check")
    if (iphra_failed(result)) return(result)
    
    result <- iphra_try_step({
      check_required_columns()
    }, step = "Required columns")
    if (iphra_failed(result)) return(result)
    
    # ... more validation steps ...
    
  }, on_error = "warn", origin = "MyData$validate")
}
```

## Best Practices

### 1. Always Provide Origin

```r
# Good
iphra_error("Invalid input", origin = "process_data")

# Bad
iphra_error("Invalid input")
```

### 2. Use Descriptive Step Names

```r
# Good
iphra_try_step({ ... }, step = "Load configuration file")

# Bad
iphra_try_step({ ... }, step = "Step 1")
```

### 3. Provide Actionable Hints

```r
# Good
iphra_error(
  "Column 'age' not found",
  hint = "Check that the dataset contains required columns"
)

# Bad
iphra_error("Column 'age' not found")
```

### 4. Check Failures After Each Step

```r
# Good
result <- iphra_try_step({ ... }, step = "Process")
if (iphra_failed(result)) return(result)

# Bad - continuing without checking
result <- iphra_try_step({ ... }, step = "Process")
# ... continue with more operations ...
```

### 5. Use Appropriate Error Modes

- **`on_error = "abort"`**: For critical operations that must succeed
- **`on_error = "warn"`**: For non-critical operations or top-level handlers
- **`on_error = "return"`**: For operations that need to bubble up errors

## Testing Error Handling

### Test Success Cases

```r
test_that("iphra_failed returns FALSE for successful operations", {
  result <- iphra_try_step({
    42  # Returns a value
  }, step = "Test")
  
  expect_false(iphra_failed(result))
  expect_equal(result, 42)
})
```

### Test Failure Cases

```r
test_that("iphra_failed returns TRUE for errors", {
  result <- iphra_try_step({
    stop("Test error")
  }, step = "Test")
  
  expect_true(iphra_failed(result))
  expect_true(is.list(result))
  expect_false(result$success)
})
```

### Test Edge Cases

```r
test_that("iphra_failed handles edge cases", {
  # Non-list results
  expect_false(iphra_failed(NULL))
  expect_false(iphra_failed(42))
  expect_false(iphra_failed("text"))
  
  # Lists without success field
  result <- list(data = 42, other = "value")
  expect_false(iphra_failed(result))
  
  # Successful results
  result <- list(success = TRUE)
  expect_false(iphra_failed(result))
})
```

## Related Documentation

- **Standardization Process**: See `docs/standardization_process.md`
- **Validation Process**: See `docs/validation_process.md`
- **Schema Overview**: See `docs/schema_overview.md`
- **Indicator Schema**: See `docs/indicator_schema.md`

## Revision History

### January 2026
- **Fixed `iphra_failed()` to check for `success` field existence**: Added `!is.null(result$success)` check to prevent warnings when checking results from successful operations that don't return error lists. This eliminated spurious "Unknown or uninitialised column: `success`" warnings in the standardization pipeline.
