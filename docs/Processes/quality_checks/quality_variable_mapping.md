# Data Quality Variable Mapping

## Overview

The Data Quality workflow now properly uses `variable_map` to translate canonical variable names to actual column names when executing quality checks. This allows quality schemas to be written using canonical names (consistent across datasets) while the checks execute against datasets with different column naming conventions.

## Problem Statement

Previously, the Data Quality workflow was checking for variables using exact names from the schema:

```r
# OLD CODE - Incorrect
variables <- check$variables  # e.g., c("household_size")
available_vars <- intersect(variables, names(self$data))
```

This approach failed when:
- Quality schema used canonical names (e.g., `household_size`)
- But the actual data had different column names (e.g., `q1_household_size`)
- Even though a `variable_map` existed to translate between them

## Solution

The fix adds a translation step that maps canonical variable names to actual column names before checking for their existence:

```r
# NEW CODE - Correct
variables <- check$variables  # Canonical names: c("household_size")
mapped_vars <- self$.translate_canonical_to_actual_vars(variables)  # Actual names: c("q1_household_size")
available_vars <- intersect(mapped_vars, names(self$data))  # Check actual names exist
```

## Changes Made

### 1. Base DataQuality Class (`R/class_data_quality.R`)

#### New get_variable Method

Added `get_variable()` method to provide consistency with the Data class:

```r
get_variable = function(role) {
  if (is.null(role) || length(role) == 0) {
    return(NULL)
  }
  
  # Return mapped column name, or NULL if not in variable_map
  if (!is.null(self$variable_map) && role %in% names(self$variable_map)) {
    return(self$variable_map[[role]])
  }
  
  return(NULL)
}
```

This method follows the same pattern as the Data class's `get_variable()` method, providing a consistent interface for accessing variable mappings.

#### New Helper Method

Added `.translate_canonical_to_actual_vars()` method that uses `get_variable()` internally:

```r
.translate_canonical_to_actual_vars = function(canonical_vars) {
  # ... validation ...
  
  # Translate each canonical name to actual name using get_variable
  actual_vars <- sapply(canonical_vars, function(canonical_name) {
    # Use get_variable for consistency with Data class
    actual_name <- self$get_variable(canonical_name)
    
    # Return mapped name if it exists, otherwise return canonical name
    if (!is.null(actual_name) && nzchar(actual_name)) {
      return(actual_name)
    } else {
      return(canonical_name)
    }
  }, USE.NAMES = FALSE)
  
  return(actual_vars)
}
```

#### Updated execute_check Method

Modified to use variable translation:

```r
execute_check = function(check) {
  # ... result initialization ...
  
  variables <- check$variables
  
  # NEW: Translate canonical variable names to actual column names
  mapped_vars <- self$.translate_canonical_to_actual_vars(variables)
  
  # Check that ALL mapped variables exist in data (not just some)
  available_vars <- intersect(mapped_vars, names(self$data))
  if (length(available_vars) != length(mapped_vars)) {
    missing_vars <- setdiff(mapped_vars, available_vars)
    result$message <- paste0("Required variables not found in data: ", paste(missing_vars, collapse = ", "))
    return(result)
  }
  
  # ... rest of execution ...
}
```

**Key Change**: The validation now ensures that **all** required variables are present, not just some. If any variable is missing, the check is skipped with a message listing the missing variables.

#### Enhanced evaluate_threshold_expressions Method

Updated to support both field naming conventions:
- `expression` or `threshold_expression` for threshold conditions
- `penalty` or `penalty_score` for penalty values

This ensures compatibility with both manual schema creation and Excel-imported schemas.

### 2. FSLDataQuality Class (`R/class_data_quality_fsl.R`)

Updated `execute_check` override to use variable translation:

```r
execute_check = function(check) {
  variables <- check$variables
  
  # Translate canonical variable names to actual column names using variable_map
  mapped_vars <- self$.translate_canonical_to_actual_vars(variables)
  
  # Check if mapped variables exist in data
  available_vars <- intersect(mapped_vars, names(self$data))
  
  # ... rest of FSL-specific execution ...
}
```

### 3. HealthDataQuality Class (`R/class_data_quality_health.R`)

Applied same variable translation pattern as FSL.

### 4. WASHDataQuality Class (`R/class_data_quality_wash.R`)

Applied same variable translation pattern as FSL.

### 5. Test Coverage (`tests/testthat/test-class_data_quality.R`)

Added comprehensive tests:

- **Variable mapping with custom variable_map**: Verifies canonical names are translated correctly
- **Missing variable_map**: Tests fallback behavior when no mapping exists
- **Unmapped variables**: Ensures proper error handling for missing variables
- **Multiple variables**: Tests correlation checks with multiple translated variables
- **Helper method tests**: Direct testing of `.translate_canonical_to_actual_vars()`

## Usage Example

### Setup Data with Variable Mapping

```r
# Create data with actual column names
df <- tibble::tibble(
  hh_id = 1:10,
  q1_household_size = sample(1:10, 10, replace = TRUE),
  q2_monthly_income = rnorm(10, mean = 1000, sd = 200)
)

# Create variable_map: canonical -> actual
var_map <- list(
  household_size = "q1_household_size",
  monthly_income = "q2_monthly_income"
)

# Create DataQuality object with mapping
dq <- DataQuality$new(
  data = df,
  variable_map = var_map,
  dataset_name = "MyDataQuality"
)
```

### Define Schema with Canonical Names

```r
# Schema uses canonical names
custom_schema <- list(
  check_household_size = list(
    check_name = "check_household_size",
    check_label = "Check Household Size Range",
    statistical_test = "range_violation",
    variables = c("household_size"),  # Canonical name!
    thresholds = list(
      list(expression = "test_statistic <= 10", penalty = 0),
      list(expression = "test_statistic > 10", penalty = 5)
    ),
    test_params = list(min_value = 1, max_value = 15)
  )
)

dq$set_quality_schema(custom_schema)
```

### Run Checks

```r
# Quality checks will automatically translate "household_size" -> "q1_household_size"
results <- dq$run_quality_checks()

# Check executes successfully using translated variable names
print(results$check_household_size$test_statistic)
```

## Benefits

1. **Schema Portability**: Quality schemas can be written once using canonical names and work across different datasets with different column naming conventions.

2. **Consistency**: Aligns quality check behavior with dependency rules and indicator calculations, which already use variable mapping.

3. **Maintainability**: Schemas remain readable and don't need dataset-specific column names.

4. **Backward Compatibility**: When no `variable_map` is provided, the code falls back to using exact column names from the schema.

## Related Documentation

- [Variable and Value Mapping Guide](variable_value_mapping_guide.md)
- [Quality Schema Overview](schema_overview.md)
- [Standardization Process](standardization_process.md)
