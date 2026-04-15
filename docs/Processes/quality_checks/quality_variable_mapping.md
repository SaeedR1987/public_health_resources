# Data Quality Variable Mapping

## Overview

The quality check workflow uses `variable_map` to translate canonical variable names to actual column names when executing quality checks. This allows quality schemas to be written using canonical names (consistent across datasets) while the checks execute against datasets with different column naming conventions.

## Problem Statement

Previously, the quality workflow checked for variables using exact names from the schema:

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

A translation step maps canonical variable names to actual column names before checking for their existence:

```r
# Correct
variables <- check$variables  # Canonical names: c("household_size")
mapped_vars <- self$.translate_canonical_to_actual_vars(variables)  # Actual names: c("q1_household_size")
available_vars <- intersect(mapped_vars, names(self$data))  # Check actual names exist
```

## How It Works in DataAnalytics

### Variable Translation

The `DataAnalytics` class (and its subclasses) include a `get_variable()` method and a `.translate_canonical_to_actual_vars()` helper that maps canonical schema names to actual column names via `variable_map`.

### execute_check Method

The `execute_check` method translates canonical variable names before running statistical tests:

```r
execute_check = function(check) {
  variables <- check$variables

  # Translate canonical variable names to actual column names
  mapped_vars <- self$.translate_canonical_to_actual_vars(variables)

  # Check that ALL mapped variables exist in data
  available_vars <- intersect(mapped_vars, names(self$data))
  if (length(available_vars) != length(mapped_vars)) {
    missing_vars <- setdiff(mapped_vars, available_vars)
    result$message <- paste0("Required variables not found in data: ", paste(missing_vars, collapse = ", "))
    return(result)
  }

  # ... rest of execution ...
}
```

**Key Behavior**: All required variables must be present. If any variable is missing, the check is skipped with a message listing the missing variables.

## Usage Example

### Setup Data with Variable Mapping

```r
# Create data with actual column names
df <- tibble::tibble(
  hh_id = 1:10,
  q1_household_size = sample(1:10, 10, replace = TRUE),
  q2_monthly_income = rnorm(10, mean = 1000, sd = 200)
)

# Create a data object and set the variable map
data <- HouseholdData$new(data = df, uuid = "hh_id")

# variable_map: canonical -> actual
var_map <- list(
  household_size = "q1_household_size",
  monthly_income = "q2_monthly_income"
)
data$variable_map <- var_map

# Generate analytics object (inherits variable_map)
analytics <- data$generate_data_analytics(stage = "standardized")
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
      list(threshold_expression = "test_statistic <= 10", penalty_score = 0),
      list(threshold_expression = "test_statistic > 10", penalty_score = 5)
    ),
    test_params = list(min_value = 1, max_value = 15)
  )
)

analytics$set_quality_schema(custom_schema)
```

### Run Checks

```r
# Quality checks automatically translate "household_size" -> "q1_household_size"
analytics$run_quality_checks()

# View results
print(analytics$quality_results$check_household_size$test_statistic)
```

## Benefits

1. **Schema Portability**: Quality schemas can be written once using canonical names and work across different datasets with different column naming conventions.

2. **Consistency**: Aligns quality check behavior with dependency rules and indicator calculations, which already use variable mapping.

3. **Maintainability**: Schemas remain readable and don't need dataset-specific column names.

4. **Backward Compatibility**: When no `variable_map` is provided, the code falls back to using exact column names from the schema.

## Related Documentation

- [Variable and Value Mapping Guide](../../Data_Structures/variable_value_mapping_guide.md)
- [Quality Schema Overview](../../Data_Structures/schema_overview.md)
- [Standardization Process](../standardization/standardization_process.md)
