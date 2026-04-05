# Dependency Schema Harmonization - Migration Guide

## Overview

This document describes the harmonization of dependency rules in the public_health_resources package. The goal was to consolidate all data quality checks into a unified `dependency_schema` structure, removing redundancy and improving maintainability.

## What Changed

### 1. Variable Schema Template Changes

**Removed Columns:**
- `pattern` - Pattern validation rules (moved to dependency_schema)
- `range` - Numeric range checks (moved to dependency_schema)
- `precision_limits` - Decimal precision checks (moved to dependency_schema)
- `mutex_group` - Mutually exclusive group definitions (moved to dependency_schema)
- `not_future` - Date validation rules (moved to dependency_schema)

**Files Updated:**
- `resources/household_variable_schema_template.xlsx`
- `resources/household_wash_variable_schema_template.xlsx`
- `resources/household_fsl_variable_schema_template.xlsx`

**Retained in Variable Schema:**
- `type` - Data type specifications (used for type coercion checks)
- `required` - Required field indicators
- `allowed` - Allowed values (kept for backward compatibility, but preferably use dependency_schema)
- `unique` - Unique constraint indicators (kept for convenience)
- `col_names` - Column name alternatives for auto-mapping
- `question_type`, `is_other`, `other_column_link` - Metadata fields

### 2. Quality Check Flag Prefix Change

**Before:** Flags were prefixed with `dq_`
- Example: `dq_age_range`, `dq_consent_allowed`

**After:** All flags now use `flag_` prefix consistently
- Example: `flag_age_range`, `flag_consent_allowed`

### 3. run_quality_checks Method

**Before:**
- Separate sections for allowed_values, ranges, patterns, unique, mutually_exclusive, date_validity
- Mixed use of variable_schema and dependency_schema

**After:**
- Unified processing through dependency_schema
- Type coercion checks still use variable_schema$types
- All other checks must be defined in dependency_schema

### 4. Schema Processing Functions

**Updated Functions:**
- `data_schema_to_table()` - No longer exports removed fields
- `data_table_to_schema()` - No longer imports removed fields
- `data_validate_schema_to_table()` - Removed fields are now optional with warnings

## Migration Instructions

### For Existing Variable Schemas

If your existing code uses the old variable_schema structure with patterns, ranges, etc., you have two options:

#### Option 1: Convert to Dependency Schema (Recommended)

Convert your quality checks to dependency rules. See examples in `resources/household_dependency_schema_example.xlsx`.

**Example: Range Check**

Before (in variable_schema):
```r
schema <- list(
  types = list(age = "numeric"),
  ranges = list(age = c(18, 120))
)
```

After (in dependency_schema):
```r
dependency_schema <- list(
  dependencies = list(
    flag_age_range = list(
      variables = "age",
      condition_if = "!is.na(age)",
      then = "age >= 18 & age <= 120",
      action = "flag_warning"
    )
  )
)
```

#### Option 2: Backward Compatibility (Temporary)

The old fields are still parsed if present (with warnings), but they won't generate quality flags. This is for temporary backward compatibility only.

### For Test Code

Tests that expect the old `dq_` prefix need to be updated to use `flag_`:

**Before:**
```r
expect_true("dq_age_range" %in% names(data$data_quality_flags))
expect_equal(data$data_quality_flags$dq_age_range, c(0,0,1,1))
```

**After:**
```r
expect_true("flag_age_range" %in% names(data$data_quality_flags))
expect_equal(data$data_quality_flags$flag_age_range, c(0,0,1,1))
```

### For Cleaning Log Generation

The `generate_cleaning_log()` method now:
- Only searches for `flag_` prefixed columns (no longer `dq_`)
- Reads the `action` field from dependency_schema to determine `changed` value
- Uses `flag_autoclean` action to set `changed="yes"`

### For Deletion Log Generation

Update critical_flags parameter to use new prefix:

**Before:**
```r
data$generate_deletion_log(critical_flags = c("dq_uuid_unique"))
```

**After:**
```r
data$generate_deletion_log(critical_flags = c("flag_uuid_unique"))
```

## Dependency Schema Structure

### Basic Structure

```r
dependency_schema <- list(
  dependencies = list(
    flag_name_1 = list(
      variables = c("var1", "var2"),
      condition_if = "logical expression",
      then = "logical expression",
      action = "flag_autoclean" # or "flag_warning", etc.
    ),
    flag_name_2 = list(...)
  ),
  soft_dependencies = list(
    flag_name_3 = list(...)
  )
)
```

### Key Fields

- **variables**: Character vector of variable names involved in the check
- **condition_if**: R expression that defines when to apply the check (evaluates to logical vector)
- **then**: R expression that defines the expected condition (evaluates to logical vector)
- **action**: Action to take when check fails:
  - `"flag_autoclean"` - Mark as requiring cleaning (changed="yes" in cleaning log)
  - `"flag_warning"` - Mark as warning only (changed="no" in cleaning log)

### Common Check Patterns

See `resources/household_dependency_schema_example.xlsx` for complete examples:

1. **Allowed Values**: `var %in% c('val1', 'val2', NA)`
2. **Range Check**: `var >= min & var <= max`
3. **Pattern Check**: `grepl("pattern", var)`
4. **Unique Constraint**: `!duplicated(var)`
5. **Mutually Exclusive**: Sum of indicators <= 1
6. **Conditional Dependency**: If condition A, then check B
7. **Date Not Future**: `date <= Sys.Date()`
8. **Precision Limit**: `var == round(var, digits)`

## Type Coercion Checks

Type information remains in `variable_schema$types`:

```r
variable_schema <- list(
  types = list(
    age = "numeric",
    name = "character",
    consent = "logical"
  )
)
```

Type coercion checks in `run_quality_checks()` will:
- Identify rows that cannot be safely coerced to the specified type
- Generate flags with pattern `flag_{varname}_type`
- Use the helper functions `.is_safely_coercible()` and `.which_bad_coercible()`

## Breaking Changes

1. **Schema Templates**: Columns removed from xlsx templates
2. **Flag Prefix**: All flags now use `flag_` instead of `dq_`
3. **Quality Checks**: Checks previously defined in variable_schema no longer generate flags automatically
4. **API Change**: `run_quality_checks()` requires dependency_schema for most checks

## Benefits of This Change

1. **Unified Structure**: All quality checks follow the same pattern
2. **Flexibility**: Complex conditional logic easily expressed
3. **Maintainability**: Rules defined in one place
4. **Consistency**: Uniform flag naming and processing
5. **Documentation**: Each check can have descriptive labels and comments

## Support

For questions or issues related to this migration, please refer to:
- Example schema: `resources/household_dependency_schema_example.xlsx`
- Test files showing new structure in `tests/testthat/`
- Issue discussion on GitHub

## Timeline

- **Current**: Both old and new structures supported (with warnings for old structure)
- **Future**: Old structure support may be fully deprecated in a future version
