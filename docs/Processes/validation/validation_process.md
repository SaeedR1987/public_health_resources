# Data Validation Process

## Overview

The validation process is a critical step in the iphRa data pipeline that ensures data quality, structural integrity, and conformance to schema definitions before proceeding with standardization or cleaning. This document provides a comprehensive overview of the validation workflow, including the role of variable and value maps.

## Purpose

Validation ensures that:
- Data structure is sound (proper data frame, no critical NAs)
- Required columns are present
- UUID column exists and is unique
- Mapped variables and values align with the dataset
- Schema-defined constraints are satisfied
- Data is ready for standardization or cleaning operations

## Validation Stages

Validation can be performed at three different stages of the data lifecycle:

1. **Raw Stage** (`stage = "raw"`): Validates the originally imported data
2. **Standardized Stage** (`stage = "standardized"`): Validates data after type conversion and enrichment
3. **Clean Stage** (`stage = "clean"`): Validates data after cleaning operations

The same validation logic applies to all stages, ensuring consistency throughout the pipeline.

---

## The Validation Pipeline

The validation process follows a structured pipeline with multiple checkpoints:

```
Data → pre_validate → Core Checks → Variable Map Checks → Value Map Checks → post_validate → Validated
```

### Validation Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                      PRE-VALIDATE HOOK                           │
│              (Subclass-specific preparation)                     │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                      CORE DATA CHECKS                            │
├─────────────────────────────────────────────────────────────────┤
│  • Is valid data frame?                                          │
│  • Required columns present?                                     │
│  • UUID column exists?                                           │
│  • UUID has no missing values?                                   │
│  • UUID values are unique?                                       │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                   VARIABLE MAP VALIDATION                        │
├─────────────────────────────────────────────────────────────────┤
│  • All mapped columns exist in dataset?                          │
│  • Warnings for missing mapped columns                           │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                    VALUE MAP VALIDATION                          │
├─────────────────────────────────────────────────────────────────┤
│  • Value map roles linked to variable_map?                       │
│  • Mapped columns exist in dataset?                              │
│  • Mapped values exist in dataset?                               │
│  • Handle both nested and flat value map formats                 │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                    POST-VALIDATE HOOK                            │
│           (Subclass-specific additional checks)                  │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                    VALIDATION RESULT                             │
│         (TRUE if all checks pass, FALSE otherwise)               │
└─────────────────────────────────────────────────────────────────┘
```

---

## Validation Components

### 1. Pre-Validate Hook

**Purpose**: Allow subclasses to perform custom pre-validation setup or checks.

**Extension Point**: Subclasses can override `pre_validate()` to implement domain-specific preparation.

**Example Use Cases**:
- Check for domain-specific required metadata
- Prepare lookup tables for validation
- Validate cross-dataset relationships

```r
# Subclass implementation
pre_validate = function() {
  # Domain-specific validation setup
  if (is.null(self$metadata$survey_date)) {
    phr_warning(self$dataset_name, "Survey date metadata missing")
  }
}
```

---

### 2. Core Data Checks

**Purpose**: Validate fundamental data structure and integrity.

#### Data Frame Validation
- Confirms the data is a valid data frame structure
- Checks that data is not NULL or corrupted
- Uses `phr_validate_dataframe()` for structural validation

#### Required Columns Check
- Validates that all columns in `self$required_columns` are present
- Issues **warnings** (soft validation) for missing required columns
- Allows validation to continue even if some columns are missing

#### UUID Validation
The UUID column receives special treatment as it's the primary identifier:

1. **Existence Check**: UUID column must exist (hard error if missing)
2. **Missing Values Check**: UUID column must not contain NA values (soft warning)
3. **Uniqueness Check**: All UUID values must be unique (soft warning)

**Why Soft Warnings?**
- Missing or duplicate UUIDs are serious but may be correctable
- Soft warnings allow the pipeline to continue and generate cleaning logs
- Quality checks will flag these issues for resolution

---

### 3. Variable Map Validation

**Purpose**: Ensure mapped variables are present and correctly referenced.

The variable map (`self$variable_map`) links canonical variable names (roles) to actual dataset column names:

```r
variable_map = list(
  uuid = "submission_id",
  age = "respondent_age", 
  sex = "person_gender"
)
```

#### Validation Checks

**Column Existence**:
- Extracts all mapped column names from variable_map
- Checks if each mapped column exists in the dataset
- Issues warnings for missing columns

**Example**:
```r
# Variable map references "respondent_age" but dataset has "age"
variable_map = list(age = "respondent_age")
names(df) = c("submission_id", "age", "sex")

# Warning: "Mapped columns missing: respondent_age"
```

**Why This Matters**:
- Prevents downstream errors when accessing mapped columns
- Helps identify schema/data mismatches early
- Essential for quality checks and cleaning operations that rely on mapped variables

---

### 4. Value Map Validation

**Purpose**: Ensure categorical value mappings are valid and complete.

The value map (`self$value_map`) links canonical categorical values to dataset-specific values:

#### Nested Format (New)
```r
value_map = list(
  sex = list(
    male = c("male", "m", "homme"),
    female = c("female", "f", "femme")
  )
)
```

#### Flat Format (Legacy)
```r
value_map = list(
  sex = c("male", "female", "m", "f")
)
```

#### Validation Checks

**1. Role Linkage**:
- Each value map role must have a corresponding variable_map entry
- Example: If `value_map$sex` exists, then `variable_map$sex` must also exist
- **Warning**: "Value map role 'sex' is not linked to any variable_map entry"

**2. Column Existence**:
- The column referenced by the value map (via variable_map) must exist
- Example: `variable_map$sex = "person_gender"` → "person_gender" must be in dataset
- **Warning**: "Value map references column 'person_gender' which is missing"

**3. Value Presence**:
- For nested format: Checks all dataset values within each canonical mapping
- For flat format: Checks all mapped values directly
- Issues warnings for values in the map that don't exist in the dataset
- **Special handling for `select_multiple` variables**: When a variable is marked as `select_multiple` in the schema's `question_types`, validation automatically extracts individual tokens from space-separated values before comparison. For example, a cell containing "farming fishing" is treated as two separate tokens: "farming" and "fishing".

**Example - Nested Format**:
```r
# Value map with nested format
value_map = list(
  sex = list(
    male = c("male", "m", "homme"),
    female = c("female", "f", "femme")
  )
)

# Dataset has values: c("male", "female", "m", "f")
# Missing from dataset: c("homme", "femme")
# Warning: "Value map for role 'sex' has values not found in dataset: homme, femme"
```

**Example - Flat Format**:
```r
# Value map with flat format
value_map = list(
  status = c("active", "inactive", "pending")
)

# Dataset has values: c("active", "inactive")  
# Missing from dataset: c("pending")
# Warning: "Value map for role 'status' has values not found in dataset: pending"
```

**Why Format Detection Matters**:
- Supports both legacy flat format and new nested format
- Ensures backward compatibility with existing code
- Properly validates regardless of map structure

#### Format Detection Logic
```r
is_nested_format <- is.list(mapped_values) &&
                   !is.null(names(mapped_values)) &&
                   length(names(mapped_values)) > 0 &&
                   all(names(mapped_values) != "")
```

---

### 5. Post-Validate Hook

**Purpose**: Allow subclasses to perform custom post-validation checks.

**Extension Point**: Subclasses can override `post_validate(df)` to implement domain-specific validation.

**Parameters**:
- `df`: The data frame being validated (at the specified stage)

**Return Value**: 
- `TRUE` if additional validation passes
- `FALSE` if additional validation fails

**Example Use Cases**:
- Validate GPS coordinates are within expected ranges
- Check that household size matches roster counts
- Verify date ranges are sensible

```r
# Subclass implementation
post_validate = function(df) {
  valid <- TRUE
  
  # GPS validation
  if ("gps_lat" %in% names(df)) {
    if (any(abs(df$gps_lat) > 90, na.rm = TRUE)) {
      phr_warning(self$dataset_name, "Invalid GPS latitude values detected")
      valid <- FALSE
    }
  }
  
  return(valid)
}
```

---

## Variable and Value Maps Integration

### Why Maps Matter for Validation

The variable and value maps serve as the bridge between:
- **Schema definitions** (canonical, portable names and values)
- **Dataset reality** (actual column names and values)

Validation ensures this bridge is intact before proceeding with operations that depend on these mappings.

### Validation Workflow with Maps

```
Schema Definition              Variable/Value Maps           Dataset Reality
==================            ===================           ===============
Canonical Names      ──────>   Mappings          ──────>   Actual Columns
(age, sex, fever)              role -> column              (person_age, ...)

Canonical Values     ──────>   Value Mappings    ──────>   Actual Values  
(yes, no)                      canonical -> dataset        (yes, y, oui)
```

### What Validation Checks

1. **Map Completeness**: All roles referenced have mappings
2. **Target Existence**: All mapped targets exist in the dataset
3. **Value Coverage**: Mapped values are present in the data

### Impact on Downstream Operations

Valid maps enable:
- **Quality Checks**: Dependency rules use canonical names, resolved via maps
- **Indicator Calculation**: Functions reference variables by canonical names
- **Cleaning Operations**: Cleaning logs use actual column names, resolved via maps
- **Data Export**: Standardized reporting with canonical names

---

## Validation Modes

### Hard Errors (Abort)
Issues that prevent any further processing:
- Data is not a data frame
- Data is NULL or corrupted
- UUID column is completely missing

**Behavior**: Validation throws an error and stops execution

### Soft Warnings (Continue)
Issues that are serious but potentially correctable:
- Missing required columns
- Missing values in UUID column
- Duplicate UUID values
- Missing mapped columns
- Missing mapped values

**Behavior**: Validation issues warnings but continues, sets `validated = FALSE`

**Rationale**: 
- Allows quality checks to run and flag these issues
- Enables cleaning log generation for correction
- Provides complete picture of all data quality issues

---

## Validation Result

The validation process returns and stores a boolean result:

**`self$validated = TRUE`**: All checks passed without warnings
**`self$validated = FALSE`**: One or more warnings or post-validation failures

### Using Validation Results

```r
# Before standardization
data_obj$validate(stage = "raw")

if (data_obj$validated) {
  # Data is clean, proceed with confidence
  data_obj$standardize()
} else {
  # Data has issues, review warnings
  print("Validation warnings detected - review before proceeding")
  # Can still proceed but with caution
  data_obj$standardize()  # Will issue additional warning
}
```

---

## Best Practices

### For Package Users

1. **Always validate before standardizing**: Call `validate()` before `standardize()`
2. **Review warnings carefully**: Soft warnings indicate data quality issues
3. **Validate at each stage**: Validate raw, standardized, and clean data
4. **Check variable_map**: Ensure all critical variables are mapped correctly
5. **Verify value_map**: Confirm categorical values match expected patterns

### For Package Developers

1. **Use pre/post validation hooks**: Add domain-specific checks without modifying core validation
2. **Return boolean in post_validate**: Always return TRUE/FALSE to indicate validation status
3. **Issue appropriate warnings**: Use soft warnings for correctable issues
4. **Document validation requirements**: Clearly document what your validation hooks check
5. **Test validation paths**: Test both passing and failing validation scenarios

### For Schema Authors

1. **Define complete variable maps**: Include all critical variables with column name alternatives
2. **Use nested value maps**: Prefer nested format for multi-language/variant support
3. **Keep maps synchronized**: Ensure variable_map and value_map are consistent
4. **Document mappings**: Explain the rationale for canonical name choices
5. **Provide examples**: Include sample data that passes validation

---

## Example Workflow

```r
# Create data object
data_obj <- Data$new(
  data = survey_data,
  dataset_name = "MySurvey",
  uuid = "submission_id"
)

# Set schema with variable and value maps
schema <- list(
  types = list(
    uuid = "character",
    age = "numeric",
    sex = "character"
  ),
  col_names = list(
    uuid = c("uuid", "submission_id", "survey_id"),
    age = c("age", "respondent_age", "person_age"),
    sex = c("sex", "gender", "person_sex")
  )
)

data_obj$set_variable_schema(schema)

# Auto-map variables from schema
data_obj$map_schema_vars()

# Manually set value map for categorical variables
data_obj$value_map$sex <- list(
  male = c("male", "m", "homme", "masculin"),
  female = c("female", "f", "femme", "féminin")
)

# Validate raw data
data_obj$validate(stage = "raw")

if (data_obj$validated) {
  print("✓ Validation passed - data is ready for standardization")
} else {
  print("⚠ Validation warnings detected - review issues")
  
  # Inspect variable map
  print("Variable Map:")
  print(data_obj$variable_map)
  
  # Inspect value map  
  print("Value Map:")
  print(data_obj$value_map)
}

# Proceed to standardization (will re-validate internally)
data_obj$standardize()

# Validate standardized data
data_obj$validate(stage = "standardized")
```

---

## Validation Error Messages

### Common Warnings and Solutions

**"Mapped columns missing: [column_name]"**
- **Cause**: Variable map references a column that doesn't exist
- **Solution**: Update variable_map or add missing column to dataset

**"Value map role '[role]' is not linked to any variable_map entry"**
- **Cause**: Value map defined for a role not in variable_map
- **Solution**: Add corresponding entry to variable_map or remove from value_map

**"Value map references column '[column]' which is missing"**
- **Cause**: Variable map points to non-existent column
- **Solution**: Correct the variable_map entry for this role

**"Value map for role '[role]' has values not found in dataset: [values]"**
- **Cause**: Mapped values don't appear in the actual data
- **Solution**: Either update value_map or verify data contains expected values (may be OK if values are optional variants)
- **Note**: For `select_multiple` variables, validation automatically extracts individual tokens from space-separated values before checking, so "farming fishing" is treated as two tokens: "farming" and "fishing"

**"UUID column '[uuid]' not found in dataset"**
- **Cause**: Critical - primary identifier column is missing
- **Solution**: Verify correct UUID column name or add column to dataset

**"UUID column has missing values"**
- **Cause**: Some rows have NA in the UUID column
- **Solution**: Fill missing UUIDs or remove incomplete rows

**"UUID values are not unique"**
- **Cause**: Duplicate submissions or data merging issues
- **Solution**: Investigate and resolve duplicates, consider using deletion log

---

## Technical Implementation

**File**: `R/class_data.R`

**Method**: `validate()`

**Lines**: ~160-290

**Extension Points**:
- `pre_validate()` - Override in subclasses (line 293)
- `post_validate(df)` - Override in subclasses (line 294)

**Key Helper Functions**:
- `phr_validate_dataframe()` - Core data frame validation
- `phr_validate_columns()` - Required columns check
- `phr_validate_no_missing()` - Missing value check
- `phr_validate_unique()` - Uniqueness check

---

## Related Documentation

- **Schema System**: See `docs/schema_overview.md` for schema structure
- **Variable Mapping**: See `docs/variable_value_mapping_guide.md` for mapping system
- **Standardization**: See `docs/standardization_process.md` for the full pipeline
- **Cleaning**: See `docs/cleaning_process.md` for cleaning and log generation

---

## Summary

The validation process is a comprehensive quality gate that ensures data integrity before standardization or cleaning. By validating both the data structure and the critical variable/value mappings, it prevents downstream errors and provides clear feedback about data quality issues.

The integration of variable and value maps into validation ensures that the mapping layer between schemas and datasets is robust, enabling reliable quality checks, indicator calculations, and cleaning operations throughout the data lifecycle.
