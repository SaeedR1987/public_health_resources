# Data Standardization Process

## Overview

The standardization process is the core data transformation pipeline in the public_health_resources package. It converts raw survey data into a consistent, validated, and analysis-ready format. This document provides a high-level overview of the standardization workflow and its key components.

## Purpose

Standardization ensures that:
- Data types are correctly assigned and safely coerced
- Categorical values are harmonized across different survey implementations
- Variables are mapped to their canonical schema definitions
- Quality checks are automatically performed
- Derived indicators are computed consistently
- The dataset is ready for downstream cleaning and analysis

## The Standardization Pipeline

The standardization process follows a structured pipeline with three main phases:

```
Raw Data → pre_standardize → standardize → post_standardize → Standardized Data
```

Each phase serves a distinct purpose and can be extended by subclasses for domain-specific transformations.

---

## Phase 1: pre_standardize

**Purpose**: Prepare and augment the raw data before standardization begins.

**Key Activities**:

### 1. Linked Dataset Processing (HouseholdData)
- Checks if any linked datasets (roster, deaths, water containers, etc.) exist
- Ensures all linked datasets are standardized before proceeding
- Automatically calls `standardize()` on linked datasets if needed

### 2. Data Aggregation
- Aggregates data from linked datasets to create household-level summary columns
- Example: Calculate household size from roster data
- Example: Sum total deaths from mortality data
- Example: Calculate demographics (children under 5, women 15-49, etc.)

### 3. Column Addition
- Adds aggregated columns to `raw_data` with naming convention `linked_<name>_<column>`
- Example: `linked_roster_hh_size`, `linked_deaths_total_deaths`
- These columns become available for the main standardization phase

### 4. Variable Mapping Update
- **NEW**: Calls `map_schema_vars(stage = "raw")` after pre_standardize completes
- Automatically maps any newly added columns to their schema definitions
- Ensures variable_map and value_map include pre-standardize additions

**Extension Point**: Subclasses can override `pre_standardize()` to implement custom pre-processing logic.

---

## Phase 2: standardize (Main Phase)

**Purpose**: Transform raw data into standardized format through type conversion, validation, and enrichment.

**Key Activities**:

### 1. Validation Check
- Runs `validate()` to ensure data structure is sound
- Checks for required columns and data integrity
- Issues warnings if validation fails but allows proceeding with caution

### 2. Column-by-Column Processing

For each column in the dataset:

#### a. Schema-Based Type Conversion
- If column is defined in `variable_schema$types`, convert to schema type
- Supported types: `numeric`, `character`, `logical`, `date`
- Uses safe coercion with validation to prevent data loss
- Special handling for logical values (yes/no, true/false, 1/0)
- Date parsing with flexible format recognition

#### b. Inference-Based Type Conversion
- For non-schema columns, infer appropriate type from data
- Apply safe coercion if type can be reliably determined
- Fallback to trimmed character type if uncertain

#### c. "Other" Column Detection
- Identifies open-text "other" response columns using heuristics
- Looks for naming patterns: `_other_text`, `_other_specify`, `_autre`
- Detects columns with high blank percentage but unique responses
- Links other columns to their parent question columns

### 3. Select Multiple Column Expansion
- Processes `select_multiple` question types from schema
- Expands space-separated multi-response values into separate dummy columns
- Creates binary indicator columns for each possible response
- Example: `"water source"` = `"tap borehole"` → `water_source_tap = 1, water_source_borehole = 1`
- Tracks "other" responses within select_multiple questions

### 4. Schema-Identified "Other" Columns
- Adds columns explicitly marked as `is_other = TRUE` in schema
- Links them to parent columns using `other_column_link` from schema
- Ensures all "other" columns are tracked for cleaning log generation

### 5. Indicator Processing
- Processes indicators defined in `indicator_schema` in sequence
- Each indicator specifies a function to call (e.g., `add_fcs_score`)
- Functions receive the dataset and compute derived variables
- Supports variable and value map references in indicator arguments
- Example: Food Consumption Score, MUAC classification, GAM prevalence
- Adds computed indicator columns to the dataset
- **NEW**: Calls `map_schema_vars(stage = "standardized")` after EACH indicator
  - Ensures newly added columns are immediately mapped
  - Allows subsequent indicators to reference variables from previous indicators
  - Supports column priority updates if preferred names are added

### 6. Standardized Data Assignment
- Assigns processed data to `self$standardized_data`
- Sets `self$standardized = TRUE` flag

### 7. Variable Mapping Update
- **NEW**: Calls `map_schema_vars(stage = "standardized")` after standardization
- Maps columns added by type conversion, select_multiple expansion, and indicators
- Ensures all new columns are linked to schema definitions

### 8. Quality Checks
- Automatically runs `run_quality_checks()` if schema exists
- Generates data quality flags for validation rules
- Checks dependency rules, type coercion issues, allowed values
- Creates flag columns (e.g., `flag_age_range`, `flag_consent_required`)

**Extension Point**: While the main standardize method cannot be overridden, type inference and coercion logic can be extended through helper functions.

---

## Phase 3: post_standardize

**Purpose**: Apply domain-specific transformations after standardization is complete.

**Key Activities**:

### 1. Domain-Specific Indicators
- Subclasses can compute indicators that depend on standardized data
- Example: Calculate mortality rates that require standardized demographics
- Example: Create composite indices from multiple standardized variables

### 2. Data Enrichment
- Add classification columns based on standardized thresholds
- Apply complex business logic that requires clean, typed data
- Create derived grouping or stratification variables

### 3. Final Transformations
- Any transformations that must occur after quality checks
- Modifications that depend on the complete standardized dataset

### 4. Variable Mapping Update
- **NEW**: Calls `map_schema_vars(stage = "standardized")` after post_standardize
- Maps any columns added during post-standardize phase
- Ensures final dataset has complete schema mappings

**Extension Point**: Subclasses can override `post_standardize()` to implement custom post-processing logic.

---

## Variable and Value Mapping

Throughout the standardization pipeline, the `map_schema_vars()` method keeps variable_map and value_map synchronized with the dataset:

### Automatic Mapping Points

1. **Initialization**: Maps variables when the Data object is created
2. **After pre_standardize**: Maps columns added during pre-processing
3. **After EACH indicator**: Maps columns from each indicator function
4. **After main standardization**: Final mapping pass for any remaining columns
5. **After post_standardize**: Maps columns from post-processing

### Mapping Behavior

- **Preference-aware**: Checks for more preferred column names (earlier in `col_names` list)
- **Updates when better match exists**: If a more preferred column is added, mapping is updated
- **Schema-driven**: Uses `col_names` field from variable_schema to find columns
- **Value-aware**: Maps categorical values for non-numeric types
- **Synchronized**: value_map is updated whenever variable_map changes
- **Stage-appropriate**: Uses correct data stage (`raw` or `standardized`)

### Benefits

- Columns added at any point are automatically discoverable immediately
- Subsequent indicators can reference variables from previous indicators
- Quality checks and indicators can reference newly added variables
- Cleaning logs correctly identify all relevant columns
- Users don't need to manually update mappings
- More preferred column names are automatically adopted when added

---

## Data Flow Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                         Raw Data                                 │
│                    (from initialization)                         │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                      PRE_STANDARDIZE                             │
├─────────────────────────────────────────────────────────────────┤
│  • Process linked datasets                                       │
│  • Aggregate data (household size, demographics, etc.)          │
│  • Add aggregated columns to raw_data                           │
├─────────────────────────────────────────────────────────────────┤
│  → map_schema_vars(stage = "raw")                               │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                        STANDARDIZE                               │
├─────────────────────────────────────────────────────────────────┤
│  • Validate data structure                                       │
│  • Type conversion (schema-based + inference)                   │
│  • Detect and track "other" columns                             │
│  • Expand select_multiple columns                               │
│  • Add schema "other" columns                                   │
│  • Process indicators from indicator_schema:                    │
│    - For each indicator:                                        │
│      * Call add_* function                                      │
│      * Add computed columns to dataset                          │
│      * → map_schema_vars(stage = "standardized")                │
│    - Allows indicators to depend on previous indicators         │
├─────────────────────────────────────────────────────────────────┤
│  → Assign to standardized_data                                  │
│  → map_schema_vars(stage = "standardized") [final pass]         │
│  → Run quality checks                                           │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                     POST_STANDARDIZE                             │
├─────────────────────────────────────────────────────────────────┤
│  • Domain-specific indicators                                    │
│  • Data enrichment                                              │
│  • Final transformations                                        │
├─────────────────────────────────────────────────────────────────┤
│  → map_schema_vars(stage = "standardized")                      │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Standardized Data                             │
│        (ready for cleaning and analysis)                         │
└─────────────────────────────────────────────────────────────────┘
```

---

## Type Conversion Details

### Schema-Based Conversion

When a column is defined in `variable_schema$types`, the standardize process attempts to convert it to the specified type:

- **numeric**: Uses `as.numeric()` with warning suppression
- **character**: Uses `trimws(as.character())` to clean whitespace
- **logical**: Maps various representations to TRUE/FALSE
  - TRUE: "true", "t", "yes", "y", "1"
  - FALSE: "false", "f", "no", "n", "0"
  - NA: any other value
- **date**: Uses `phr_convert_date()` for flexible date parsing

### Safe Coercion

Before converting, the process checks if coercion is "safe" using `.is_safely_coercible()`:
- Validates that conversion won't lose critical information
- Issues warnings for unsafe conversions
- Leaves data unchanged if conversion would be destructive

### Inference-Based Conversion

For columns not in the schema, the process infers the best type:
- Examines data patterns and content
- Applies conservative coercion rules
- Defaults to character type if uncertain

---

## Quality Checks Integration

After standardization completes, automatic quality checks run if schemas are defined:

1. **Type Checks**: Flags rows where type coercion failed
2. **Dependency Rules**: Evaluates logical rules from dependency_schema
3. **Value Validation**: Checks against allowed_values from variable_schema
4. **Pattern Matching**: Validates regex patterns from schema

Quality flags are appended as columns to `standardized_data` with prefix `flag_`.

---

## Best Practices

### For Package Users

1. **Define comprehensive schemas**: Include types, allowed values, and col_names
2. **Use indicator_schema**: Compute derived variables during standardization
3. **Leverage pre_standardize**: Add aggregated data from linked datasets
4. **Check quality flags**: Review generated quality flags before cleaning
5. **Trust the pipeline**: Let standardization handle type conversion automatically

### For Package Developers

1. **Override hooks carefully**: pre_standardize and post_standardize should be focused
2. **Preserve data**: Never delete or fundamentally alter raw_data in pre_standardize
3. **Add, don't replace**: Augment standardized_data, don't replace existing columns
4. **Document transformations**: Clearly document what your hooks add or modify
5. **Test thoroughly**: Ensure hooks work with various data formats and edge cases

### For Schema Authors

1. **Include col_names**: List alternative column names for auto-mapping
2. **Define types explicitly**: Specify expected types for all key variables
3. **Map categorical values**: Use value_map for multi-language or variant values
4. **Define indicators**: Use indicator_schema for computed variables
5. **Specify dependencies**: Define quality rules in dependency_schema

---

## Example Workflow

```r
# Create household data object with raw data
hh <- HouseholdData$new(
  data = raw_survey_data,
  dataset_name = "MyHouseholdSurvey",
  uuid = "submission_uuid"
)

# Set schema (defines types, values, indicators)
hh$set_variable_schema(household_schema)
hh$set_indicator_schema(household_indicators)
hh$set_dependency_schema(household_dependencies)

# Auto-map variables based on schema
hh$map_schema_vars()

# Link related datasets
hh$add_linked_dataset("roster", roster_data)
hh$add_linked_dataset("deaths", mortality_data)

# Run standardization pipeline
# This executes: pre_standardize → standardize → post_standardize
hh$standardize()

# Check results
summary(hh$standardized_data)
View(hh$variable_map)   # All columns mapped, including new ones
View(hh$value_map)      # All categorical values mapped

# Quality flags are automatically created
flag_cols <- grep("^flag_", names(hh$standardized_data), value = TRUE)
print(flag_cols)

# Generate cleaning log from quality flags
hh$generate_cleaning_log(stage = "standardized")

# Proceed to cleaning
hh$clean()
```

---

## Related Documentation

- **Schema System**: See `docs/schema_overview.md` for schema structure details
- **Variable Mapping**: See `docs/variable_value_mapping_guide.md` for mapping system
- **Indicator Schema**: See `docs/indicator_schema.md` for indicator definitions
- **Quality Checks**: See `docs/dependency_schema_enhancements.md` for quality rules
- **Cleaning Process**: See `docs/cleaning_log_enhancements.md` for cleaning workflow

---

## Technical Implementation

**File**: `R/class_data.R`

**Method**: `standardize()`

**Lines**: ~302-723

**Key Helper Functions**:
- `.is_safely_coercible()` - Type conversion safety check
- `phr_infer_column_type()` - Type inference from data
- `phr_convert_date()` - Flexible date parsing
- `process_select_multiple_columns()` - Select multiple expansion
- `map_schema_vars()` - Variable and value mapping

**Extension Points**:
- `pre_standardize()` - Override in subclasses (line 726)
- `post_standardize()` - Override in subclasses (line 727)
- `post_standardize_domain()` - Domain-specific hook (line 729)

---

## Version History

- **v1.0**: Initial standardization pipeline
- **v1.1**: Added select_multiple support and "other" column tracking
- **v1.2**: Integrated indicator schema processing
- **v1.3**: Added automatic quality checks integration
- **v1.4**: Added automatic variable mapping at pre/post standardize points (current)

---

## Summary

The standardization process is a comprehensive, extensible pipeline that transforms raw survey data into a consistent, validated, analysis-ready format. By leveraging schemas, automatic mapping, and extension hooks, it provides both powerful automation and flexibility for domain-specific requirements.

The three-phase approach (pre_standardize → standardize → post_standardize) with automatic variable mapping ensures that all data transformations are properly tracked and integrated with the schema system, making the data lifecycle transparent and maintainable.
