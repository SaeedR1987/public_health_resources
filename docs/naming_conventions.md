# Naming Conventions in public_health_resources

## Overview

This document describes the naming conventions used throughout the public_health_resources codebase to ensure consistency, clarity, and maintainability.

## Variable and Column Naming

### Dummy Columns (Generated Variables)

Dummy columns created from select_multiple expansion use **period (`.`)** as a separator:
- `skills.reading`, `skills.writing`, `skills.other`
- `livelihood.farming`, `livelihood.fishing`, `livelihood.trading`

**Benefits:**
1. **Clear Distinction**: Period clearly marks columns as generated dummy variables
2. **No Ambiguity**: `skills.other` is clearly a dummy, `skills_other_text` is clearly original data
3. **Pattern Matching**: Easier to identify dummy columns with `\\.` pattern vs `_` pattern
4. **Namespace Separation**: Original data columns and generated columns are clearly separated

### Text Columns (Original Data)

Text "other" columns use **underscore (`_`)** as separator:
- `skills_other_text`
- `skills_other_specify`
- `skills_other_value`
- `water_source_other_text`

### Standard Variable Names

Standard variable names use **underscore (`_`)** as word separator:
- `uuid`, `household_id`, `interview_date`
- `respondent_age`, `respondent_gender`
- `fsl_fcs_score`, `fsl_hdds_score`

## R6 Class Naming

### Base Classes

Base classes use PascalCase without prefixes:
- `Data` - Base class for all data structures
- `DataAnalytics` - Base class for quality and quantitative analytics
- `Log` - Base class for logging

### Subclasses

Subclasses use PascalCase with descriptive suffixes:
- `HouseholdData`, `IndividualData`
- `WomenIndividualData`, `HealthIndividualData`
- `FSLDataAnalytics`, `HealthDataAnalytics`
- `CleaningLog`, `DeletionLog`

## Function Naming

### Public Methods

Public methods use snake_case:
- `set_variable_schema()`
- `standardize()`
- `add_fcs()`
- `run_quality_checks()`

### Private Methods

Private methods use snake_case with a leading dot:
- `.validate_schema()`
- `.check_required_columns()`
- `.expand_select_multiple()`

### Utility Functions

Utility functions use snake_case with descriptive prefixes:
- `validate_*()` - Validation functions
- `check_*()` - Check functions
- `get_*()` - Getter functions
- `set_*()` - Setter functions
- `add_*()` - Functions that add indicators or features

## File Naming

### R Source Files

R source files use snake_case:
- `class_data.R` - Base class definitions
- `class_data_household.R` - Subclass definitions
- `utils_data_class.R` - Utility functions
- `validators.R` - Validation functions

### Test Files

Test files use snake_case with `test-` prefix:
- `test-schema_enhancements.R`
- `test-select_multiple_other.R`
- `test-data_class.R`

### Documentation Files

Documentation files use snake_case or kebab-case:
- `naming_conventions.md`
- `error_handling.md`
- `validation_process.md`

## Schema Field Naming

### Variable Schema Fields

Schema fields use snake_case:
- `question_types`
- `value_map`
- `allowed_values`
- `col_names`
- `date_validity`

### Indicator Schema Fields

Indicator schema fields use snake_case:
- `indicator_name`
- `function_name`
- `variables`
- `arguments`

## Pattern Matching

### Dummy Column Pattern

Use escaped period for matching dummy columns:
```r
if (grepl("\\.other$", col_name)) {
  # This is a dummy column
}
```

### Text Column Pattern

Use underscore patterns for matching text columns:
```r
if (grepl("_other_text$|_other_specify$|_other_value$", col_name)) {
  # This is a text column
}
```

## Examples

### Complete Example

```r
# Data with various column types
df <- data.frame(
  # Standard columns (underscore)
  uuid = "id_1",
  respondent_age = 25,
  
  # Select multiple original column
  skills = "reading other",
  
  # Text column (underscore)
  skills_other_text = "programming"
)

# After standardization, dummy columns use period
# Standard columns: uuid, respondent_age, skills
# Dummy columns: skills.reading, skills.other
# Text columns: skills_other_text
```

### Migration from Old Convention

If you have code that references old dummy column names with underscores:

**Old (no longer works):**
```r
df$skills_reading
df$skills_other
```

**New (use period):**
```r
df$skills.reading
df$skills.other

# Or use bracket notation if column name is in a variable
col <- "skills.reading"
df[[col]]
```

## Technical Details

### Column Name Composition

- **Base name**: The original variable name (e.g., `skills`)
- **Separator**: Period for generated dummy columns, underscore for original columns
- **Suffix**: The value label (e.g., `reading`, `other`)

### Column Detection Logic

```r
# Detect dummy columns
is_dummy <- grepl("\\.", col_name) && 
            all(unique(df[[col_name]]) %in% c(0, 1, "0", "1"))

# Detect text columns
is_text <- grepl("_other_text$|_other_specify$|_other_value$", col_name)

# Detect standard columns
is_standard <- !is_dummy && !is_text
```

## Best Practices

1. **Consistency**: Always use the appropriate separator for the column type
2. **Clarity**: Choose descriptive names that clearly indicate the purpose
3. **Avoid Conflicts**: Don't create column names that could be confused with generated columns
4. **Documentation**: Document any deviation from these conventions with clear comments
5. **Pattern Matching**: Use the appropriate pattern for the column type when searching or filtering

## Related Documentation

- See `schema_overview.md` for schema structure details
- See `standardization_process.md` for how naming conventions are applied
- See `variable_value_mapping_guide.md` for value mapping conventions
