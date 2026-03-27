# Data Object Initialization

## Overview

The `Data` class serves as the base class for all data structures in iphRa. Its initialization process establishes the foundation for data validation, standardization, and cleaning operations.

**File**: `R/class_data.R`, lines 64-185

## Initialization Steps

### 1. Input Validation
- **Validates data frame**: Ensures the provided data is a valid data.frame structure
- **Checks UUID parameter**: Enforces that an explicit UUID column name is provided (required parameter)
- **Structural validation**: Confirms the data frame has valid dimensions and structure

### 2. Core Data Storage
- **Stores raw data**: Creates `self$raw_data` as an immutable copy of the original data
- **Initializes data slots**: Sets up `self$standardized_data` and `self$clean_data` as NULL (to be populated later)
- **Sets dataset name**: Stores `self$dataset_name` for identification and logging

### 3. UUID Configuration
- **Validates UUID column existence**: Checks that the specified UUID column exists in the data
- **Sets UUID reference**: Stores the UUID column name in `self$uuid`
- **Primary identifier**: Establishes UUID as the primary key for all subsequent operations

### 4. Metadata Initialization
- **Creates metadata list**: Initializes `self$metadata` as an empty list
- **Adds timestamp**: Records creation time with `metadata$created_at = Sys.time()`
- **Tracks updates**: Adds `metadata$last_updated = Sys.time()`
- **Extensible structure**: Metadata can be extended with additional fields by subclasses

### 5. Log Objects Creation
- **Creates CleaningLog**: Initializes `self$cleaning_log` as a new `CleaningLog` R6 object
  - Linked to the parent Data object
  - Ready to track cleaning operations
- **Creates DeletionLog**: Initializes `self$deletion_log` as a new `DeletionLog` R6 object
  - Linked to the parent Data object
  - Ready to track deleted records

### 6. Mapping Structures
- **Initializes variable_map**: Creates `self$variable_map` as an empty list
  - Maps canonical variable names (roles) to actual dataset column names
  - Example: `list(age = "respondent_age", sex = "person_gender")`
- **Initializes value_map**: Creates `self$value_map` as an empty list
  - Maps canonical categorical values to dataset-specific values
  - Supports both nested and flat formats

### 7. Schema Placeholder
- **Initializes required_columns**: Creates `self$required_columns` as an empty character vector
  - Will be populated when schema is set via `set_variable_schema()`
  - Used for validation checks

### 8. Validation Flags
- **Sets initial validation state**: All validation flags set to FALSE
  - `self$validated = FALSE`
  - `self$standardized = FALSE`
  - `self$cleaned = FALSE`
- **State tracking**: Flags indicate which operations have been completed successfully

## Post-Initialization State

After initialization, a Data object has:
- ✓ Validated structure with raw data stored
- ✓ UUID column identified and validated
- ✓ Associated CleaningLog and DeletionLog objects
- ✓ Empty but ready-to-use mapping structures
- ✓ Metadata tracking creation and update times
- ✓ Validation flags set to FALSE (operations pending)

## Example

```r
# Create a new Data object
data_obj <- Data$new(
  data = survey_data,
  dataset_name = "HouseholdSurvey",
  uuid = "submission_id"
)

# Object is now initialized with:
# - raw_data: original survey_data
# - dataset_name: "HouseholdSurvey"
# - uuid: "submission_id"
# - cleaning_log: empty CleaningLog object
# - deletion_log: empty DeletionLog object
# - variable_map: list()
# - value_map: list()
# - validated: FALSE
```

## Best Practices

1. **Always provide explicit UUID**: Required parameter, must be provided
2. **Set schema early**: Use `set_variable_schema()` soon after initialization
3. **Map variables**: Call `map_schema_vars()` to populate variable_map
4. **Validate before operations**: Run `validate()` before standardization/cleaning

## Troubleshooting

### "UUID column not found"
- Ensure the UUID parameter matches an actual column name in the data
- Check for typos in column name

### "Data must be a data frame"
- Verify data is loaded correctly
- Convert to data.frame if needed: `as.data.frame(data)`

## Related Documentation

- **Data Class**: See `docs/Data_Structures/data_class.md` for detailed Data class documentation
- **Validation Process**: See `docs/Processes/validation/validation_process.md` for validation workflow
- **Naming Conventions**: See `docs/naming_conventions.md` for naming standards
- **Object Initialization Overview**: See `docs/Processes/initialization_overview.md` for common patterns
