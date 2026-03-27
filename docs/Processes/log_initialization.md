# Log Object Initialization

## Overview

The `Log` class serves as the base class for various logging mechanisms (CleaningLog, DeletionLog, etc.). It provides a standardized structure for tracking operations and issues throughout the data pipeline.

**File**: `R/class_log.R`, lines 27-105

## Initialization Steps

### 1. Log DataFrame Setup
- **Accepts optional log_df**: Can initialize with an existing log data frame or create a new empty one
- **Creates empty template**: If no log provided, creates an empty data frame structure
- **Validates structure**: Ensures the log is a valid data.frame

### 2. Required Columns Enforcement
- **Defines required columns**: Establishes the mandatory column structure for the log
  - Typically includes: timestamp, variable, old_value, new_value, reason, etc.
- **Adds missing columns**: If any required columns are missing, adds them filled with NA
  - Ensures log always has complete structure
  - Prevents downstream errors from missing columns
- **Preserves existing data**: Only adds missing columns, doesn't overwrite existing ones

### 3. Column Ordering
- **Reorders columns**: Places required columns first in a consistent order
  - Makes logs easier to read and work with
  - Ensures consistency across all log objects
- **Maintains additional columns**: Any extra columns are preserved at the end

### 4. Type Conversion
- **Converts to tibble**: Uses `tibble::as_tibble()` to create modern data frame
  - Better printing and display
  - Consistent with tidyverse conventions
  - Preserves all data while improving usability

### 5. Metadata Initialization
- **Creates metadata list**: Initializes `self$metadata` with tracking information
- **Records creation time**: Adds `metadata$created_at = Sys.time()`
- **Sets log name**: Stores `self$log_name` for identification

### 6. Schema Storage
- **Stores required columns**: Saves `self$required_columns` as a reference
- **Stores schema definition**: If provided, saves `self$schema` for validation
- **Extensible structure**: Subclasses can define custom schemas

### 7. Validation and Issues Tracking
- **Sets validation flag**: Initializes `self$validated = FALSE`
- **Creates issues list**: Initializes `self$issues = list()` to track validation problems
  - Empty list indicates no issues found yet
  - Will be populated during validation operations

## Post-Initialization State

After initialization, a Log object has:
- ✓ Properly structured log_df with all required columns
- ✓ Columns in consistent order (required columns first)
- ✓ Tibble format for better display
- ✓ Metadata with creation timestamp
- ✓ Empty issues list ready to track problems
- ✓ Validation flag set to FALSE

## Example

```r
# Create a new Log object (typically done automatically by Data object)
cleaning_log <- CleaningLog$new(
  log_name = "HouseholdSurvey_cleaning"
)

# Object is now initialized with:
# - log_df: empty tibble with required columns
# - log_name: "HouseholdSurvey_cleaning"
# - required_columns: character vector of column names
# - metadata$created_at: current timestamp
# - validated: FALSE
# - issues: list()
```

## Key Design Features

**Flexible Initialization**: Can start with empty log or existing log data
**Self-Repairing**: Automatically adds missing columns to maintain structure
**Consistent Structure**: Enforces column ordering and required fields
**Metadata Tracking**: Records creation and modification history

## Best Practices

1. **Let Data create logs**: Typically created automatically by Data objects
2. **Check structure**: Required columns are enforced automatically
3. **Use provided methods**: Add entries through Log methods, not direct manipulation

## Troubleshooting

### "Missing required columns"
- Automatically fixed during initialization
- Columns added with NA values

### "Log structure invalid"
- Ensure log_df is a data.frame if provided
- Let initialization create empty log if unsure

## Related Documentation

- **Data Class**: See `docs/Data_Structures/data_class.md` for detailed Data class documentation
- **Cleaning Process**: See `docs/Processes/cleaning/cleaning_process.md` for cleaning workflow
- **Object Initialization Overview**: See `docs/Processes/initialization_overview.md` for common patterns
