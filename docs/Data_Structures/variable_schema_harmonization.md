# Variable Schema Method Harmonization

## Overview

This document describes the harmonization of variable schema import/export methods to match the naming conventions used by indicator and dependency schemas.

## Problem Statement

Previously, the Data class and its subclasses had inconsistent naming for schema-related methods:

- **Indicator schema**: Used clearly named methods like `import_indicator_schema()`, `export_indicator_schema()`, and `set_indicator_schema()`
- **Dependency schema**: Used clearly named methods like `import_dependency_schema()`, `export_dependency_schema()`, and `set_dependency_schema()`
- **Variable schema**: Used generic methods like `import_schema()`, `set_schema()`, and `get_schema()`, with **NO export method at all**

This inconsistency made the API confusing and less discoverable.

## Solution

### New Methods Added

The following new methods have been added to the Data class:

1. **`import_variable_schema(df)`** - Import variable schema from a data frame
2. **`export_variable_schema()`** - Export variable schema to a data frame (NEW!)
3. **`set_variable_schema(schema_list)`** - Set variable schema from a list
4. **`get_variable_schema()`** - Get the current variable schema as a list

### Backward Compatibility

The old methods are preserved as backward-compatible wrappers:

- **`import_schema(df)`** - Calls `import_variable_schema()` with a deprecation warning
- **`set_schema(schema_list)`** - Calls `set_variable_schema()` silently (no warning to minimize noise)
- **`get_schema()`** - Calls `get_variable_schema()` silently (no warning to minimize noise)

This ensures existing code continues to work without breaking changes.

### Updated Subclasses

All Data subclasses have been updated to use the new method names:

- `HouseholdData`
- `HouseholdFSLData`
- `HouseholdHealthData`
- `HouseholdMortalityData`
- `HouseholdWASHData`
- `IndividualData`
- `IndividualDeathData`
- `IndividualHealthData`
- `IndividualNutritionData`
- `IndividualWaterContainerData`

## Consistent API

After this harmonization, all three schema types now follow the same naming pattern with import/export/set/get methods:

### Variable Schema
```r
data$import_variable_schema(df)
data$export_variable_schema()
data$set_variable_schema(schema_list)
data$get_variable_schema()
```

### Indicator Schema
```r
data$import_indicator_schema(df)
data$export_indicator_schema()
data$set_indicator_schema(schema_list)
data$get_indicator_schema()
```

### Dependency Schema
```r
data$import_dependency_schema(df)
data$export_dependency_schema()
data$set_dependency_schema(schema_list)
data$get_dependency_schema()
```

**Consistency Achieved**: All three schema types now have complete import/export/set/get methods, providing a uniform API across the entire schema system.

## Migration Guide

### For Package Maintainers

If you maintain code that uses the old deprecated methods, you must update to the new method names:

```r
# Old (deprecated - removed)
data$set_schema(my_schema)
schema <- data$get_schema()

# New (required)
data$set_variable_schema(my_schema)
schema <- data$get_variable_schema()
```

### For New Code

Always use the new method names:

```r
# Setting a variable schema
my_schema <- list(
  types = list(id = "character", value = "numeric"),
  required = c("id")
)
data$set_variable_schema(my_schema)

# Getting the variable schema
current_schema <- data$get_variable_schema()

# Exporting variable schema to a table (NEW!)
schema_table <- data$export_variable_schema()

# Importing variable schema from a table
data$import_variable_schema(schema_table)
```

## Benefits

1. **Consistency**: All schema types now use the same naming pattern
2. **Discoverability**: Method names clearly indicate what type of schema they operate on
3. **Completeness**: Variable schema now has an export method like the other schema types
4. **Backward compatibility**: Existing code continues to work without breaking changes

## Testing

A manual test script has been added at `tests/manual/test_schema_harmonization.R` to verify:

- New methods work correctly
- Backward compatibility is preserved
- Consistency across all schema types
- Export functionality works for variable schema

To run the test:

```r
source("tests/manual/test_schema_harmonization.R")
```

## Related Files

- `R/class_data.R` - Main Data class with new methods and backward-compatible wrappers
- All `R/class_data_*.R` files - All 10 subclasses updated to use `set_variable_schema()` internally
- `tests/manual/test_schema_harmonization.R` - Manual test script
