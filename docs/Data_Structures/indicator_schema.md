# Indicator Schema Implementation

## Overview

The indicator schema is a **separate schema structure** from the main data schema that enables automatic computation of derived indicators during data standardization. This feature allows dynamic calculation of composite indicators (like FCS, HHS, rCSI, HDDS) by calling predefined `add_*` functions with configurable arguments.

## Key Design Decision

The indicator schema is **separate and independent** from the main data schema:

- **Main Schema** (`self$schema`): Defines variable types, validation rules, dependencies, etc.
- **Indicator Schema** (`self$indicator_schema`): Defines indicators to be computed during standardization

This separation provides:
- Cleaner architecture with focused schemas
- Independent management of indicators vs. data validation
- Simpler structure with only indicator-relevant fields

## Indicator Schema Structure

### Fields

The indicator schema uses a minimal, focused field set:

| Field | Required | Purpose | Example |
|-------|----------|---------|---------|
| `indicator_name` | Yes | Unique identifier | `fsl_fcs_indicator` |
| `function_name` | Yes | Function to call (must start with `add_`) | `add_fcs` |
| `variables` | Yes | Required variables (comma-separated) | `fsl_fcs_cereal,fsl_fcs_legumes,...` |
| `arguments` | No | Function arguments (key=value pairs) | `cutoffs=normal,fsl_fcs_cereal=@variable_map$fsl_fcs_cereal` |
| `label` | No | Human-readable description | `Food Consumption Score calculation` |
| `comment` | No | Additional notes | `Computes FCS score and category` |

### Schema List Structure

```r
indicator_schema <- list(
  indicator_name = list(
    indicator_name = "indicator_name",
    function_name = "add_function_name",
    variables = c("var1", "var2", "var3"),
    arguments = list(
      arg1 = "value1",
      arg2 = "@variable_map$role"
    ),
    label = "Optional label",
    comment = "Optional comment"
  )
)
```

## New Functions

Three new exported functions handle indicator schema conversions:

### `indicator_schema_to_table()`

Converts an indicator schema list to a flat table format.

```r
indicator_schema <- list(
  fcs_ind = list(
    indicator_name = "fcs_ind",
    function_name = "add_fcs",
    variables = c("fsl_fcs_cereal", "fsl_fcs_legumes"),
    arguments = list(cutoffs = "normal")
  )
)

table <- indicator_schema_to_table(indicator_schema)
# Returns a tibble with columns: indicator_name, function_name, variables, arguments, label, comment
```

### `indicator_table_to_schema()`

Converts an indicator table back to a schema list.

```r
indicator_table <- tibble::tibble(
  indicator_name = "fcs_ind",
  function_name = "add_fcs",
  variables = "fsl_fcs_cereal,fsl_fcs_legumes",
  arguments = "cutoffs=normal",
  label = "FCS Indicator",
  comment = NA
)

schema <- indicator_table_to_schema(indicator_table)
# Returns nested list structure
```

### `indicator_validate_table_to_schema()`

Validates an indicator table before conversion.

```r
is_valid <- indicator_validate_table_to_schema(indicator_table)
```

## Data Class Methods

The Data class now has dedicated methods for indicator schema:

### `import_indicator_schema()`

Import indicator schema from a table.

```r
d <- Data$new(data = my_data, uuid = "id")
d$import_indicator_schema(indicator_table)
```

### `export_indicator_schema()`

Export the current indicator schema to a table.

```r
indicator_table <- d$export_indicator_schema()
```

### `set_indicator_schema()`

Directly set the indicator schema list.

```r
d$set_indicator_schema(indicator_schema_list)
```

## Dynamic Argument Resolution

The indicator system supports dynamic argument resolution using special prefixes:

### Variable Map References

Use `@variable_map$role` to reference columns via the Data object's variable_map:

```r
# In indicator schema:
arguments = list(fsl_fcs_cereal = "@variable_map$fsl_fcs_cereal")

# Resolves to actual column name:
fsl_fcs_cereal = "actual_column_name_in_dataset"
```

### Value Map References

Use `@value_map$role` to reference standardized values:

```r
# In indicator schema:
arguments = list(yes_values = "@value_map$consent$yes")

# Resolves to:
yes_values = c("yes", "y", "1", "oui")
```

### Vector Arguments

Arguments can be specified as vectors using `c(...)` syntax with multiple `@value_map` or `@variable_map` references:

```r
# In indicator schema table (arguments column):
arguments = "improved_val=c(@value_map$wash_water_source$piped_dwelling,@value_map$wash_water_source$tap,@value_map$wash_water_source$borehole)"

# Resolves to a character vector:
improved_val = c("piped_dwelling", "tap", "borehole", "piped_dwelling_local", ...)
# (each @value_map reference may resolve to multiple values)
```

The parser respects parentheses, so commas inside `c(...)` are not treated as argument separators. Each element within the vector is resolved independently:
- `@variable_map$role` → resolves to column name
- `@value_map$role$canonical` → resolves to mapped value(s) for that canonical value
- Literal strings → used as-is (quotes are optional)

Example with multiple vector arguments:

```r
arguments = "source_col=@variable_map$wash_water_source,improved=c(@value_map$wash_water_source$piped_dwelling,@value_map$wash_water_source$tap),unimproved=c(@value_map$wash_water_source$unprotected_well)"

# Resolves to:
source_col = "actual_water_source_column"
improved = c("piped_dwelling", "tap", ...)
unimproved = c("unprotected_well", ...)
```

## Execution Flow

1. **Standardization Phase**: During `Data$standardize()`, after type conversion and value standardization
2. **Indicator Processing**: After `self$standardized_data` is assigned
3. **Function Calling**: For each indicator in `self$indicator_schema`:
   - Check if function exists
   - Prepare arguments (resolve @variable_map and @value_map references)
   - Call function with `.dataset` as first argument
   - Update `self$standardized_data` with result

## Usage Example

```r
# Create indicator schema table
indicator_table <- tibble::tibble(
  indicator_name = "fcs_calc",
  function_name = "add_fcs",
  variables = "fsl_fcs_cereal,fsl_fcs_legumes,fsl_fcs_veg,fsl_fcs_fruit,fsl_fcs_meat,fsl_fcs_dairy,fsl_fcs_sugar,fsl_fcs_oil",
  arguments = "cutoffs=normal,fsl_fcs_cereal=@variable_map$fsl_fcs_cereal,fsl_fcs_legumes=@variable_map$fsl_fcs_legumes,fsl_fcs_veg=@variable_map$fsl_fcs_veg,fsl_fcs_fruit=@variable_map$fsl_fcs_fruit,fsl_fcs_meat=@variable_map$fsl_fcs_meat,fsl_fcs_dairy=@variable_map$fsl_fcs_dairy,fsl_fcs_sugar=@variable_map$fsl_fcs_sugar,fsl_fcs_oil=@variable_map$fsl_fcs_oil",
  label = "FCS Calculation",
  comment = "Computes Food Consumption Score and category"
)

# Create Data object
d <- FSLHouseholdData$new(data = my_data)

# Import indicator schema
d$import_indicator_schema(indicator_table)

# Standardize (indicators will be automatically computed)
d$standardize()

# Access results
fcs_scores <- d$standardized_data$fsl_fcs_score
fcs_categories <- d$standardized_data$fsl_fcs_cat
```

## Creating New `add_*` Functions

To create a new indicator function:

1. **Function Signature**: Must accept `.dataset` as first parameter
2. **Return Value**: Must return a modified data frame
3. **Naming Convention**: Must start with `add_`
4. **Parameter Names**: Use descriptive names that match variable roles

Example:

```r
add_my_indicator <- function(.dataset,
                             input_var = "my_var",
                             threshold = 10) {
  
  origin <- "add_my_indicator"
  
  iphra_try({
    # Validate inputs
    iphra_validate_dataframe(.dataset, origin, soft = FALSE)
    iphra_validate_columns(.dataset, input_var, origin, soft = FALSE)
    
    # Compute indicator
    .dataset$my_indicator_score <- ifelse(
      .dataset[[input_var]] > threshold,
      "high",
      "low"
    )
    
    return(.dataset)
    
  }, on_error = "abort", origin = origin)
}
```

## Error Handling

The indicator system handles errors gracefully:

- **Missing Function**: Warning logged, standardization continues
- **Missing Function Name**: Warning logged, indicator skipped
- **Function Error**: Warning logged via `iphra_try`, standardization continues
- **Invalid Return Value**: Warning logged if function doesn't return data frame

## Testing

Tests are located in:
- `tests/testthat/test-indicator_schema.R` - Indicator schema conversion tests
- `tests/testthat/test-data_class_indicator_schema.R` - Data class indicator execution tests

## Implementation Details

### Files Modified

1. **R/utils_data_class.R**:
   - Added `indicator_schema_to_table()` function
   - Added `indicator_table_to_schema()` function
   - Added `indicator_validate_table_to_schema()` function

2. **R/class_data.R**:
   - Added `indicator_schema` field
   - Added `import_indicator_schema()` method
   - Added `export_indicator_schema()` method
   - Added `set_indicator_schema()` method
   - Added indicator processing in `standardize()` method

3. **Tests**:
   - Created `test-indicator_schema.R` for schema function tests
   - Created `test-data_class_indicator_schema.R` for Data class tests

### Backward Compatibility

The implementation is fully backward compatible:
- New `indicator_schema` field is separate from existing `schema`
- Existing code works unchanged
- Indicator schema is optional

## Comparison with Main Schema

| Feature | Main Schema | Indicator Schema |
|---------|-------------|------------------|
| **Purpose** | Define variables, types, validation | Define computed indicators |
| **Field** | `self$schema` | `self$indicator_schema` |
| **Import Method** | `import_schema()` | `import_indicator_schema()` |
| **Export Method** | N/A (uses schema_to_table helper) | `export_indicator_schema()` |
| **Set Method** | `set_schema()` | `set_indicator_schema()` |
| **Fields** | 20+ (types, ranges, patterns, etc.) | 6 (focused on indicators only) |
| **Integration** | Deeply integrated with validation | Separate, executed after standardization |

## Future Enhancements

Potential future improvements:
1. Add support for conditional indicator execution
2. Support indicator dependencies (execute in specific order)
3. Add indicator metadata (version, author, date)
4. Support for indicator validation rules
5. Caching of indicator results
