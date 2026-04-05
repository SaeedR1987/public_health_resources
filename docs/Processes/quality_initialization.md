# DataQuality Object Initialization

## Overview

The `DataQuality` class provides comprehensive quality assessment capabilities for Data objects. Its initialization establishes the framework for running quality checks, storing results, and generating quality reports.

**File**: `R/class_data_quality.R`, lines 34-119

## Initialization Steps

### 1. Data Validation and Storage
- **Validates data parameter**: Ensures data is provided and is a valid data.frame
  - Throws error if data is NULL or not a data frame
  - Critical for quality assessment operations
- **Stores data reference**: Saves `self$data` for quality checks
- **Validates structure**: Confirms data has rows and columns

### 2. Parent Object Linkage
- **Stores parent reference**: If provided, saves `self$parent_data_object`
  - Links quality object to its parent Data object
  - Enables access to variable_map, value_map, and schema
  - Allows coordination between quality checks and data operations
- **Optional linkage**: Parent can be NULL for standalone quality objects

### 3. Dataset Identification
- **Extracts dataset name**: If parent exists, uses `parent$dataset_name`
- **Fallback naming**: If no parent, uses `"DataQuality"` as default
- **Stores name**: Sets `self$dataset_name` for reporting and logging

### 4. Metadata Initialization
- **Creates rich metadata**: Initializes comprehensive tracking information
  - `metadata$created_at = Sys.time()`: Creation timestamp
  - `metadata$n_records`: Number of rows in data
  - `metadata$n_columns`: Number of columns in data
  - `metadata$parent_name`: Name of parent Data object (if available)
  - `metadata$data_hash`: Hash of data for change detection
- **Change tracking**: Data hash enables detection of modifications

### 5. Schema Loading
- **Attempts template loading**: Tries to load quality schema from package resources
  - Path: `system.file("resources", "data_quality_template.json", package = "public_health_resources")`
  - Template defines default quality checks and thresholds
- **Error handling**: If template unavailable, creates empty schema
  - `self$quality_schema = list()` as fallback
  - Allows custom schema to be set later
- **Flexible configuration**: Schema can be customized after initialization

### 6. Results Initialization
- **Creates results container**: Initializes `self$results = list()`
  - Will store results from each quality check
  - Structure: named list where names are check identifiers
- **Creates summary stats container**: Initializes `self$summary_stats = list()`
  - Will store aggregated statistics across checks
  - Example: pass rates, error counts, threshold violations
- **Sets overall score**: Initializes `self$overall_score = NA_real_`
  - Will be calculated after quality checks run
  - NA indicates quality assessment not yet performed

### 7. Quality Check Preparation
- **Ready state**: Object is prepared to run quality checks
- **Schema-driven**: Quality checks defined by loaded or custom schema
- **Extensible**: Additional checks can be defined in subclasses

## Post-Initialization State

After initialization, a DataQuality object has:
- ✓ Validated data reference stored
- ✓ Link to parent Data object (if provided)
- ✓ Rich metadata including record counts and data hash
- ✓ Quality schema loaded from template or empty
- ✓ Empty results and summary_stats structures
- ✓ Overall score set to NA (pending assessment)

## Example

```r
# Create a DataQuality object linked to a Data object
quality_obj <- DataQuality$new(
  data = data_obj$standardized_data,
  parent_data_object = data_obj
)

# Object is now initialized with:
# - data: reference to standardized_data
# - parent_data_object: link to data_obj
# - dataset_name: extracted from data_obj
# - metadata: creation time, row/column counts, data hash
# - quality_schema: loaded from template
# - results: list()
# - summary_stats: list()
# - overall_score: NA
```

## Key Design Features

**Parent Awareness**: Maintains link to parent Data object for context
**Template-Based**: Loads default quality checks from package resources
**Comprehensive Metadata**: Tracks detailed information for reporting
**Result Storage**: Structured containers for check results and statistics
**Extensible**: Schema and checks can be customized per use case

## Best Practices

1. **Link to parent**: Always provide parent_data_object when possible
2. **Use standardized data**: Quality checks work best on standardized data
3. **Custom schemas**: Extend quality_schema for domain-specific checks
4. **Review metadata**: Check n_records and data_hash for sanity

## Troubleshooting

### "Data parameter required"
- DataQuality requires data parameter
- Cannot be NULL or missing

### "Template not found"
- Normal if package resources not installed
- Quality object still created with empty schema
- Provide custom schema via `quality_obj$quality_schema <- custom_schema`

## Related Documentation

- **Data Class**: See `docs/Data_Structures/data_class.md` for detailed Data class documentation
- **Quality Checks**: See `docs/Processes/quality_checks/` for quality assessment details
- **Object Initialization Overview**: See `docs/Processes/initialization_overview.md` for common patterns
