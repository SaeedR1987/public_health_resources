# QuantDataAnalysis Object Initialization

## Overview

The `QuantDataAnalysis` class provides the framework for quantitative data analysis, including survey-weighted analysis and indicator calculations. Its initialization establishes connections to data sources, loads schemas, and prepares the analysis environment.

**File**: `R/class_quant_data_analysis.R`, lines 6-92

## Initialization Steps

### 1. Survey Design Reference
- **Stores survey design**: If provided, saves `self$survey_design`
  - Survey design object (typically from `survey` package)
  - Contains sampling weights and stratification information
  - Essential for weighted analysis operations
- **Optional parameter**: Can be NULL for unweighted analysis

### 2. Parent Data Object Linkage
- **Stores parent reference**: Saves `self$parent_data_object`
  - Links analysis object to parent Data object
  - Provides access to data at different stages (raw, standardized, clean)
  - Enables access to variable_map and value_map
- **Critical relationship**: Parent provides data source and mapping context

### 3. Dataset Identification
- **Extracts dataset name**: Uses `parent_data_object$dataset_name`
- **Stores name**: Sets `self$dataset_name` for reporting
- **Consistent naming**: Ensures analysis results are properly attributed

### 4. Data Stage Tracking
- **Determines data stage**: Identifies which data version is being analyzed
  - `"raw"`: Original imported data
  - `"standardized"`: Type-converted and enriched data
  - `"clean"`: Fully cleaned and validated data
- **Stores stage info**: Sets both `self$data_stage` and `self$data_stage_name`
- **Traceability**: Ensures analysis results are linked to specific data version

### 5. Data Fingerprinting
- **Calculates data hash**: Creates `self$data_hash` from current data
- **Change detection**: Hash enables detection of data modifications
- **Analysis validity**: Helps ensure analysis matches data state

### 6. Mapping Structures
- **Copies variable_map**: Stores `self$variable_map` from parent
  - Maps canonical variable names to dataset columns
  - Essential for analysis functions that use standard names
- **Copies value_map**: Stores `self$value_map` from parent
  - Maps canonical values to dataset-specific values
  - Critical for categorical variable analysis
- **Snapshot approach**: Creates point-in-time copy of mappings

### 7. Analysis Schema Loading
- **Attempts template loading**: Tries to load analysis schema from resources
  - Path: `system.file("resources", "quant_analysis_template.json", package = "public_health_resources")`
  - Template defines available analysis functions and parameters
- **Error handling**: If template unavailable, creates empty schema
  - `self$analysis_schema = list()` as fallback
  - Allows custom schema to be set later
- **Function registry**: Schema defines what analysis operations are available

### 8. Data Analysis Plan (DAP) Setup
- **Attempts DAP template loading**: Tries to load default DAP from resources
  - Path: `system.file("resources", "data_analysis_plan_template.csv", package = "public_health_resources")`
  - DAP defines which analyses to run and their parameters
- **Accepts provided DAP**: If `data_analysis_plan` parameter provided, uses it instead
- **Creates empty DAP**: If neither available, creates empty tibble
  - `self$data_analysis_plan = tibble::tibble()`
- **Flexible planning**: DAP can be customized or loaded from various sources

### 9. Results Initialization
- **Creates results container**: Initializes `self$results = tibble::tibble()`
  - Will store results from executed analyses
  - Structured as tibble for easy manipulation
- **Creates issue log**: Initializes `self$analysis_plan_issue_log = tibble::tibble()`
  - Tracks problems encountered during analysis
  - Records failed analyses, missing variables, invalid parameters

### 10. Analysis Preparation
- **Ready state**: Object is prepared to execute analyses
- **Schema-driven**: Analyses defined by loaded or custom schema
- **Plan-based execution**: Analyses run according to DAP specifications

## Post-Initialization State

After initialization, a QuantDataAnalysis object has:
- ✓ Survey design object stored (if weighted analysis)
- ✓ Link to parent Data object
- ✓ Dataset name and data stage tracking
- ✓ Data hash for change detection
- ✓ Variable and value maps copied from parent
- ✓ Analysis schema loaded from template or empty
- ✓ Data analysis plan loaded from template or parameter
- ✓ Empty results and issue log tibbles

## Example

```r
# Create a QuantDataAnalysis object linked to a Data object
analysis_obj <- QuantDataAnalysis$new(
  parent_data_object = data_obj,
  survey_design = my_survey_design
)

# Object is now initialized with:
# - survey_design: my_survey_design object
# - parent_data_object: link to data_obj
# - dataset_name: extracted from data_obj
# - data_stage: "standardized" or "clean"
# - data_hash: hash of current data
# - variable_map: copied from data_obj
# - value_map: copied from data_obj
# - analysis_schema: loaded from template
# - data_analysis_plan: loaded from template or parameter
# - results: empty tibble
# - analysis_plan_issue_log: empty tibble
```

## Key Design Features

**Survey-Aware**: Supports weighted analysis through survey design
**Multi-Stage Support**: Works with raw, standardized, or clean data
**Parent Integration**: Accesses mappings and data from parent object
**Template-Based**: Loads default schemas and DAPs from resources
**Flexible Planning**: Supports custom or template-based analysis plans
**Comprehensive Tracking**: Records results and issues systematically

## Best Practices

1. **Provide parent**: Always link to parent_data_object
2. **Use survey design**: Include survey_design for weighted analysis
3. **Set up DAP**: Provide or customize data_analysis_plan
4. **Match data stage**: Ensure analysis uses appropriate data stage (clean preferred)

## Troubleshooting

### "Parent data object required"
- QuantDataAnalysis must have parent_data_object
- Ensure Data object exists first

### "Survey design invalid"
- Verify survey_design is from survey package
- Can be NULL for unweighted analysis

### "DAP template not found"
- Normal if package resources not installed
- Analysis object still created with empty DAP
- Provide custom DAP via parameter or set later

## Related Documentation

- **Data Class**: See `docs/Data_Structures/data_class.md` for detailed Data class documentation
- **Analysis Process**: See `docs/Processes/analysis/analysis_process.md` for analysis workflow
- **Object Initialization Overview**: See `docs/Processes/initialization_overview.md` for common patterns
