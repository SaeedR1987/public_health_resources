# Analysis Process

## Overview

The analysis process in iphRa involves calculating indicators, performing statistical analysis, and generating analytical outputs from cleaned and validated survey data. This process transforms standardized data into meaningful indicators and insights.

## Process Flow

```
Standardized & Validated Data
    ↓
Indicator Calculation
    ↓
Statistical Analysis
    ↓
Disaggregation & Subgroup Analysis
    ↓
Confidence Interval Calculation
    ↓
Output Generation & Export
```

## Steps

### 1. Data Preparation

Before analysis, ensure data is:
- Standardized using Data classes
- Validated against schemas
- Quality checked using DataQuality classes
- Cleaned using CleaningLog

```r
# Prepare data
data <- HouseholdData$new(data = df, uuid = "uuid")
data$set_variable_schema(schema)
data$standardize()

# Validate and check quality
quality <- DataQuality$new(data = data)
quality$run_quality_checks()
```

### 2. Indicator Calculation

Calculate domain-specific indicators:

```r
# Food security indicators
data$add_fcs()  # Food Consumption Score
data$add_hdds()  # Household Dietary Diversity Score
data$add_rcsi()  # Reduced Coping Strategy Index

# Nutrition indicators
data$add_muac_categories()
data$add_wfh_zscore()

# Or use indicator schema
data$calculate_indicators()
```

### 3. Create Analysis Object

Initialize the appropriate Analysis class:

```r
# Choose domain-specific analysis class
analysis <- HealthAnalysis$new(
  data = data,
  weights = "survey_weight"
)

# Or for FSL
fsl_analysis <- QuantDataAnalysisFSL$new(
  data = data,
  weights = "survey_weight"
)
```

### 4. Calculate Prevalence and Means

Calculate key statistics:

```r
# Prevalence calculation
prevalence <- analysis$calculate_prevalence(
  variable = "illness_last_2weeks",
  by = c("age_group", "region")
)

# Mean calculation
mean_fcs <- analysis$calculate_mean(
  variable = "fcs_score",
  by = "livelihood_zone"
)

# Distribution analysis
distribution <- analysis$calculate_distribution(
  variable = "fcs_category",
  by = "region"
)
```

### 5. Disaggregation

Analyze results by subgroups:

```r
# Disaggregate by multiple variables
results <- analysis$disaggregate_by(
  indicators = c("illness_prevalence", "treatment_sought"),
  by = c("age_group", "sex", "region", "urban_rural")
)

# Cross-tabulation
cross_tab <- analysis$cross_tabulate(
  var1 = "malnutrition_status",
  var2 = "illness_last_2weeks",
  by = "age_group"
)
```

### 6. Confidence Intervals

Calculate confidence intervals for estimates:

```r
# Add confidence intervals to results
results_with_ci <- analysis$calculate_ci(
  results = results,
  confidence_level = 0.95,
  method = "wilson"  # For proportions
)
```

### 7. Statistical Testing

Perform statistical tests:

```r
# Compare two groups
comparison <- analysis$compare_groups(
  variable = "illness_prevalence",
  group1 = "urban",
  group2 = "rural",
  test = "chi_square"
)

# Test trend over time
trend <- analysis$test_trend(
  variable = "malnutrition_prevalence",
  by = "survey_round"
)
```

### 8. Export Results

Export analytical outputs:

```r
# Export to Excel
analysis$export_results(
  file = "analysis_results.xlsx",
  sheets = c("prevalence", "means", "distribution")
)

# Export specific tables
analysis$export_table(
  table = "prevalence_by_age",
  file = "prevalence.csv"
)

# Export figures
analysis$export_figures(
  figures = c("age_pyramid", "prevalence_map"),
  format = "png"
)
```

## Domain-Specific Analysis

### Health Analysis

```r
health_analysis <- HealthAnalysis$new(data = data, weights = "weight")

# Calculate health indicators
illness_prev <- health_analysis$calculate_illness_prevalence()
treatment_seeking <- health_analysis$calculate_treatment_seeking_rate()
facility_usage <- health_analysis$calculate_facility_utilization()

# Export
health_analysis$export_results("health_results.xlsx")
```

### Nutrition Analysis

```r
nutrition_analysis <- NutritionAnalysis$new(data = data, weights = "weight")

# Calculate nutrition indicators
gam_prev <- nutrition_analysis$calculate_gam_prevalence()
sam_prev <- nutrition_analysis$calculate_sam_prevalence()
stunting_prev <- nutrition_analysis$calculate_stunting_prevalence()

# Export
nutrition_analysis$export_results("nutrition_results.xlsx")
```

### FSL Analysis

```r
fsl_analysis <- QuantDataAnalysisFSL$new(data = data, weights = "weight")

# Calculate FSL indicators
fcs_distribution <- fsl_analysis$calculate_fcs_distribution()
hdds_mean <- fsl_analysis$calculate_hdds_mean()
rcsi_mean <- fsl_analysis$calculate_rcsi_mean()

# Food insecurity prevalence
food_insecurity <- fsl_analysis$calculate_food_insecurity_prevalence()

# Export
fsl_analysis$export_results("fsl_results.xlsx")
```

### WASH Analysis

```r
wash_analysis <- WASHAnalysis$new(data = data, weights = "weight")

# Calculate WASH indicators
water_access <- wash_analysis$calculate_water_access()
sanitation_access <- wash_analysis$calculate_sanitation_access()
handwashing <- wash_analysis$calculate_handwashing_facilities()

# Export
wash_analysis$export_results("wash_results.xlsx")
```

## Weighted Analysis

Survey weights should be applied for representative estimates:

```r
# Set weights when creating analysis object
analysis <- HealthAnalysis$new(
  data = data,
  weights = "survey_weight"
)

# All calculations will use weights
weighted_prevalence <- analysis$calculate_prevalence("illness")
```

## Best Practices

### 1. Always Validate Data First

```r
# Good - validate before analysis
quality <- DataQuality$new(data = data)
quality$run_quality_checks()
report <- quality$get_quality_report()

# Review quality report before proceeding
if (acceptable_quality(report)) {
  analysis <- HealthAnalysis$new(data = data)
}
```

### 2. Use Appropriate Confidence Intervals

```r
# For proportions, use Wilson or exact methods
prevalence_ci <- analysis$calculate_ci(
  results = prevalence,
  method = "wilson"
)

# For means, use t-distribution
mean_ci <- analysis$calculate_ci(
  results = means,
  method = "t"
)
```

### 3. Document Analysis Parameters

```r
# Document analysis settings
analysis_metadata <- list(
  date = Sys.Date(),
  analyst = "analyst_name",
  data_source = "survey_2024",
  weights = "survey_weight",
  confidence_level = 0.95,
  software = "iphRa",
  version = packageVersion("iphRa")
)

# Include in output
analysis$set_metadata(analysis_metadata)
```

### 4. Check Sample Sizes

```r
# Ensure adequate sample size for subgroups
sample_sizes <- analysis$get_sample_sizes(by = c("age_group", "region"))

# Flag subgroups with small n
small_n <- sample_sizes[sample_sizes$n < 30, ]
if (nrow(small_n) > 0) {
  warning("Some subgroups have n < 30")
}
```

### 5. Review and Validate Results

```r
# Compare results to expected ranges
results_summary <- analysis$get_summary()

# Check for unusual values
check_plausibility(results_summary)

# Cross-check with other sources
compare_with_reference(results_summary, reference_data)
```

## Output Formats

### Excel Workbook

Multi-sheet Excel file with:
- Summary sheet with key indicators
- Detailed tables by domain
- Disaggregated results
- Metadata and notes

### CSV Files

Individual CSV files for each table:
- Easy to import into other tools
- Suitable for further processing
- Version control friendly

### JSON Format

Machine-readable format for:
- Web applications
- APIs and integrations
- Automated workflows

## Error Handling

Handle analysis errors gracefully:

```r
tryCatch({
  analysis$calculate_all_indicators()
}, error = function(e) {
  message("Error in analysis: ", e$message)
  # Log error
  log$add_entry(
    type = "error",
    message = e$message,
    context = "indicator_calculation"
  )
})
```

## Related Documentation

- See Analysis Classes overview for class-specific methods
- See Data Classes for data preparation
- See Quality Classes for quality checks before analysis
- See `validation_process.md` for validation requirements
