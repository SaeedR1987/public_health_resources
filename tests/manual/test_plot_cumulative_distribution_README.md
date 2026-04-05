# Manual Test: plot_cumulative_distribution

## Overview

This manual test demonstrates the refactored `plot_cumulative_distribution` function with comprehensive examples using dummy data. The function has been improved to be more generalizable and flexible, following the same pattern as `plot_zscore_distribution`.

## What Changed

The function was refactored to:
- **Remove hardcoded indices**: Accepts any numeric column name (not limited to wfhz, hfaz, wfaz, mfaz, muac)
- **Remove flags parameter**: Users specify exact column names (e.g., "wfhz" or "wfhz_noflag")
- **Dynamic reference lines**: Customizable vertical lines via `vline_intercepts` and `vline_colors`
- **Custom labels and limits**: Added `x_label`, `xlim`, and `breaks_by` parameters
- **Auto-detection**: Automatically detects MUAC vs z-score data based on column name for appropriate defaults

## Test File

**test_plot_cumulative_distribution.R**
- Comprehensive demonstration of the refactored function
- 11 examples covering different use cases
- Generates visual outputs for inspection
- Uses simple dummy data for easy understanding

## Running the Test

From the public_health_resources project root directory:

```r
# Option 1: Source the script in R
source("tests/manual/test_plot_cumulative_distribution.R")

# Option 2: Run from command line
Rscript tests/manual/test_plot_cumulative_distribution.R
```

## Examples Included

### Example 1: Basic Usage with Z-Score Data
Simple cumulative distribution plot with default settings.

```r
plot_cumulative_distribution(df, data_var = "wfhz")
```

### Example 2: Custom Labels
Adding custom title and x-axis label.

```r
plot_cumulative_distribution(
  df, 
  data_var = "wfhz",
  x_label = "Weight-for-Height Z-Score",
  title_name = "Cumulative Distribution Analysis"
)
```

### Example 3: Grouping
Cumulative curves colored by grouping variable.

```r
plot_cumulative_distribution(
  df, 
  data_var = "wfhz",
  grouping = "district"
)
```

### Example 4: MUAC Data with Auto-Detection
Automatically detects MUAC data and sets appropriate defaults.

```r
plot_cumulative_distribution(
  df, 
  data_var = "muac"
)
```

### Example 5: MUAC with Grouping
MUAC data grouped by age.

```r
plot_cumulative_distribution(
  df, 
  data_var = "muac",
  grouping = "age_group"
)
```

### Example 6: Custom Reference Lines
WHO standard cutoffs with custom colors.

```r
plot_cumulative_distribution(
  df, 
  data_var = "wfhz",
  vline_intercepts = c(-3, -2, -1, 2),
  vline_colors = c("red", "orange", "yellow", "orange")
)
```

### Example 7: No Reference Lines
Plot without vertical reference lines.

```r
plot_cumulative_distribution(
  df, 
  data_var = "wfhz",
  vline_intercepts = NULL
)
```

### Example 8: Flagged vs Non-Flagged
Comparing distributions with and without outliers.

```r
# With flagged values
plot_cumulative_distribution(df, data_var = "wfhz")

# Without flagged values
plot_cumulative_distribution(df, data_var = "wfhz_noflag")
```

### Example 9: Custom Axis Limits and Breaks
Control over x-axis range and tick marks.

```r
plot_cumulative_distribution(
  df, 
  data_var = "wfhz",
  xlim = c(-4, 4),
  breaks_by = 1
)
```

### Example 10: Different Color Palettes
Testing various color palettes for grouped plots.

```r
plot_cumulative_distribution(
  df, 
  data_var = "wfhz",
  grouping = "district",
  color_palette = "reach1"
)
```

### Example 11: Realistic Anthropometric Data
Simulating realistic anthropometric indices with multiple groupings.

```r
# Multiple indices (wfhz, hfaz, muac)
# Multiple groupings (age_group, survey_round)
```

## Output

The test script generates PNG files in `tests/manual/output/`:

- `cumulative_01_basic_zscore.png` - Basic z-score usage
- `cumulative_02_custom_labels.png` - Custom labels
- `cumulative_03_grouped_by_district.png` - Grouped plot
- `cumulative_04_muac_autodetect.png` - MUAC with auto-detection
- `cumulative_05_muac_grouped.png` - MUAC grouped by age
- `cumulative_06_custom_reference_lines.png` - Custom WHO cutoffs
- `cumulative_07_no_reference_lines.png` - No reference lines
- `cumulative_08a_with_flags.png` / `cumulative_08b_without_flags.png` - Comparison
- `cumulative_09_custom_axis.png` - Custom axis settings
- `cumulative_10a_palette_reach1.png` / `cumulative_10b_palette_reach3.png` - Color palettes
- `cumulative_11a_wfhz_by_age.png` / `cumulative_11b_hfaz_by_age.png` / `cumulative_11c_muac_by_round.png` - Realistic data

## Migration from Old API

### Old Usage (Before Refactoring)
```r
# Limited to specific indices with flags parameter
plot_cumulative_distribution(df, index = "wfhz", flags = "yes")
plot_cumulative_distribution(df, index = "wfhz", flags = "no")
plot_cumulative_distribution(df, index = "muac", flags = "yes")
```

### New Usage (After Refactoring)
```r
# Any numeric column
plot_cumulative_distribution(df, data_var = "wfhz")
plot_cumulative_distribution(df, data_var = "wfhz_noflag")
plot_cumulative_distribution(df, data_var = "muac")

# With custom features
plot_cumulative_distribution(
  df, 
  data_var = "custom_indicator",
  vline_intercepts = c(10, 20),
  vline_colors = c("red", "green"),
  xlim = c(0, 100),
  x_label = "Custom Indicator Value"
)
```

## Benefits Demonstrated

1. **Flexibility**: Works with any numeric column
2. **Simplicity**: Direct column specification (no flags parameter)
3. **Customization**: Control over reference lines, colors, labels, and axis limits
4. **Auto-detection**: Intelligently sets defaults based on data type (MUAC vs z-scores)
5. **Generalizability**: Not limited to anthropometric indices
6. **Clarity**: Better parameter names and documentation

## Data Requirements

The function expects:
- A data frame with at least one numeric column
- Column values appropriate for cumulative distribution (continuous numeric data)
- NA values are handled automatically
- Optional grouping column for stratified plots

## Notes

- All examples use synthetic dummy data
- Plots are automatically saved to `tests/manual/output/`
- The script is self-contained and doesn't require external data files
- Visual inspection recommended for all generated plots
- MUAC data is auto-detected based on column name containing "muac" (case-insensitive)

## For Developers

When testing new features or modifications:

1. Run the full test script to ensure backward compatibility
2. Inspect all generated plots visually
3. Add new examples if introducing new parameters
4. Update this README with any new functionality

## References

- Function definition: `R/utils_quality_analyisis_outputs.R`
- Unit tests: `tests/testthat/test-utils_quality_analysis_outputs.R`
- Automated tests run as part of CI/CD pipeline
- Related function: `plot_zscore_distribution` (similar refactoring pattern)
