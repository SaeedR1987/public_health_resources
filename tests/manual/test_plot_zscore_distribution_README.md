# Manual Test: plot_zscore_distribution

## Overview

This manual test demonstrates the refactored `plot_zscore_distribution` function with comprehensive examples using dummy data. The function has been improved to be more generalizable and flexible.

## What Changed

The function was refactored to:
- **Remove hardcoded indices**: Accepts any z-score column name (not limited to wfhz, hfaz, wfaz, mfaz)
- **Remove flags parameter**: Users specify exact column names (e.g., "wfhz" or "wfhz_noflag")
- **Dynamic reference lines**: Customizable vertical lines via `vline_intercepts` and `vline_colors`
- **Custom labels**: Added `x_label` parameter for x-axis customization

## Test File

**test_plot_zscore_distribution.R**
- Comprehensive demonstration of the refactored function
- 10 examples covering different use cases
- Generates visual outputs for inspection
- Uses simple dummy data for easy understanding

## Running the Test

From the iphRa project root directory:

```r
# Option 1: Source the script in R
source("tests/manual/test_plot_zscore_distribution.R")

# Option 2: Run from command line
Rscript tests/manual/test_plot_zscore_distribution.R
```

## Examples Included

### Example 1: Basic Usage
Simple z-score plot with default settings.

```r
plot_zscore_distribution(df, zscore_var = "wfhz")
```

### Example 2: Custom Labels
Adding custom title and x-axis label.

```r
plot_zscore_distribution(
  df, 
  zscore_var = "wfhz",
  x_label = "Weight-for-Height Z-Score",
  title_name = "Distribution Analysis"
)
```

### Example 3: Grouping
Density curves colored by grouping variable.

```r
plot_zscore_distribution(
  df, 
  zscore_var = "wfhz",
  grouping = "district"
)
```

### Example 4: Custom Reference Lines
Only show ±2 SD lines with custom colors.

```r
plot_zscore_distribution(
  df, 
  zscore_var = "wfhz",
  vline_intercepts = c(-2, 2),
  vline_colors = c("blue", "blue")
)
```

### Example 5: No Reference Lines
Plot without vertical reference lines.

```r
plot_zscore_distribution(
  df, 
  zscore_var = "wfhz",
  vline_intercepts = NULL
)
```

### Example 6: Flagged vs Non-Flagged
Comparing distributions with and without outliers.

```r
# With flagged values
plot_zscore_distribution(df, zscore_var = "wfhz")

# Without flagged values
plot_zscore_distribution(df, zscore_var = "wfhz_noflag")
```

### Example 7: Non-Anthropometric Z-Scores
Using the function for other types of z-scores (e.g., income).

```r
plot_zscore_distribution(
  df, 
  zscore_var = "income_zscore",
  vline_intercepts = c(-1.96, 1.96),
  vline_colors = c("green", "green")
)
```

### Example 8: Different Color Palettes
Testing various color palettes for grouped plots.

```r
plot_zscore_distribution(
  df, 
  zscore_var = "wfhz",
  grouping = "district",
  color_palette = "reach1"
)
```

### Example 9: Multiple Reference Lines
Using many reference lines with custom colors.

```r
plot_zscore_distribution(
  df, 
  zscore_var = "wfhz",
  vline_intercepts = c(-3, -2, -1, 0, 1, 2, 3),
  vline_colors = c("red", "orange", "yellow", "black", "yellow", "orange", "red")
)
```

### Example 10: Realistic Anthropometric Data
Simulating realistic anthropometric indices with multiple groupings.

```r
# Multiple indices (wfhz, hfaz, wfaz)
# Multiple groupings (age_group, survey_round)
```

## Output

The test script generates PNG files in `tests/manual/output/`:

- `01_basic_zscore_plot.png` - Basic usage
- `02_custom_labels.png` - Custom labels
- `03_grouped_by_district.png` - Grouped plot
- `04_custom_reference_lines.png` - Custom ±2 SD lines
- `05_no_reference_lines.png` - No reference lines
- `06a_with_flags.png` / `06b_without_flags.png` - Comparison
- `07_income_zscore.png` - Non-anthropometric example
- `08a_palette_reach1.png` / `08b_palette_reach2.png` - Color palettes
- `09_multiple_reference_lines.png` - Many reference lines
- `10a_wfhz_by_age.png` / `10b_hfaz_by_age.png` / `10c_wfaz_by_round.png` - Realistic data

## Migration from Old API

### Old Usage (Before Refactoring)
```r
# Limited to specific indices
plot_zscore_distribution(df, index = "wfhz", flags = "yes")
plot_zscore_distribution(df, index = "wfhz", flags = "no")
```

### New Usage (After Refactoring)
```r
# Any z-score column
plot_zscore_distribution(df, zscore_var = "wfhz")
plot_zscore_distribution(df, zscore_var = "wfhz_noflag")

# With custom features
plot_zscore_distribution(
  df, 
  zscore_var = "custom_zscore",
  vline_intercepts = c(-2, 2),
  x_label = "Custom Z-Score"
)
```

## Benefits Demonstrated

1. **Flexibility**: Works with any z-score column
2. **Simplicity**: Direct column specification (no flags parameter)
3. **Customization**: Control over reference lines, colors, and labels
4. **Generalizability**: Not limited to anthropometric indices
5. **Clarity**: Better parameter names and documentation

## Data Requirements

The function expects:
- A data frame with at least one numeric column containing z-scores
- Column values should ideally be between -6 and 6 (typical z-score range)
- NA values are handled automatically
- Optional grouping column for stratified plots

## Notes

- All examples use synthetic dummy data
- Plots are automatically saved to `tests/manual/output/`
- The script is self-contained and doesn't require external data files
- Visual inspection recommended for all generated plots

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
