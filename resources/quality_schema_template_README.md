# Quality Schema Template Guide - Logical Threshold Expressions

## Overview

The `quality_schema_template.xlsx` provides a template for defining statistical plausibility tests using **logical threshold expressions**. This approach allows flexible, interpretable quality checks with multiple complementary penalty levels.

## Key Concept

Quality checks use **logical expressions** to evaluate test results, not fixed min/max values. For example:
- `"p_value >= 0.05"` - Test is not significant (0 penalty)
- `"p_value < 0.05"` - Test is significant (15 penalty)
- `"test_statistic >= 0.7"` - Strong correlation (0 penalty)
- `"test_statistic >= 0.5 & test_statistic < 0.7"` - Moderate correlation (5 penalty)

## Template Structure

### Columns

1. **check_name** - Unique identifier for the check
   - Same check_name can appear on multiple rows (one per threshold)
   - Example: `fcs_mean_plausibility`

2. **check_label** - Human-readable description
   - Example: "FCS mean is plausible (expected around 45)"

3. **variables** - Comma-separated variable names
   - Variables needed from the dataset
   - Example: `fsl_fcs_score,household_income`

4. **statistical_test** - Name of statistical test function
   - Must correspond to a function in `utils_quality_tests.R`
   - Example: `correlation`, `ttest`, `chisq`, `missing_percentage`

5. **threshold_expression** - Logical expression to evaluate
   - Can reference `test_statistic` and `p_value`
   - Uses R logical operators: `>=`, `<=`, `>`, `<`, `&`, `|`
   - Example: `"p_value > 0.01 & p_value < 0.05"`

6. **penalty_score** - Penalty points if threshold is met
   - Numeric value, typically 0 or more
   - 0 = passing, higher values = worse quality
   - Example: `0`, `5`, `10`, `20`

7. **test_params** - Test-specific parameters (optional)
   - Comma-separated key=value pairs
   - Example: `method=pearson`, `mu=45`, `z_threshold=3`

### Multiple Rows Per Check

**Important**: A single check can (and should) have multiple rows, each with a different threshold expression and penalty. The first matching threshold is used.

Example:
```
check_name           | threshold_expression              | penalty_score
---------------------|-----------------------------------|---------------
fcs_income_corr      | test_statistic >= 0.7            | 0
fcs_income_corr      | test_statistic >= 0.5 & < 0.7    | 5
fcs_income_corr      | test_statistic >= 0.3 & < 0.5    | 10
fcs_income_corr      | test_statistic < 0.3             | 20
```

## Logical Expression Examples

### Simple Comparisons
- `test_statistic >= 0.7` - Greater than or equal
- `p_value < 0.05` - Less than
- `test_statistic <= 15` - Less than or equal

### Compound Expressions (using &)
- `test_statistic >= 0.5 & test_statistic < 0.7` - Between 0.5 and 0.7
- `p_value > 0.01 & p_value <= 0.05` - Marginally significant
- `test_statistic <= 5 | test_statistic >= 95` - Either tail

### Complex Expressions
- `(test_statistic >= 0.7 | p_value < 0.01) & test_statistic >= 0` - Combined conditions
- `test_statistic > 10 & test_statistic < 20` - Range check

## Available Statistical Tests

### correlation
- Returns: `test_statistic` (correlation coefficient, -1 to 1)
- Test params: `method=pearson|spearman|kendall`
- Variables: 2

### ttest
- Returns: `test_statistic` (t-value), `p_value`
- Test params: `mu=<expected_mean>`, `paired=TRUE|FALSE`
- Variables: 1 or 2

### chisq
- Returns: `test_statistic` (chi-squared), `p_value`
- Test params: None
- Variables: 2 (categorical)

### flag_percentage
- Returns: `test_statistic` (percentage, 0-100)
- Test params: `flag_value=<value_to_count>`
- Variables: 1

### missing_percentage
- Returns: `test_statistic` (percentage, 0-100)
- Test params: None
- Variables: 1 or more

### outlier_percentage
- Returns: `test_statistic` (percentage, 0-100)
- Test params: `z_threshold=<number>`
- Variables: 1

### coefficient_variation
- Returns: `test_statistic` (CV percentage)
- Test params: None
- Variables: 1

### range_violation
- Returns: `test_statistic` (percentage, 0-100)
- Test params: `min_value=<number>`, `max_value=<number>`
- Variables: 1

### sd
- Returns: `test_statistic` (standard deviation)
- Test params: None
- Variables: 1

### sd_across_percentage
- Returns: `test_statistic` (percentage, 0-100)
- Test params: `threshold=<number>` (default: 0.8)
- Variables: 2 or more
- Description: Percentage of rows where SD across specified columns falls below threshold

### any_flag_percentage
- Returns: `test_statistic` (percentage, 0-100)
- Test params: `flag_value=<value>` (default: 1)
- Variables: 1 or more
- Description: Percentage of rows where at least one variable has the flag value

## Usage Workflow

### 1. Create/Modify Template

1. Open `quality_schema_template.xlsx`
2. Add rows for each check-threshold combination
3. Define logical threshold expressions
4. Set penalty scores (0 = passing)
5. Save the file

### 2. Import in R

```r
library(public_health_resources)
library(openxlsx)

# Read template
quality_table <- read.xlsx("resources/quality_schema_template.xlsx")

# Convert to nested list schema
quality_schema <- quality_table_to_schema(quality_table)

# Create DataQuality object
dq <- DataQuality$new(
  data = my_data,
  quality_schema = quality_schema
)

# Run checks
dq$run_quality_checks()

# Generate report
report <- dq$generate_plausibility_report()

# View results
print(report$plausibility_score)
print(report$test_results)
```

### 3. Understand Results

Each check evaluation:
1. Runs the statistical test (e.g., correlation, t-test)
2. Extracts `test_statistic` and `p_value` (if applicable)
3. Evaluates each threshold expression in order
4. Uses the penalty from the **first TRUE expression**
5. Records which expression matched

**Plausibility Score**: `100 - (total_penalty / max_possible_penalty * 100)`

## Example Schemas

### Example 1: Correlation with 4 Levels

```
check_name: fcs_income_correlation
statistical_test: correlation
test_params: method=pearson

Threshold 1: test_statistic >= 0.7        → penalty: 0   (excellent)
Threshold 2: test_statistic >= 0.5 & < 0.7 → penalty: 5   (good)
Threshold 3: test_statistic >= 0.3 & < 0.5 → penalty: 10  (fair)
Threshold 4: test_statistic < 0.3          → penalty: 20  (poor)
```

### Example 2: T-Test with P-Value

```
check_name: fcs_mean_plausibility
statistical_test: ttest
test_params: mu=45

Threshold 1: p_value >= 0.05  → penalty: 0   (not significant - plausible)
Threshold 2: p_value < 0.05   → penalty: 15  (significant - questionable)
```

### Example 3: Missing Data with 4 Levels

```
check_name: key_variables_completeness
statistical_test: missing_percentage

Threshold 1: test_statistic <= 5              → penalty: 0   (excellent)
Threshold 2: test_statistic > 5 & <= 10       → penalty: 5   (acceptable)
Threshold 3: test_statistic > 10 & <= 20      → penalty: 15  (poor)
Threshold 4: test_statistic > 20              → penalty: 25  (critical)
```

## Best Practices

### 1. Define Complementary Thresholds
Ensure thresholds cover all possible outcomes:
- ✓ Good: `>= 0.7`, `>= 0.5 & < 0.7`, `< 0.5`
- ✗ Bad: `>= 0.7`, `< 0.5` (gap: 0.5 to 0.7)

### 2. Order Matters
Put more specific conditions first:
```
# Correct order:
test_statistic >= 0.7          # Check this first
test_statistic >= 0.5          # Then this
test_statistic < 0.5           # Finally this

# Wrong order:
test_statistic >= 0.5          # Would match 0.8, never reaching >= 0.7
test_statistic >= 0.7          # Unreachable!
```

### 3. Use Clear Expressions
- Keep expressions simple and readable
- Use parentheses for clarity: `(a > 5) & (b < 10)`
- Document intent in check_label

### 4. Penalty Scoring
- 0 = passing/acceptable
- 5-10 = minor concern
- 15-20 = moderate concern
- 25+ = critical issue
- Balance penalties across checks

### 5. Test Incrementally
- Start with one check
- Verify it works
- Add more checks gradually

## Troubleshooting

### Expression Never Matches
- Check variable names: `test_statistic`, `p_value` (not `statistic` or `pvalue`)
- Verify logical operators: `>=` not `=>`, `&` not `AND`
- Check for gaps in threshold coverage

### Unexpected Penalties
- Verify threshold order (first match wins)
- Check for overlapping conditions
- Ensure complementary ranges

### Evaluation Errors
- Expressions must be valid R code
- Can only reference `test_statistic` and `p_value`
- Check for typos in variable names

### Missing p_value
- Not all tests return p-values
- Use `!is.na(p_value) & p_value < 0.05` to check if p_value exists

### List Name Mismatch Error
- If you see "Check list name does not match check_name field value"
- The list name in `checks$<name>` must exactly match the `check_name` field
- Example: `checks$completeness = list(check_name = "completeness", ...)`
- This ensures proper round-trip conversion between table and schema formats

## Storage Format

### In Excel/Table
- Multiple rows per check
- One row = one threshold

### In R (Nested List)

**Important**: The list name (e.g., `fcs_corr` in `checks$fcs_corr`) **must** be identical to the `check_name` field value within that check. This ensures consistency when converting between table and schema formats.

```r
quality_schema <- list(
  metadata = list(version = "3.0.0"),
  checks = list(
    fcs_corr = list(
      check_name = "fcs_corr",  # Must match the list name!
      check_label = "FCS-Income Correlation",
      variables = c("fsl_fcs_score", "household_income"),
      statistical_test = "correlation",
      thresholds = list(
        list(expression = "test_statistic >= 0.7", penalty = 0),
        list(expression = "test_statistic >= 0.5 & test_statistic < 0.7", penalty = 5),
        list(expression = "test_statistic < 0.5", penalty = 10)
      ),
      test_params = list(method = "pearson")
    )
  )
)
```

## Related Files

- `R/class_data_quality.R` - DataQuality class with expression evaluation
- `R/utils_quality_tests.R` - Statistical test functions
- `R/utils_data_class.R` - Schema conversion utilities (table ↔ list)

## Support

For questions, refer to the public_health_resources package documentation or GitHub repository.
