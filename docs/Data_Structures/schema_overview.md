# Schema System Overview

## Introduction

The iphRa package uses a comprehensive schema system to define, validate, and standardize survey data. This document provides an overview of the different schema types used in the system, their structure, interactions, and practical usage.

## Schema Types

The iphRa schema system consists of four main schema types:

1. **Variable Schema** - Defines variable types, validation rules, and value mappings
2. **Dependency Schema** - Defines logical quality check rules between variables
3. **Indicator Schema** - Defines computed indicators to be calculated during standardization
4. **Quality Schema** - Defines statistical plausibility tests for data quality assessment

Each schema serves a specific purpose and can be used independently or in combination with others.

---

## 1. Variable Schema

### Purpose

The variable schema is the core schema that defines:
- Variable data types (character, numeric, date, etc.)
- Required variables
- Allowed values for categorical variables
- Value mappings (canonical values → dataset-specific values)
- Question types (select_one, select_multiple, text, etc.)
- Column name alternatives for auto-mapping
- Data validation rules (patterns, ranges, uniqueness constraints)
- Date validation rules
- "Other" column identification

### Structure

The variable schema is stored as a nested list with the following components:

```r
variable_schema <- list(
  # Core components
  required = c("uuid", "age"),                    # Required variables
  types = list(                                   # Data types
    uuid = "character",
    age = "numeric",
    gender = "character",
    consent = "logical"
  ),
  
  # Value mapping (canonical → dataset values)
  value_map = list(
    gender = list(
      male = c("male", "m", "1"),
      female = c("female", "f", "2")
    ),
    consent = list(
      yes = c("yes", "y", "1", "oui"),
      no = c("no", "n", "0", "non")
    )
  ),
  
  # Question types from xlsform/ODK
  question_types = list(
    gender = "select_one",
    livelihood = "select_multiple",
    age = "integer"
  ),
  
  # Allowed values (for validation)
  allowed_values = list(
    gender = c("male", "female", "other")
  ),
  
  # Column name alternatives for auto-mapping
  col_names = list(
    uuid = c("id", "uuid", "survey_id"),
    age = c("age", "age_years", "respondent_age")
  ),
  
  # Validation rules
  patterns = list(
    phone = "^\\+?[0-9]{10,15}$"
  ),
  ranges = list(
    age = c(0, 120)
  ),
  precision_limits = list(
    height_cm = 1
  ),
  
  # Uniqueness constraints
  unique = c("uuid"),
  
  # Mutually exclusive groups
  mutually_exclusive = list(
    employment = c("employed", "unemployed", "retired")
  ),
  
  # Date validation
  date_validity = list(
    interview_date = list(not_future = TRUE)
  ),
  
  # "Other" column identification
  is_other = list(
    water_source_other = TRUE
  ),
  other_column_link = list(
    water_source_other = c("water_source")
  )
)
```

### Table Format

Variable schemas can be imported/exported as tables (e.g., Excel files):

| rule_type | variable | value | required | type | question_type | is_other | other_column_link | allowed | col_names | pattern | range | precision_limits | unique | mutex_group | not_future | label | comment |
|-----------|----------|-------|----------|------|---------------|----------|-------------------|---------|-----------|---------|-------|-----------------|--------|-------------|------------|-------|---------|
| variable | uuid | NA | TRUE | character | NA | NA | NA | NA | id,uuid | NA | NA | NA | TRUE | NA | NA | Unique ID | Survey ID |
| variable | gender | male | FALSE | character | select_one | NA | NA | male,m,1 | NA | NA | NA | NA | NA | NA | NA | Gender | Male values |
| variable | gender | female | FALSE | character | select_one | NA | NA | female,f,2 | NA | NA | NA | NA | NA | NA | NA | Gender | Female values |

**Key columns:**
- `rule_type`: Always "variable" for variable rows
- `variable`: Variable name (canonical/role name)
- `value`: Canonical value (for value_map entries; one row per canonical value)
- `type`: Data type (character, numeric, date, logical)
- `question_type`: xlsform question type
- `allowed`: Comma-separated list of allowed dataset values for this canonical value
- `col_names`: Alternative column names for auto-mapping

### Key Features

1. **Value Mapping**: Maps canonical values to dataset-specific values
   - Enables standardization across different data collection formats
   - One variable can have multiple canonical values (e.g., gender: male, female, other)
   - Each canonical value maps to multiple dataset values

2. **Select Multiple Expansion**: Automatically expands space-separated values into dummy columns
   - `livelihood = "farming fishing"` → `livelihood.farming = 1, livelihood.fishing = 1`

3. **"Other" Column Detection**: Identifies and links "other" text columns to their main question
   - Used for cleaning log generation
   - Supports both schema-defined and auto-detected "other" columns

4. **Auto-mapping**: Alternative column names help automatically map schema to dataset columns

### Usage

```r
# Import from table
library(openxlsx)
schema_table <- read.xlsx("resources/household_variable_schema.xlsx")
schema_list <- data_table_to_schema(schema_table)

# Set schema on Data object
d <- Data$new(data = my_data, uuid = "survey_id")
d$set_variable_schema(schema_list)

# Standardize data (applies variable schema)
d$standardize()

# Export schema to table
schema_table <- data_schema_to_table(d$get_variable_schema())
```

### Related Functions

- `data_table_to_schema()` - Convert table to schema list
- `data_schema_to_table()` - Convert schema list to table
- `data_validate_table_to_schema()` - Validate table before conversion
- `data_validate_schema_to_table()` - Validate schema list before export

---

## 2. Dependency Schema

### Purpose

The dependency schema defines logical quality check rules that specify relationships between variables. These rules are used to:
- Flag data quality issues
- Identify missing data patterns
- Validate logical consistency
- Generate cleaning logs

### Structure

The dependency schema uses a named list structure where each dependency is identified by a unique name:

```r
dependency_schema <- list(
  dependencies = list(
    other_water_source = list(
      variables = c("water_source", "water_source_other"),
      condition_if = "water_source == 'other'",
      then = "!is.na(water_source_other)",
      action = "flag_autoclean"
    ),
    child_age_months = list(
      variables = c("age", "age_months"),
      condition_if = "age < 5",
      then = "!is.na(age_months)",
      action = "flag_warning"
    ),
    pregnant_weeks = list(
      variables = c("pregnant", "pregnant_weeks"),
      condition_if = "pregnant == 'yes'",
      then = "pregnant_weeks >= 0 & pregnant_weeks <= 42",
      action = "flag_autoclean"
    )
  ),
  
  soft_dependencies = list(
    hh_composition = list(
      variables = c("hh_size", "num_children", "num_adults"),
      condition_if = "!is.na(hh_size)",
      then = "(num_children + num_adults) == hh_size",
      action = "flag_analysis"
    )
  )
)
```

### Fields

| Field | Required | Description | Example |
|-------|----------|-------------|---------|
| `variables` | Yes | Variables involved in the check | `c("fever", "temperature")` |
| `condition_if` | Yes | Condition when the rule applies | `"fever == 'yes'"` |
| `then` | Yes | Required condition that must be true | `"!is.na(temperature)"` |
| `action` | No | How to handle violations | `"flag_autoclean"` |

### Dependency Types

1. **Hard Dependencies** (`dependencies`):
   - Critical quality issues that should be addressed
   - Typically result in warnings or auto-cleaning

2. **Soft Dependencies** (`soft_dependencies`):
   - Less critical issues for analysis only
   - May indicate data patterns worth investigating

### Actions

The `action` field determines how violations are handled in cleaning logs:

- `"flag_autoclean"`: Sets `changed = "yes"` (ready for automatic cleaning)
- `"flag_warning"`: Sets `changed = "no"` (requires manual review)
- `"flag_analysis"`: Sets `changed = "no"` (for analysis purposes only)
- Empty or omitted: Defaults to `changed = "no"`

### Expression Translation

Dependency expressions use **canonical names and values** directly. The system automatically translates them to dataset-specific names and values at runtime using `variable_map` and `value_map`.

**Input expression:**
```r
condition_if = "fever == 'yes' & !is.na(temperature)"
```

**After translation** (using variable_map and value_map):
```r
"q7_fever_symptom %in% c('yes', 'y', '1', 'oui') & !is.na(q8_temp_celsius)"
```

**Key translation rules:**
- Variable names are replaced using `variable_map`
- `==` with canonical values becomes `%in%` with dataset values
- Function calls like `is.na()` are preserved
- Logical operators (`&`, `|`, `!`) are preserved
- Only `==` and `!=` operators trigger value translation

### Table Format

| rule_type | dep_group | variables | condition_if | then | action | label | comment |
|-----------|-----------|-----------|--------------|------|--------|-------|---------|
| dependency | other_water_source | water_source,water_source_other | water_source == 'other' | !is.na(water_source_other) | flag_autoclean | Check other text | Requires text when other selected |

**Key columns:**
- `rule_type`: "dependency" or "soft_dependency"
- `dep_group`: Name of the dependency (becomes flag column name)
- `variables`: Comma-separated list of involved variables
- `condition_if`: R expression defining when rule applies
- `then`: R expression defining requirement
- `action`: Cleaning action type

### Named List Structure

The dependency schema uses a **named list** where the list name (key) must match the dependency name:

```r
dependencies = list(
  flag_other_check = list(       # List name
    dep_group = "flag_other_check",  # Must match list name!
    variables = c("status", "status_other"),
    condition_if = "status == 'other'",
    then = "!is.na(status_other)"
  )
)
```

The flag column name is derived from the list name with automatic `flag_` prefix if not present.

### Usage

```r
# Import from table
dep_table <- read.xlsx("resources/household_dependency_schema.xlsx")
dep_schema <- dependency_table_to_schema(dep_table)

# Set on Data object
d$set_dependency_schema(dep_schema)

# Run quality checks
d$run_quality_checks("standardized")

# Check flags
head(d$data_quality_flags)

# Generate cleaning log
d$generate_cleaning_log()

# Access cleaning log
d$cleaning_log$log_df
```

### Related Functions

- `dependency_table_to_schema()` - Convert table to dependency schema
- `dependency_schema_to_table()` - Convert dependency schema to table
- `dependency_validate_table_to_schema()` - Validate table before conversion
- `dependency_validate_schema_to_table()` - Validate schema list before export

### Related Documentation

See `docs/dependency_schema_enhancements.md` for detailed information on dependency schema features.

---

## 3. Indicator Schema

### Purpose

The indicator schema defines computed indicators that should be automatically calculated during data standardization. It enables:
- Dynamic calculation of composite indicators (FCS, HHS, rCSI, HDDS, etc.)
- Automatic indicator computation with configurable parameters
- Flexible argument resolution using variable and value maps

### Structure

The indicator schema is a separate, independent schema from the main variable schema:

```r
indicator_schema <- list(
  fcs_calc = list(
    indicator_name = "fcs_calc",
    function_name = "add_fcs",
    variables = c("fsl_fcs_cereal", "fsl_fcs_legumes", "fsl_fcs_veg", 
                  "fsl_fcs_fruit", "fsl_fcs_meat", "fsl_fcs_dairy", 
                  "fsl_fcs_sugar", "fsl_fcs_oil"),
    arguments = list(
      cutoffs = "normal",
      fsl_fcs_cereal = "@variable_map$fsl_fcs_cereal",
      fsl_fcs_legumes = "@variable_map$fsl_fcs_legumes",
      fsl_fcs_veg = "@variable_map$fsl_fcs_veg",
      fsl_fcs_fruit = "@variable_map$fsl_fcs_fruit",
      fsl_fcs_meat = "@variable_map$fsl_fcs_meat",
      fsl_fcs_dairy = "@variable_map$fsl_fcs_dairy",
      fsl_fcs_sugar = "@variable_map$fsl_fcs_sugar",
      fsl_fcs_oil = "@variable_map$fsl_fcs_oil"
    ),
    label = "Food Consumption Score Calculation",
    comment = "Computes FCS score and category"
  ),
  
  hhs_calc = list(
    indicator_name = "hhs_calc",
    function_name = "add_hhs",
    variables = c("fsl_hhs_nofoodhh", "fsl_hhs_sleephungry"),
    arguments = list(
      fsl_hhs_nofoodhh = "@variable_map$fsl_hhs_nofoodhh",
      fsl_hhs_sleephungry = "@variable_map$fsl_hhs_sleephungry"
    ),
    label = "Household Hunger Scale",
    comment = NA
  )
)
```

### Fields

| Field | Required | Description | Example |
|-------|----------|-------------|---------|
| `indicator_name` | Yes | Unique identifier | `"fcs_calc"` |
| `function_name` | Yes | Function to call (must start with `add_`) | `"add_fcs"` |
| `variables` | Yes | Required variables (used for validation) | `c("fsl_fcs_cereal", "fsl_fcs_legumes")` |
| `arguments` | No | Function arguments with values or references | `list(cutoffs = "normal")` |
| `label` | No | Human-readable description | `"FCS Calculation"` |
| `comment` | No | Additional notes | `"Uses normal cutoffs"` |

### Dynamic Argument Resolution

The indicator system supports special syntax for referencing mapped values:

#### Variable Map References

Use `@variable_map$role` to reference column names:

```r
arguments = list(
  cereal_col = "@variable_map$fsl_fcs_cereal"
)

# Resolves to:
cereal_col = "actual_cereal_column_name"
```

#### Value Map References (Specific Value)

Use `@value_map$role$canonical_value` to reference standardized values:

```r
arguments = list(
  yes_values = "@value_map$consent$yes"
)

# Resolves to:
yes_values = c("yes", "y", "1", "oui")
```

#### Value Map References (All Values)

Use `@value_map$role` to reference all values for a role:

```r
arguments = list(
  status_values = "@value_map$enrollment_status"
)

# Resolves to:
status_values = list(active = c("active", "a"), inactive = c("inactive", "i"))
```

#### Literal Values

Arguments without `@` prefix are passed as literal values:

```r
arguments = list(
  cutoffs = "normal",        # Literal string
  threshold = 10,            # Literal number
  include_oil = TRUE         # Literal boolean
)
```

### Table Format

| indicator_name | function_name | variables | arguments | label | comment |
|----------------|---------------|-----------|-----------|-------|---------|
| fcs_calc | add_fcs | fsl_fcs_cereal,fsl_fcs_legumes,fsl_fcs_veg,fsl_fcs_fruit,fsl_fcs_meat,fsl_fcs_dairy,fsl_fcs_sugar,fsl_fcs_oil | cutoffs=normal,fsl_fcs_cereal=@variable_map$fsl_fcs_cereal,fsl_fcs_legumes=@variable_map$fsl_fcs_legumes | FCS Calculation | Food Consumption Score |

**Key columns:**
- `indicator_name`: Unique identifier for the indicator
- `function_name`: Name of the `add_*` function to call
- `variables`: Comma-separated list of required variables
- `arguments`: Key=value pairs, comma-separated (supports `@` references)
- `label`: Optional descriptive label
- `comment`: Optional notes

### Execution Flow

1. **Timing**: Indicators are computed during `Data$standardize()`, after type conversion and value standardization
2. **For each indicator**:
   - Check if function exists (must start with `add_`)
   - Resolve arguments (translate `@variable_map$` and `@value_map$` references)
   - Call function with `.dataset` as first argument
   - Update `self$standardized_data` with result
3. **Error Handling**: Errors are logged as warnings; standardization continues

### Creating Indicator Functions

To create a new indicator function:

1. **Function signature**: Must accept `.dataset` as first parameter
2. **Return value**: Must return a modified data frame
3. **Naming convention**: Must start with `add_`
4. **Error handling**: Use `iphra_try()` for graceful error handling

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

### Usage

```r
# Create indicator schema table
indicator_table <- tibble::tibble(
  indicator_name = "fcs_calc",
  function_name = "add_fcs",
  variables = "fsl_fcs_cereal,fsl_fcs_legumes,fsl_fcs_veg,fsl_fcs_fruit,fsl_fcs_meat,fsl_fcs_dairy,fsl_fcs_sugar,fsl_fcs_oil",
  arguments = "cutoffs=normal,fsl_fcs_cereal=@variable_map$fsl_fcs_cereal,fsl_fcs_legumes=@variable_map$fsl_fcs_legumes",
  label = "FCS Calculation",
  comment = "Food Consumption Score"
)

# Convert to schema list
indicator_schema <- indicator_table_to_schema(indicator_table)

# Import into Data object
d <- Data$new(data = my_data, uuid = "survey_id")
d$import_indicator_schema(indicator_table)

# Standardize (indicators computed automatically)
d$standardize()

# Access computed indicators
fcs_scores <- d$standardized_data$fsl_fcs_score
fcs_categories <- d$standardized_data$fsl_fcs_cat

# Export indicator schema
indicator_table <- d$export_indicator_schema()
```

### Related Functions

- `indicator_table_to_schema()` - Convert table to indicator schema list
- `indicator_schema_to_table()` - Convert indicator schema list to table
- `indicator_validate_table_to_schema()` - Validate table before conversion
- `Data$import_indicator_schema()` - Import indicator schema into Data object
- `Data$export_indicator_schema()` - Export indicator schema to table
- `Data$set_indicator_schema()` - Set indicator schema directly

### Related Documentation

See `docs/indicator_schema.md` for detailed information on indicator schema implementation.

---

## 4. Quality Schema

### Purpose

The quality schema defines statistical plausibility tests for data quality assessment. It uses **logical threshold expressions** to evaluate test results and assign penalty scores. The quality schema is still under development but provides a flexible framework for:
- Statistical plausibility testing
- Multi-level quality thresholds
- Automated quality scoring
- Quality report generation

### Structure

The quality schema consists of metadata and a list of checks:

```r
quality_schema <- list(
  metadata = list(
    version = "3.0.0",
    created_date = "2024-01-15",
    description = "Quality checks for household survey"
  ),
  
  checks = list(
    fcs_income_corr = list(
      check_name = "fcs_income_corr",
      check_label = "FCS-Income Correlation",
      variables = c("fsl_fcs_score", "household_income"),
      statistical_test = "correlation",
      test_params = list(method = "pearson"),
      thresholds = list(
        list(expression = "test_statistic >= 0.7", penalty = 0),
        list(expression = "test_statistic >= 0.5 & test_statistic < 0.7", penalty = 5),
        list(expression = "test_statistic >= 0.3 & test_statistic < 0.5", penalty = 10),
        list(expression = "test_statistic < 0.3", penalty = 20)
      )
    ),
    
    fcs_mean_plausibility = list(
      check_name = "fcs_mean_plausibility",
      check_label = "FCS mean is plausible",
      variables = c("fsl_fcs_score"),
      statistical_test = "ttest",
      test_params = list(mu = 45),
      thresholds = list(
        list(expression = "p_value >= 0.05", penalty = 0),
        list(expression = "p_value < 0.05", penalty = 15)
      )
    ),
    
    missing_data_check = list(
      check_name = "missing_data_check",
      check_label = "Key variables completeness",
      variables = c("fsl_fcs_score", "household_income", "hh_size"),
      statistical_test = "missing_percentage",
      test_params = list(),
      thresholds = list(
        list(expression = "test_statistic <= 5", penalty = 0),
        list(expression = "test_statistic > 5 & test_statistic <= 10", penalty = 5),
        list(expression = "test_statistic > 10 & test_statistic <= 20", penalty = 15),
        list(expression = "test_statistic > 20", penalty = 25)
      )
    )
  )
)
```

### Fields

| Field | Required | Description | Example |
|-------|----------|-------------|---------|
| `check_name` | Yes | Unique identifier (must match list name) | `"fcs_income_corr"` |
| `check_label` | No | Human-readable description | `"FCS-Income Correlation"` |
| `variables` | Yes | Variables involved in the test | `c("fsl_fcs_score", "household_income")` |
| `statistical_test` | Yes | Name of statistical test function | `"correlation"` |
| `test_params` | No | Test-specific parameters | `list(method = "pearson")` |
| `thresholds` | Yes | List of threshold evaluations | See below |

### Threshold Structure

Each threshold is a list with:
- `expression`: Logical R expression to evaluate (uses `test_statistic` and `p_value`)
- `penalty`: Numeric penalty score if expression is TRUE (0 = passing)

**Important**: The first matching threshold is used. Order thresholds from most specific to least specific.

### Logical Threshold Expressions

Threshold expressions evaluate statistical test results using logical operators:

#### Simple Comparisons
```r
"test_statistic >= 0.7"      # Greater than or equal
"p_value < 0.05"             # Less than
"test_statistic <= 15"       # Less than or equal
```

#### Compound Expressions
```r
"test_statistic >= 0.5 & test_statistic < 0.7"  # Between 0.5 and 0.7
"p_value > 0.01 & p_value <= 0.05"              # Marginally significant
"test_statistic <= 5 | test_statistic >= 95"    # Either tail
```

#### Available Variables in Expressions
- `test_statistic`: Numeric result from statistical test
- `p_value`: P-value (if test produces one)

### Available Statistical Tests

| Test | Returns | Test Params | Variables | Description |
|------|---------|-------------|-----------|-------------|
| `correlation` | `test_statistic` (correlation coefficient, -1 to 1) | `method=pearson\|spearman\|kendall` | 2 | Correlation between two numeric variables |
| `ttest` | `test_statistic` (t-value), `p_value` | `mu=<expected_mean>`, `paired=TRUE\|FALSE` | 1 or 2 | T-test for mean comparison |
| `chisq` | `test_statistic` (chi-squared), `p_value` | None | 2 | Chi-squared test for categorical variables |
| `flag_percentage` | `test_statistic` (percentage, 0-100) | `flag_value=<value>` | 1 | Percentage of rows with specific flag value |
| `missing_percentage` | `test_statistic` (percentage, 0-100) | None | 1+ | Percentage of missing values |
| `outlier_percentage` | `test_statistic` (percentage, 0-100) | `z_threshold=<number>` | 1 | Percentage of outliers beyond z-score |
| `coefficient_variation` | `test_statistic` (CV percentage) | None | 1 | Coefficient of variation |
| `range_violation` | `test_statistic` (percentage, 0-100) | `min_value=<n>`, `max_value=<n>` | 1 | Percentage violating range |

### Table Format

| check_name | check_label | variables | statistical_test | threshold_expression | penalty_score | test_params |
|------------|-------------|-----------|------------------|---------------------|---------------|-------------|
| fcs_income_corr | FCS-Income Correlation | fsl_fcs_score,household_income | correlation | test_statistic >= 0.7 | 0 | method=pearson |
| fcs_income_corr | FCS-Income Correlation | fsl_fcs_score,household_income | correlation | test_statistic >= 0.5 & test_statistic < 0.7 | 5 | method=pearson |
| fcs_income_corr | FCS-Income Correlation | fsl_fcs_score,household_income | correlation | test_statistic < 0.5 | 10 | method=pearson |

**Key points:**
- Multiple rows per check (one row per threshold)
- Same `check_name` groups thresholds together
- Order matters: first matching threshold is used

### Named List Structure

**Critical**: The list name (key) in `checks$<name>` **must** exactly match the `check_name` field value:

```r
checks = list(
  fcs_corr = list(              # List name
    check_name = "fcs_corr",    # MUST match list name!
    check_label = "FCS-Income Correlation",
    ...
  )
)
```

This ensures proper round-trip conversion between table and schema formats.

### Plausibility Score

After running quality checks, a plausibility score is calculated:

```
Plausibility Score = 100 - (total_penalty / max_possible_penalty * 100)
```

- Score of 100 = Perfect (no penalties)
- Score of 0 = All checks failed with maximum penalties
- Higher scores indicate better data quality

### Usage

```r
library(openxlsx)

# Read quality schema from template
quality_table <- read.xlsx("resources/quality_schema_template.xlsx")

# Convert to schema list
quality_schema <- quality_table_to_schema(quality_table)

# Create DataQuality object
dq <- DataQuality$new(
  data = my_data,
  quality_schema = quality_schema
)

# Run quality checks
dq$run_quality_checks()

# Generate report
report <- dq$generate_plausibility_report()

# View results
print(report$plausibility_score)
print(report$test_results)
print(report$summary)

# Access detailed results
dq$quality_results
```

### Best Practices

1. **Define complementary thresholds**: Ensure thresholds cover all possible outcomes
   - ✓ Good: `>= 0.7`, `>= 0.5 & < 0.7`, `< 0.5`
   - ✗ Bad: `>= 0.7`, `< 0.5` (gap: 0.5 to 0.7)

2. **Order matters**: Put more specific conditions first
   ```r
   # Correct order:
   test_statistic >= 0.7          # Check this first
   test_statistic >= 0.5          # Then this
   test_statistic < 0.5           # Finally this
   ```

3. **Penalty scoring**:
   - 0 = passing/acceptable
   - 5-10 = minor concern
   - 15-20 = moderate concern
   - 25+ = critical issue

4. **Test incrementally**: Start with one check, verify it works, then add more

### Development Status

The quality schema is **still under development**. Current limitations:
- Limited set of statistical tests (more will be added)
- Basic quality scoring algorithm (may be enhanced)
- Template structure may evolve
- Documentation is being actively refined

### Related Functions

- `quality_table_to_schema()` - Convert table to quality schema list
- `quality_schema_to_table()` - Convert quality schema list to table
- `DataQuality$new()` - Create DataQuality object with quality schema
- `DataQuality$run_quality_checks()` - Execute quality checks
- `DataQuality$generate_plausibility_report()` - Generate quality report

### Related Documentation

See `resources/quality_schema_template_README.md` for detailed information on creating quality schema templates.

---

## Schema Interactions and Workflow

### Variable Map and Value Map

The `variable_map` and `value_map` are central to the schema system and link schemas to actual datasets:

**Variable Map**: Maps canonical role names to dataset column names
```r
variable_map = list(
  uuid = "survey_id",
  fever = "q7_fever_symptom",
  temperature = "q8_temp_celsius"
)
```

**Value Map**: Maps canonical values to dataset-specific values
```r
value_map = list(
  fever = list(
    yes = c("yes", "y", "1", "oui"),
    no = c("no", "n", "0", "non")
  )
)
```

### Schema Usage Patterns

Different schemas use variable_map and value_map differently:

| Schema | Variable Reference | Value Reference | Translation |
|--------|-------------------|-----------------|-------------|
| **Variable Schema** | Defines canonical names | Defines canonical-to-dataset mappings | Built into schema |
| **Dependency Schema** | Uses canonical names directly | Uses canonical values directly | Automatic (implicit) |
| **Indicator Schema** | Uses `@variable_map$role` | Uses `@value_map$role$value` | Explicit references |
| **Quality Schema** | Uses variable names | N/A (statistical tests) | Uses actual column names |

### Typical Workflow

1. **Define Variable Schema**
   ```r
   # Import or create variable schema
   schema <- data_table_to_schema(schema_table)
   d$set_variable_schema(schema)
   ```

2. **Set Variable and Value Maps**
   ```r
   # Maps canonical names/values to dataset
   d$variable_map <- list(uuid = "survey_id", fever = "q7_fever")
   d$value_map <- list(fever = list(yes = c("yes", "1"), no = c("no", "0")))
   ```

3. **Import Dependency Schema** (optional)
   ```r
   # Define quality check rules
   dep_schema <- dependency_table_to_schema(dep_table)
   d$set_dependency_schema(dep_schema)
   ```

4. **Import Indicator Schema** (optional)
   ```r
   # Define computed indicators
   d$import_indicator_schema(indicator_table)
   ```

5. **Import Quality Schema** (optional)
   ```r
   # Define statistical quality tests
   quality_schema <- quality_table_to_schema(quality_table)
   dq <- DataQuality$new(data = d$standardized_data, 
                         quality_schema = quality_schema)
   ```

6. **Standardize Data**
   ```r
   # Applies variable schema, computes indicators
   d$standardize()
   ```

7. **Run Quality Checks**
   ```r
   # Applies dependency schema
   d$run_quality_checks("standardized")
   
   # Run statistical quality tests
   dq$run_quality_checks()
   ```

8. **Generate Cleaning Log**
   ```r
   # Create cleaning log from quality flags
   d$generate_cleaning_log()
   ```

9. **Generate Quality Report**
   ```r
   # Statistical plausibility report
   report <- dq$generate_plausibility_report()
   ```

### Schema Interaction Diagram

```
┌─────────────────────────────────────────────────────────┐
│                    Variable Schema                       │
│  - Defines types, validation, value mappings            │
│  - Creates variable_map and value_map                   │
└─────────────────────┬───────────────────────────────────┘
                      │
                      ├──► Dependency Schema
                      │    - Uses canonical names/values
                      │    - Automatic translation via maps
                      │    - Creates quality flags
                      │
                      ├──► Indicator Schema
                      │    - Uses @variable_map$ and @value_map$
                      │    - Explicit argument resolution
                      │    - Computes indicators during standardize()
                      │
                      └──► Quality Schema
                           - Uses variable names from data
                           - Statistical plausibility tests
                           - Generates quality scores

┌─────────────────────────────────────────────────────────┐
│                     Data Workflow                        │
│                                                          │
│  1. Import schemas → 2. Set maps → 3. Standardize       │
│  4. Run quality checks → 5. Generate reports            │
└─────────────────────────────────────────────────────────┘
```

### Schema Storage Formats

All schemas support two storage formats:

1. **List Format** (R nested lists)
   - Used internally in R objects
   - Hierarchical structure
   - Easy to manipulate programmatically

2. **Table Format** (Excel/CSV)
   - Used for import/export
   - Flat structure (one row per rule/variable/threshold)
   - Easy to edit in spreadsheet software

Conversion functions are provided for each schema type:
- `*_table_to_schema()` - Table → List
- `*_schema_to_table()` - List → Table
- `*_validate_table_to_schema()` - Validate table
- `*_validate_schema_to_table()` - Validate list

### Best Practices

1. **Start with variable schema**: Define types and value mappings first
2. **Use templates**: Start from template files in `resources/` folder
3. **Test incrementally**: Test each schema component before combining
4. **Document canonical names**: Maintain consistent naming across schemas
5. **Version control schemas**: Track schema changes with version numbers
6. **Validate before using**: Use validation functions before importing
7. **Keep schemas separate**: Store each schema type in separate files
8. **Use descriptive names**: Use clear, descriptive names for variables and checks
9. **Document assumptions**: Add comments/labels explaining rules and thresholds

---

## Summary Table

| Feature | Variable Schema | Dependency Schema | Indicator Schema | Quality Schema |
|---------|----------------|------------------|------------------|----------------|
| **Purpose** | Define variables & validation | Quality check rules | Computed indicators | Statistical tests |
| **Storage Field** | `variable_schema` | `dependency_schema` | `indicator_schema` | `quality_schema` |
| **Import Method** | `set_schema()` | `set_dependency_schema()` | `import_indicator_schema()` | `DataQuality$new()` |
| **Export Method** | `data_schema_to_table()` | `dependency_schema_to_table()` | `export_indicator_schema()` | `quality_schema_to_table()` |
| **Execution** | During `standardize()` | During `run_quality_checks()` | During `standardize()` | During `run_quality_checks()` |
| **Output** | Standardized data | Quality flags | Indicator columns | Quality scores |
| **Uses Maps** | Defines maps | Uses maps (implicit) | Uses maps (explicit @) | Uses column names |
| **Format** | List or table | List or table | List or table | List or table |
| **Status** | Stable | Stable | Stable | In Development |

---

## Resources

### Template Files

Templates are available in the `resources/` folder:

- Variable schema templates: `*_variable_schema_template.xlsx`
- Dependency schema templates: `*_dependency_schema_template.xlsx`
- Indicator schema templates: `*_indicator_schema_template.xlsx`
- Quality schema templates: `quality_schema_*_template.xlsx`

### Documentation Files

- `docs/variable_value_mapping_guide.md` - Variable and value mapping
- `docs/dependency_schema_enhancements.md` - Dependency schema features
- `docs/indicator_schema.md` - Indicator schema implementation
- `resources/quality_schema_template_README.md` - Quality schema guide
- `docs/schema_enhancements_examples.md` - Examples of schema features

### Related Functions

All schema conversion and validation functions are in:
- `R/utils_data_class.R` - Schema utility functions

Schema usage is implemented in:
- `R/class_data.R` - Data class with schema methods
- `R/class_data_quality.R` - DataQuality class

---

## Version History

- **Version 3.0.0** (Current)
  - Variable schema with value mapping
  - Dependency schema with named list structure
  - Indicator schema with dynamic argument resolution
  - Quality schema with logical threshold expressions (in development)

---

## Support

For questions or issues:
- Review the detailed documentation for each schema type
- Check template files in `resources/` folder
- Examine test files in `tests/testthat/test-*_schema.R`
- Refer to the iphRa package documentation
