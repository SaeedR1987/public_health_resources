# Variable and Value Mapping Guide

## Overview

The public_health_resources data pipeline uses a mapping system to translate between **canonical names/values** (standardized across all datasets) and **dataset-specific names/values** (actual column names and values in your data).

This guide explains how to reference mapped variables and values in different parts of your schema.

## Quick Reference

| Schema Type | Recommended Syntax | Example |
|-------------|-------------------|---------|
| **Dependency Schema** | Canonical names directly | `fever == 'yes'` |
| **Indicator Schema** | `@variable_map$` and `@value_map$` | `@variable_map$fever` |

## Core Concepts

### Variable Mapping

Maps **canonical role names** to **dataset column names**:

```r
variable_map = list(
  fever = "q7_fever_symptom",      # Canonical name → Dataset column
  temperature = "q8_temp_celsius"
)
```

### Value Mapping

Maps **canonical values** to **dataset-specific values** for categorical variables:

```r
value_map = list(
  fever = list(
    yes = c("yes", "y", "1", "oui"),   # Canonical value → Dataset values
    no = c("no", "n", "0", "non")
  )
)
```

## Dependency Schema: Implicit Translation

### Purpose

Dependency schemas define quality check rules as R expressions. These expressions are evaluated against your dataset to identify data quality issues.

### Syntax: Use Canonical Names Directly

Write your conditions using canonical variable names and values **without any special syntax**:

```r
dependency_schema <- list(
  dependencies = list(
    flag_fever_temp = list(
      variables = c("fever", "temperature"),
      condition_if = "fever == 'yes'",
      then = "!is.na(temperature)",
      action = "flag_warning"
    )
  )
)
```

### How It Works

At runtime, the `.translate_expression()` method automatically translates canonical names to dataset-specific names:

**Input expression:**
```r
"fever == 'yes' & !is.na(temperature)"
```

**After translation (using mappings above):**
```r
"q7_fever_symptom %in% c('yes', 'y', '1', 'oui') & !is.na(q8_temp_celsius)"
```

**Key points:**
- Variable names are replaced: `fever` → `q7_fever_symptom`
- Canonical values are expanded: `'yes'` → `c('yes', 'y', '1', 'oui')`
- `==` becomes `%in%` when there are multiple dataset values
- Function calls like `is.na()` are preserved

### Why Implicit Syntax?

Dependency expressions are **R code**. Using canonical names directly makes them:
- **Readable:** Looks like natural R code
- **Maintainable:** Easy to understand the logic
- **Portable:** Same expression works across different datasets

### Examples

#### Example 1: Simple Equality Check

```r
# Schema
flag_gender_check = list(
  variables = c("gender"),
  condition_if = "!is.na(gender)",
  then = "gender %in% c('male', 'female')"
)

# If variable_map has: gender = "q1_respondent_gender"
# If value_map has: gender = list(male = c("m", "male"), female = c("f", "female"))
# Expression translates to:
# "!is.na(q1_respondent_gender) & q1_respondent_gender %in% c('m', 'male', 'f', 'female')"
```

#### Example 2: Multiple Variables

```r
flag_pregnant_dob = list(
  variables = c("pregnant", "date_of_birth"),
  condition_if = "pregnant == 'yes'",
  then = "!is.na(date_of_birth)"
)

# Translates canonical names and values automatically
```

#### Example 3: Complex Logic

```r
flag_hh_composition = list(
  variables = c("hh_size", "num_children", "num_adults"),
  condition_if = "!is.na(hh_size)",
  then = "(num_children + num_adults) == hh_size"
)

# Variable names are translated, numeric operations are preserved
```

### Supported Operators for Value Translation

The value translation supports:

- **Equality:** `variable == 'canonical_value'`
- **Inequality:** `variable != 'canonical_value'`
- **Set membership:** `variable %in% c('canonical_value1', 'canonical_value2', ...)`

Other operators are not translated for values:
- `<`, `>`, `<=`, `>=`, etc. → No value expansion

### Limitations

1. **Variable must be in variable_map for value translation:**
   - `fever == 'yes'` ✅ (if `fever` is in variable_map)
   - `unknown_var == 'yes'` ⚠️ (variable name not translated, value not translated)

2. **Supported operators for value translation:**
   - `fever == 'yes'` ✅ Expands to `%in%` when multiple values mapped
   - `fever != 'no'` ✅ Expands to `!(fever %in% ...)` when multiple values
   - `fever %in% c('yes', 'no')` ✅ Expands each canonical value to dataset values

## Indicator Schema: Explicit References

### Purpose

Indicator schemas define computed indicators (like FCS, HHS) by calling `add_*` functions. Arguments specify which columns to use and what values to check for.

### Syntax: Use @variable_map$ and @value_map$

Use explicit reference syntax to indicate which arguments should be resolved from mappings:

```r
indicator_schema <- list(
  fcs_calc = list(
    indicator_name = "fcs_calc",
    function_name = "add_fcs",
    variables = c("fsl_fcs_cereal", "fsl_fcs_legumes"),
    arguments = list(
      cereal_col = "@variable_map$fsl_fcs_cereal",
      legumes_col = "@variable_map$fsl_fcs_legumes",
      cutoffs = "normal"  # Literal value, not mapped
    )
  )
)
```

### How It Works

At runtime, the indicator argument resolution code:
1. Checks each argument value for `@variable_map$` or `@value_map$` prefix
2. If found, looks up the value from the mapping
3. Passes the resolved value to the `add_*` function

**Variable reference:**
```r
cereal_col = "@variable_map$fsl_fcs_cereal"
# Resolves to:
cereal_col = "actual_cereal_column_name"
```

**Value reference (specific canonical value):**
```r
yes_values = "@value_map$consent$yes"
# Resolves to:
yes_values = c("yes", "y", "1", "oui")
```

**Value reference (all values for a role):**
```r
all_status_values = "@value_map$status"
# Resolves to entire value_map for that role:
all_status_values = list(yes = c("yes", "y"), no = c("no", "n"))
```

### Why Explicit Syntax?

Indicator arguments are **function parameters**, not expressions. The @ syntax:
- **Clarity:** Obvious which arguments are mapped vs literal
- **Safety:** Won't accidentally translate literal values
- **Flexibility:** Can specify exactly what to resolve

### Examples

#### Example 1: Column Name References

```r
indicator_schema <- list(
  nutrition_ind = list(
    function_name = "add_muac_category",
    variables = c("muac_mm"),
    arguments = list(
      muac_column = "@variable_map$muac_mm",
      age_column = "@variable_map$age_months"
    )
  )
)

# The add_muac_category function receives:
# muac_column = "actual_muac_column_name"
# age_column = "actual_age_column_name"
```

#### Example 2: Value References

```r
indicator_schema <- list(
  food_security_ind = list(
    function_name = "add_food_security_status",
    variables = c("meal_frequency"),
    arguments = list(
      meal_freq_col = "@variable_map$meal_frequency",
      insufficient_values = "@value_map$meal_frequency$insufficient",
      sufficient_values = "@value_map$meal_frequency$sufficient"
    )
  )
)

# The function receives the actual dataset values for each category
```

#### Example 3: Mixed Literal and Mapped

```r
indicator_schema <- list(
  fcs_calc = list(
    function_name = "add_fcs",
    arguments = list(
      cereal_col = "@variable_map$fsl_fcs_cereal",  # Mapped
      legumes_col = "@variable_map$fsl_fcs_legumes", # Mapped
      cutoffs = "normal",                            # Literal
      include_oil = TRUE                             # Literal
    )
  )
)

# Mapped arguments are resolved, literals are passed as-is
```

### Syntax Details

**Variable reference:**
```r
"@variable_map$role_name"
```
- Must start with `@variable_map$`
- Followed by the canonical role name
- Resolves to the column name from `variable_map[[role_name]]`

**Value reference (specific canonical value):**
```r
"@value_map$role_name$canonical_value"
```
- Must start with `@value_map$`
- Followed by role name and canonical value separated by `$`
- Resolves to `value_map[[role_name]][[canonical_value]]`
- Returns a vector of dataset values

**Value reference (all values for role):**
```r
"@value_map$role_name"
```
- Must start with `@value_map$`
- Followed by only the role name (no second $)
- Resolves to `value_map[[role_name]]`
- Returns the entire mapping list for that role

## Comparison Table

| Feature | Dependency Schema | Indicator Schema |
|---------|------------------|------------------|
| **Context** | R expressions (quality checks) | Function arguments |
| **Syntax** | Canonical names directly | `@variable_map$` / `@value_map$` |
| **Example** | `fever == 'yes'` | `@value_map$fever$yes` |
| **Translation** | Automatic (implicit) | Manual (explicit) |
| **Variable replacement** | All canonical names | Only marked with @ |
| **Value replacement** | == and != operators | Explicit reference |
| **Use for column names** | In expressions | As function arguments |
| **Use for values** | In comparisons | As function arguments |

## Best Practices

### For Dependency Schemas

1. **Use canonical names consistently:**
   ```r
   ✅ condition_if = "fever == 'yes'"
   ❌ condition_if = "q7_fever == 'yes'"  # Don't use dataset-specific names
   ```

2. **Ensure variables are in variable_map:**
   - If a variable isn't mapped, translation won't occur
   - This may cause the expression to fail

3. **Use canonical values in comparisons:**
   ```r
   ✅ condition_if = "status == 'active'"
   ❌ condition_if = "status == 'a'"  # Don't use dataset-specific values
   ```

4. **Test your expressions:**
   - Use `data$data_diagnose()` to check mappings
   - Verify canonical names are in variable_map
   - Verify canonical values are in value_map

### For Indicator Schemas

1. **Use @ syntax for all mapped arguments:**
   ```r
   ✅ cereal_col = "@variable_map$fsl_fcs_cereal"
   ❌ cereal_col = "fsl_fcs_cereal"  # Ambiguous - is this canonical or literal?
   ```

2. **Don't use @ for literal values:**
   ```r
   ✅ cutoffs = "normal"
   ❌ cutoffs = "@variable_map$cutoffs"  # Unless cutoffs is really a column name
   ```

3. **Be specific with value references:**
   ```r
   ✅ yes_vals = "@value_map$consent$yes"  # Specific canonical value
   ✅ all_vals = "@value_map$consent"      # All values for role
   ❌ yes_vals = "@value_map$yes"          # Which role's 'yes'?
   ```

4. **Document non-obvious arguments:**
   ```r
   arguments = list(
     status_col = "@variable_map$enrollment_status",  # Column name
     active_values = "@value_map$enrollment_status$active",  # Values to check
     threshold = 5  # Literal number (not mapped)
   )
   ```

## Troubleshooting

### Dependency Schema Issues

**Problem:** Expression fails with "object 'fever' not found"

**Cause:** Canonical name 'fever' not translated (missing from variable_map)

**Solution:**
```r
# Add to variable_map
d$variable_map$fever <- "q7_fever_column"
```

**Problem:** Expression doesn't flag expected rows

**Cause:** Canonical value not expanded to dataset values

**Solution:**
```r
# Add to value_map
d$value_map$fever <- list(
  yes = c("yes", "y", "oui"),
  no = c("no", "n", "non")
)
```

### Indicator Schema Issues

**Problem:** Warning "Variable map role 'xyz' not found"

**Cause:** `@variable_map$xyz` reference but xyz not in variable_map

**Solution:**
```r
# Add mapping
d$variable_map$xyz <- "actual_column_name"
```

**Problem:** Warning "Value map role 'status' not found"

**Cause:** `@value_map$status$yes` reference but status not in value_map

**Solution:**
```r
# Add mapping
d$value_map$status <- list(
  yes = c("yes", "y"),
  no = c("no", "n")
)
```

**Problem:** Function receives "@variable_map$xyz" as literal string

**Cause:** Forgot to add @ prefix or typo in syntax

**Solution:**
```r
# Check syntax
✅ arg = "@variable_map$role"
❌ arg = "variable_map$role"   # Missing @
❌ arg = "@variablemap$role"   # Typo
❌ arg = "@variable_map.role"  # Wrong separator
```

## Advanced Topics

### When to Use Each Approach

**Use Dependency Schema (implicit translation) when:**
- Defining data quality rules
- Writing conditions for flagging issues
- Checking logical relationships between variables
- Need natural R expression syntax

**Use Indicator Schema (explicit @ syntax) when:**
- Computing derived indicators
- Calling add_* functions
- Passing column names as function arguments
- Passing sets of values as function arguments
- Need clear distinction between mapped and literal arguments

### Can I Mix Syntaxes?

**In dependency schemas:** No, use canonical names directly. The @ syntax is not currently supported in dependency expressions (could be added if needed).

**In indicator schemas:** Technically yes - you can omit @ for literal values. But using @ consistently for all mapped arguments is recommended for clarity.

### Cross-Schema Consistency

If you have a canonical variable used in both schemas:

**Dependency schema:**
```r
condition_if = "fever == 'yes'"
```

**Indicator schema:**
```r
fever_col = "@variable_map$fever",
yes_values = "@value_map$fever$yes"
```

Both reference the same mappings:
```r
variable_map$fever = "q7_fever"
value_map$fever = list(yes = c("yes", "y"), no = c("no", "n"))
```

## Summary

- **Dependency schemas:** Use canonical names directly in R expressions
  - Natural, readable syntax
  - Automatic translation at runtime
  
- **Indicator schemas:** Use `@variable_map$` and `@value_map$` for mapped arguments
  - Explicit, clear references
  - Resolved before function calls

Both approaches work with the same underlying `variable_map` and `value_map` structures, just with different syntaxes appropriate for their use cases.

## Automatic Mapping with `map_schema_vars`

The `map_schema_vars()` method automatically maps schema variables to dataset columns based on the `col_names` field in your variable schema.

### Column Priority Behavior

When multiple column names from the schema's `col_names` list exist in your dataset, `map_schema_vars()` will **preferentially map to the first matching column in the order they are listed**.

**Example:**

```r
# Schema defines multiple possible column names
schema <- list(
  col_names = list(
    deaths = c("linked_num_deaths", "num_deaths", "death_count")
  )
)

# Dataset contains both "linked_num_deaths" and "num_deaths"
df <- data.frame(
  id = 1:5,
  linked_num_deaths = c(2, 0, 1, 3, 0),
  num_deaths = c(1, 0, 2, 1, 0)
)

# After calling map_schema_vars()
d$map_schema_vars()

# Result: Maps to "linked_num_deaths" (first in schema list)
d$variable_map$deaths  # "linked_num_deaths"
```

### How It Works

The implementation iterates through the `col_names` list **sequentially** and maps to the **first column found**:

```r
# Pseudocode of the algorithm
for (col_candidate in possible_cols) {
  if (col_candidate %in% data_cols) {
    matched_col <- col_candidate
    break  # Stops at first match
  }
}
```

### Key Points

1. **Order matters:** List preferred column names first in `col_names`
2. **First match wins:** Stops searching after finding the first available column
3. **Fallback behavior:** If the first choice isn't available, uses the next one in line
4. **Smart updates:** Updates to more preferred columns when they become available
   - If a more preferred column (earlier in `col_names`) is added to the dataset, the mapping is automatically updated
   - Example: If mapped to "num_deaths" and "linked_num_deaths" is added later, updates to "linked_num_deaths"
   - Only updates if both columns are in the schema's `col_names` list
   - Preserves user-defined mappings (columns not in schema)

### Practical Use Cases

**Use Case 1: Prefer linked data**

When you have both linked and original versions of a variable, list the linked version first:

```r
col_names = list(
  deaths = c("linked_num_deaths", "num_deaths")  # Prefer linked version
)
```

**Use Case 2: Multi-language support**

Support datasets with different language column names:

```r
col_names = list(
  consent = c("consent", "consentement", "consentimiento")  # English first
)
```

**Use Case 3: Legacy column names**

Support both new standardized names and legacy names:

```r
col_names = list(
  age = c("age_years", "age", "respondent_age")  # New standard first
)
```

**Use Case 4: Dynamic column additions during standardization**

When indicators or linked datasets add preferred columns during standardization:

```r
# Initial dataset has less preferred column
df <- data.frame(
  id = 1:5,
  num_deaths = c(1, 0, 2, 1, 0)
)

# Schema lists preferred column first
col_names = list(
  deaths = c("linked_num_deaths", "num_deaths")  # Prefer linked
)

# Initially maps to "num_deaths"
d$map_schema_vars()  # deaths → "num_deaths"

# During standardization, a linked dataset adds "linked_num_deaths"
# (e.g., via pre_standardize aggregation)

# Next map_schema_vars call automatically updates
d$map_schema_vars()  # deaths → "linked_num_deaths" (updated!)
```

This automatic update behavior is especially important when processing indicator schemas, where each indicator may add columns that are more preferred versions of existing variables.

### Test Coverage

Comprehensive tests verify this behavior in `tests/testthat/test-map_schema_vars_column_priority.R` and `tests/testthat/test-map_schema_vars_indicator_loop.R`:

- Maps first matching column when multiple exist
- Falls back to subsequent columns when earlier ones are missing
- Respects priority order with 3+ options
- Order changes affect which column is selected
- Updates to more preferred columns when they become available
- Preserves user-defined mappings (columns not in schema)
- Corrects invalid existing mappings
- Works correctly during indicator processing loop
- Dependent indicators can reference variables from previous indicators

## Additional Resources

For more information on schemas:
- Dependency Schema Documentation: `docs/dependency_schema_enhancements.md`
- Indicator Schema Documentation: `docs/indicator_schema.md`
- Variable Schema Documentation: `docs/variable_schema.md`

