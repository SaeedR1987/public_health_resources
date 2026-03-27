# NA Handling in Dependency Rules

## Overview

This document explains how NA (missing) values are handled in dependency rule expressions, particularly in `%in%` expressions used for allowed value checks.

## Problem

Prior to this fix, dependency rules with allowed value checks would incorrectly flag NA values as errors, even when NA was explicitly included in the allowed values list. For example:

```r
# Dependency rule
then = "sex %in% c('male', 'female', NA)"

# Data with NA values
sex = c("male", "female", NA, "other")

# Before fix: NA would be flagged as an error (incorrect)
# After fix: NA is NOT flagged (correct)
```

## Root Cause

The issue occurred because:

1. **String Parsing**: The `.translate_expression()` function parsed NA as the string `"NA"` rather than R's NA constant
2. **String Wrapping**: All values were wrapped in quotes, resulting in `'NA'` (a string) instead of `NA_character_`
3. **%in% Behavior**: In R, `NA %in% c('male', 'female', 'NA')` returns NA (not TRUE) because the string `'NA'` doesn't match the NA constant

## Solution

The fix involves three components:

### 1. NA Detection

The code now detects unquoted NA constants in value lists:

```r
item_trimmed <- trimws(item)
is_unquoted_na <- (item_trimmed == "NA" && !grepl("^['\"].*['\"]$", item_trimmed))
```

This distinguishes between:
- `NA` (unquoted) → R's NA constant
- `'NA'` or `"NA"` (quoted) → the string "NA"

### 2. NA Translation

When building the translated expression, unquoted NA values are converted to `NA_character_`:

```r
# Input expression
"sex %in% c('male', 'female', NA)"

# Translated expression
"sex %in% c('male', 'female', NA_character_)"
```

### 3. NA Result Handling

When the condition evaluates to NA (e.g., when checking `NA %in% c('male', 'female')`), the flag calculation treats NA results as failures:

```r
flag_vec <- rep(0, nrow(df))
flag_vec[cond_if & !cond_then] <- 1
flag_vec[is.na(flag_vec)] <- 1  # Treat NA as flagged
```

## R's %in% Behavior with NA

Understanding R's `%in%` operator behavior with NA is crucial:

```r
# Without NA_character_ in the list
NA %in% c('male', 'female')           # Returns: NA

# With NA_character_ in the list
NA %in% c('male', 'female', NA_character_)  # Returns: TRUE
```

This means:
- **NA in allowed list**: NA values pass the check (evaluate to TRUE)
- **NA not in allowed list**: NA values fail the check (evaluate to NA, then flagged)

## Use Cases

### Example 1: Sex with NA allowed

```r
# Dependency schema
dependencies = list(
  flag_values_sex = list(
    variables = c("sex"),
    condition_if = "TRUE",
    then = "sex %in% c('male', 'female', NA)",
    action = "flag_warning"
  )
)

# Results
# "male"   → 0 (not flagged)
# "female" → 0 (not flagged)
# NA       → 0 (not flagged, NA is allowed)
# "other"  → 1 (flagged)
```

### Example 2: Height sticks with NA allowed

```r
# From roster dependency schema
then = "height_sticks %in% c('under6m','6m_to_23m','23m_to59m','60m_plus',NA)"

# Results
# "under6m"       → 0 (not flagged)
# "6m_to_23m"     → 0 (not flagged)
# NA              → 0 (not flagged, NA is allowed)
# "invalid_value" → 1 (flagged)
```

### Example 3: Alternative using is.na()

If you prefer to be explicit about NA handling, you can use `is.na()`:

```r
then = "sex %in% c('male', 'female') | is.na(sex)"
```

This is equivalent to including NA in the list but makes the intent clearer.

## Testing

Comprehensive tests are available in `tests/testthat/test-na-in-dependency-rules.R`:

1. NA values with explicit NA in allowed list → not flagged
2. NA values without NA in allowed list → flagged
3. is.na() check works correctly → not flagged
4. height_sticks real-world case → not flagged

## Migration Guide

If you have existing dependency rules and want to allow NA values:

### Before
```r
then = "sex %in% c('male', 'female')"
# This flags NA values
```

### After
```r
# Option 1: Add NA to the list
then = "sex %in% c('male', 'female', NA)"

# Option 2: Use is.na() explicitly
then = "sex %in% c('male', 'female') | is.na(sex)"
```

Both options produce the same result after the fix.

## Technical Details

### Code Location

- **Translation logic**: `R/class_data.R` in the `.translate_expression()` method
- **Flag calculation**: `R/class_data.R` in the `run_quality_checks()` method
- **Tests**: `tests/testthat/test-na-in-dependency-rules.R`

### Related Issues

This fix resolves the inconsistent behavior where:
- Some checks (like sex) would sometimes work with NA
- Other checks (like height_sticks) would consistently fail with NA
- The behavior depended on implementation details rather than being predictable

Now all checks handle NA consistently according to whether NA is in the allowed values list.
